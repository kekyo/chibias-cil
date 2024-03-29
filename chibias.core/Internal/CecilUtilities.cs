﻿/////////////////////////////////////////////////////////////////////////////////////
//
// chibias-cil - The specialized backend CIL assembler for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using Mono.Cecil;
using Mono.Cecil.Cil;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace chibias.Internal;

internal enum ScopeDescriptors
{
    Public,
    Internal,
    File,
    _Module_,   // For internal use only.
}

internal static class CecilUtilities
{
    private static readonly Dictionary<string, OpCode> opCodes =
        typeof(OpCodes).GetFields().
        Where(field =>
            field.IsPublic && field.IsStatic && field.IsInitOnly &&
            field.FieldType.FullName == "Mono.Cecil.Cil.OpCode").
        Select(field => (OpCode)field.GetValue(null)!).
        ToDictionary(opCode => opCode.Name.Replace('_', '.').ToLowerInvariant());

    private static readonly Dictionary<string, string> aliasTypeNames =
        new Dictionary<string, string>()
    {
        { "void", "System.Void" },
        { "uint8", "System.Byte" },
        { "int8", "System.SByte" },
        { "int16", "System.Int16" },
        { "uint16", "System.UInt16" },
        { "int32", "System.Int32" },
        { "uint32", "System.UInt32" },
        { "int64", "System.Int64" },
        { "uint64", "System.UInt64" },
        { "float32", "System.Single" },
        { "float64", "System.Double" },
        { "nint", "System.IntPtr" },
        { "nuint", "System.UIntPtr" },
        { "bool", "System.Boolean" },
        { "char", "System.Char" },
        { "object", "System.Object" },
        { "string", "System.String" },
        { "typedref", "System.TypedReference" },
        { "byte", "System.Byte" },
        { "sbyte", "System.SByte" },
        { "short", "System.Int16" },
        { "ushort", "System.UInt16" },
        { "int", "System.Int32" },
        { "uint", "System.UInt32" },
        { "long", "System.Int64" },
        { "ulong", "System.UInt64" },
        { "single", "System.Single" },
        { "float", "System.Single" },
        { "double", "System.Double" },
        { "char16", "System.Char" },
        { "intptr", "System.IntPtr" },
        { "uintptr", "System.UIntPtr" },
    };

    private static readonly HashSet<string> enumerationUnderlyingTypes = new HashSet<string>()
    {
        "System.Byte", "System.SByte", "System.Int16", "System.UInt16",
        "System.Int32", "System.UInt32", "System.Int64", "System.UInt64",
    };

    private static readonly HashSet<char> invalidMemberNameChars = new()
    {
        '-', '+', '=', '#', '@', '$', '%', '~', '.', ',', ':', ';',
        '*', '&', '^', '?', '!', '\'', '"', '`', '|', '/', '\\',
        '[', ']', '(', ')', '<', '>', '{', '}',
        '\a', '\b', '\t', '\n', '\v', '\f', '\r',
        '\u0001', '\u0002', '\u0003', '\u0004', '\u0005', '\u0006', '\u000e', '\u000f',
        '\u0010', '\u0011', '\u0012', '\u0013', '\u0014', '\u0015', '\u0016', '\u0017',
        '\u0018', '\u0019', '\u001a', '\u001b', '\u001c', '\u001d', '\u001e', '\u001f',
    };

    public static int GetOpCodeStackSize(StackBehaviour sb) =>
        sb switch
        {
            StackBehaviour.Pop0 => 0,
            StackBehaviour.Pop1 => -1,
            StackBehaviour.Pop1_pop1 => -2,
            StackBehaviour.Popi => -1,
            StackBehaviour.Popi_pop1 => -2,
            StackBehaviour.Popi_popi => -2,
            StackBehaviour.Popi_popi8 => -2,
            StackBehaviour.Popi_popi_popi => -3,
            StackBehaviour.Popi_popr4 => -2,
            StackBehaviour.Popi_popr8 => -2,
            StackBehaviour.Popref => -1,
            StackBehaviour.Popref_pop1 => -2,
            StackBehaviour.Popref_popi => -2,
            StackBehaviour.Popref_popi_popi => -3,
            StackBehaviour.Popref_popi_popi8 => -3,
            StackBehaviour.Popref_popi_popr4 => -3,
            StackBehaviour.Popref_popi_popr8 => -3,
            StackBehaviour.Popref_popi_popref => -3,
            StackBehaviour.Varpop => -1,
            StackBehaviour.Push0 => 0,
            StackBehaviour.Push1 => 1,
            StackBehaviour.Push1_push1 => 2,
            StackBehaviour.Pushi => 1,
            StackBehaviour.Pushi8 => 1,
            StackBehaviour.Pushr4 => 1,
            StackBehaviour.Pushr8 => 1,
            StackBehaviour.Pushref => 1,
            StackBehaviour.Varpush => 1,
            _ => 0,
        };

    public static Instruction CreateInstruction(OpCode opCode, object operand) =>
        operand switch
        {
            MethodReference method => Instruction.Create(opCode, method),
            FieldReference field => Instruction.Create(opCode, field),
            TypeReference type => Instruction.Create(opCode, type),
            CallSite callSite => Instruction.Create(opCode, callSite),
            Instruction instruction => Instruction.Create(opCode, instruction),
            _ => throw new InvalidOperationException(),
        };

    public static string SanitizeFileNameToMemberName(string fileName)
    {
        var sb = new StringBuilder(fileName);
        for (var index = 0; index < sb.Length; index++)
        {
            if (invalidMemberNameChars.Contains(sb[index]))
            {
                sb[index] = '_';
            }
        }
        if (fileName.EndsWith(".s") || fileName.EndsWith(".o"))
        {
            sb.Remove(sb.Length - 2, 2);
        }
        return sb.ToString();
    }

    public static bool TryLookupScopeDescriptorName(
        string scopeDescriptorName,
        out ScopeDescriptors scopeDescriptor) =>
        Enum.TryParse(scopeDescriptorName, true, out scopeDescriptor);

    public static bool TryLookupOriginTypeName(
        string typeName,
        out string originTypeName) =>
        aliasTypeNames.TryGetValue(typeName, out originTypeName!);

    public static bool IsEnumerationUnderlyingType(
        string typeName)
    {
        if (TryLookupOriginTypeName(typeName, out var originName))
        {
            typeName = originName;
        }
        return enumerationUnderlyingTypes.Contains(typeName);
    }

    public static bool TryParseOpCode(
        string word,
        out OpCode opCode) =>
        opCodes.TryGetValue(word, out opCode);

    public static bool TryMakeFunctionPointerType(
        this MethodReference method,
        out FunctionPointerType type)
    {
        if (method.HasThis)
        {
            type = null!;
            return false;
        }

        type = new FunctionPointerType
        {
            ReturnType = method.ReturnType,
            CallingConvention = method.CallingConvention,
            HasThis = method.HasThis,
            ExplicitThis = method.ExplicitThis,
        };
        foreach (var parameter in method.Parameters)
        {
            type.Parameters.Add(new(
                parameter.Name, parameter.Attributes, parameter.ParameterType));
        }

        return true;
    }

    public static TypeDefinition CreateDummyType(int postfix) =>
        new("", $"<placeholder_type>_${postfix}",
            TypeAttributes.NotPublic | TypeAttributes.Abstract | TypeAttributes.Sealed);
    public static FieldDefinition CreateDummyField(int postfix) =>
        new($"<placeholder_field>_${postfix}",
            FieldAttributes.Private | FieldAttributes.InitOnly,
            CreateDummyType(postfix));
    public static MethodDefinition CreateDummyMethod(int postfix) =>
        new($"<placeholder_method>_${postfix}",
            MethodAttributes.Private | MethodAttributes.Abstract,
            CreateDummyType(postfix));

    public static bool IsPlaceholder(TypeReference type) =>
        type.Name.StartsWith("<placeholder_type>_$");
    public static bool IsPlaceholder(FieldReference field) =>
        field.Name.StartsWith("<placeholder_field>_$");
    public static bool IsPlaceholder(MethodReference method) =>
        method.Name.StartsWith("<placeholder_method>_$");

    public static bool IsValidCAbiParameter(
        MethodReference method, string[] parameterTypeNames) =>
        method.CallingConvention switch
        {
            MethodCallingConvention.VarArg =>
                method.Parameters.
                    Zip(parameterTypeNames, (p, ptn) => p.ParameterType.FullName == ptn).
                    All(eq => eq),
            _ =>
                parameterTypeNames.Length == 0,
        };

    public static void SetFieldType(
        FieldDefinition field, TypeReference type)
    {
        field.FieldType = type;

        // Special case: Force 1 byte footprint on boolean type.
        if (type.FullName == "System.Boolean")
        {
            field.MarshalInfo = new(NativeType.U1);
        }
        else if (type.FullName == "System.Char")
        {
            field.MarshalInfo = new(NativeType.U2);
        }
    }
}
