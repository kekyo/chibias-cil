/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibicc.toolchain.Parsing;
using Mono.Cecil;
using Mono.Cecil.Cil;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace chibild.Internal;

internal readonly struct LoadedCAbiMetadata
{
    public readonly AssemblyDefinition Assembly;
    public readonly Dictionary<string, TypeDefinition> Types;
    public readonly Dictionary<string, FieldDefinition> Fields;
    public readonly Dictionary<string, MethodDefinition[]> Methods;

    public LoadedCAbiMetadata(
        AssemblyDefinition assemblyDefinition,
        Dictionary<string, TypeDefinition> types,
        Dictionary<string, FieldDefinition> fields,
        Dictionary<string, MethodDefinition[]> methods)
    {
        this.Assembly = assemblyDefinition;
        this.Types = types;
        this.Fields = fields;
        this.Methods = methods;
    }
}

internal static class CecilUtilities
{
    private static readonly Dictionary<string, OpCode> opCodes =
        CecilDefinition.GetOpCodes();

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

#if DEBUG
    static CecilUtilities()
    {
        var translator = CilParser.GetOpCodeTranslator();
        Debug.Assert(translator.All(t => opCodes.ContainsKey(t.Value)));
    }
#endif

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

    public static OpCode ParseOpCode(
        string word) =>
        opCodes[word];

    public static TypeDefinition CreatePlaceholderType(int postfix) =>
        new("", $"<placeholder_type>_${postfix}",
            TypeAttributes.NotPublic | TypeAttributes.Abstract | TypeAttributes.Sealed);

    public static FieldDefinition CreatePlaceholderField(int postfix) =>
        new($"<placeholder_field>_${postfix}",
            FieldAttributes.Private | FieldAttributes.Static | FieldAttributes.InitOnly,
            CreatePlaceholderType(postfix));

    public static MethodDefinition CreatePlaceholderMethod(int postfix) =>
        new($"<placeholder_method>_${postfix}",
            MethodAttributes.Private | MethodAttributes.Abstract | MethodAttributes.Final,
            CreatePlaceholderType(postfix));

    public static Instruction CreatePlaceholderInstruction(int postfix) =>
        Instruction.Create(OpCodes.Ldc_I4, postfix);

    private sealed class ParameterReferenceComparer :
        IEqualityComparer<ParameterReference>
    {
        public bool Equals(ParameterReference? lhs, ParameterReference? rhs) =>
            lhs is { } && rhs is { } &&
            lhs.ParameterType.FullName.Equals(rhs.ParameterType.FullName);

        public int GetHashCode(ParameterReference obj) =>
            obj.ParameterType.FullName.GetHashCode();

        public static readonly ParameterReferenceComparer Instance = new();
    }

    public static bool Equals(
        MethodReference lhs,
        MethodReference rhs) =>
        lhs.Name == rhs.Name &&
        lhs.ReturnType.FullName == rhs.ReturnType.FullName &&
        (lhs.CallingConvention, rhs.CallingConvention) switch
        {
            (Mono.Cecil.MethodCallingConvention.VarArg, Mono.Cecil.MethodCallingConvention.VarArg) =>
                lhs.Parameters.
                    Take(Math.Min(lhs.Parameters.Count, rhs.Parameters.Count)).
                    SequenceEqual(rhs.Parameters.
                        Take(Math.Min(lhs.Parameters.Count, rhs.Parameters.Count)),
                        ParameterReferenceComparer.Instance),
            (Mono.Cecil.MethodCallingConvention.VarArg, _) when
                lhs.Parameters.Count <= rhs.Parameters.Count =>
                lhs.Parameters.
                    SequenceEqual(rhs.Parameters.
                        Take(lhs.Parameters.Count),
                        ParameterReferenceComparer.Instance),
            (_, Mono.Cecil.MethodCallingConvention.VarArg) when
                rhs.Parameters.Count <= lhs.Parameters.Count =>
                rhs.Parameters.
                    SequenceEqual(lhs.Parameters.
                        Take(rhs.Parameters.Count),
                        ParameterReferenceComparer.Instance),
            _ => lhs.Parameters.
                SequenceEqual(rhs.Parameters,
                    ParameterReferenceComparer.Instance),
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

    public static TypeReference SafeImport(
        this ModuleDefinition targetModule,
        TypeReference tr) =>
        (tr.Module?.Equals(targetModule) ?? tr is TypeDefinition) ?
            tr : targetModule.ImportReference(tr);
        
    public static FieldReference SafeImport(
        this ModuleDefinition targetModule,
        FieldReference fr) =>
        (fr.Module?.Equals(targetModule) ?? fr is FieldDefinition) ?
            fr : targetModule.ImportReference(fr);
        
    public static MethodReference SafeImport(
        this ModuleDefinition targetModule,
        MethodReference mr) =>
        (mr.Module?.Equals(targetModule) ?? mr is MethodDefinition) ?
            mr : targetModule.ImportReference(mr);

    public static MemberReference SafeImport(
        this ModuleDefinition targetModule,
        MemberReference member) =>
        member switch
        {
            TypeReference type => targetModule.SafeImport(type),
            FieldReference field => targetModule.SafeImport(field),
            MethodReference method => targetModule.SafeImport(method),
            _ => throw new InvalidOperationException(),
        };

    public static LoadedCAbiMetadata LoadCAbiMetadataFromAssembly(
        string assemblyPath,
        CachedAssemblyResolver assemblyResolver)
    {
        var assembly = assemblyResolver.ReadAssemblyFrom(
            assemblyPath);

        static IEnumerable<TypeDefinition> IterateTypesDescendants(TypeDefinition type)
        {
            yield return type;

            foreach (var childType in type.NestedTypes.Where(nestedType =>
                nestedType.IsNestedPublic &&
                (nestedType.IsClass || nestedType.IsInterface || nestedType.IsValueType || nestedType.IsEnum) &&
                // Excepts all generic types because CABI does not support it.
                !nestedType.HasGenericParameters).
                SelectMany(IterateTypesDescendants))
            {
                yield return childType;
            }
        }

        var targetTypes = assembly.Modules.
            SelectMany(module => module.Types).
            Where(type =>
                type.IsPublic &&
                (type.IsClass || type.IsInterface || type.IsValueType || type.IsEnum) &&
                // Excepts all generic types because CABI does not support it.
                !type.HasGenericParameters).
            SelectMany(IterateTypesDescendants).
            ToArray();

        var types = targetTypes.
            // Combine both CABI types and .NET types.
            Where(type => type.Namespace is "C.type").
            Select(type => (name: type.Name, type)).
            Concat(targetTypes.
                Select(type => (name: type.FullName.Replace('/', '.'), type))).
            ToDictionary(entry => entry.name, entry => entry.type);

        var targetFields = targetTypes.
            Where(type => type is
            {
                IsPublic: true, IsClass: true,
            } or
            {
                IsPublic: true, IsValueType: true, IsEnum: false,
            }).
            SelectMany(type => type.Fields).
            Where(field => field is
            {
                IsPublic: true,
            }).
            ToArray();

        var fields = targetFields.
            // Combine both CABI variables and .NET fields.
            Where(field => field.DeclaringType.FullName is "C.data" or "C.rdata").
            Select(field => (name: field.Name, field)).
            Concat(targetFields.
                Select(field => (name: $"{field.DeclaringType.FullName}.{field.Name}", field))).
            ToDictionary(entry => entry.name, entry => entry.field);

        var targetMethods = targetTypes.
            Where(type => type is
            {
                IsPublic: true, IsClass: true,
            } or
            {
                IsPublic: true, IsValueType: true,
            }).
            SelectMany(type => type.Methods).
            Where(method => method is
            {
                IsPublic: true,
                // Excepts all generic methods because CABI does not support it.
                HasGenericParameters: false
            }).
            ToArray();

        var methods = targetMethods.
            // Combine both CABI function and .NET methods.
            Where(method =>
                method.IsStatic &&
                method.DeclaringType.FullName is "C.text").
            Select(method => (name: method.Name, method)).
            Concat(targetMethods.
                Select(method =>
                (
                    name: $"{method.DeclaringType.FullName}.{method.Name}",
                    method
                ))).
            GroupBy(
                entry => entry.name,
                entry => entry.method).
            ToDictionary(
                g => g.Key,
                // Sorted descending longer parameters.
                g => g.OrderByDescending(method => method.Parameters.Count).ToArray());

        return new(assembly, types, fields, methods);
    }
}
