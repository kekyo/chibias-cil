/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibicc.toolchain.Internal;
using chibicc.toolchain.Logging;
using chibicc.toolchain.Parsing;
using chibicc.toolchain.Tokenizing;
using chibild.Internal;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Security.Cryptography;

namespace chibild.Generating;

partial class CodeGenerator
{
    private TypeDefinition CreatePlaceholderType() =>
        CecilUtilities.CreatePlaceholderType(this.placeholderIndex++);

    private FieldDefinition CreatePlaceholderField() =>
        CecilUtilities.CreatePlaceholderField(this.placeholderIndex++);

    private MethodDefinition CreatePlaceholderMethod() =>
        CecilUtilities.CreatePlaceholderMethod(this.placeholderIndex++);

    private Instruction CreatePlaceholderInstruction() =>
        CecilUtilities.CreatePlaceholderInstruction(this.placeholderIndex++);

    //////////////////////////////////////////////////////////////

    private void ConsumeGlobalVariable(
        LookupContext context,
        GlobalVariableNode globalVariable)
    {
        // Check public variable already declared in another fragment.
        if (this.ContainsPriorityVariableDeclaration(
            context,
            globalVariable.Name,
            out var declaredFragment) &&
            declaredFragment != context.CurrentFragment)
        {
            this.OutputTrace(
                globalVariable.Name.Token,
                $"Declaration ignored, because already declared in: {declaredFragment.ObjectPath}");
            return;
        }
        
        var fa = globalVariable.Scope.Scope switch
        {
            Scopes.Public => FieldAttributes.Public | FieldAttributes.Static,
            Scopes.File => FieldAttributes.Public | FieldAttributes.Static,
            _ => FieldAttributes.Assembly | FieldAttributes.Static,
        };

        var field = new FieldDefinition(
            globalVariable.Name.Identity,
            fa,
            this.CreatePlaceholderType());

        if (globalVariable.InitializingData is var (data, _))
        {
            field.InitialValue = data;
            field.IsInitOnly = true;
        }

        this.DelayLookingUpType(
            context,
            globalVariable.Type,
            false,
            type => CecilUtilities.SetFieldType(
                field,
                context.SafeImport(type)));

        switch (globalVariable.Scope.Scope)
        {
            case Scopes.Public:
            case Scopes.Internal:
                context.AddVariable(field, false);
                break;
            case Scopes.__Module__:
                this.OutputWarning(
                    globalVariable.Scope.Token,
                    $"Ignored variable module scope, will place on internal scope.");
                context.AddVariable(field, false);
                break;
            default:
                context.AddVariable(field, true);
                break;
        }
    }

    //////////////////////////////////////////////////////////////

    private void ConsumeGlobalConstant(
        LookupContext context,
        GlobalConstantNode globalConstant)
    {
        // Check public variable already declared in another fragment.
        if (this.ContainsPriorityVariableDeclaration(
            context,
            globalConstant.Name,
            out var declaredFragment) &&
            declaredFragment != context.CurrentFragment)
        {
            this.OutputTrace(
                globalConstant.Name.Token,
                $"Declaration ignored, because already declared in: {declaredFragment.ObjectPath}");
            return;
        }
        
        var fa = globalConstant.Scope.Scope switch
        {
            Scopes.Public => FieldAttributes.Public | FieldAttributes.Static | FieldAttributes.InitOnly,
            Scopes.File => FieldAttributes.Public | FieldAttributes.Static | FieldAttributes.InitOnly,
            _ => FieldAttributes.Assembly | FieldAttributes.Static | FieldAttributes.InitOnly,
        };

        var field = new FieldDefinition(
            globalConstant.Name.Identity,
            fa,
            this.CreatePlaceholderType());

        field.InitialValue = globalConstant.InitializingData.Data;

        this.DelayLookingUpType(
            context,
            globalConstant.Type,
            false,
            fieldType =>
            {
                var isConstTypeIdentity = new IdentityNode(
                    globalConstant.Type.Token.AsText(
                        "System.Runtime.CompilerServices.IsConst"));

                this.DelayLookingUpType(
                    context,
                    isConstTypeIdentity,
                    false,
                    isConstType =>
                    {
                        var modifiedType = context.SafeImport(fieldType).
                            MakeOptionalModifierType(
                                context.SafeImport(isConstType));

                        CecilUtilities.SetFieldType(
                            field,
                            modifiedType);
                    },
                    () =>
                    {
                        CecilUtilities.SetFieldType(
                            field,
                            context.SafeImport(fieldType));

                        this.OutputWarning(
                            isConstTypeIdentity.Token,
                            $"{isConstTypeIdentity} was not found, so not applied. Because maybe did not reference core library.");
                    });
            });

        switch (globalConstant.Scope.Scope)
        {
            case Scopes.Public:
            case Scopes.Internal:
                context.AddConstant(field, false);
                break;
            case Scopes.__Module__:
                this.OutputWarning(
                    globalConstant.Scope.Token,
                    $"Ignored constant module scope, will place on internal scope.");
                context.AddConstant(field, false);
                break;
            default:
                context.AddConstant(field, true);
                break;
        }
    }

    //////////////////////////////////////////////////////////////

    private MethodDefinition SetupMethodDefinition(
        string functionName,
        TypeReference returnType,
        ParameterDefinition[] parameters,
        MethodAttributes attribute)
    {
        var method = new MethodDefinition(
            functionName,
            attribute,
            returnType);
        method.HasThis = false;

        foreach (var parameter in parameters)
        {
            method.Parameters.Add(parameter);
        }

        var body = method.Body;
        body.InitLocals = false;   // Derived C behavior.

        return method;
    }

    //////////////////////////////////////////////////////////////

    private void ConsumeLocalVariables(
        LookupContext context,
        MethodDefinition method,
        LocalVariableNode[] localVariables,
        Dictionary<string, VariableDebugInformation> vdis)
    {
        var variables = method.Body.Variables;

        foreach (var localVariable in localVariables)
        {
            var variable = new VariableDefinition(
                this.CreatePlaceholderType());

            this.DelayLookingUpType(
                context,
                localVariable.Type,
                false,
                type =>
                {
                    // Mark pinned.
                    if (type.IsByReference)
                    {
                        var resolved = type.Resolve();
                        var elementType = resolved.GetElementType();
                        if (elementType.IsValueType)
                        {
                            variable.VariableType = context.SafeImport(
                                type).
                                MakePinnedType();
                            return;
                        }
                    }
                    variable.VariableType =
                        context.SafeImport(type);
                });

            if (localVariable.Name is { } name)
            {
                var vdi = new VariableDebugInformation(
                    variable, name.Identity);

                vdis[name.Identity] = vdi;
            }

            variables.Add(variable);
        }
    }

    //////////////////////////////////////////////////////////////

    private void DelayInsertingSequencePoint(
        MethodDefinition method,
        Instruction instruction,
        Location location)
    {
        lock (this.delayDebuggingInsertionEntries)
        {
            this.delayDebuggingInsertionEntries.
                Enqueue((cachedDocuments, isEmbeddingSourceFile) =>
            {
                if (!cachedDocuments.TryGetValue(location.File.RelativePath, out var document))
                {
                    var documentPath = location.File.BasePath is { } basePath ?
                        Path.Combine(basePath, location.File.RelativePath) :
                        location.File.RelativePath;

                    document = new Document(documentPath);

                    document.Type = DocumentType.Text;
                    document.HashAlgorithm = DocumentHashAlgorithm.None;
                    
                    if (location.File.Language is { } language)
                    {
                        document.Language = (DocumentLanguage)language;
                    }

                    try
                    {
                        if (File.Exists(documentPath))
                        {
                            var content = File.ReadAllBytes(documentPath);

                            using var sha1 = SHA1.Create();
                            var hash = sha1.ComputeHash(content);
                            
                            document.HashAlgorithm = DocumentHashAlgorithm.SHA1;
                            document.Hash = hash;
                            
                            this.logger.Trace($"Emit source file hash: {documentPath}");

                            if (isEmbeddingSourceFile)
                            {
                                document.EmbeddedSource = content;
                                this.logger.Trace($"Embedded source file: {documentPath}");
                            }
                        }
                    }
                    catch (Exception ex)
                    {
                        this.logger.Warning($"Could not compute source file hash: {documentPath}, {ex.Message}");
                    }

                    cachedDocuments.Add(location.File.RelativePath, document);
                }

                var sequencePoint = new SequencePoint(
                    instruction, document);

                sequencePoint.StartLine = (int)(location.StartLine + 1);
                sequencePoint.StartColumn = (int)(location.StartColumn + 1);
                sequencePoint.EndLine = (int)(location.EndLine + 1);
                sequencePoint.EndColumn = (int)(location.EndColumn + 1);

                method.DebugInformation.SequencePoints.Add(sequencePoint);
            });
        }
    }

    private void ConsumeInstructions(
        LookupContext context,
        MethodDefinition method,
        InstructionNode[] instructions,
        Dictionary<string, VariableDebugInformation> vdis)
    {
        var labelTargets = new Dictionary<string, Instruction>();
        var delayBranchFixups = new Queue<Action>();

        var cilInstructions = method.Body.Instructions;

        foreach (var instruction in instructions)
        {
            var opCode = CecilUtilities.ParseOpCode(instruction.OpCode.Identity);
            Instruction? cilInstruction = null;

            switch (instruction)
            {
                case SingleInstructionNode _:
                    cilInstruction = Instruction.Create(
                        opCode);
                    break;

                case BranchInstructionNode(_, var (labelTarget, labelToken)):
                    cilInstruction = Instruction.Create(
                        opCode,
                        this.CreatePlaceholderInstruction());
                    delayBranchFixups.Enqueue(() =>
                    {
                        if (!labelTargets.TryGetValue(labelTarget, out var targetInstruction))
                        {
                            this.OutputError(
                                labelToken,
                                $"Could not find the label: {labelToken}");
                        }
                        else
                        {
                            cilInstruction.Operand = targetInstruction;
                        }
                    });
                    break;

                case FieldInstructionNode(_, var field):
                    cilInstruction = Instruction.Create(
                        opCode,
                        this.CreatePlaceholderField());
                    this.DelayLookingUpField(
                        context,
                        field,
                        false,
                        f => cilInstruction.Operand =
                            context.SafeImport(f));
                    break;

                case TypeInstructionNode(_, var type):
                    cilInstruction = Instruction.Create(
                        opCode,
                        this.CreatePlaceholderType());
                    this.DelayLookingUpType(
                        context,
                        type,
                        false,
                        t => cilInstruction.Operand =
                            context.SafeImport(t));
                    break;

                case MetadataTokenInstructionNode(_, var identity, var signature):
                    cilInstruction = Instruction.Create(
                        opCode,
                        this.CreatePlaceholderType());
                    this.DelayLookingUpMember(
                        context,
                        identity,
                        signature,
                        false,
                        m => cilInstruction.Operand =
                            context.SafeImport(m));
                    break;

                case NumericValueInstructionNode(_, var (value, _)):
                    cilInstruction = value switch
                    {
                        byte v => Instruction.Create(opCode, v),
                        sbyte v => Instruction.Create(opCode, v),
                        short v => Instruction.Create(opCode, v),
                        ushort v => Instruction.Create(opCode, v),
                        int v => Instruction.Create(opCode, v),
                        uint v => Instruction.Create(opCode, v),
                        long v => Instruction.Create(opCode, v),
                        ulong v => Instruction.Create(opCode, v),
                        float v => Instruction.Create(opCode, v),
                        double v => Instruction.Create(opCode, v),
                        _ => throw new InvalidOperationException(),
                    };
                    break;

                case StringValueInstructionNode(_, var (text, _), _):
                    cilInstruction = Instruction.Create(
                        opCode,
                        text);
                    break;

                case IndexReferenceInstructionNode(_, var isArgumentIndirection, var (variableIndex, variableIndexToken)):
                    var index = Convert.ToInt32(variableIndex);
                    if (isArgumentIndirection)
                    {
                        if (index >= method.Parameters.Count)
                        {
                            this.OutputError(
                                variableIndexToken,
                                $"Could not find the variable: {variableIndexToken}");
                        }
                        else
                        {
                            cilInstruction = Instruction.Create(
                                opCode,
                                method.Parameters[index]);
                        }
                    }
                    else
                    {
                        if (index >= method.Body.Variables.Count)
                        {
                            this.OutputError(
                                variableIndexToken,
                                $"Could not find the variable: {variableIndexToken}");
                        }
                        else
                        {
                            cilInstruction = Instruction.Create(
                                opCode,
                                method.Body.Variables[index]);
                        }
                    }
                    break;

                case NameReferenceInstructionNode(_, var isArgumentIndirection, var (variableName, variableNameToken)):
                    if (isArgumentIndirection)
                    {
                        if (method.Parameters.FirstOrDefault(p => p.Name == variableName) is not { } parameter)
                        {
                            this.OutputError(
                                variableNameToken,
                                $"Could not find the variable: {variableNameToken}");
                        }
                        else
                        {
                            cilInstruction = Instruction.Create(
                                opCode,
                                parameter);
                        }
                    }
                    else
                    {
                        if (!vdis.TryGetValue(variableName, out var vdi))
                        {
                            this.OutputError(
                                variableNameToken,
                                $"Could not find the variable: {variableNameToken}");
                        }
                        else
                        {
                            cilInstruction = Instruction.Create(
                                opCode,
                                method.Body.Variables[vdi.Index]);
                        }
                    }
                    break;

                case CallInstructionNode(_, var function, var signature):
                    cilInstruction = Instruction.Create(
                        opCode,
                        this.CreatePlaceholderMethod());
                    this.DelayLookingUpMethod(
                        context,
                        function,
                        signature,
                        false,
                        (m, ptrs) =>
                        {
                            // Will construct specialized method reference with variadic parameters.
                            if (m.CallingConvention == Mono.Cecil.MethodCallingConvention.VarArg)
                            {
                                if (signature == null)
                                {
                                    this.OutputError(
                                        function.Token,
                                        $"Could not call variadic function without signature: {function}, Target={m.FullName}");
                                    return;
                                }
                                
                                var emr = new MethodReference(
                                    m.Name,
                                    context.SafeImport(m.ReturnType),
                                    context.SafeImport(m.DeclaringType));
                                emr.CallingConvention = Mono.Cecil.MethodCallingConvention.VarArg;
                                foreach (var parameter in m.Parameters)
                                {
                                    var pt = context.SafeImport(parameter.ParameterType);
                                    emr.Parameters.Add(new(
                                        parameter.Name,
                                        parameter.Attributes,
                                        pt));
                                }

                                // Append sentinel parameters each lack parameter types.
                                var first = true;
                                foreach (var parameterType in
                                    ptrs.Skip(m.Parameters.Count))
                                {
                                    var pt = context.SafeImport(parameterType);
                                    if (first)
                                    {
                                        // Mark only first sentinel.
                                        emr.Parameters.Add(new(pt.MakeSentinelType()));
                                        first = false;
                                    }
                                    else
                                    {
                                        emr.Parameters.Add(new(pt));
                                    }
                                }
                                m = emr;
                            }
                            cilInstruction.Operand =
                                context.SafeImport(m);
                        });
                    break;

                case SignatureInstructionNode(_, var signature):
                    var callSite = new CallSite(this.CreatePlaceholderType())
                    {
                        CallingConvention = signature.CallingConvention switch
                        {
                            chibicc.toolchain.Parsing.MethodCallingConvention.VarArg =>
                                Mono.Cecil.MethodCallingConvention.VarArg,
                            _ =>
                                Mono.Cecil.MethodCallingConvention.Default,
                        },
                        HasThis = false,
                        ExplicitThis = false,
                    };

                    cilInstruction = Instruction.Create(
                        opCode,
                        callSite);

                    this.DelayLookingUpType(
                        context,
                        signature.ReturnType,
                        false,
                        rt => callSite.ReturnType =
                            context.SafeImport(rt));

                    foreach (var (parameterType, parameterName, _) in signature.Parameters)
                    {
                        var parameter = new ParameterDefinition(
                            this.CreatePlaceholderType());
                        parameter.Name = parameterName;   // Not sure if that makes sense.
                        callSite.Parameters.Add(parameter);

                        this.DelayLookingUpType(
                            context,
                            parameterType,
                            false,
                            pt => parameter.ParameterType =
                                context.SafeImport(pt));
                    }
                    break;

                default:
                    throw new InvalidOperationException();
            }

            if (cilInstruction != null)
            {
                // Assign labels.
                foreach (var label in instruction.Labels)
                {
                    labelTargets.Add(label.Name, cilInstruction);
                }

                // Schedule injecting sequence point.
                if (this.produceDebuggingInformation &&
                    instruction.Location is { File.IsVisible: true } location)
                {
                    this.DelayInsertingSequencePoint(
                        method,
                        cilInstruction,
                        location);
                }

                cilInstructions.Add(cilInstruction);
            }
        }

        // Fixup branch targettings.
        while (delayBranchFixups.Count >= 1)
        {
            var fixup = delayBranchFixups.Dequeue();
            fixup();
        }
    }

    //////////////////////////////////////////////////////////////

    private void DelayInsertingVariableNames(
        MethodDefinition method,
        Dictionary<string, VariableDebugInformation> vdis)
    {
        // As a confusing CIL assembly specification,
        // the naming of local variables is part of the debugging information, NOT metadata.
        // This specification is not a defect in the CIL assembly,
        // but rather because the naming of local variables
        // must be tied to the scope of the instruction range.
        // (And local variable names are not required for CLR execution.）
        // Since chibild does not handle the strict scope of local variable names,
        // it simply ignores the scope and adds them.
        // In the future, it would be desirable
        // to extend the `.local` directive so that a range can be specified.

        lock (this.delayDebuggingInsertionEntries)
        {
            this.delayDebuggingInsertionEntries.Enqueue((_, _) =>
            {
                var scope = new ScopeDebugInformation(
                    method.Body.Instructions.First(),
                    method.Body.Instructions.Last());

                var variables = scope.Variables;
                foreach (var vdi in vdis.Values)
                {
                    variables.Add(vdi);
                }

                method.DebugInformation.Scope = scope;
            });
        }
    }

    private void ConsumeFunction(
        LookupContext context,
        FunctionDeclarationNode function)
    {
        var ma = function.Scope.Scope switch
        {
            Scopes.Public => MethodAttributes.Public | MethodAttributes.Static,
            Scopes.File => MethodAttributes.Public | MethodAttributes.Static,
            _ => MethodAttributes.Assembly | MethodAttributes.Static,
        };

        var parameters = function.Signature.Parameters.
            Select(p =>
            {
                var pd = new ParameterDefinition(this.CreatePlaceholderType());
                pd.Name = p.ParameterName;
                return pd;
            }).
            ToArray();

        var method = this.SetupMethodDefinition(
            function.Name.Identity,
            this.CreatePlaceholderType(),
            parameters,
            ma);

        method.CallingConvention = function.Signature.CallingConvention switch
        {
            chibicc.toolchain.Parsing.MethodCallingConvention.VarArg =>
                Mono.Cecil.MethodCallingConvention.VarArg,
            _ =>
                Mono.Cecil.MethodCallingConvention.Default,
        };

        this.DelayLookingUpType(
            context,
            function.Signature.ReturnType,
            false,
            rt =>
            {
                method.ReturnType =
                    context.SafeImport(rt);

                // Special case: Force 1 byte footprint on boolean type.
                if (rt.FullName == "System.Boolean")
                {
                    method.MethodReturnType.MarshalInfo = new(NativeType.U1);
                }
                else if (rt.FullName == "System.Char")
                {
                    method.MethodReturnType.MarshalInfo = new(NativeType.U2);
                }
            });

        for (var index = 0; index < function.Signature.Parameters.Length; index++)
        {
            var capturedParameter = method.Parameters[index];

            this.DelayLookingUpType(
                context,
                function.Signature.Parameters[index].ParameterType,
                false,
                pt =>
                {
                    capturedParameter.ParameterType =
                        context.SafeImport(pt);

                    // Special case: Force 1 byte footprint on boolean type.
                    if (pt.FullName == "System.Boolean")
                    {
                        capturedParameter.MarshalInfo = new(NativeType.U1);
                    }
                    else if (pt.FullName == "System.Char")
                    {
                        capturedParameter.MarshalInfo = new(NativeType.U2);
                    }
                });
        }

        var vdis = new Dictionary<string, VariableDebugInformation>();

        this.ConsumeLocalVariables(
            context,
            method,
            function.LocalVariables,
            vdis);

        this.ConsumeInstructions(
            context,
            method,
            function.Instructions,
            vdis);

        if (this.produceDebuggingInformation &&
            vdis.Count >= 1)
        {
            this.DelayInsertingVariableNames(method, vdis);
        }

        switch (function.Scope.Scope)
        {
            case Scopes.Public:
            case Scopes.Internal:
                context.AddFunction(method, false);
                break;
            case Scopes.__Module__:
                context.AddModuleFunction(method);
                break;
            default:
                context.AddFunction(method, true);
                break;
        }
    }

    //////////////////////////////////////////////////////////////

    internal static readonly string IntiializerMethodName = "initializer_$";
    
    private void ConsumeInitializer(
        LookupContext context,
        InitializerDeclarationNode initializer)
    {
        var method = this.SetupMethodDefinition(
            IntiializerMethodName,
            this.CreatePlaceholderType(),
            CommonUtilities.Empty<ParameterDefinition>(),
            MethodAttributes.Private | MethodAttributes.Static);
        method.ReturnType = context.FallbackModule.TypeSystem.Void;

        var vdis = new Dictionary<string, VariableDebugInformation>();

        this.ConsumeLocalVariables(
            context,
            method,
            initializer.LocalVariables,
            vdis);

        this.ConsumeInstructions(
            context,
            method,
            initializer.Instructions,
            vdis);

        if (this.produceDebuggingInformation &&
            vdis.Count >= 1)
        {
            this.DelayInsertingVariableNames(method, vdis);
        }

        switch (initializer.Scope.Scope)
        {
            case Scopes.Public:
            case Scopes.Internal:
                context.AddInitializer(method, false);
                break;
            case Scopes.__Module__:
                this.OutputWarning(
                    initializer.Scope.Token,
                    $"Ignored initializer module scope, will place on internal scope.");
                context.AddInitializer(method, false);
                break;
            default:
                context.AddInitializer(method, true);
                break;
        }
    }

    //////////////////////////////////////////////////////////////

    private void ConsumeEnumeration(
        LookupContext context,
        EnumerationNode enumeration)
    {
        // Check public type already declared in another fragment.
        var r = TypeParser.TryParse(enumeration.Name.Token, out var type);
        Debug.Assert(r);
        if (this.ContainsPriorityTypeDeclaration(
            context, type,
            out var declaredFragment) &&
            declaredFragment != context.CurrentFragment)
        {
            this.OutputTrace(
                enumeration.Name.Token,
                $"Declaration ignored, because already declared in: {declaredFragment.ObjectPath}");
            return;
        }
        
        var typeAttributes = enumeration.Scope.Scope switch
        {
            Scopes.Public => TypeAttributes.Public | TypeAttributes.Sealed,
            Scopes.Internal => TypeAttributes.NotPublic | TypeAttributes.Sealed,
            _ => TypeAttributes.NestedPublic | TypeAttributes.Sealed,
        };

        //if (!context.UnsafeGetCoreType("System.Enum", out var setr))
        //{
        //    setr = this.CreatePlaceholderType();
        //    this.OutputError(
        //        enumeration.Token,
        //        $"Could not find System.Enum type.");
        //}

        var enumerationType = new TypeDefinition(
            enumeration.Scope.Scope switch
            {
                Scopes.Public => "C.type",
                Scopes.Internal => "C.type",
                Scopes.__Module__ => "C.type",
                _ => "",
            },
            enumeration.Name.Identity,
            typeAttributes,
            // HACK: This is immitation TypeReference which is value type supplier.
            //   Will be replaced strict imported TypeReference.
            new TypeReference("System", "Enum", context.FallbackModule, context.FallbackModule, true));

        this.DelayLookingUpType(
            context,
            TypeParser.UnsafeParse<TypeNode>(Token.Identity("System.Enum")),
            false,
            etr => enumerationType.BaseType = context.SafeImport(etr));

        this.DelayLookingUpType(
            context,
            enumeration.UnderlyingType,
            false,
            utr =>
            {
                var ut = context.SafeImport(utr);

                var enumerationValueField = new FieldDefinition(
                    "value__",
                    FieldAttributes.Public | FieldAttributes.SpecialName | FieldAttributes.RTSpecialName,
                    ut);
                enumerationType.Fields.Add(enumerationValueField);

                foreach (var enumValue in enumeration.Values)
                {
                    var field = new FieldDefinition(
                        enumValue.Name.Identity,
                        FieldAttributes.Public | FieldAttributes.Static | FieldAttributes.Literal,
                        ut);
                    field.Constant = enumValue.Value!.Value;

                    enumerationType.Fields.Add(field);
                }
            });

        // 'Try' add the type, sometimes failed adding and will be ignored.
        // Because this context in parallel.
        switch (enumeration.Scope.Scope)
        {
            case Scopes.Public:
            case Scopes.Internal:
                context.TryAddEnumeration(enumerationType, false);
                break;
            case Scopes.__Module__:
                this.OutputWarning(
                    enumeration.Scope.Token,
                    $"Ignored enumeration module scope, will place on internal scope.");
                context.TryAddEnumeration(enumerationType, false);
                break;
            default:
                context.TryAddEnumeration(enumerationType, true);
                break;
        }
    }

    //////////////////////////////////////////////////////////////

    private void ConsumeStructure(
        LookupContext context,
        StructureNode structure)
    {
        // Check public type already declared in another fragment.
        var r = TypeParser.TryParse(structure.Name.Token, out var type);
        Debug.Assert(r);
        if (this.ContainsPriorityTypeDeclaration(
            context,
            type,
            out var declaredFragment) &&
            declaredFragment != context.CurrentFragment)
        {
            this.OutputTrace(
                structure.Name.Token,
                $"Declaration ignored, because already declared in: {declaredFragment.ObjectPath}");
            return;
        }
        
        var typeAttributes = structure.Scope.Scope switch
        {
            Scopes.Public => TypeAttributes.Public | TypeAttributes.Sealed,
            Scopes.Internal => TypeAttributes.NotPublic | TypeAttributes.Sealed,
            _ => TypeAttributes.NestedPublic | TypeAttributes.Sealed,
        };

        typeAttributes |= (structure.IsExplicit?.Value ?? false) ?
            TypeAttributes.ExplicitLayout : TypeAttributes.SequentialLayout;

        //if (!context.UnsafeGetCoreType("System.ValueType", out var vttr))
        //{
        //    vttr = this.CreatePlaceholderType();
        //    this.OutputError(
        //        structure.Token,
        //        $"Could not find System.ValueType type.");
        //}

        var structureType = new TypeDefinition(
            structure.Scope.Scope switch
            {
                Scopes.Public => "C.type",
                Scopes.Internal => "C.type",
                Scopes.__Module__ => "C.type",
                _ => "",
            },
            structure.Name.Identity,
            typeAttributes,
            // HACK: This is immitation TypeReference which is value type supplier.
            //   Will be replaced strict imported TypeReference.
            new TypeReference("System", "ValueType", context.FallbackModule, context.FallbackModule, true));

        if (structure.PackSize?.Value is { } packSize)
        {
            structureType.PackingSize = (short)packSize;
            structureType.ClassSize = 0;
        }

        this.DelayLookingUpType(
            context,
            TypeParser.UnsafeParse<TypeNode>(Token.Identity("System.ValueType")),
            false,
            vtr => structureType.BaseType = context.SafeImport(vtr));

        foreach (var structureField in structure.Fields)
        {
            var fieldAttribute = structureField.Scope.Scope switch
            {
                Scopes.Public => FieldAttributes.Public,
                _ => FieldAttributes.Assembly,
            };

            var field = new FieldDefinition(
                structureField.Name.Identity,
                fieldAttribute,
                this.CreatePlaceholderType());

            this.DelayLookingUpType(
                context,
                structureField.Type,
                false,
                ftr => CecilUtilities.SetFieldType(
                    field,
                    context.SafeImport(ftr)));

            if (typeAttributes.HasFlag(TypeAttributes.ExplicitLayout))
            {
                field.Offset = (int)structureField.Offset!.Value;
            }

            structureType.Fields.Add(field);
        }

        // 'Try' add the type, sometimes failed adding and will be ignored.
        // Because this context in parallel.
        switch (structure.Scope.Scope)
        {
            case Scopes.Public:
            case Scopes.Internal:
                context.TryAddStructure(structureType, false);
                break;
            case Scopes.__Module__:
                this.OutputWarning(
                    structure.Scope.Token,
                    $"Ignored structure module scope, will place on internal scope.");
                context.TryAddStructure(structureType, false);
                break;
            default:
                context.TryAddStructure(structureType, true);
                break;
        }
    }
}
