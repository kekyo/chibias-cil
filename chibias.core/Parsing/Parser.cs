/////////////////////////////////////////////////////////////////////////////////////
//
// chibias-cil - The specialized backend CIL assembler for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibias.Internal;
using chibias.Parsing.Embedding;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Rocks;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;

namespace chibias.Parsing;

internal sealed partial class Parser
{
    private static readonly FileDescriptor unknown = 
        new(null, "unknown", DocumentLanguage.Other, true);

    private readonly ILogger logger;
    private readonly ModuleDefinition module;
    private readonly TypeDefinition cabiTextType;
    private readonly TypeDefinition cabiDataType;
    private readonly TypeDefinition cabiRDataType;
    private readonly MethodDefinition cabiDataTypeInitializer;
    private readonly MemberDictionary<MemberReference> cabiSpecificSymbols;
    private readonly MemberDictionary<TypeDefinition> referenceTypes;
    private readonly Dictionary<string, TypeReference> importantTypes = new();
    private readonly Dictionary<string, Instruction> labelTargets = new();
    private readonly Dictionary<string, FileDescriptor> files = new();
    private readonly Dictionary<Instruction, Location> locationByInstructions = new();
    private readonly List<string> willApplyLabelingNames = new();
    private readonly Queue<Action> delayedLookupBranchTargetActions = new();
    private readonly Queue<Action> delayedLookupTypeActions = new();
    private readonly Queue<Action> delayedLookupLocalMemberActions = new();
    private readonly Queue<Action> delayedCheckAfterLookingupActions = new();
    private readonly Dictionary<string, List<VariableDebugInformation>> variableDebugInformationLists = new();
    private readonly Lazy<TypeReference> systemValueTypeType;
    private readonly Lazy<TypeReference> systemEnumType;
    private readonly Lazy<MethodReference> indexOutOfRangeCtor;
    private readonly bool produceExecutable;
    private readonly bool produceDebuggingInformation;

    private int placeholderIndex;
    private FileDescriptor currentCilFile;
    private FileDescriptor currentFile;
    private Location? queuedLocation;
    private Location? lastLocation;
    private bool isProducedOriginalSourceCodeLocation = true;
    private TypeDefinition fileScopedType;
    private MethodDefinition fileScopedTypeInitializer;
    private MethodDefinition? method;
    private MethodBody? body;
    private ICollection<Instruction>? instructions;
    private TypeDefinition? structureType;
    private TypeDefinition? enumerationType;
    private TypeReference? enumerationUnderlyingType;
    private EnumerationMemberValueManipulator? enumerationManipulator;
    private int checkingMemberIndex = -1;
    private int initializerIndex = 0;
    private bool caughtError;

    /////////////////////////////////////////////////////////////////////

    public Parser(
        ILogger logger,
        ModuleDefinition module,
        MemberDictionary<MemberReference> cabiSpecificSymbols,
        TypeDefinitionCache referenceTypes,
        bool produceExecutable,
        bool produceDebuggingInformation,
        ModuleDefinition? mergeOriginModule)
    {
        this.logger = logger;
        
        this.module = module;
        this.cabiSpecificSymbols = cabiSpecificSymbols;
        this.referenceTypes = new(
            this.logger,
            referenceTypes.OfType<TypeDefinition>(),
            type => type.FullName.Replace('/', '.'));
        this.produceExecutable = produceExecutable;
        this.produceDebuggingInformation = produceDebuggingInformation;

        this.importantTypes.Add("System.Object", this.module.TypeSystem.Object);
        this.importantTypes.Add("System.Void", this.module.TypeSystem.Void);
        this.importantTypes.Add("System.Byte", this.module.TypeSystem.Byte);
        this.importantTypes.Add("System.SByte", this.module.TypeSystem.SByte);
        this.importantTypes.Add("System.Int16", this.module.TypeSystem.Int16);
        this.importantTypes.Add("System.UInt16", this.module.TypeSystem.UInt16);
        this.importantTypes.Add("System.Int32", this.module.TypeSystem.Int32);
        this.importantTypes.Add("System.UInt32", this.module.TypeSystem.UInt32);
        this.importantTypes.Add("System.Int64", this.module.TypeSystem.Int64);
        this.importantTypes.Add("System.UInt64", this.module.TypeSystem.UInt64);
        this.importantTypes.Add("System.Single", this.module.TypeSystem.Single);
        this.importantTypes.Add("System.Double", this.module.TypeSystem.Double);
        this.importantTypes.Add("System.Boolean", this.module.TypeSystem.Boolean);
        this.importantTypes.Add("System.String", this.module.TypeSystem.String);
        this.importantTypes.Add("System.IntPtr", this.module.TypeSystem.IntPtr);
        this.importantTypes.Add("System.UIntPtr", this.module.TypeSystem.UIntPtr);
        this.importantTypes.Add("System.TypedReference", this.module.TypeSystem.TypedReference);

        this.systemValueTypeType = new(() =>
            this.UnsafeGetType("System.ValueType"));
        this.systemEnumType = new(() =>
            this.UnsafeGetType("System.Enum"));
        this.indexOutOfRangeCtor = new(() =>
            this.UnsafeGetMethod(
                "System.IndexOutOfRangeException..ctor",
                TypeParser.UnsafeParse<FunctionSignatureNode>("void()")));

        this.cabiTextType = mergeOriginModule?.GetType("C", "text") ??
            new TypeDefinition(
                "C",
                "text",
                TypeAttributes.Public | TypeAttributes.Abstract | TypeAttributes.Sealed |
                TypeAttributes.Class,
                this.module.TypeSystem.Object);
        this.cabiDataType = mergeOriginModule?.GetType("C", "data") ??
            new TypeDefinition(
                "C",
                "data",
                TypeAttributes.Public | TypeAttributes.Abstract | TypeAttributes.Sealed |
                TypeAttributes.Class | TypeAttributes.BeforeFieldInit,
                this.module.TypeSystem.Object);
        this.cabiRDataType = mergeOriginModule?.GetType("C", "rdata") ??
            new TypeDefinition(
                "C",
                "rdata",
                TypeAttributes.Public | TypeAttributes.Abstract | TypeAttributes.Sealed |
                TypeAttributes.Class | TypeAttributes.BeforeFieldInit,
                this.module.TypeSystem.Object);

        this.cabiDataTypeInitializer = this.CreateTypeInitializer();

        this.currentCilFile = unknown;
        this.currentFile = unknown;
        this.fileScopedType = this.CreateFileScopedType(
             CecilUtilities.SanitizeFileNameToMemberName(unknown.RelativePath));
        this.fileScopedTypeInitializer = this.CreateTypeInitializer();
    }

    /////////////////////////////////////////////////////////////////////

    private TypeDefinition CreateFileScopedType(string typeName) => new(
        "",
        $"<{typeName}>$",
        TypeAttributes.NotPublic | TypeAttributes.Abstract | TypeAttributes.Sealed |
        TypeAttributes.BeforeFieldInit | TypeAttributes.Class,
        this.module.TypeSystem.Object);

    private MethodDefinition CreateTypeInitializer() => new(
        ".cctor",
        MethodAttributes.Private | MethodAttributes.Static |
        MethodAttributes.SpecialName | MethodAttributes.RTSpecialName | MethodAttributes.HideBySig,
        this.module.TypeSystem.Void);

    private void BeginNewFileScope(string sourcePathDebuggerHint)
    {
        // Add latest file scoped type.
        if ((this.fileScopedType.Methods.Count >= 1 ||
            this.fileScopedType.Fields.Count >= 1 ||
            this.fileScopedType.NestedTypes.Count >= 1) &&
            !this.module.Types.Contains(this.fileScopedType))
        {
            // Schedule checking for type initializer when all initializer was applied.
            var capturedFileScopedType = this.fileScopedType;
            var capturedFileScopedTypeInitializer = this.fileScopedTypeInitializer;
            this.delayedCheckAfterLookingupActions.Enqueue(() =>
            {
                // Add type initializer when body was appended.
                if (capturedFileScopedTypeInitializer.Body is { } body &&
                    body.Instructions is { } instructions &&
                    instructions.Count >= 1)
                {
                    instructions.Add(Instruction.Create(OpCodes.Ret));
                    body.InitLocals = false;
                    capturedFileScopedType.Methods.Add(capturedFileScopedTypeInitializer);
                }
            });

            this.module.Types.Add(this.fileScopedType);
        }

        // Lookup exist type or create new type.
        var typeName = CecilUtilities.SanitizeFileNameToMemberName(sourcePathDebuggerHint);
        if (this.module.Types.FirstOrDefault(type => type.Name == typeName) is { } type)
        {
            this.fileScopedType = type;
            if (this.fileScopedType.Methods.
                FirstOrDefault(m => m.IsConstructor && m.IsStatic) is { } cctor)
            {
                this.fileScopedTypeInitializer = cctor;
            }
        }
        else
        {
            this.fileScopedType = this.CreateFileScopedType(typeName);
            this.fileScopedTypeInitializer = this.CreateTypeInitializer();
        }
    }

    public void BeginNewCilSourceCode(
        string? basePath,
        string sourcePathDebuggerHint,
        bool isVisible)
    {
        this.files.Clear();

        this.BeginNewFileScope(sourcePathDebuggerHint);

        this.currentCilFile = new(
            basePath != null ? Path.GetFullPath(basePath) : null,
            sourcePathDebuggerHint,
            DocumentLanguage.Cil,
            isVisible);
        this.currentFile = this.currentCilFile;

        this.isProducedOriginalSourceCodeLocation = true;
        this.queuedLocation = null;
        this.lastLocation = null;
    }

    /////////////////////////////////////////////////////////////////////

    private void OutputError(Token token, string message)
    {
        this.caughtError = true;
        this.logger.Error($"{this.currentCilFile.RelativePath}:{token.Line + 1}:{token.StartColumn + 1}: {message}");
    }

    private void OutputError(Location location, string message)
    {
        this.caughtError = true;
        this.logger.Error($"{location.File.RelativePath}:{location.StartLine + 1}:{location.StartColumn + 1}: {message}");
    }

    private void OutputWarning(Token token, string message)
    {
        this.caughtError = true;
        this.logger.Warning($"{this.currentCilFile.RelativePath}:{token.Line + 1}:{token.StartColumn + 1}: {message}");
    }

    /////////////////////////////////////////////////////////////////////

    private TypeDefinition CreateDummyType() =>
        CecilUtilities.CreateDummyType(this.placeholderIndex++);

    private FieldDefinition CreateDummyField() =>
        CecilUtilities.CreateDummyField(this.placeholderIndex++);

    private MethodDefinition CreateDummyMethod() =>
        CecilUtilities.CreateDummyMethod(this.placeholderIndex++);

    /////////////////////////////////////////////////////////////////////

    private void ParseLabel(Token token)
    {
        if (this.instructions == null)
        {
            this.OutputError(token, $"Function directive is not defined.");
        }
        else
        {
            this.willApplyLabelingNames.Add(token.Text);
        }
    }

    /////////////////////////////////////////////////////////////////////

    public void Parse(Token[] tokens)
    {
        if (tokens.FirstOrDefault() is { } token0)
        {
            switch (token0.Type)
            {
                // Is it an assembler directive?
                case TokenTypes.Directive:
                    this.ParseDirective(token0, tokens);
                    break;
                // Is it a label?
                case TokenTypes.Label:
                    this.ParseLabel(token0);
                    break;
                // Is it an OpCode?
                case TokenTypes.Identity
                    when this.instructions != null &&
                         CecilUtilities.TryParseOpCode(token0.Text, out var opCode):
                    this.ParseInstruction(opCode, tokens);
                    break;
                // Is it an enumeration member?
                case TokenTypes.Identity
                    when this.enumerationType != null:
                    this.ParseEnumerationMember(tokens);
                    break;
                // Is it a structure member?
                case TokenTypes.Identity
                    when this.structureType != null:
                    this.ParseStructureMember(tokens);
                    break;
                // Other, invalid syntax.
                default:
                    this.OutputError(token0, $"Invalid syntax.");
                    break;
            }
        }
    }

    /////////////////////////////////////////////////////////////////////

    private void FireDelayedBranchTargetActions()
    {
        while (this.delayedLookupBranchTargetActions.Count >= 1)
        {
            var action = this.delayedLookupBranchTargetActions.Dequeue();
            action();
        }
    }

    private void FinishCurrentState()
    {
        if (this.method != null)
        {
            Debug.Assert(this.instructions != null);
            Debug.Assert(this.body != null);
            Debug.Assert(this.structureType == null);
            Debug.Assert(this.enumerationType == null);

            if (!this.caughtError)
            {
                this.FireDelayedBranchTargetActions();
            }

            this.delayedLookupBranchTargetActions.Clear();
            this.labelTargets.Clear();
            this.willApplyLabelingNames.Clear();
            this.instructions = null;
            this.body = null;
            this.method = null;

            return;
        }

        if (this.enumerationType != null)
        {
            Debug.Assert(this.method == null);
            Debug.Assert(this.instructions == null);
            Debug.Assert(this.body == null);
            Debug.Assert(this.structureType == null);

            Debug.Assert(this.delayedLookupBranchTargetActions.Count == 0);

            if (this.checkingMemberIndex >= 0 &&
                this.checkingMemberIndex < this.enumerationType.Fields.
                Count(f => f.IsPublic && f.IsStatic && f.IsLiteral))
            {
                this.caughtError = true;
                this.logger.Error(
                    $"Enumeration member difference exists before declared type: {this.enumerationType.Name}");
            }

            this.enumerationType = null;
            this.checkingMemberIndex = -1;

            this.enumerationUnderlyingType = null;
            this.enumerationManipulator = null;

            return;
        }
        
        if (this.structureType != null)
        {
            Debug.Assert(this.method == null);
            Debug.Assert(this.instructions == null);
            Debug.Assert(this.body == null);
            Debug.Assert(this.enumerationType == null);
            
            Debug.Assert(this.delayedLookupBranchTargetActions.Count == 0);
            Debug.Assert(this.labelTargets.Count == 0);
            Debug.Assert(this.willApplyLabelingNames.Count == 0);

            if (this.checkingMemberIndex >= 1 &&
                this.checkingMemberIndex < this.structureType.Fields.Count)
            {
                this.caughtError = true;
                this.logger.Error(
                    $"Structure member difference exists before declared type: {this.structureType.Name}");
            }

            this.structureType = null;
            this.checkingMemberIndex = -1;

            return;
        }
    }

    /////////////////////////////////////////////////////////////////////

    private void FireDelayedActions()
    {
        // Fire lookups delayed types.
        while (this.delayedLookupTypeActions.Count >= 1)
        {
            var action = this.delayedLookupTypeActions.Dequeue();
            action();
        }

        // Fire lookups delayed local member.
        while (this.delayedLookupLocalMemberActions.Count >= 1)
        {
            var action = this.delayedLookupLocalMemberActions.Dequeue();
            action();
        }

        // Fire lookups checker.
        while (this.delayedCheckAfterLookingupActions.Count >= 1)
        {
            var action = this.delayedCheckAfterLookingupActions.Dequeue();
            action();
        }
    }

    private void AddFundamentalAttributes(
        TargetFramework targetFramework,
        bool disableJITOptimization)
    {
        // Apply TFA if could be imported.
        if (this.TryGetMethod(
            "System.Runtime.Versioning.TargetFrameworkAttribute..ctor",
            TypeParser.UnsafeParse<FunctionSignatureNode>("void(string)"),
            out var tfactor))
        {
            var tfa = new CustomAttribute(tfactor);
            tfa.ConstructorArguments.Add(new(
                this.UnsafeGetType("System.String"),
                targetFramework.ToString()));
            this.module.Assembly.CustomAttributes.Add(tfa);

            this.logger.Trace(
                $"TargetFrameworkAttribute is applied.");
        }
        else
        {
            this.logger.Warning(
                $"TargetFrameworkAttribute was not found, so not applied. Because maybe did not reference core library.");
        }

        // Apply DA if could be imported.
        if (disableJITOptimization)
        {
            if (this.TryGetMethod(
                "System.Diagnostics.DebuggableAttribute..ctor",
                TypeParser.UnsafeParse<FunctionSignatureNode>("void(System.Diagnostics.DebuggableAttribute.DebuggingModes)"),
                out var dactor) &&
                this.TryGetType(
                    "System.Diagnostics.DebuggableAttribute.DebuggingModes",
                    out var dm))
            {
                var da = new CustomAttribute(dactor);
                da.ConstructorArguments.Add(new(
                    dm, (int)(DebuggableAttribute.DebuggingModes.Default | DebuggableAttribute.DebuggingModes.DisableOptimizations)));
                this.module.Assembly.CustomAttributes.Add(da);

                this.logger.Trace(
                    $"DebuggableAttribute is applied.");
            }
            else
            {
                this.logger.Warning(
                    $"DebuggableAttribute was not found, so not applied. Because maybe did not reference core library.");
            }
        }

        // Apply pointer visualizers if could be imported.
        if (this.TryGetType(
                "System.Type",
                out var typeType,
                LookupTargets.All) &&
            this.TryGetMethod(
                "System.Diagnostics.DebuggerTypeProxyAttribute..ctor",
                TypeParser.UnsafeParse<FunctionSignatureNode>("void(System.Type)"),
                out var dtpactor) &&
            this.TryGetType(
                "C.type.__pointer_visualizer",
                out var pvtype,
                LookupTargets.All))
        {
            foreach (var targetTypeName in new[] { "void*", "byte*", "sbyte*" })
            {
                var targetType = this.UnsafeGetType(targetTypeName);

                var dtpa = new CustomAttribute(dtpactor);
                dtpa.ConstructorArguments.Add(new(typeType, pvtype));
                dtpa.Properties.Add(new("Target", new(typeType, targetType)));
                this.module.Assembly.CustomAttributes.Add(dtpa);
            }

            this.logger.Trace(
                $"DebuggerTypeProxyAttribute is applied.");
        }
        else
        {
            this.logger.Warning(
                $"DebuggerTypeProxyAttribute or related types was not found, so not applied. Because maybe did not reference core library and/or libc.");
        }
    }

    private void SetupMainEntryPoint()
    {
        static bool IsMainEntryPoint(IList<ParameterDefinition> parameters)
        {
            var typeNames = parameters.Select(p => p.ParameterType.FullName).ToArray();
            return
                typeNames.SequenceEqual(Utilities.Empty<string>()) ||
                typeNames.SequenceEqual(new[] { "System.Int32", "System.SByte**" });
        }

        // Lookup main entry point.
        var mainFunction = this.module.Types.
            Where(type => type.IsAbstract && type.IsSealed && type.IsClass).
            SelectMany(type => type.Methods).
            FirstOrDefault(m =>
                m.IsStatic && m.Name == "main" &&
                (m.ReturnType.FullName == "System.Void" ||
                 m.ReturnType.FullName == "System.Int32") &&
                 IsMainEntryPoint(m.Parameters));

        // Inject startup code when declared.
        if (mainFunction != null)
        {
            this.BeginNewCilSourceCode(null, "_start.s", false);

            Token[][] startupTokensList;
            if (mainFunction.ReturnType.FullName == "System.Void")
            {
                startupTokensList = mainFunction.Parameters.Count == 0 ?
                    EmbeddingCodeFragments.Startup_Void_Void :
                    EmbeddingCodeFragments.Startup_Void;
            }
            else
            {
                startupTokensList = mainFunction.Parameters.Count == 0 ?
                    EmbeddingCodeFragments.Startup_Int32_Void :
                    EmbeddingCodeFragments.Startup_Int32;
            }

            foreach (var tokens in startupTokensList)
            {
                this.Parse(tokens);
            }

            Debug.Assert(this.method != null);
            this.module.EntryPoint = this.method!;

            this.FinishCurrentState();

            // Fire delayed actions.
            this.FireDelayedActions();

            if (!this.caughtError)
            {
                this.logger.Information($"Injected startup code.");
            }
        }
        else
        {
            this.caughtError = true;
            this.logger.Error($"Could not find main entry point.");
        }
    }

    private void ApplyFinalAdjustments(bool applyOptimization)
    {
        var documents = new Dictionary<string, Document>();
        foreach (var method in this.module.Types.
            SelectMany(type => type.Methods).
            Where(method =>
                !method.IsAbstract &&
                method.HasBody &&
                method.Body.Instructions.Count >= 1))
        {
            var body = method.Body;

            // Apply optimization.
            if (applyOptimization)
            {
                body.Optimize();
            }

            // After optimization, the instructions maybe changed absolute layouts,
            // so we could set debugging information scope after that.
            if (this.produceDebuggingInformation)
            {
                method.DebugInformation.Scope = new ScopeDebugInformation(
                    body.Instructions.First(),
                    body.Instructions.Last());

                // Will make sequence points:
                foreach (var instruction in body.Instructions)
                {
                    if (this.locationByInstructions.TryGetValue(instruction, out var location) &&
                        location.File.IsVisible)
                    {
                        if (!documents.TryGetValue(location.File.RelativePath, out var document))
                        {
                            document = new(location.File.BasePath is { } basePath ?
                                Path.Combine(basePath, location.File.RelativePath) :
                                location.File.RelativePath);
                            document.Type = DocumentType.Text;
                            if (location.File.Language is { } language)
                            {
                                document.Language = language;
                            }
                            documents.Add(location.File.RelativePath, document);
                        }

                        var sequencePoint = new SequencePoint(
                            instruction, document);

                        sequencePoint.StartLine = (int)(location.StartLine + 1);
                        sequencePoint.StartColumn = (int)(location.StartColumn + 1);
                        sequencePoint.EndLine = (int)(location.EndLine + 1);
                        sequencePoint.EndColumn = (int)(location.EndColumn + 1);

                        method.DebugInformation.SequencePoints.Add(
                            sequencePoint);
                    }
                }

                // Will make local variable naming.
                if (this.variableDebugInformationLists.TryGetValue(method.Name, out var list))
                {
                    foreach (var variableDebugInformation in list)
                    {
                        method.DebugInformation.Scope.Variables.Add(
                            variableDebugInformation);
                    }
                }
            }
        }
    }

    /////////////////////////////////////////////////////////////////////

    public bool Finish(
        TargetFramework? targetFramework,
        bool? disableJITOptimization,
        bool applyOptimization)
    {
        this.FinishCurrentState();

        if (!this.caughtError)
        {
            // Clean up for file scope type.
            this.BeginNewFileScope(unknown.RelativePath);

            // Add text type when exist methods.
            if (this.cabiTextType.Methods.Count >= 1 &&
                !this.module.Types.Any(t => object.ReferenceEquals(t, this.cabiTextType)))
            {
                this.module.Types.Add(this.cabiTextType);
            }

            // Add data type when exist fields.
            if (this.cabiDataType.Fields.Count >= 1 &&
                !this.module.Types.Any(t => object.ReferenceEquals(t, this.cabiDataType)))
            {
                this.module.Types.Add(this.cabiDataType);
            }

            // Add rdata type when exist fields.
            if (this.cabiRDataType.Fields.Count >= 1 &&
                !this.module.Types.Any(t => object.ReferenceEquals(t, this.cabiRDataType)))
            {
                this.module.Types.Add(this.cabiRDataType);
            }

            // Fire delayed actions.
            this.FireDelayedActions();

            // Setup entry point when set producing executable.
            if (this.produceExecutable)
            {
                this.SetupMainEntryPoint();
            }
        }

        if (!this.caughtError)
        {
            // Clean up for file scope type.
            this.BeginNewFileScope(unknown.RelativePath);

            // Add type initializer when body was appended.
            if (this.cabiDataTypeInitializer.Body is { } cabiDataTypeInitializerBody &&
                cabiDataTypeInitializerBody.Instructions is { } cabiDataTypeInitializerInstructions &&
                cabiDataTypeInitializerInstructions.Count >= 1)
            {
                cabiDataTypeInitializerInstructions.Add(Instruction.Create(OpCodes.Ret));
                cabiDataTypeInitializerBody.InitLocals = false;
                this.cabiDataType.Methods.Add(this.cabiDataTypeInitializer);
            }

            // (Completed all CIL implementations in this place.)

            ///////////////////////////////////////////////

            if (targetFramework is { } tf && disableJITOptimization is { } djo)
            {
                // Try add fundamental attributes.
                this.AddFundamentalAttributes(tf, djo);
            }

            // Apply final adjustments.
            this.ApplyFinalAdjustments(applyOptimization);
        }

        this.delayedLookupTypeActions.Clear();
        this.delayedLookupLocalMemberActions.Clear();
        this.delayedCheckAfterLookingupActions.Clear();

        this.files.Clear();
        this.locationByInstructions.Clear();
        this.variableDebugInformationLists.Clear();

        this.isProducedOriginalSourceCodeLocation = true;
        this.currentCilFile = unknown;
        this.queuedLocation = null;
        this.lastLocation = null;
        this.initializerIndex = 0;

        this.referenceTypes.Finish();

        var finished = !this.caughtError;
        this.caughtError = false;

        return finished;
    }
}
