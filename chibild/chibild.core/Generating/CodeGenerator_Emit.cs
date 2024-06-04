/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibicc.toolchain.Internal;
using chibicc.toolchain.IO;
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
using System.Text;
using System.Threading.Tasks;

namespace chibild.Generating;

partial class CodeGenerator
{
    private static readonly Dictionary<string, string> cabiStartUpSignatureTypeNames = new()
    {
        { "System.Void", "v" },
        { "System.Void,System.Int32", "vc" },
        { "System.Void,System.Int32,System.SByte**", "vcv" },
        { "System.Void,System.Int32,System.SByte**,System.SByte**", "vcve" },
        { "System.Int32", "i" },
        { "System.Int32,System.Int32", "ic" },
        { "System.Int32,System.Int32,System.SByte**", "icv" },
        { "System.Int32,System.Int32,System.SByte**,System.SByte**", "icve" },
    };
    
    private static bool UnsafeGetCoreType(
        ModuleDefinition targetModule,
        InputFragment[] inputFragments,
        string coreTypeName,
        out TypeReference tr)
    {
        var coreType = new TypeIdentityNode(coreTypeName, Token.Unknown);
        foreach (var fragment in inputFragments)
        {
            if (fragment.TryGetType(
                coreType,
                targetModule,
                out tr))
            {
                return true;
            }
        }
        tr = null!;
        return false;
    }
        
    private void AddTargetFrameworkAttribute(
        InputFragment[] inputFragments,
        TargetFramework targetFramework)
    {
        // Apply TargetFrameworkAttribute if could be imported.
        if (UnsafeGetCoreType(
            this.targetModule,
            inputFragments,
            "System.Runtime.Versioning.TargetFrameworkAttribute",
            out var tfatr))
        {
            if (tfatr.Resolve().Methods.FirstOrDefault(m =>
                m is { IsPublic: true, IsStatic: false, IsConstructor: true, } &&
                m.Parameters.Count == 1 &&
                m.Parameters[0].ParameterType.FullName == "System.String") is MethodReference ctor)
            {
                ctor = this.targetModule.SafeImport(ctor);
                
                var tfa = new CustomAttribute(ctor);
                tfa.ConstructorArguments.Add(new(
                    this.targetModule.TypeSystem.String,
                    targetFramework.ToString()));
                this.targetModule.Assembly.CustomAttributes.Add(tfa);

                this.logger.Trace(
                    "TargetFrameworkAttribute is applied.");
            }
            else
            {
                this.logger.Warning(
                    "TargetFrameworkAttribute constructor was not found.");
            }
        }
        else
        {
            this.logger.Warning(
                "TargetFrameworkAttribute was not found, so not applied. Because maybe did not reference core library.");
        }
    }

    private void AddDebuggableAttribute(
        InputFragment[] inputFragments)
    {
        if (UnsafeGetCoreType(
                this.targetModule,
                inputFragments,
                "System.Diagnostics.DebuggableAttribute.DebuggingModes",
            out var dadmtr) &&
            UnsafeGetCoreType(
                this.targetModule,
                inputFragments,
                "System.Diagnostics.DebuggableAttribute",
            out var datr))
        {
            if (datr.Resolve().Methods.FirstOrDefault(m =>
                m is { IsPublic: true, IsStatic: false, IsConstructor: true, } &&
                m.Parameters.Count == 1 &&
                m.Parameters[0].ParameterType.FullName.EndsWith("DebuggingModes")) is MethodReference ctor)
            {
                dadmtr = this.targetModule.SafeImport(dadmtr);
                ctor = this.targetModule.SafeImport(ctor);
                
                var da = new CustomAttribute(ctor);
                da.ConstructorArguments.Add(new(
                    dadmtr,
                    (int)(DebuggableAttribute.DebuggingModes.Default |
                          DebuggableAttribute.DebuggingModes.DisableOptimizations)));
                this.targetModule.Assembly.CustomAttributes.Add(da);

                this.logger.Trace(
                    "DebuggableAttribute is applied.");
            }
            else
            {
                this.logger.Warning(
                    "DebuggableAttribute constructor was not found.");
            }
        }
        else
        {
            this.logger.Warning(
                "DebuggableAttribute and/or DebuggingModes was not found, so not applied. Because maybe did not reference core library.");
        }
    }

    private void AddPointerVisualizerAttributes(
        InputFragment[] inputFragments)
    {
        // Apply pointer visualizers if could be imported.
        if (UnsafeGetCoreType(
                this.targetModule,
                inputFragments,
                "System.Type",
                out var sttr) &&
            UnsafeGetCoreType(
                this.targetModule,
                inputFragments,
                "System.Diagnostics.DebuggerTypeProxyAttribute",
                out var dtpatr) &&
            UnsafeGetCoreType(
                this.targetModule,
                inputFragments,
                "C.type.__pointer_visualizer",
                out var pvtr))
        {
            if (dtpatr.Resolve().Methods.FirstOrDefault(m =>
                m is { IsPublic: true, IsStatic: false, IsConstructor: true, } &&
                m.Parameters.Count == 1 &&
                m.Parameters[0].ParameterType.FullName ==
                "System.Type") is MethodReference ctor)
            {
                sttr = this.targetModule.SafeImport(sttr);
                pvtr = this.targetModule.SafeImport(pvtr);
                ctor = this.targetModule.SafeImport(ctor);

                foreach (var targetTypeName in new[]
                    { "System.Void", "System.Byte", "System.SByte", })
                {
                    if (UnsafeGetCoreType(
                        this.targetModule,
                        inputFragments,
                        targetTypeName,
                        out var ttr))
                    {
                        var ptr = this.targetModule.SafeImport(ttr).MakePointerType();
                        
                        var dtpa = new CustomAttribute(ctor);
                        dtpa.ConstructorArguments.Add(new(sttr, pvtr));
                        dtpa.Properties.Add(new(
                            "Target",
                            new(sttr, ptr)));
                        this.targetModule.Assembly.CustomAttributes.Add(dtpa);

                        this.logger.Trace(
                            $"DebuggerTypeProxyAttribute({ptr.FullName}) is applied.");
                    }
                    else
                    {
                        this.logger.Warning(
                            "DebuggerTypeProxyAttribute was not found, so not applied. Because maybe did not reference core library.");
                    }
                }
            }
            else
            {
                this.logger.Warning(
                    "DebuggerTypeProxyAttribute and/or pointer visualizer type was not found, so not applied. Because maybe did not reference core library or libc.");
            }
        }
    }

    ///////////////////////////////////////////////////////////////////////////////

    private sealed class TypeDefinitionsHolder
    {
        private sealed class InnerHolder
        {
            private readonly ModuleDefinition module;
            private readonly string namespaceName;
            private readonly string typeName;
            private readonly string typeFullName;
            private readonly TypeAttributes attributes;


            private TypeDefinition? type;

            public InnerHolder(
                ModuleDefinition module,
                string? namespaceName,
                string typeName,
                TypeAttributes attributes)
            {
                this.module = module;
                this.namespaceName = namespaceName ?? "";
                this.typeName = typeName;
                this.typeFullName = namespaceName != null ?
                    $"{this.namespaceName}.{this.typeName}" :
                    typeName;
                this.attributes = attributes | TypeAttributes.Abstract | TypeAttributes.Sealed;
                this.type = this.module.GetType(this.typeFullName);
            }

            public TypeDefinition? GetIfAvailable() =>
                this.type;

            public TypeDefinition GetOrCreate()
            {
                if (this.type == null)
                {
                    if (this.module.GetType(this.typeFullName) is not { } type)
                    {
                        type = new(this.namespaceName,
                            this.typeName,
                            this.attributes,
                            this.module.TypeSystem.Object);
                        this.module.Types.Add(type);
                    }
                    this.type = type;
                }
                return this.type;
            }
        }
        
        private readonly InnerHolder fileScopedType;
        private readonly InnerHolder dataType;
        private readonly InnerHolder rdataType;
        private readonly InnerHolder textType;
        private readonly Lazy<MethodBody> fileScopedInitializerBody;
        private readonly Lazy<MethodBody> dataInitializerBody;

        public TypeDefinitionsHolder(
            ObjectInputFragment fragment,
            ModuleDefinition targetModule)
        {
            this.fileScopedType = new(
                targetModule,
                null,
                $"<{CecilUtilities.SanitizeFileNameToMemberName(fragment.ObjectName)}>$",
                TypeAttributes.NotPublic | TypeAttributes.BeforeFieldInit);
            this.dataType = new(
                targetModule,
                "C",
                "data",
                TypeAttributes.Public | TypeAttributes.BeforeFieldInit);
            this.rdataType = new(
                targetModule,
                "C",
                "rdata",
                TypeAttributes.Public | TypeAttributes.BeforeFieldInit);
            this.textType = new(
                targetModule,
                "C",
                "text",
                TypeAttributes.Public);

            MethodBody GetInitializerBody(TypeDefinition type)
            {
                if (type.Methods.FirstOrDefault(m => m.IsConstructor && m.IsStatic) is not { HasBody: true } cctor)
                {
                    cctor = new MethodDefinition(
                        ".cctor",
                        MethodAttributes.Private | MethodAttributes.Static |
                        MethodAttributes.SpecialName | MethodAttributes.RTSpecialName | MethodAttributes.HideBySig,
                        targetModule.TypeSystem.Void);

                    var body = cctor.Body;
                    body.InitLocals = false;
                    body.Instructions.Add(Instruction.Create(OpCodes.Ret));

                    type.Methods.Add(cctor);
                }
                return cctor.Body;
            }

            this.fileScopedInitializerBody = new(() =>
                GetInitializerBody(this.fileScopedType.GetOrCreate()));
            this.dataInitializerBody = new(() =>
                GetInitializerBody(this.dataType.GetOrCreate()));
        }

        public TypeDefinition GetFileScopedType() =>
            this.fileScopedType.GetOrCreate();

        public TypeDefinition? GetFileScopedTypeIfAvailable() =>
            this.fileScopedType.GetIfAvailable();

        public TypeDefinition GetDataType() =>
            this.dataType.GetOrCreate();

        public TypeDefinition? GetDataTypeIfAvailable() =>
            this.dataType.GetIfAvailable();

        public TypeDefinition GetRDataType() =>
            this.rdataType.GetOrCreate();

        public TypeDefinition GetTextType() =>
            this.textType.GetOrCreate();

        public Mono.Collections.Generic.Collection<Instruction> GetFileScopedInitializerBody() =>
            this.fileScopedInitializerBody.Value.Instructions;

        public Mono.Collections.Generic.Collection<Instruction> GetDataInitializerBody() =>
            this.dataInitializerBody.Value.Instructions;
    }

    private void EmitMembers(
        ObjectInputFragment fragment)
    {
        var holder = new TypeDefinitionsHolder(
            fragment,
            this.targetModule);

        ///////////////////////////////////////////////////

        var count = 0;
        foreach (var enumeration in fragment.GetDeclaredEnumerations(true))
        {
            holder.GetFileScopedType().
                NestedTypes.
                Add(enumeration);
            count++;
        }

        this.logger.Trace($"Total emitted enumerations [file]: {count}");

        count = 0;
        foreach (var structure in fragment.GetDeclaredStructures(true))
        {
            holder.GetFileScopedType().
                NestedTypes.
                Add(structure);
            count++;
        }

        this.logger.Trace($"Total emitted structures [file]: {count}");

        count = 0;
        foreach (var function in fragment.GetDeclaredFunctions(true))
        {
            holder.GetFileScopedType().
                Methods.
                Add(function);
            count++;
        }

        this.logger.Trace($"Total emitted functions [file]: {count}");

        count = 0;
        foreach (var variable in fragment.GetDeclaredVariables(true))
        {
            holder.GetFileScopedType().
                Fields.
                Add(variable);
            count++;
        }
        
        this.logger.Trace($"Total emitted variables [file]: {count}");

        count = 0;
        foreach (var constant in fragment.GetDeclaredConstants(true))
        {
            holder.GetFileScopedType().
                Fields.
                Add(constant);
            count++;
        }
        
        this.logger.Trace($"Total emitted constants [file]: {count}");

        var fileScopedInitializerNames = (holder.GetFileScopedTypeIfAvailable()?.
            Methods.
            Select(m => m.Name) ?? CommonUtilities.Empty<string>()).
            ToHashSet();
        var subIndex = 1;
        count = 0;
        foreach (var initializer in fragment.GetDeclaredInitializer(true).Reverse())
        {
            // Change method name when initializer symbol name is duplicate.
            var baseName = initializer.Name;
            while (fileScopedInitializerNames.Contains(initializer.Name))
            {
                initializer.Name = $"{baseName}_{subIndex++}";
            }
            fileScopedInitializerNames.Add(initializer.Name);
            
            holder.GetFileScopedType().
                Methods.
                Insert(0, initializer);

            // Insert type initializer caller.
            var instructions = holder.GetFileScopedInitializerBody();
            instructions.Insert(
                0,
                Instruction.Create(OpCodes.Call, initializer));
            count++;
        }
        
        this.logger.Trace($"Total emitted initializers [file]: {count}");

        ///////////////////////////////////////////////////

        count = 0;
        foreach (var enumeration in fragment.GetDeclaredEnumerations(false))
        {
            this.targetModule.Types.Add(enumeration);
            count++;
        }
        
        this.logger.Trace($"Total emitted enumerations [public/internal]: {count}");

        count = 0;
        foreach (var structure in fragment.GetDeclaredStructures(false))
        {
            this.targetModule.Types.Add(structure);
            count++;
        }
        
        this.logger.Trace($"Total emitted structures [public/internal]: {count}");

        count = 0;
        foreach (var function in fragment.GetDeclaredFunctions(false))
        {
            holder.GetTextType().
                Methods.
                Add(function);
            count++;
        }
        
        this.logger.Trace($"Total emitted functions [public/internal]: {count}");

        count = 0;
        foreach (var variable in fragment.GetDeclaredVariables(false))
        {
            holder.GetDataType().
                Fields.
                Add(variable);
            count++;
        }
        
        this.logger.Trace($"Total emitted variables [public/internal]: {count}");

        count = 0;
        foreach (var constant in fragment.GetDeclaredConstants(false))
        {
            holder.GetRDataType().
                Fields.
                Add(constant);
            count++;
        }
        
        this.logger.Trace($"Total emitted constants [public/internal]: {count}");

        var initializerNames = (holder.GetDataTypeIfAvailable()?.
            Methods.
            Select(m => m.Name) ?? CommonUtilities.Empty<string>()).
            ToHashSet();
        subIndex = 1;
        count = 0;
        foreach (var initializer in fragment.GetDeclaredInitializer(false).Reverse())
        {
            // Change method name when initializer symbol name is duplicate.
            var baseName = initializer.Name;
            while (initializerNames.Contains(initializer.Name))
            {
                initializer.Name = $"{baseName}_{subIndex++}";
            }
            initializerNames.Add(initializer.Name);
            
            holder.GetDataType().
                Methods.
                Insert(0, initializer);

            // Insert type initializer caller.
            var instructions = holder.GetDataInitializerBody();
            instructions.Insert(
                0,
                Instruction.Create(OpCodes.Call, initializer));
            count++;
        }
        
        this.logger.Trace($"Total emitted initializers [internal]: {count}");

        ///////////////////////////////////////////////////

        var moduleType = this.targetModule.Types.
            First(t => t.FullName == "<Module>");

        count = 0;
        foreach (var function in fragment.GetDeclaredModuleFunctions())
        {
            moduleType.
                Methods.
                Add(function);
            count++;
        }
        
        this.logger.Trace($"Total emitted module functions: {count}");
    }

    ///////////////////////////////////////////////////////////////////////////////

    private void OptimizeMethods(
        ObjectInputFragment fragment)
    {
        var count = 0;
        foreach (var function in fragment.GetDeclaredFunctions(true))
        {
            function.Body.Optimize();
            count++;
        }

        this.logger.Trace($"Total optimized methods [file]: {count}");

        count = 0;
        foreach (var function in fragment.GetDeclaredFunctions(false))
        {
            function.Body.Optimize();
            count++;
        }

        this.logger.Trace($"Total optimized methods [public/internal]: {count}");

        count = 0;
        foreach (var initializer in fragment.GetDeclaredInitializer(true))
        {
            initializer.Body.Optimize();
            count++;
        }

        this.logger.Trace($"Total optimized methods [file initializer]: {count}");

        count = 0;
        foreach (var initializer in fragment.GetDeclaredInitializer(false))
        {
            initializer.Body.Optimize();
            count++;
        }

        this.logger.Trace($"Total optimized methods [public/internal initializer]: {count}");
    }

    ///////////////////////////////////////////////////////////////////////////////

    private bool TryLoadAndConsumeAdhocObject(
        InputFragment[] inputFragments,
        string baseInputPath,
        string relativePath,
        TextReader objectReader,
        out ObjectFileInputFragment fragment)
    {
        // Load this startup object file.
        if (!ObjectFileInputFragment.TryLoad(
            this.logger,
            baseInputPath,
            relativePath,
            objectReader,
            false,
            out fragment))
        {
            return false;
        }

        // Step 1. Consume the object.
        this.ConsumeFragment(fragment, inputFragments);
        if (this.caughtError)
        {
            return false;
        }

        // Step 2. Consume scheduled object referring in archives.
        this.ConsumeArchivedObject(inputFragments, false);
        if (this.caughtError)
        {
            return false;
        }

        return true;
    }

    private bool TryLoadAndConsumeCAbiStartUpObjectIfRequired(
        InputFragment[] inputFragments,
        string cabiStartUpObjectDirectoryPath,
        out ObjectFileInputFragment fragment)
    {
        // Search for a set of functions built on the target module,
        // and if a function that is considered to be a main function exists,
        // build an additional `crt0` startup code that matches the signature of the function.

        // Originally, this operation should be done on chibicc side.
        // However, since the CLR cannot use the CDECL convention,
        // it is necessary to identify the function signature,
        // and all related object files must be analyzed on the chibicc side,
        // which is quite difficult.

        static string? GetCAbiStartUpObjectFilePostfix(MethodDefinition method)
        {
            if (method is { IsStatic: true, Name: "main" } &&
                method.Parameters.Count <= 3)
            {
                var typeNames = method.Parameters.
                    Select(p => p.ParameterType.FullName).
                    Prepend(method.ReturnType.FullName).
                    ToArray();
                if (cabiStartUpSignatureTypeNames.TryGetValue(
                    string.Join(",", typeNames),
                    out var postfix))
                {
                    return postfix;
                }
            }
            return null;
        }

        // Search for the main function with the most arguments
        // and get the corresponding startup postfix.
        if (this.targetModule.Types.
            Where(type => type is { IsClass: true, FullName: "C.text" }).   // Priority search for 'C.text'.
            Concat(this.targetModule.Types.Where(type => type.IsClass && type.FullName != "C.text")).
            SelectMany(type => type.Methods.Select(GetCAbiStartUpObjectFilePostfix).Where(p => p != null)).
            OrderByDescending(pf => pf!.Length).
            FirstOrDefault() is { } postfix)
        {
            var fileName = $"crt0_{postfix}.o";
            var objectPath = Path.Combine(
                cabiStartUpObjectDirectoryPath,
                fileName);
            if (!File.Exists(objectPath))
            {
                this.caughtError = true;
                this.logger.Error($"Could not find CABI startup object: {fileName}");
                fragment = null!;
                return false;
            }

            using var fs = CompressionStreamUtilities.OpenStream(
                objectPath,
                false);

            var tr = new StreamReader(fs, Encoding.UTF8, true);

            if (!this.TryLoadAndConsumeAdhocObject(
                inputFragments,
                cabiStartUpObjectDirectoryPath,
                fileName,
                tr,
                out fragment))
            {
                return false;
            }

            this.logger.Information($"Loaded CABI startup object: {fileName}");

            return true;
        }
        else
        {
            this.logger.Warning("Could not find CABI startup function.");
        }

        fragment = null!;
        return false;
    }
    
    private void AssignEntryPoint(
        string entryPointSymbol)
    {
        // The entry point is a pure CLR entry point, unlike the CABI startup code.
        // However, the CABI startup code includes this,
        // and the entry point symbol (`_start` by default) is found and set with the following code.
        
        // Priority search for a module type.
        if (this.targetModule.Types.
            Where(type => type is { IsClass: true, FullName: "<Module>" }).   // Priority search for module class.
            Concat(this.targetModule.Types.Where(type => type.IsClass && type.FullName != "<Module>")).
            SelectMany(type => type.Methods).
            FirstOrDefault(method => method.IsStatic && method.Name == entryPointSymbol) is { } startup)
        {
            // Assign startup method when declared.
            this.targetModule.EntryPoint = startup;
            this.logger.Trace($"Found entry point: {startup.FullName}");
        }
        else
        {
            this.caughtError = true;
            this.logger.Error($"Could not find any entry point.");
        }
    }

    private void InsertPrependExecutionPath(
        InputFragment[] inputFragments,
        string[] prependExecutionSearchPaths,
        MethodDefinition targetMethod)
    {
        var function = IdentityNode.Create("__prepend_path_env");

        foreach (var fragment in inputFragments)
        {
            if (fragment.ContainsFunctionAndSchedule(
                function,
                null,
                out var scope) &&
                fragment.LoadObjectIfRequired(this.logger, false) == LoadObjectResults.Loaded &&
                fragment.TryGetMethod(
                    function,
                    null,
                    this.targetModule,
                    out var m))
            {
                var method = targetModule.SafeImport(m);
                var instructions = targetMethod.Body.Instructions;

                foreach (var prependPath in prependExecutionSearchPaths.Reverse())
                {
                    instructions.Insert(0, Instruction.Create(OpCodes.Ldstr, prependPath));
                    instructions.Insert(1, Instruction.Create(OpCodes.Call, method));

                    this.logger.Information($"Set prepend execution search path: {prependPath}");
                }

                return;
            }
        }

        this.caughtError = true;
        this.logger.Error($"Could not find prepender implementation.");
    }

    ///////////////////////////////////////////////////////////////////////////////

    private void InvokeDelayedLookingUps()
    {
        // Apply all delayed looking up (types).
        var count = 0;
        while (this.delayLookingUpEntries1.Count >= 1)
        {
            var action = this.delayLookingUpEntries1.Dequeue();
            action();
            count++;
        }

        this.logger.Trace($"Total delayed looking up [1]: {count}");

        // Apply all delayed looking up (not types).
        count = 0;
        while (this.delayLookingUpEntries2.Count >= 1)
        {
            var action = this.delayLookingUpEntries2.Dequeue();
            action();
            count++;
        }

        this.logger.Trace($"Total delayed looking up [2]: {count}");

        // Assert reverse dependencies are nothing.
        Debug.Assert(this.delayLookingUpEntries1.Count == 0);
    }
    
    public bool Emit(
        InputFragment[] inputFragments,
        bool applyOptimization,
        bool isEmbeddingSourceFile,
        LinkerCreationOptions? creationOptions,
        string[] prependExecutionSearchPaths)
    {
        // Try add fundamental attributes.
        if (creationOptions != null)
        {
            if (creationOptions.TargetFramework is { } targetFramework)
            {
                this.AddTargetFrameworkAttribute(
                    inputFragments,
                    targetFramework);
            }

            if (creationOptions.AssemblyOptions.HasFlag(
                AssemblyOptions.DisableJITOptimization))
            {
                this.AddDebuggableAttribute(
                    inputFragments);
            }
            
            this.AddPointerVisualizerAttributes(
                inputFragments);
        }

        // Combine all object fragments into target module.
        foreach (var fragment in inputFragments.
            OfType<ObjectInputFragment>())
        {
            this.EmitMembers(fragment);
        }

        // Invoke delayed looking ups.
        this.InvokeDelayedLookingUps();

        // Load CABI main object.
        if (creationOptions is { } co &&
            co.AssemblyType != AssemblyTypes.Dll &&
            co.CAbiStartUpObjectDirectoryPath is { } cabiStartUpObjectDirectoryPath)
        {
            if (this.TryLoadAndConsumeCAbiStartUpObjectIfRequired(
                inputFragments,
                cabiStartUpObjectDirectoryPath,
                out var fragment1))
            {
                // Emit this object.
                this.EmitMembers(fragment1);

                // Invoke delayed looking ups.
                this.InvokeDelayedLookingUps();
            }
        }

        // Assign entry point.
        if (creationOptions is { } co2 &&
            co2.AssemblyType != AssemblyTypes.Dll)
        {
            this.AssignEntryPoint(
                co2.EntryPointSymbol);
        }

        // Insert prepend search path.
        if (prependExecutionSearchPaths.Length >= 1 &&
            targetModule.EntryPoint is { } targetMethod)
        {
            this.InsertPrependExecutionPath(
                inputFragments,
                prependExecutionSearchPaths,
                targetMethod);
        }

        // Apply method optimization for all object fragments.
        if (applyOptimization)
        {
            foreach (var fragment in inputFragments.
                OfType<ObjectInputFragment>())
            {
                this.OptimizeMethods(fragment);
            }
        }

        // Apply all delayed debugger information.
        if (this.produceDebuggingInformation)
        {
            var cachedDocuments = new Dictionary<string, Document>();
            while (this.delayDebuggingInsertionEntries.Count >= 1)
            {
                var action = this.delayDebuggingInsertionEntries.Dequeue();
                action(cachedDocuments, isEmbeddingSourceFile);
            }
        }

        Debug.Assert(this.delayDebuggingInsertionEntries.Count == 0);

        // (Completed all CIL implementations in this place.)

        ///////////////////////////////////////////////

        return !this.caughtError;
    }
}
