/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibicc.toolchain.Generating;
using chibicc.toolchain.Internal;
using chibicc.toolchain.IO;
using chibicc.toolchain.Parsing;
using chibicc.toolchain.Logging;
using chibild.Internal;
using Mono.Cecil;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Threading;

namespace chibild.Generating;

internal sealed class AssemblyInputFragment :
    InputFragment
{
    private enum RequiredStates
    {
        Ignore,
        Required,
        Loaded,
    }

    private readonly string assemblyPath;

    private readonly Dictionary<string, Symbol> typeSymbols;
    private readonly Dictionary<string, Symbol> variableSymbols;
    private readonly Dictionary<string, Symbol> functionSymbols;

    private Func<CachedAssemblyResolver> assemblyResolverFactory;
    private AssemblyDefinition? assembly;
    private Dictionary<string, TypeDefinition> types;
    private Dictionary<string, FieldDefinition> fields;
    private Dictionary<string, MethodDefinition[]> methods;
    private Dictionary<string, ModuleDefinition> resolvedModules = new();

    private int requiredState;

    private AssemblyInputFragment(
        string baseInputPath,
        string relativePath,
        string assemblyPath,
        Func<CachedAssemblyResolver> assemblyResolverFactory,
        AggregatedSymbols extraction) :
        base(baseInputPath, relativePath)
    {
        this.assemblyPath = assemblyPath;
        this.assemblyResolverFactory = assemblyResolverFactory;

        this.typeSymbols = extraction.TypeSymbols;
        this.variableSymbols = extraction.VariableSymbols;
        this.functionSymbols = extraction.FunctionSymbols;

        this.types = new();
        this.fields = new();
        this.methods = new();

        this.requiredState = (int)RequiredStates.Ignore;
    }

    private AssemblyInputFragment(
        string baseInputPath,
        string relativePath,
        string assemblyPath,
        AggregatedSymbols extraction,
        LoadedCAbiMetadata cabiMetadata) :
        base(baseInputPath, relativePath)
    {
        this.assemblyPath = assemblyPath;
        this.assemblyResolverFactory = null!;

        this.typeSymbols = extraction.TypeSymbols;
        this.variableSymbols = extraction.VariableSymbols;
        this.functionSymbols = extraction.FunctionSymbols;

        this.assembly = cabiMetadata.Assembly;
        this.types = cabiMetadata.Types;
        this.fields = cabiMetadata.Fields;
        this.methods = cabiMetadata.Methods;

        this.requiredState = (int)RequiredStates.Loaded;
    }

    //////////////////////////////////////////////////////////////

    private void PrepareToLoadAssembly()
    {
        if (this.assembly == null)
        {
            var assemblyResolver = this.assemblyResolverFactory();

            var cabiMetadata = CecilUtilities.LoadCAbiMetadataFromAssembly(
                this.assemblyPath,
                assemblyResolver);

            this.assemblyResolverFactory = null!;

            this.assembly = cabiMetadata.Assembly;
            this.types = cabiMetadata.Types;
            this.fields = cabiMetadata.Fields;
            this.methods = cabiMetadata.Methods;
        }
    }

    public AssemblyDefinition GetAssembly()
    {
        if (this.requiredState != (int)RequiredStates.Loaded)
        {
            this.PrepareToLoadAssembly();
            this.requiredState = (int)RequiredStates.Loaded;
        }
        Debug.Assert(this.assembly != null);

        return this.assembly!;
    }

    public override string ToString() =>
        $"Assembly: {this.ObjectPath}";

    //////////////////////////////////////////////////////////////

    public override bool ContainsTypeAndSchedule(
        TypeNode type,
        out Scopes scope,
        out int? memberCount)
    {
        if (this.typeSymbols.TryGetValue(type.TypeIdentity, out var ts))
        {
            Interlocked.CompareExchange(
                ref this.requiredState,
                (int)RequiredStates.Required,
                (int)RequiredStates.Ignore);
            CommonUtilities.TryParseEnum(ts.Scope, out scope);
            memberCount = ts.MemberCount;
            return true;
        }
        scope = default;
        memberCount = null;
        return false;
    }

    public override bool ContainsVariableAndSchedule(
        IdentityNode variable,
        out Scopes scope)
    {
        if (this.variableSymbols.TryGetValue(variable.Identity, out var vs))
        {
            Interlocked.CompareExchange(
                ref this.requiredState,
                (int)RequiredStates.Required,
                (int)RequiredStates.Ignore);
            CommonUtilities.TryParseEnum(vs.Scope, out scope);
            return true;
        }
        scope = default;
        return false;
    }

    public override bool ContainsFunctionAndSchedule(
        IdentityNode function,
        FunctionSignatureNode? signature,
        out Scopes scope)
    {
        // Ignored the signature, because contains only CABI functions.
        if (this.functionSymbols.TryGetValue(function.Identity, out var fs))
        {
            Interlocked.CompareExchange(
                ref this.requiredState,
                (int)RequiredStates.Required,
                (int)RequiredStates.Ignore);
            CommonUtilities.TryParseEnum(fs.Scope, out scope);
            return true;
        }
        scope = default;
        return false;
    }

    //////////////////////////////////////////////////////////////

    public override LoadObjectResults LoadObjectIfRequired(
        ILogger logger,
        bool isLocationOriginSource)
    {
        if (Interlocked.CompareExchange(
            ref this.requiredState,
            (int)RequiredStates.Loaded,
            (int)RequiredStates.Required) == (int)RequiredStates.Required)
        {
            logger.Information($"Loading: {this.assemblyPath}");

            this.PrepareToLoadAssembly();
            return LoadObjectResults.Loaded;
        }

        return this.requiredState == (int)RequiredStates.Loaded ?
            LoadObjectResults.Loaded : LoadObjectResults.Ignored;
    }

    //////////////////////////////////////////////////////////////

    private ModuleDefinition ResolveOnFallbackModule(
        ModuleDefinition fallbackModule,
        MemberReference mr)
    {
        var anr = mr.Module.Assembly.Name;
        
        lock (this.resolvedModules)
        {
            if (!this.resolvedModules.TryGetValue(anr.Name, out var module))
            {
                lock (fallbackModule)
                {
                    var assembly = fallbackModule.AssemblyResolver.Resolve(anr);
                    module = assembly.MainModule;
                }
                this.resolvedModules.Add(anr.Name, module);
            }
            return module;
        }
    }

    public override bool TryGetType(
        TypeNode type,
        ModuleDefinition fallbackModule,
        out TypeReference tr)
    {
        if (this.types.TryGetValue(type.TypeIdentity, out var td))
        {
            if (td.Module == fallbackModule)
            {
                tr = td;
                return true;
            }

            // Resolve on fallback assembly resolver.
            var exactModule = this.ResolveOnFallbackModule(fallbackModule, td);
            if (exactModule.GetType(td.FullName) is { } ftd)
            {
                this.types[type.TypeIdentity] = ftd;

                tr = ftd;
                return true;
            }
            else
            {
                Debug.Fail($"Could not resolve a type on fallback assembly: {td.FullName}");
            }
        }

        tr = null!;
        return false;
    }

    public override bool TryGetField(
        IdentityNode variable,
        ModuleDefinition fallbackModule,
        out FieldReference fr)
    {
        if (this.fields.TryGetValue(variable.Identity, out var fd))
        {
            if (fd.Module == fallbackModule)
            {
                fr = fd;
                return true;
            }

            // Resolve on fallback assembly resolver.
            var exactModule = this.ResolveOnFallbackModule(fallbackModule, fd);
            if (exactModule.GetType(fd.DeclaringType.FullName) is { } ftd &&
                ftd.Fields.FirstOrDefault(f => f.Name == fd.Name) is { } ffd)
            {
                this.fields[variable.Identity] = ffd;

                fr = ffd;
                return true;
            }
            else
            {
                Debug.Fail($"Could not resolve a field on fallback assembly: {fd.DeclaringType.FullName}.{fd.Name}");
            }
        }

        fr = null!;
        return false;
    }

    private static bool TryGetMatchedMethodIndex(
        FunctionSignatureNode signature,
        MethodDefinition[] overloads,
        out int index)
    {
        // If target signature is variadic, will ignore exact match.
        if (signature.CallingConvention ==
            chibicc.toolchain.Parsing.MethodCallingConvention.Default)
        {
            for (index = 0; index < overloads.Length; index++)
            {
                var overload = overloads[index];

                // Match exactly.
                if (overload.Parameters.
                    Select(p => p.ParameterType.FullName).
                    SequenceEqual(signature.Parameters.Select(p => p.ParameterType.CilTypeName)))
                {
                    return true;
                }
            }
        }

        for (index = 0; index < overloads.Length; index++)
        {
            var overload = overloads[index];

            // Match partially when overload is variadic.
            if (overload.CallingConvention == Mono.Cecil.MethodCallingConvention.VarArg)
            {
                if (overload.Parameters.
                    Select(p => p.ParameterType.FullName).
                    SequenceEqual(signature.Parameters.
                        Take(overload.Parameters.Count).
                        Select(p => p.ParameterType.CilTypeName)))
                {
                    return true;
                }
            }
            if (signature.CallingConvention ==
                chibicc.toolchain.Parsing.MethodCallingConvention.VarArg)
            {
                if (overload.Parameters.
                    Take(signature.Parameters.Length).
                    Select(p => p.ParameterType.FullName).
                    SequenceEqual(signature.Parameters.
                        Select(p => p.ParameterType.CilTypeName)))
                {
                    return true;
                }
            }
        }

        index = -1;
        return false;
    }

    public override bool TryGetMethod(
        IdentityNode function,
        FunctionSignatureNode? signature,
        ModuleDefinition fallbackModule,
        out MethodReference mr)
    {
        if (!this.methods.TryGetValue(function.Identity, out var overloads))
        {
            mr = null!;
            return false;
        }

        // Resolve on fallback assembly resolver.
        MethodReference ResolveOnFallbackModule(MethodDefinition md)
        {
            var exactModule = this.ResolveOnFallbackModule(fallbackModule, md);
            if (exactModule.GetType(md.DeclaringType.FullName) is { } ftd &&
                ftd.Methods.FirstOrDefault(m => CecilUtilities.Equals(m, md)) is { } fmd)
            {
                return fmd;
            }
            else
            {
                Debug.Fail($"Could not resolve a method on fallback assembly: {md.DeclaringType.FullName}.{md.Name}({signature})");
                return md;
            }
        }

        if (signature == null)
        {
            mr = ResolveOnFallbackModule(overloads[0]);
            return true;
        }
        if (TryGetMatchedMethodIndex(signature, overloads, out var index))
        {
            mr = ResolveOnFallbackModule(overloads[index]);
            return true;
        }

        mr = null!;
        return false;
    }
    
    //////////////////////////////////////////////////////////////

    private static string GetAssemblyPathHashedPath(string assemblyPath)
    {
        using var alg = MD5.Create();
        var path = BitConverter.ToString(alg.ComputeHash(Encoding.UTF8.GetBytes(assemblyPath))).
            Replace("-", string.Empty).
            ToLowerInvariant();

        return Path.Combine(
            path.Substring(0, 2),
            path.Substring(2));
    }
    
    private static bool IsValidCache(
        TextReader tr,
        string assemblyPath)
    {
        var header1 = tr.ReadLine()?.Split(' ') ?? CommonUtilities.Empty<string>();
        if (header1.FirstOrDefault() is ".path" &&
            header1.ElementAt(1) is { } path)
        {
            return assemblyPath == path;
        }

        return false;
    }

    //////////////////////////////////////////////////////////////

    public static AssemblyInputFragment Load(
        ILogger logger,
        string baseInputPath,
        string relativePath,
        string? cacheBasePath,
        Func<CachedAssemblyResolver> assemblyResolverFactory)
    {
        var assemblyPath = Path.Combine(baseInputPath, relativePath);
        var assemblyHashedPath = GetAssemblyPathHashedPath(assemblyPath) + ".symtab";
        //var assemblyHash = GetAssemblyHash(assemblyPath);

        // Found cached file.
        var cachePath = cacheBasePath != null ?
            Path.Combine(cacheBasePath, assemblyHashedPath) :
            null;
        if (cachePath != null)
        {
            try
            {
                if (File.Exists(cachePath))
                {
                    var assemblyDate = File.GetLastWriteTime(assemblyPath);
                    var cacheDate = File.GetLastWriteTime(cachePath);

                    if (cacheDate >= assemblyDate)
                    {
                        using var cacheStream = CompressionStreamUtilities.OpenStream(
                            cachePath, false);
                        var tr = new StreamReader(cacheStream, Encoding.UTF8, true);

                        if (IsValidCache(tr, assemblyPath))
                        {
                            var symbolLists = SymbolUtilities.EnumerateSymbolTable(
                                tr,
                                cachePath);

                            var aggSymbols = SymbolAggregator.AggregateSymbolsFromSymbolTable(
                                logger,
                                symbolLists).
                                ToArray();

                            if (aggSymbols.SingleOrDefault() is { } aggSymbol &&
                                aggSymbol.ObjectName == "symcache")
                            {
                                logger.Information($"Loading cached symbols: {relativePath}");
                                logger.Trace($"Cached symbols: {cachePath}");

                                return new(
                                    baseInputPath,
                                    relativePath,
                                    assemblyPath,
                                    assemblyResolverFactory,
                                    aggSymbol);
                            }
                        }
                    }
                }
            }
            catch
            {
            }
        }

        // TODO: native dll

        logger.Information($"Loading assembly: {relativePath}");

        // Load from assembly immediate.
        var assemblyResolver = assemblyResolverFactory();
        var cabiMetadata = CecilUtilities.LoadCAbiMetadataFromAssembly(
            assemblyPath,
            assemblyResolver);

        // Construct symbol aggregation and symbol table.
        var aggSymbol2 = SymbolAggregator.CreateSymbolAggregationFromDefinitions(
            "symcache",
            cabiMetadata.Types,
            cabiMetadata.Fields,
            cabiMetadata.Methods);

        var symbolList = new SymbolList(
            aggSymbol2.ObjectName,
            aggSymbol2.TypeSymbols.Values.
            Concat(aggSymbol2.VariableSymbols.Values).
            Concat(aggSymbol2.FunctionSymbols.Values).
            ToArray());

        if (cachePath != null)
        {
            var newCachePath = cachePath + $"_{Guid.NewGuid():N}";
            var newCacheBasePath = CommonUtilities.GetDirectoryPath(newCachePath);
            if (!Directory.Exists(newCacheBasePath))
            {
                try
                {
                    Directory.CreateDirectory(newCacheBasePath);
                }
                catch
                {
                }
            }

            try
            {
                using (var cacheStream = CompressionStreamUtilities.OpenStream(
                    newCachePath, true))
                {
                    var tw = new StreamWriter(cacheStream, Encoding.UTF8);
                    tw.WriteLine($".path {assemblyPath}");

                    SymbolUtilities.WriteSymbolTable(tw, new[] { symbolList });
                }
            }
            catch
            {
                File.Delete(newCachePath);
                throw;
            }

            try
            {
                // Swap new cache file. Totally ignored any failures.
                File.Delete(cachePath);
                File.Move(newCachePath, cachePath);
            }
            catch
            {
                File.Delete(newCachePath);
            }
        }

        return new(
            baseInputPath,
            relativePath,
            assemblyPath,
            aggSymbol2,
            cabiMetadata);
    }
}
