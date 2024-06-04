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
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Cecil.Mdb;
using Mono.Cecil.Pdb;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace chibild.Internal;

// Imported from ILCompose project.
// https://github.com/kekyo/ILCompose

internal sealed class MultipleSymbolReaderProvider : ISymbolReaderProvider
{
    // HACK: cecil will lock symbol file when uses defaulted reading method.
    //   Makes safer around entire building process.

    private static readonly EmbeddedPortablePdbReaderProvider embeddedProvider = new();
    private static readonly MdbReaderProvider mdbProvider = new();
    private static readonly PdbReaderProvider pdbProvider = new();

    private readonly ILogger logger;
    private readonly HashSet<string> loaded = new();
    private readonly HashSet<string> notFound = new();

    public MultipleSymbolReaderProvider(ILogger logger) =>
        this.logger = logger;

    private ISymbolReader? TryGetSymbolReader<TSymbolReaderProvider>(
        TSymbolReaderProvider provider, ModuleDefinition module,
        string fullPath, string? extension)
        where TSymbolReaderProvider : ISymbolReaderProvider
    {
        var path = extension != null ?
            Path.Combine(
                CommonUtilities.GetDirectoryPath(fullPath),
                Path.GetFileNameWithoutExtension(fullPath) + extension) :
            fullPath;

        try
        {
            if (File.Exists(path))
            {
                var ms = new MemoryStream();
                using (var stream = new FileStream(
                    path, FileMode.Open, FileAccess.Read, FileShare.Read, 1024 * 1024))
                {
                    stream.CopyTo(ms);
                }
                ms.Position = 0;

                var sr = provider.GetSymbolReader(module, ms);
                if (this.loaded.Add(path))
                {
                    this.logger.Debug($"Debug symbol is loaded from: {path}");
                }
                
                return sr;
            }
        }
        catch (Exception ex)
        {
            this.logger.Warning(ex);
        }

        return null;
    }

    public ISymbolReader? GetSymbolReader(ModuleDefinition module, string fileName)
    {
        if (module.HasDebugHeader)
        {
            var fullPath = Path.GetFullPath(fileName);

            var header = module.GetDebugHeader();
            var entry = header.Entries.
                FirstOrDefault(e => e.Directory.Type == ImageDebugType.EmbeddedPortablePdb);

            lock (this.loaded)
            {
                if (!this.notFound.Contains(fileName))
                {
                    if (entry != null)
                    {
                        try
                        {
                            if (this.TryGetSymbolReader(embeddedProvider, module, fullPath, null) is { } sr2)
                            {
                                if (this.loaded.Add(fullPath))
                                {
                                    this.logger.Debug($"Embedded debug symbol is loaded from: {fullPath}");
                                }
                                return sr2;
                            }
                        }
                        catch (Exception ex)
                        {
                            this.logger.Warning(ex);
                        }
                    }
                    else if (this.TryGetSymbolReader(mdbProvider, module, fullPath, ".dll.mdb") is { } sr1)
                    {
                        return sr1;
                    }
                    else if (this.TryGetSymbolReader(pdbProvider, module, fullPath, ".pdb") is { } sr3)
                    {
                        return sr3;
                    }

                    if (this.notFound.Add(fileName))
                    {
                        this.logger.Trace($"Debug symbol is not found: {fileName}");
                    }
                }
            }
        }

        return null;
    }

    public ISymbolReader? GetSymbolReader(ModuleDefinition module, Stream symbolStream)
    {
        var ms = new MemoryStream();
        symbolStream.CopyTo(ms);
        ms.Position = 0;

        symbolStream.Dispose();

        try
        {
            return embeddedProvider.GetSymbolReader(module, ms);
        }
        catch
        {
        }

        try
        {
            ms.Position = 0;
            return mdbProvider.GetSymbolReader(module, ms);
        }
        catch
        {
        }

        try
        {
            ms.Position = 0;
            return pdbProvider.GetSymbolReader(module, ms);
        }
        catch
        {
        }

        return null;
    }
}
