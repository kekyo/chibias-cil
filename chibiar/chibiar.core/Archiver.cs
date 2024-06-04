/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibiar.Cli;
using chibicc.toolchain.Archiving;
using chibicc.toolchain.Generating;
using chibicc.toolchain.Internal;
using chibicc.toolchain.IO;
using chibicc.toolchain.Logging;
using System;
using System.Collections.Generic;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace chibiar;

public sealed class Archiver
{
    private readonly ILogger logger;

    public Archiver(ILogger logger) =>
        this.logger = logger;

    private static bool IsSourceFile(string path) =>
        Path.GetExtension(path) is not ".o";

    private static Stream OpenObjectStreamToCompressed(string objectFilePath)
    {
        var ofs = StreamUtilities.OpenStream(objectFilePath, false);
    
        return IsSourceFile(objectFilePath) ?
            new GZipStream(ofs, CompressionLevel.Optimal) : ofs;
    }

    private static string GetObjectName(string objectFilePath) =>
        IsSourceFile(objectFilePath) ?
            (Path.GetFileNameWithoutExtension(objectFilePath) + ".o") :
            Path.GetFileName(objectFilePath);
        
    private static bool TryAddOrUpdateObjectFiles(
        string archiveFilePath,
        string outputArchiveFilePath,
        string[] objectFilePaths,
        bool isDryrun,
        out string[] outputObjectNames)
    {
        if (!isDryrun)
        {
            var objectNames = objectFilePaths.
                Select(GetObjectName).
                ToArray();
            
            using var outputArchive = ZipFile.Open(
                outputArchiveFilePath,
                ZipArchiveMode.Create,
                Encoding.UTF8);
            
            using var readArchive = File.Exists(archiveFilePath) ?
                ZipFile.Open(
                    archiveFilePath,
                    ZipArchiveMode.Read,
                    Encoding.UTF8) :
                null;

            var outputObjectNameList = new List<string>();
            var isReplaced = new bool[objectFilePaths.Length];
      
            foreach (var readEntry in
                 readArchive?.Entries.AsEnumerable() ??
                 CommonUtilities.Empty<ZipArchiveEntry>())
            {
                if (readEntry.Name != ArchiverUtilities.SymbolTableFileName)
                {
                    outputObjectNameList.Add(readEntry.Name);
                
                    var index = Array.IndexOf(objectNames, readEntry.Name);
                    if (index >= 0)
                    {
                        var objectFilePath = objectFilePaths[index];
                        var objectLastWriteTime = File.GetLastWriteTime(objectFilePath);

                        if (objectLastWriteTime > readEntry.LastWriteTime)
                        {
                            var objectName = objectNames[index];

                            var outputEntry = outputArchive.CreateEntry(
                                objectName, CompressionLevel.NoCompression);

                            outputEntry.LastWriteTime = objectLastWriteTime;

                            using var outputStream = outputEntry.Open();
                            using var readStream = OpenObjectStreamToCompressed(objectFilePath);
                        
                            readStream.CopyTo(outputStream);
                            outputStream.Flush();

                            isReplaced[index] = true;
                            continue;
                        }
                    }

                    if (readEntry.Name != ArchiverUtilities.SymbolTableFileName)
                    {
                        var outputEntry = outputArchive.CreateEntry(
                            readEntry.Name, CompressionLevel.NoCompression);

                        outputEntry.LastWriteTime = readEntry.LastWriteTime;

                        using var outputStream = outputEntry.Open();
                        using var readStream = readEntry.Open();
                        
                        readStream.CopyTo(outputStream);
                        outputStream.Flush();
                    }
                }
            }

            for (var index = 0; index < objectFilePaths.Length; index++)
            {
                if (!isReplaced[index])
                {
                    var objectFilePath = objectFilePaths[index];
                    var objectLastWriteTime = File.GetLastWriteTime(objectFilePath);

                    var objectName = objectNames[index];
                    outputObjectNameList.Add(objectName);

                    var outputEntry = outputArchive.CreateEntry(
                        objectName, CompressionLevel.NoCompression);

                    outputEntry.LastWriteTime = objectLastWriteTime;

                    using var outputStream = outputEntry.Open();
                    using var readStream = OpenObjectStreamToCompressed(objectFilePath);
                        
                    readStream.CopyTo(outputStream);
                    outputStream.Flush();
                }
            }
            
            outputObjectNames = outputObjectNameList.
                ToArray();
            return readArchive == null;
        }
        else
        {
            outputObjectNames = objectFilePaths.
                Select(GetObjectName).
                ToArray();
            return false;
        }
    }

    private static SymbolList[] GetSymbolLists(
        string archiveFilePath,
        string[] objectNames,
        bool isDryrun)
    {
        var symbolLists = new SymbolList[objectNames.Length]!;

        if (!isDryrun || File.Exists(archiveFilePath))
        {
            Parallel.ForEach(objectNames,
                new() { MaxDegreeOfParallelism = Math.Max(1, Environment.ProcessorCount - 1) },
                (objectName, _, index) =>
                {
                    if (ArchiverUtilities.TryOpenArchivedObject(
                        archiveFilePath,
                        objectName,
                        true,
                        out var stream))
                    {
                        using var _s = stream;
                        
                        var symbols = ArchiverUtilities.EnumerateSymbolsFromObjectFile(stream).
                            ToArray();

                        symbolLists[index] = new(objectName, symbols);
                    }
                });
        }
        else
        {
            for (var index = 0; index < symbolLists.Length; index++)
            {
                symbolLists[index] = new(objectNames[index], CommonUtilities.Empty<Symbol>());
            }
        }

        return symbolLists;
    }

    private static void AddSymbolTable(
        string archiveFilePath,
        SymbolList[] symbolLists,
        bool isDryrun)
    {
        if (!isDryrun)
        {
            using var archive = ZipFile.Open(
                archiveFilePath,
                ZipArchiveMode.Update,
                Encoding.UTF8);

            var symbolTableEntry = archive.CreateEntry(
                ArchiverUtilities.SymbolTableFileName,
                CompressionLevel.Optimal);
        
            using var outputStream = symbolTableEntry.Open();

            SymbolUtilities.WriteSymbolTable(outputStream, symbolLists);
        }
        else
        {
            SymbolUtilities.WriteSymbolTable(new MemoryStream(), symbolLists);
        }
    }

    internal bool AddOrUpdate(
        string archiveFilePath,
        string[] objectFilePaths,
        bool isCreateSymbolTable,
        bool isDryrun)
    {
        var outputArchiveFilePath = archiveFilePath + $"_{Guid.NewGuid():N}";

        try
        {
            if (TryAddOrUpdateObjectFiles(
                archiveFilePath,
                outputArchiveFilePath,
                objectFilePaths,
                isDryrun,
                out var outputObjectNames))
            {
                var symbolLists = GetSymbolLists(
                    outputArchiveFilePath,
                    outputObjectNames,
                    isDryrun);

                if (isCreateSymbolTable)
                {
                    AddSymbolTable(
                        outputArchiveFilePath,
                        symbolLists,
                        isDryrun);
                }
            
                File.Delete(archiveFilePath);
                File.Move(outputArchiveFilePath, archiveFilePath);

                return true;
            }
        }
        catch
        {
            File.Delete(outputArchiveFilePath);
            throw;
        }

        return false;
    }

    internal void Extract(
        string archiveFilePath,
        string[] objectNames,
        bool isDryrun)
    {
        if (!isDryrun || File.Exists(archiveFilePath))
        {
            Parallel.ForEach(objectNames,
                new() { MaxDegreeOfParallelism = Math.Max(1, Environment.ProcessorCount - 1) },
                objectName =>
                {
                    if (ArchiverUtilities.TryOpenArchivedObject(
                        archiveFilePath,
                        objectName,
                        false,
                        out var inputStream))
                    {
                        using var outputStream = isDryrun ?
                            new MemoryStream() :
                            StreamUtilities.OpenStream(objectName, true);
    
                        inputStream.CopyTo(outputStream);
                        outputStream.Flush();
                    }
                    else
                    {
                        this.logger.Error($"Object is not found: {objectName}");
                    }
                });
        }
    }

    internal void List(
        string archiveFilePath,
        string[] objectNames)
    {
        if (objectNames.Length >= 1)
        {
            var existObjectNames = new HashSet<string>(
                ArchiverUtilities.EnumerateArchivedObjectNames(archiveFilePath));

            foreach (var objectName in objectNames.
                Where(existObjectNames.Contains))
            {
                Console.WriteLine(objectName);
            }
        }
        else
        {
            foreach (var objectName in
                ArchiverUtilities.EnumerateArchivedObjectNames(archiveFilePath))
            {
                Console.WriteLine(objectName);
            }
        }
    }

    public void Archive(CliOptions options)
    {
        switch (options.Mode)
        {
            case ArchiveModes.AddOrUpdate:
                if (this.AddOrUpdate(
                    options.ArchiveFilePath,
                    options.ObjectNames.ToArray(),
                    options.IsCreateSymbolTable,
                    options.IsDryRun) &&
                    !options.IsSilent)
                {
                    this.logger.Information($"creating {Path.GetFileName(options.ArchiveFilePath)}");
                }
                break;
            case ArchiveModes.Extract:
                this.Extract(
                    options.ArchiveFilePath,
                    options.ObjectNames.ToArray(),
                    options.IsDryRun);
                break;
            case ArchiveModes.List:
                this.List(
                    options.ArchiveFilePath,
                    options.ObjectNames.ToArray());
                break;
            default:
                throw new NotImplementedException();
        }
    }
}
