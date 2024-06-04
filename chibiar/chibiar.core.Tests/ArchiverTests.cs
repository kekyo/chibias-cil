/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibicc.toolchain.Archiving;
using chibicc.toolchain.Logging;
using NUnit.Framework;
using System;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using static VerifyNUnit.Verifier;
using static chibiar.ArchiverTestRunner;

namespace chibiar;

[TestFixture]
[Parallelizable(ParallelScope.All)]
public sealed class ArchiverTests
{
    private static async Task VerifySymbolTableAsync(ZipArchive zip)
    {
       var symTableEntry = zip.GetEntry(ArchiverUtilities.SymbolTableFileName)!;
        using var afs = symTableEntry.Open();

        var tr = new StreamReader(afs, Encoding.UTF8, true);
        var actual = await tr.ReadToEndAsync();

        await Verify(actual);
    }
    
    [Test]
    public Task ArchiveOne()
    {
        return RunAsync(async (basePath, logger) =>
        {
            logger.Information($"Test runner BasePath={basePath}");

            var archivePath = Path.Combine(basePath, "output.a");
            
            var archiver = new Archiver(logger);
            var actual = archiver.AddOrUpdate(
                archivePath,
                new[]
                {
                    Path.Combine(ArtifactsBasePath, "parse.o"),
                },
                true,
                false);
            
            Assert.That(actual, Is.True);

            using var zip = ZipFile.OpenRead(archivePath);
            
            Assert.That(
                zip.Entries.Select(e => e.Name),
                Is.EqualTo(new[] { "parse.o", ArchiverUtilities.SymbolTableFileName }));

            await VerifySymbolTableAsync(zip);
        });
    }
    
    [Test]
    public Task ArchiveTwo()
    {
        return RunAsync(async (basePath, logger) =>
        {
            var archivePath = Path.Combine(basePath, "output.a");
            
            var archiver = new Archiver(logger);
            var actual = archiver.AddOrUpdate(
                archivePath,
                new[]
                {
                    Path.Combine(ArtifactsBasePath, "parse.o"),
                    Path.Combine(ArtifactsBasePath, "codegen.o"),
                },
                true,
                false);
            
            Assert.That(actual, Is.True);

            using var zip = ZipFile.OpenRead(archivePath);
            
            Assert.That(
                zip.Entries.Select(e => e.Name),
                Is.EqualTo(new[] { "parse.o", "codegen.o", ArchiverUtilities.SymbolTableFileName }));

            await VerifySymbolTableAsync(zip);
        });
    }
    
    [Test]
    public Task Update()
    {
        return RunAsync(async (basePath, logger) =>
        {
            var archivePath = Path.Combine(basePath, "output.a");
            
            var newCodegenPath = Path.Combine(basePath, "codegen.o");
            
            // It's made for dummy updated codegen.o.
            File.Copy(
                Path.Combine(ArtifactsBasePath, "tokenize.o"),
                newCodegenPath);
      
            var archiver = new Archiver(logger);
            var actual1 = archiver.AddOrUpdate(
                archivePath,
                new[]
                {
                    Path.Combine(ArtifactsBasePath, "parse.o"),
                    Path.Combine(ArtifactsBasePath, "codegen.o"),
                },
                true,
                false);
            
            Assert.That(actual1, Is.True);

            var actual2 = archiver.AddOrUpdate(
                archivePath,
                new[]
                {
                    newCodegenPath,
                },
                true,
                false);
            
            Assert.That(actual2, Is.False);

            using var zip = ZipFile.OpenRead(archivePath);
            
            Assert.That(
                zip.Entries.Select(e => e.Name),
                Is.EqualTo(new[] { "parse.o", "codegen.o", ArchiverUtilities.SymbolTableFileName }));

            await VerifySymbolTableAsync(zip);
        });
    }
}
