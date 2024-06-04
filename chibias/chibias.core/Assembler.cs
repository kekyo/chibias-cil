/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using System;
using chibicc.toolchain.IO;
using chibicc.toolchain.Logging;
using chibicc.toolchain.Parsing;
using chibicc.toolchain.Tokenizing;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using chibicc.toolchain.Internal;

namespace chibias;

public sealed class Assembler
{
    private readonly ILogger logger;

    public Assembler(ILogger logger) =>
        this.logger = logger;

    public bool Assemble(
        string outputObjectFilePath,
        string sourceFilePath,
        bool isDryrun)
    {
        var outputTemporaryFilePath =
            Path.Combine(
                CommonUtilities.GetDirectoryPath(outputObjectFilePath),
                Guid.NewGuid().ToString("N"));
        var count = 0;
        
        var tasks = new[]
        {
            () =>
            {
                // Checking only parsing.
                using (var inputStream = StreamUtilities.OpenStream(sourceFilePath, false))
                {
                    var tr = new StreamReader(inputStream, Encoding.UTF8, true);

                    var parser = new CilParser(this.logger);
                    var _ = parser.Parse(
                        CilTokenizer.TokenizeAll("", sourceFilePath, tr),
                        false).
                        ToArray();

                    if (!parser.CaughtError)
                    {
                        Interlocked.Increment(ref count);
                    }
                }
            },
            () =>
            {
                // Convert source code to object file.
                using var outputStream = isDryrun ?
                    null : CompressionStreamUtilities.OpenStream(outputTemporaryFilePath, true);

                if (outputStream != null)
                {
                    using (var inputStream = StreamUtilities.OpenStream(sourceFilePath, false))
                    {
                        inputStream.CopyTo(outputStream);
                        outputStream.Flush();
                    }
                }

                Interlocked.Increment(ref count);
            },
        };

        try
        {
            Parallel.Invoke(tasks);
        }
        catch
        {
            try
            {
                File.Delete(outputTemporaryFilePath);
            }
            catch
            {
            }
            throw;
        }

        if (!isDryrun)
        {
            if (count == 2)
            {
                try
                {
                    File.Delete(outputObjectFilePath);
                }
                catch
                {
                }
                File.Move(outputTemporaryFilePath, outputObjectFilePath);
            }
            else
            {
                try
                {
                    File.Delete(outputTemporaryFilePath);
                }
                catch
                {
                }
            }
        }
        
        return count == 2;
    }
}
