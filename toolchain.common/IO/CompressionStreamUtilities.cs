/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using System;
using System.IO;
using System.IO.Compression;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace chibicc.toolchain.IO;

public static class CompressionStreamUtilities
{
    public static Stream OpenStream(
        string objectFilePath, bool writable)
    {
        var s = StreamUtilities.OpenStream(objectFilePath, writable);
        if (Path.GetExtension(objectFilePath) is not ".s")
        {
            return writable ?
                new GZipStream(s, CompressionLevel.Optimal) :
                new GZipStream(s, CompressionMode.Decompress);
        }
        else
        {
            return s;
        }
    }

    [MethodImpl(MethodImplOptions.NoInlining | MethodImplOptions.NoOptimization)]
    public static Stream OpenEmbeddedStream(
        string objectResourcePath,
        Assembly? assembly = null)
    {
        assembly ??= Assembly.GetCallingAssembly();

        var s = assembly.GetManifestResourceStream(objectResourcePath);
        if (s == null)
        {
            throw new FileNotFoundException(
                $"Assembly={assembly.FullName}, Path={objectResourcePath}");
        }
        if (Path.GetExtension(objectResourcePath) is not ".s")
        {
            return new GZipStream(s, CompressionMode.Decompress);
        }
        else
        {
            return s;
        }
    }
}
