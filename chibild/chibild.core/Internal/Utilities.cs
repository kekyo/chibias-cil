/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;

namespace chibild.Internal;

[Flags]
internal enum chmodFlags
{
    S_IRUSR = 0x100,
    S_IWUSR = 0x80,
    S_IXUSR = 0x40,
    S_IRGRP = 0x20,
    S_IWGRP = 0x10,
    S_IXGRP = 0x8,
    S_IROTH = 0x4,
    S_IWOTH = 0x2,
    S_IXOTH = 0x1,
}

internal static class Utilities
{
    private static readonly HashSet<string> invalidPathStrings = new()
    {
        ":", "//", @"\\",
    };

    public const int EINTR = 4;
    
    [DllImport("libc", SetLastError = true)]
    public static extern int chmod(string path, chmodFlags mode);

#if NETFRAMEWORK || NETSTANDARD2_0
    public static bool TryAdd<TKey, TValue>(
        this Dictionary<TKey, TValue> dict,
        TKey key,
        TValue value)
    {
        if (!dict.ContainsKey(key))
        {
            dict.Add(key, value);
            return true;
        }
        else
        {
            return false;
        }
    }
#endif

#if NET45 || NET461
    public static IEnumerable<T> Prepend<T>(
        this IEnumerable<T> enumerable,
        T value)
    {
        yield return value;
        foreach (var item in enumerable)
        {
            yield return item;
        }
    }
#endif

#if NET45 || NET461 || NETSTANDARD2_0
    public static HashSet<T> ToHashSet<T>(this IEnumerable<T> enumerable) =>
        new(enumerable);
#endif

#if !NET6_0_OR_GREATER
    public static IEnumerable<T> DistinctBy<T, U>(
        this IEnumerable<T> enumerable,
        Func<T, U> selector)
    {
        var taken = new HashSet<U>();
        foreach (var item in enumerable)
        {
            var value = selector(item);
            if (taken.Add(value))
            {
                yield return item;
            }
        }
    }
#endif

    public static bool UpdateBytes(
        MemoryStream ms,
        byte[] targetBytes,
        byte[] replaceBytes)
    {
        var data = ms.GetBuffer();
        var index = 0;
        while (index < ms.Length)
        {
            var targetIndex = 0;
            while (targetIndex < targetBytes.Length)
            {
                if (data[index + targetIndex] != targetBytes[targetIndex])
                {
                    break;
                }
                targetIndex++;
            }
            if (targetIndex >= targetBytes.Length)
            {
                Array.Copy(replaceBytes, 0, data, index, replaceBytes.Length);
                for (var index2 = replaceBytes.Length; index2 < targetBytes.Length; index2++)
                {
                    data[index + index2] = 0x00;
                }
                return true;
            }
            index++;
        }
        return false;
    }

    public static void SafeCopy(string from, string to)
    {
        // Note that although named 'Safe',
        // atomic replacement of files is not realized.
        
        var to1 = Guid.NewGuid().ToString("N");
        var to2 = Guid.NewGuid().ToString("N");
                
        try
        {
            File.Copy(from, to1);
        }
        catch
        {
            File.Delete(to1);
            throw;
        }

        var isExistTo = File.Exists(to);
        if (isExistTo)
        {
            try
            {
                File.Move(to, to2);
            }
            catch
            {
                isExistTo = false;
            }
        }
                
        try
        {
            File.Move(to1, to);
        }
        catch
        {
            File.Delete(to1);
            if (isExistTo)
            {
                try
                {
                    File.Move(to2, to);
                }
                catch
                {
                    File.Delete(to2);
                }
            }
            return;
        }

        if (isExistTo)
        {
            File.Delete(to2);
        }
    }
}
