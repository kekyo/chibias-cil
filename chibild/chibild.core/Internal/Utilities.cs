﻿/////////////////////////////////////////////////////////////////////////////////////
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
    public static readonly bool IsInWindows =
        Environment.OSVersion.Platform == PlatformID.Win32NT;

    public const int EINTR = 4;
    
    [DllImport("libc", SetLastError = true)]
    public static extern int chmod(string path, chmodFlags mode);

#if NET40 || NET45
    private static class ArrayEmpty<T>
    {
        public static readonly T[] Empty = new T[0];
    }

    public static T[] Empty<T>() =>
        ArrayEmpty<T>.Empty;
#else
    public static T[] Empty<T>() =>
        Array.Empty<T>();
#endif

    public static string GetDirectoryPath(string path) =>
        Path.GetDirectoryName(path) is { } d ?
            Path.GetFullPath(string.IsNullOrWhiteSpace(d) ? "." : d) :
            Path.DirectorySeparatorChar.ToString();

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
}