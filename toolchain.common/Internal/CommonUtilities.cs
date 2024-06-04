/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using System;
using System.Globalization;
using System.IO;

#if NET45 || NET461
using System.Collections.Generic;

namespace System
{
    internal readonly struct ValueTuple<T1, T2>
    {
        public readonly T1 Item1;
        public readonly T2 Item2;

        public ValueTuple(T1 item1, T2 item2)
        {
            this.Item1 = item1;
            this.Item2 = item2;
        }
    }
}

namespace System.Runtime.CompilerServices
{
    [AttributeUsage(
        AttributeTargets.Class | AttributeTargets.Event |
        AttributeTargets.Field | AttributeTargets.Parameter |
        AttributeTargets.Property | AttributeTargets.ReturnValue |
        AttributeTargets.Struct)]
    internal sealed class TupleElementNamesAttribute : Attribute
    {
        public TupleElementNamesAttribute(string?[] transformNames) =>
            this.TransformNames = transformNames;

        public IList<string?> TransformNames { get; }
    }
}
#endif

namespace chibicc.toolchain.Internal
{
    internal static class CommonUtilities
    {
        private static readonly IFormatProvider invariantCulture = CultureInfo.InvariantCulture;

        public static readonly bool IsInWindows =
            Environment.OSVersion.Platform == PlatformID.Win32NT;

        public static string? GetHomePath()
        {
            if (IsInWindows)
            {
                var homeDrive = Environment.GetEnvironmentVariable("HOMEDRIVE");
                var homePath = Environment.GetEnvironmentVariable("HOMEPATH");
                if (!string.IsNullOrWhiteSpace(homeDrive) &&
                    !string.IsNullOrWhiteSpace(homePath))
                {
                    var path1 = Path.GetFullPath(
                        $"{homeDrive}{homePath}".
                        TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar));
                    if (Directory.Exists(path1))
                    {
                        return path1;
                    }
                }
            }

            if (Environment.GetEnvironmentVariable("HOME") is { } path2 &&
                !string.IsNullOrWhiteSpace(path2))
            {
                path2 = Path.GetFullPath(
                    path2.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar));
                if (Directory.Exists(path2))
                {
                    return path2;
                }
            }

            return null;
        }

        public static string GetDirectoryPath(string path) =>
            Path.GetDirectoryName(path) is { } d ?
                Path.GetFullPath(string.IsNullOrWhiteSpace(d) ? "." : d) :
                Path.DirectorySeparatorChar.ToString();

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

        public static bool TryParseUInt8(
            string word,
            out byte value) =>
            byte.TryParse(
                word,
                NumberStyles.Integer,
                invariantCulture,
                out value) ||
            (word.StartsWith("0x") &&
             byte.TryParse(
                 word.Substring(2),
                 NumberStyles.HexNumber,
                 invariantCulture,
                 out value));

        public static bool TryParseInt8(string word, out sbyte value) =>
            sbyte.TryParse(
                word,
                NumberStyles.Integer,
                invariantCulture,
                out value) ||
            (word.StartsWith("0x") &&
             sbyte.TryParse(
                 word.Substring(2),
                 NumberStyles.HexNumber,
                 invariantCulture,
                 out value));

        public static bool TryParseInt16(string word, out short value) =>
            short.TryParse(
                word,
                NumberStyles.Integer,
                invariantCulture,
                out value) ||
            (word.StartsWith("0x") &&
             short.TryParse(
                 word.Substring(2),
                 NumberStyles.HexNumber,
                 invariantCulture,
                 out value));

        public static bool TryParseUInt16(string word, out ushort value) =>
            ushort.TryParse(
                word,
                NumberStyles.Integer,
                invariantCulture,
                out value) ||
            (word.StartsWith("0x") &&
             ushort.TryParse(
                 word.Substring(2),
                 NumberStyles.HexNumber,
                 invariantCulture,
                 out value));

        public static bool TryParseInt32(string word, out int value) =>
            int.TryParse(
                word,
                NumberStyles.Integer,
                invariantCulture,
                out value) ||
            (word.StartsWith("0x") &&
             int.TryParse(
                 word.Substring(2),
                 NumberStyles.HexNumber,
                 invariantCulture,
                 out value));

        public static bool TryParseUInt32(string word, out uint value) =>
            uint.TryParse(
                word,
                NumberStyles.Integer,
                invariantCulture,
                out value) ||
            (word.StartsWith("0x") &&
             uint.TryParse(
                 word.Substring(2),
                 NumberStyles.HexNumber,
                 invariantCulture,
                 out value));

        public static bool TryParseInt64(string word, out long value) =>
            long.TryParse(
                word,
                NumberStyles.Integer,
                invariantCulture,
                out value) ||
            (word.StartsWith("0x") &&
             long.TryParse(
                 word.Substring(2),
                 NumberStyles.HexNumber,
                 invariantCulture,
                 out value));

        public static bool TryParseUInt64(string word, out ulong value) =>
            ulong.TryParse(
                word,
                NumberStyles.Integer,
                invariantCulture,
                out value) ||
            (word.StartsWith("0x") &&
             ulong.TryParse(
                 word.Substring(2),
                 NumberStyles.HexNumber,
                 invariantCulture,
                 out value));

        public static bool TryParseFloat32(string word, out float value)
        {
            switch (word.ToLowerInvariant())
            {
                case "inf":
                case "+inf":
                    value = float.PositiveInfinity;
                    return true;
                case "-inf":
                    value = float.NegativeInfinity;
                    return true;
                case "nan":
                    value = float.NaN;
                    return true;
                default:
                    return float.TryParse(
                        word,
                        NumberStyles.Float,
                        invariantCulture,
                        out value);
            }
        }

        public static bool TryParseFloat64(string word, out double value)
        {
            switch (word.ToLowerInvariant())
            {
                case "inf":
                case "+inf":
                    value = double.PositiveInfinity;
                    return true;
                case "-inf":
                    value = double.NegativeInfinity;
                    return true;
                case "nan":
                    value = double.NaN;
                    return true;
                default:
                    return double.TryParse(
                        word,
                        NumberStyles.Float,
                        invariantCulture,
                        out value);
            }
        }

        public static bool TryParseEnum<TEnum>(string word, out TEnum value)
            where TEnum : struct, Enum =>
            Enum.TryParse(word, true, out value);
    }
}
