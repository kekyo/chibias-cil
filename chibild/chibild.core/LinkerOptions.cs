/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibicc.toolchain.Internal;
using System;
using System.IO;

namespace chibild;

public enum AssemblyTypes
{
    Dll,
    Exe,
    WinExe,
}

public enum TargetWindowsArchitectures
{
    AnyCPU,
    Preferred32Bit,
    X86,
    X64,
    IA64,
    ARM,
    ARMv7,
    ARM64,
}

public enum DebugSymbolTypes
{
    None,
    Portable,
    Embedded,
    Mono,
    WindowsProprietary,
}

[Flags]
public enum AssemblyOptions
{
    None = 0x00,
    DisableJITOptimization = 0x01,
}

public enum RuntimeConfigurationOptions
{
    Omit,
    ProduceCoreCLR,
    ProduceCoreCLRMajorRollForward,
    ProduceCoreCLRMinorRollForward,
    ProduceCoreCLRFeatureRollForward,
    ProduceCoreCLRPatchRollForward,
    ProduceCoreCLRLatestMajorRollForward,
    ProduceCoreCLRLatestMinorRollForward,
    ProduceCoreCLRLatestFeatureRollForward,
    ProduceCoreCLRLatestPatchRollForward,
    ProduceCoreCLRDisableRollForward,
}

public sealed class LinkerCreationOptions
{
    public AssemblyTypes AssemblyType =
        AssemblyTypes.Exe;

    public TargetWindowsArchitectures TargetWindowsArchitecture =
        TargetWindowsArchitectures.AnyCPU;

    public string? CAbiStartUpObjectDirectoryPath =
        default;
    public string EntryPointSymbol =
        "_start";

    public AssemblyOptions AssemblyOptions =
        AssemblyOptions.DisableJITOptimization;

    public Version Version = new(1, 0, 0, 0);
    public TargetFramework TargetFramework = TargetFramework.Default;

    public RuntimeConfigurationOptions RuntimeConfiguration =
        RuntimeConfigurationOptions.ProduceCoreCLRMajorRollForward;

    public string? AppHostTemplatePath = default;
}

//////////////////////////////////////////////////////////////

public abstract class InputReference : IEquatable<InputReference>
{
    bool IEquatable<InputReference>.Equals(InputReference? other) =>
        this.Equals(other);
}

public abstract class ObjectInputReference : InputReference
{
}

public sealed class ObjectFilePathReference : ObjectInputReference
{
    public readonly string RelativePath;

    public ObjectFilePathReference(string relativePath) =>
        this.RelativePath = relativePath;

    public override string ToString() =>
        this.RelativePath;

    private bool Equals(ObjectFilePathReference other) =>
        this.RelativePath == other.RelativePath;

    public override bool Equals(object? obj) =>
        ReferenceEquals(this, obj) ||
        obj is ObjectFilePathReference other &&
        this.Equals(other);

    public override int GetHashCode() =>
        this.RelativePath.GetHashCode();

    public void Deconstruct(out string relativePath) =>
        relativePath = this.RelativePath;
}

public sealed class ObjectReaderReference : ObjectInputReference
{
    public readonly Func<TextReader> Reader;
    public readonly string Identity;

    public ObjectReaderReference(
        string identity, Func<TextReader> reader)
    {
        this.Identity = identity;
        this.Reader = reader;
    }

    public override string ToString() =>
        $"<{this.Identity}>";

    private bool Equals(ObjectReaderReference other) =>
        this.Identity == other.Identity;

    public override bool Equals(object? obj) =>
        ReferenceEquals(this, obj) ||
        obj is ObjectReaderReference other &&
        this.Equals(other);

    public override int GetHashCode() =>
        this.Identity.GetHashCode();

    public void Deconstruct(
        out string identity, out Func<TextReader> reader)
    {
        identity = this.Identity;
        reader = this.Reader;
    }
}

public sealed class LibraryNameReference : InputReference
{
    public readonly string Name;

    public LibraryNameReference(string name) =>
        this.Name = name;

    public override string ToString() =>
        $"-l{this.Name}";

    private bool Equals(LibraryNameReference other) =>
        this.Name == other.Name;

    public override bool Equals(object? obj) =>
        ReferenceEquals(this, obj) ||
        obj is LibraryNameReference other &&
        this.Equals(other);

    public override int GetHashCode() =>
        this.Name.GetHashCode();

    public void Deconstruct(out string name) =>
        name = this.Name;
}

public sealed class LibraryPathReference : InputReference
{
    public readonly string RelativePath;

    public LibraryPathReference(string relativePath) =>
        this.RelativePath = relativePath;

    public override string ToString() =>
        this.RelativePath;

    private bool Equals(LibraryPathReference other) =>
        this.RelativePath == other.RelativePath;

    public override bool Equals(object? obj) =>
        ReferenceEquals(this, obj) ||
        obj is LibraryPathReference other &&
        this.Equals(other);

    public override int GetHashCode() =>
        this.RelativePath.GetHashCode();

    public void Deconstruct(out string relativePath) =>
        relativePath = this.RelativePath;
}

//////////////////////////////////////////////////////////////

public sealed class LinkerOptions
{
    public string[] LibraryReferenceBasePaths =
        CommonUtilities.Empty<string>();

    public DebugSymbolTypes DebugSymbolType =
        DebugSymbolTypes.Embedded;

    public bool IsDeterministic = true;
    public bool ApplyOptimization = false;
    public bool WillCopyRequiredAssemblies = true;

    public LinkerCreationOptions? CreationOptions =
        new();
    
    public string[] PrependExecutionSearchPaths =
        CommonUtilities.Empty<string>();

    public string? CacheBasePath =
        GetDefaultCacheBasePath();

    public bool IsDryRun = false;

    public static string GetDefaultCacheBasePath() =>
        Path.Combine(
            CommonUtilities.GetHomePath() ?? Environment.CurrentDirectory,
            ".cache",
            "chibild-cil",
            "symtab");
}
