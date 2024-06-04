/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibicc.toolchain.Logging;
using chibicc.toolchain.Parsing;
using System.IO;
using Mono.Cecil;

namespace chibild.Generating;

internal enum LoadObjectResults
{
    Ignored,
    Loaded,
    CaughtError,
}

internal abstract class InputFragment
{
    public readonly string BaseInputPath;
    public readonly string RelativePath;

    protected InputFragment(
        string baseInputPath,
        string relativePath)
    {
        this.BaseInputPath = baseInputPath;
        this.RelativePath = relativePath;
    }

    public virtual string ObjectName =>
        Path.GetFileNameWithoutExtension(this.RelativePath);

    public virtual string ObjectPath =>
        this.RelativePath;

    //////////////////////////////////////////////////////////////

    public abstract bool ContainsTypeAndSchedule(
        TypeNode type,
        out Scopes scope,
        out int? memberCount);

    public abstract bool ContainsVariableAndSchedule(
        IdentityNode variable,
        out Scopes scope);

    public abstract bool ContainsFunctionAndSchedule(
        IdentityNode function,
        FunctionSignatureNode? signature,
        out Scopes scope);

    //////////////////////////////////////////////////////////////

    public abstract LoadObjectResults LoadObjectIfRequired(
        ILogger logger,
        bool isLocationOriginSource);

    //////////////////////////////////////////////////////////////

    public abstract bool TryGetType(
        TypeNode type,
        ModuleDefinition fallbackModule,
        out TypeReference tr);

    public abstract bool TryGetField(
        IdentityNode variable,
        ModuleDefinition fallbackModule,
        out FieldReference fr);

    public abstract bool TryGetMethod(
        IdentityNode function,
        FunctionSignatureNode? signature,
        ModuleDefinition fallbackModule,
        out MethodReference mr);
}
