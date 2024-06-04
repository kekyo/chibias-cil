/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibicc.toolchain.Generating;
using chibicc.toolchain.Archiving;
using chibicc.toolchain.Parsing;
using chibicc.toolchain.Logging;
using chibicc.toolchain.Tokenizing;
using chibicc.toolchain.Internal;
using chibild.Internal;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;

namespace chibild.Generating;

internal sealed class ArchivedObjectInputFragment :
    ObjectInputFragment
{
    // This class corresponds to the archived object file.
    // Initially, only the symbol table of the archive is loaded,
    // and the base class information is empty.
    // When a symbol is referenced, it is recorded with `isRequiredLoading`
    // and the information originally needed for the base class is
    // loaded from the archive with `LoadObjectIfRequired()`.

    private enum RequiredStates
    {
        Ignore,
        Required,
        Loaded,
    }

    private readonly string archivedObjectName;

    private readonly Dictionary<string, Symbol> typeSymbols;
    private readonly Dictionary<string, Symbol> variableSymbols;
    private readonly Dictionary<string, Symbol> functionSymbols;

    private GlobalVariableNode[] globalVariables = CommonUtilities.Empty<GlobalVariableNode>();
    private GlobalConstantNode[] globalConstants = CommonUtilities.Empty<GlobalConstantNode>();
    private FunctionDeclarationNode[] functions = CommonUtilities.Empty<FunctionDeclarationNode>();
    private InitializerDeclarationNode[] initializers = CommonUtilities.Empty<InitializerDeclarationNode>();
    private EnumerationNode[] enumerations = CommonUtilities.Empty<EnumerationNode>();
    private StructureNode[] structures = CommonUtilities.Empty<StructureNode>();

    private int requiredState = (int)RequiredStates.Ignore;

    private ArchivedObjectInputFragment(
        string baseInputPath,
        string relativePath,
        AggregatedSymbols extraction) :
        base(baseInputPath, relativePath)
    {
        this.archivedObjectName = extraction.ObjectName;
        this.ObjectName = Path.GetFileNameWithoutExtension(this.archivedObjectName);
        this.ObjectPath = $"{this.archivedObjectName}@{base.ObjectPath}";
        
        this.typeSymbols = extraction.TypeSymbols;
        this.variableSymbols = extraction.VariableSymbols;
        this.functionSymbols = extraction.FunctionSymbols;
    }

    public override string ObjectName { get; }

    public override string ObjectPath { get; }

    public bool IsRequired =>
        this.requiredState == (int)RequiredStates.Required;

    public override string ToString() =>
        $"ArchivedObject: {this.ObjectPath}, State={(RequiredStates)this.requiredState}";

    //////////////////////////////////////////////////////////////

    public override GlobalVariableNode[] GlobalVariables =>
        this.globalVariables;

    public override GlobalConstantNode[] GlobalConstants =>
        this.globalConstants;

    public override FunctionDeclarationNode[] Functions =>
        this.functions;

    public override InitializerDeclarationNode[] Initializers =>
        this.initializers;

    public override EnumerationNode[] Enumerations =>
        this.enumerations;

    public override StructureNode[] Structures =>
        this.structures;

    //////////////////////////////////////////////////////////////

    public override bool ContainsTypeAndSchedule(
        TypeNode type,
        out Scopes scope,
        out int? memberCount)
    {
        if (this.typeSymbols.TryGetValue(type.TypeIdentity, out var ts))
        {
            Interlocked.CompareExchange(
                ref this.requiredState,
                (int)RequiredStates.Required,
                (int)RequiredStates.Ignore);
            CommonUtilities.TryParseEnum(ts.Scope, out scope);
            memberCount = ts.MemberCount;
            return true;
        }
        scope = default;
        memberCount = null;
        return false;
    }

    public override bool ContainsVariableAndSchedule(
        IdentityNode variable,
        out Scopes scope)
    {
        if (this.variableSymbols.TryGetValue(variable.Identity, out var vs))
        {
            Interlocked.CompareExchange(
                ref this.requiredState,
                (int)RequiredStates.Required,
                (int)RequiredStates.Ignore);
            CommonUtilities.TryParseEnum(vs.Scope, out scope);
            return true;
        }
        scope = default;
        return false;
    }

    public override bool ContainsFunctionAndSchedule(
        IdentityNode function,
        FunctionSignatureNode? signature,
        out Scopes scope)
    {
        // Ignored the signature, because contains only CABI functions.
        if (this.functionSymbols.TryGetValue(function.Identity, out var fs))
        {
            Interlocked.CompareExchange(
                ref this.requiredState,
                (int)RequiredStates.Required,
                (int)RequiredStates.Ignore);
            CommonUtilities.TryParseEnum(fs.Scope, out scope);
            return true;
        }
        scope = default;
        return false;
    }

    //////////////////////////////////////////////////////////////

    public override LoadObjectResults LoadObjectIfRequired(
        ILogger logger,
        bool isLocationOriginSource)
    {
        if (Interlocked.CompareExchange(
            ref this.requiredState,
            (int)RequiredStates.Loaded,
            (int)RequiredStates.Required) == (int)RequiredStates.Required)
        {
            logger.Information($"Loading: {this.ObjectPath}");

            if (!ArchiverUtilities.TryOpenArchivedObject(
                Path.Combine(this.BaseInputPath, this.RelativePath),
                this.archivedObjectName,
                true,
                out var stream))
            {
                logger.Error(
                    $"Unable find an object on archive: ObjectName={this.archivedObjectName}, ArchiveFile={this.RelativePath}");
                return LoadObjectResults.CaughtError;
            }

            using var _s = stream;
            var tr = new StreamReader(stream, Encoding.UTF8, true);

            var parser = new CilParser(logger);
            var declarations = parser.Parse(
                CilTokenizer.TokenizeAll(this.BaseInputPath, this.RelativePath, tr),
                isLocationOriginSource).
                ToArray();

            this.globalVariables = declarations.
                OfType<GlobalVariableNode>().
                ToArray();
            this.globalConstants = declarations.
                OfType<GlobalConstantNode>().
                ToArray();
            this.functions = declarations.
                OfType<FunctionDeclarationNode>().
                ToArray();
            this.initializers = declarations.
                OfType<InitializerDeclarationNode>().
                ToArray();
            this.enumerations = declarations.
                OfType<EnumerationNode>().
                OrderByDescending(e => e.Values.Length).
                DistinctBy(e => e.Name).
                ToArray();
            this.structures = declarations.
                OfType<StructureNode>().
                OrderByDescending(s => s.Fields.Length).
                DistinctBy(s => s.Name).
                ToArray();

            return parser.CaughtError ?
                LoadObjectResults.CaughtError :
                LoadObjectResults.Loaded;
        }

        return this.requiredState == (int)RequiredStates.Loaded ?
            LoadObjectResults.Loaded : LoadObjectResults.Ignored;
    }

    public static ArchivedObjectInputFragment[] Load(
        ILogger logger,
        string baseInputPath,
        string relativePath)
    {
        logger.Information($"Loading symbol table: {relativePath}");

        var symbolLists = ArchiverUtilities.EnumerateSymbolTable(
            Path.Combine(baseInputPath, relativePath));

        return SymbolAggregator.AggregateSymbolsFromSymbolTable(logger, symbolLists).
            Select(extraction => new ArchivedObjectInputFragment(
                baseInputPath,
                relativePath,
                extraction)).
            ToArray();
    }
}
