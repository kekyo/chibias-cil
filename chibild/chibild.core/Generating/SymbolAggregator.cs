/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibicc.toolchain.Generating;
using chibicc.toolchain.Logging;
using chibild.Internal;
using Mono.Cecil;
using System.Collections.Generic;
using System.Linq;

namespace chibild.Generating;

internal readonly struct AggregatedSymbols
{
    public readonly string ObjectName;
    public readonly Dictionary<string, Symbol> TypeSymbols;
    public readonly Dictionary<string, Symbol> VariableSymbols;
    public readonly Dictionary<string, Symbol> FunctionSymbols;

    public AggregatedSymbols(
        string objectName,
        Dictionary<string, Symbol> typeSymbols,
        Dictionary<string, Symbol> variableSymbols,
        Dictionary<string, Symbol> functionSymbols)
    {
        this.ObjectName = objectName;
        this.TypeSymbols = typeSymbols;
        this.VariableSymbols = variableSymbols;
        this.FunctionSymbols = functionSymbols;
    }
}

internal static class SymbolAggregator
{
    public static IEnumerable<AggregatedSymbols> AggregateSymbolsFromSymbolTable(
        ILogger logger,
        IEnumerable<SymbolList> symbolLists) =>
        symbolLists.Select(symbolList =>
        {
            var symbols = symbolList.Symbols.
                GroupBy(symbol =>
                {
                    switch (symbol.Directive)
                    {
                        case "enumeration": return "type";
                        case "structure": return "type";
                        case "global": return "variable";
                        case "constant": return "variable";
                        case "function": return "function";
                        default:
                            logger.Warning($"Ignored invalid symbol table entry: {symbol.Directive}");
                            return "unknown";
                    }
                }).
                ToDictionary(
                    g => g.Key,
                    g => g.
                        // Takes largest member count.
                        OrderByDescending(symbol => symbol.MemberCount ?? 0).
                        DistinctBy(symbol => symbol.Name).
                        ToDictionary(symbol => symbol.Name));

            var empty = new Dictionary<string, Symbol>();

            return new AggregatedSymbols(
                symbolList.ObjectName,
                symbols.TryGetValue("type", out var types) ? types : empty,
                symbols.TryGetValue("variable", out var variableNames) ? variableNames : empty,
                symbols.TryGetValue("function", out var functionNames) ? functionNames : empty);
        });

    public static AggregatedSymbols CreateSymbolAggregationFromDefinitions(
        string objectName,
        Dictionary<string, TypeDefinition> types,
        Dictionary<string, FieldDefinition> fields,
        Dictionary<string, MethodDefinition[]> methods) =>
        new AggregatedSymbols(
            objectName,
            types.ToDictionary(
                entry => entry.Key,
                entry => new Symbol(
                    entry.Value.IsEnum ? "enumeration" : "structure",
                    entry.Value.IsPublic ? "public" : entry.Value.Namespace == "C.type" ? "internal" : "file",
                    entry.Key,
                    entry.Value.IsEnum ? entry.Value.Fields.Count(f => f is { IsStatic: true, IsInitOnly: true }) : entry.Value.Fields.Count)),
            fields.ToDictionary(
                entry => entry.Key,
                entry => new Symbol(
                    entry.Value.IsInitOnly ? "constant" : "global",
                    entry.Value.IsPublic ? "public" : entry.Value.DeclaringType.FullName is "C.data" or "C.rdata" ? "internal" : "file",
                    entry.Key,
                    null)),
            methods.SelectMany(entries =>
                entries.Value.Select(m => (entries.Key, symbol: new Symbol(
                    "function",
                    m.IsPublic ? "public" : m.DeclaringType.FullName is "C.text" ? "internal" : "file",
                    entries.Key,
                    null)))).
                DistinctBy(entry => entry.Key).   // Omitted overload definitions.
                ToDictionary(
                    entry => entry.Key,
                    entry => entry.symbol));
}
