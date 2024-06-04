/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibicc.toolchain.Internal;
using chibicc.toolchain.Parsing;
using chibicc.toolchain.Tokenizing;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;

namespace chibicc.toolchain.Generating;

public static class SymbolUtilities
{
    public static void WriteSymbolTable(
        TextWriter tw,
        SymbolList[] symbolLists)
    {
        foreach (var symbolList in symbolLists)
        {
            tw.WriteLine($".object {symbolList.ObjectName}");

            foreach (var symbol in symbolList.Symbols.Distinct())
            {
                tw.WriteLine($"    {symbol.Directive} {symbol.Scope} {symbol.Name}{(symbol.MemberCount is { } mc ? $" {mc}" : "")}");
            }
        }

        tw.Flush();
    }

    public static void WriteSymbolTable(
        Stream symbolTableStream,
        SymbolList[] symbolLists)
    {
        var tw = new StreamWriter(symbolTableStream, Encoding.UTF8);
        WriteSymbolTable(tw, symbolLists);
    }

    public static IEnumerable<SymbolList> EnumerateSymbolTable(
        TextReader tr,
        string symbolTableName)
    {
        Token? currentObjectName = null;
        var symbols = new List<Symbol>();            
        
        foreach (var tokens in CilTokenizer.TokenizeAll("", symbolTableName, tr))
        {
            if (tokens.Length < 2)
            {
                continue;
            }

            switch (tokens[0])
            {
                // .object [objectName]
                case (TokenTypes.Directive, "object")
                    when tokens[1] is (TokenTypes.Identity, _):
                    if (currentObjectName is (_, var objectName))
                    {
                        yield return new(
                            objectName,
                            symbols.Distinct().ToArray());
                    }
                    currentObjectName = tokens[1];
                    symbols.Clear();
                    break;
                // function public funcfoo
                // global public varbar
                // enumeration public enumbaz 3
                // structure public structhoge 5
                case (TokenTypes.Identity, var directive)
                    when tokens.Length >= 3 &&
                        tokens[1] is (TokenTypes.Identity, var scope) &&
                        CommonUtilities.TryParseEnum<Scopes>(scope, out _) &&
                        tokens[2] is (TokenTypes.Identity, var name):
                    if (tokens.Length >= 4 &&
                        tokens[3] is (TokenTypes.Identity, var mc) &&
                        int.TryParse(mc, NumberStyles.Integer, CultureInfo.InvariantCulture, out var memberCount) &&
                        memberCount >= 0)
                    {
                        symbols.Add(new(directive, scope, name, memberCount));
                    }
                    else
                    {
                        symbols.Add(new(directive, scope, name, null));
                    }
                    break;
            }
        }

        if (currentObjectName is var (_, con2))
        {
            yield return new(
                con2,
                symbols.Distinct().ToArray());
        }
    }
}
