/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using chibicc.toolchain.Generating;
using chibicc.toolchain.Internal;
using chibicc.toolchain.Parsing;
using chibicc.toolchain.Tokenizing;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.IO.Compression;
using System.Linq;
using System.Text;

namespace chibicc.toolchain.Archiving;

public static class ArchiverUtilities
{
    public static readonly string SymbolTableFileName = "__.SYMDEF";
    
    private enum ObjectSymbolStates
    {
        Idle,
        Enumeration,
        Structure,
    }
    
    private static IEnumerable<Symbol> InternalEnumerateSymbols(
        TextReader tr)
    {
        var state = ObjectSymbolStates.Idle;
        string? currentDirective = null;
        string? currentScope = null;
        string? currentName = null;
        var members = new List<Token[]>();

        foreach (var tokens in CilTokenizer.TokenizeAll("", SymbolTableFileName, tr))
        {
            if (tokens.Length >= 3 &&
                tokens[0] is (TokenTypes.Directive, var directive and ("function" or "global" or "enumeration" or "structure")) &&
                tokens[1] is (TokenTypes.Identity, var scope) &&
                CommonUtilities.TryParseEnum<Scopes>(scope, out _))
            {
                switch (state)
                {
                    case ObjectSymbolStates.Enumeration:
                    case ObjectSymbolStates.Structure:
                        Debug.Assert(currentDirective != null);
                        Debug.Assert(currentScope != null);
                        Debug.Assert(currentName != null);
                        yield return new(
                            currentDirective!,
                            currentScope!,
                            currentName!,
                            members.Count);
                        state = ObjectSymbolStates.Idle;
                        currentScope = null;
                        currentName = null;
                        currentDirective = null;
                        members.Clear();
                        break;
                }
                
                switch (directive)
                {
                    // .function public int32() funcfoo
                    // .global public int32 varbar
                    case "function":
                    case "global":
                        if (tokens.Length >= 4 &&
                            tokens[3] is (TokenTypes.Identity, var name1))
                        {
                            yield return new(directive, scope, name1, null);
                        }
                        break;
                    // .enumeration public int32 enumbaz
                    case "enumeration":
                        if (tokens.Length >= 4 &&
                            tokens[3] is (TokenTypes.Identity, var name2))
                        {
                            currentDirective = directive;
                            currentScope = scope;
                            currentName = name2;
                            members.Clear();
                            state = ObjectSymbolStates.Enumeration;
                        }
                        break;
                    // .structure public structhoge
                    case "structure":
                        if (tokens[2] is (TokenTypes.Identity, var name3))
                        {
                            currentDirective = directive;
                            currentScope = scope;
                            currentName = name3;
                            members.Clear();
                            state = ObjectSymbolStates.Structure;
                        }
                        break;
                }
            }
            else if (state != ObjectSymbolStates.Idle &&
                tokens.Length >= 1 &&
                tokens[0] is (TokenTypes.Identity, _))
            {
                members.Add(tokens);
            }
        }
        
        switch (state)
        {
            case ObjectSymbolStates.Enumeration:
            case ObjectSymbolStates.Structure:
                Debug.Assert(currentDirective != null);
                Debug.Assert(currentScope != null);
                Debug.Assert(currentName != null);
                yield return new(
                    currentDirective!,
                    currentScope!,
                    currentName!,
                    members.Count);
                state = ObjectSymbolStates.Idle;
                break;
        }
    }

    public static IEnumerable<Symbol> EnumerateSymbolsFromObjectFile(
        Stream objectFileStream)
    {
        var tr = new StreamReader(objectFileStream, Encoding.UTF8, true);
        return InternalEnumerateSymbols(tr).
            Distinct();
    }
    
    public static IEnumerable<SymbolList> EnumerateSymbolTable(
        string archiveFilePath)
    {
        using var archive = ZipFile.Open(
            archiveFilePath,
            ZipArchiveMode.Read,
            Encoding.UTF8);

        if (archive.GetEntry(SymbolTableFileName) is { } entry)
        {
            using var stream = entry.Open();
            var tr = new StreamReader(stream, Encoding.UTF8, true);

            foreach (var symbolList in
                SymbolUtilities.EnumerateSymbolTable(tr, SymbolTableFileName))
            {
                yield return symbolList;
            }
        }
    }

    public static IEnumerable<string> EnumerateArchivedObjectNames(
        string archiveFilePath)
    {
        using var archive = ZipFile.Open(
            archiveFilePath,
            ZipArchiveMode.Read,
            Encoding.UTF8);

        foreach (var entry in archive.Entries.
            Where(entry => entry.Name != SymbolTableFileName))
        {
            yield return entry.Name;
        }
    }

    public static bool TryOpenArchivedObject(
        string archiveFilePath,
        string objectName,
        bool is_decoded_body,
        out Stream stream)
    {
        var archive = ZipFile.Open(
            archiveFilePath,
            ZipArchiveMode.Read,
            Encoding.UTF8);

        try
        {
            if (archive.GetEntry(objectName) is { } entry)
            {
                var ofs = is_decoded_body ?
                    new GZipStream(entry.Open(), CompressionMode.Decompress) :
                    entry.Open();

                stream = new ArchiveObjectStream(archive, ofs);
                return true;
            }
            else
            {
                archive.Dispose();
                stream = null!;
                return false;
            }
        }
        catch
        {
            archive.Dispose();
            throw;
        }
    }
}
