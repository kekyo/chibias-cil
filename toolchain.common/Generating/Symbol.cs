/////////////////////////////////////////////////////////////////////////////////////
//
// chibicc-toolchain - The specialized backend toolchain for chibicc-cil
// Copyright (c) Kouji Matsui(@kozy_kekyo, @kekyo @mastodon.cloud)
//
// Licensed under MIT: https://opensource.org/licenses/MIT
//
/////////////////////////////////////////////////////////////////////////////////////

using System;
using System.Linq;

namespace chibicc.toolchain.Generating;

public readonly struct Symbol : IEquatable<Symbol>
{
    public readonly string Directive;
    public readonly string Scope;
    public readonly string Name;
    public readonly int? MemberCount;

    public Symbol(
        string directive,
        string scope,
        string name,
        int? memberCount)
    {
        this.Directive = directive;
        this.Scope = scope;
        this.Name = name;
        this.MemberCount = memberCount;
    }

    public bool Equals(Symbol rhs) =>
        this.Directive.Equals(rhs.Directive) &&
        this.Scope.Equals(rhs.Scope) &&
        this.Name.Equals(rhs.Name) &&
        this.MemberCount == rhs.MemberCount;

    public override bool Equals(object? obj) =>
        obj is Symbol rhs && this.Equals(rhs);

    public override int GetHashCode() =>
        this.Directive.GetHashCode() ^
        this.Scope.GetHashCode() ^
        this.Name.GetHashCode() ^
        this.MemberCount?.GetHashCode() ?? 0;

    public void Deconstruct(
        out string directive,
        out string scope,
        out string name,
        out int? memberCount)
    {
        directive = this.Directive;
        scope = this.Scope;
        name = this.Name;
        memberCount = this.MemberCount;
    }

    public override string ToString() =>
        $".{this.Directive} {this.Scope} {this.Name}{(this.MemberCount is { } mc ? $" {mc}" : "")}";
}

public readonly struct SymbolList : IEquatable<SymbolList>
{
    public readonly string ObjectName;
    public readonly Symbol[] Symbols;

    public SymbolList(
        string objectName,
        Symbol[] symbols)
    {
        this.ObjectName = objectName;
        this.Symbols = symbols;
    }

    public bool Equals(SymbolList rhs) =>
        this.ObjectName.Equals(rhs.ObjectName) &&
        this.Symbols.SequenceEqual(rhs.Symbols);

    public override bool Equals(object? obj) =>
        obj is SymbolList rhs && this.Equals(rhs);

    public override int GetHashCode() =>
        this.ObjectName.GetHashCode() ^
        this.Symbols.Aggregate(0, (agg, s) => agg ^ s.GetHashCode());

    public void Deconstruct(
        out string objectName,
        out Symbol[] symbols)
    {
        objectName = this.ObjectName;
        symbols = this.Symbols;
    }

    public override string ToString() =>
        $".{this.ObjectName} SYMBOLS={this.Symbols.Length}";
}

