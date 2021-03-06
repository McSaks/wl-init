# wl-init #

<!-- language-all: mathematica -->

Some Wolfram Language (a.k.a. Mathematica) packages that make things easier

* `AllowTrailingComma.m`: Drop every trailing comma in `RowBox`.
  Only boxes interface. For some reason, `$PreRead` doesn’t work in text-based interface at all!
  Also, the package allows some constructs (`Module`, `With`, `Block`, `Function`)
  have empty (`Null`) elements in their first argument.
  In particular, trailing and leading commas are ignored.

* `RangeSyntax.m`: Interpret (in boxes) `x = a … b as {x, a, b}` and `x = a … b … d as {x, a, b, d}`
  to be used in range specification.

* `DifferentialD.m`: Prettyprint `Dt[x]` as `ⅆx`.
  Conversely, interpret `ⅆx` as `Dt[x]`.

* `ForEach.m`: Another syntax for `Do` loop or `Table` list comprehension
  with an iterator in the first argument.

* `FormatUsage.m`: A ``Private`FormatUsage`` function that is used in other packages to, yes, format usage messages.
  Later, I found ``Macros`SetUsage`` but it didn’t satisfy me.

* `UsageOnly.m`: A ``Private`UsageOnly[symb]`` auxiliary function that is used in some packages
  to indicate that the symbol does nothing but holds `symb::usage` message.
  It may be used in a package that does not introduce valuable eponymous symbol.

* `IfThenElse.m`: The `If`’s syntax makes code be very confusing
  when used not as ternary operator with several-word branches
  but as conditional statement with large branches. Else branch starts somewhere below with a tiny comma (one of many).
  `If [test] Then [ifTrue] Else [ifFalse]` is much easier to read.
  Also, `IfQ` is added to surely choose between branches;
  it is guaranteed to be evaluated to one of the branches,
  unlike `If` which stays unevaluated unless `test` evaluates to boolean.

* `QuantityInput.m`: Simple input of `Quantity[5, "cm"]` as `5::cm`.

* `MethodCall.m`: OO-style syntactic sugar: `expr::foo[args…]` evaluates to `foo[expr, args…]`.
  Some functions like `Map`, `Apply`, `Nest`, etc. are treated with first two arguments reversed.
  Method calls may be chained, thus
  
          l = {{1, 2, 3}, {4, 5, 6}};
          (l ::Append[{7, 8, 9}]
             ::Apply[Plus, {1}]
             ::Riffle[", and "]
             ::Map[ToString]
             ::StringJoin[])
  gives `"6, and 15, and 24"`.  
  The package may conflict with the ``QuantityInput` `` package, so the code above works only due to `l` being not explicitly (in its unevaluated form) a number or a list of numbers.
  (Also note the parentheses: without them, newlines would terminate expression leading to a syntax error.)
  `expr //$@ foo[args…]` is a version with very low precedence.

* `ScopeExit.m`: Write here, defer evaluation until end of block.
  Useful for assured resources release.
  Inspired by `scope(exit)` in D language.

* `SequenceParse.m`: When in boxes interface, `Sequence`s may be entered in parentheses separated by comma:
  `()` is empty `Sequence[]`, `(1,)` is unary `Sequence[1]`, `(1,2,3)` is `Sequence[1,2,3]`.
  `∅∅` interprets as (not evaluates to) `Sequence[]` even in text interface.

* `SwitchPattern.m` (maybe not the best name, couldn’t think up better):
  System `Switch` function compares its first argument to several pattern forms,
  and evaluates to the value corresponding to the first match.
  The problem is, values cannot use variables (named patterns) bound in pattern forms.
  The `SwitchPattern` function solves this problem.

* `HeldPureFunction.m`: Easily construct anonymous functions with `HoldAllComplete` attribute.
  E.g. `List @@ (Length[Unevaluated@#]&!) /@ Hold[1+1+1, 2+2]` gives `{3, 2}`.

* `ThrowGeneral.m`: Two different forms of `Throw` & `Catch` — tagged and untagged — is a headache.
  They are unrelated: tagged `Catch` can’t catch untagged `Throw` and vice versa.
  So, both `Catch[ Throw[expr, tag] ]` and `Catch[ Throw[expr] , _]` catch nothing!
  This package makes a tag be used implicitly: the untagged versions are simply redirected to the tagged ones.
  `Catch[expr]` catches any thrown thing; `Throw[expr]` uses `General` tag.

* `TypeSystemEither.m`: ``TypeSystem`Either`` union type is declared but not working (at least, in 10.0.1).
  This is a minor fix just to type check via ``TypeSystem`ConformsQ``.
  Complex things with `Dataset` are not considered.

* `IsQ.m`: `IsQ[expr, head]` or `IsQ[head][expr]` is equivalent to `Head[expr] === head`. The same way, `IsntQ[expr, head]` or `IsntQ[head][expr]` is a negation of that.

* `WithNest.m`: The package provide versions of `With` and `Module` — `WithNest` and `ModuleNest`
  in which each local-variable initialisations may refer to previously defined ones.
  E.g. `WithNest[{a = 1, b = -a, c = 42}, b + c]` gives `41`.
  Braces are optional, i.e. one can write `WithNest[a = 1, b = -a, c = 42, b + c]`.
  ``System`Block`` already has the property of consecutive initialisers, so `BlockNest` is provided
  just in consistency and for a braceless form.
  The `*Nest` names are for historical reasons;
  now I doubt they are good enough, but cannot think up better ones.
  ``GeneralUtilities`Where`` provides the same behavior as `WithNest` in a braceless form.

* `TheSymbol.m`: Some single-letter system symbols — `E`, `I`, `O`, `D`, `K`, `C`, `N`, `π` — are protected.
  `TheSymbolE, TheSymbolI, …, TheSymbolPi` are provided that are displayed as a letter and are assignable.
  `TheSymbol[⟨symbol or string⟩]` can be used to be evaluated to one of the above symbols.

* `Associate.m`: `Association` has an undocumented usage as a replace-value/add-key function. There’s no need to use this `Associate` since its functionality is exactly in `Assiciation`.

* `Second.m`: Convenient functions `Second`, `Third`, …, `Tenth` — wrappers of `Part`.
