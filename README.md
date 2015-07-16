# wl-init #

Some Wolfram Language (a.k.a. Mathematica) packages that make things easier

* `AllowTrailingComma.m`: Drop every trailing comma in `RowBox`.
  Only boxes interface. For some reason, `$PreRead` doesn’t work in text-based interface at all!
  Also, the package allows some constructs (`Module`, `With`, `Block`, `Function`)
  have empty (`Null`) elements in their first argument.
  In particular, trailing and leading commas are ignored.

* `DifferentialD.m`: Prettyprint `Dt[x]` as `ⅆx`.
  Conversely, interpret `ⅆx` as `Dt[x]`.

* `ForEach.m`: Another syntax for `Do` loop or `Table` list comprehension
  with an iterator in the first argument.

* `FormatUsage.m`: A ``Private`FormatUsage`` function that is used in other packages to, yes, format usage messages.
  Later, I found ``Macros`SetUsage`` but it didn’t satisfy me.

* `UsageOnly.m`: A ``Private`UsageOnly[symb]`` auxillary function that is used in some packages
  to indicate that the symbol does nothing but holds `symb::usage` message.
  It may be used in a package that does not introduce valuable eponymous symbol.

* `IfThenElse.m`: The `If`’s syntax makes code be very confusing
  when used not as ternary operator with several-word branches
  but as conditional statement with large branches. Else branch starts somewhere below with a tiny comma (one of many).
  `If [test] Then [ifTrue] Else [ifFalse]` is much easier to read.
  Also, `IfQ` is added to surely choose bwtween branches;
  it is guaranteed to be evaluated to one of the branches,
  unlike `If` which stays unevaluated unless `test` evaluates to boolean.

* `QuantityInput.m`: Simple input of `Quantity[5, "cm"]` as `5::cm`.

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

* `ThrowGeneral.m`: Two different forms of `Throw` & `Catch` — tagged and untagged — is a headache.
  They are unrelated: tagged `Catch` can’t catch untagged `Throw` and vice versa.
  So, both `Catch[ Throw[expr, tag] ]` and `Catch[ Throw[expr] , _]` catch nothing!
  This package makes a tag be used implicitly: the untagged versions are simply redirected to the tagged ones.
  `Catch[expr]` catches any thrown thing; `Throw[expr]` uses `General` tag.

* `TypeSystemEither.m`: ``TypeSystem`Either`` union type is declared but not working (at least, in 10.0.1).
  This is a minor fix just to type check via ``TypeSystem`ConformsQ``.
  Complex things with `Dataset` are not considered.
