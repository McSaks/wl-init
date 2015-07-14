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

* `IfThenElse.m`: The `If`’s syntax makes code be very confusing
  when used not as ternary operator with several-word branches
  but as conditional statement with large branches. Else branch starts somewhere below with a tiny comma (one of many).
  `If [test] Then [ifTrue] Else [ifFalse]` is much easier to read.
  Also, `IfQ` is added to surely choose bwtween branches;
  it is guaranteed to be evaluated to one of the branches,
  unlike `If` which stays unevaluated unless `test` evaluates to boolean.
