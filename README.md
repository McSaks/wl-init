# wl-init #

Some Wolfram Language (a.k.a. Mathematica) packages that make things easier

* `AllowTrailingComma.m`: Drop every trailing comma in `RowBox`.
  Only boxes interface. For some reason, `$PreRead` doesn’t work in text-based interface at all!
  Also, the package allows some constructs (`Module`, `With`, `Block`, `Function`)
  have empty (`Null`) elements in their first argument.
  In particular, trailing and leading commas are ignored.

* `DifferentialD.m`: Prettyprint `Dt[x]` as `ⅆx`.
  Conversely, interpret `ⅆx` as `Dt[x]`.
  
