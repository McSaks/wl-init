BeginPackage["RangeSyntax`", {"FormatUsage`", "UsageOnly`"}];

RangeSyntax::usage = "Interpret (in boxes) `_x_ = _a_ \[Ellipsis] _b_` as `{_x_, _a_, _b_}` \
to be used in range specification." //Private`FormatUsage;

Private`UsageOnly[RangeSyntax];

Begin["`Private`"];


If[$PreRead === Unevaluated[$PreRead], $PreRead = Identity];
$PreRead = (# /. {
    RowBox[{pre___, x_, "=", RowBox[{a_, "\[Ellipsis]", b_}], post___}] :>
      RowBox[{pre, "{", RowBox[{x, ",", a, ",", b}], "}", post}],
    RowBox[{pre___, x_, "=", RowBox[{a_, "\[Ellipsis]", b_, "\[Ellipsis]", d_}], post___}] :>
      RowBox[{pre, "{", RowBox[{x, ",", a, ",", b, ",", d}], "}", post}]
  } &) ~Composition~ $PreRead;


End[];

EndPackage[];
