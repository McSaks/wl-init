BeginPackage["AllowTrailingComma`"];

AllowTrailingComma::usage = "Drop every trailing comma in `RowBox`.
Only boxes interface. For some reason, `$PreRead` doesn't work in text-based interface at all!
Also, the package allows some constructs (`Module`, `With`, `Block`, `Function`) \
have empty (`Null`) elements in their first argument. \
In particular, trailing and leading commas are ignored." //Private`FormatUsage;

Private`UsageOnly[AllowTrailingComma];

Begin["`Private`"];

If[$PreRead === Unevaluated[$PreRead], $PreRead = Identity];
$PreRead = (ReplaceAll[#, RowBox[{ first__, ","|"\[InvisibleComma]" }] :> RowBox[{first}]] &) ~Composition~ $PreRead;

(* Module[{, a = 1, b = 2,}, body] *)
(*         ^             ^         *)
$blocks = {Module, With, Block, Function};
Unprotect /@ $blocks;
Do[ With[{block = $block},
  $block[{pre___, Null.., post___}, body__] := block[{pre, post}, body]
  ], {$block, $blocks}];
Protect /@ $blocks;


End[];

EndPackage[];
