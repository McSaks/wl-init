BeginPackage["ThrowGeneral`"];

ThrowGeneral::usage = "Two different forms of `Throw` & `Catch` is a headache.
Both `Catch[ Throw[_expr_, _tag_] ]` and `Catch[ Throw[_expr_] , /_]` catch nothing!
This package makes a tag be used implicitly." //Private`FormatUsage;
Private`UsageOnly[ThrowGeneral];

Begin["`Private`"];

Unprotect /@ {Throw, Catch};
(* Throw[ex_] := Throw[ex, General]; *) (* default tag is General (why not?) *)
(* Catch[ex_] := Catch[ex, _]; *) (* catching w/o a tag is catching with any tag (isn't it reasonable?) *)
Protect /@ {Throw, Catch};

End[];

EndPackage[];
