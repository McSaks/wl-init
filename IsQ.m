BeginPackage["IsQ`", {"FormatUsage`"}];

IsQ::usage = "`IsQ[_expr_, _head_]` tests if `_expr_`'s Head is `_head_`.
`IsQ[_head_]` represents an operator form of `IsQ` that can be applied to an expression.
N.B. `/_?IsQ[_head_]` is parsed as `(/_?IsQ)[_head_]`, not as `/_?(IsQ[_head_])`. \
In this particular case, however, one should use `/__head_`." //Private`FormatUsage;

Begin["`Private`"];

IsQ[expr_, head_] := Head[expr] === head;
IsQ[head_][expr_] := IsQ[expr, head];

End[];

EndPackage[];
