BeginPackage["WithNest`", {"FormatUsage`"}];

Unprotect[Evaluate @ Names[Context[] <> "*"]];

WithNest::usage = "`WithNest[{..., _x\_i_ = _expr\_i_, ...}, _body_]` works just like `With` \
but each `_expr\_j_` may refer any preceding `_x\_i_`.
Braces are optional, i.e. `WithNest[..., _x\_i_ = _expr\_i_, ..., _body_]` is valid syntax." // Private`FormatUsage;
ModuleNest::usage = "`ModuleNest[{..., _x\_i_ = _expr\_i_, ...}, _body_]` works just like `Module` \
but each `_expr\_j_` may refer any preceding `_x\_i_`.
Braces are optional, i.e. `ModuleNest[..., _x\_i_ = _expr\_i_, ..., _body_]` is valid syntax." // Private`FormatUsage;
BlockNest::usage = "`BlockNest[{..., _x\_i_ = _expr\_i_, ...}, _body_]` works just like `Block` \
but braces are optional, i.e. `BlockNest[..., _x\_i_ = _expr\_i_, ..., _body_]` is valid syntax.
See also `GeneralUtilities'Match`." // Private`FormatUsage //StringReplace[#, "'" -> "`"]&;

Begin["`Private`"];


WithNest[{eq1_, eq2__}, body_] := With[{eq1}, WithNest[{eq2}, body]];
WithNest[{eq1: (_ | PatternSequence[])}, body_] := With[{eq1}, body];
WithNest[eqs: ((_Set | _SetDelayed)...), body_] := WithNest[{eqs}, body];
SetAttributes[WithNest, HoldAll];
SyntaxInformation[WithNest] = {"LocalVariables" -> {"Table", {1, 1}},
  "ArgumentsPattern" -> {{__}, __}};
WithNest[___] /; Message[WithNest::args, WithNest] = $Failed;

ModuleNest[{eq1_, eq2__}, body_] := Module[{eq1}, ModuleNest[{eq2}, body]];
ModuleNest[{eq1: (_ | PatternSequence[])}, body_] := Module[{eq1}, body];
ModuleNest[eqs: ((_Set | _SetDelayed | _Symbol)...), body_] := ModuleNest[{eqs}, body];
SetAttributes[ModuleNest, HoldAll];
SyntaxInformation[ModuleNest] = {"LocalVariables" -> {"Table", {1, 1}},
  "ArgumentsPattern" -> {{__}, __}};
ModuleNest[___] /; Message[ModuleNest::args, ModuleNest] = $Failed;

BlockNest[eqs_List, body_] := Block[eqs, body];
BlockNest[eqs: ((_Set | _SetDelayed | _Symbol)...), body_] := Block[{eqs}, body];
SetAttributes[BlockNest, HoldAll];
SyntaxInformation[BlockNest] = {"LocalVariables" -> {"Table", {1, 1}},
  "ArgumentsPattern" -> {{__}, __}};
BlockNest[___] /; Message[BlockNest::args, BlockNest] = $Failed;

End[];

Protect[Evaluate @ Names[Context[] <> "*"]];

EndPackage[];
