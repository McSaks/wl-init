BeginPackage["Associate`"];

Unprotect[Evaluate @ Names[Context[] <> "*"]];

Associate::usage = "Associate[assoc, key -> value] adds entry to assoc or modifies one.
Associate[assoc, <|key -> value, ...|>] doed this repeatedly.";

Begin["`Private`"];


(* Associate[a_?AssociationQ, r: (Rule|RuleDelayed)[k_, v_]] /; KeyExistsQ[a, k] := Insert[a, r, k]; *)
Associate[a_?AssociationQ, r : (Rule | RuleDelayed)[k_, v_]] /; KeyExistsQ[a, k] :=
  Association @ Replace[Normal[a], (Rule | RuleDelayed)[k, _] -> r, 1];
Associate[a: {(Rule|RuleDelayed)[_, _]...}, r : (Rule | RuleDelayed)[k_, v_]] /; KeyExistsQ[a, k] :=
  Association @ Replace[a, (Rule | RuleDelayed)[k, _] -> r, 1];
Associate[a_?AssociationQ, r: (Rule|RuleDelayed)[k_, v_]] /; ! KeyExistsQ[a, k] := Append[a, r];
Associate[r_][a_] := Associate[a, r];

Associate[a: {(Rule|RuleDelayed)[_, _]...}, rest_] := Associate[Association[a], rest];

Associate[a_?AssociationQ, {r: (Rule|RuleDelayed)[k_, v_], rest: (Rule|RuleDelayed)[_, _]...}] := Associate[Associate[a, r], rest];
Associate[a_?AssociationQ, {}] := a;
Associate[a_?AssociationQ, r_?AssociationQ] := Associate[a, Normal[r]];

Associate[] /; Message[Associate::argt, Associate, 0, 2, 3] = $Failed;
Associate[_, _, rest__] /; Message[Associate::argt, Associate, Length@rest + 2, 2, 3] = $Failed;
Associate[a_ /; !AssociationQ[a], __] /; Message[Associate::invrl, a] = $Failed;


End[];

Protect[Evaluate @ Names[Context[] <> "*"]];

EndPackage[];
