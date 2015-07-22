BeginPackage["SequenceParse`", {"FormatUsage`", "UsageOnly`"}];

SequenceParse::usage = "`\[EmptySet]\[EmptySet]`, `()`, `(1,)`, `(1,2)`, `(1,2,3,)` are now parsed as `Sequence`s.
Only boxes interface. For some reason, `$PreRead` doesn't work in text-based interface at all!
\[EmptySet]\[EmptySet] is converted to `Sequence[]` even in text interface as it is based on `$Pre`, not `$PreRead`." //Private`FormatUsage;

Private`UsageOnly[SequenceParse];

Begin["`Private`"];

(* Where could it possible be bad? *)
(* If it appears buggy I’ll remove it. *)
$rules = {Rule, RuleDelayed};
SetAttributes[#, SequenceHold]& /@ $rules;


(* Assumes SetAttributes[Rule, SequenceHold], as set above *)
If[$Pre === Unevaluated[$Pre], $Pre = Identity];
SetAttributes[emptyPre, HoldAllComplete];
emptyPre[expr___] := Unevaluated @ expr /. HoldPattern[System`\[EmptySet]\[EmptySet]] -> Sequence[];
$Pre = emptyPre ~Composition~ $Pre;

comma = "," | "\[InvisibleComma]";
If[$PreRead === Unevaluated[$PreRead], $PreRead = Identity];
$PreRead = (Replace[#, RowBox[{"(", ")"}] -> "Sequence[]", {0, Infinity}] &) ~Composition~ $PreRead;
$PreRead = (Replace[#, RowBox[{"(",
    RowBox[{ first_, rest: PatternSequence[comma.., _].., comma|PatternSequence[] }],
  ")"}] :> RowBox[{"Sequence", "[", RowBox[{first, rest}], "]"}], {0, Infinity}] &) ~Composition~ $PreRead;
$PreRead = (Replace[#, RowBox[{"(",
    RowBox[{ first_, comma.. }],
  ")"}] :> RowBox[{"Sequence", "[", RowBox[{first}], "]"}], {0, Infinity}] &) ~Composition~ $PreRead;

(* Display sequences *)
MakeBoxes[Sequence[args___], form_] :=
  TemplateBox[MakeBoxes[#, form]& /@ {args}, "Sequence",
    DisplayFunction -> (Evaluate[
      RowBox[{"(", "\[NegativeThinSpace]",
        RowBox[{"(",
          RowBox[{TemplateSlotSequence[1, ","]}],
        ")"}],
      "\[NegativeThinSpace]", ")"}]
    ]&)
  ];

End[];

EndPackage[];
