BeginPackage["DifferentialD`"];

DifferentialD::usage = StringDrop[DifferentialD::usage, -1] <> "and is the same as \!\(\*\
RowBox[{\" Dt \", \"[\", StyleBox[\"x\", \"TI\"], \"]\"}]\)";

Begin["`Private`"];

(* Display Dt[x] as dx (actually, ⅆx, or \[DifferentialD]x) *)
MakeBoxes[Dt[f_], form_] := BasicDtForm[Parenthesize[f, form, 625, 1]];
SetAttributes[BasicDtForm, HoldAllComplete];
BasicDtForm[f_] := TemplateBox[
  {f},
  "Dt", Tooltip -> Automatic,
  DisplayFunction -> (RowBox[{"\[DifferentialD]", #}]&)];
MakeBoxes[Dt[f_, x_], form_] := TemplateBox[
  {MakeBoxes[f, form], MakeBoxes[x, form]},
  "Dt", Tooltip -> Automatic,
  DisplayFunction -> (FractionBox[RowBox[{"\[DifferentialD]", #}], RowBox[{"\[DifferentialD]", #2}]]&)];

(* Show f[x] dx instead of ugly dx f[x]. *)
MakeBoxes[factor_ Dt[x_], form_] := RowBox[{MakeBoxes[factor, form], MakeBoxes[Dt[x], form]}];

(* d(x^2) -- parentheses are mandatory *)
MakeBoxes[Dt[x_^a_], form_] := BasicDtForm[RowBox[{"(", MakeBoxes[x^a, form], ")"}]];

DifferentialD = Dt;

End[];

EndPackage[];
