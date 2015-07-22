BeginPackage["ForEach`", {"FormatUsage`"}];

ForEach::usage = "`ForEach[_var_, _from_, _to_, _[step]_, _body_]`, or \
`ForEach[_var_ -> _from_ ;; _to_ ;; _[step]_, _body_]`, or `ForEach[_var_ -> _list_, _body_]` is another syntax \
for `Do` loop or `Table` list comprehension, depending on the semicolon at body's end.
This syntax keeps `ForEach` and iterator together, which is more readible, especially for a long body.
Compare:
  `ForEach[var -> {one, two, three},`
  \t`Very;`
  \t`long;`
  \t`body;`
  `]`
and
  `Do[`
  \t`Very;`
  \t`long;`
  \t`body;`
  `, {var, {one, two, three}}]`" //Private`FormatUsage;

Begin["`Private`"];

SetAttributes[ForEach, HoldAll];

ForEach[varlist_List, do: CompoundExpression[___, Null]] := Do[do, varlist];
ForEach[var_ -> list_List, do: CompoundExpression[___, Null]] := Do[do, {var, list}];
ForEach[var_, f_, do: CompoundExpression[___, Null]] := Do[do, {var, f}];
ForEach[var_, i_, f_, step: _|PatternSequence[], do: CompoundExpression[___, Null]] := Do[do, {var, i, f, step}];

ForEach[varlist_List, do_] := Table[do, varlist];
ForEach[var_ -> list_List, do_] := Table[do, {var, list}];
ForEach[var_, f_, do_] := Table[do, {var, f}];
ForEach[var_, i_, f_, step: _|PatternSequence[], do_] := Table[do, {var, i, f, step}];
SyntaxInformation[ForEach] = {"LocalVariables" -> {"Table", {1, 1}},
  "ArgumentsPattern" -> {_, _, _., _., _.}};

ForEach[var_ -> i_ ;; f_ ;; step: (_|PatternSequence[]), do_] := ForEach[var, i, f, step, do];
ForEach[var_ -> i_ ;; All ;; step: (_|PatternSequence[]), do_] := ForEach[var, i, Infinity, step, do]; (* Infinite loops should be implemented *)

ForEach[_] /; ArgumentCountQ[ForEach, 1, 2, 5] = $Failed;
ForEach[] /; ArgumentCountQ[ForEach, 0, 2, 5] = $Failed;
ForEach[args: PatternSequence[_, _, _, _, _, __]] /; ArgumentCountQ[ForEach, Length[Hold[args]], 2, 5] = $Failed;
ForEach[expr_, _] /; Message[ForEach::rule, HoldForm[expr]] = $Failed;
ForEach[args: PatternSequence[___]] /; 3 <= Length[Hold[args]] <= 5 && Message[ForEach::args, ForEach] = $Failed;

ForEach::rule = "Tho-argument form of `ForEach` must be either `ForEach[_var_ -> _list_, _body_]` or `ForEach[_list_, _body_]`" //Private`FormatUsage;

End[];

EndPackage[];
