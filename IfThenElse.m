BeginPackage["IfThenElse`"];

IfQ::usage = "`IfQ[_test_, ...]` is equivalent to `If[TrueQ[_test_], ...]" //Private`FormatUsage;

Then::usage = "`If [_test_] Then [_trueBranch_] Else [_falseBranch_]` is equivalent to `If[_test_, _trueBranch_, _falseBranch_]`.
`IfQ` may be used in place of `If`.
Either `Then[...]` or `Else[...]` is omittable (not both), `Null` is returned in its place.
This hack overloads `Times` and is in fact `If[_test_] * Then[_trueBranch_] * Else[_falseBranch_]`, \
so do not break lines at \[OpenCurlyQuote]`*`\[CloseCurlyQuote] in a top-level expression, as it is parsed as three separate expressions." //Private`FormatUsage;

Else::usage = Then::usage;

Begin["`Private`"];

(* IF[test][trueBranch][falseBranch]. Not more readible than builtin If. Will be removed. *)
IfThenElse`IF[___] /; Message[IfThenElse`IF::obsalt, IfThenElse`IF, If] = Null;
IfThenElse`IF[test_] :=
  Function[trueBranch,
    Function[falseBranch,
      System`If[ TrueQ[test], trueBranch, falseBranch],
    {HoldFirst}],
  {HoldFirst}]


(* If [test] Then [trueBranch] Else [falseBranch]. Quite another matter.
 * Either Then[...] or Else[...] is omittable (not both), Null is returned in its place.
 * Wierd things are possible like Else["test == False"] If[test] due to commutativity of Times;
 * avoid them, since it's not readible.
 *
 * Note:
 *   If [test]
 *   Then[
 *     trueBranch
 *   ]
 *   Else[
 *     falseBranch
 *   ]
 * is not parsed as expected in top-level expression, but as three separate expressions.
 * When enclosed in delimeters this code is not an issue.
 * Use something like
 *   If [test] [
 *     trueBranch
 *   ] Else [
 *     falseBranch
 *   ]
 * if needed, to parse it as a single expression. *)
Unprotect[System`If];
SetAttributes[Then, HoldFirst];
SetAttributes[Else, HoldFirst];
SetAttributes[IfQ, HoldRest];
Off[If::argbu];
expr: IfQ[_, _, _, __] /;
  Message[IfQ::argb, IfQ, Length @ Unevaluated @ expr, 1, 3] := $Failed;
SyntaxInformation @ If = {
  "ArgumentsPattern" -> {_, _., _., _.},
  "LocalVariables" -> None,
  "ColorEqualSigns" -> {1, 1} };
SyntaxInformation @ IfQ = {
  "ArgumentsPattern" -> {_, _., _.},
  "LocalVariables" -> None,
  "ColorEqualSigns" -> {1, 1} };
IfQ[test_, trueBranch_, falseBranch_ : Null] := If[TrueQ[test], trueBranch, falseBranch];
If /: If[test_] Then[trueBranch_] Else[falseBranch_] := If[test, trueBranch, falseBranch];
If /: If[test_] Then[trueBranch_] := If[test, trueBranch];
If /: If[test_] Else[falseBranch_] := If[!test, falseBranch];
IfQ /: IfQ[test_] Then[trueBranch_] Else[falseBranch_] := If[TrueQ[test], trueBranch, falseBranch];
IfQ /: IfQ[test_] Then[trueBranch_] := If[TrueQ[test], trueBranch];
IfQ /: IfQ[test_] Else[falseBranch_] := If[!TrueQ[test], falseBranch];
Protect[If];


End[];

EndPackage[];
