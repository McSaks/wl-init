BeginPackage["IfThenElse`"];

IfQ::usage = "`IfQ[_condition_, _t_, _f_]` gives `_t_` if `_condition_` evaluates to `True`, and `_f_` otherwise. \
`_f_` is evaluated if `_condition_` evaluates to anything but `True`.
`IfQ[_condition_, _t_]` gives `_t_` if only `_condition_` evaluates to `True`, and `Null` otherwise.
Note that `IfQ[_condition_, _t_, _f_]` and `IfQ[!_condition_, _f_, _t_]` are not equivalent. \
In both cases the third argument is returned unless `_condition_` is neither `True` nor `False`.
`IfQ[_condition_, ...]` is equivalent to `If[TrueQ[_condition_], ...]`.
Unlike `If`, there is no form `\[Null]\^*\[InvisibleSpace]IfQ[_condition_, _t_, _f_, _u_]`, \
because a choice is always made between `_t_` and `_f_`.
`IfQ [_condition_] Then [_t_] Else [_f_]` can also be used." //Private`FormatUsage;

Then::usage = "`If [_condition_] Then [_t_] Else [_f_]` is equivalent to `If[_condition_, _t_, _f_]`.
`IfQ` may be used in place of `If`.
Either `Then[...]` or `Else[...]` is omittable (not both), `Null` is returned in its place.
This hack overloads `Times` and is in fact `If[_condition_] * Then[_t_] * Else[_f_]`, \
so do not break lines at \[OpenCurlyQuote]`*`\[CloseCurlyQuote] in a top-level expression, as it is parsed as three separate expressions." //Private`FormatUsage;

IfThenElse::usage = Else::usage = Then::usage;
IfThenElse::usage = IfThenElse::usage <> Private`FormatUsage @ "
`IfQ[_condition_, _t_, _f_]` gives `_t_` if `_condition_` evaluates to `True`, and `_f_` otherwise.";
Private`UsageOnly[IfThenElse];

Begin["`Private`"];

(* IF[test][t][f]. Not more readible than builtin If. Will be removed. *)
IfThenElse`IF[___] /; Message[IfThenElse`IF::obsalt, IfThenElse`IF, If] = Null;
IfThenElse`IF[test_] :=
  Function[t,
    Function[f,
      System`If[ TrueQ[test], t, f],
    {HoldFirst}],
  {HoldFirst}]


(* If [test] Then [t] Else [f]. Quite another matter.
 * Either Then[...] or Else[...] is omittable (not both), Null is returned in its place.
 * Wierd things are possible like Else["test == False"] If[test] due to commutativity of Times;
 * avoid them, since it's not readible.
 *
 * Note:
 *   If [test]
 *   Then[
 *     t
 *   ]
 *   Else[
 *     f
 *   ]
 * is not parsed as expected in top-level expression, but as three separate expressions.
 * When enclosed in delimeters this code is not an issue.
 * Use something like
 *   If [test] [
 *     t
 *   ] Else [
 *     f
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
IfQ[test_, t_, f_ : Null] := If[TrueQ[test], t, f];
If /: If[test_] Then[t_] Else[f_] := If[test, t, f];
If /: If[test_] Then[t_] := If[test, t];
If /: If[test_] Else[f_] := If[!test, f];
IfQ /: IfQ[test_] Then[t_] Else[f_] := If[TrueQ[test], t, f];
IfQ /: IfQ[test_] Then[t_] := If[TrueQ[test], t];
IfQ /: IfQ[test_] Else[f_] := If[!TrueQ[test], f];
Protect[If];


End[];

EndPackage[];
