BeginPackage["ScopeExit`"];

Scope::usage = "`Scope[ ...; ScopeExit[ _release_ ]; ... ]` evaluates scope body and \
at end of scope all `_release_`s in reverse order, no matter whether the scope \
is exited via normal evaluation, throwing an exception, or abortion.
`Scope[{_var\_1_, _var\_2_ = _value_}, _body_]` uses `Module` inside `Scope`.
Inspired by `scope(exit)` statement in D language." //Private`FormatUsage;
ScopeExit::usage = "`ScopeExit[_release_]` collects expressions to be evaluated at end of `Scope`. \
All `_release_`s are evaluated in reverse order i.e. `_release_` in first `ScopeExit` is evaluated last." //Private`FormatUsage;
ScopeReturn::usage = "`ScopeReturn[_expr_]` immediately returns out of `Scope`." //Private`FormatUsage;

Begin["`Private`"];

(* AtEnd[doFinally] [ some;scope ]
SetAttributes[System`AtEnd, HoldFirst];
System`AtEnd[finally_] := Function[{body}, First@{body, finally}, {HoldFirst}]; *)

(* Scope[ ...; ScopeExit[ release resources ]; ... ]
 * like D’s scope(exit) { release resources }. *)
SetAttributes[Scope, HoldAll];
Scope[{vars___}, body_] := Scope[Module[{vars}, body]];
(* Old stable version, only ScopeSuccess behaviour:
Scope[body_] :=
  With[{o =
      Reap[
        Catch[
          Sow[Null, Scope]; (* sowing dummy expr *)
          body
        , Scope]
      , Scope] },
    ReleaseHold @ Reverse @ Last @ Last @ o; (* ⟵ evaluate 'finally' upward *)
    First @ o (* ⟵ return result *)
  ];
*)
Scope[body_] :=
  With[{o =
      Reap[ CheckAbort[
        Catch[
          Sow[Null, Scope]; (* sowing dummy expr *)
          body
        , ex_, Exception]
      , abort], Scope] },
    ReleaseHold @ Reverse @ Last @ Last @ o; (* ⟵ evaluate 'finally' upward *)
    With[ {f = First @ o},
      If[f === abort, Abort[]];
      If[ Head @ f === Exception,
        If[ Last @ f =!= Scope,
          Throw @@ f, (* ⟵ rethrow *)
          First @ f   (* ⟵ return via ScopeReturn *)
        ],
        f   (* ⟵ return result *)
      ]
    ]
  ];
SetAttributes[ScopeExit, HoldFirst];
ScopeExit[finally_] := Sow[Hold[finally], Scope];
ScopeReturn[ret_] := Throw[ret, Scope];

End[];

EndPackage[];
