BeginPackage["MethodCall`", {"FormatUsage`", "UsageOnly`"}];

MethodCall::usage = "`_obj_::_meth_[_args_...]` is evaluated as `_meth_[_obj_, _args_...]`, provided that the `_meth_` symbol exists.
Some functions like `Map`, `Apply`, `Nest`, etc. are treated so that a list (or the like) is an object and function argument takes first place." //Private`FormatUsage;
Private`UsageOnly[MethodCall];

$::usage = "`_obj_\[ThickSpace]//\[InvisibleSpace]$\[InvisibleSpace]@\[ThickSpace]_meth_[``_arg\_1_,` `...]` \
works the same as `_obj_::_meth_[``_arg\_1_,` `...]` but has very low precedence (of `//` postfix application operator).
It is evaluated as `_meth_[_obj_,` `_arg\_1_,` `...]` or\[NonBreakingSpace]\[LongDash] for some functions like \
`Map`, `Apply`, `Nest`, etc., where the first arguments applies somehow on (parts of) the second one\[NonBreakingSpace]\[LongDash] \
as  `_meth_``[_arg\_1_,` `_obj_,` `_args\_2_,` `...]`.
See usage for `MethodCall`" //Private`FormatUsage;

Begin["`Private`"];

Unprotect @ MessageName;

(* General syntax, no hold *)
PrependTo[ SubValues@MessageName,
  HoldPattern @ MessageName[this_, meth_String?NameQ][args___] :> ToExpression[meth][this, args] ];

(* Hold if begins with capital *)
PrependTo[ DownValues@MessageName,
  HoldPattern @ MessageName[this_, meth_String?NameQ
    /; MemberQ[Characters["ABCDEFGHIJKLMNOPQRSTUVWXYZ"], StringTake[meth, 1]]
    ] :> Function[Null, ToExpression[meth][this, ##], HoldAllComplete] ];

(* Reverse argument position in certain functions *)
reverseArgs = ToString /@ {
  Map, MapAll, MapAt, MapIndexed, MapThread, AssociationMap, KeyMap, KeyValueMap,
  Fold, FoldList,
  Nest, NestList, NestWhile, NestWhileList, FixedPoint, FixedPointList,
  Apply, Scan
};
(* Inclusion of AssociationMap in the list above protects MessageName. HOW?! So, unprotect again: *)
Unprotect @ MessageName;
PrependTo[ DownValues@MessageName,
  HoldPattern @ MessageName[this_, meth_String?NameQ
    /; MemberQ[reverseArgs, meth]
    ] :> Function[Null, ToExpression[meth][#1, this, ##2], HoldAllComplete]];

$[meth_Symbol[first_, args___]] /; MemberQ[reverseArgs, SymbolName[meth]] :=
    Function[Null, meth[first, #, args], HoldAllComplete];
$[meth_Symbol[args___]] :=
    Function[Null, meth[#, args], HoldAllComplete];
SetAttributes[$, HoldAllComplete];

Protect @ MessageName;

Off[Message::name];

End[];

EndPackage[];
