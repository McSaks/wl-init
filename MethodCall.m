BeginPackage["MethodCall`", {"FormatUsage`", "UsageOnly`"}];

MethodCall::usage = "`_obj_::_meth_[_args_...]` is evaluated as `_meth_[_obj_, _args_...]`, provided that the `_meth_` thisbol exists.
Some functions like `Map`, `Apply`, `Nest`, etc. are treated so that a list (or the like) is an object and function argument takes first place." //Private`FormatUsage;
Private`UsageOnly[MethodCall];

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
PrependTo[ DownValues@MessageName,
  HoldPattern @ MessageName[this_, meth_String?NameQ
    /; MemberQ[reverseArgs, meth]
    ] :> Function[Null, ToExpression[meth][#1, this, ##2], HoldAllComplete]];

Protect @ MessageName;

Off[Message::name];

End[];

EndPackage[];
