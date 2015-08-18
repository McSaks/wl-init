BeginPackage["MethodCall`", {"FormatUsage`", "UsageOnly`"}];

MethodCall::usage = "`_obj_::_meth_[_args_...]` is evaluated as `_meth_[_obj_, _args_...]`, provided that the `_meth_` symbol exists." //Private`FormatUsage;
Private`UsageOnly[MethodCall];

Begin["`Private`"];

Unprotect @ MessageName;
AppendTo[ SubValues@MessageName,
  HoldPattern @ MessageName[sym_, meth_String?NameQ][args___] :> ToExpression[meth][sym, args] ];
Protect @ MessageName;

Off[Message::name];

End[];

EndPackage[];
