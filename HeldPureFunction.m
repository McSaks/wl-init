BeginPackage["HeldPureFunction`", {"FormatUsage`", "UsageOnly`"}];

HeldPureFunction::usage = "`_body_&!` is now interpreted as a function with `HoldAllComplete` attribute." // Private`FormatUsage;
Private`UsageOnly[HeldPureFunction];

Begin["`Private`"];

Unprotect @ Factorial;

body_&! := Function[Null, body, {HoldAllComplete}];
Function[args_, body_]! := Function[args, body, {HoldAllComplete}];

Protect @ Factorial;


End[];

EndPackage[];
