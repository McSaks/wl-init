BeginPackage["QuantityInput`", {"FormatUsage`", "UsageOnly`"}];

QuantityInput::usage = "5::cm is now interpreted as Quantity[5, \"cm\"] as 5 is a numeric.";
Private`UsageOnly[QuantityInput];

Begin["`Private`"];

NumericListQ[_?NumericQ] = True;
NumericListQ[{__?NumericListQ}] = True;
NumericListQ[_] = False;
SetAttributes[NumericListQ, HoldFirst];

Unprotect @ MessageName;
AppendTo[ DownValues@MessageName,
  HoldPattern @ MessageName[x:_?NumericListQ, u_String] :> Quantity[x, u] ];
Protect @ MessageName;


End[];

EndPackage[];
