BeginPackage["QuantityInput`", {"FormatUsage`", "UsageOnly`"}];

QuantityInput::usage = "5::cm is now interpreted as Quantity[5, \"cm\"] as 5 is not a symbol.";
Private`UsageOnly[QuantityInput];

Begin["`Private`"];

Unprotect @ MessageName;
AppendTo[ DownValues@MessageName,
  HoldPattern @ MessageName[x : Except[_Symbol], u_String] :> Quantity[x, u] ];
Protect @ MessageName;


End[];

EndPackage[];
