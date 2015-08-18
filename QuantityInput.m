BeginPackage["QuantityInput`", {"FormatUsage`", "UsageOnly`"}];

QuantityInput::usage = "5::cm is now interpreted as Quantity[5, \"cm\"] as 5 is a numeric.";
Private`UsageOnly[QuantityInput];

Begin["`Private`"];

NumericList = Numeric | {Numeric..};
Numeric = _Integer | _Rational | _Real | _Complex;

Unprotect @ MessageName;
AppendTo[ DownValues@MessageName,
  With[{n = NumericList}, HoldPattern @ MessageName[x: n, u_String]] :> Quantity[x, u] ];
Protect @ MessageName;


End[];

EndPackage[];
