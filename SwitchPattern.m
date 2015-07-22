BeginPackage["SwitchPattern`", {"FormatUsage`"}];

Unprotect @ SwitchPattern;
SwitchPattern::usage = StringReplace[Switch::usage, "Switch" -> "SwitchPattern"] <> "
Unlike `Switch`, any `_value\_i_` can use named patterns bound in `_form\_i_`.
`SwitchPattern` fails if no match is found. Use universal pattern _ explicitly if needed." //Private`FormatUsage;

Begin["`Private`"];

expr: SwitchPattern[x_, ps: PatternSequence[_, _]...] :=
  Replace[x,
    Append[
      List @@ ReplacePart[
        Partition[Hold[ps], 2],
        {_, 0} -> RuleDelayed],
      _ :> (Message[SwitchPattern::nomatch, HoldForm[expr]]; $Failed)
    ]
  ];
expr: SwitchPattern[args___] /; Message[SwitchPattern::narg, HoldForm[expr], Length @ Hold @ args] = $Failed;
SwitchPattern::nomatch = "Mo match found in `1`";
SwitchPattern::narg = "SwitchPattern must have odd number of arguments (recieved `2`) in `1`";
SetAttributes[SwitchPattern, Attributes[Switch]];

End[];

EndPackage[];
