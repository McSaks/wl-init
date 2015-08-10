BeginPackage["TheSymbol`", "FormatUsage`"];

`Private`$symbols = "TheSymbol`TheSymbol" <> #& /@ {"", "E", "I", "O", "D", "K", "C", "N"};

Clear @@ `Private`$symbols;

TheSymbol::usage = "`TheSymbol[_symbol_]` or `TheSymbol[_symbolName_]` is displayed as `_symbol_` but \
is interpreted as a separate object.
When `_symbol_` is one of `E`, `I`, `O`, `D`, `K`, `C`, `N`, or `\[Pi]`, this is evaluated to `TheSymbolE`, etc." //Private`FormatUsage;
TheSymbolE::usage = "`TheSymbolE` is displayed as `E` but interpreted as a separate symbol. \
Useful for a variable `E` that is not interpreted as `\[ExponentialE]`, the exponential constant." //Private`FormatUsage;
TheSymbolI::usage = "`TheSymbolI` is displayed as `I` but interpreted as a separate symbol. \
Useful for a variable `I` that is not interpreted as `\[ImaginaryI] = \@-1`." //Private`FormatUsage;
TheSymbolO::usage = "`TheSymbolO` is displayed as `O` but interpreted as a separate symbol. \
Useful for a variable `O` that is not interpreted as `System'O`, a remainder term of a power series." //Private`FormatUsage//StringReplace[#, "'"->"`"]&;
TheSymbolD::usage = "`TheSymbolD` is displayed as `D` but interpreted as a separate symbol. \
Useful for a variable `D` that is not interpreted as `System'D`, the partial derivative operator." //Private`FormatUsage//StringReplace[#, "'"->"`"]&;
TheSymbolK::usage = "`TheSymbolK` is displayed as `K` but interpreted as a separate symbol. \
Useful for a variable `K` that is not interpreted as `System'K`, a default generic name for a summation index in a symbolic sum." //Private`FormatUsage//StringReplace[#, "'"->"`"]&;
TheSymbolC::usage = "`TheSymbolC` is displayed as `C` but interpreted as a separate symbol. \
Useful for a variable `C` that is not interpreted as `System'C`, a default form for a constant generated in various symbolic computations." //Private`FormatUsage//StringReplace[#, "'"->"`"]&;
TheSymbolN::usage = "`TheSymbolN` is displayed as `N` but interpreted as a separate symbol. \
Useful for a variable `N` that is not interpreted as `System'N`, the converter to a numeric value." //Private`FormatUsage//StringReplace[#, "'"->"`"]&;
TheSymbolPi::usage = "`TheSymbolPi` is displayed as `\[Pi]` but interpreted as a separate symbol. \
Useful for a variable `\[Pi]` that is not interpreted as the \[Pi] constant, the ratio of the circumference of a circle to its diameter." //Private`FormatUsage//StringReplace[#, "'"->"`"]&;

Begin["`Private`"];

MakeBoxes[TheSymbol[s_String?Internal`SymbolNameQ], form_] :=
  TemplateBox[{s}, "TheSymbol",
    DisplayFunction -> (# &),
    InterpretationFunction -> (RowBox[{
      "TheSymbol", "[", ToString[#, InputForm], "]"
    }] &),
    Tooltip -> Automatic];
ToString[TheSymbol[s_String?Internal`SymbolNameQ],
    form: Except[InputForm] | PatternSequence[]] ^:=
  s;

SetAttributes[TheSymbol, HoldFirst];
TheSymbol[s_Symbol] := TheSymbol[Evaluate @ SymbolName @ Unevaluated @ s];
TheSymbol[Complex[0, 1]] := TheSymbolI; (* I is evaluated as Complex[0, 1] and is not asymbol *)

TheSymbol[s: "E" | "I" | "O" | "D" | "K" | "C" | "N" | "Pi"] := Symbol["TheSymbol" <> s];
TheSymbol["\[Pi]"] := TheSymbolPi;

e: TheSymbol[Except[_String]] /; Message[TheSymbol::fnsym, HoldForm[e]] = $Failed;
TheSymbol[] /; Message[TheSymbol::argx, TheSymbol, 0] = $Failed;
TheSymbol[_, args__] /; Message[TheSymbol::argx, TheSymbol, Length @ Hold @ args + 1] = $Failed;

MakeBoxes[TheSymbolE, form_] := TemplateBox[{}, "TheSymbolE",
  DisplayFunction -> ("E" &),
  InterpretationFunction -> ("TheSymbolE" &),
  Tooltip -> Automatic];
ToString[TheSymbolE, form: Except[InputForm] | PatternSequence[]] ^:= "E";

MakeBoxes[TheSymbolI, form_] := TemplateBox[{}, "TheSymbolI",
  DisplayFunction -> ("I" &),
  InterpretationFunction -> ("TheSymbolI" &),
  Tooltip -> Automatic];
ToString[TheSymbolI, form: Except[InputForm] | PatternSequence[]] ^:= "I";

MakeBoxes[TheSymbolO, form_] := TemplateBox[{}, "TheSymbolO",
  DisplayFunction -> ("O" &),
  InterpretationFunction -> ("TheSymbolO" &),
  Tooltip -> Automatic];
ToString[TheSymbolO, form: Except[InputForm] | PatternSequence[]] ^:= "O";

MakeBoxes[TheSymbolD, form_] := TemplateBox[{}, "TheSymbolD",
  DisplayFunction -> ("D" &),
  InterpretationFunction -> ("TheSymbolD" &),
  Tooltip -> Automatic];
ToString[TheSymbolD, form: Except[InputForm] | PatternSequence[]] ^:= "D";

MakeBoxes[TheSymbolK, form_] := TemplateBox[{}, "TheSymbolK",
  DisplayFunction -> ("K" &),
  InterpretationFunction -> ("TheSymbolK" &),
  Tooltip -> Automatic];
ToString[TheSymbolK, form: Except[InputForm] | PatternSequence[]] ^:= "K";

MakeBoxes[TheSymbolC, form_] := TemplateBox[{}, "TheSymbolC",
  DisplayFunction -> ("C" &),
  InterpretationFunction -> ("TheSymbolC" &),
  Tooltip -> Automatic];
ToString[TheSymbolC, form: Except[InputForm] | PatternSequence[]] ^:= "C";

MakeBoxes[TheSymbolN, form_] := TemplateBox[{}, "TheSymbolN",
  DisplayFunction -> ("N" &),
  InterpretationFunction -> ("TheSymbolN" &),
  Tooltip -> Automatic];
ToString[TheSymbolN, form: Except[InputForm] | PatternSequence[]] ^:= "N";

MakeBoxes[TheSymbolPi, form_] := TemplateBox[{}, "TheSymbolPi",
  DisplayFunction -> ("\[Pi]" &),
  InterpretationFunction -> ("TheSymbolPi" &),
  Tooltip -> Automatic];
ToString[TheSymbolN, form: Except[InputForm] | PatternSequence[]] ^:= "\[Pi]";

End[];

EndPackage[];
