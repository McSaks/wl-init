Begin["Private`"];

General::usageonly = "`1` must not be evaluated. It only describes what `1``.` package does.";
UsageOnly::usage = "`UsageOnly[_symbol_]` indicates that the symbol does nothing but holds `_symbol_::usage`.
Must be used after `_symbol_::usage` is set." //Private`FormatUsage;
SetAttributes[UsageOnly, HoldFirst];
UsageOnly[sym_] := (
  sym /; Message[sym::usageonly, HoldForm[sym]] = Null;
  sym::usage = sym::usage <> "\nThis symbol must not be evaluated. It only describes what the package does.";
);

End[];
