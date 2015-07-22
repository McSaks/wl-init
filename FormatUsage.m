
Begin["Private`"];

FormatUsage[str_] := StringReplace[str,
  "`" ~~ Shortest[s__] ~~ "`" :>
  "\!\(\*StyleBox[\(" <> StringReplace[s, {
    RegularExpression["(?!</)_"] ~~
      Shortest[i__] ~~ "\_" ~~ Shortest[num__] ~~
      RegularExpression["(?!</)_"] :>
        "\*StyleBox[\(" <> i <> "\), \"TI\"]\_\*StyleBox[\(" <> num <> "\), \"TR\", \"TraditionalForm\"]",
    RegularExpression["(?!</)_"] ~~
      Shortest[i__] ~~
      RegularExpression["(?!</)_"] :>
        "\*StyleBox[\(" <> i <> "\), \"TI\"]",
    "/_" -> "_",
    "->" -> "\[Rule]",
    "..." -> "\[Ellipsis]"
  }] <> "\),\"\",ShowStringCharacters->True]\)"];

End[];

BeginPackage["FormatUsage`"];
EndPackage[];
