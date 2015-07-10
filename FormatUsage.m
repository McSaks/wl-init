
Begin["Private`"];

FormatUsage[str_] := StringReplace[str,
  "`" ~~ Shortest[s__] ~~ "`" :>
  "\!\(\*StyleBox[\(" <> StringReplace[s, {
    RegularExpression["(?!</)_"] ~~
      Shortest[i__] ~~
      RegularExpression["(?!</)_"] :>
        "\*StyleBox[\(" <> i <> "\), \"TI\"]",
    "/_" -> "_",
    "->" -> "\[Rule]",
    "..." -> "\[Ellipsis]"
  }] <> "\),\"MR\",ShowStringCharacters->True]\)"];

End[];
