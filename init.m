(* ::Package:: *)

(** User Mathematica initialization file **)

Begin["System`Private`"];

<< FormatUsage.m

<< UsageOnly.m

<< QuantityInput.m

<< DifferentialD.m

<< SwitchPattern.m

<< ScopeExit.m

<< IfThenElse.m

<< ForEach.m

<< SequenceParse.m

<< AllowTrailingComma.m

If[ $VersionNumber >= 10,
<< TypeSystemEither.m ]


(* (BeginPackage[#];EndPackage[];)& ~Scan~ {"Internal`", "GeneralUtilities`", "Macros`"}; *)
Quiet[
  <<Internal`;
  <<GeneralUtilities`;
  <<Macros`;
]

System`RunShell[cmd___String] := RunProcess[{"zsh", "-c", StringJoin@Riffle[{cmd}, " "]}, "StandardOutput"];
System`RunShell[___] := $Failed;

System`KeyList[list___][assn_] := assn[[#]] & /@ {list};

System`$PlotThemes := System`$PlotThemes = (
  ListPlot[{}, PlotTheme -> Automatic];
  DownValues[System`PlotThemeDump`resolvePlotTheme][[;;, 1, 1, 1]]
);
System`FindPlotTheme[theme_] := (
  System`$PlotThemes;
  DownValues[System`PlotThemeDump`resolvePlotTheme]
    // Select[ !FreeQ[#[[1]], theme ]& ]
) // Quiet;
System`PrintPlotThemes[theme_] :=
  Scan[Print] @
    Replace[System`FindPlotTheme[theme],
      (Verbatim[HoldPattern][patt_] :> expr_) :>
        RawBoxes[
          RowBox[{
            StyleBox[MakeBoxes[patt], FontColor->RGBColor[0.22932000000000002`, 0.32741333333333333`, 0.5995733333333334]],
            ":=",
            MakeBoxes[expr]
          }]
        ],
      {1}] // Quiet

System`DirectoryNames[args___] := Select[FileNames[args], DirectoryQ]

System`ShowColors[c_:"Indexed"]:=Multicolumn[
 Overlay[{ColorData[#, "Image"], 
     Framed[Style[
       Row[{#, Sequence @@ ColorData[#, "AlternateNames"]}, ", "], 
       FontFamily -> "Ubuntu", Bold], 
      Background -> Opacity[.3, White], 
      FrameMargins -> {{2, 2}, {1, 1}}, FrameStyle -> None, 
      RoundingRadius -> 5]}, Alignment -> Center] & /@ 
  ColorData[c], 3, Alignment -> Center]

System`PlotThemeOptions[theme_, smbl_:Plot] := Last /@ System`PlotThemeDump`resolvePlotTheme[theme, smbl];
System`ShowPlotTheme[theme_, smbl_:Plot] := Misc`EvalOnce @ System`PlotThemeDump`resolvePlotTheme[theme, smbl];
System`FilterThemeOptions[theme_, smbl_:Plot, dest_:Graphics] := FilterRules[System`PlotThemeOptions[theme, smbl], Options @ dest];

System`SortN[list_] := Sort[list, If[#1 < #2, True, False, OrderedQ[{#1, #2}]] &];

System`ToUnit[unit_][quantity_] := UnitConvert[quantity, unit];
AppendTo[ UpValues@System`ToUnit, HoldPattern[MessageName[System`ToUnit, unit_]] :> System`ToUnit[unit] ];

(* to be used as the value of ComplexityFunction option *)
System`SymbolComplexity[expr_] := Total[Last /@ Tally[Cases[expr, _Symbol, Infinity]]];

(* $SystemShell = "zsh"; *)






(* ::Subsection:: *)
(*Try/Raise*)

(* To be thoght of:
System`Try::usage = "Try[exceptionForm] [things to try] [if exception cought]";
Try[exForm_] := Function[try, 
   Function[catch, Raise[ex_] := Throw[TryTag@catch, ex]; 
    With[{expr = Catch[try, exForm]}, 
     If[Head@expr === TryTag, First@expr, 
      expr]], {HoldFirst}], {HoldFirst}];
SetAttributes[TryTag, HoldAllComplete]
Format[TryTag[___]] := "\[SkeletonIndicator]"
*)


(* ::Subsection:: *)
(*NotebookPath*)

Unprotect@NotebookPath;
System`NotebookPath[s_String] := FileNameJoin[{NotebookDirectory[], s}];
Protect@NotebookPath;



(* ::Subsection:: *)
(*Settings*)

$CharacterEncoding = "UTF-8";
$HistoryLength = 3;


Unprotect @ $Permissions;
$Permissions = "Public";

auth = Get["mail_auth.wl"] // Quiet;
If[auth =!= $Failed, SetOptions[SendMail, auth]];


(*Unprotect @ Convolve;
SyntaxInformation[Convolve] = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}};
Convolve[f_, g_] := Convolve[f@\[FormalX], g@\[FormalX], \[FormalX], #]&;*)

$Path = Join[$Path,DirectoryNames[Except@"~" ~~ ___, FileNameJoin[{$UserBaseDirectory, "Applications"}]]]
(* AppendTo[$Path, FileNameJoin[{$UserBaseDirectory, "Applications", "LevelScheme"}]] *)

System`Private`GetIfExists[file_] := If[FileExistsQ@file, Get@file];
SetAttributes[System`Private`GetIfExists, Listable];
System`Private`GetIfExists @ {
    FileNameJoin@{#, "Import.m"},
    FileNameJoin@{#, "Export.m"}
  } & /@ DirectoryNames[Except@"~" ~~ ___,
    FileNameJoin[{$UserBaseDirectory, "SystemFiles", "Formats"}]];

MakeBoxes[System`GrammarToken[str_String], form_] :=
  TemplateBox[ {MakeBoxes[str, form]}, "GrammarToken",
    DisplayFunction -> (
      StyleBox[
        RowBox[{
          "\[LeftGuillemet]", "\[InvisibleSpace]",
          StyleBox[#, "GrammarTokenString", ShowStringCharacters -> False],
          "\[InvisibleSpace]", "\[RightGuillemet]"
        }],
        ShowSyntaxStyles -> False
      ]
    &),
    Tooltip -> "GrammarToken"];

End[];
