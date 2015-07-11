(* ::Package:: *)

(** User Mathematica initialization file **)

Begin["System`Private`"];

<< FormatUsage.m

<< UsageOnly.m

<< QuantityInput.m

System`RunShell[cmd___String] := RunProcess[{"zsh", "-c", StringJoin@Riffle[{cmd}, " "]}, "StandardOutput"];
System`RunShell[___] := $Failed;

System`KeyList[list___][assn_] := assn[[#]] & /@ {list};


<< DifferentialD.m

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

<< SwitchPattern.m

System`PlotThemeOptions[theme_, smbl_:Plot] := Last /@ System`PlotThemeDump`resolvePlotTheme[theme, smbl];
System`ShowPlotTheme[theme_, smbl_:Plot] := Misc`EvalOnce @ System`PlotThemeDump`resolvePlotTheme[theme, smbl];
System`FilterThemeOptions[theme_, smbl_:Plot, dest_:Graphics] := FilterRules[System`PlotThemeOptions[theme, smbl], Options @ dest];

System`SortN[list_] := Sort[list, If[#1 < #2, True, False, OrderedQ[{#1, #2}]] &];

System`ToUnit[unit_][quantity_] := UnitConvert[quantity, unit];
AppendTo[ UpValues@System`ToUnit, HoldPattern[MessageName[System`ToUnit, unit_]] :> System`ToUnit[unit] ];

(* to be used as the value of ComplexityFunction option *)
System`SymbolComplexity[expr_] := Total[Last /@ Tally[Cases[expr, _Symbol, Infinity]]];

(* AtEnd[doFinally] [ some;scope ]
SetAttributes[System`AtEnd, HoldFirst];
System`AtEnd[finally_] := Function[{body}, First@{body, finally}, {HoldFirst}]; *)

(* Scope[ ...; ScopeExit[ release resources ]; ... ]
 * like D’s scope(return) { release resources }. *)
SetAttributes[System`Scope, HoldAll];
Scope[{vars___}, body_] := Scope[Module[{vars}, body]];
(* Old stable version, only ScopeSuccess behaviour:
Scope[body_] :=
  With[{o =
      Reap[
        Catch[
          Sow[Null, Scope]; (* sowing dummy expr *)
          body
        , Scope]
      , Scope] },
    ReleaseHold @ Reverse @ Last @ Last @ o; (* ⟵ evaluate 'finally' upward *)
    First @ o (* ⟵ return result *)
  ];
*)
Scope[body_] :=
  With[{o =
      Reap[
        Catch[
          Sow[Null, Scope]; (* sowing dummy expr *)
          body
        , ex_, Exception]
      , Scope] },
    ReleaseHold @ Reverse @ Last @ Last @ o; (* ⟵ evaluate 'finally' upward *)
    With[ {f = First @ o},
      If[ Head @ f === Exception,
        If[ Last @ f =!= Scope,
          Throw @@ f, (* ⟵ rethrow *)
          First @ f   (* ⟵ return via ScopeReturn *)
        ],
        f   (* ⟵ return result *)
      ]
    ]
  ];
SetAttributes[System`ScopeExit, HoldFirst];
ScopeExit[finally_] := Sow[Hold[finally], Scope];
System`ScopeReturn[ret_] := Throw[ret, Scope];

(* $SystemShell = "zsh"; *)





(* ::Subsection:: *)
(*IF*)

<< IfThenElse.m


(* ::Subsection:: *)
(*ForEach*)

<< ForEach.m


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
(*Correct some system behaviour*)


<< ThrowGeneral.m


(* Either type is declared but not working *)
TypeSystem`Predicates`PackagePrivate`valid[TypeSystem`Either[types__]] :=
  AnyTrue[{types}, TypeSystem`Predicates`PackagePrivate`valid];
TypeSystem`TypeMap[f_, TypeSystem`Either[types__]] := f /@ TypeSystem`Either[types];
TypeSystem`Validation`PackagePrivate`vtor[TypeSystem`Either[types__], x_] :=
  AnyTrue[{types}, TypeSystem`Validation`PackagePrivate`vtor[#, x] &];


(* ::Subsection:: *)
(*Simple input of Sequence*)

<< SequenceParse.m


(* ::Subsection:: *)
(*Allow trailing comma*)

<< AllowTrailingComma.m


(* ::Subsection:: *)
(*Preload*)

(BeginPackage[#];EndPackage[];)& ~Scan~ {"Internal`", "GeneralUtilities`", "Macros`"};


(* ::Subsection:: *)
(*Settings*)

$CharacterEncoding = "UTF-8";
$HistoryLength = 3;


Unprotect @ $Permissions;
$Permissions = "Public";

SetOptions[SendMail, Get["mail_auth.wl"]] // Quiet;


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
