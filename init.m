(* ::Package:: *)

(** User Mathematica initialization file **)

Begin["System`Private`"];

<< FormatUsage.m

<< UsageOnly.m

<< QuantityInput.m

System`RunShell[cmd___String] := RunProcess[{"zsh", "-c", StringJoin@Riffle[{cmd}, " "]}, "StandardOutput"];
System`RunShell[___] := $Failed;

System`KeyList[list___][assn_] := assn[[#]] & /@ {list};


MakeBoxes[Dt[f_], form_] := TemplateBox[
  {Parenthesize[f, form, 625, 1]},
  "Dt", Tooltip -> Automatic,
  DisplayFunction -> (RowBox[{"\[DifferentialD]", #}]&)];
MakeBoxes[Dt[f_, x_], form_] := TemplateBox[
  {MakeBoxes[f, form], MakeBoxes[x, form]},
  "Dt", Tooltip -> Automatic,
  DisplayFunction -> (FractionBox[RowBox[{"\[DifferentialD]", #}], RowBox[{"\[DifferentialD]", #2}]]&)];


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

Unprotect[System`SwitchPattern];
expr : System`SwitchPattern[x_, ps : PatternSequence[_, _] ...] :=
  Replace[x,
    Append[
      List @@ ReplacePart[
        Partition[Hold[ps], 2],
        {_, 0} -> RuleDelayed],
      _ :> (Message[SwitchPattern::nomatch, HoldForm[expr]]; $Failed)
    ]
  ];
expr : System`SwitchPattern[args___]:=$Failed /; Message[SwitchPattern::narg, HoldForm[expr], Length@Hold@args];
SwitchPattern::nomatch = "Mo match found in `1`";
SwitchPattern::narg = "SwitchPattern must have odd number of arguments (recieved `2`) in `1`";
SetAttributes[System`SwitchPattern, Attributes[Switch]];

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


(* Where could it possible be bad? *)
(* If it appears buggy I’ll remove it. *)
$rules = {Rule, RuleDelayed};
SetAttributes[#, SequenceHold]& /@ $rules;


(* Two different forms of Throw & Catch is a headache.
 * Both Catch[ Throw[expr, tag] ] and Catch[ Throw[expr] , tag] catch nothing!
 * This makes a tag be used implicitly. *)
Unprotect /@ {Throw, Catch};
Throw[ex_] := Throw[ex, General]; (* default tag is General (why not?) *)
Catch[ex_] := Catch[ex, _]; (* catching w/o a tag is catching with any tag (isn't it reasonable?) *)
Protect /@ {Throw, Catch};


(* Either type is declared but not working *)
TypeSystem`Predicates`PackagePrivate`valid[TypeSystem`Either[types__]] :=
  AnyTrue[{types}, TypeSystem`Predicates`PackagePrivate`valid];
TypeSystem`TypeMap[f_, TypeSystem`Either[types__]] := f /@ TypeSystem`Either[types];
TypeSystem`Validation`PackagePrivate`vtor[TypeSystem`Either[types__], x_] :=
  AnyTrue[{types}, TypeSystem`Validation`PackagePrivate`vtor[#, x] &];


(* ::Subsection:: *)
(*Simple input of Sequence*)


(* Assumes SetAttributes[Rule, SequenceHold], as set in the previous subsection *)
If[$Pre === Unevaluated[$Pre], $Pre = Identity];
SetAttributes[emptyPre, HoldAllComplete];
emptyPre[expr___] := Unevaluated @ expr /. HoldPattern[System`\[EmptySet]] -> Sequence[];
$Pre = $Pre /* emptyPre;

(* Only boxes interface. For some reason, $PreRead doesn't work in text-based interface at all! *)
If[$PreRead === Unevaluated[$PreRead], $PreRead = Identity];
$PreRead = $PreRead /* (Replace[#, RowBox[{"(", ")"}] -> "Sequence[]", {0, Infinity}] &);
$PreRead = $PreRead /* (Replace[#, RowBox[{"(",
    RowBox[{ first_, rest: PatternSequence[",".., _].., ","|PatternSequence[] }],
  ")"}] :> RowBox[{"Sequence", "[", RowBox[{first, rest}], "]"}], {0, Infinity}] &);
$PreRead = $PreRead /* (Replace[#, RowBox[{"(",
    RowBox[{ first_, ",".. }],
  ")"}] :> RowBox[{"Sequence", "[", RowBox[{first}], "]"}], {0, Infinity}] &);


(* ::Subsection:: *)
(*Allow trailing comma*)


$PreRead = $PreRead /* (Replace[#, RowBox[{ first__, "," }] :> RowBox[{first}], {0, Infinity}] &);

(* Module[{, a = 1, b = 2,}, body] *)
(*         ^             ^         *)
$blocks = {Module, With, Block, Function};
Unprotect /@ $blocks;
Do[ With[{block = $block},
  $block[{pre___, Null .., post___}, body__] := block[{pre, post}, body]
  ], {$block, $blocks}];
Protect /@ $blocks;


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
