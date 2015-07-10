(* ::Package:: *)

(** User Mathematica initialization file **)

Begin["System`Private`"];

<< FormatUsage.m


Unprotect @ MessageName;
AppendTo[ DownValues@MessageName,
  HoldPattern @ MessageName[x : Except[_Symbol], u_String] :> Quantity[x, u] ];
Protect @ MessageName;

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


(* IF[test][trueBranch][falseBranch]. Not more readible than builtin If. Will be removed. *)
System`IF[___] /; Message[System`IF::obsalt, System`IF, If] = Null;
System`IF[test_] :=
  Function[trueBranch,
    Function[falseBranch,
      System`If[ TrueQ[test], trueBranch, falseBranch],
    {HoldFirst}],
  {HoldFirst}]


(* If [test] Then [trueBranch] Else [falseBranch]. Quite another matter.
 * Either Then[...] or Else[...] is omittable (not both), Null is returned in its place.
 * Wierd things are possible like Else["test == False"] If[test] due to commutativity of Times;
 * avoid them, since it's not readible.
 *
 * Note:
 *   If [test]
 *   Then[
 *     trueBranch
 *   ]
 *   Else[
 *     falseBranch
 *   ]
 * is not parsed as expected in top-level expression, but as three separate expression.
 * When enclosed in delimeters this code is not an issue.
 * Use something like
 *   If [test] [
 *     trueBranch
 *   ] Else [
 *     falseBranch
 *   ]
 * if needed, to parse it as a single expression. *)
Unprotect[System`If];
SetAttributes[System`Then, HoldFirst];
SetAttributes[System`Else, HoldFirst];
SetAttributes[System`IfQ, HoldRest];
Off[System`If::argbu];
expr: System`IfQ[_, _, _, __] /;
  Message[System`IfQ::argb, System`IfQ, Length @ Unevaluated @ expr, 1, 3] := $Failed;
SyntaxInformation @ System`If = {
  "ArgumentsPattern" -> {_, _., _., _.},
  "LocalVariables" -> None,
  "ColorEqualSigns" -> {1, 1} };
SyntaxInformation @ System`IfQ = {
  "ArgumentsPattern" -> {_, _., _.},
  "LocalVariables" -> None,
  "ColorEqualSigns" -> {1, 1} };
IfQ[test_, trueBranch_, falseBranch_ : Null] := If[TrueQ[test], trueBranch, falseBranch];
If /: If[test_] Then[trueBranch_] Else[falseBranch_] := If[test, trueBranch, falseBranch];
If /: If[test_] Then[trueBranch_] := If[test, trueBranch];
If /: If[test_] Else[falseBranch_] := If[!test, falseBranch];
IfQ /: IfQ[test_] Then[trueBranch_] Else[falseBranch_] := If[TrueQ[test], trueBranch, falseBranch];
IfQ /: IfQ[test_] Then[trueBranch_] := If[TrueQ[test], trueBranch];
IfQ /: IfQ[test_] Else[falseBranch_] := If[!TrueQ[test], falseBranch];
Protect[If];


(* ::Subsection:: *)
(*ForEach*)


System`ForEach::usage = "ForEach[var, from, to, body] or ForEach[var -> list, body] is another syntax \
for Do or Table, repending on the semicolon at body's end.
This syntax keeps ForEach and iterator together, which is more readible, especially for a long body.
Compare:
  ForEach[var -> {one, two, three},
    Very;
    long;
    body;
  ]
and
  Do[
    Very;
    long;
    body;
  , {one, two, three}]";

SetAttributes[System`ForEach, HoldAll];

ForEach[varlist_List, do: CompoundExpression[___, Null]] := Do[do, varlist];
ForEach[var_ -> list_, do: CompoundExpression[___, Null]] := Do[do, {var, list}];
ForEach[var_, i_, f_, step:_|PatternSequence[], do: CompoundExpression[___, Null]] := Do[do, {var, i, f, step}];

ForEach[varlist_List, do_] := Table[do, varlist];
ForEach[var_ -> list_, do_] := Table[do, {var, list}];
ForEach[var_, i_, f_, step:_|PatternSequence[], do_] := Table[do, {var, i, f, step}];
SyntaxInformation[ForEach] = {"LocalVariables" -> {"Table", {1, 1}},
  "ArgumentsPattern" -> {_, _, _., _., _.}};


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

SetOptions[SendMail, "To" -> "mcsaksik@gmail.com", "From" -> "mcsaksik@gmail.com", "Server" -> "smtp.gmail.com",
 "UserName" -> "mcsaksik", "Password" -> "*******", "PortNumber" -> 465, "EncryptionProtocol" -> "SSL"];


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
