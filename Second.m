BeginPackage["Second`"];

Unprotect[Evaluate @ Names[Context[] <> "*"]];

`Private`usage[`Private`nth_] := StringReplace[First::usage, {"First" -> `Private`nth, "first" -> ToLowerCase[`Private`nth]}];

Second::usage  = `Private`usage["Second"];
Third::usage   = `Private`usage["Third"];
Fourth::usage  = `Private`usage["Fourth"];
Fifth::usage   = `Private`usage["Fifth"];
Sixth::usage   = `Private`usage["Sixth"];
Seventh::usage = `Private`usage["Seventh"];
Eighth::usage  = `Private`usage["Eighth"];
Ninth::usage   = `Private`usage["Ninth"];
Tenth::usage   = `Private`usage["Tenth"];

Begin["`Private`"];

Second[x_]  := x[[2]];
Third[x_]   := x[[3]];
Fourth[x_]  := x[[4]];
Fifth[x_]   := x[[5]];
Sixth[x_]   := x[[6]];
Seventh[x_] := x[[7]];
Eighth[x_]  := x[[8]];
Ninth[x_]   := x[[9]];
Tenth[x_]   := x[[10]];

Second[]  /; Message[Second::argx,  Second, 0]  = $Failed;
Third[]   /; Message[Third::argx,   Third, 0]   = $Failed;
Fourth[]  /; Message[Fourth::argx,  Fourth, 0]  = $Failed;
Fifth[]   /; Message[Fifth::argx,   Fifth, 0]   = $Failed;
Sixth[]   /; Message[Sixth::argx,   Sixth, 0]   = $Failed;
Seventh[] /; Message[Seventh::argx, Seventh, 0] = $Failed;
Eighth[]  /; Message[Eighth::argx,  Eighth, 0]  = $Failed;
Ninth[]   /; Message[Ninth::argx,   Ninth, 0]   = $Failed;
Tenth[]   /; Message[Tenth::argx,   Tenth, 0]   = $Failed;

Second[arg1_, args__]  /; Message[Second::argx,  Second, Length@Hold@args + 1]  = $Failed;
Third[arg1_, args__]   /; Message[Third::argx,   Third, Length@Hold@args + 1]   = $Failed;
Fourth[arg1_, args__]  /; Message[Fourth::argx,  Fourth, Length@Hold@args + 1]  = $Failed;
Fifth[arg1_, args__]   /; Message[Fifth::argx,   Fifth, Length@Hold@args + 1]   = $Failed;
Sixth[arg1_, args__]   /; Message[Sixth::argx,   Sixth, Length@Hold@args + 1]   = $Failed;
Seventh[arg1_, args__] /; Message[Seventh::argx, Seventh, Length@Hold@args + 1] = $Failed;
Eighth[arg1_, args__]  /; Message[Eighth::argx,  Eighth, Length@Hold@args + 1]  = $Failed;
Ninth[arg1_, args__]   /; Message[Ninth::argx,   Ninth, Length@Hold@args + 1]   = $Failed;
Tenth[arg1_, args__]   /; Message[Tenth::argx,   Tenth, Length@Hold@args + 1]   = $Failed;


End[];

Protect[Evaluate @ Names[Context[] <> "*"]];

EndPackage[];
