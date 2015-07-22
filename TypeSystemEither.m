BeginPackage["TypeSystemEither`", {"FormatUsage`", "UsageOnly`"}];

TypeSystemEither::usage = "Either type from WL10 is declared but not working.
`TypeSystem‘Either[_type\_1_, _type\_2_, ...]` represents a union type." //Private`FormatUsage // StringReplace[#, "‘"->"`"]&;
Private`UsageOnly[TypeSystemEither];

Begin["`Private`"];

TypeSystem`Predicates`PackagePrivate`valid[TypeSystem`Either[types__]] :=
  AnyTrue[{types}, TypeSystem`Predicates`PackagePrivate`valid];
TypeSystem`TypeMap[f_, TypeSystem`Either[types__]] := f /@ TypeSystem`Either[types];
TypeSystem`Validation`PackagePrivate`vtor[TypeSystem`Either[types__], x_] :=
  AnyTrue[{types}, TypeSystem`Validation`PackagePrivate`vtor[#, x] &];

End[];

EndPackage[];
