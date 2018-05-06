BeginPackage["KnoxPackages`MakeOptionsTable`",{"KnoxPackages`Calculus`"}];

MakeOptionsTable::usage = 
"MakeOptionsTable[] returns the options to all commands in the packages in KnoxPackages. ";

Begin["`Private`"];

names = Flatten[
   Join[Map[
     Names, {"KnoxPackages`Calculus`*", 
      "KnoxPackages`CommonFunctions`*", 
      "KnoxPackages`LinearAlgebra`*", "KnoxPackages`Manipulations`*", 
      "KnoxPackages`ModifySystem`*", "KnoxPackages`QuadricsNew`*", 
      "KnoxPackages`SpecialFunctions`*"}]]];

opts = Union[
   ToString /@ 
    Flatten[Table[
      Options[ToExpression[names[[i]]]], {i, 1, Length[names]}]]];

CommandsWithOption[opt_] := Module[{returntable = {}, j = 1},
  
  While[j <= Length[names],
   If[MemberQ[ToString /@ Options[ToExpression[names[[j]]]], 
     ToString[opt]],
    returntable = Join[returntable, {ToString[names[[j]]]}]
    ];
   j = j + 1;
   ];
  Sort[returntable];
  returntable
  ]

MakeOptionsTable[] := 
 Grid[Join[{{"Option Name", "Commands with Option"}}, 
   Table[{opts[[i]], CommandsWithOption[opts[[i]]]}, {i, 1, 
     Length[opts]}]], Frame -> All]
     
End[];
EndPackage[];