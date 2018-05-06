(* ::Package:: *)
(* Context:  KnoxPackages`ConvertToUsage` *)

(* Directory Path:  Packages/KnoxPackages/ConvertToUsage.m *)

(* Mathematica Version:  8.0 *)

(* Author: Michael J. Kaminski *)

(* Copyright 2011-12 by Michael J. Kaminski *)

(* Work on this package partially supported by grants from:
         Mellon Funding through Knox College
*)

(* Special Thanks to Dennis M. Schneider *)



BeginPackage["KnoxPackages`ConvertToUsage`"];

ConvertToUsage::usage=
"ConvertToUsage\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"input\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) takes the Expression form of a text cell as \!\(\*StyleBox[\"input\",\"TI\"]\), inline cells included, and carries out the conversions to the string representation of the boxes.
ConvertToUsage\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) takes the previous cell as \!\(\*StyleBox[\"input\",\"TI\"]\). ";


Begin["`Private`"];

ConvertRowBox[stream_,input_]:=Module[{n = 1},
	WriteString[stream,"\("];
	While[n<=Length[input[[1]]],
		MasterConverter[stream,input[[1,n]]];
		n=n+1;
	];
	WriteString[stream,"\)"];
	Return[];
]


ConvertSuperscriptBox[stream_,input_]:=Module[{},
	WriteString[stream,"\("];
	MasterConverter[stream,input[[1]]];
	WriteString[stream,"\^"];
	MasterConverter[stream,input[[2]]];
	WriteString[stream,"\)"];
	Return[];
]


ConvertSubscriptBox[stream_,input_]:=Module[{},
	WriteString[stream,"\("];
	MasterConverter[stream,input[[1]]];
	WriteString[stream,"\_"];
	MasterConverter[stream,input[[2]]];
	WriteString[stream,"\)"];
	Return[];
]


ConvertSubsuperscriptBox[stream_,input_]:=Module[{},
	WriteString[stream,"\("];
	MasterConverter[stream,input[[1]]];
	WriteString[stream,"\_"];
	MasterConverter[stream,input[[2]]];
	WriteString[stream,"\%"];
	MasterConverter[stream,input[[3]]];
	WriteString[stream,"\)"];
	Return[];
]


ConvertOverscriptBox[stream_,input_]:=Module[{},
	WriteString[stream,"\("];
	MasterConverter[stream,input[[1]]];
	WriteString[stream,"\&"];
	MasterConverter[stream,input[[2]]];
	WriteString[stream,"\)"];
	Return[];
]


ConvertUnderscriptBox[stream_,input_]:=Module[{},
	WriteString[stream,"\("];
	MasterConverter[stream,input[[1]]];
	WriteString[stream,"\+"];
	MasterConverter[stream,input[[2]]];
	WriteString[stream,"\)"];
	Return[];
]


ConvertUnderoverscriptBox[stream_,input_]:=Module[{},
	WriteString[stream,"\("];
	MasterConverter[stream,input[[1]]];
	WriteString[stream,"\+"];
	MasterConverter[stream,input[[2]]];
	WriteString[stream,"\%"];
	MasterConverter[stream,input[[3]]];
	WriteString[stream,"\)"];
	Return[];
]


ConvertFractionBox[stream_,input_]:=Module[{},
	WriteString[stream,"\("];
	MasterConverter[stream,input[[1]]];
	WriteString[stream,"\/"];
	MasterConverter[stream,input[[2]]];
	WriteString[stream,"\)"];
	Return[];
]


ConvertSqrtBox[stream_,input_]:=Module[{},
	WriteString[stream,"\("];
	WriteString[stream,"\@"];
	MasterConverter[stream,input[[1]]];
	WriteString[stream,"\)"];
	Return[];
]


ConvertRadicalBox[stream_,input_]:=Module[{},
	WriteString[stream,"\("];
	WriteString[stream,"\@"];
	MasterConverter[stream,input[[1]]];
	WriteString[stream,"\%"];
	MasterConverter[stream,input[[2]]];
	WriteString[stream,"\)"];
	Return[];
]


ConvertFormBox[stream_,input_]:=Module[{},
	WriteString[stream,"\("];
	WriteString[stream,ToString[input[[2]]]];
	WriteString[stream,"\`"];
	MasterConverter[stream,input[[1]]];
	WriteString[stream,"\)"];
	Return[];
]


ConvertStyleBox[stream_,input_]:=Module[{},
(*Special Cases*)

(*Ellipsis*)
If[input[[1]]=="\[Ellipsis]",
	WriteString[stream,"\("];
	WriteString[stream,"\*StyleBox[\\\""];
	WriteString[stream,"\\[Ellipsis]"];
	WriteString[stream,"\\\",\\\""];
	WriteString[stream,input[[2]]];
	WriteString[stream,"\\\"]"];
	WriteString[stream,"\)"];
	Return[];
];

(*Rule*)
If[input[[1]]=="\[Rule]"||input[[1]]=="->",
	WriteString[stream,"\("];
	WriteString[stream,"\*StyleBox[\\\""];
	WriteString[stream,"\\[Rule]"];
	WriteString[stream,"\\\",\\\""];
	WriteString[stream,input[[2]]];
	WriteString[stream,"\\\"]"];
	WriteString[stream,"\)"];
	Return[];
];

(*Standard Case*)
	WriteString[stream,"\("];
	WriteString[stream,"\*StyleBox[\\\""];
	WriteString[stream,input[[1]]];
	WriteString[stream,"\\\",\\\""];
	WriteString[stream,input[[2]]];
	WriteString[stream,"\\\"]"];
	WriteString[stream,"\)"];
	Return[];
]


StyleChooser[string_]:=Module[{retval,replacementstyle1 = "TI",replacementstyle2 = "TR"},

(*Special Cases: replacementstyle1--Add special cases by adding ||MatchQ[string,specialcase] in the condition statement for If*)
If[False,
	Return[replacementstyle1]
];

(*Special Cases: replacementstyle2--Add special cases by adding ||MatchQ[string,specialcase] in the condition statement for If*)
If[MatchQ[string,"\[Del]"]||MatchQ[string,"->"]||MatchQ[string,"\[Rule]"] || MatchQ[string, "\[DoubleStruckCapitalR]"],
	Return[replacementstyle2]
];

(*Standard Case*)

If[(NumericQ[Quiet[Read[StringToStream[string]]]]||MatchQ[Quiet[Read[StringToStream[string]]],$Failed]||MemberQ[Names["System`*"],string]||MemberQ[Flatten[Join[Map[Names,{"KnoxPackages`Calculus`*","KnoxPackages`CommonFunctions`*","KnoxPackages`LinearAlgebra`*","KnoxPackages`Manipulations`*","KnoxPackages`ModifySystem`*","KnoxPackages`QuadricsNew`*","KnoxPackages`SpecialFunctions`*"}]]],string]),	
	retval=replacementstyle2,
	retval=replacementstyle1,
	retval=replacementstyle2
];

retval
];


MasterConverter[stream_,input_]:=Module[{style},
	Switch[Head[input],
		RowBox,
			ConvertRowBox[stream,input],
		SuperscriptBox,
			ConvertSuperscriptBox[stream,input],
		SubscriptBox,
			ConvertSubscriptBox[stream,input],
		SubsuperscriptBox,
			ConvertSubsuperscriptBox[stream,input],
		OverscriptBox,
			ConvertOverscriptBox[stream,input],
		UnderscriptBox,
			ConvertUnderscriptBox[stream,input],
		UnderoverscriptBox,
			ConvertUnderoverscriptBox[stream,input],
		FractionBox,
			ConvertFractionBox[stream,input],
		SqrtBox,
			ConvertSqrtBox[stream,input],
		RadicalBox,
			ConvertRadicalBox[stream,input],
		FormBox,
			ConvertFormBox[stream,input],
		String,
			style=StyleChooser[input];
			ConvertStyleBox[stream,{input,style}],
		_,
			Print["The head of ",input," was not recognized. It will be converted to a string and dealt with as such."];
			style=StyleChooser[ToString[input]];
			ConvertStyleBox[stream,{ToString[input],style}]
	];
	Return[];
];


ConvertToUsage[]:=Module[{},
	Print[];  (*Appear to be needed for because code directs focus to be two lines up *)
	SelectionMove[InputNotebook[],Previous,Cell];
	SelectionMove[InputNotebook[],Previous,Cell];
	SelectionMove[InputNotebook[],Previous,Cell];

	If[MatchQ[NotebookRead[InputNotebook[]][[2]],"Text"],
		ConvertToUsage[NotebookRead[InputNotebook[]]];,
		Print["Either enter the Expression form of the usage message into ConvertToUsage or have the usage message as the cell immediately preceding the input cell containg ConverToUsage."];
	];
Return[];
];


ConvertToUsage[input_]:=Module[{tempfilename = "ConvertToUsageTemp",tempfilelocation,stream,index = 1},

(*Check To See If The Temp File Exists Already*)
tempfilelocation=FileNameJoin[{$TemporaryDirectory,tempfilename}];
If[FileExistsQ[tempfilelocation],
	Print["The temporary file this function needs to create already exists. To prevent overwriting this file, change the tempfilename local variable."];
	Return[];
];

(*Open A Stream To Write To The Temp File*)
stream=OpenWrite[tempfilelocation];

(*Write Usage Message*)
While[index<=Length[input[[1,1]]],
	If[SameQ[Head[input[[1,1,index]]],Cell],
		WriteString[stream,"\!"];
		MasterConverter[stream,input[[1,1,index,1,1,1]]],
		WriteString[stream,ToString[input[[1,1,index]]]]
	];
	index=index+1;
];

(*Close The Stream, Print The Output, Delete the Temp File, Return*)
Close[stream];
FilePrint[tempfilelocation];
DeleteFile[tempfilelocation];
Return[];

];

End[];
EndPackage[];

(* ::Text:: *)
(*Expand out the cell and paste it into the function.*)


(* ::Example::Closed:: *)
(**)


(* ::Text:: *)
(*Plot[f,{x,Subscript[x, min],Subscript[x, max]}] generates a plot of f as a function of x from Subscript[x, min] to Subscript[x, max].*)
(*Plot[{Subscript[f, 1],Subscript[f, 2],\[Ellipsis]},{x,Subscript[x, min],Subscript[x, max]}] plots several functions Subscript[f, i].*)


(* ::Input:: *)
(*ConvertToUsage[]*)


(* ::Example::Closed:: *)
(**)


(* ::Text:: *)
(*PlotJump[f,{x,Subscript[x, min],Subscript[x, max]},Jump->{Subscript[j, 1],Subscript[j, 2],\[Ellipsis],Subscript[j, n]},Asymptote->{Subscript[a, 1],Subscript[a, 2],\[Ellipsis],Subscript[a, m]}] generates a plot of f as a function of x from Subscript[x, min] to Subscript[x, max] with special function values at Subscript[j, 1],Subscript[j, 2],\[Ellipsis],Subscript[j, n] and asymptotes at Subscript[a, 1],Subscript[a, 2],\[Ellipsis],Subscript[a, m].*)


(* ::Input:: *)
(*ConvertToUsage[]*)


(* ::Example::Closed:: *)
(**)


(* ::Text:: *)
(*ParametricPlot[{Subscript[f, x],Subscript[f, y]},{u,Subscript[u, min],Subscript[u, max]}] generates a parametric plot of a curve with x and y coordinates Subscript[f, x] and Subscript[f, y] as a function of u.*)
(*ParametricPlot[{{Subscript[f, x],Subscript[f, y]},{Subscript[g, x],Subscript[g, y]},\[Ellipsis]},{u,Subscript[u, min],Subscript[u, max]}] plots several parametric curves.*)
(*ParametricPlot[{Subscript[f, x],Subscript[f, y]},{u,Subscript[u, min],Subscript[u, max]},{v,Subscript[v, min],Subscript[v, max]}] plots a parametric region.*)
(*ParametricPlot[{{Subscript[f, x],Subscript[f, y]},{Subscript[g, x],Subscript[g, y]},\[Ellipsis]},{u,Subscript[u, min],Subscript[u, max]},{v,Subscript[v, min],Subscript[v, max]}] plots several parametric regions.*)


(* ::Input:: *)
(*ConvertToUsage[]*)
