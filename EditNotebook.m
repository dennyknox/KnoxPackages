(* ::Package:: *)
(* Context:  KnoxPackages`EditNotebook` *)

(* Directory Path:  Packages/KnoxPackages/ConvertToUsage.m *)

(* Mathematica Version:  8.0 *)

(* Author: Michael J. Kaminski *)

(* Copyright 2011-12 by Michael J. Kaminski *)

(* Work on this package partially supported by grants from:
         Mellon Funding through Knox College
*)

(* Special Thanks to Dennis M. Schneider *)



BeginPackage["KnoxPackages`EditNotebook`",
{"DocumentationTools`DocumentationTools`"}];

CreateStudentCopy::usage="CreateStudentCopy[] successively runs DeleteAnnotations[], (*DeleteAnswers[],*) and DeleteSolutions[]. ";

DeleteAnnotations::usage="DeleteAnnotations[] searches through the notebook and removes the annotations created using the DocumentationTools`AnnotationsInsert[] command. ";

DeleteAnswers::usage="DeleteAnswers[] searches through the notebook and removes answer cell groups whose answer cell is not marked with the CellTag KeepAnswer. ";

DeleteSolutions::usage="DeleteSolutions[] searches through the notebook and removes solution cell groups whose solution cell is not marked with the CellTag KeepSolution. ";

Begin["`Private`"];

DeleteAnnotations[]:=Module[{},
	SelectionMove[InputNotebook[],All,Notebook];
	FrontEndTokenExecute["SelectionOpenAllGroups"];
	SelectionMove[InputNotebook[],Before,Notebook];
	Quiet[
		While[
			MatchQ[DocumentationTools`AnnotationSearch["Down"],Null;;Null],
				DocumentationTools`AnnotationRemove[];
		];
	];
];

SetSolutionTags[]:=Module[{},
	SelectionMove[InputNotebook[],All,Notebook];
	FrontEndTokenExecute["SelectionOpenAllGroups"];
	SelectionMove[InputNotebook[],Before,Notebook];
	SelectionMove[InputNotebook[],Next,Cell];
	While[Not[MatchQ[NotebookRead[InputNotebook[]],{}]],
	
		(*Check if the Current Cell is a Subsubsection, has the word Solution, and does not have the CellTag KeepSolution*)
		If[MatchQ[NotebookRead[InputNotebook[]][[2]],"Subsubsection"]&&Not[MatchQ[StringPosition[ToString[NotebookRead[InputNotebook[]]],"Solution"],{}]]&&Not[MemberQ[Options[NotebookSelection[],CellTags],"KeepSolution",Infinity]],
			SelectionMove[InputNotebook[],All,CellGroup];

			(*Verify the Cell Group is a Solution Group and Set Tag*)
			If[Not[MatchQ[StringPosition[ToString[NotebookRead[InputNotebook[]][[1,1,1,1]]],"Solution"],{}]],
				SetOptions[NotebookSelection[],CellTags->"SolutionForDeletion"];
			];

		];
		SelectionMove[InputNotebook[],Next,Cell];
	];
	SelectionMove[InputNotebook[],Next,EvaluationCell];
];

FindSolutions[]:=Module[{},
	SelectionMove[InputNotebook[],All,Notebook];
	FrontEndTokenExecute["SelectionOpenAllGroups"];
	NotebookLocate["SolutionForDeletion"];
];

DeleteSolutions[]:=Module[{},
	SetSolutionTags[];
	FindSolutions[];
	NotebookDelete[];
];

SetAnswerTags[]:=Module[{},
	SelectionMove[InputNotebook[],All,Notebook];
	FrontEndTokenExecute["SelectionOpenAllGroups"];
	SelectionMove[InputNotebook[],Before,Notebook];
	SelectionMove[InputNotebook[],Next,Cell];
	While[Not[MatchQ[NotebookRead[InputNotebook[]],{}]],
	
		(*Check if the Current Cell is a Subsubsection, has the word Answer, and does not have the CellTag KeepAnswer*)
		If[MatchQ[NotebookRead[InputNotebook[]][[2]],"Subsubsection"]&&Not[MatchQ[StringPosition[ToString[NotebookRead[InputNotebook[]]],"Answer"],{}]]&&Not[MemberQ[Options[NotebookSelection[],CellTags],"KeepAnswer",Infinity]],
			SelectionMove[InputNotebook[],All,CellGroup];

			(*Verify the Cell Group is an Answer Group and Set Tag*)
			If[Not[MatchQ[StringPosition[ToString[NotebookRead[InputNotebook[]][[1,1,1,1]]],"Answer"],{}]],
				SetOptions[NotebookSelection[],CellTags->"AnswerForDeletion"];
			];

		];
		SelectionMove[InputNotebook[],Next,Cell];
	];
	SelectionMove[InputNotebook[],Next,EvaluationCell];
];

FindAnswers[]:=Module[{},
	SelectionMove[InputNotebook[],All,Notebook];
	FrontEndTokenExecute["SelectionOpenAllGroups"];
	NotebookLocate["AnswerForDeletion"];
];

DeleteAnswers[]:=Module[{},
	SetAnswerTags[];
	FindAnswers[];
	NotebookDelete[];
];

CreateStudentCopy[]:=Module[{},
	DeleteAnnotations[];
(*	DeleteAnswers[];*)
	DeleteSolutions[];
];

End[];
EndPackage[];

