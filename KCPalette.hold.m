(* ::Package:: *)

(* ::Section:: *)
(*Main*)


BeginPackage["KnoxPackages`KCPalette`"];


GeneratePalette::usage = "GeneratePalette[] generates the palette notebook along with a system 'save' dialog for deployment.";


Begin["`Private`"];


$buttonInheritedOptionRules = Cases[Options[Button], Rule[a_,__] :> Rule[a, Inherited]];


Attributes[pasteButton] = Attributes[genericButton] = {
	HoldRest
};


iStringTake[s_String, par_Integer] :=
	With[{len = StringLength[s]},
		If[len < par, s, StringTake[s, par]]
	]


Options[pasteButton] = Options[genericButton] = {
	Sequence @@ Options[StyleBox],
	Sequence @@ Options[ButtonBox],
	"OutputFormat" -> "Boxes",
	"MaxLabelStringCount" :> If[MatchQ[$OperatingSystem, "MacOSX"], 18, 16],
	"LabelLengthQ" -> True,
	"TooltipOverride" -> ""
};


genericButton[label_, actions_, opts:OptionsPattern[]] :=
	With[{l = If[TrueQ[OptionValue["LabelLengthQ"]], Replace[label, {s_String :> iStringTake[s, OptionValue["MaxLabelStringCount"]]}], label]},
		Replace[OptionValue["OutputFormat"], {
			"Boxes" :>
				ButtonBox[
					TooltipBox[
						StyleBox[l, Sequence @@ FilterRules[{opts}, Options[StyleBox]]],
						Replace[OptionValue["TooltipOverride"], {s_String /; (StringLength[s] > 0) :> s, _ :> label}],
						TooltipStyle -> "TextStyling"
					],
					ButtonFunction :> actions,
					BaseStyle -> "Button",
					ButtonNote -> "",
					Sequence @@ FilterRules[{opts}, Options[ButtonBox]]
				],
			else_ :>
				Button[
					Tooltip[
						Style[l, Sequence @@ FilterRules[{opts}, Options[Style]]],
						Replace[OptionValue["TooltipOverride"], {s_String /; (StringLength[s] > 0) :> s, _ :> label}]
					],
					actions,
					BaseStyle -> "Button",
					ButtonNote -> "",
					ImageSize -> Scaled[1],
					Sequence @@ FilterRules[{opts}, Options[Button]],
					Sequence @@ $buttonInheritedOptionRules
				]
		}]
	]


genericButton[___] := $Failed


pasteButton[label_, {"Cell", cells:(_Cell | List[__Cell])}, opts:OptionsPattern[]] :=
	With[{l = If[TrueQ[OptionValue["LabelLengthQ"]], Replace[label, {s_String :> iStringTake[s, OptionValue["MaxLabelStringCount"]]}], label]},
		Replace[OptionValue["OutputFormat"], {
			"Boxes" :>
				ButtonBox[
					TooltipBox[
						StyleBox[l, Sequence @@ FilterRules[{opts}, Options[StyleBox]]],
						Replace[OptionValue["TooltipOverride"], {s_String /; (StringLength[s] > 0) :> s, _ :> label}],
						TooltipStyle -> "TextStyling"
					],
					ButtonData -> cells,
					BaseStyle -> "PasteCellsButton",
					ButtonNote -> "Paste a styled cell into a notebook",
					Sequence @@ FilterRules[{opts}, Options[ButtonBox]]
				],
			else_ :>
				Button[
					Tooltip[
						Style[l, Sequence @@ FilterRules[{opts}, Options[Style]]],
						Replace[OptionValue["TooltipOverride"], {s_String /; (StringLength[s] > 0) :> s, _ :> label}]
					],
					Inherited, (*defined in style sheet*)
					ButtonData -> cells,
					BaseStyle -> "PasteCellsButton",
					ButtonNote -> "Paste a styled cell into a notebook",
					ImageSize -> Scaled[1],
					Sequence @@ FilterRules[{opts}, Options[Button]],
					Sequence @@ $buttonInheritedOptionRules
				]
		}]
	]


pasteButton[label_, {"Expr", expr_}, opts:OptionsPattern[]] :=
	With[{l = If[TrueQ[OptionValue["LabelLengthQ"]], Replace[label, {s_String :> iStringTake[s, OptionValue["MaxLabelStringCount"]]}], label]},
		Replace[OptionValue["OutputFormat"], {
			"Boxes" :>
				ButtonBox[
					TooltipBox[
						StyleBox[l, Sequence @@ FilterRules[{opts}, Options[StyleBox]]],
						Replace[OptionValue["TooltipOverride"], {s_String /; (StringLength[s] > 0) :> s, _ :> label}],
						TooltipStyle -> "TextStyling"
					],
					ButtonData -> expr,
					BaseStyle -> "PasteExprButton",
					ButtonNote -> "Paste a mathematical template into a notebook",
					Sequence @@ FilterRules[{opts}, Options[ButtonBox]]
				],
			else_ :>
				Button[
					Tooltip[
						Style[l, Sequence @@ FilterRules[{opts}, Options[Style]]],
						Replace[OptionValue["TooltipOverride"], {s_String /; (StringLength[s] > 0) :> s, _ :> label}]
					],
					Inherited, (*defined in style sheet*)
					ButtonData -> expr,
					BaseStyle -> "PasteExprButton",
					ButtonNote -> "Paste a mathematical template into a notebook",
					ImageSize -> Scaled[1],
					Sequence @@ FilterRules[{opts}, Options[Button]],
					Sequence @@ $buttonInheritedOptionRules
				]
		}]
	]


pasteButton[label_, {"Style", style_String}, opts:OptionsPattern[]] :=
	With[{l = If[TrueQ[OptionValue["LabelLengthQ"]], Replace[label, {s_String :> iStringTake[s, OptionValue["MaxLabelStringCount"]]}], label]},
		Replace[OptionValue["OutputFormat"], {
			"Boxes" :>
				ButtonBox[
					TooltipBox[
						StyleBox[l, Sequence @@ FilterRules[{opts}, Options[StyleBox]]],
						Replace[OptionValue["TooltipOverride"], {s_String /; (StringLength[s] > 0) :> s, _ :> label}],
						TooltipStyle -> "TextStyling"
					],
					ButtonData -> style,
					BaseStyle -> "PasteStyleButton",
					ButtonNote -> "Paste a style into a notebook",
					Sequence @@ FilterRules[{opts}, Options[ButtonBox]]
				],
			else_ :>
				Button[
					Tooltip[
						Style[l, Sequence @@ FilterRules[{opts}, Options[Style]]],
						Replace[OptionValue["TooltipOverride"], {s_String /; (StringLength[s] > 0) :> s, _ :> label}]
					],
					Inherited, (*defined in style sheet*)
					ButtonData -> style,
					BaseStyle -> "PasteStyleButton",
					ButtonNote -> "Paste a style into a notebook",
					ImageSize -> Scaled[1],
					Sequence @@ FilterRules[{opts}, Options[Button]],
					Sequence @@ $buttonInheritedOptionRules
				]
		}]
	]


pasteButton[label_, {"Style", cells:(_Cell | List[__Cell])}, opts:OptionsPattern[]] := pasteButton[label, {"Cell", cells}, opts]


pasteButton[___] := $Failed


(* ::Section:: *)
(*Styles Tab*)


$styleButtons = {
	{
		"Chapter",
		{
			Cell[TextData[{"Chapter ", StyleBox[CounterBox["BookChapterLabel"], "BookChapterNumber"]}],
				"BookChapterLabel"
			],
			Cell["Chapter Title","BookChapterTitle"]
		}
	},
	{
		"SectionN",
		"SectionNumbered"
	},
	{
		"ProblemN",
		"ProblemNumbered"
	},
	{
		"Example",
		"Example"
	},
	{
		"Exercise",
		"Exercise"
	},
	{
		"Definition",
		"Definition"
	},
	{
		"Theorem",
		"Theorem"
	},
	{
		"Proof",
		Cell["Proof", "Proof"]
	},
	{
		"QED",
		Cell["QED", "QED"]
	},
	{
		"Commentary",
		"Commentary"
	}
};


styleGroup1[] :=
	Cell[BoxData[
		GridBox[
			Transpose[{pasteButton[#1, {"Style", #2}]& @@@ $styleButtons}],
			BaseStyle -> "1ButtonGrid"
		] 
	],
	"FrameCell" 
];


$styleGridButtons = {
	{
		{
			"B1",
			"Item1"
		},
		{
			"B2",
			"Item2"
		},
		{
			"B3",
			"Item3"
		}
	},
	{
		{
			"N1",
			"Item1Numbered"
		},
		{
			"N2",
			"Item2Numbered"
		},
		{
			"N3",
			"Item3Numbered"
		}
	},
	{
		{
			"P1",
			"Item1Paragraph"
		},
		{
			"P2",
			"Item2Paragraph"
		},
		{
			"P3",
			"Item3Paragraph"
		}
	},
	{
		{
			"NN1",
			"Item1NNumbered"
		},
		{
			"NN2",
			"Item2NNumbered"
		},
		{
			"NN3",
			"Item3NNumbered"
		}
	}
};




styleGroup2Checkbox[] :=
	Cell[BoxData[
		GridBox[(((pasteButton[#1, {"Style", #2}]&) @@@ #&) /@ $styleGridButtons), BaseStyle -> "3ButtonGrid"]
		],
	"FrameCell"
]




gradingStyles[] :=
	Cell[BoxData[
		GridBox[
								{
									{
										pasteButton["Grading", {"Style", "Grading"}, FontColor -> RGBColor[1,0,0]],
										pasteButton[
											"ST",
											{"Style", "StrikeThrough"},
											FontColor -> RGBColor[1,0,0], FontVariations -> {"StrikeThrough" -> True}
										]
									},
									{
										pasteButton["Comment", {"Style", "Comment"}, FontColor -> RGBColor[0,0,1]],
										pasteButton["UL", {"Style", "Underline"}, FontColor -> RGBColor[0,0,1], FontVariations -> {"Underline" -> True}]
									}
								},
								GridBoxItemSize -> {"Columns" -> {Scaled[0.6666], Scaled[0.3333]}, "Rows" -> {{Automatic}}},
								GridBoxSpacings -> {"Columns" -> {{0}}, "Rows" -> {{0}}}
							]
		],
	"FrameCell"
]





typeStyles[] :=
	Cell[BoxData[
		GridBox[
								{
									{
										StyleBox["Times", FontFamily -> "Times"]
									},
									{
										GridBox[
											{
												With[{
														label = #1,
														func := (
															FrontEnd`FrontEndExecute[{FrontEnd`FrontEndToken[FrontEnd`InputNotebook[], "Style", #2]}];
															FrontEnd`SelectionMove[FrontEnd`InputNotebook[], After, Character]
														),
														options = (Sequence @@ Flatten[{#3}])
													},
													genericButton[label, func, options]
												]& @@@ {
														{"R", "TR", FontWeight -> Plain},
														{"B", "TB", FontWeight -> Bold},
														{"I", "TI", FontSlant -> Italic},
														{"BI", "TBI", {FontWeight -> Bold, FontSlant -> Italic}}
														}
											},
											GridBoxItemSize -> {"Columns" -> {{Scaled[0.24]}}, "Rows" -> {{Automatic}}},
											GridBoxSpacings -> {"Columns" -> {{0}}, "Rows" -> {{0}}}
										]
									},
									{
										StyleBox["Courier", FontFamily -> "Courier"]
									},
									{
										GridBox[
											{
												With[{
														label = #1,
														func := (
															FrontEnd`FrontEndExecute[{FrontEnd`FrontEndToken[FrontEnd`InputNotebook[], "Style", #2]}];
															FrontEnd`SelectionMove[FrontEnd`InputNotebook[], After, Character]
														),
														options = (Sequence @@ Flatten[{#3}])
													},
													genericButton[label, func, options]
												]& @@@ {
														{"R", "MR", {FontFamily -> "Courier", FontWeight -> Plain}},
														{"B", "MB", {FontFamily -> "Courier", FontWeight -> Bold}},
														{"I", "MO", {FontFamily -> "Courier", FontSlant -> Italic}},
														{"BI", "MBO", {FontFamily -> "Courier", FontWeight -> Bold, FontSlant -> Italic}}
														}
											},
											GridBoxItemSize -> {"Columns" -> {{Scaled[0.24]}}, "Rows" -> {{Automatic}}},
											GridBoxSpacings -> {"Columns" -> {{0}}, "Rows" -> {{0}}}
										]
									}
								},
								GridBoxItemSize -> {"Columns" -> {{Scaled[1]}}, "Rows" -> {{Automatic}}},
								GridBoxSpacings -> {"Columns" -> {{0}}, "Rows" -> {0, 0, 1, 0, 0}},
								GridBoxBackground -> {"Columns" -> {{Automatic}}, "Rows" -> {{GrayLevel[0.6], Inherited}}}
							]
		],
	"FrameCell"
]



$stylesTab = {
	Cell["", "TopBorder"],
	styleGroup1[],
	Cell["", "BottomBorder"],
	Cell["", "TopBorder"],
	styleGroup2Checkbox[],
	Cell["", "BottomBorder"],
	Cell["", "TopBorder"],
	gradingStyles[],
	Cell["", "BottomBorder"],
	Cell["", "TopBorder"],
	typeStyles[],
	Cell["", "BottomBorder"](*,
	Cell["", "TopBorder"],
	stylesForSelectedNotebook[]
	Cell["", "BottomBorder"]*)
};


(* ::Section:: *)
(*Typesetting Tab*)


typeTemplates1[] :=
	Cell[BoxData[
		GridBox[
			Partition[
				(pasteButton[#, {"Expr", #}, "LabelLengthQ" -> False]&) /@
					{
						"\!\(\*FractionBox[\(\[SelectionPlaceholder]\), \(\[Placeholder]\)]\)",
						"\!\(\*SuperscriptBox[\(\[SelectionPlaceholder]\), \(\[Placeholder]\)]\)",
						"\!\(\*SubscriptBox[\(\[SelectionPlaceholder]\), \(\[Placeholder]\)]\)",
						"\!\(\*SubsuperscriptBox[\(\[SelectionPlaceholder]\), \(\[Placeholder]\), \(\[Placeholder]\)]\)",
						"\!\(\*SubscriptBox[\(\[InvisiblePrefixScriptBase]\), \(\[Placeholder]\)]\)\[SelectionPlaceholder]",
						"\!\(\*UnderscriptBox[\(\[SelectionPlaceholder]\), \(\[Placeholder]\)]\)",
						"\!\(\*UnderoverscriptBox[\(\[SelectionPlaceholder]\), \(\[Placeholder]\), \(\[Placeholder]\)]\)",
						"\!\(\*OverscriptBox[\(\[SelectionPlaceholder]\), \(\[Placeholder]\)]\)"
				},
				4
			],
			BaseStyle -> "4ButtonGrid"
		]
	],
	"FrameCell"
]


typeTemplates2[] :=
	Cell[BoxData[
		GridBox[
			Partition[
				(pasteButton[#, {"Expr", #}, "LabelLengthQ" -> False]&) /@
					{
						"\[LeftBracketingBar]\[SelectionPlaceholder]\[RightBracketingBar]",
						"\[LeftDoubleBracketingBar]\[SelectionPlaceholder]\[RightDoubleBracketingBar]",
						"\[LeftDoubleBracketingBar]\!\(\*OverscriptBox[\(\[SelectionPlaceholder]\), \(\[RightVector]\)]\)\[RightDoubleBracketingBar]",
						"\[LeftAngleBracket]\[SelectionPlaceholder]\[RightAngleBracket]",
						"\[LeftAngleBracket]\!\(\*OverscriptBox[\(\[SelectionPlaceholder]\), \(\[RightVector]\)]\),\!\(\*OverscriptBox[\(\[Placeholder]\), \(\[RightVector]\)]\)\[RightAngleBracket]",
						"\!\(\*OverscriptBox[\(\[SelectionPlaceholder]\), \(\[RightVector]\)]\)",
						"\!\(\*OverscriptBox[\(\[SelectionPlaceholder]\), \(_\)]\)",
						"\!\(\*TagBox[OverscriptBox[\"\[SelectionPlaceholder]\", \"^\"],\n DisplayForm]\)",
						"\!\(\*TagBox[OverscriptBox[\"\[SelectionPlaceholder]\", \"\[DoubleDot]\"],\n DisplayForm]\)"
				},
				3
			],
			BaseStyle -> "3ButtonGrid"
		]
	],
	"FrameCell"
]


typeTemplates3[] :=
	Cell[BoxData[
		GridBox[
			Partition[
				(pasteButton[#, {"Expr", #}, "LabelLengthQ" -> False]&) /@
					{
						"\!\(\*SqrtBox[\(\[SelectionPlaceholder]\)]\)",
						"\!\(\*RadicalBox[\(\[SelectionPlaceholder]\), \(\[Placeholder]\)]\)",
						"\[Integral]\[SelectionPlaceholder]\[DifferentialD]\[Placeholder]",
						"\!\(\*SubsuperscriptBox[\(\[Integral]\), \(\[Placeholder]\), \(\[Placeholder]\)]\)\[SelectionPlaceholder]\[DifferentialD]\[Placeholder]",
						"\!\(\*SubscriptBox[\(\[PartialD]\), \(\[Placeholder]\)]\)\[SelectionPlaceholder]",
						"\!\(\*SubscriptBox[\(\[PartialD]\), \(\[Placeholder], \[Placeholder]\)]\)\[SelectionPlaceholder]",
						"\!\(\*UnderoverscriptBox[\(\[Sum]\), \(\[Placeholder] = \[Placeholder]\), \(\[Placeholder]\)]\)\[SelectionPlaceholder]",
						"\!\(\*UnderoverscriptBox[\(\[Product]\), \(\[Placeholder] = \[Placeholder]\), \(\[Placeholder]\)]\)\[SelectionPlaceholder]"
				},
				2
			],
			BaseStyle -> "2ButtonGrid"
		]
	],
	"FrameCell"
]


typeTemplates4[] :=
	Cell[BoxData[
		GridBox[
			{
				{
					pasteButton[
						RowBox[{
							"(",
							GridBox[{{"\[SelectionPlaceholder]", "\[Placeholder]"}, {"\[Placeholder]", "\[Placeholder]"}},
								GridBoxAlignment -> {
									"Columns" -> {{Right}},
									"ColumnsIndexed" -> {},
									"Rows" -> {{Baseline}},
									"RowsIndexed"->{}
								},
								GridBoxItemSize -> {"Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
								GridBoxAlignment -> {"Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
								GridBoxSpacings -> {"Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
								ColumnsEqual -> False,
								RowsEqual -> False
							],
							")"
						}],
						{"Expr", RowBox[{"(", GridBox[{{"\[SelectionPlaceholder]", "\[Placeholder]"}, {"\[Placeholder]", "\[Placeholder]"}}, ColumnAlignments -> Right], ")"}]},
						FontSize -> 8,
						"LabelLengthQ" -> False
					],
					pasteButton[
						RowBox[{
							"\[SelectionPlaceholder]",
							":=",
							RowBox[{
								"\[Piecewise]",
								GridBox[{{"\[Placeholder]", "\[Placeholder]"}, {"\[Placeholder]", "\[Placeholder]"}},
									GridBoxSpacings -> {
										"Columns" -> {Offset[0.27999999999999997`], {Offset[1.4`]}, Offset[0.27999999999999997`]},
										"ColumnsIndexed" -> {},
										"Rows" -> {Offset[0.2`], {Offset[0.4`]}, Offset[0.2`]},
										"RowsIndexed" -> {}
									},
									GridBoxItemSize -> {"Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
									GridBoxAlignment -> {"Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
									GridBoxSpacings -> {"Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
									ColumnsEqual -> False,
									RowsEqual -> False
								]
							}]
						}],
						{"Expr", RowBox[{"\[SelectionPlaceholder]", ":=", RowBox[{"\[Piecewise]", GridBox[{{"\[Placeholder]", "\[Placeholder]"}, {"\[Placeholder]", "\[Placeholder]"}}, ColumnSpacings -> 2]}]}]},
						FontSize -> 8,
						"LabelLengthQ" -> False
					]
				},
				{
					pasteButton[
						GridBox[{{"\[SelectionPlaceholder]", "=", "\[Placeholder]"}, {"\[InvisibleSpace]", "=", "\[Placeholder]"}},
							GridBoxItemSize -> {"Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
							GridBoxAlignment -> {"Columns" -> {Right, Center, {Left,Center}}, "Rows" -> {{Automatic}}},
							GridBoxSpacings -> {"Columns" -> {{Automatic}}, "Rows" -> {{Automatic}}},
							ColumnsEqual -> False,
							RowsEqual -> False
						],
						{"Expr", GridBox[{{"\[SelectionPlaceholder]", "=", "\[Placeholder]"}, {"\[InvisibleSpace]", "=", "\[Placeholder]"}}, ColumnAlignments -> {Right, Center, {Left, Center}}]},
						FontSize -> 8,
						"LabelLengthQ" -> False
					],
					genericButton[
						RowBox[{"\[InvisibleSpace]", RowBox[{"=", "\[Placeholder]"}]}],
						(
							FrontEnd`FrontEndExecute[{FrontEnd`FrontEndToken[FrontEnd`InputNotebook[], "NewRow"],
							FrontEnd`FrontEndToken[FrontEnd`InputNotebook[], "SelectNext"],
							FrontEnd`FrontEndToken[FrontEnd`InputNotebook[], "SelectNext"],
							FrontEnd`NotebookWrite[FrontEnd`InputNotebook[], GridBox[{{"\[InvisibleSpace]\[InvisibleSpace]", "=", "\[Placeholder]"}}]]}]
						),
						"LabelLengthQ" -> False
					]
				}
			},
			BaseStyle -> "2ButtonGrid"
		]
	],
	"FrameCell"
]


typeCharacters1[] :=
	With[{buttons = Partition[pasteButton[#, {"Expr", #}]& /@ CharacterRange["\[Alpha]", "\[Omega]"], 5]},
		Cell[BoxData[
			GridBox[
				buttons,
				BaseStyle -> "5ButtonGrid"
			]
		],
		"FrameCell"
	]
]


typeCharacters2[] :=
	With[{buttons = Partition[pasteButton[#, {"Expr", #}]& /@ {"\[CapitalGamma]", "\[CapitalDelta]", "\[CapitalTheta]", "\[CapitalLambda]", "\[CapitalXi]", "\[CapitalPhi]", "\[CapitalPsi]", "\[CapitalOmega]"}, 4]},
		Cell[BoxData[
			GridBox[
				buttons,
				BaseStyle -> "4ButtonGrid"
			]
		],
		"FrameCell"
	]
]


typeCharacters3[] :=
	With[{buttons = 
			Partition[pasteButton[#, {"Expr",#}]& /@ {
				"\[Pi]", "\[ExponentialE]", "\[ImaginaryI]", "\[Infinity]", "\[Degree]",
				"\[Times]", "\[Divide]", "\[Rule]", "\[RuleDelayed]", "\[Equal]",
				"\[NotEqual]", "\[LessEqual]", "\[GreaterEqual]", "\[Element]", "\[SuchThat]",
				"\[Not]", "\[And]", "\[Or]", "\[Union]", "\[Intersection]"
				},
				5
			]
		},
		Cell[BoxData[
			GridBox[
				buttons,
				BaseStyle -> "5ButtonGrid"
			]
		],
		"FrameCell"
	]
]


$typesettingTab = {
	Cell["", "TopBorder"],
	typeTemplates1[],
	Cell["", "BottomBorder"],
	Cell["", "TopBorder"],
	typeTemplates2[],
	Cell["", "BottomBorder"],
	Cell["", "TopBorder"],
	typeTemplates3[],
	Cell["", "BottomBorder"],
	Cell["", "TopBorder"],
	typeTemplates4[],
	Cell["", "BottomBorder"],
	Cell["", "TopBorder"],
	typeCharacters1[],
	Cell["", "BottomBorder"],
	Cell["", "TopBorder"],
	typeCharacters2[],
	Cell["", "BottomBorder"],
	Cell["", "TopBorder"],
	typeCharacters3[],
	Cell["", "BottomBorder"]
};


(* ::Section:: *)
(*Tools Tab*)


(* ::Section:: *)
(*Final*)


$stylesheet =
	Notebook[{
		Cell[
			CellGroupData[{
				Cell["Parent", "Section"],
				Cell[StyleData[StyleDefinitions -> "Default.nb"]]
				}, Closed]
		], 
		Cell[
			CellGroupData[{
				Cell["Button Styles", "Section"], 
				Cell[StyleData["Button"],
					StyleMenuListing -> None,
					ButtonStyleMenuListing -> None,
					FontFamily -> "Times",
					FontSize -> 12,
					FontColor -> GrayLevel[0],
					ButtonBoxOptions -> {
						Appearance -> {Automatic, "FramedPalette"},
						Evaluator -> None,
						Method -> "Preemptive"
					}
				],
				Cell[StyleData["PasteCellsButton", StyleDefinitions -> StyleData["Button"]],
					StyleMenuListing -> None,
					ButtonStyleMenuListing -> None,
					ButtonBoxOptions -> {
						ButtonFunction :> (
							FrontEnd`FrontEndExecute[{
								FrontEnd`SelectionMove[FrontEnd`InputNotebook[], After, Cell],
								FrontEnd`NotebookWrite[FrontEnd`InputNotebook[], #1],
								FrontEnd`SelectionMove[FrontEnd`InputNotebook[], Previous, Cell],
								FrontEnd`SelectionMove[FrontEnd`InputNotebook[], After, CellContents]
							}]
						&),
						Evaluator -> None,
						Method -> "Preemptive"
					}
				],
				Cell[StyleData["PasteExprButton", StyleDefinitions -> StyleData["Button"]],
					StyleMenuListing -> None,
					ButtonStyleMenuListing -> None,
					ButtonBoxOptions -> {
						ButtonFunction :> (
							FrontEnd`FrontEndExecute[{
								FrontEnd`NotebookApply[FrontEnd`InputNotebook[], #1, Placeholder]
							}]
						&),
						Evaluator -> None,
						Method -> "Preemptive"
					}
				],
				Cell[StyleData["PasteStyleButton", StyleDefinitions -> StyleData["Button"]],
					StyleMenuListing -> None,
					ButtonStyleMenuListing -> None,
					ButtonBoxOptions -> {
						ButtonFunction :> (
							FrontEnd`FrontEndExecute[{FrontEnd`FrontEndToken[FrontEnd`InputNotebook[], "Style", #1]}]
						&),
						Evaluator -> None,
						Method -> "Preemptive"
					}
				]
			}, Closed]
		],
		Cell[
			CellGroupData[{
				Cell["Button Grid Styles", "Section"],
				Cell[StyleData["ButtonGrid"],
					CellFrameMargins -> {{0, 0}, {0, 0}},
					StyleMenuListing -> None,
					GridBoxOptions -> {
						ColumnsEqual -> True,
						GridBoxAlignment -> {"Columns" -> {{Center}}},
						GridBoxSpacings -> {"Columns" -> {{0}}, "Rows" -> {{0}}},
						RowsEqual -> True
					}
				],
				Cell[StyleData["1ButtonGrid", StyleDefinitions -> StyleData["ButtonGrid"]],
					StyleMenuListing -> None,
					GridBoxOptions -> {
						GridBoxItemSize -> {"Columns" -> {{Scaled[1]}}}
					}
				],
				Cell[StyleData["2ButtonGrid", StyleDefinitions -> StyleData["ButtonGrid"]],
					StyleMenuListing -> None,
					GridBoxOptions -> {
						GridBoxItemSize -> {"Columns" -> {{Scaled[0.5]}}}
					}
				],
				Cell[StyleData["3ButtonGrid", StyleDefinitions -> StyleData["ButtonGrid"]],
					StyleMenuListing -> None,
					GridBoxOptions -> {
						GridBoxItemSize -> {"Columns" -> {{Scaled[0.3333]}}}
					}
				],
				Cell[StyleData["4ButtonGrid", StyleDefinitions -> StyleData["ButtonGrid"]],
					StyleMenuListing -> None,
					GridBoxOptions -> {
						GridBoxItemSize -> {"Columns" -> {{Scaled[0.25]}}}
					}
				],
				Cell[StyleData["5ButtonGrid", StyleDefinitions -> StyleData["ButtonGrid"]],
					StyleMenuListing -> None,
					GridBoxOptions -> {
						GridBoxItemSize -> {"Columns" -> {{Scaled[0.2]}}}
					}
				]
			}, Closed]
		], 
		Cell[
			CellGroupData[{
				Cell["Control Grid", "Section"],
				Cell[StyleData["Control"],
					StyleMenuListing -> None,
					FontFamily -> "BitStreamCharter",
					FontSize -> 10,
					FontColor -> GrayLevel[0]
				]
			}, Closed]
		],
		Cell[
			CellGroupData[{
				Cell["Cells", "Section"],
				Cell[StyleData["Cell"],
					CellFrameColor -> RGBColor[0.827, 0.816, 0.784],
					StyleMenuListing -> None,
					Background -> GrayLevel[1]
				],
				Cell[StyleData["TopBorder", StyleDefinitions -> StyleData["Cell"]],
					CellFrame -> {{1, 1}, {0, 1}},
					CellMargins -> {{2, 2}, {0, 1}},
					CellFrameMargins -> {{0, 0}, {-9, 0}},
					CellSize -> {Automatic, 12},
					StyleMenuListing -> None
				],
				Cell[StyleData["BottomBorder", StyleDefinitions -> StyleData["Cell"]],
					CellFrame -> {{1, 1}, {2, 0}},
					CellMargins -> {{2, 2}, {1, 0}},
					CellFrameMargins -> {{0, 0}, {0, -9}},
					CellSize -> {Automatic, 5},
					StyleMenuListing -> None
				],
				Cell[StyleData["FrameCell", StyleDefinitions -> StyleData["Cell"]],
					CellFrame -> {{1, 1}, {0, 0}},
					CellMargins -> {{2, 2}, {0, 0}},
					CellFrameMargins -> {{5, 1}, {1, 1}},
					StyleMenuListing -> None
				],
				Cell[StyleData["Group", StyleDefinitions -> StyleData["FrameCell"]],
					CellFrameMargins -> {{5, 1}, {5, 1}},
					TextAlignment -> Center,
					StyleMenuListing -> None,
					FontFamily -> "Times",
					FontSize -> 14,
					FontWeight -> Bold,
					FontColor -> GrayLevel[0.3]
				],
				Cell[StyleData["Spacer"],
					CellMargins -> {{5, 5}, {0, 2}},
					CellFrameMargins -> {{0, 0}, {0, 0}},
					CellSize -> {Automatic, 12},
					StyleMenuListing -> None
				]
			}, Closed]
		]
	},
	StyleDefinitions -> "PrivateStylesheetFormatting.nb"
];


$tabRules = {
	1 -> $stylesTab,
	2 -> $typesettingTab(*,
	3 -> $toolsTab,
	4 -> $helpTab*)
};


dockedCells[] :=
	{
		Cell[BoxData[
			ToBoxes @ Grid[
				Partition[
					Button[
						Dynamic[
							Graphics[{
								Directive[EdgeForm[GrayLevel[0.7]], Thickness[Large]],
								#1,
								Polygon[{{-1, 0}, {-0.9, 0.3}, {0.9, 0.3}, {1, 0}}],
								Text[
									Style[
										#2,
										If[SameQ[CurrentValue[ButtonNotebook[], {TaggingRules, "TabValue"}], #3],
											Sequence @@ {8.3, Bold, GrayLevel[1]},
											Sequence @@ {8, GrayLevel[0.9]}
										]
									],
									{0, 0.15},
									BaseStyle -> "Control"
								]
								},
								ImageMargins -> None,
								ImagePadding -> 0,
								ImageSize -> {70, Automatic},
								PlotRangePadding -> None
							],
							TrackedSymbols :> {CurrentValue[ButtonNotebook[], {TaggingRules, "TabValue"}]}
						],
						CurrentValue[ButtonNotebook[], {TaggingRules, "TabValue"}] = #3;
						SelectionMove[ButtonNotebook[], All, Notebook, AutoScroll -> False];
						NotebookWrite[ButtonNotebook[], #3 /. $tabRules, AutoScroll -> False];
						SelectionMove[ButtonNotebook[], Previous, Cell, AutoScroll -> False],
						Appearance -> None,
						Method -> "Preemptive"
					]& @@@ {
						{RGBColor[0,0,0.258824], "Styles", 1},
						{RGBColor[0.0541238,0.217197,0.109926], "Typesetting", 2}(*,
						{RGBColor[0.339361,0.323339,0.128634], "Tools", 3},
						{RGBColor[0.339361,0.0686656,0.0576028], "Help", 4}*)
						},
					2
				],
				Spacings -> {0,0}
				]
			],
			"DockedCell",
			CellFrameMargins -> {{0, 0}, {-1, 0}},
			CellSize -> {Automatic, 25},
			Background -> GrayLevel[0]
		]
	}


palleteNotebookExpression[] :=
	Notebook[
		$stylesTab,
		StyleDefinitions -> $stylesheet,
		TaggingRules -> {
			"ColorToggler" -> False,
			"StylesheetStyleToggler" -> False,
			"GradingStyleToggler" -> False,
			"TypeStyleToggler" -> False,
			"NewUserToggler" -> False,
			"TabValue" -> 1
		},
		DockedCells -> dockedCells[],
		Background ->
			Dynamic[
				If[CurrentValue[EvaluationNotebook[], {TaggingRules, "TabValue"}]===1, 
						RGBColor[0,0,0.258824],
						RGBColor[0.0541238,0.217197,0.109926]
				]
			],
		NotebookDynamicExpression :> Refresh[Needs["KnoxPackages`KCPalette`"],None],
		ClosingAutoSave -> True,
		Editable -> False,
		Saveable -> True,
		WindowToolbars -> {},
		WindowSize -> {145,460},
		WindowFrame -> "Palette",
		WindowElements -> {"VerticalScrollBar"},
		WindowFrameElements -> {"CloseBox", "ResizeArea"},
		WindowClickSelect -> False,
		WindowTitle -> "KCPalette_v3.0",
		ScrollingOptions -> {"VerticalScrollRange" -> Fit},
		ShowCellBracket -> False,
		ShowGroupOpener -> False,
		WholeCellGroupOpener -> False,
		CellMargins -> 0,
		Deployed -> False, (*if set to True, docked cell cannot write new nb contents*)
		CellOpen -> True,
		ShowCellLabel -> False,
		ShowCellTags -> False,
		ContextMenu -> {}, 
		ComponentwiseContextMenu -> {},
		ShowAutoStyles -> False,
		ShowSyntaxStyles -> False,
		ShowStringCharacters -> False,
		ImageMargins -> 0
	];


Options[GeneratePalette] = {
	"Version" -> "v3.0",
	"ShowSaveDialog" -> True
};


(*Path set to my install directory. Originally it was set to $HomeDirectory *)
GeneratePalette[OptionsPattern[]] :=
	With[{expr = palleteNotebookExpression[]},
		If[TrueQ[OptionValue["ShowSaveDialog"]],
			NotebookSave[
				NotebookPut[expr],
				SystemDialogInput["FileSave", ToFileName[{"C:\\ProgramData\\Mathematica\\Applications\\KnoxPackages\\FrontEnd\\Palettes"}, 
					"KCPalette_"<>OptionValue["Version"]<>".nb"]]
			],
			NotebookPut[expr]
		]
	]


End[];


EndPackage[];


(* ::Section:: *)
(*Generate*)


(* ::Input:: *)
(*GeneratePalette["ShowSaveDialog" -> True]*)

(*
$HomeDirectory
*)
