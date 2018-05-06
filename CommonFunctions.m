(* ::Package:: *)

(* Context:  KnoxPackages`CommonFunctions` *) 

(* Mathematica Version:  11.1 *)

(* Author: Dennis M. Schneider *)

(* Copyright 1991-18 by Dennis M. Schneider *)

(* Work on this package partially supported by grants from:
         Pew Charitable Trusts
                   and
         National Science Foundation
         	NSF-ILI Grant # USE-9050757
         	NSF Grant # USE-9153249
             	   and
         Knox College
*)



Unprotect[SetAttributes, Set, Attributes];
Clear[SetAttributes, Set, Attributes];
Protect[SetAttributes, Set, Attributes];

(*
Abort::nolic = "KnoxPackages are not licensed to this machine.";
If[Not[MatchQ["5119-78768-59794", $MachineID]], Message[Abort::nolic]; Quit[]];
*)


Off[General::spell,General::spell1]
Off[ParametricPlot::ppcom]


BeginPackage["KnoxPackages`CommonFunctions`"]

TruthTable::usage = 
"TruthTable[vars, expression] gives the truth table for the expression involving the list of statement variables vars. Several expressions can be given in a list. ";

OrderODE::usage=
"OrderODE[de, y[t],t] returns the order of the differential equation de. OrderODE[sys, vars, t] returns a list of the highest order derivative of each var in the system.";

TimingCommand::usage = 
"TimingCommand[expr,opts...] evaluates expr 15 times, and returns the average time used.  Setting NXTiming -> n sets the number of evaluations.";



nonzerotest::usage = 
"nonzerotest[][expr] tests whether expr is different from 0.  nonzerotest[tol][expr] tests whether Chop[expr, tol] is different from 0.";

zerotest::usage = 
"zerotest[][expr] tests whether expr is 0.  zerotest[tol][expr] tests whether Chop[expr, tol] is 0.";

firstnonzero::usage = 
"firstnonzero[mat] returns the columns of the matrix containing the first non-zero element in each row of the matrix. ";

Swap::usage = 
"Swap[list,m,n] interchanges the mth and nth elements of list.  Both m and n may be negative integers.";


ToSpherical::usage =
"ToSpherical[{x,y,z}] gives the {rho, phi, theta} spherical coordinates corresponding to the Cartesian coordinates {x, y, z}. ToSpherical[expr, {x,y,z}, {rho, phi, theta}] returns the result of substituting x -> rho Sin[phi] Cos[theta], y -> rho Sin[phi] Sin[theta], z -> rho Cos[phi] in expr. ToSpherical[expr, {x,y,z}, {rho, phi, theta}, {a,b,c}] returns the result of substituting x -> a rho Sin[phi] Cos[theta], y -> b rho Sin[phi] Sin[theta], z -> c rho Cos[phi] in expr. ToSpherical[{fx,fy,fz}, {x,y,z}, {rho, phi, theta}] will interpret the first argument as a vector-valued function and convert from the Cartesian {i,j,k} basis to the spherical {eRho, ePhi, eTheta} basis in addition to changing the coordinates."

FromSpherical::usage=
"FromSpherical[expr,{r,p,t},{x,y,z}] returns the result of substituting r->Sqrt[x2+y2+z2], p->ArcCos[z/Sqrt[x2+y2]], t->ArcTan[x,y] into expr. FromSpherical[expr,{r,p,t}, {x,y,z}, {a,b,c}], will do the same substiutions but use x/a, y/b, and z/c. FromSpherical[{f_r,f_p,f_t}, {r,p,t,}, {x,y,z}] will interpret the first argument as a vector-valued function and convert from the spherical {eRho, ePhi, eTheta} basis to the Cartesian {i,j,k} basis in addition to changing coordinates."

ToCylindrical::usage =
"ToCylindrical[{x, y, z}] gives the {r, Theta, z} cylindrical coordinates corresponding to the Cartesian coordinates {x, y, z}. ToCylindrical[expr, {x,y,z}, {r, theta}] returns the result of substituting x -> r Cos[theta], y -> r Sin[theta] in expr. ToCylindrical[expr, {x,y,z}, {r, theta}, {a, b}] returns the result of substututing x -> a r Cos[theta], y -> b r Sin[theta] in expr. ToCylindrical[{fx,fy,fz},{x,y,z},{r,t}] will interpret the first argument as a vector-valued function and covert from the {i,j,k} basis to the {er, eTheta, ez} basis in addition to changing the coordinates."

FromCylindrical::usage=
"FromCylindrical[expr,{r,t,z},{x,y}] returns the result of substituting r->Sqrt[x^2+y^2],  t->ArcTan[x,y] into expr. FromCylindrical[expr,{r,t,z}, {x,y}, {a,b}], will do the same substiutions but use x/a and y/b, instead. FromCylindrical[{f_r,f_t,f_z}, {r,t,z}, {x,y}] will interpret the first argument as a vector-valued function and convert from the cylindrical {er, eTheta, ez} basis to the Cartesian {i,j,k} basis in addition to changing coordinates."

ToPolar::usage = 
"ToPolar[{x,y}] gives the {r, theta} coordinates corresponding to the Cartesian coordinates {x,y}. ToPolar[expr, {x,y}, {r, theta}] returns the result of substituting x -> r Cos[theta], y -> r Sin[theta] in expr.";

FromPolar::usage = 
"FromPolar[{r,t}] returns the Cartesian coordinates of the point with polar coordinates {r,t}.
FromPolar[expr,{r,t},{x,y}] returns the result of substituting r->Sqrt[x^2+y^2],  t->ArcTan[x,y] into expr. FromPolar[expr,{r,t,z}, {x,y}, {a,b}] will do the same substitutions but use x/a and y/b instead. FromPolar[{f_r,f_t}, {r,t}, {x,y}] will interpret the first argument as a vector-valued function and convert from the polar {er, eTheta} basis to the Cartesian {i,j} basis in addition to changing coordinates."


PointsToRules::usage =
"PointsToRules[pts,vars] converts a list of points into a list of substitution rules in vars.";

RulesToPoints::usage =
"RulesToPoints[rules,vars] converts a list of rules in vars to a list of points.";

KnoxTicks::usage = "Obsolute.";

(* ConstantTerm is used heavily in LinearAlgebra.m *)
ConstantTerm::usage = 
"ConstantTerm[expr] returns the terms that numerically evaluate to a number.  Constants -> {c1, ....} specifies that the ci are constants.  ConstantTerm[expr,var] returns the terms that do not involve var.  Var may be a list of symbols.";


MakeMesh::usage = 
"MakeMesh returns the data for MakePolygons.";


MakePolygons::usage = 
"MakePolygons take a list of data of the form generated by SurfaceGraphics or a table command and returns a list of polygons which can be used by Graphics3D.  For example: MakePolygons[{{p11,p12,p13,...},{p21,p22,p23,...},{p31,p32,p33,...},...}] returns {Polygon[{p11,p21,p22,p12}],Polygon[{p12,p22,p23,p13}],...,Polygon[{p21,p31,p32,p22}],Polygon[{p22,p32,p33,p23}],...}, where pij is a coordinate-triple.";

(*
WireFrame::usage = 
"WireFrame[object] replaces polygons by lines in object.  WireFrame[object,dir] applies the graphics directive(s) dir to the lines.";
*)

setps5::usage = 
"setps5[commandname, optionname, cyclelength, opts] adds the default options of optionname in commandname to those found in opts. Cyclelength determines the number of times the option is repeated. ";


SlopeLines::usage = "Obsolete.";

PolynomialDegree::usage = 
"PolynomialDegree[polynomial, vars] returns the degree of polynomial in vars.";

Polynomial::usage = 
"Polynomial[var,n,a] returns the polynomial a[0] + a[1] x + ... + a[n] x^n.  Polynomial[var,n,coeffs] returns the polynomial of degree n whose coefficients are coeffs.";

NewUnsortedUnion::usage = 
"NewUnsortedUnion[list1,list2,...] returns the unsorted union of the lists.";

UnsortedComplement::usage = 
"UnsortedComplement[list1,list2] returns the elements of list1 that are not in list2 without sorting the output.";

setdp::usage = 
"setdp[command, points, plotrange, styles] is an internal command that returns a list whose first component is a list of the points with styles attached and whose second component is the plot range needed to include the points in the plot. ";

TanLines::usage = 
"Coming soon. ";
(*****************************************  Messages   *******************************************)

Poly::notpoly = "The input expression `1` is not a polynomial in the specified variable(s) `2`.";

Poly::degree = "The input polynomial `1` is not a polynomial of degree `2` in the specified variable(s) `3`.";

(*SlopeLines::SingularPoint = "Singular point at `1`.";*)

TimingCommand::dumbuserinput = "NXTiming has to be greater than 0.";

TanLines::SingularPoint = "Singular point at `1`.";

dpoint::drawpt = "DrawPoint must be set to a list of points. ";
(*****************************************  Messages   *******************************************)

Options[TimingCommand]={NXTiming->15}; 

Options[Polynomial] = {Subscript->True};

Options[SlopeLines]={LineStyle->{}};

Options[TanLines]={TangentLineStyle->{},TangentPointStyle->PointSize[.01]};

Options[setdp]={DrawPointStyle->PointSize[.02]};

Begin["`Private`"];


TruthTable[vars_List, expn_List] := 
	TableForm[BooleanTable[Join[vars, expn], vars], TableHeadings -> {None, Join[vars, expn]}]/.{True->"T",False->"F"}

TruthTable[vars_List, expn_] := TruthTable[vars, {expn}]

TruthTable[var_, expn_] := TruthTable[{var}, expn]




TimingCommand[expr_, opts___] := 
   Module[{opt,data}, 
           opt = NXTiming/.{opts}/.Options[TimingCommand]; 
           If[opt<=0,Return[Message[TimingCommand::dumbuserinput]]];
           data=Table[First[Timing[expr]], {opt}];
          	Print["Data: " ,data];
          	Print["Average: " , (Plus @@ data)/opt]
         ]



OrderODE[de_Equal, (y_)[t_], t_] := 
  Max[Cases[Expand[de], Derivative[k_][y][t] | ___*Derivative[k_][y][t] -> k, Infinity]]
  
OrderODE[sys:{_Equal..}, iv:{_[t_]..}, t_]:=
	Max/@Transpose[Outer[OrderODE[#1,#2,t]&, sys, iv]]



zerotest[tolerance___] := 
  With[{tmp = Chop[N[#], tolerance]}, If[tmp == 0 #, True, False, #]] & 

nonzerotest[tolerance___] :=
	 ( Chop[N[#],tolerance] =!= (0 #) )&
	
firstnonzero[matrix_?MatrixQ] := 
	Map[Position[#,_?(N[#] != 0  &),{1},1,Heads->False]&,matrix]//Flatten


Swap[list_List,m_Integer,n_Integer] := 
	 (*ReplacePart[ReplacePart[list,list[[n]],m],list[[m]],n] *)
	 ReplacePart[ReplacePart[list,m -> list[[n]]], n -> list[[m]]]
	 


ToSpherical[expr_, vars:{_, _, _}, {r_, p_, t_}, list_:{1,1,1}] := 
  Simplify[First[
	expr /. PointsToRules[{list[[1]] r Cos[t] Sin[p], list[[2]] r Sin[t] Sin[p], list[[3]] r Cos[p]}, vars]
	], Assumptions -> {r > 0, Sin[p] >= 0}]

ToSpherical[{a_,b_,c_}] := ToSphericalCoordinates[{a,b,c}];

	
ToSpherical[fvec: {_, _, _}, vars:{_, _, _}, {r_ ,p_, t_}] :=
	Simplify[
		Map[
			Map[ToSpherical[#, vars, {r, p, t}]&, fvec].#&, 
			{(*eRho*){Cos[t] Sin[p], Sin[t] Sin[p], Cos[p]}, (*ePhi*){Cos[t] Cos[p], Sin[t] Cos[p] , -Sin[p]}, (*eTheta*){-Sin[t], Cos[t], 0}}]
			]
		
		
FromSpherical[expr_, vars:{_,_,_}, {x_,y_,z_}, list_:{1,1,1}]:=
  Module[{a, b, c},
	{a,b,c} = list;
	Simplify[First[expr/.PointsToRules[{Sqrt[(x/a)^2+(y/b)^2+(z/c)^2], 
													ArcCos[ z/c/Sqrt[(x/a)^2+(y/b)^2+(z/c)^2]],
													ArcTan[(b x),(a y)]
												},
												vars
								]
					],
					Assumptions->{a > 0, b > 0, c > 0, Element[{x,y,z},Reals]}(*,
					TransformationFunctions->
						{Automatic, (#/. (Cos[ArcTan[denom_,num_]] -> denom/Sqrt[(num)^2+(denom)^2]))&,
									(#/. (Sin[ArcTan[denom_,num_]]->num/Sqrt[(num)^2+(denom)^2]))&
						},
					ComplexityFunction->(LeafCount[#] + 100 Count[#, ArcTan[_,_], {0, Infinity}]&)*)
			]
	  ]	

FromSpherical[fvec:{_, _, _}]:=FromSphericalCoordinates[fvec]
	  
FromSpherical[fvec:{_, _, _}, vars:{_, _, _}, {x_, y_, z_}] := 
		(FromSpherical[#, vars, {x, y, z}] & ) /@ (fvec . # & ) /@ (ToSpherical[#, {x, y, z}, vars]& /@ IdentityMatrix[3])
		

ToCylindrical[expr_, vars:{_, _, _}, {r_, t_},list_:{1,1}] := 
  Simplify[First[expr /. PointsToRules[{list[[1]] r Cos[t], list[[2]] r Sin[t]}, Drop[vars, -1]]],r>0]
  
ToCylindrical[{a_, b_, c_}] := {Sqrt[a^2 + b^2], ArcTan[a, b], c};
  
ToCylindrical[fvec: {_,_,_}, vars:{_,_,_}, {r_, t_}] :=
	Simplify[Map[Map[ToCylindrical[#, vars, {r, t}]&, fvec].#&, {(*er*){Cos[t], Sin[t], 0}, (*eTheta*){-Sin[t], Cos[t], 0}, (*ez*){0, 0, 1}}]]
		
FromCylindrical[expr_, vars:{_, _, _}, {x_, y_}, list:{_, _}:{1, 1}] := 
 With[{a = list[[1]], b = list[[2]]}, 
     FullSimplify[
   		First[expr /. PointsToRules[{Sqrt[(x/a)^2 + (y/b)^2], ArcTan[b*x, a*y]}, Drop[vars, -1]]], 
			Assumptions -> {a > 0, b > 0, Element[{x, y}, Reals]}(*, 
   		TransformationFunctions -> 
         		{Automatic, #1 /. Cos[ArcTan[denom_, num_]] -> denom/Sqrt[num^2 + denom^2] & , 
					#1 /. Sin[ArcTan[denom_, num_]] -> num/Sqrt[num^2 + denom^2] & }, 
       		ComplexityFunction -> (LeafCount[#1] + 100*Count[#1, ArcTan[_, _], {0, Infinity}] & )*)]]

FromCylindrical[fvec:{_, _, _}, {r_, t_, z_}, {x_, y_}] := 
	(FromCylindrical[#1, {r, t, z}, {x, y}] & ) /@ (#1 . fvec & ) /@ (ToCylindrical[#1, {x, y, z}, {r, t}] & ) /@ IdentityMatrix[3]

ToPolar[expr_, vars:{_,_}, {r_, t_}] := 
	expr /. PointsToRules[{r Cos[t], r Sin[t]}, vars]//First//Simplify
	
ToPolar[{a_,b_}] := ToPolarCoordinates[{a,b}];

FromPolar[{r_,t_}]:=FromPolarCoordinates[{r,t}]
	
FromPolar[expr_, vars:{_,_}, {x_,y_}, list:{_,_}:{1,1}] :=
	FromCylindrical[expr, Flatten[{vars,Unique[]}],{x,y},list]
	
		
RulesToPoints[rules_List, vars_] := 
	Flatten[{vars}]/.rules

VarCheck::variables = "Incorrect variables or number of variables.";

PointsToRules[pts_List,vars_] := 
	Block[{varss, ptss},
	varss = Flatten[{vars}];
	ptss = Which[VectorQ[pts[[1]]],pts,
		          Length[varss] == 1,Partition[pts,1],
		          True,{pts}];
	If[Length[varss] != Length[ptss[[1]]],Return[Message[VarCheck::variables]]];
	Map[Thread[varss -> #]&,ptss]
	]

(*Coefficients[expr_,vars_List] := Coefficient[expr,#]& /@ Flatten[{vars}]*)



(*KnoxTicks[a_,s_:1] := a Range[Ceiling[N[1/a #1]], Floor[N[1/a #2]],s]&*)




ConstantTerm[0, ___] := 0

ConstantTerm[expr_] :=  
	Select[FixedPoint[Expand,expr] + Unique[], NumberQ[N[#]]&] 

ConstantTerm[expr_, opts__?OptionQ] := 
	ConstantTerm[expr,
    		Select[Variables[expr], FreeQ[#, Alternatives @@ Flatten[{Constants /. {opts} /. Constants -> 0}]] & ]]
		
ConstantTerm[expr_,varslist_] := 
	Select[FixedPoint[Expand,expr] + Unique[] First[Flatten[{varslist}]], 
				FreeQ[#,Alternatives[Sequence @@ varslist]]& ]



(*
SubsetEqualQ[expr1_, expr2_] :=
	Length @ Complement[expr1, expr2] == 0
*)




(* Maeder command *)   

MakeMesh[vl_List] :=
Block[{l = vl, l1 = Map[RotateLeft, vl], mesh},
	mesh = {l, RotateLeft[l], RotateLeft[l1], l1};
	mesh = Map[Drop[#, -1]&, mesh, {1}];
	mesh = Map[Drop[#, -1]&, mesh, {2}];
	Transpose[ Map[Flatten[#, 1]&, mesh] ]]
	
MakePolygons[vl_List] :=
	Polygon /@ MakeMesh[vl] 



(*
WireFrame[object_, style___] := 
 	object /. 
  {GraphicsGroup[__] -> {}, 
  Line[a_] -> Flatten[{style, Line[{a}]}],
  Polygon[x_] :> Flatten[{style, Line[ Append[x, First[x]] ]}]}
*)  
  
(*setdp[command_, {}, prange_?MatrixQ, opts___?OptionQ] := 
	{Point[{}], prange}*)
(*Changes 11-23-16*)
setdp[command_, {}, prange_?MatrixQ, opts___?OptionQ] := 
	{{}, prange}

setdp[command_, point_?(VectorQ[#,NumericQ]&), prange_?MatrixQ, opts___?OptionQ] := 
	setdp[command, {point}, prange, opts]
	
setdp[command_, point_, prange_?MatrixQ, opts___?OptionQ] :=
	Module[{ptstyle},
		If[MatrixQ[Join[point,Transpose[prange]](*point*),NumericQ],
			ptstyle = setps5[command, DrawPointStyle, Length[point], opts];
			{MapThread[Flatten[{#1,Point[#2]}]&,{ptstyle, point}],{Min[#], Max[#]} & /@ Transpose[Join[point,Transpose[prange]]]}(*,
			Message[dpoint::drawpt];{Point[{}],prange}*)]
		]


setps5[command_Symbol, options_Symbol, rest__] :=
	setps5[command, {options}, rest]

setps5[command_Symbol, {options__Symbol}, number_?(IntegerQ[#] && NonNegative[#]&), opts___Rule] :=
	setps5[command, {options}, Table[number,{Length[{options}]}], opts]

setps5[command_Symbol, {options__Symbol}, numbers_?(VectorQ[#, (IntegerQ[#] && NonNegative[#])&]&), opts___Rule] := 
	Module[{defaultstyles, userstyles, lengths, newstyles},
	defaultstyles = {options} /. Options[command];
	userstyles = {options} /. {opts} /. Options[command];
	userstyles = (If[ListQ[#],  #, {#}]& /@ userstyles) /. {} -> {{}};
	lengths = Length /@ userstyles;

	newstyles = MapThread[
			MapThread[
				Flatten[{#1,#2}]&,
				{Table[#1,{#2}],#3}]&,
			{defaultstyles, lengths, userstyles}];
	newstyles = MapThread[
			Join[Flatten[Table[#1,{Floor[#2/#3]}],1], Take[#1,Mod[#2,#3]]]&,
			{newstyles,numbers,lengths}];
	(*With[{temp = Map[NewUnsortedUnion, DeleteCases[newstyles, Automatic, Infinity], {2}]}, 
		If[Length[{options}] === 1, First[temp], temp]]	*)
	With[{temp = DeleteCases[newstyles, Automatic, Infinity]}, 
		If[Length[{options}] === 1, First[temp], temp]]	
		
		]

idealtanline = Transpose[1/10{{-1, 0}, {1, 0}}];

TanLines[vectors_List, cpoint_List, plotrange_List, aspect_, linelength_,idealline_:idealtanline] := 
  Module[{shift, dvs, rotmats,vecs,cpt}, 
	vecs = If[VectorQ[vectors],{vectors},vectors];
	cpt = If[VectorQ[cpoint],{cpoint},cpoint];
	shift = N[-Subtract @@ Transpose[plotrange]/{1, aspect}]; 
	dvs = #1/shift & /@ vecs;
	rotmats = MapThread[If[#1 != {0,0}, shift*({{1, -1}*#1, Reverse[#1]}/Sqrt[#1.#1]),
			Message[TanLines::SingularPoint,#2]; {#1,#1}] &, {dvs,cpoint}]; 	
	MapThread[{{#2}, Transpose[ #1 . (linelength idealline ) + #2]}&,{rotmats,cpt}]]





(*This command is used in PlotSlopeField *)
idealline = Transpose[{{0, 0}, {1/30, 0}}]

SlopeLines[vectors_List, plotrange_List, aspect_, lstyle_, linelength_,idealline_:idealline] := 
  Module[{ shift, dvs, rotmats}, 
  
 linestyle = setps5[SlopeLines, LineStyle, Length[vectors], LineStyle -> lstyle];
	shift = N[-Subtract @@ Transpose[plotrange]/{1, aspect}]; 

	dvs = (-Subtract @@ #1/shift & ) /@ vectors; 

	rotmats =  If[#1 != {0,0},shift*({{1, -1}*#1, Reverse[#1]}/Sqrt[#1.#1]),Message[SlopeLines::SingularPoint,#1];{#1,#1}] &  /@ dvs; 

	MapThread[Flatten[{#3,Line[Transpose[#1 . (linelength idealline) + First[#2]]]}] & , {rotmats, vectors,linestyle}]]





idealarrow =   Transpose[N[{{-1/30,1/55},{-1/35,0}, {-1/30,-1/55},{0,0}}]];

ArrowHead[anything_]:=Arrowhead[anything]

Arrowhead[vectors_List, plotrange_List, aspect_, plotstyle_, asize_: 1] :=
	Module[{nvectors = N[vectors],  shift, dvs, rotmats},
(*	pstyle	= Map[Cases[#, Thickness[n_] -> n, Infinity]&, plotstyle];
	pstyle	= Map[If[# == {}, 1, 130 Last[#]]&, pstyle];
*)
	shift		= -Subtract @@ Transpose[plotrange]/{1, aspect}//N;
	dvs		= Map[((-Subtract @@ #)/shift)&, nvectors];
	rotmats	= Map[ If[# != {0,0}, shift ({{1,-1} #, Reverse[#]}/Sqrt[#.#]), {{0,0},{0,0}}]&, dvs];
	MapThread[Flatten[{#3,Polygon[Transpose[#1.(asize idealarrow) + Last[#2]]]}]&, {rotmats,vectors,plotstyle}]
]


(*

idealarrow3D1 = Transpose /@ ( .005  {{{-3, 0, 1}, {-3, -Sqrt[3]/2, -1/2}, {0, 0, 0}}, 
     {{-3, -Sqrt[3]/2, -1/2}, {-3, Sqrt[3]/2, -1/2}, {0, 0, 0}}, 
     {{-3, Sqrt[3]/2, -1/2}, {-3, 0, 1}, {0, 0, 0}}});
     
ArrowHead3D[everything_] := Arrowhead3D[everything]

				 
Arrowhead3D[vectors_List, plotrange_List, box_List, plotstyle_, asize_: 1] :=
	Module[{ pstyle, color, shift, dvs, rotmats, heads = Polygon /@ (asize idealarrow3D1)},

	pstyle	= Map[Cases[#, Thickness[n_] -> n, Infinity]&, plotstyle];

	pstyle	= Map[If[# == {}, 1, 100 Last[#]]&, pstyle];

	color 	= plotstyle /. {Hue[n_] :> Glow[Hue[n]],
				RGBColor[n__] :> Glow[RGBColor[n]],
				GrayLevel[n_] :> Glow[GrayLevel[n]]};
	shift		= (-Subtract @@ Transpose[plotrange])/box Sqrt[box.box]//N;
	dvs		= Map[((-Subtract @@ #)/shift &), vectors];
	rotmats 	= Map[If[Drop[#,1] == {0,0},
					(Sign[#[[1]]] shift) {{1,0,0},{0,1,0},{0,0,1}},
					shift RotationMatrix[ArcCos[({1,0,0}.#)/Sqrt[#.#]],Cross[{1,0,0},#]]]&, dvs];

	MapThread[
		heads /.Polygon[data_] :> 
			Flatten[{#4, #5, Polygon[Transpose[#1.(#3 data) + Last[#2]]]}]&,
			{rotmats,vectors,pstyle,plotstyle,color}]
]

*)




PolynomialDegree[poly_, varslist_] :=
	Module[{epoly = Expand[poly]},
		If[!PolynomialQ[epoly, varslist], Message[Poly::notpoly, poly, varslist]; Return[] ];
	If[Head[epoly] =!= Plus,
		Plus @@ Exponent[epoly, varslist],
		Max @ Map[(Plus @@ Exponent[#, varslist])&,Level[epoly, 1] ]]
]


Polynomial[x_, (deg_Integer)?Positive, coeffs_List] := 
  First[coeffs]+ x^Range[1, deg] . Rest[coeffs] /; Length[coeffs] === deg + 1

Polynomial[x_,deg_Integer?Positive,a_Symbol, opts___?OptionQ] := 
  Module[{subscript},
    subscript=Subscript/.{opts}/.Options[Polynomial];
    If[subscript,
      Polynomial[x, deg, Map[Subscript[a,#]&,Range[0,deg]]], Polynomial[x, deg, Array[a,deg + 1,0]]]]


NewUnsortedUnion[{}] := {}

NewUnsortedUnion[list_List] := 
	Fold[If[MemberQ[#1,#2],#1,Join[#1,{#2}]]&,
		{First[list]},Rest[list]]

NewUnsortedUnion[list__List] := 
	Fold[If[MemberQ[#1,#2],#1,Join[#1,{#2}]]&,
		First[{list}],Join[Sequence @@ Rest[{list}]]]

UnsortedComplement[list1_List,list2_List]:=
	If[MatchQ[Flatten[list2],{}],
		list1,
		Select[list1,Not[MemberQ[list2,#1]]&]
]


End[];
SetAttributes[TimingCommand,HoldFirst]
SetAttributes[{TruthTable, setdp, TanLines, OrderODE, (*Triangle, Parallelogram,*) Swap, (*NRotationMat3D,*)
Polynomial, PolynomialDegree,
ToSpherical, ToCylindrical, ToPolar, FromSpherical, FromCylindrical, FromPolar, PointsToRules, RulesToPoints,

NewUnsortedUnion, UnsortedComplement,  (*ArrowHead, Arrowhead, ArrowHead3D, Arrowhead3D, *)
ConstantTerm, 
firstnonzero, (*idealarrow, idealarrow3D1,*) MakePolygons, nonzerotest, zerotest, setps5, WireFrame},
{ReadProtected, Protected, Locked}];
	
EndPackage[];
