(* Context:  KnoxPackages`ModifySystem` *)  

(* Mathematica Version 11.1 *)  

(*:Author: Dennis M. Schneider  *) 

(* :Copyright: Copyright 1990 - 2018, Dennis M. Schneider *)

(* Work on this package partially supported by grants from:
         Pew Charitable Trusts
                   and
         National Science Foundation
         	NSF-ILI Grant # USE-9050757
         	NSF Grant # USE-9153249
             	   and
         Knox College
*)

(*:Keywords:
	Power
*)

(*:Requirements: None. *)

(*:Warnings: This packages modifies the Power command*)


(*:Summary:
This package allows one to:
	specify that Mathematica should use real roots instead of principle roots by setting the option PowerBehavior;
	convert results returned by Mathematica commands from exponential form to the form a + b I;
	specify that Norm and Normalize should consider n-tuples to be n-tuples of reals;
Piecewise has been modified so that the default value is not zero;
*)


Unprotect[SetAttributes, Set, Attributes];
Clear[SetAttributes, Set, Attributes];
Protect[SetAttributes, Set, Attributes];

(*
Abort::nolic = "KnoxPackages are not licensed to this machine.";
If[Not[MatchQ["5119-78768-59794", $MachineID]], Message[Abort::nolic]; Quit[]];
*)

BeginPackage["KnoxPackages`ModifySystem`", 
	{"KnoxPackages`CommonFunctions`","KnoxPackages`Calculus`"}];

RemovePB::usage = "RemovePB[] removes modifications to Power made by PowerBehavior.";

RestorePB::usage = "RestorePB[] restores modifications to Power made by PowerBehavior.";

RemovePiecewise::usage = "RemovePiecewise[] removes modifications to Piecewise.";

RestorePiecewise::usage = "RestorePiecewise[] restores modifications to Piecewise, namely that the default value should be the numeric zero. ";

PowerBehavior::usage = 
"PowerBehavior -> Real is an option to all Mathematica commands that requests Mathematica to return the real solution to an odd root of a negative number instead of its complex roots. Use with care.";

RealPower::usage = 
"RealPower[expression] evaluates expression using real roots rather than complex roots.";

KnoxPower::usage = 
"KnoxPower[expression] obsolute; it returns RealPower[expression].";


ReImForm::usage = 
"ReImForm[expression] returns expression with complex numbers converted from exponential form to a + b I form.";

$PowerBehavior::usage = 
"$PowerBehavior = Real forces Mathematica to always choose the real root instead of the principal complex root.";

$PowerFlag::usage = "A flag to prevent an infinite loop.";

$LimitFlag::usage = "A flag to prevent an infinite loop.";

$PiecewiseFlag::usage = "A flag to prevent an piecewise from converting Integrate to NIntegrate.";

$AbsFlag::usage = "Coming soon";

Norm::usage =      StringJoin[Norm::usage, "\n", "Norm[v, Reals] treats all symbolic entries in v as real." ]; 

Normalize::usage = StringJoin[Normalize::usage, "\n", "Normalize[v, Reals] treats all symbolic entries in v as real." ]; 

PowerBehavior::wrongvalue = 
	"PowerBehavior -> `1` is not a valid setting:  possible settings are Real or Complex. ";

PowerBehavior::notoption = "PowerBehavior is not an option for Solve or Reduce. You probably want to add the domain Reals as the third argument. ";

Plot::dui = "Plot, ParametricPlot, Plot3D, or ParametricPlot3D will not accept an equation as input. ";

ExpForm::usage = 
"ExpForm -> False is an option to all Mathematica commands that requests Mathematica to express complex numbers in the form a + b I. ";

ExpForm::wrongvalue = 
	"ExpForm -> `1` is not a valid setting;  possible settings are True or False. ";

Norm::dui = 
  "Vector `1` contains complex entries. Mathematica's default norm will be used. ";



Begin["`Private`"];

$PowerBehavior = Complex;
$PiecewiseFlag = True;

$PowerFlag = False;
$ExpForm = False;

$AbsFlag = True;




RemovePiecewise[] := $PiecewiseFlag = False;

RestorePiecewise[] := $PiecewiseFlag = True;


RemovePB[]:=(Unprotect[Rule]; Clear[Rule]; $PowerFlag = False; Protect[Rule]; Print["Modifications to Power have been removed."])

RestorePB[] := 
	(
		$PowerFlag = True;
		Unprotect[Rule];
		Clear[Rule];
	

				
(*	Rule /: head_[expr_, x_ -> b_, before___, PowerBehavior -> Real, after___] :=
			head[RealPower[expr], x -> b, before, after];  (*Modified 12-01-15 *)*)


(*	Required to get Plot to work well with Abs and PowerBehavior 
	Rule /: head_[expr_, more___, PowerBehavior -> (pb:(Real | Complex)), after___] :=
			Block[{$PowerBehavior, neww, return}, 
					$PowerBehavior = pb;	
					Update[Power];
					neww = expr/.Abs[stuff_]:>ReleaseHold[Abs[stuff]];
					return = head[neww, more, after];
					Update[Power];
					return
				]/; (Not[FreeQ[expr,Abs,Infinity]]);	
				


	Rule /: head_[before___, PowerBehavior -> (pb:(Real | Complex)), after___] :=
			Block[{$PowerBehavior, return}, 
					$PowerBehavior = pb;	
					Update[Power];
					return = head[before, after];
					Update[Power];
					return
				];				

*)
		Rule /: head_[before___, PowerBehavior -> (pb:(Real | Complex)), after___] :=
			Block[{$PowerBehavior,return}, 
			Which[
				MatchQ[head, Solve | Reduce],
					Message[PowerBehavior::notoption];
					Update[];
					head[before, after],
					
				MatchQ[head, Roots],
					Update[];
					head[before, after],

				MatchQ[head, _],
					$PowerBehavior = pb;				
					Update[Power];
				(*	Update[Abs];*)
					return = head[before, after];
					Update[];
					return]
				];
 
		Rule /: head_[before___, PowerBehavior -> other_, after___] :=
			Return[Message[PowerBehavior::wrongvalue, other]];


		Protect[Rule];
	)

RestorePB[];

Unprotect[Rule]

Rule /: head_[before___, ExpForm -> (ef:(True | False)), after___] :=
	Block[{$ExpForm = ef}, 
	If[ef, 
		head[before, after],
		ReImForm[head[before, after]] ]]

Rule /: head_[before___, ExpForm -> other_, after___] :=
	Return[Message[ExpForm::wrongvalue, other]]; 


Protect[Rule];


Unprotect[Power];
(*
Power[x_?(Head[N[#]] === Real && N[#] < 0&), Rational[p_, q_?OddQ]] :=
	Block[{$PowerFlag = False},
  		(-1)^p ((-x)^(1/q))^p
	] /; $PowerFlag && $PowerBehavior === Real
*)

Power[x_, Rational[p_, q_?OddQ]] :=
	Block[{$PowerFlag = False},
  		Surd[x, q]^p
	]/; $PowerFlag && $PowerBehavior === Real	

Power[Surd[x_, q_?OddQ], p_?Negative] := 
	Block[{$PowerFlag = False},
		Surd[x, q]^(q + p)/x
	]/; $PowerFlag && $PowerBehavior === Real  
	
Protect[Power];


(*
(* Fixed some problems with PlotJump and Piecewise functions, but perhaps not for the right reasons *)(* 07-25-2014 Uncommented Abs *) (*12-15-15 Commented out*)
Unprotect[Abs]
Abs[x_]:=
	Block[{$AbsFlag = False},
			ReleaseHold[Abs[x]]
	] /; $AbsFlag && $PowerBehavior === Real
Protect[Abs]
*)


(* Commented out on 09/05/2013. Uncommented out 09/20/2013. Changed 12/14/15*)
Unprotect[Piecewise];
Piecewise[{anything___List, {0, a_}, moreanything___List}] := 
		Piecewise[{anything, {0., a}, moreanything}]/; $PiecewiseFlag
Protect[Piecewise];

(*
Unprotect[Integrate];
Integrate[fun_,rest__]:=
	Block[{funh, $PiecewiseFlag = False},
		funh = fun /. {0.,a_}->{0,a};
		Integrate[funh,rest]
	]/;Not[FreeQ[fun,Piecewise]]&& $PiecewiseFlag
Protect[Integrate];	
*)
(*
RealPower[expr_] := 
	expr/.Power[x_, Rational[p_, q_?OddQ]] :> Surd[x, q]^p
*)	

RealPower[expr_] := 
	Block[{$PowerBehavior = Real, return},
	Update[Power];
	return = expr;
	Update[];
	return
	] 

	
KnoxPower[expr_] := RealPower[expr]

ReImForm[expr_List] := Map[Factor[ExpToTrig[#]]& , expr, Infinity] /. 
     			(Complex[0, c_])*(Complex[0, b_] + a_) -> Expand[(Complex[0, c])*(Complex[0, b] + a)]

ReImForm[expr_] := Map[Factor[ExpToTrig[#]]& , {expr}, Infinity] /. 
     			(Complex[0, c_])*(Complex[0, b_] + a_) -> Expand[(Complex[0, c])*(Complex[0, b] + a)]//First


Unprotect[Norm]

Norm[vec_?VectorQ, Reals] :=
 If[FreeQ[vec, Complex], 
 	Sqrt[Simplify[vec.vec]],
 	Message[Norm::dui, vec]; Norm[vec]
  ]

Norm[vec_?VectorQ, norm_, Reals] :=
 If[FreeQ[vec, Complex], 
 	Sqrt[Simplify[vec.vec]],
 	Message[Norm::dui, vec]; Norm[vec]
  ]
  
 Norm[number_, Reals]:=
 Norm[number]


Protect[Norm]


Unprotect[Normalize]
Normalize[vec_, Reals] := vec/Norm[vec, Reals]
Protect[Normalize]




End[];

SetAttributes[{ $PowerBehavior, $PowerFlag, (*$PiecewiseFlag,*) $PlotFlag}, 
	{Protected}];  (* $PiecewiseFlag cannot be protected because of RemovePiecewise command *)


SetAttributes[RealPower, HoldAll (*HoldFirst*)];  (*Allows RealPower to work with plotting function that have attributes HoldFirst. *)

SetAttributes[{PowerBehavior, RemovePB, RestorePB, RemovePiecewise, RestorePiecewise, ReImForm, RealPower, KnoxPower}, 
	{ReadProtected, Protected, Locked}];

EndPackage[];
