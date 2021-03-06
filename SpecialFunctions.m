
(* Context: KnoxPackages`SpecialFunctions` *)

(* Mathematica Version 11.1 *)

(* Author: Dennis M. Schneider *)

(* Copyright 1990-18 by Dennis M. Schneider *)

(* 
   :Work on this package partially supported by grants from
		Pew Charitable Trusts
		National Science Foundation
         	NSF-ILI Grant # USE-9050757
         	NSF Grant # USE-9153249
		Knox College
		Lilly Foundation 
*)

(* Commands taken over in version 7: SymmetricMatrixQ, SquareWave, SawtoothWave. Replace by KPCommandName *)

Unprotect[SetAttributes, Set, Attributes];
Clear[SetAttributes, Set, Attributes];
Protect[SetAttributes, Set, Attributes];

(*
Abort::nolic = "KnoxPackages are not licensed to this machine.";
If[Not[MatchQ["5119-78768-59794", $MachineID]], Message[Abort::nolic]; Quit[]];
*)

BeginPackage["KnoxPackages`SpecialFunctions`",
	{"KnoxPackages`CommonFunctions`","KnoxPackages`LinearAlgebra`"}];




Stair::usage = "Stair[a][t] gives a stair with a rise of 1 (starting at t = 0) and a run of a.";

SquarePulse::usage = "SquarePulse[a][t] gives a unit square pulse over the interval 0 <= t < a.";

KPSquareWave::usage = "KPSquareWave[d,T][t] gives a periodic square wave of period T with duty cycle d, 0 <= d <= 1.";

SawtoothPulse::usage = "SawtoothPulse[a][t] gives a unit sawtooth over the interval 0 <= t < a.";

KPSawtoothWave::usage = "KPSawtoothWave[d,T][t] gives a periodic unit sawtooth wave of period T with duty cycle d, 0 <= d <= 1.";

TrianglePulse::usage = "TrianglePulse[a][t] gives a unit triangular pulse over the interval 0 <= t <= a.";

KPTriangleWave::usage = "KPTriangleWave[d,T][t] gives a periodic triangular wave of period T with duty cycle d, 0 <= d <= 1.";





InvoluteEqn::usage ="InvoluteEqn[r][t] gives parametric equations of the involute of a circle of radius r.";

TrochoidEqn::usage ="TrochoidEqn[a,b][t] gives parametric equations of the trochoid generated by a point b units from the center of a circle of radius a.";

CycloidEqn::usage ="CycloidEqn[r][t] gives parametric equations of the cycloid generated by a circle of radius r.";

EpitrochoidEqn::usage ="EpitrochoidEqn[a, b, c][t] gives parametric equations of the curve that is traced out by a point c units from the center of a circle of radius b rolling outside a circle of radius a.";

EpicycloidEqn::usage ="EpicycloidEqn[a, b][t] gives parametric equations of the epicycloid generated by a circle of radius b rolling on the outside of a circle of radius a.";

HypotrochoidEqn::usage ="HypotrochoidEqn[a, b, c][t] gives parametric equations of the curve that is traced out by a point c units from the center of a circle of radius b rolling inside a circle of radius a.";

HypocycloidEqn::usage ="HypocycloidEqn[a, b][t] gives parametric equations of the hypocycloid generated by a circle of radius b rolling on the inside of a circle of radius a.";

AgnesiEqn::usage ="AgnesiEqn[a][t] returns parametric equations for the witch of Agnesi whose Cartesian equation is x^2 y = (2 a)^2 (2 a - y).";

CissoidEqn::usage ="CissoidEqn[a][t] returns parametric equations for the cissoid of Diocles whose Cartesian equation is (x^2 + y^2) x = 2 a y^2.";

Hyperbolic::usage = "Setting Hyperbolic->True uses hyperbolic functions in the parametrization. ";


(*  3D curves and surfaces  *)

MobiusStripEqn::usage=
"MobiusStripEqn[R][u,t], -d <= u <= d, 0<= t <=2Pi,  returns parametric equations for a Mobius strip whose center circle is in the plane z = 0 and has radius R (default value 2) and whose width is 2d."

KleinBottle::usage = 
"Parametric equations for usual embedding of the Klein bottle can be found at http://mathworld.wolfram.com/KleinBottle.html";

TorusEqn::usage ="TorusEqn[r, a][u, v] returns parametric equations of a torus with wheel radius r and tube radius a. The parameter u gives the angle of rotation of the wheel, and v gives the portion of the tube.  TorusEqn[r, a, b][u, v] returns parametric equations of a torus with wheel radius r and elliptical tube determined by a and b. ";

TorusKnotEqn::usage=
"TorusKnotEqn[p, q ,r:2,a:1][t] returns parametric equations of the (p,q)-torus knot, p and q relatively prime, which winds q times around a circle inside the torus and p times around a line through the hole in the torus. The curve lives on torus with wheel radius r (default value 2) and tube radius a (default value 1). ";

SphereEqn::usage ="SphereEqn[r][phi, theta] returns parametric equations of the sphere x^2 + y^2 + z^2 = r^2 using spherical coordinates phi and theta as parameters.";

EllipsoidEqn::usage ="EllipsoidEqn[a, b, c][t, p] returns parametric equations of the ellipsoid with x^2/a^2 + y^2/b^2 + z^2/c^2 = 1 using spherical coordinates phi and theta as parameters.";

SphericalHelixEqn::usage ="SphericalHelixEqn[r,m][t] returns a helix on a sphere of radius r.  m determines the number of winds around the z-axis when t ranges from 0 to 2 Pi m.";




ConeEqn::usage = 
"ConeEqn[a,b,c][u,t] returns parametric equations for the cone x^2/a^2 + y^2/b^2 - z^2/c^2 = 0 whose axis is the z-axis.  Axis -> {1,0,0} or {0,1,0} returns equations for the cone whose axis is the x-axis (-x^2/a^2 + y^2/b^2 + z^2/c^2 = 0) or the y-axis (x^2/a^2 - y^2/b^2 + z^2/c^2 = 0).  The parameter t controls the rotation about the axis of the cone, and u (which can be negative) determines the extent along its axis.";


Hyperboloid1Eqn::usage = 
"Hyperboloid1Eqn[a,b,c,opts][u,t] returns parametric equations for the hyperboloid of one sheet x^2/a^2 + y^2/b^2 - z^2/c^2 = 1 whose axis is the z-axis.  Axis -> {1,0,0} or {0,1,0} returns equations for the hyperboloid of one sheet whose axis is the x-axis (-x^2/a^2 + y^2/b^2 + z^2/c^2 = 1) or the y-axis (x^2/a^2 - y^2/b^2 + z^2/c^2 = 1).  Hyperbolic -> True will return equations using hyperbolic functions.  The parameter t controls the rotation about the axis of the hyperboloid, and u (which must be greater than 1 for non-hyperbolic equations) determines the extent along its axis.";


Hyperboloid2Eqn::usage = 
"Hyperboloid2Eqn[a,b,c,opts][u,t] returns parametric equations for the hyperboloid of two sheets -x^2/a^2 - y^2/b^2 + z^2/c^2 = 1 whose axis is the z-axis.  Axis -> {1,0,0} or {0,1,0} returns equations for the hyperboloid of two sheets whose axis is the x-axis (x^2/a^2 - y^2/b^2 - z^2/c^2 = 1) or the y-axis (-x^2/a^2 + y^2/b^2 - z^2/c^2 = 1).  Hyperbolic -> True will return equations using hyperbolic functions.  The parameter t controls the rotation about the axis of the hyperboloid, and u determines the extent along its axis. In the first template, u must be at least 1. For the hyperbolic equations, u must be at least 0.";

Options[Hyperboloid2Eqn] = {Axis -> {0,0,1}, Hyperbolic -> False};

Options[Hyperboloid1Eqn] = Options[ConeEqn] = {Axis -> {0,0,1}, Hyperbolic -> False}; 
	
Eqn::positive = "`1` called with at least one nonpositive argument; positive numbers expected.";



Begin["`Private`"];






(*SquarePulse[t_, a_] := UnitStep[t] - UnitStep[t-a]*)
SquarePulse[a_][t_] := PiecewiseExpand[UnitStep[t] - UnitStep[t-a]]
KPSquareWave[d_, T_][t_] := PiecewiseExpand[SquarePulse[d T][Mod[t,T]]]



SawtoothPulse[a_][t_] := PiecewiseExpand[SquarePulse[a][t]/a t]
KPSawtoothWave[d_, T_][t_] := PiecewiseExpand[SawtoothPulse[d T][Mod[t, T]]]


Stair[a_][t_] := UnitStep[t] Floor[t/a]


TrianglePulse[a_][t_] := PiecewiseExpand[2/a (SquarePulse[a/2][t] t - SquarePulse[a/2][t-a/2](t-a))]
KPTriangleWave[d_, T_][t_] := PiecewiseExpand[TrianglePulse[d T][Mod[t, T]]]


ConeEqn[a_,b_,c_,opts___Rule][u_,t_] := 
Module[{axis,eqns = {u Cos[t], u Sin[t], u}  },
	axis = Axis /. {opts} /. Options[Hyperboloid1Eqn];
	{a,b,c} Switch[Position[axis,1][[1,1]],
		1, RotateRight[eqns],
		2, Swap[eqns,2,3],
		3, eqns]]


Hyperboloid1Eqn[a_, b_, c_, opts___Rule][u_, t_] := 
	Module[{axis, type, eqns}, 
	If[Not[And@@Positive[{a,b,c}]], Return[Message[Eqn::positive, "Hyperboloid1Eqn"]]];
	{axis, type} = {Axis, Hyperbolic} /. {opts} /. Options[Hyperboloid1Eqn]; 
	eqns = If[type, 
		{{Cosh[u]*Cos[t], Cosh[u]*Sin[t], Sinh[u]}}, 
		{{u*Cos[t], u*Sin[t], Sqrt[u^2-1]},{u*Cos[t], u*Sin[t], -Sqrt[u^2-1]}}]; 
	eqns = {a,b,c}*#& /@ 
 		(Switch[Position[axis, 1][[1,1]], 
 			1, RotateRight/@eqns, 
 			2, Swap[#, 2, 3]& /@ eqns, 
 			3, eqns]);
	 If[Length[eqns] == 2, eqns, First[eqns]]]




Hyperboloid2Eqn[a_, b_, c_, opts___Rule][u_, t_] := 
	Module[{axis, type, eqns}, 
	If[Not[And@@Positive[{a,b,c}]], Return[Message[Eqn::positive, "Hyperboloid2Eqn"]]];
	{axis, type} = {Axis, Hyperbolic} /. {opts} /. Options[Hyperboloid1Eqn]; 
      	
	eqns = If[type, 
  		{{Sinh[u] Cos[t], Sinh[u] Sin[t], Cosh[u]},
			{Sinh[u] Cos[t], Sinh[u] Sin[t], -Cosh[u]}},
  		{{Sqrt[-1 + u^2] Cos[t], Sqrt[-1 + u^2] Sin[t], u},
  			{Sqrt[-1 + u^2] Cos[t], Sqrt[-1 + u^2] Sin[t], -u}}];

	{a,b,c} #& /@  Switch[Position[axis, 1][[1,1]], 
 		1, RotateRight /@ eqns, 
 		2, Swap[#, 2, 3]& /@ eqns, 
 		3, eqns]]

InvoluteEqn[r_][t_] := r {Cos[t] + t Sin[t], Sin[t] - t Cos[t]}

TrochoidEqn[a_,b_][t_] := {a t - b Sin[t], a - b Cos[t]}

CycloidEqn[r_][t_] := TrochoidEqn[r,r][t]

EpitrochoidEqn[a_,b_,c_][t_] := {(a + b) Cos[t] - c Cos[(a + b)/b t],(a + b) Sin[t] - c Sin[(a + b)/b t]}

EpicycloidEqn[a_, b_][t_] := EpitrochoidEqn[a,b,b][t]

HypotrochoidEqn[a_,b_,c_][t_] := {(a - b) Cos[t] + c Cos[(a - b)/b t],(a - b) Sin[t] - c Sin[(a - b)/b t]}

HypocycloidEqn[a_, b_][t_] := HypotrochoidEqn[a,b,b][t]

AgnesiEqn[r_][t_] :=  2 r {Tan[t],Cos[t]^2}

CissoidEqn[a_][t_] := 2a {t^2/(1 + t^2),t^3/(1 + t^2)}





(*  3D curves and surfaces  *)

MobiusStripEqn[R_:2][u_, t_] := 
  {(R + u Cos[t/2]) Cos[t], (R + u Cos[t/2]) Sin[t], u Sin[t/2]}


TorusEqn[r_,a_,b_][u_, v_]:= {(r + a Cos[v]) Cos[u],(r + a Cos[v]) Sin[u], b Sin[v]}
TorusEqn[r_, a_][u_, v_] := TorusEqn[r,a,a][u,v]

TorusKnotEqn[p_, q_,r_:2, a_:1][t_] := {(r + a Cos[(q/p) t]) Cos[t], 
     (r + a Cos[(q/p) t]) Sin[t], a Sin[(q/p) t]}

SphereEqn[r_][p_, t_] := EllipsoidEqn[r,r,r][p,t]
EllipsoidEqn[a_,b_,c_][p_, t_] := {a Sin[p] Cos[t], b Sin[p] Sin[t], c Cos[p]}
SphericalHelixEqn[r_, m_][t_] := r {Sin[t/(2 m)] Cos[t], Sin[t/(2 m)] Sin[t], Cos[t/(2 m)]}

End[];

SetAttributes[{KPSquareWave, SquarePulse, SawtoothPulse, KPSawtoothWave, TrianglePulse, KPTriangleWave, Stair, ConeEqn, Hyperboloid1Eqn, Hyperboloid2Eqn, 

InvoluteEqn, TrochoidEqn, CycloidEqn, EpitrochoidEqn, EpicycloidEqn, HypotrochoidEqn, 
HypocycloidEqn, AgnesiEqn, CissoidEqn, TorusEqn, TorusKnotEqn, MobiusStripEqn, SphereEqn, EllipsoidEqn, 
SphericalHelixEqn}, {ReadProtected, Protected, Locked}];

EndPackage[];
