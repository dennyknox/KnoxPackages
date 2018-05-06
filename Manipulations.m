(* ::Package:: *)

(* Context:  KnoxPackages`Manipulations` *)  

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

BeginPackage["KnoxPackages`Manipulations`",
	{"KnoxPackages`Calculus`", "KnoxPackages`LinearAlgebra`", "KnoxPackages`CommonFunctions`", "KnoxPackages`ModifySystem`"}];

	
(*
BeginPackage["KnoxPackages`Manipulations`"(*,{"KnoxPackages`Calculus`"}*)];
*)
ManipulateSecantLine::usage = 
"Under construction. When done, ManipulateSecantLine[fun,{x,x0,x1},tanpt] produces an animation of the secant lines as x approaches tanpt from the right.  With Direction -> Left (Automatic), x approaches tanpt from the left (both sides).  ShowTangentLine -> True includes the tangent line.  DisplaySlope -> True prints the slopes of the secant lines.  Frames -> n produces an animation with n frames (default is 15 frames).  With Limit -> True, the derivative is computed as a limit of difference quotients.  Styles for the secant lines and points are set with LineStyle and PointStyle.  Other options are those of PlotJump.";

ManipulateCircle::usage = 
"ManipulateCircle[r] produces an animation of a circle centered at {0,0} with radius r being traced out.  Frames->n produces an animation with n frames.  Styles for the radius and angle text can be set with RadiusStyle and AngleStyle.  The style of the circle is set by PlotStyle.";

ManipulateEllipse::usage =
"ManipulateEllipse[{xradius,yradius}] produces an animation of an ellipse centered at {0,0} with semi-axes of lengths xradius and yradius being traced out.  Frames->n produces an animation with n frames.  Styles for the radius and angle text can be set with RadiusStyle and AngleStyle.  The style of the ellipse is set by PlotStyle.";

ManipulateTrochoid::usage = 
"ManipulateTrochoid[r, d, {t,tmin,tmax}] produces an animation of the trochoid determined by a point on a spoke at distance d from the center of the rolling circle for t between tmin and tmax. ManipulateTrochoid[r,d,{t,tmin,tmax,dt}] allows the value of t to vary between tmin and tmax in steps of dt.  Styles for the circle, point and spoke can be set with CircleStyle, PointStyle, and SpokeStyle.  PlotStyle sets the style for the curve.";

ManipulateCycloid::usage = 
"ManipulateCycloid[r,{t,tmin,tmax}] produces an animation of the cycloid determined by a point on the boundary of a rolling circle of radius r for t between tmin and tmax.  ManipulateCycloid[r,{t,tmin,tmax,dt}] allows the value of t to vary between tmin and tmax in steps of dt.  Styles for the circle, point and spoke can be set with CircleStyle, PointStyle, and SpokeStyle.  PlotStyle sets the style for the curve.";

ManipulateInvolute::usage = 
"ManipulateInvolute[r,{t,t0,t1}] produces an animation of the involute determined by the circle of radius r when t is between t0 and t1.  Frames->n produces an animation with n frames.  Styles for the disk, radius, point, and tangent can be set with DiskStyle, RadiusStyle, TangentLineStyle.  PlotStyle sets the style for the involute.";

(*
EpiHypoTrochoid::usage = 
"EpiHypoTrochoid is the main routine for animating trochoids.";
*)

(*
Epi::usage = 
"Epi determines an epitrochoid.";
Hypo::usage = 
"Hypo determines a hypotrochoid.";
*)

ManipulateHypocycloid::usage = 
"ManipulateHypocycloid[bigradius,smallradius,{t,tmin,tmax,dt:.05}] produces an animation of the hypocycloid determined by a point on a circle of radius smallradius rolling on the inside of a circle of radius bigradius when t is between tmin and tmax in steps of dt (default value is .05). Styles for the fixed circle, rolling circle, point, and radius can be set with FixedCircleStyle, RollingCircleStyle, PointStyle, RadiusStyle.  PlotStyle sets the style for the hypocycloid.";

DisplayHypocycloid::usage = 
"DisplayHypocycloid[bigradius,smallradius,{t, tmin,tmax}] produces a picture of the portion of the hypocycloid determined by a point on a circle of radius smallradius rolling on the inside of a circle of radius bigradius when t is between tmin and tmax.";

ManipulateHypotrochoid::usage = 
"ManipulateHypotrochoid[bigradius,smallradius,d,{t, tmin, tmax, dt:.05}] produces an animation of the hypotrochoid determined by a point on a spoke of a circle of radius smallradius that is d units from the center of the circle that is rolling on the inside of a circle of radius bigradius when t is between tmin and tmax in steps of dt (default value is .05).  Styles for the fixed circle, rolling circle, point, and radius can be set with FixedCircleStyle, RollingCircleStyle, PointStyle, RadiusStyle.  PlotStyle sets the style for the hypotrochoid.";

DisplayHypotrochoid::usage = 
"DisplayHypocycloid[bigradius,smallradius,d,{t,tmin,tmax}] produces a picture of the portion of the hypotrochoid determined by a point on a spoke of a circle of radius smallradius that is d units from the center of the circle that is rolling on the inside of a circle of radius bigradiusrolling on the inside of a circle of radius bigradius when t is between tmin and tmax.";

ManipulateEpicycloid::usage = 
"ManipulateEpicycloid[bigradius,smallradius,{t,tmin,tmax,dt:.05}] produces an animation of the epicycloid determined by a point 
on a circle of radius smallradius rolling on the outside of a circle of radius bigradius when t is between tmin and tmax in steps of dt (default value is .05)  Styles for the fixed circle, rolling circle, point, and radius can be set with FixedCircleStyle, RollingCircleStyle, PointStyle, RadiusStyle.  PlotStyle sets the style for the epicycloid.";

DisplayEpicycloid::usage = 
"DisplayEpicycloid[bigradius,smallradius,{t, tmin,tmax}] produces a picture of the portion of the epicycloid determined by a point on a circle of radius smallradius rolling on the outside of a circle of radius bigradius when t is between tmin and tmax.";

ManipulateEpitrochoid::usage = 
"ManipulateEpitrochoid[bigradius,smallradius,d,{t, tmin, tmax, dt:.05}] produces an animation of the epitrochoid determined by a point on a spoke of a circle of radius smallradius that is d units from the center of the circle that is rolling on the outside of a circle of radius bigradius when t is between tmin and tmax in steps of dt (default value is .05). Styles for the fixed circle, rolling circle, point, and radius can be set with FixedCircleStyle, RollingCircleStyle, PointStyle, RadiusStyle.  PlotStyle sets the style for the epitrochoid.";

DisplayEpitrochoid::usage = 
"DisplayEpicycloid[bigradius,smallradius,d,{t,tmin,tmax}] produces a picture of the portion of the epitrochoid determined by a point on a spoke of a circle of radius smallradius d units from the center rolling on the outside of a circle of radius bigradius when t is between tmin and tmax.";

AnimateNewton::usage = 
"Under construction.";

ManipulateTaylor::usage = 
"ManipulateTaylor[f,{x,x0,x1}, center, deg, stepsize] produces an animation whose frames show f and its Taylor polynomials up through degree deg in steps of stepsize.";

ManipulateIntegral::usage = 
"ManipulateIntegral[f,{x,xmin,xmax}] produces an animation of the indefinite integral from xmin to xmax.  Other options are those of PlotIntegral.";

ManipulateParametricPlot::usage = 
"ManipulateParametricPlot[{fx,fy},{t,tmin,tmax}] produces an animation of the tip of the vector tracing out the curve.  VectorStyle sets the style for the vector.  With DrawVector->False, it returns an animation of a point moving along the curve.  PointStyle sets the style for the point. Other options are those of ParametricPlot.";

ManipulateParametricPlot3D::usage = 
"ManipulateParametricPlot3D[{fx,fy,fz},{t,t0,t1}] produces an animation of the tip of the vector tracing out the curve. VectorStyle sets the style for the vector.  With DrawVector->False, it returns an animation of a point moving along the curve.  PointStyle sets the style for the point. Other options are those of ParametricPlot3D.";



AngleStyle::usage = 
"AngleStyle -> style applies style to the angle label in ManipulateCircle and ManipulateEllipse.";

DiskStyle::usage = 
"DiskStyle -> style applies style to the disk in ManipulateInvolute.";

FixedCircleStyle::usage =
"FixedCircleStyle -> style applies style to the fixed circle in ManipulateHypocycloid, ManipulateHypotrochoid, ManipulateEpicycloid and ManipulateEpitrochoid.";

RadiusStyle::usage =
"RadiusStyle -> style applies style to the radius in ManipulateCircle, ManipulateEllipse, ManipulateInvolute, ManipulateHypocycloid, ManipulateHypotrochoid, ManipulateEpicycloid and ManipulateEpitrochoid.";

RollingCircleStyle::usage = 
"RollingCircleStyle -> style applies style to the rolling circle in ManipulateHypocycloid, ManipulateHypotrochoid, ManipulateEpicycloid and ManipulateEpitrochoid.";

SpokeStyle::usage = 
"SpokeStyle -> style applies style to the spoke in ManipulateTrochoid and ManipulateCycloid.";

ManipulatePolarPlot::usage = 
"ManipulatePolarPlot[fun,{t,tmin,tmax}] plots the ray being traced out at t increases. Options are those of PolarPlot and PlotVector.";

ManipulateTrigFunction::usage = 
"ManipulateTrigFunction[f[t], {t,tmin,tmax}] produces an animation of the trig function f[t] being traced out on the interval [t0, t1] as the ray making an angle t with the x-axis moves across the interval [t0, t1].  Frames -> n produces an animation with n frames (default is 15).  Other options are those of Plot."; 

ManipulateTangentVector::usage = 
"ManipulateTangentVector[fun, {t,tmin,tmax}, tpt, opts] returns an animation of the secant vectors approaching the tangent vector. ";

ManipulateTangentVector3D::usage = 
"ManipulateTangentVector3D[fun, {t,tmin,tmax}, tpt, opts] returns an animation of the secant vectors approaching the tangent vector. ";

Options[ManipulateCircle] = Options[ManipulateEllipse] = {PlotStyle->{RGBColor[0,0,1],Thickness[.01]},
	RadiusStyle->{Thickness[.0075],GrayLevel[.33]},AngleStyle->GrayLevel[.33]};

Options[ManipulateTrochoid] = {PlotStyle -> AbsoluteThickness[2], CircleStyle -> {{Hue[0], AbsoluteThickness[2]}}, 
	SpokeStyle -> {{GrayLevel[0.4`], AbsoluteThickness[2]}}, PointStyle -> AbsolutePointSize[5]};

Options[ManipulateCycloid] = Options[ManipulateTrochoid];

Options[ManipulateInvolute] = {PlotStyle->Thickness[.014],DiskStyle->{Opacity[.5],RGBColor[0,0,1]},
	RadiusStyle->{GrayLevel[.4],Thickness[.008]},PointStyle->PointSize[.03],
	TangentLineStyle->{RGBColor[0,0,1],Thickness[.008]}};

Options[EpiHypotrochoid] = {PlotStyle->Thickness[.01],PointStyle->PointSize[.02],
	FixedCircleStyle->{RGBColor[0,0,1],Thickness[.0075]},RadiusStyle->{GrayLevel[.4],Thickness[.0075]},
	RollingCircleStyle->{RGBColor[1,.5,0],Thickness[.0075]},Labels->True};

Options[ManipulateHypocycloid] = Drop[Options[EpiHypotrochoid],-1];

Options[DisplayHypocycloid] = Options[DisplayHypotrochoid] = Options[DisplayEpicycloid] = 
	Options[DisplayEpitrochoid] = Drop[Options[EpiHypotrochoid],-1];

Options[ManipulateEpicycloid] = Options[ManipulateHypocycloid];

Options[ManipulateHypotrochoid] = Options[ManipulateHypocycloid];

Options[ManipulateEpitrochoid] = Options[ManipulateHypocycloid];

Options[ManipulateTaylor] = {PolyStyle -> RGBColor[0., 0., 1.], PointStyle -> PointSize[0.02], Print -> False};

Options[ManipulateIntegral] = {IntegralStyle -> Thickness[0.01]};

Options[AnimateNewton] = Options[PlotNewton];

Options[ManipulateParametricPlot] = Options[ManipulateParametricPlot3D] =
	{DrawVector -> True, VectorStyle -> GrayLevel[.3], PointStyle -> {PointSize[Large], Red}};



Options[ManipulatePolarPlot] = {PlotPoints -> 75};

(* Not Fixed
Options[ManipulateSecantLine] = 
   {ShowTangentLine -> False, Direction -> Right, LineStyle -> {Thickness[.01],Red}, PointStyle -> {Red, PointSize[0.025]}, Frames -> 15, 
    DisplaySlopes -> False, Limit->False}; 
   *)

Options[ManipulateTrigFunction] = {PlotRange -> {-1.1, 1.1}, AspectRatio -> Automatic}//Sort; 

Options[ManipulateTangentVector] = {PointStyle -> {{PointSize[0.03], RGBColor[1, 0, 0]}}}//Sort; 

Options[ManipulateTangentVector3D] = {Frames -> 20, PointStyle -> {{PointSize[0.035],RGBColor[1,0,0]}}}//Sort;



Begin["`Private`"];

ManipulateTangentVector[fun_, {t_, t0_, t1_}, pt_, opts___] := 
	Module[{ptstyle, iter, f, point, curve, npt = N[pt]},
	ptstyle = setps5[ManipulateTangentVector, PointStyle, 1, opts];
	If[pt == t1, t11 = t1 + (t1 - t0)/5, t11 = t1];
	iter = Min[{(t1 - t0)/5, Abs[pt - t0], Abs[pt - t11]}]//N;
	If[iter == 0, iter = (t1 - t0)/5]//N;
	f = Function[t, fun];
	point = f[npt];
	curve = 
		PlotTangentVector[f[t] // Evaluate, {t, t0, t11}, npt, VectorStyle -> Black, PointStyle -> {Flatten[{ptstyle, Black}]}, 
			AspectRatio -> Automatic, Sequence @@ FilterRules[{opts}, Options[PlotTangentVector]] // Evaluate];
	Manipulate[
		Show[
			curve,
			Graphics[Flatten[{ptstyle, Point[f[npt + h]]}]],
			KnoxPackages`Calculus`PlotVector[Evaluate[{point, point + (f[npt + h] - point)/h}], Sequence@@FilterRules[{opts}, Options[KnoxPackages`Calculus`PlotVector]] // Evaluate, 
				VectorStyle -> Red]], 
	{{h, iter, "Parameter"}, iter, 0.0001}]]


ManipulateTangentVector3D[fun_, {t_, t0_, t1_}, pt_, opts___] := 
	Module[{ptstyle, t11, iter, f, point, curve, prange, npt = N[pt]}, 
	ptstyle = setps5[ManipulateTangentVector3D, PointStyle, 1, opts]; 
	If[pt == t1, t11 = t1 + (t1 - t0)/5, t11 = t1]; 
	iter = N[Min[{(t1 - t0)/5, Abs[pt - t0], Abs[pt - t11]}]]; 
	N[If[iter == 0, iter = (t1 - t0)/5]]; 
    f = Function[t, fun]; point = f[npt]; 
	curve = 
		PlotTangentVector3D[Evaluate[f[t]], {t, t0, t11}, npt, VectorStyle -> Black, Boxed -> False, Axes -> False,
			PointStyle -> {Flatten[{ptstyle, Black}]}, AspectRatio -> Automatic, PlotRange -> All, 
			Evaluate[Sequence @@ FilterRules[{opts}, Options[PlotTangentVector3D]]]]; 
	prange = PlotRange /. AbsoluteOptions[ 
		Show[
			curve, 
			Table[Graphics3D[Line[{point, point + (f[pt + h] - point)/h}]], {h, iter, 0.0001, -0.01}], PlotRange -> All, Boxed -> False], 
		PlotRange]; 
	Manipulate[
		Show[
		{curve, 
			Graphics3D[Flatten[{ptstyle, Point[f[npt + h]]}]], 
			KnoxPackages`Calculus`PlotVector3D[Evaluate[{point, point + (f[npt + h] - point)/h}], VectorStyle -> Red, Axes -> False, 
				Boxed -> False]}, PlotRange -> prange, PlotRangePadding -> Scaled[0.06], 
				Sequence @@ Evaluate[FilterRules[{opts}, Options[Graphics3D]]], 
			Boxed -> True, Axes -> Automatic], 
		{{h, iter, "Parameter"}, iter, 0.00001}]]



ManipulateTrigFunction[fun_, {x_, x0_, x1_, dx___}, opts___] := 
	Module[{prange, aratio, line,f}, 
	{prange, aratio} = {PlotRange, AspectRatio} /. {opts} /. Options[ManipulateTrigFunction]; 
	If[Length[Flatten[prange]] == 2, prange = {{x0, x1}, prange}];
	line = Line[{{0, 0}, {Cos[x0], Sin[x0]}}]; 
	f = Function[x,fun];
	Manipulate[Show[GraphicsRow[
   		{Graphics[{Circle[{0, 0}, 1, {x0, x}], 
           Line[{{0, 0}, {Cos[x], Sin[x]}}], line}, AspectRatio -> Automatic, Axes -> Automatic, 
          PlotRange -> {{-1.02, 1.02}, {-1.02, 1.02}}], 
        Quiet[PlotJump[f[t], {t, x0 + .00001, x}, PlotRange -> prange, AspectRatio -> aratio, 
          Evaluate[Sequence@@FilterRules[{opts},Flatten[{Options[PlotJump],Options[Plot]}]]]]]}, 
        GraphicsSpacing -> 0.1]], 
      {x, x0 , x1, dx}]
 ]



(*******************NOT DONE***********************)
(*
ManipulateSecantLine[fun_, {x_, x0_, x1_, dx_:.05}, tanpt_, opts___] := 
	Module[{tline, dir, dslope, limit, lstyle, ptstyle, xstart, xfinish, pt, mainplot, mainplot1, prange, deriv, secpt}, 
	{tline, dir, frames, dslope, limit} = 
		{ShowTangentLine, Direction, Frames, DisplaySlopes, Limit} /. {opts} /. 
    							Options[ManipulateSecantLine]; 
Print[{opts}];
	pb = PowerBehavior /. {opts} /. PowerBehavior->Complex;
Print[pb];
Print[fun];
	{lstyle, ptstyle} = setps5[ManipulateSecantLine, {LineStyle, PointStyle}, 1, opts]; 
	pt = {x, fun} /. x -> tanpt; 
	{xstart,xfinish} = {tanpt - x0, x1 - tanpt};
	iter = Min[{xstart,xfinish}];

	If[iter == 0, iter = Max[{xstart,xfinish}]];

	Switch[dir,
		Right | -1,  
			{xstart,xfinish} = {tanpt+iter,tanpt+iter}; {x00, x11}={x0, Min[{xfinish(*tanpt+iter*),x1}]}; dir = -1; limit = True,
		Left | 1,   		
			{xstart,xfinish} = {tanpt-iter, tanpt-iter}; {x00, x11}={Min[{xstart,x0}], x1};  dir = 1; limit = True,
		_,  
			{xstart,xfinish} = {tanpt-iter, tanpt+iter}; {x00, x11}={Min[{xstart,x0}], Max[{xfinish,x1}]};  dir = Automatic];

	If[tline,
     		mainplot = PlotTangentLine[fun, {x, x00, x11}, tanpt, Limit -> limit, Direction -> dir, opts]];
	If[mainplot === Null || !tline,
     		mainplot1 = PlotJump[fun, {x, x00, x11}, opts],
     		mainplot1 = mainplot];
	prange = PlotRange[mainplot1];
		
	deriv = If[limit, Limit[Chop[Expand[((fun /. x-> tanpt + h) - (fun /. x-> tanpt))/h]], h -> 0, Direction -> dir], 
				D[fun,x]];


	endpt = Max[Norm[{x,fun}-pt]/.{{x->x00},{x->x11}}];
	
secpt = Table[{x, fun} /. x -> i - dx (i - tanpt),{i, x00, x11, dx}];

f=Function[x,{x,fun}];
Print[f[(-1)^(1/3)]];
	Manipulate[
(*		secpt = Evaluate[{x, fun} /. x -> i - dx (i - tanpt)]; *)
secpt=Evaluate[f[i - dx (i - tanpt)]];
Print[secpt];
       	Show[{mainplot1, Graphics[Flatten[{ptstyle, Point[secpt], Point[pt]}]], 
			Graphics[{lstyle,Line[{pt,pt+secpt}]}](*,
			ParametricPlot[pt + t Normalize[secpt - pt],{t, 0, endpt}, PlotStyle->lstyle, PlotPoints->2]*)},
        		PlotRange->prange, PlotLabel -> If[dslope&& Not[Norm[pt-secpt]==0] ,Apply[Divide, Reverse[N[pt - secpt]]],""]],
    	{i, x00, x11, dx}, opts]]

*)


CircleEllipse[axes:{xrad_,yrad_},type_,opts___] :=
	Module[{pstyle, radstyle, angstyle, smallr, bigr, xradius, yradius, plotrange, vec},
	{pstyle,radstyle,angstyle} = {PlotStyle,RadiusStyle,AngleStyle} /. {opts} /. Options[ManipulateEllipse];											
	{pstyle,radstyle,angstyle} = setps5[ManipulateEllipse,{PlotStyle,RadiusStyle,AngleStyle},1,opts];
	{smallr,bigr} = {Min[axes],Max[axes]};
	plotrange = PlotRange[Graphics[Circle[{0,0},bigr]]]; 
	{xradius,yradius} = If[type == 1,{xrad,xrad},{xrad,yrad}];
	Manipulate[
		vec = KnoxPackages`Calculus`PlotVector[{{0,0},{xradius,yradius} {Cos[k],Sin[k]}},PlotStyle->radstyle,
			PlotRange->plotrange,AspectRatio->Automatic];
		Show[Graphics[{{pstyle,Circle[{0,0},{xradius,yradius},{0,k}]}//Flatten,
			{angstyle,Text[k,smallr {1/2,1/4}]}//Flatten}],vec,
		Sequence@@FilterRules[{opts}, Options[Graphics]],PlotRange->plotrange,AspectRatio->Automatic,
		Axes->Automatic],
	{{k,0,"Parameter"},0, 2 Pi}]
]

ManipulateCircle[rad_,opts___] := CircleEllipse[{rad,rad},1,opts]

ManipulateEllipse[{xrad_,yrad_},opts___] := CircleEllipse[{xrad,yrad},0,opts]

ManipulateTrochoid[r_,d_,{k_,tmin_,tmax_,dk___},opts___] := 
	Module[{pltstyle, crcstyle, ptstyle, spkstyle, tro, point, circle, plotrange},
(*	dkk = If[dk === Automatic, (tmax - tmin)/50, dk];*)
	{pltstyle,crcstyle,ptstyle,spkstyle} = 
		setps5[ManipulateTrochoid,{PlotStyle,CircleStyle,PointStyle,SpokeStyle},1,opts];

	tro = Function[t,{r t - d Sin[t], r - d Cos[t]}];
	point = {ptstyle,Point[tro[tmin]]}//Flatten;
	circle = Show[Graphics[{{crcstyle,Circle[{r tmin,r},r]}//Flatten,
					{spkstyle,Line[{{r tmin,r}, tro[tmin]}]}//Flatten,point}],
				Sequence@@FilterRules[{opts}, Options[Graphics]]//Evaluate,
				Axes->Automatic,AspectRatio->Automatic,
				PlotRange->{{r tmin - Max[r,d],r tmax + Max[r,d]},1.25 {Min[-0.2,r-d],Max[2 r,r + d]}}];

	plotrange = PlotRange[circle];

	Manipulate[
		Show[	ParametricPlot[tro[t]//Evaluate,{t,tmin,k},PlotStyle->pltstyle,
				Sequence@@FilterRules[{opts}, Options[ParametricPlot]]//Evaluate],
				(*PlotVector[{{r k,r}, tro[k]},VectorStyle->spkstyle,ArrowSize->3/4],*)
				Graphics[
					{{crcstyle,Circle[{r k,r},r]}//Flatten,
					{spkstyle,Line[{{r k,r}, tro[k]}]}//Flatten,
					point,{ptstyle,Point[tro[k]],Point[{r k,r}]}//Flatten}],
			Sequence@@FilterRules[{opts},Options[Graphics]]//Evaluate,
			PlotRange->plotrange, AspectRatio->Automatic,AxesOrigin->{0,0}],
	{k, tmin + .00001, tmax, dk}]
]


ManipulateCycloid[r_,{k_,tmin_,tmax_,dk___},opts___] := ManipulateTrochoid[r,r,{k,tmin,tmax,dk},opts]


ManipulateInvolute[rad_,{t_,t0_,t1_, dt___},opts___] := 
	Module[{pltstyle,dskstyle,ptstyle,radstyle,tanstyle,circle,c,r,involute,finalpict,pr},
	{pltstyle,dskstyle,ptstyle,radstyle,tanstyle} = 
		{PlotStyle,DiskStyle,PointStyle,RadiusStyle,TangentLineStyle} /. {opts} /. 
			Options[ManipulateInvolute];
	circle = {{dskstyle,Disk[{0,0},rad]}//Flatten,{ptstyle,Point[{0,0}]}//Flatten};
	c = Function[t,rad {Cos[t],Sin[t]}];
	r = Function[t,rad {t Sin[t], -t Cos[t]}];	
	involute = Function[t, r[t] + c[t]];
	finalpict = Show[
			ParametricPlot[involute[t]//Evaluate,{t,t0,t1},PlotStyle->pltstyle,
					Sequence@@FilterRules[{opts}, Options[ParametricPlot]]//Evaluate, AspectRatio->Automatic],
			Graphics[{circle,{radstyle,Line[{{0,0},c[t1]}]}//Flatten,
					{tanstyle,Line[{c[t1],involute[t1]}]}//Flatten,
					{ptstyle,Point[involute[t1]]}//Flatten}],
			Sequence@@FilterRules[{opts}, Options[Graphics]],PlotRange->All];
	pr = PlotRange[finalpict];

	Manipulate[
		Show[
			{ParametricPlot[involute[s]//Evaluate,{s,t0,t},PlotStyle->pltstyle,AspectRatio->Automatic],
			Graphics[{circle,{radstyle,Line[{{0,0},c[t]}]}//Flatten,
					{tanstyle,Line[{c[t],involute[t]}]}//Flatten,
					{ptstyle,Point[involute[t]]}//Flatten}]},
			AxesOrigin->{0,0},
			PlotRange->pr],
		{t, t0 + .0001, t1, dt}]
]


EpiHypotrochoid[a_, b_, d_,{t0_, t1_},type_,opts___] :=
	Module[{labels, plotstyle, fcstyle,rcstyle,ptstyle,radstyle, c, r, hypotro, solidcurve, 
		point,cpoint,radialangle,arcrangeb, outer, inner2, start,  radiusb, nt0 = N[t0], 
		nt1 = N[t1] ,pr, labs},
	{labels,plotstyle,fcstyle,rcstyle,ptstyle,radstyle} = 
		{Labels,PlotStyle,FixedCircleStyle,RollingCircleStyle,PointStyle,RadiusStyle} /. {opts} /. 
			Options[EpiHypotrochoid];
	{c,r} = Which[
		MatchQ[type, Hypo],
			{Function[t, (a - b) {Cos[t], Sin[t]}],
					Function[t, d {Cos[(a - b) t/b], -Sin[(a - b) t/b]}]},
		MatchQ[type, Epi],
			{Function[t, (a + b) {Cos[t], Sin[t]}],
					Function[t, d {-Cos[(a + b) t/b], -Sin[(a + b) t/b]}]}  ];
	hypotro = Function[t, c[t] + r[t]];
	solidcurve = ParametricPlot[ hypotro[t]//Evaluate,{t, nt0, nt1}, PlotStyle->plotstyle,
				Sequence@@FilterRules[{opts}, Options[ParametricPlot]]//Evaluate];
	point = { ptstyle, Point[hypotro[nt1]] };
	cpoint = { ptstyle, Point[c[nt1]] };
	radialangle = ArcTan @@ r[nt1];
	arcrangeb = Sort[ {0, radialangle} ];
	outer = {fcstyle,Circle[{0,0}, a]}//Flatten;
	inner2 = {rcstyle, Circle[c[nt1], b]}//Flatten;
	start = { ptstyle, Point[hypotro[nt0]] }//Flatten;
	radiusb = { radstyle,
		Line[{ c[nt1], hypotro[nt1] }] }//Flatten;
	labs = If[labels,
		Graphics[{				
			(*radiusb2 *) { GrayLevel[0.5], Dashing[{ 0.007, 0.007 }],
				Line[{ c[nt1], c[nt1] + {b, 0} }] },
			(*arca *) Circle[{0,0}, 0.25a, {nt0, nt1}],
			(*arcb *) Circle[c[nt1], 0.3b, arcrangeb],
			(*radiusa *) Line[{ {0,0}, a {Cos[nt0], Sin[nt0]} }];
			(*radiusa2 *) Line[{ {0,0}, c[nt1] }],
			(*radiusa3 *) { GrayLevel[0.5], Dashing[{ 0.007, 0.007 }],
				Line[{ c[nt1], a {Cos[nt1], Sin[nt1]} }] },
			(*labelt *) Text[ Style["t", Italic,Small], 
				0.25a {Cos[(nt0 + nt1)/2],Sin[(nt0 + nt1)/2]},
					-2 {Cos[(nt0 + nt1)/2],Sin[(nt0 + nt1)/2]} ],
			(*labelP *) Text[ Style["P",Small], hypotro[nt1], -3 r[nt1]/d ]}],
		Graphics[{}]];
	pr = If[MatchQ[type,Hypo],Max[a,a + d - b],Max[a + 2b,a + b + d]];
	Show[solidcurve, labs, Graphics[{outer,inner2,start,radiusb,point,cpoint}],
		 Sequence@@FilterRules[{opts}, Options[Graphics]], 
			AxesOrigin->{0, 0}, Ticks->None, AspectRatio->Automatic,Axes->True,
			PlotRange-> pr { {- 1.1, 1.1}, {-1.1, 1.1} } ]
]


DisplayEpicycloid[bigr_,smallr_,{t_, t0_,t1_},opts___] := 
	EpiHypotrochoid[bigr,smallr,smallr,{t0,t1},Epi,opts]

DisplayEpitrochoid[bigr_,smallr_,d_,{t_, t0_,t1_},opts___] := 
	EpiHypotrochoid[bigr,smallr,d,{t0,t1},Epi,opts]

DisplayHypocycloid[bigr_,smallr_,{t_, t0_, t1_},opts___] := 
	EpiHypotrochoid[bigr,smallr,smallr,{t0,t1},Hypo,opts]

DisplayHypotrochoid[bigr_,smallr_,d_,{t_, t0_,t1_},opts___] := 
	EpiHypotrochoid[bigr,smallr,d,{t0,t1},Hypo,opts]


ManipulateHypocycloid[  bigr_, smallr_, {t_, t0_, t1_, dt___},opts___Rule] :=
	Manipulate[ EpiHypotrochoid[bigr, smallr, smallr, {t0, t}, Hypo, opts, Labels->False], 
		{t, t0+.0001, t1, dt}]


ManipulateHypotrochoid[  bigr_, smallr_, dist_, {t_, t0_, t1_, dt___},opts___Rule] :=
	Manipulate[ EpiHypotrochoid[bigr, smallr, dist, {t0, t}, Hypo, opts, Labels->False], 
		{t, t0 + .0001, t1, dt}]



ManipulateEpicycloid[  bigr_, smallr_, {t_, t0_, t1_, dt___},opts___] :=
	Manipulate[ EpiHypotrochoid[bigr, smallr, smallr, {t0, t}, Epi, opts, Labels->False], 
		{t, t0 + .0001, t1, dt}]



ManipulateEpitrochoid[  bigr_, smallr_, dist_, {t_, t0_, t1_, dt___}, opts___] :=
	Manipulate[ EpiHypotrochoid[bigr, smallr, dist, {t0, t}, Epi, opts, Labels->False], 
		{t, t0 + .0001, t1, dt}]

(*
AnimateNewton[func_,{t_,t0_,t1_},pt_,opts___Rule] :=
	Module[{iter,tanstyle,ptstyle,deriv,intercepts,fullplot,anew,bnew},
tanstyle = TangentLineStyle /. {opts} /. (TangentLineStyle -> (LineStyle/.{opts}/.Options[PlotNewton]));
	{iter,ptstyle} = {Iterations,PointStyle} /. 
						{opts} /. Options[PlotNewton];
	deriv = D[func,t];

	intercepts = NestList[N[t - func/deriv/.t->#] &,N[pt],iter];
	If[Not[VectorQ[intercepts,NumberQ[N[#]]&]],Return @ Message[PlotNewton::zeroderiv]];
	Print[N[intercepts,10]];
	{anew,bnew} = {Min[N[t0],intercepts],Max[N[t1],intercepts]};
	intercepts = Partition[Flatten[Map[{{#,0},{#,func/.t->#}}&,intercepts],1],3,2];
	fullplot = 
		Show[
				plot = Plot[func,{t,anew,bnew},
						Evaluate[Sequence@@FilterRules[{opts}, Options[Plot]]]],
				Graphics[Flatten[{tanstyle,ptstyle,Map[Line,intercepts],
						Map[Point,intercepts,{2}]}]],
			Evaluate[Sequence@@FilterRules[{opts}, Options[Plot]]],PlotRange->All];
	plotrange = PlotRange[fullplot];


Show[plot,Graphics[Flatten[{tanstyle,ptstyle,Point[First[First[intercepts]]]}]],PlotRange->plotrange];
	Manipulate[Show[plot,
			Graphics[Flatten[{tanstyle,ptstyle,Map[Line,Drop[intercepts,k - iter]],
				Map[Point,Drop[intercepts,k - iter],{2}]}]]//Evaluate,
				PlotRange->plotrange],
		{k,1,iter}]
]/; NumberQ[N[pt]]
*)


ManipulateTaylor[fun_, {x_Symbol, x0_,x1_}, centerpoint_?(NumericQ), degree_Integer, step_Integer:1, opts___?OptionQ] :=
	Module[{polystyle, ptstyle, plot, seriesplot},
	{polystyle, ptstyle} = {PolyStyle, PointStyle} /. {opts} /. Options[PlotTaylorPoly];
	polystyle = If[Head[polystyle] === List, polystyle, {polystyle}];
	polystyle = Join @@ Map[Table[#, {i, 1, step}]&, polystyle];
	polystyle = setps5[PlotTaylorPoly, PolyStyle, degree, PolyStyle -> polystyle];
	plot = Plot[fun, {x,x0,x1}, Evaluate[Sequence@@FilterRules[{opts}, Options[Plot]]]];
	seriesplot = Map[Normal[Series[fun, {x,centerpoint,#}]]&, Range[1, degree]];
	Manipulate[
		Show[	
			plot,
			Plot[seriesplot[[k]], {x,x0,x1},PlotStyle->polystyle[[k]]]],
		{{k,1,"Degree"}, 1, degree, step}]]
	

ManipulateIntegral[fun_, {x_, x0_, x1_, dx___}, opts___] := 
 Module[{istyle, newf, integral, print, plot, plotrange},
	{istyle, print} = {IntegralStyle, PrintDisplay} /. {opts} /. Options[ManipulateIntegral];

newfun=PiecewiseExpand[fun,x0<=x<=x1];
	integral = Integrate[newfun(*fun*), {x, x0, x},Sequence@@FilterRules[{opts},Options[Integrate]]//Evaluate, Assumptions -> Element[x,Reals]]//Evaluate;
	If[print, Print["The integral of ", fun, " from ", x0, " to ", x," is: ", integral]];
	If[Head[integral] === Integrate, 
		integral = y[x] /. NDSolve[{y'[x] == fun, y[x0] == 0}, y[x], {x, x0, x1}]];
	plot = Plot[Evaluate[{newfun(*fun*), integral}], {x, x0, x1}, FilterRules[{opts}, Options[Plot]] // Evaluate, PlotRange -> All];
	plotrange = PlotRange /. AbsoluteOptions[plot,PlotRange]// Chop;

  	Manipulate[
		Show[{
			PlotJump[newfun(*fun*)//Evaluate, {x, x0, k}, Filling -> Axis, opts, AxesOrigin->{0,0},FillingStyle->{Red,Yellow}]//Quiet, 
			PlotJump[integral//Evaluate, {x, x0, k}, PlotStyle -> istyle, PlotRange -> plotrange, opts]}, 
				PlotRange -> plotrange, PlotRangePadding -> Scaled[.05]], 
			{{k, x0+.001, "Parameter"}, x0+.001, x1, dx}]]


ManipulateParametricPlot[fun_List, {t_, t0_, t1_, dt___}, opts___] :=
	Module[{vector, vstyle, ptstyle, funl, plot, plotrange},
	{vector, vstyle, ptstyle} = {DrawVector, VectorStyle, PointStyle} /. {opts} /. Options[ManipulateParametricPlot];
	funl = If[Head[fun[[1]]] === List, fun, {fun}];	
	ptstyle = setps5[ManipulateParametricPlot, PointStyle, Length[funl], opts];
	plot = ParametricPlot[fun, {t, t0, t1}, FilterRules[{opts}, Options[ParametricPlot]] // Evaluate];
	plotrange = PlotRange/.AbsoluteOptions[plot,PlotRange];
	plot = Show[plot, If[vector, Graphics[Point[{0, 0}]], Graphics[{}]]];

	If[vector,
		Manipulate[
				Show[{
					ParametricPlot[fun, {t, t0 + 0.0000001, k}, PlotRange -> plotrange, Sequence@@FilterRules[{opts}, Options[ParametricPlot]] // Evaluate,
						PlotRangePadding->Scaled[.06]], 
					PlotVector[({{0, 0}, #} & /@ funl) /. t -> k, VectorStyle -> vstyle, Sequence@@FilterRules[{opts}, Options[PlotVector]]//Evaluate]}],
			{{k, t0, "Parameter"}, t0, t1, dt}],
		Manipulate[				
				Show[plot, 
					Graphics[Flatten[MapThread[{#1, Point[#2]} &, {ptstyle, (funl /. t -> k)}]]], PlotRange -> plotrange],
			{{k, t0, "Parameter"}, t0, t1, dt}]]]
	

ManipulateParametricPlot3D[fun_List, {t_, t0_, t1_, dt___}, opts___] :=
	Module[{vector, vstyle, ptstyle, funl, plot, plotrange}, 
	{vector, vstyle, ptstyle} = {DrawVector, VectorStyle, PointStyle} /. {opts} /. Options[ManipulateParametricPlot3D];
	funl = If[Head[fun[[1]]] === List, fun, {fun}];
	ptstyle = setps5[ManipulateParametricPlot3D, PointStyle, Length[funl], opts];
	plot = ParametricPlot3D[fun, {t, t0, t1}, FilterRules[{opts}, Options[ParametricPlot3D]] // Evaluate];
	plot = Show[plot, If[vector, Graphics3D[Point[{0, 0, 0}]], Graphics3D[{}]], PlotRange -> All];
	plotrange = PlotRange[plot];
	If[vector,
		Manipulate[
				Show[
					ParametricPlot3D[fun, {t, t0 + 0.0000001, k}, PlotRange -> plotrange, Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]] // Evaluate],
					KnoxPackages`Calculus`PlotVector3D[({{0, 0, 0}, #} & /@ funl) /. t -> k, VectorStyle -> vstyle, Sequence@@FilterRules[{opts}, Options[KnoxPackages`Calculus`PlotVector3D]]//Evaluate],PlotRangePadding->Scaled[.05]],{{k, t0, "Parameter"}, t0, t1, dt}],
		Manipulate[				
				Show[plot, 
					Graphics3D[Flatten[MapThread[{#1, Point[#2]} &, {ptstyle, (funl /. t -> k)}]]], PlotRange -> plotrange],
			{{k, t0, "Parameter"}, t0, t1, dt}]]]

ManipulatePolarPlot[fun_, {t_, t0_, t1_, dt___}, opts___] :=
	Module[{vector, vstyle, ptstyle, funl, plot, plotrange},
	{vector, vstyle, ptstyle} = {DrawVector, VectorStyle, PointStyle} /. {opts} /. Options[ManipulateParametricPlot];
	funl = Flatten[{fun}];	


	ptstyle = setps5[ManipulateParametricPlot, PointStyle, Length[funl], opts];

	plot = NewPolarPlot[fun, {t, t0, t1}, Sequence@@FilterRules[{opts}, Options[NewPolarPlot]] // Evaluate];

	plot = Show[plot, If[vector, Graphics[Point[{0, 0}]], Graphics[{}]], PlotRange -> All];
	plotrange = PlotRange[plot]; 

	If[vector,
		Manipulate[
				Show[{
					NewPolarPlot[fun, {t, t0 + 0.0000001, k}, PlotRange -> plotrange, Sequence@@FilterRules[{opts}, Options[NewPolarPlot]] // Evaluate,PlotRangePadding->Scaled[.06]],
					PlotVector[({{0, 0}, #{Cos[t],Sin[t]}} & /@ funl) /. t -> k, VectorStyle -> vstyle, Sequence@@FilterRules[{opts}, Options[PlotVector]]//Evaluate]}],{{k, t0, "Parameter"}, t0, t1, dt}],
		pts = Map[# {Cos[t], Sin[t]}&, funl];
		Manipulate[				
				Show[plot, 
					Graphics[Flatten[MapThread[Flatten[{#1, Point[#2]}] &, 
						{ptstyle, pts/. t -> k} ]]], PlotRange -> plotrange],
			{{k, t0, "Parameter"}, t0, t1, dt}]]]

End[];

SetAttributes[{ManipulateCircle, ManipulateEllipse, ManipulateTrochoid, ManipulateCycloid, EpiHypotrochoid, 
	ManipulateHypocycloid, DisplayHypocycloid, ManipulateHypotrochoid, DisplayHypotrochoid, ManipulateEpicycloid, 
	DisplayEpicycloid, ManipulateEpitrochoid, DisplayEpitrochoid, AnimateNewton, ManipulateTaylor, ManipulateIntegral,
		ManipulateParametricPlot, ManipulateParametricPlot3D},
  {HoldFirst, ReadProtected, Protected, Locked}];
	
SetAttributes[{ManipulateTangentVector, ManipulateTangentVector3D, ManipulateSecantLine, ManipulatePolarPlot, 
Epi, Hypo, 
ManipulateInvolute,
  Frames, AngleStyle, DiskStyle, FixedCircleStyle, Labels, RollingCircleStyle, SpokeStyle},
  {ReadProtected, Protected, Locked}];
	
EndPackage[];

