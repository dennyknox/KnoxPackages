(* ::Package:: *)

(* Context:  KnoxPackages`Calculus` *)

(* Mathematica Version:  11.1 *)

(* :Author: Dennis M. Schneider with assistance from 
	Nate Bode, Sylvia Cook, Jeff Dunn, Sanjay Nath, Anthony Rebello, 
	pete schultz, Robby S. Villegas, Jun Zhu *)

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
If[Not[MatchQ["5110-66125-99731", $MachineID]], Message[Abort::nolic]; Quit[]];
*)

(*
PacletManager`RestartPacletManager[
 FileNameJoin[{$TemporaryDirectory, "Paclets"}]]
*)

BeginPackage["KnoxPackages`Calculus`",
	{"KnoxPackages`CommonFunctions`", "KnoxPackages`LinearAlgebra`", "KnoxPackages`ModifySystem`"}];

Acceleration::usage = 
"Acceleration is an option for PlotTangentVector and PlotTangentVector3D which determines whether or not to include the acceleration vector. ";

AccelerationVectorStyle::usage = 
"AccelerationVectorStyle is an option for PlotTangentVector and PlotTangentVector3D that specifies styles in which acceleration vectors are to be drawn. ";

If[Not[ValueQ[AlternatingSum::usage]], 
AlternatingSum::usage = "AlternatingSum[\!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"...a\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)] returns \!\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"-\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"+\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\(\*StyleBox[\"+\",\"TR\"]\)\(\(\(\(\*StyleBox[\"(\",\"TR\"]\)\(\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\^\(\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\)\). "];


ArrowSize::usage = 
"ArrowSize is an option for plotting commands that use arrowheads that determines the factor by which the arrowhead should be magnified. ";

Asymptote::usage = 
"Asymptote is an option for PlotJump and related commands that specifies where vertical asymptotes should be drawn. ";

AsymptoteStyle::usage = 
"AsymptoteStyle is an option for PlotJump and related commands that specifies styles in which asymptotes are to be drawn";

BaseLength::usage = 
"BaseLength is an option for PlotProjection and PlotProjection3D that extends the base line symmetrically from the center of the projection. ";

DrawBaseLine::usage = 
"An option for PlotProjection and PlotProjection3D to determined whether to draw the base line. ";

DrawBaseVector::usage =
"An option for PlotProjection and PlotProjection3D to determined whether to draw the base vector. ";

BaseLineStyle::usage = 
"BaseLineStyle is an option for PlotProjection and PlotProjection3D to specify a style for the base line. ";

BasePoint::usage = 
"BasePoint is an option for PlotProjection and PlotProjection3D that allows the base of vectors to be at a point other than the origin. ";

BaseVectorStyle::usage = 
"BaseVectorStyle is an option that applies styles to the base vectors in PlotProjection and PlotProjection3D. ";

Binormal::usage = 
"Binormal\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the binormal to the curve parametrized by the variable \!\(\*StyleBox[\"t\",\"TI\"]\).
Binormal\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the binormal to the curve parametrized by the variable \!\(\*StyleBox[\"t\",\"TI\"]\) when \!\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\). ";

BisectionMethod::usage = 
"BisectionMethod\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the result of applying the bisection method \!\(\*StyleBox[\"n\",\"TI\"]\) times to a continuous function \!\(\*StyleBox[\"f\",\"TI\"]\) with \!\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"(\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\"*\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"(\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"<\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"0\",\"TR\"]\)\). ";
 
BoundVector::usage = 
"BoundVector is an option for PlotVector and PlotVector3D. BoundVector -> True draws the vector from the first vector in the list in the direction of the second vector in the list. ";

CenterOfCurvature::usage = 
"CenterOfCurvature\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the center of curvature of a two or three dimensional curve \!\(\*StyleBox[\"f\",\"TI\"]\) parametrized by \!\(\*StyleBox[\"t\",\"TI\"]\) at the point \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\). ";

CircleStyle::usage = 
"CircleStyle is an option for PlotOsculatingCircle and PlotOsculatingCircle3D that applies a style to the osculating circle. ";

ClassifyCriticalPoints::usage = 
"ClassifyCriticalPoints\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"cpts\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) attempts to classify the critical points \!\(\*StyleBox[\"cpts\",\"TI\"]\). The function \!\(\*StyleBox[\"f\",\"TI\"]\) may be a function of any number of variables.  The critical points \!\(\*StyleBox[\"cpts\",\"TI\"]\) may be a list of substitution rules or points. ";

ClassifyCriticalPoints::notempty = 
	"The thrid argument must be a nonempty list of critical points. ";	
Color::usage = 
"Color is an option for ContourPlotGradientMethod, PlotGradientMethod, PlotGradientMethodArray and PlotOrbits.  Color -> {GrayLevel, startlevel, finishlevel} shades the curve beginning at GrayLevel[startlevel] to GrayLevel[finishlevel].  Only GrayLevel or Hue can be used as the first argument. ";

ComplexPlot::usage = 
"ComplexPlot\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"d\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"d\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the image under \!\(\*StyleBox[\"f\",\"TI\"]\) of the coordinate grid. ";

ConstraintStyle::usage = 
"ConstraintStyle is an option for PlotConstraint that specifies styles for the constraint curves. ";

ContourPlotGradientMethod::usage = 
"ContourPlotGradientMethod\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"h\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the contour plot of \!\(\*StyleBox[\"f\",\"TI\"]\) and  the points corresponding to \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(TraditionalForm\`\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\), \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"+\",\"TR\"]\)\(\(\*StyleBox[\"h\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)\), ..., \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"+\",\"TR\"]\)\(\(\*StyleBox[\"h\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)\). ";

ContourPlotPosition::usage = 
"ContourPlotPosition is an option for PlotContourSurface that specifies the plane in which to draw the contour plot. ";

ContourPlotSection::usage = 
"ContourPlotSection\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) produces a contour plot of \!\(\*StyleBox[\"f\",\"TI\"]\) together with the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for \!\(\*StyleBox[\"t\",\"TI\"]\) between \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\). ";

CubicSpline::usage = 
"CubicSpline\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"data\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns a CubicSplineFunction object for the natural cubic spline function determined by the data. 
CubicSpline\!\(\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"data\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the spline function. ";

CubicSplineFunction::usage = 
"CubicSplineFunction\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"<>\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) is the data object representing the cubic spline function determined by the data.  \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) is the range of \!\(\*StyleBox[\"x\",\"TI\"]\)-values. 
CubicSplineFunction\!\(\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"<>\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the value of the cubic spline fit to the data at \!\(\*StyleBox[\"x\",\"TI\"]\). ";

Curvature::usage = 
"Curvature\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the curvature of the two or three dimensional curve \!\(\*StyleBox[\"f\",\"TI\"]\) parametrized by \!\(\*StyleBox[\"t\",\"TI\"]\).
Curvature\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the curvature at the point \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\). ";

DashedLine::usage =
"An option to determine whether to include a dashed line from the vector to its projection. ;"

DrawCylinder::usage = 
"DrawCylinder is an option for PlotSection and ParaPlotSection that determines whether to draw the cylinder. ";

DrawLines::usage = 
"DrawLines is an option for ContourPlotGradientMethod and PlotGradientMethodArray that determines if lines connecting the points should be drawn. ";

CylinderBase::usage = 
"CylinderBase is an option for PlotSection and ParaPlotSection that determines where to place the base of the cylinder. If CylinderRange -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) is also specified, then CylinderBase defaults to \!\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\). ";

CylinderRange::usage = 
"CylinderRange is an option for PlotSection and ParaPlotSection that determines the range of the cylinder. ";

CylinderStyle::usage = 
"CylinderStyle is an option for PlotSection and ParaPlotSection that determines the style to use for the cylinder. ";

CylindricalPlot::usage =
"CylindricalPlot\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Cos\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Sin\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) as a function of \!\(\*StyleBox[\"r\",\"TI\"]\) and \!\(\*StyleBox[\"\[Theta]\",\"TI\"]\).  The ranges \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\), \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) must be given in that order, radius first, angle second.  Several functions may be given in a list. 
CylindricalPlot\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Cos\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Sin\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) where \!\(\*StyleBox[\"r\",\"TI\"]\), \!\(\*StyleBox[\"\[Theta]\",\"TI\"]\), and \!\(\*StyleBox[\"z\",\"TI\"]\) are all functions of a parameter \!\(\*StyleBox[\"u\",\"TI\"]\). Several curves can be given in a list. ";

DashedLineStyle::usage = 
"DashedLineStyle is an option for PlotProjection and PlotProjection3D that applies a style to the dashed line. ";

DerivativeStyle::usage = 
"DerivativeStyle is an option for PlotDerivative that applies style to the derivative";

DirCylindricalPlot::usage = 
"DirCylindricalPlot\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) is the same as CylindricalPlot expect that it calls on DirParametricPlot3D instead of ParametricPlot3D. ";

DirectionVector::usage = 
"DirectionVector->{a,b} specifies that the directional derivative should be computed in the direction {a,b}. The vector {a,b} need not be a unit vector - it will be normalized. ";

DirectionalDerivative::usage = 
"DirectionalDerivative\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TR\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) finds the directional derivative of \!\(\*StyleBox[\"f\",\"TI\"]\) at the point \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TR\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) in the direction \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) assuming that \!\(\*StyleBox[\"f\",\"TI\"]\) is differentiable at \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TR\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). The vector \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) need not be a unit vector. If \!\(\*StyleBox[\"f\",\"TI\"]\) is not differentiable at \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\), UseLimit \!\(\*StyleBox[\"->\",\"TR\"]\) True will use the limit definition to calculate the directional derivative. ";

DirParametricPlot3D::usage = 
"DirParametricPlot3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the parametric curve \!\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\), \!\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\), \!\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\) with the direction of the curve indicated by arrows and color.  More than one curve can plotted. ";

DirParametricPlot::usage = 
"DirParametricPlot[\!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)] plots the parametric curve \!\(\(\*StyleBox[\"(\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\")\",\"TR\"]\)\) from \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) to \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\) with the direction of the curve indicated by arrows and color.
DirParameticPlot[\!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)] plots the parametric curves \!\(\(\(\*StyleBox[\"(\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"(\",\"TR\"]\)\(\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\). ";

DirPolarPlot::usage = 
"DirPolarPlot\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the polar equation \!\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\) with the direction of the curve indicated by arrows and color. More than one curve can be plotted. ";

DirSphericalPlot::usage = 
"DirSphericalPlot\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"p\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) is the same as SphericalPlot expect that it calls on DirParametricPlot3D instead of ParametricPlot3D. ";

DistancePointLine::usage = 
"DistancePointLine\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"+\",\"TR\"]\)\(\(\*StyleBox[\"b\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\*StyleBox[\"c\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the distance from the point \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) to the line \!\(\(\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"+\",\"TR\"]\)\(\(\*StyleBox[\"b\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\*StyleBox[\"c\",\"TI\"]\)\). 
DistancePointLine\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"pt\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"paraeqns\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the distance from the point \!\(\*StyleBox[\"pt\",\"TI\"]\) to the line defined parametrically by \!\(\*StyleBox[\"paraeqns\",\"TI\"]\) with \!\(\*StyleBox[\"t\",\"TI\"]\) as parameter in any dimensional space.
DistancePointLine\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"pt\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)  returns the distance from the point \!\(\*StyleBox[\"pt\",\"TI\"]\) to the line determined by the two points \!\(\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\) in any dimensional space. ";

DistancePointPlane::usage = 
"DistancePointPlane\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"pt\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"eqn\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the distance from \!\(\*StyleBox[\"pt\",\"TI\"]\) to the hyperplane defined by \!\(\*StyleBox[\"eqn\",\"TI\"]\) in any dimensional space. 
DistancePointPlane\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"pt\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"paraeqns\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the distance from \!\(\*StyleBox[\"pt\",\"TI\"]\) to the hyperplane defined parametrically by \!\(\*StyleBox[\"paraeqns\",\"TI\"]\) with \!\(\*StyleBox[\"t\",\"TI\"]\) as parameter in any dimensional space. ";

KPDiv::usage = 
"KPDiv\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) computes the divergence \!\(\(\*StyleBox[\"\[Del]\",\"TR\"]\)\(\(\*StyleBox[\"\[CenterDot]\",\"TR\"]\)\(\*StyleBox[\"f\",\"TI\"]\)\)\) of the vector field with respect to the given variables. 
KPDiv\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"pt\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) computes the divergence at the point \!\(\*StyleBox[\"pt\",\"TI\"]\). ";

DividedDifferenceTable::usage = 
"DividedDifferenceTable\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"data\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns a list of lists, with the \!\(\(\*StyleBox[\"k\",\"TI\"]\)\^\(\*StyleBox[\"th\",\"TI\"]\)\) list being the divided difference of order \!\(\*StyleBox[\"k\",\"TI\"]\). 
DividedDifferenceTable\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"data\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"m\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the divided differences of orders 0 through \!\(\*StyleBox[\"m\",\"TI\"]\). ";

DrawArrowheads::usage = 
"DrawArrowheads is an option for plotting commands that use arrowheads that determines whether to suppress the arrowheads. ";

DrawAxis::usage = 
"DrawAxis is an option for SurfaceOfRevolution and ParaSurfaceOfRevolution that specifies that the axis about which the curve is revolved be drawn. ";

DrawRegion::usage = 
"DrawRegion is an option for SurfaceOfRevolution used to determine whether to draw the region to be revolved. ";

DvecS::usage=
"DvecS\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the cross product of the partial of the vector-valued function \!\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) with respect to \!\(\*StyleBox[\"u\",\"TI\"]\) and the partial of \!\(\*StyleBox[\"f\",\"TI\"]\) with respect to \!\(\*StyleBox[\"v\",\"TI\"]\). ";

Euler::usage = 
"Euler is an option for PlotSlopeField that will include an Euler approximation with the initial value(s) set with the option InitialValue. ";

EulerMethod::usage = 
"EulerMethod\!\(\(\*StyleBox[\"[\",\"TI\"]\)\(\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\*StyleBox[\"eqn\",\"TI\"]\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\"initialvalue\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"[\",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"d\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TI\"]\)\) returns the Euler approximation with step size \!\(\(\*StyleBox[\"d\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\) to the solution \!\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"[\",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TI\"]\)\) of the differential equation eqn with the given initial value. If \!\(\(\*StyleBox[\"d\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\) is not given, then it is set to \!\(\(\(\*StyleBox[\"(\",\"TI\"]\)\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\"-\",\"TI\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\)\(\*StyleBox[\")\",\"TI\"]\)\)\(\*StyleBox[\"/\",\"TI\"]\)\(\*StyleBox[\"20\",\"TR\"]\)\).  
EulerMethod\!\(\(\*StyleBox[\"[\",\"TI\"]\)\(\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\*StyleBox[\"eqns\",\"TI\"]\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\"initialvalues\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"[\",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"d\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TI\"]\)\) returns the Euler approximations to the solutions \!\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"[\",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TI\"]\)\) of the system of differential equations eqns with the given initial values. ";

Exact::usage = 
"Exact is an option for most of the plot integral approximation commands. If set to True and if exact numbers are given as input, exact arithmetic is used to compute the approximation ";

FaceStyle::usage=
"FaceStyle is an option for SurfaceOfRevolution that specifies the style of the region used to connect the two surfaces.";

FindCriticalPoints::usage = 
"FindCriticalPoints\!\(\(\*StyleBox[\"[\",\"TI\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TI\"]\)\) attempts to solve \[Del]\!\(\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TI\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TI\"]\)\)\(\*StyleBox[\"==\",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\*StyleBox[\"0\",\"TR\"]\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"}\",\"TI\"]\)\)\). The function \!\(\*StyleBox[\"f\",\"TI\"]\) can be a function of any number of variables. 
FindCriticalPoints\!\(\(\*StyleBox[\"[\",\"TI\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"pts\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TI\"]\)\) applies FindRoot to solve the equations \[Del]\!\(\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TI\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TI\"]\)\)\(\*StyleBox[\"==\",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\*StyleBox[\"0\",\"TR\"]\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"}\",\"TI\"]\)\)\) for each of the points in the list \!\(\*StyleBox[\"pts\",\"TI\"]\) which can be a list of points \!\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TI\"]\)\) or a list of replacement rules \!\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TI\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TI\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TI\"]\)\). ";

FindRoots::usage = 
"FindRoots[\!\(\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) applies FindRoot using each of the starting points \!\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\).
FindRoots[\!\(\(\(\(\*StyleBox[\"lhs\",\"TI\"]\)\(\*StyleBox[\"==\",\"TR\"]\)\(\*StyleBox[\"rhs\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) applies FindRoot using each of the starting points \!\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\).
FindRoots[\!\(\(\(\(\*StyleBox[\"lhs\",\"TI\"]\)\(\*StyleBox[\"==\",\"TR\"]\)\(\*StyleBox[\"rhs\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) attempts to approximate all solutions in the interval \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\).
FindRoots[\!\(\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) searches for simultaneous numerical roots of the \!\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\).
FindRoots[\!\(\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"eqn\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"eqn\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) searches for simultaneous numerical solutions of the equations \!\(\(\*StyleBox[\"eqn\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\). ";

KPGrad::usage = 
"KPGrad[\!\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)] computes the gradient of the function with respect to \!\(\*StyleBox[\"vars\",\"TI\"]\).
KPGrad[\!\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"pt\",\"TI\"]\)\)] computes the gradient at the point \!\(\*StyleBox[\"pt\",\"TI\"]\).
KPGrad[\!\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)] computes the gradient at the points \!\(\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\). ";

Grad::usage = StringJoin[Grad::usage, "\n", 
"See documentation for KPGrad for additional functionality provided by KnoxPackages." ]; 

Div::usage = StringJoin[Div::usage, "\n", "See documentation for KPDiv for additional functionality provided by KnoxPackages. " ]; 

GradientMethod::usage = 
"GradientMethod\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"h\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the result of applying the gradient method starting at \!\(\(\*StyleBox[\"(\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\")\",\"TR\"]\)\) with stepsize \!\(\*StyleBox[\"h\",\"TI\"]\) until the result no longer changes. 
GradientMethod\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"h\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) stops after \!\(\*StyleBox[\"n\",\"TI\"]\) iterations. ";

DrawGraph::usage = 
"DrawGraph is an option for various plotting commands which if set to False will suppress the graph in the output. ";

HessianMatrix::usage = 
"HessianMatrix\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) computes the Hessian matrix of the function \!\(\*StyleBox[\"f\",\"TI\"]\) with respect to the variables \!\(\*StyleBox[\"vars\",\"TI\"]\). 
HessianMatrix\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"pt\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) computes the Hessian at the point \!\(\*StyleBox[\"pt\",\"TI\"]\). UseLimit -> True uses limits to compute the partial derivatives. This option is set automatically for functions with head Piecewise. ";

ImplicitD::usage = 
"ImplicitD\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"eqn\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) is a generalization of Dt that returns the derivatives up through order \!\(\*StyleBox[\"n\",\"TI\"]\) of \!\(\*StyleBox[\"y\",\"TI\"]\) with respect to \!\(\*StyleBox[\"x\",\"TI\"]\) as determined by the equation \!\(\*StyleBox[\"eqn\",\"TI\"]\). The default value of \!\(\*StyleBox[\"n\",\"TI\"]\) is 1. ";

ImplicitPlot::usage=
"ImplicitPlot\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"eqn\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) uses a contour plot method for generating the plot. More than one equation may be given in a list. ";

InitialValue::usage = 
"InitialValue is an option for PlotSlopeField, PlotDSolve, PlotNDSolve for specifying initial values. ";

IntegralApprox::usage = 
"IntegralApprox\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"ApproxType\",\"TR\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"method\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"partition\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns an approximation to the integral of \!\(\*StyleBox[\"f\",\"TI\"]\) over the interval \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) based on the specified approximation \!\(\*StyleBox[\"method\",\"TI\"]\) and the \!\(\*StyleBox[\"partition\",\"TI\"]\). ";

IntegralStyle::usage = 
"IntegralStyle is an option for PlotIntegral that specifies styles to be applied to the plot of the integral. ";

IntegrationRegion3D::usage = 
"IntegrationRegion3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"(\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\(\*StyleBox[\"(\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\")\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"F\",\"TI\"]\)\(\*StyleBox[\"(\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"G\",\"TI\"]\)\(\*StyleBox[\"(\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the region \!\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\"<=\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"<=\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\), \!\(\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"(\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\"<=\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"<=\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\(\*StyleBox[\"(\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\")\",\"TR\"]\)\)\), \!\(\(\(\*StyleBox[\"F\",\"TI\"]\)\(\*StyleBox[\"(\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\"<=\",\"TR\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\"<=\",\"TR\"]\)\(\(\*StyleBox[\"G\",\"TI\"]\)\(\*StyleBox[\"(\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\). Faces -> False suppresses the faces. FaceStyle applies a style to the faces. FaceMesh -> Automatic applies a mesh to the faces. ";

InverseStyle::usage = 
"InverseStyle is an option for PlotInverse that specifies styles to be applied to the plot of the inverse. ";

IteratedLimit::usage = 
"IteratedLimit\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"expr\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\*StyleBox[\"a\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) computes the iterated limit Limit[Limit[\!\(\(\(\(\(\*StyleBox[\"expr\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\*StyleBox[\"a\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\). The expression \!\(\*StyleBox[\"expr\",\"TI\"]\) can involve any number of variables. ";

Iterations::usage = 
"Iterations is an option for PlotNewton that specifies how many iterations to perform. ";

JacobianDet::usage = 
"JacobianDet\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"funs\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)] returns Det[JacobianMatrix\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"funs\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)]. 
JacobianDet\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"funs\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"pt\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) computes the Jacobian determinant at the point \!\(\*StyleBox[\"pt\",\"TI\"]\). UseLimit -> True uses limits to compute the partial derivatives. This option is set automatically for functions with head Piecewise. ";

JacobianMatrix::usage = 
"JacobianMatrix[\!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"funs\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)] computes the Jacobian matrix of the functions \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"funs\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\) with respect to the variables \!\(\*StyleBox[\"vars\",\"TI\"]\).
JacobianMatrix[\!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"funs\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"pt\",\"TI\"]\)\)] computes the Jacobian matrix at the point \!\(\*StyleBox[\"pt\",\"TI\"]\).
JacobianMatrix[\!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"funs\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\)\)] computes the Jacobian matrix at the points \!\(\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\). ";

Jump::usage = 
"Jump is an option for PlotJump and related commands that specifies a list of x-coordinates of special function values. ";


KPLaplacian::usage = 
"KPLaplacian\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) computes the Laplacian of the function \!\(\*StyleBox[\"f\",\"TI\"]\) with respect to \!\(\*StyleBox[\"vars\",\"TI\"]\). KPLaplacian\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"pt\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) computes the Laplacian of the function at the point \!\(\*StyleBox[\"pt\",\"TI\"]\). UseLimit -> True uses limits to compute the partial derivatives. This option is set automatically for functions with head Piecewise. ";

LevelCurve::usage = 
"LevelCurve\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the level curves of \!\(\*StyleBox[\"f\",\"TI\"]\) at levels \!\(\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\). More than one function \!\(\*StyleBox[\"f\",\"TI\"]\) can be given. ";

LineLength::usage = 
"LineLength is an option for PlotTangentLine, ParaPlotTangentLine, PlotSlopeField, ImplicitPlot that determines the factor by which the length of lines should be magnified. ";

LinePoint::usage = 
"LinePoint is an option for SurfaceOfRevolution that specifies that the revolution axis should pass through the specified point. ";

LineStyle::usage = 
"LineStyle is an option to set the style for lines in ContourPlotGradientMethod, ParaPlotArea, PlotArcLengthApprox, PlotArcLengthApprox3D, PlotEuler, PlotGradientMethod, PlotGradientMethodArray, PlotInverse, PlotNewton, PlotProjection, PlotRegression, PlotRegression3D, PlotSlopeField, PolarPlotArea. ";

LUSumPlot::usage = 
"Main body of code for PlotIntergalApprox and related commands. ";

MaxDeltax::usage = 
"MaxDeltax\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"dx\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) determines a partition of an interval \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) whose mesh is less than \!\(\*StyleBox[\"dx\",\"TI\"]\). ";

MidpointError::usage = 
"MidpointError\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the error estimate for the midpoint sum approximation of the function \!\(\*StyleBox[\"f\",\"TI\"]\) with continuous second derivative on the interval \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) based on a regular partition of the interval into \!\(\*StyleBox[\"n\",\"TI\"]\) subintervals. "


NewLimit::usage = 
"NewLimit\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"expr\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) determines both the right and left hand limits and, if they are equal, returns their common value. If they are not equal, it reports that the limit does not exist as well as the values of the right and left hand limits. If Direction is given as an option, then it will respect that directive. ";

NewNLimit::usage = "Coming soon. ";


NewPolarPlot::usage=
"Same as PolarPlot with the options PolarRay and PolarRayStyle available. ";

NewtonMethod::usage = 
"NewtonMethod\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) applies Newton's method \!\(\*StyleBox[\"n\",\"TI\"]\) times to a differentiable function \!\(\*StyleBox[\"f\",\"TI\"]\) with starting value \!\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\).  
NewtonMethod\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"lhs\",\"TI\"]\)\(\*StyleBox[\"==\",\"TR\"]\)\(\*StyleBox[\"rhs\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns NewtonMethod\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"lhs\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"rhs\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\).  
NewtonMethod\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"eqns\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) applies Newton's method \!\(\*StyleBox[\"n\",\"TI\"]\) times to the system of differentiable functions \!\(\*StyleBox[\"eqns\",\"TI\"]\) with starting values \!\(\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\). ";

NPlotRoots::usage = 
"NPlotRoots\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"eqn\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the solutions of \!\(\*StyleBox[\"eqn\",\"TI\"]\) found by using NSolve. ";

Nullcline::usage = 
"Nullcline is an option for PlotSlopeField that will include the nullcline of the differential equationin the plot. ";

NullclineStyle::usage = 
"NullclineStyle is an option that allows a style to be set for a nullcline. ";

Orbit::usage = 
"Orbit is an option for PlotDSolve, PlotNDSolve, PlotEuler, PlotRungeKutta. Orbit -> True will result in a plot of the orbit (\!\(\*StyleBox[\"y\",\"TI\"]\) vs \!\(\(\*StyleBox[\"y\",\"TI\"]\)\^\(\*StyleBox[\",\",\"TI\"]\)\)) of the solution of a second order differential equation.  It will give the phase plot for a system of 2 or 3 equations. ";

Orientation::usage = 
"Orientation is an option for SurfaceOfRevolution and ParaSurfaceOfRevolution. Orientation -> Textbook gives a plot with the \!\(\*StyleBox[\"y\",\"TI\"]\)- and \!\(\*StyleBox[\"z\",\"TI\"]\)-axes interchanged. ";

OsculatingCircle::usage = 
"OsculatingCircle\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"fun\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns parametric equations of the osculating circle to the curve \!\(\*StyleBox[\"fun\",\"TI\"]\) defined parametrically by \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) or \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) at the point corresponding to the parameter value \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\) with \!\(\*StyleBox[\"u\",\"TI\"]\) as the parameter. ";

OsculatingPlane::usage = 
"OsculatingPlane\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns parametric equations of the osculating plane to the curve \!\(\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\) at a point \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) with \!\(\*StyleBox[\"u\",\"TI\"]\) and \!\(\*StyleBox[\"v\",\"TI\"]\) as parameters. 
OsculatingPlane\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns parametric equations of the osculating plane to the curve \!\(\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\) at the point \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) with u and v as parameters. "

OSection::usage = 
"OSection is an option for PlotSection, ParaPlotSection, ContourPlotSection, and Plot3DArray. OSection -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)  specifies that the oblique planes (surfaces) \!\(\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\) be plotted together with the given surface.  In ParaPlotSection, the independent variables \!\(\*StyleBox[\"x\",\"TI\"]\) and \!\(\*StyleBox[\"y\",\"TI\"]\) must be specified using the option SectionVariables. ";

OSectionStyle::usage = 
"OSectionStyle applies styles to an oblique section. ";

ParaCubicSpline::usage = 
"ParaCubicSpline\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"data\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) should be used for \!\(\*StyleBox[\"data\",\"TI\"]\) that is not given by the graph of a function.  It returns a ParaCubicSplineFunction object for the natural cubic spline function determined by the data; the parameter lies in the interval \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"1\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Length\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"data\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\).  
ParaCubicSpline\!\(\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"data\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the parametric equations for the spline function. Setting the option SecondDerivatives -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"sd\",\"TI\"]\)\_\(\(\*StyleBox[\"1\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"sd\",\"TI\"]\)\_\(\(\*StyleBox[\"1\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"2\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"sd\",\"TI\"]\)\_\(\(\*StyleBox[\"2\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"sd\",\"TI\"]\)\_\(\(\*StyleBox[\"2\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"2\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) returns the cubic spline whose second derivatives at the endpoints are the vectors \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"sd\",\"TI\"]\)\_\(\(\*StyleBox[\"1\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"sd\",\"TI\"]\)\_\(\(\*StyleBox[\"1\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"2\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) and \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"sd\",\"TI\"]\)\_\(\(\*StyleBox[\"2\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"sd\",\"TI\"]\)\_\(\(\*StyleBox[\"2\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"2\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\); the default is SecondDerivatives -> {{0,0},{0,0}} for the natural cubic spline. FirstDerivatives -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"fd\",\"TI\"]\)\_\(\(\*StyleBox[\"1\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"fd\",\"TI\"]\)\_\(\(\*StyleBox[\"1\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"2\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"fd\",\"TI\"]\)\_\(\(\*StyleBox[\"2\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"fd\",\"TI\"]\)\_\(\(\*StyleBox[\"2\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"2\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) returns the clamped cubic spline determined by these first derivative conditions at the endpoints. ";

ParaCubicSplineFunction::usage = 
"ParaCubicSplineFunction[{CubicSplineFunction[{1, n} ,<>], CubicSplineFunction[{1, n} ,<>]}] is the data object representing parametric equations of the cubic spline function determined by the data; the parameter lies in the interval (1, Length[data]).  ParaCubicSplineFunction[{CubicSplineFunction[{1, n} ,<>], CubicSplineFunction[{1, n} ,<>]}][val] returns the value of the cubic spline fit to the data at val. ";


ParaPlotArea::usage = 
"ParaPlotArea\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the area bounded by the graph of \!\(\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\) for \!\(\*StyleBox[\"t\",\"TI\"]\) between \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\) and the \!\(\*StyleBox[\"x\",\"TI\"]\)-axis. If two sets of parametric equations are given, the area between the two curves is shaded. Regions that overlap are not shaded. Joined -> False will prevent the regions from being joined to form one region. Between -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) will shade the area swept out for \!\(\*StyleBox[\"t\",\"TI\"]\) between \!\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\) and \!\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\), \!\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\) and \!\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\), ....  Styles for the curve and region are set with PlotStyle and FillingStyle. DrawArrowheads -> False will suppress the arrowheads. ";


ParaPlotCubicSpline::usage = 
"ParaPlotCubicSpline[data] plots the parametric equations of the cubic spline function determined by the data corresponding to SecondDerivatives -> {{0,0},{0,0}}.  Setting the option SecondDerivatives -> {{sd11,sd12}, {sd21, sd22}} plots the cubic spline whose second derivatives at the endpoints are the vectors {sd11,sd12} and {sd21, sd22}; the default is SecondDerivatives -> {{0,0},{0,0}} for the natural cubic spline. FirstDerivatives -> {{fd11, fd12}, {fd21, fd22}} plots the clamped cubic spline determined by these first derivative conditions at the endpoints.  Points -> False suppresses the plotting of the data points.  PointStyle -> style applies style to the points.   Other options are those of ParametricPlot. ";


ParaPlotSection::usage =
"ParaPlotSection\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)  plots the surface \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\) and the section determined by the cylinder described parametrically by \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\), drawn from the base of the plot box to the surface. If the thrid component of the cylinder is not given, then it is drawn to the top of the box. The cylinder is an optional argument.  ";

ParaPlotSurfaceField::usage = 
"ParaPlotSurfaceField\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"s\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"s\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"s\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) 
plots the vector field \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) and the surface \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). ";

ParaPlotTangentLine::usage = 
"ParaPlotTangentLine\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve given parametrically by \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\) and tangent lines at points whose coordinates are \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\).  More than one curve may be specified in a list. UseLimit -> True uses the limit definition to compute derivatives. ";

ParaPlotTangentPlane::usage = 
"ParaPlotTangentPlane[\!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"3\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)] plots the parametric surface and the tangent plane at the point \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"(\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"(\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"3\",\"TR\"]\)\)\(\*StyleBox[\"(\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\).
ParaPlotTangentPlane[\!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"3\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)] plots the parametric surface and the tangent planes at several points.
ParaPlotTangentPlane[\!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"3\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"3\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)] plots the parametric surfaces and the tangent planes at several points. ";

ParaSurfaceOfRevolution::usage = 
"ParaSurfaceOfRevolution\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"AxisOfRevolution\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"w\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns plots of the curve whose parametric equations are\!\(\(\*StyleBox[\" \",\"TI\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\)\), and the surface obtained by revolving the curve about the line through the origin whose direction vector is \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"w\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\).  Two sets of parametric equations can be given. ";

ParaTangentLine::usage = 
"ParaTangentLine\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns parametric equations of the tangent line to the parametrically defined curve \!\(\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\) at the point \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). Several vector functions and several points may be given. ";

ParaTangentPlane::usage = 
"ParaTangentPlane[\!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"3\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)] returns a vector equation of the tangent plane to the surface parametrized by \!\(\*StyleBox[\"u\",\"TI\"]\) and \!\(\*StyleBox[\"v\",\"TI\"]\) at the point \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"(\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"(\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"3\",\"TR\"]\)\)\(\*StyleBox[\"(\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\).
ParaTangentPlane[\!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"3\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)] returns vector equations of the tangent planes at several points. ";

PhiSection::usage = 
"PhiSection -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) plots the sections determined by \!\(\(\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\) in SphericalPlot. ";

DrawPlane::usage = 
"DrawPlane is an option for several commands that determines whether or not to draw a plane. ";

PlaneSize::usage = 
"PlaneSize is an option for enlarging or shrinking the size of a plane in a plot like PlotTangentPlane. ";

Plot3DArray::usage = 
"Plot3DArray\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns a graphics row consisting of the plots produced by PlotSection and ContourPlotSection. DensityPlot -> True will include the density plot. ";

PlotAcceleration3D::usage = 
"PlotAcceleration3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for \!\(\*StyleBox[\"t\",\"TI\"]\) between \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\) along with the acceleration vectors at the points on the curve corresponding to \!\(\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\).  TNProjections -> True will plot the tangential and normal components of the acceleration vector along with the acceleration vector. "

PlotAcceleration::usage = 
"PlotAcceleration\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for \!\(\*StyleBox[\"t\",\"TI\"]\) between \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)  along with the acceleration vectors at the points on the curve corresponding to \!\(\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\).  TNProjections -> True will plot the tangential and normal components of the acceleration vector along with the acceleration vector. ";

PlotArcLengthApprox3D::usage = 
"PlotArcLengthApprox3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for \!\(\*StyleBox[\"t\",\"TI\"]\) between \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\), the polygonal approximation based on \!\(\(\(\*StyleBox[\"(\",\"TR\"]\)\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\"-\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\"/\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\) points, and prints the approximation of the arc length based on this polygonal path. ";

PlotArcLengthApprox::usage = 
"PlotArcLengthApprox\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for \!\(\*StyleBox[\"t\",\"TI\"]\) between \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\), the polygonal approximation based on \!\(\(\(\*StyleBox[\"(\",\"TR\"]\)\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\"-\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\"/\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\) points, and prints the approximation of the arc length based on this polygonal path. 
PlotArcLengthApprox\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots PlotArcLengthApprox\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"tmin\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"tmax\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\). ";


PlotBand::usage = 
"PlotBand[\!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the band determined by the two curves in space. ";

PlotConstraint::usage = 
"PlotConstraint\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the contour plot of \!\(\*StyleBox[\"f\",\"TI\"]\) along with the constraint curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for \!\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\"<=\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"<=\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\).  
PlotConstraint\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"constrainteqns\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the contour plot of \!\(\*StyleBox[\"f\",\"TI\"]\) along with the constraint curve defined by the \!\(\*StyleBox[\"constrainteqns\",\"TI\"]\). ";

PlotContourSurface::usage = 
"PlotContourSurface\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns a plot of the surface with the contour lines drawn on the surface and the contour plot below the surface. ";

PlotCubicSpline::usage = 
"PlotCubicSpline\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"data\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the natural cubic spline function (the cubic spline whose second derivatives at the endpoints are 0) determined by the data.  
PlotCubicSpline\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"data\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"SecondDerivative\",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"sd\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"sd\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the cubic spline whose second derivatives at the endpoints are \!\(\(\*StyleBox[\"sd\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\) and \!\(\(\*StyleBox[\"sd\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\); the default is SecondDerivatives -> {0,0} for the natural cubic spline.  
PlotCubicSpline\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"data\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"FirstDerivative\",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"fd\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"fd\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the clamped cubic spline determined by these first derivative conditions at the endpoints. ";

PlotCylinder::usage = 
"PlotCylinder\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"h\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"h\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"h\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"h\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots a cylinder over the base curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) in the \!\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)-plane with height from \!\(\(\*StyleBox[\"h\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) to \!\(\(\*StyleBox[\"h\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\). 
PlotCylinder\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"h\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"h\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"h\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"h\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots a cylinder over the base curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) in the \!\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\)-plane with height from \!\(\(\*StyleBox[\"h\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) to \!\(\(\*StyleBox[\"h\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\).
PlotCylinder\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"h\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"h\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"h\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"h\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots a cylinder over the base curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) in the \!\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\)-plane with height from \!\(\(\*StyleBox[\"h\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) to \!\(\(\*StyleBox[\"h\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\).
PlotCylinder\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"3\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"Base\",\"TR\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"i\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"j\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots a cylinder over the base curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"j\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) in the \!\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"j\",\"TI\"]\)\)\)-plane whose height is determined by \!\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"k\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\), \!\(\*StyleBox[\"k\",\"TI\"]\) different from \!\(\*StyleBox[\"i\",\"TI\"]\) and \!\(\*StyleBox[\"j\",\"TI\"]\) .";

PlotDerivative::usage = 
"PlotDerivative\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots \!\(\*StyleBox[\"f\",\"TI\"]\) together with its first \!\(\*StyleBox[\"n\",\"TI\"]\) derivatives; default value is \!\(\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\).  
PlotDerivative\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"n\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"n\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots \!\(\*StyleBox[\"f\",\"TI\"]\) together with its derivatives of order \!\(\(\(\*StyleBox[\"n\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"n\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TR\"]\)\). ";

PiecewiseD::usage = 
"PiecewiseD[fun, x] returns the derivative of a piecewise function returning the value Null for points or regions where the function is not defined. 
PiecewiseD[fun, {x,n}] computes the nth derivative. ";

PlotDirectionalDerivative::usage = 
"PlotDirectionalDerivative\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"p\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"p\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\), DirectionVector\!\(\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"v\",\"TI\"]\)\&\(\*StyleBox[\"\[RightVector]\",\"TR\"]\)\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"v\",\"TI\"]\)\&\(\*StyleBox[\"\[RightVector]\",\"TR\"]\)\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)] returns a plot of \!\(\*StyleBox[\"f\",\"TI\"]\) along with its directional derivatives at each of the points \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"p\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"p\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) in the directions of the vectors \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"v\",\"TI\"]\)\&\(\*StyleBox[\"\[RightVector]\",\"TR\"]\)\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"v\",\"TI\"]\)\&\(\*StyleBox[\"\[RightVector]\",\"TR\"]\)\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). The directions may also be specified with the option DirectionAngle\!\(\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\). UseLimit \!\(\*StyleBox[\"->\",\"TR\"]\) True will use the limit definition to calculate the directional derivatives. ";

PlotDSolve::usage =
"PlotDSolve\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"'\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"InitialValue\",\"TR\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the solutions to the initial value problems \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\(\*StyleBox[\"y\",\"TI\"]\)\^\(\*StyleBox[\",\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\(\*StyleBox[\"y\",\"TI\"]\)\^\(\*StyleBox[\",\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\).  With Orbit -> True, the orbit \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"'\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) of the solution is plotted. TimeState -> True plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"'\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). 
PlotDSolve\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"eqn\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"eqn\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"InitialValue\",\"TR\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) solves the correspoinding initial value problems for systems. ";

PlotEquation3D::usage = 
"PlotEquation3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"lhs\",\"TI\"]\)\(\*StyleBox[\"==\",\"TR\"]\)\(\*StyleBox[\"rhs\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) uses ContourPlot3D to plot the surface in the given region.  More than one equation can be given in a list. ";

PlotEquation::usage = 
"PlotEquation[\!\(\(\(\*StyleBox[\"lhs\",\"TI\"]\)\(\*StyleBox[\"==\",\"TR\"]\)\(\*StyleBox[\"rhs\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)] plots \!\(\(\(\*StyleBox[\"lhs\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"rhs\",\"TI\"]\)\)\(\*StyleBox[\"=\",\"TR\"]\)\(\*StyleBox[\"0\",\"TR\"]\)\) over the rectangle determined by \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) and \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\).
PlotEquation[\!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"lhs\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\(\*StyleBox[\"rhs\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"lhs\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\(\*StyleBox[\"rhs\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)] plots \!\(\(\(\(\(\*StyleBox[\"lhs\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\"-\",\"TR\"]\)\(\(\*StyleBox[\"rhs\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"=\",\"TR\"]\)\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\(\(\*StyleBox[\"lhs\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\"-\",\"TR\"]\)\(\(\*StyleBox[\"rhs\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"=\",\"TR\"]\)\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\) over the rectangle determined by \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) and \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\). ";

PlotEuler::usage = 
"PlotEuler\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"eqn\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"initialcondition\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"dt\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the Euler approximation with step size \!\(\*StyleBox[\"dt\",\"TI\"]\) to the solution \!\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) of the differential equation \!\(\*StyleBox[\"eqn\",\"TI\"]\) with the given initial condition.  If \!\(\*StyleBox[\"dt\",\"TI\"]\) is not given, then it is set to \!\(\(\(\*StyleBox[\"(\",\"TR\"]\)\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\"-\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\)\(\*StyleBox[\")\",\"TR\"]\)\)\(\*StyleBox[\"/\",\"TR\"]\)\(\*StyleBox[\"20\",\"TR\"]\)\).  
PlotEuler\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"eqns\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"initialconditions\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"dt\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the Euler approximations to the solutions \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TR\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) of the system of differential equations \!\(\*StyleBox[\"eqns\",\"TI\"]\) with the given initial conditions. ";

PlotFunction::usage = 
"PlotFunction -> False suppresses the plot of the function in PlotInverse. ";

PlotGradient::usage = 
"PlotGradient\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\) returns a contour plot of the function with gradients drawn at \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\). ";

PlotGradientField::usage = 
"PlotGradientField\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns PlotVectorField\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"\[Del]\",\"TR\"]\)\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\). ";

PlotGradientMethod::usage = 
"PlotGradientMethod\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"d\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the points on the surface corresponding to \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\), \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"+\",\"TR\"]\)\(\(\*StyleBox[\"d\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"\[Del]\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\)\)\), ..., \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"+\",\"TR\"]\)\(\(\*StyleBox[\"d\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"\[Del]\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\)\)\) and returns \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). ";

PlotGradientMethodArray::usage = 
"PlotGradientMethodArray\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"d\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the graphics array consisting of ContourPlotGradientMethod and PlotGradientMethod. ";


PlotIntegral::usage = 
"PlotIntegral\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the signed area bounded by the graph of the function together with the integral of \!\(\*StyleBox[\"f\",\"TI\"]\).  When dealing with an improper integral with a singularity at the left-hand endpoint, set the options Singularity -> Left; the default setting is Singularity -> Right. ";

PlotIntegralApprox3D::usage = 
"PlotIntegralApprox3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"ApproxType\",\"TR\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"method\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"Regular\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"m\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots \!\(\*StyleBox[\"f\",\"TI\"]\) and an approximation to the integral of \!\(\*StyleBox[\"f\",\"TI\"]\) over the rectangle determined by \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) and \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) based on the specified approximation \!\(\*StyleBox[\"method\",\"TI\"]\) (Lower, Midpoint, Upper). ";

PlotIntegralApprox::usage = 
"PlotIntegralApprox[\!\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"ApproxType\",\"TR\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"method\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"partition\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\) plots \!\(\*StyleBox[\"f\",\"TI\"]\) and an approximation to the integral of \!\(\*StyleBox[\"f\",\"TI\"]\) over the interval \!\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\"<=\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"<=\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\) based on the approximation \!\(\*StyleBox[\"method\",\"TI\"]\) and type of \!\(\*StyleBox[\"partition\",\"TI\"]\).
Possible approximation methods are: Upper, Lower, Right, Left, Midpoint, Trap, Riemann, Simpson.
Possible partitions are: \!\(\(\*StyleBox[\"Regular\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) for a partition with \!\(\*StyleBox[\"n\",\"TI\"]\) equal subintervals, \!\(\(\*StyleBox[\"MaxDeltax\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"dx\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) for a partition with mesh less than \!\(\*StyleBox[\"dx\",\"TI\"]\), or a list \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) specifying the partition points. ";
PlotDiscreteIntegral::usage = 
"PlotDiscreteIntegral[data, ApproxType -> method] plots an approximation to the area of the region determined by data. Method can be set to Right, Left, Lower, Upper, Trap, Simpson, Spline. ";

DiscreteIntegral::usage = 
"DiscreteIntegral[data, ApproxType -> method] returns the area of the region determined by the data. ";

PlotInverse::usage = 
"PlotInverse\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots \!\(\*StyleBox[\"f\",\"TI\"]\), \!\(\(\*StyleBox[\"f\",\"TI\"]\)\^\(\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"1\",\"TR\"]\)\)\), and the reflection line \!\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\).  More than one function can be given. ";

PlotJump::usage = 
"PlotJump\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Jump\",\"TR\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"Asymptote\",\"TR\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots \!\(\*StyleBox[\"f\",\"TI\"]\) between \!\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\) with special function values plotted at \!\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\) and asymptotes plotted at \!\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\). HorizontalAsymptote -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) will include horizontal asymptotes at \!\(\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\).  Several functions can be plotted together. ";

PlotLine::usage = 
"PlotLine -> False supresses the plot of the line \!\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\) in PlotInverse. ";

PlotNDSolve::usage = 
"PlotNDSolve is the same as PlotDSolve except that it uses NDSolve instead of DSolve. ";

PlotNewton::usage = 
"PlotNewton\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"initpt\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots \!\(\*StyleBox[\"f\",\"TI\"]\) together with 5 Newton iterations starting at the initial point \!\(\*StyleBox[\"initpt\",\"TI\"]\). ";

PlotNIntegral::usage = 
"PlotNIntegral is the same as PlotIntegral except that it used numerical methods to compute the integral. ";

PlotOrbits::usage = 
"PlotOrbits\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"k\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"k\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"k\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"step\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"iternumber\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"dropnumber\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the orbits of a function \!\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"k\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) when the parameter \!\(\*StyleBox[\"k\",\"TI\"]\) varies from \!\(\(\*StyleBox[\"k\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) to \!\(\(\*StyleBox[\"k\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\) in steps of \!\(\*StyleBox[\"step\",\"TI\"]\). \!\(\*StyleBox[\"Iternumber\",\"TI\"]\) specifies the number of iterations to perform for each value of the parameter \!\(\*StyleBox[\"k\",\"TI\"]\), and the last \!\(\(\*StyleBox[\"iternumber\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"dropnumber\",\"TI\"]\)\) are plotted. ";

PlotOsculatingCircle3D::usage = 
"PlotOsculatingCircle3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) together with its osculating circle at the point \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). More than one point \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\) can be given. ";

PlotOsculatingCircle::usage = 
"PlotOsculatingCircle\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) together with its osculating circle at the point \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). More than one point \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\) can be given. ";

PlotPlane::usage = 
"PlotPlane\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"eqn\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots a plane whose equation is \!\(\*StyleBox[\"eqn\",\"TI\"]\). Several equations can be given in a list. ";

PlotPointsContour::usage = 
"PlotPointsContour is an option for setting the PlotPoints for the contour plot in ContourPlotSection. ";

PlotPointsSection::usage = 
"PlotPointsSection is an option for setting PlotPoints for the section in PlotSection, ContourPlotSection and ParaPlotSection. ";


PlotProjection3D::usage = 
"PlotProjection3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"vec\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"basevectors\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the projection of \!\(\*StyleBox[\"vec\",\"TI\"]\) onto the line or plane determined by \!\(\*StyleBox[\"basevectors\",\"TI\"]\).  More than one vector can be given in a list. ";

PlotProjection::usage = 
"PlotProjection\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"vector\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"basevector\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the projection of \!\(\*StyleBox[\"vec\",\"TI\"]\) onto \!\(\*StyleBox[\"basevector\",\"TI\"]\).  More than one vector can be given in a list. ";

PlotProperty::usage = 
"PlotProperty\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"property\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns a plot of the function \!\(\*StyleBox[\"f\",\"TI\"]\) with the portions of the graph satisfying the \!\(\*StyleBox[\"property\",\"TI\"]\) highlighted.  Possible properties are: Positive, Negative, Increasing, Decreasing, ConcaveUp, ConcaveDown. ";

PlotReflection3D::usage = 
"PlotReflection3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"vec\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"basevectors\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) calls on PlotProjection3D to plot the reflection of \!\(\*StyleBox[\"vec\",\"TI\"]\) in the line or plane spanned by \!\(\*StyleBox[\"basevectors\",\"TI\"]\). ";

PlotReflection::usage = 
"PlotReflection\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"vec\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"base\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) calls on PlotProjection to plot the reflection of \!\(\*StyleBox[\"vec\",\"TI\"]\) in the line spanned by \!\(\*StyleBox[\"base\",\"TI\"]\). ";

PlotRegression3D::usage = 
"PlotRegression3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"data\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"funs\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"var\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"var\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the data and the least-squares fit to the list of data as a linear combination of the functions \!\(\*StyleBox[\"funs\",\"TI\"]\) of variables \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"var\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"var\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). The data must be of the form \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). ";

PlotRegression::usage = 
"PlotRegression\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"data\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"funs\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"var\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the data and the least-squares fit to \!\(\*StyleBox[\"data\",\"TI\"]\) as a linear combination of the functions \!\(\*StyleBox[\"funs\",\"TI\"]\) of the variable \!\(\*StyleBox[\"var\",\"TI\"]\). The data must be of the form \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). ";

PlotRoots::usage = 
"PlotRoots\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"eqn\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the solutions of \!\(\*StyleBox[\"eqn\",\"TI\"]\) in the variable \!\(\*StyleBox[\"x\",\"TI\"]\). ";

(*
PlotRungeKutta5::usage = 
"PlotRungeKutta5 is exactly like PlotEuler except that it uses the Runge Kutta Fehlberg 5th order method instead of Euler's method. ";
*)
PlotRungeKutta::usage = 
"PlotRungeKutta is exactly like PlotEuler except that it uses the Runge Kutta method instead of Euler's method. ";

PlotSection::usage =
"PlotSection\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the sections of the surface specified by the options XSection, YSection, ZSection, VSection, and OSection. 
PlotSection\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the sections of the surface determined by the cylinders described parametrically by \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) together with other sections specified with the options XSection, YSection, ZSection, VSection, and OSection. ";


PlotSlopeField::usage = 
"PlotSlopeField\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"'\",\"TR\"]\)\)\(\*StyleBox[\"==\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the slope field for the differential equation. ";

PlotSurfaceField::usage = 
"PlotSurfaceField\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the the surface \!\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\"==\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\) and the vector field \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\" \",\"TI\"]\)\) as a function of \!\(\*StyleBox[\"x\",\"TI\"]\), \!\(\*StyleBox[\"y\",\"TI\"]\), and \!\(\*StyleBox[\"z\",\"TI\"]\). ";

PlotTangentLine::usage = 
"PlotTangentLine\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots \!\(\*StyleBox[\"f\",\"TI\"]\) and its tangent lines at points with \!\(\*StyleBox[\"x\",\"TI\"]\)-coordinates \!\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\). More than one function may be given in a list. ";

PlotTangentPlane::usage = 
"PlotTangentPlane\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots a surface \!\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\) together with its tangent plane at the point \!\(\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\).  Functions with special values should be given as Piecewise functions. More than one point and/or function can be given. 
PlotTangentPlane\!\(\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots a surface \!\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\) together with its tangent plane at the point \!\(\(\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). ";

PlotTangentVector3D::usage = 
"PlotTangentVector3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for \!\(\*StyleBox[\"t\",\"TI\"]\) between \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\) along with the tangent vectors at the points on the curve corresponding to \!\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\). ";
	
PlotTangentVector::usage = 
"PlotTangentVector\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for \!\(\*StyleBox[\"t\",\"TI\"]\) between \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\) along with the tangent vectors at the points on the curve corresponding to \!\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\). ";

PlotTaylorPoly3D::usage = 
"PlotTaylorPoly3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the function \!\(\*StyleBox[\"f\",\"TI\"]\) together with its Taylor polynomial about \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) of degree \!\(\*StyleBox[\"n\",\"TI\"]\). ";

PlotTaylorPoly::usage = 
"PlotTaylorPoly\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"n\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"n\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the function \!\(\*StyleBox[\"f\",\"TI\"]\) together with its Taylor polynomials about \!\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\) of degrees \!\(\(\(\*StyleBox[\"n\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"n\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\). ";

PlotTN3D::usage = 
"PlotTN3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for \!\(\*StyleBox[\"t\",\"TI\"]\) between \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\) along with its principal tangent and principal normal at the points on the curve corresponding to \!\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\). ";

PlotTN::usage = 
"PlotTN\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for \!\(\*StyleBox[\"t\",\"TI\"]\) between \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\) along with its principal tangent and principal normal at the points on the curve corresponding to \!\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\). ";

PlotTNB::usage = 
"PlotTNB\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for \!\(\*StyleBox[\"t\",\"TI\"]\) between \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\) along with its principal tangent, principal normal, and binormal at the points on the curve corresponding to \!\(\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\). ";

PlotTV::usage = 
"Main body of code for PlotTangentVector and related commands. ";

PlotTube::usage = 
"PlotTube\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots a tube along the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for \!\(\*StyleBox[\"t\",\"TI\"]\) between \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\) whose radius at each value of \!\(\*StyleBox[\"t\",\"TI\"]\) is \!\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\). ";

PlotVector3D::usage = 
"PlotVector3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plot the vector from \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) to \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). 
PlotVector3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"w\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"w\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the vectors from \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) to \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"w\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\), \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) to \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"w\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\).  Setting the option BoundVector to True plots the vector based at \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) in the direction \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"w\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). 
PlotVector3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"BoundVector\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"True\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the vectors based at the origin in the directions \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). ";

PlotVector::usage = 
"PlotVector\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plot the vector from \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) to \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). 
PlotVector\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the vectors from \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) to \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\), \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) to \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\).  Setting the option BoundVector to True plots the vector based at \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) in the direction \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"u\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"v\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). 
PlotVector\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"BoundVector\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"True\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the vectors based at the origin in the directions \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"i\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). ";

PlotVectorField3D::usage = 
"PlotVectorField3D\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the vector field \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for the given range of \!\(\*StyleBox[\"x\",\"TI\"]\), \!\(\*StyleBox[\"y\",\"TI\"]\) and \!\(\*StyleBox[\"z\",\"TI\"]\) values using \!\(\(\(\*StyleBox[\"Mathematica\",\"TI\"]\)\(\*StyleBox[\"'\",\"TR\"]\)\)\(\*StyleBox[\"s\",\"TI\"]\)\) VectorPlot3D. ";

PlotVectorField::usage = 
"PlotVectorField\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the vector field \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) for the given range of \!\(\*StyleBox[\"x\",\"TI\"]\) and \!\(\*StyleBox[\"y\",\"TI\"]\) values using \!\(\(\(\*StyleBox[\"Mathematica\",\"TI\"]\)\(\*StyleBox[\"'\",\"TR\"]\)\)\(\*StyleBox[\"s\",\"TI\"]\)\) VectorPlot. ";


PlotWeb::usage = 
"PlotWeb\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns a web plot of the \!\(\*StyleBox[\"n\",\"TI\"]\) fixed point iterations of \!\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\) starting at \!\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\). 
PlotWeb\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"k\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) suppresses the first \!\(\*StyleBox[\"k\",\"TI\"]\) iterations in the plot. ";

Points::usage = 
"Points -> False suppresses the plotting of points in PlotJump and related commands. ";

PointStyle::usage = 
"PointStyle -> style applies style to points in PlotJump and related commands. ";


PolarPlotArea::usage = 
"PolarPlotArea\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the area swept out by the graph of the polar equation\!\(\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\) for \!\(\*StyleBox[\"\[Theta]\",\"TI\"]\) between \!\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\). If two sets of parametric equations are given, the area between the two curves is shaded. Joined -> False will prevent the regions from being joined. Regions that overlap are not shaded.  Between -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) will shade the area swept out for \!\(\*StyleBox[\"\[Theta]\",\"TI\"]\) between the polar rays \!\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\) and \!\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\), \!\(\(\*StyleBox[\"a\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\) and \!\(\(\(\*StyleBox[\"b\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\). ";

PolarPlotTangentVector::usage = 
"PolarPlotTangentVector\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots PlotTangentVector\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Cos\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Sin\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"  \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\). ";

PolarRay::usage = 
"\!\(\(\*StyleBox[\"PolarRay\",\"TR\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\) draws rays to the curve(s) at angle(s) \!\(\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\) in DirPolarPlot and NewPolarPlot. ";



PolarRayStyle::usage = 
"PolarRayStyle is an option to apply a style to the polar rays in NewPolalPlot and DirPolarPlot. ";

PolarTangentLine::usage = 
"PolarTangentLine\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the cartesian equation of the tangent line to the polar curve \!\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\) at the point \!\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\) as a function of \!\(\*StyleBox[\"x\",\"TI\"]\). ";

PolyStyle::usage = 
"PolyStyle is an option used to apply styles to the plot of polynomials in various commands. ";

Potential::usage =
"Potential[{f1, f2}, {x, y}] returns the potential function f[x,y] for the vector field {f1[x,y], f2[x,y]}. Potential[{{f1, f2, f3}, {x, y, z}] returns the potential function f[x,y,z] for the vector field {f1[x,y,z], f2[x,y,z], f3[x,y,z]}. ";

PrincipalNormal::usage = 
"PrincipalNormal\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the principal normal to the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\).
PrincipalNormal\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the principal normal to the curve at \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\).
PrincipalNormal\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the principal normal to the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\).
PrincipalNormal\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the principal normal to the curve at \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). ";

PrincipalTangent::usage = 
"PrincipalTangent\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"fun\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns parametric equations of the unit tangent vector to the curve \!\(\*StyleBox[\"fun\",\"TI\"]\) defined parametrically by \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). ";

PrintDisplay::usage = 
"PrintDisplay is an option for various commands that determines whether auxilary output is printed. ";

ProjectionStyle::usage = 
"ProjectionStyle -> style applies style to the projection in PlotProjection and PlotProjection3D. ";

RegionStyle::usage = 
"RegionStyle is an option for SurfaceOfRevolution that specifies a style to be applied to the region to be revolved.";

AxisLength::usage = 
"AxisLength is an option for SurfaceOfRevolution and ParaSurfaceOfRevolution used to determine the length of the axis of revolution. ";

AxisStyle::usage = 
"AxisStyle is an option to specify the style for the axis of revolution in SurfaceOfRevolution and ParaSurfaceOfRevolution. ";

Regular::usage = 
"Regular\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns a regular partition of an interval \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) into \!\(\*StyleBox[\"n\",\"TI\"]\) equal subintervals in integral approximation commands. Regular\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"m\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns a regular partition in each dimension. ";

RSection::usage = 
"RSection -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"r\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) plots the sections determined by \!\(\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"r\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\) in CylindricalPlot. ";

RSectionStyle::usage = 
"RSectionStyle an option to add a style to the r\[Dash]sections. ";

(*
RungeKutta5::usage = 
"Same as EulerMethod except it uses the 5th order Runge-Kutta method instead of Euler's method. ";
*)

RungeKutta::usage = 
"Same as EulerMethod except it uses the 4th order Runge-Kutta method instead of Euler's method. ";

SecantMethod::usage = 
"SecantMethod\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the result of applying the secant method \!\(\*StyleBox[\"n\",\"TI\"]\) times to a continuous function \!\(\*StyleBox[\"f\",\"TI\"]\) with \!\(\(\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"<\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"0\",\"TR\"]\)\). 
SecantMethod\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"==\",\"TR\"]\)\(\*StyleBox[\"g\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns SecantMethod\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"-\",\"TR\"]\)\(\*StyleBox[\"g\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\). ";

SectionStyle::usage = 
"SectionStyle -> style applies style to sections. ";

SectionVariables::usage = 
"SectionVariables -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"var\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"var\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) is an option for ParaPlotSection which specifies the independent variables used in the equations for VSection and OSection. ";

SelectReal::usage =
"SelectReal\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"rules\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"vars\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) selects from the list of lists of rules those sublists whose rules involve only real numbers.
SelectReal\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"points\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) selects those points each of whose coordinates are real. ";

ShowDifference::usage = 
"ShowDifference -> True draws the vertical line segment from the point to the curve or surface in regression commands. ";
 
SimpsonError::usage = 
"SimpsonError\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the error estimate for the Simpson approximation of the function \!\(\*StyleBox[\"f\",\"TI\"]\) with continuous fourth derivative on the interval \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) based on a regular partition of the interval into n (even) subintervals. ";

SphericalPlot::usage = 
"SphericalPlot\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"\[Rho]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\(\*StyleBox[\"\[Rho]\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Sin\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Cos\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"\[Rho]\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Sin\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Sin\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\(\*StyleBox[\"\[Rho]\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Cos\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\), where \!\(\*StyleBox[\"\[Phi]\",\"TI\"]\) is the angle from the positive \!\(\*StyleBox[\"z\",\"TI\"]\)-axis and \!\(\*StyleBox[\"\[Theta]\",\"TI\"]\) is  the angle from the positive \!\(\*StyleBox[\"x\",\"TI\"]\)-axis.  IF \!\(\*StyleBox[\"\[Rho]\",\"TI\"]\) IS INDEPENDENT OF \!\(\*StyleBox[\"\[Phi]\",\"TI\"]\) AND \!\(\*StyleBox[\"\[Theta]\",\"TI\"]\), THEN THE \!\(\*StyleBox[\"\[Phi]\",\"TI\"]\) RANGE MUST BE GIVEN FIRST, FOLLOWED BY THE \!\(\*StyleBox[\"\[Theta]\",\"TI\"]\) RANGE. Several functions can be given in a list. 
SphericalPlot\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"\[Rho]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the curve \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"\[Rho]\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Sin\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Cos\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Rho]\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Sin\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Sin\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Rho]\",\"TI\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"Cos\",\"TR\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"\[Phi]\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) where \!\(\*StyleBox[\"\[Rho]\",\"TI\"]\), \!\(\*StyleBox[\"\[Phi]\",\"TI\"]\), and \!\(\*StyleBox[\"\[Theta]\",\"TI\"]\) are all functions of \!\(\*StyleBox[\"t\",\"TI\"]\). ";

Surface::usage = 
"Surface -> False suppresses the plot of the surface in PlotSection, ParaPlotSection and PlotContourSurface. ";

SurfaceOfRevolution::usage = 
"SurfaceOfRevolution\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"AxisOfRevolution\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"w\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) plots the the curve \!\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\), the surface obtained by revolving its graph about the line through the origin in the direction \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"w\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). The default is AxisOfRevolution -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"1\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"0\",\"TR\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). Two curves can be given. When two curves are given, the region is shaded (set with the option RegionStyle) and the 3D region is closed. FaceStyle controls the style of the faces. LinePoint -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"c\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) will revolve the graph about the line through \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"c\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) in the direction \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"u\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"v\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"w\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\).  
SurfaceOfRevolution\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"AxisOfRevolution\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"->\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"a\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"b\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"c\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) will plot the portion of the surface generated when the curve is rotated between \!\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\) and \!\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\). Orientation -> Textbook will plot the surface with the \!\(\*StyleBox[\"y\",\"TI\"]\)- and \!\(\*StyleBox[\"z\",\"TI\"]\)-axes interchanged. ";

CurveStyle::usage = 
"CurveStyle is an option to set the style of the curve in CylindricalPlot, PlotBand, SphericalPlot, SurfaceOfRevolution, ParaSurfaceOfRevolution. ";

TableDerivatives::usage = 
"TableDerivatives\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) gives a table of the derivatives of the function \!\(\*StyleBox[\"f\",\"TI\"]\) of the variables \!\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\) of order \!\(\*StyleBox[\"j\",\"TI\"]\), \!\(\(\*StyleBox[\"0\",\"TR\"]\)\(\*StyleBox[\"<=\",\"TR\"]\)\(\*StyleBox[\"j\",\"TI\"]\)\(\*StyleBox[\"<=\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\).  TableDerivatives\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the derivative of order \!\(\*StyleBox[\"n\",\"TI\"]\). ";

TangentLine::usage = 
"TangentLine\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the cartesian equation of the tangent line to the graph of \!\(\*StyleBox[\"f\",\"TI\"]\) at the point \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\). Several functions and several points can be given in lists. ";


TangentPlane::usage = 
"TangentPlane\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the cartesian equation of the tangent plane to the surface \!\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\) at the point \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\).  Functions with special values should be given as Piecewise functions. Several points can be given.  UseLimit -> True will use the limit definition of the partials to compute the gradient. ";


TangentPlaneStyle::usage = 
"TangentPlaneStyle -> style applies style to the tangent plane(s). ";

DrawTangentLine::usage = 
"DrawTangentLine -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) or DrawTangentLine -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) is an option for PlotEquation and ImplicitPlot used to specify where tangent lines should be drawn. ";

TangentLineStyle::usage = 
"TangentLineStyle is used to apply a style for tangent lines in various commands. ";

TangentVectorStyle::usage = 
"TangentVectorStyle is used to apply a style for tangent vectors in various commands. ";
   
TaylorPoly::usage = 
"TaylorPoly\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the series expansion of \!\(\*StyleBox[\"f\",\"TI\"]\) about \!\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\) of degree \!\(\*StyleBox[\"n\",\"TI\"]\).  
TaylorPoly\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the series expansion of \!\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TR\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) about \!\(\(\*StyleBox[\"(\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\")\",\"TR\"]\)\) of degree \!\(\*StyleBox[\"n\",\"TI\"]\). UseLimit -> True uses limits to evaluate the function and its derivatives at the center point. UseSeries->False avoids using Mathematica's Series command to compute the series. ";

Textbook::usage = 
"An option for SurfaceOfRevolution and ParaSurfaceOfRevolution that interchanges the y and z axes. ";

ThetaSection::usage = 
"ThetaSection -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) plots the sections determined by \!\(\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"\[Theta]\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\) in CylindricalPlot and SphericalPlot. ";

ThetaSectionStyle::usage = 
"ThetaSectionStyle is an option to add a style to the \[Theta]\[Dash]sections. ";

TimeState::usage = 
"TimeState is an option for PlotDSolve, PlotNDSolve, PlotEuler, PlotRungeKutta. TimeState -> True will result in a timestate plot. ";
 
TN::usage = 
"TN\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"curve\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the list {principal tangent, principal normal}. 
TN\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"curve\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the list {principal tangent, principal normal} when \!\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\). Warning. With Simplify -> True (the default), PowerExpand is applied to the exression. ";


TNB::usage = 
"TNB\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the list {principal tangent, principal normal, binormal}.  
TNB\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the list {principal tangent, principal normal, binormal} when \!\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\). Warning. With Simplify -> True (the default), PowerExpand is applied to the exression. ";

TNComponents::usage = 
"TNComponents\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"curve\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the tangential and normal scalar components of the acceleration vector of the curve.  
TNComponents\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"curve\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the tangential and normal components of the acceleration when \!\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\). ";

Torsion::usage = 
"Torsion\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\" \",\"TI\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the torsion of the curve.  
Torsion\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"y\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"z\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the torsion when \!\(\(\*StyleBox[\"t\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"t\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\). ";

TrapError::usage = 
"TrapError\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the error estimate for the trapezoidal sum approximation of the function \!\(\*StyleBox[\"f\",\"TI\"]\) with continuous second derivative based on a partition of the interval \!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"min\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"max\",\"TI\"]\)\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) into n subintervals. ";


ApproxType::usage = 
"ApproxType is an option for PlotIntegralApprox, PlotIntegralApprox3D, PlotDiscreteIntegral, IntegralApprox, DiscreteIntegral that specifies the approximation method and the partition. Possible approximation methods are: Upper, Lower, Right, Left, Midpoint, Trap, Riemann, Simpson. Possible partitions are: Regular\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) for functions of one variable; Regular\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\*StyleBox[\"m\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"n\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) for functions of several variables. For functions of one variable, the partition can be MaxDeltax\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"dx\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\) for a partition with mesh less than \!\(\*StyleBox[\"dx\",\"TI\"]\), or a list \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) specifying the partition points. ";

VectorStyle::usage = 
"VectorStyle is an option used to apply a style to vectors. ";

VSection::usage = 
"VSection is an option for PlotSection, ParaPlotSection, ContourPlotSection, and Plot3DArray which is used to specify vertical sections to be plotted together with the given surface. These sections can be spacified by equations of the form \!\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\) or \!\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"g\",\"TI\"]\)\(\*StyleBox[\"[\",\"TR\"]\)\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"]\",\"TR\"]\)\)\) or by an expression in \!\(\*StyleBox[\"x\",\"TI\"]\) or \!\(\*StyleBox[\"y\",\"TI\"]\). In PlotSection, the sections are only drawn from the surface downwards.  In ParaPlotSection, the sections are drawn from the bottom to the top of the plotting region and the variables must be specified by using the option SectionVariables. ";

VSectionStyle::usage = 
"VSectionStyle -> style applies style to a vertical section. "

WronskianDet::usage = 
"WronskianDet\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the determinant of WronskianMat\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\). ";

WronskianMat::usage = 
"WronskianMat\!\(\(\*StyleBox[\"[\",\"TR\"]\)\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"x\",\"TI\"]\)\)\(\*StyleBox[\"]\",\"TR\"]\)\) returns the matrix whose rows are \!\(\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\%\(\*StyleBox[\",\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\%\(\*StyleBox[\",\",\"TI\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"f\",\"TI\"]\)\_\(\*StyleBox[\"n\",\"TI\"]\)\%\(\*StyleBox[\",\",\"TI\"]\)\)\)\(\*StyleBox[\"}\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\).";

XSection::usage = 
"XSection -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) is an option for PlotSection, ParaPlotSection, ContourPlotSection, and Plot3DArray which specifies that the planes \!\(\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"x\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\) be plotted together with the given surface.  In PlotSection, the planes are only drawn from the surface downwards. In ParaPlotSection, the sections are drawn from the bottom to the top of the plotting region. ";

XSectionStyle::usage = 
"XSectionStyle -> \!\(\*StyleBox[\"style\",\"TI\"]\) applies \!\(\*StyleBox[\"style\",\"TI\"]\) to an \!\(\*StyleBox[\"x\",\"TI\"]\)-section. ";

YSection::usage = 
"YSection -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) is an option for PlotSection, ParaPlotSection, ContourPlotSection, and Plot3DArray which specifies that the planes \!\(\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"y\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\) be plotted together with the given surface.  In PlotSection, the planes are only drawn from the surface downwards. In ParaPlotSection, the sections are drawn from the bottom to the top of the plotting region. ";

YSectionStyle::usage = 
"YSectionStyle -> \!\(\*StyleBox[\"style\",\"TI\"]\) applies \!\(\*StyleBox[\"style\",\"TI\"]\) to a \!\(\*StyleBox[\"y\",\"TI\"]\)-section. ";

ZeroToNull::usage = 
"ZeroToNull -> True changes \!\(\*StyleBox[\"Mathematica\",\"TI\"]\)'s default value of a piecewise function from Zero to Null. See also RemovePiecewise[] and RestorePiecewise[]. ";

ZSection::usage = 
"ZSection -> \!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) is an option for PlotSection, ParaPlotSection, ContourPlotSection, and Plot3DArray which specifies that the horizontal planes \!\(\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"0\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\(\*StyleBox[\"=\",\"TR\"]\)\(\(\*StyleBox[\"z\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"\[Ellipsis]\",\"TI\"]\)\) be plotted together with the given surface. ";

ZSectionStyle::usage = 
"ZSectionStyle -> \!\(\*StyleBox[\"style\",\"TI\"]\) applies \!\(\*StyleBox[\"style\",\"TI\"]\) to a \!\(\*StyleBox[\"z\",\"TI\"]\)-section. ";

LinePoints::usage = 
"LinePoints->{m,n} plots an m x n grid of line segments in PlotSlopeField.
LinePoints -> n plots an n x n grid. ";


DrawPoint::usage = 
"DrawPoint->\!\(\*StyleBox[\"pt\",\"TI\"]\) includes the point \!\(\*StyleBox[\"pt\",\"TI\"]\) in various plotting commands. DrawPoint->\!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) includes several points. ";

DrawPointStyle::usage = 
"DrawPointStyle -> style applies style to points specified by DrawPoint. ";

DrawVector::usage =
"DrawVector is an option to replace the vectors from the origin to the curve by the point on the curve in ManipulateParametricPlot. ";


DrawGradient::usage = 
"DrawGradient->\!\(\*StyleBox[\"pt\",\"TI\"]\) adds the gradient to a contour plot in several commands. DrawGradient->\!\(\(\*StyleBox[\"{\",\"TR\"]\)\(\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"1\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\(\*StyleBox[\"pt\",\"TI\"]\)\_\(\*StyleBox[\"2\",\"TR\"]\)\)\(\*StyleBox[\",\",\"TR\"]\)\(\*StyleBox[\"...\",\"TI\"]\)\)\(\*StyleBox[\"}\",\"TR\"]\)\) adds the gradient at several points. ";

GradientStyle::usage = 
"GradientStyle is an option for applying style(s) to gradients. ";

ShowJumpPoint::usage = 
"ShowJumpPoint is an option for PlotJump. When set to False, it will suppress plotting jump point(s) in PlotJump (but not the empty circle(s)). ";

ShowPoints::usage = 
"ShowPoints is an option for ContourPlotGradientMethod and PlotGradientMethodArray. When set to False it will suppress the plotting of points and automatically set the option DrawLines to True. ";

UseLimit::usage = 
"UseLimit is an option for various command specifying that limits should be used for computing derivatives, etc. ";

PlotPointsConstraint::usage=
"PlotPointsConstraint is an option for setting the number of points to plot for the constraint curve in PlotConstraint.";

Faces::usage = 
"an option for IntegrationRegion3D that determines whether to draw the faces of the region. ";

Color::uages = 
"Color is an option for PlotOrbits. ";
(*
WireFrameStyle::usage = 
"WireFrameStyle -> style applies style to the wire frame. ";
*)
(*DrawGradient::usage = 
"DrawGradient->{{a1,b1},{a2,b2},...} draws a gradient at the points {a1,b1},{a2,b2},.... ";
*)
(*GradientPointStyle::usage = 
"GradientPointStyle->{PointSize[d1],PointSize[d2],...} applies a point size to the gradient points. ";*)


(******************************************Messages********************************************)
KPGrad::nopoint = "Please specify a point at which you want the gradient drawn. ";
KPGrad::nograd = "Mathematica was unable to compute the gradient at one or more of the specified points. Try setting UseLimit -> True; if the function has special values, use Piecewise. ";
dsolve::nosoln = "DSolve was unable to find a solution. ";

PlotVectorField::ndsolve = "If NDSolve -> True, InitialValue must be set to list {x[tinit]=xinit,y[tinit]=yinit} and PlotInterval to {tmin, tmax}. ";
PlotVectorField::dsolve = "DSolve is not an option, use NDSolve. ";
InRange::notinrange = "Warning: Not all of the points `1` are in the plot range `2`.  Plotting will be done over the range `2`. ";
ClassifyCriticalPoints::notcritpt = "Warning: The gradient at the points `1` did not evaluate to the zero vector and hence cannot be classified and will be dropped. ";
ClassifyCriticalPoints::unabletoeval = "The Hessian matrix at one or more of the critical points did not evaluate to a numerical matrix and hence could not be used to classify the corresponding critical point(s). ";
ClassifyCriticalPoints::twoormore = "ClassifyCriticalPoints does not apply to functions of one variable. ";
PlotPlane::notpoly = "At least one of the input expressions is not a polynomial. ";
PlotPlane::polydegree = "At least one of the input expressions is not a polynomial of degree <= 1. ";
lusumplot::impint = "Improper Integral";
lusumplot::input = "`1` must be a single function or a list consisting of a single function";
Potential::notconservative = "The vector field is not conservative. "; 
PlotEquation::var = "Equation `1` must have one variable other than `2`. To plot an expression of one variable, use Plot or PlotJump. ";
PlotEquation::sfail = "Equation `1` could not be solved for `2`.  Use PlotEquation specifying a range for both variables. ";
PlotEquation::eqn = "Input expression `1` must be an equation and both an x and y range for plotting must be specified. ";
PlotEquation3D::eqn = "Input expression `1` must be an equation and x, y and z ranges for plotting must be specified. ";
nooption::direction = "No directions have been specified with the options DirectionAngle or DirectionVector. ";
option::direction = "`1` must be set to `2` or a list of `3`. ";
function::nodef = "The function `1` is not defined at at least one of the points `2`. ";
dderiv::noexist = "Unable to compute a finite directional derivative in some directions `1` at the point `2`.  Try setting UseLimit -> True. ";
PlotProperty::invalid = "`1` is not a valid property.  Possibilities are: Positive, Negative, Increasing, Decreasing, ConcaveUp, ConcaveDown. ";

PlotNewton::zeroderiv = "A zero derivative was encountered or Mathematica was unable to compute the derivative. ";
PlotNewton::imaginary = "Imaginary numbers encountered.  Try setting PowerBehavior -> Real. ";
PlotIntegral::noint = "Mathematica was unable to find an exact integral.  Use PlotNIntegrate. ";
DirParametricPlot::ppts = " The first element of PlotPoints -> `1` must be an integer >= 3. ";

ColorFunction::vars = "ColorFunction must be set to a function of three variables in DirParametricPlot or of four variables in DirParametricPlot3D. The default color scheme will be used. ";


dpp::incorrectinput = "`1` does not evaluate to a `2`-tuple or a list of `2`-tuples. ";
PlotProjection3D::wrongop = "Cannot set Base -> Line when projecting onto a plane. ";
SimpApprox::even = "For Simpson's Rule, set ApproxType -> {Simpson, Regular[n]} (or ApproxType -> {Simpson, Regular[m,n]}) where n (or m and n) is an even interer(s). ";
SimpError::even = "Simpson's Rule requires an even number of subintervals and `1` is an odd integer.";
SimpApprox::partition = "Simpson's Rule requires an even number of subintervals so the partition must have an odd number of points. ";
PlotSection::imp = "The list(s) `1` must be of length 2 or have 0 as one of its components. "; 
PlotSection::colorfunctionsurface = "PlotSection does not allow separate color functions for different input functions. ";
ParaPlotTangentPlane::singpt = "At least one of the points is a singular point of the parameterization. ";
WrongSize::size = "PlaneSize cannot be set to 0. ";
PlotEuler::initialval = "The initial condition is specified at `1` = `2` which is outside the plot interval `3`. ";
PlotEuler::initialcond = "Initial conditions were specified for the following values of `1`: `2`.  Initial conditions must be specified for the same value of `1`. ";
InitialValue::noiv = "You must specify one or more initial values with the option InitialValue. ";
DSolve::nosoln = "DSolve failed to find a solution.  Try using NDSolve. ";
ImplicitPlot::ptfail = "The equation could not be solved.  Try setting DrawTangentLine to a list {{x1,y1},...} of the approximate xy-coordinates of the points. "; 
ImplicitPlot::pr = "PlotPoints must be set to Automatic or a list of two lists. ";
section::notnumber = "The options XSection, YSection, ZSection must be set to numbers or a list of numbers. ";
section::wrongvars = "The variables used in VSection and OSection must be those specified with the option SectionVariables. ";
section::vsect = "VSection must be set to a list of expressions or equations.  Use XSection, YSection, or ZSection to specify sections determined by constants. ";
section::extravars = "Only one variable can be used in specifing the functions given by VSection in PlotSection. ";
section::outrange = "Warning: At least one section is not within the plotrange of the surface. ";
section::linear = "At least one of `1` specified in OSection is not a polynomial in `2`. ";
section::wrongnum = "In ParaPlotSection, the variables used when setting the option VSection or OSection to a list of one or more equations must be listed with the option SectionVariables. Two variables must be specified. ";
section::equation = "In ParaPlotSection, VSection must be a list of equations. ";
PlotSlopeField::nonullcline = "Mathematica was unable to find the nullcline(s).  You can manually specify the nullclines by setting Nullcline -> fun. ";

PlotRange::cprng = "Value of PlotRange -> `1` is not {zmin,zmax}, All, or Automatic. ";
PlotEuler::degree = "This method only applies to first order differential equations. ";
Limit::noderiv = "Mathematica was unable to find the derivative at `1` = `2`. ";
Limit::noderiv1 = "Mathematica was unable to find the derivative at one or more of the points. ";
VectorSize::wrongsize = "All vectors must be of length `1`. ";
PlotSlopeField::order = "The order of the differential equation `1` is not 1. ";
PlotDSolve::nosoln = "DSolve could not find a solution for the initial condition(s) `1`. ";
dsolve::adjustplotrange = "Mathematica had trouble finding an appropriate plot range.  Set the plot range with PlotRange -> {ymin,ymax}. ";
Orbit::orderode = "Cannot produce a plot of the orbit for an ODE or system of order `1`. ";
TimeState::orderode = "Cannot produce a plot of the timestate curve for an ODE or system of order `1`. ";
DSolve::nvld = "Improper system of differential equations. ";
BisectionMethod::oppositesign = "`1` and `2` do not have opposite signs. ";
CubicSpline::firstandsecondderivs =  "You may specify the first derivatives or the second derivatives at the endpoint, but not both. ";
DistancePointPlane::nothplane = "The given parametric equations do not define a hyperplane. ";
Curvature::curvaturezero = "The curvature is zero when `1` =  `2`. ";
NB::normalbinormal = "The normal and binormal do not exist when `1` = `2`. ";
TaylorPoly::notdefined = "The function is not defined at the center point `1`. Try setting the option UseLimit -> True to see if the function has a removable discontinuity at `1`."
TaylorPoly::subscript = "Subscripted variables are not allowed in this command";
vsect::badinput = "VSection must be specified by equations of the form y == f(x) or x == g(y) or by expressions involving only x or y. The bad input will be deleted and plotting will continue with what remains. ";
ParaPlotTangentPlane::NoTanPlane = "Mathematica was unable to find the tangent plane at the specified point(s). ";
DirParametricPlot3D::badinput = "Bad input. The function must be a list of three components or a list of such lists. ";
DirParametricPlot::badinput = "Bad input. The function must be a list of two components or a list of such lists. ";
PlotJump::ptsize = "PlotJump requires that the size of points be specified with AbsolutePointSize. PointSize will be set to the default AbsolutePointSize[7]. The minimum possible setting is AbsolutePointSize[3]. The symbolic forms Tiny, Small, Medium, Large may be used. ";
DrawPoint::dpt2 = "DrawPoint must be set to a 2-tuple, a list of 2-tuples, or a list of numbers. ";
Graphics::ptsize = "In AbsolutePointSize[size], size must be an integer greater than 2 or one of the symbolic forms: Tiny, Small, Medium, Large. ";

plotdsolve::orbitST = "Both Orbit and TimeState were set to True. TimeState will be set to false. ";
dpp::noderiv = "Mathematica was unable to find the derivative which is used to produce the arrowheads. Plotting will proceed without arrowheads. ";
dpp::improperinput = "The input function is not a vector or a list of vectors. ";
dpp::pscolor = "PlotStyle cannot be used to specify color in DirParametricPlot or DirParametricPlot3D. Use the option ColorFunction instead. ";
dpp::notallowed = "Tooltips are not allowed in this command. ";
PlotNDSolve::noprint = "PrintDisplay is not an option for PlotNDSolve. Setting DrawGraph -> False will return the interpolating function. ";
PlotDSolve::wrongiv = "Initial values were not specified correctly. See the usage message. ";
ShowPlots::repeat = "ShowPlots must contain a list of distinct variables. ";
Command::notooltip = "This command does not support tooltips --- Plotting will proceed without tooltips. ";

Command::onefunction= "This command does not support a list of functions as input. ";
TangentPlane::singpt = "The point `1` on the surface corresponding to `2` is a possible singular point. Try setting UseLimit -> True. ";

Limit::piecewise="Limit cannot handle vector-valued Piecewise functions or Piecewise functions of two or more variables. ";		
PD::one = "PlotDerivative can plot only one function. ";
PD::Abs = "Your input `1` involves an absolute value which Mathematica cannot differentiate correctly. Try writing your function as a piecewise function. ";
PD::noderiv = "Mathematica was unable to find the derivative. If your expression expr involves an absolute value, yry rewriting your function as a piecewise function. ";
PlotEuler::eqnsvars = "The number of equations is not equal to the number of variables. ";
PlotEuler::badvar = "One of the dependent variables does not appear in the list of equations. ";
PlotEuler::order = "PlotEuler requires that the left endpoint be less than the right endpoint. ";
EulerMethod::dmval = "Some values extrapolated.  Ensure that all inital conditions occur at the same endpoint. ";
PlotDSolve::plotinterval = "Mathematica was unable to solve over the entire plot interval. Recommend plotting over a smaller interval. ";
sectstyle::tube = "All occurances of Thickness[r] in section styles are replaced by Tube[2 r] except in certain cases with ZSectionStyle and OSectionStyle when the style is set to Thickness[.01] and cannot be changed. ";
NewLimit::nolimit = "It appears that the limit does not exist. The right hand limit is `1`. The left hand limit is `2`. ";

pdd::nodd = "The function does not have a directional derivative at the given point(s) in the given direction(s). ";
syntax::changed = "The syntax of this command has changed. See its usage message. ";
PlotJump::realpower = "Instead of using RealPower, set PowerBehavior -> Real. ";
GradientMethod::dir = "Set Direction to 1 when seeking a maximum, and -1 when seeking a minimum. ";

surfofrevol::dui = "Orientation -> Textbook only applies when the surface is revolved about the y-axis. The option has been removed and plotting will continue. ";
surfofrevol::notallowed1 = "The options DrawRegion -> True and DrawVolumn -> True are not allowed with three or more functions. Plotting will continue with these options set to False. "; 
surfofrevol::notallowed = "Revolving functions y = f(x) about the z-axis is not allowed. ";

PIA::regular = "With Regular[n], n must be a positive integer. With Regular[m,n], both m and n must be positive integers. ";
PIA::points = "The partition is missing one or both of the endpoints of the interval or has one or more repeated points. The missing endpoints will be added to the partition, and the new partition is `1`. ";
PIA::partition = "Possible settings for the partition are Regular[n], n a positive integer, MaxDeltax[n], n a number, or a list of points determining a partition of the interval. ";
PIA::notordered = "The points in the partition were not ordered or the partition contained repeated points. The ordered partition is `1`. ";
IA2::type = "For functions of more than one variable, IntegralApprox only supports approximations of type Midpoint, Trap, and Simpson. ";
IA::partition = "IntegralApprox does not support partitions specified by MaxDeltax. ";
PIA3D::notallowed = "PlotIntegralApprox3D only accepts regular partitions of the form Regular[m,n]. ";
PIA::type = "Specity the type of approximation by setting the option ApproxType -> {type, partition}. Possible values for type are: Upper, Lower, Right, Left, Midpoint, Trap, Riemann, Simpson. Specity the partition with the option Partition. ";
PIA3D::type = "Specity the type of approximation by setting the option ApproxType -> {type, partition}. Possible values for type are: Upper, Lower, Midpoint; partition must be set to Regular[m,n]. ";
IA::type = "Specity the type of approximation by setting the option ApproxType -> {type, partition}. Possible values for type are: Upper, Lower, Right, Left, Midpoint, Trap, Simpson. Specity the partition with the option Partition. ";
PIA::notpartition = "One or more of the points in the partition are not between `1` and `2`. These points have been excluded from the partition. ";
PIA::specifytype = "The option ApproxType (formerly called Type) must be specified. Set ApproxType -> {ApproximationType, PartitionType}. ";
PIA::emptypartition = "A partition must be a non-empty list of points in the interval. ";

PI::onefunction = "PlotIntegral only allows one function as input. ";
PolarPlot::ray = "PolarRay must be set to a number or a list of numbers. ";
PE3::pts = "DrawPoint must be set to a list of 2-tuples or a list of 3-tuples. ";
PE3::pt = "The option Point has been changed to DrawPoint. ";
PE::pts = "DrawPoint must be set to a number, a list of numbers, or list of 2-tuples. ";
PE3::nopoint = "One or more of the given points do not correspond to points on the graph. ";
Newton::zeros = "Command finds the zeros of an expression. ";
PTP::notp = "At least one of the requested Taylor polynomials `1` does not exist. ";
point::drawpt = "DrawPoint should be set to a point or a list of points in the plane. Plotting will continue without drawing points. ";
point::drawpt3D = "DrawPoint should be set to a point or a list of points in space. Plotting will continue without drawing points. ";
cylinderrange::list = "CylinderRange must be set to Automatic or a list of two numeric quantities. CylinderRange will be set to Automatic and plotting will continue.";
PlotIntegral::twosingularities = "The integrand is singular at both endpoints. Suggest decomposing into two integrals.";
obsolete::RevolutionAxis = "Obsolete. The option RevolutionAxis has been renamed AxisOfRevolution. ";
RealPower::slow = "Plotting commands using RealPower are very slow. Set PowerBehavior->Real instead. ";
RealPower::notallowed = "This command does not allow RealPower. Set PowerBehavior->Real instead. ";
sor::inputerror = "Improper input. First argument should be a function or a list of functions. "; 
PT::detailed = "Setting PlotTheme to Detailed is not allowed. PlotTheme has been reset to Classic. Use options like GridLines and PlotLegends to reproduce the behavior of Detailed.";
color::cstyle = "CylinderStyle does not accept a color directive. The cylinder inherites its color from SectionStyle.";
notopt::sstyle = "SectionStyle does not accept an opacity or thickness directive. Thickness is replaced by Tube. ";
opacity::xyzvosection = "XSectionStyle, YSectionStyle, ZSectionStyle, VSectionStyle, and OSectionStyle do not accept an opacity directive. Opacity for these and other sections is set with the option CylinderStyle. ";
PlotSection::pw="Parametrically defined curves defining sections must be two-vectors. No parametrically defined sections will be plotted. ";

plotconstr::nograd = "Mathematica was unable to find the gradient at one or more of the specified points; plotting will continue using the remaining points.";

plotderiv::piecewise = "Derivatives of piecewise functions might not be computed correctly at break points. ";
plotderiv::piecewiseplotd = "Use PlotPiecewiseD when the function is a piecewise function." ;

PPTL::vector = "Input must be a 2-vector. Might want to use PlotTangentVector. ";
PTV::length2 = "Input must be a 2-vector. Might want to use PlotTangentVector3D. ";
PDI::midpt = "When Type->Midpoint in PlotDiscreteIntegral, the length of the data must be odd. ";
PDI::type = "In PlotDiscreteIntegral or DiscreteIntegral, set ApproxTyle->method where method is Right, Left, Lower, Upper, Trap, Simpson, Spline. ";
plottn::numeric = "The third argument must be a numeric quanity or a list of numeric quantities. ";
PV::wrongsize = "The list of vectors and the list of initial points must have the same length.";
(****************************************************************************)



(********************************************************************)

Options[DirectionalDerivative] = {UseLimit -> False};
Options[KPGrad] = {UseLimit -> False};
Options[KPLaplacian] = {UseLimit -> False};
Options[KPDiv] = {UseLimit -> False};
Options[JacobianMatrix] = {UseLimit -> False};
Options[JacobianDet] = {UseLimit -> False};
Options[HessianMatrix] = {UseLimit -> False};

Options[ImplicitD] = {Constants -> None};

Options[PlotEquation] = Options[ImplicitPlot] = 
{DrawPoint->{}, DrawPointStyle->{{RGBColor[1, 0, 0], PointSize[Large]}}, DrawTangentLine->{}, LineLength->1, 
	PlotStyle->{{AbsoluteThickness[1.6], RGBColor[
  0.368417, 0.506779, 0.709798]}, {AbsoluteThickness[1.6], RGBColor[
  0.880722, 0.611041, 0.142051]}, {AbsoluteThickness[1.6], RGBColor[
  0.560181, 0.691569, 0.194885]}, {AbsoluteThickness[1.6], RGBColor[
  0.922526, 0.385626, 0.209179]}, {AbsoluteThickness[1.6], RGBColor[
  0.528488, 0.470624, 0.701351]}, {AbsoluteThickness[1.6], RGBColor[
  0.772079, 0.431554, 0.102387]}, {AbsoluteThickness[1.6], RGBColor[
  0.363898, 0.618501, 0.782349]}, {AbsoluteThickness[1.6], RGBColor[
  1, 0.75, 0]}, {AbsoluteThickness[1.6], RGBColor[
  0.647624, 0.37816, 0.614037]}, {AbsoluteThickness[1.6], RGBColor[
  0.571589, 0.586483, 0.]}, {AbsoluteThickness[1.6], RGBColor[
  0.915, 0.3325, 0.2125]}, {AbsoluteThickness[1.6], RGBColor[
  0.40082222609352647`, 0.5220066643438841, 
   0.85]}, {AbsoluteThickness[1.6], RGBColor[
  0.9728288904374106, 0.621644452187053, 
   0.07336199581899142]}, {AbsoluteThickness[1.6], RGBColor[
  0.736782672705901, 0.358, 0.5030266573755369]}, {AbsoluteThickness[
   1.6], RGBColor[0.28026441037696703`, 0.715, 0.4292089322474965]}},  
	PrintDisplay->False, TangentLineStyle->Automatic, TangentPointStyle->PointSize[Medium], UseLimit->False};

Options[PlotEquation3D] = {DrawPoint-> {}, DrawPointStyle->{{RGBColor[1, 0, 0], PointSize[Large]}}, PlotStyle -> Automatic};

Options[PlotDSolve] = {DrawGraph->True,DrawPoint->{},DrawPointStyle->AbsolutePointSize[Large],InitialValue->{},Orbit->False,PrintDisplay->False,ShowPlots->All,TimeState->False};

Options[PlotNDSolve] = {DrawGraph->True,InitialValue->{},Orbit->False,ShowPlots->All,TimeState->False};

Options[PlotProperty] = {(*PlotStyle -> {},*) PropertyStyle -> {{Red, AbsoluteThickness[2.5]}}};
	
Options[PlotWeb] = {ArrowSize->1, PointStyle->{{PointSize[0.03],RGBColor[1,0,0]}}, PrintDisplay->True};

Options[PlotOrbits] = {Color -> {Hue, 0, .8}, PointStyle -> {}};

Options[PlotJump] = {Asymptote->None,AsymptoteStyle->Dashing[Medium],DrawPoint->{}, DrawPointStyle->AbsolutePointSize[7],HorizontalAsymptote->None,HorizontalAsymptoteStyle->Dashing[Medium],Jump->None,PointStyle->{AbsolutePointSize[7]},ShowJumpPoint->True,ZeroToNull->True};
	
Options[PlotInverse] = {InverseStyle->RGBColor[1,0,0],LineStyle->{{GrayLevel[0.5],Dashing[Tiny]}},PlotFunction->True,PlotLine->True};
						
Options[PlotRoots] = {PointStyle -> PointSize[.027], PrintDisplay -> False};
						
Options[PlotTangentLine] = 
	{DrawPoint -> {}, DrawPointStyle->PointSize[Large], LineLength -> 1, PlotStyle -> Automatic, 
 	PrintDisplay -> False, TangentLineStyle -> Thick, TangentPointStyle -> PointSize[Large], UseLimit -> False};
 	
Options[ParaPlotTangentLine] = {DrawPoint->{},DrawPointStyle->PointSize[Large], LineLength->1,PlotStyle->Automatic,PrintDisplay->False,
	TangentLineStyle->Thick,TangentPointStyle->PointSize[Large],UseLimit->False};

Options[TangentLine] = {UseLimit -> False};

Options[ParaTangentLine] = {Normalize -> False, UseLimit -> False};
	
Options[PlotDerivative] = {DerivativeStyle -> Automatic, PrintDisplay -> False, PlotFunction -> True, ZeroToNull -> False};

Options[PiecewiseD] = {NonConstants -> {},ZeroToNull->True};

Options[PlotNewton] = {DrawGraph->True,Iterations->5,LineStyle->{},PointStyle->{PointSize[0.017]},PrintDisplay->True,WorkingPrecision->MachinePrecision};

Options[BisectionMethod] = Options[SecantMethod] = Options[NewtonMethod] = 
		{PrintDisplay -> False, WorkingPrecision -> MachinePrecision};

Options[PlotIntegral] = Options[PlotNIntegral] = {IntegralStyle -> {Thickness[.007],Hue[0.67, 0.6, 0.6]}, (*PlotStyle -> GrayLevel[0],*) PrintDisplay -> False}; 
Options[PlotNIntegral] = {IntegralStyle -> {Thickness[.007],Hue[0.67, 0.6, 0.6]}}; 

Options[ParaSurfaceOfRevolution] = 
	{AxisLength->1,AxisOfRevolution->{1,0,0},AxisStyle->{RGBColor[1,0,0],Thickness[0.01]},CurveStyle->{},DrawAxis->True,DrawRegion->True,FaceStyle->{RGBColor[1,0.666667,.3],Opacity[0.8]},LinePoint->{0,0,0},Orientation->Usual,RegionStyle->{RGBColor[.3,1,.3],Opacity[0.7]}};

Options[SurfaceOfRevolution] = 
	{AxisLength->1,AxisOfRevolution->{1,0,0},AxisStyle->{RGBColor[1,0,0],Thickness[0.01]},CurveStyle->{},DrawAxis->True,DrawRegion->True,FaceStyle->{RGBColor[1,0.666667,.3],Opacity[0.8]},LinePoint->{0,0,0},Orientation->Usual,RegionStyle->{RGBColor[.3,1,.3],Opacity[0.7]},DrawVolume->True};


Options[DirParametricPlot] = 
		{ArrowNumber->15, ArrowSize->Medium (*Large*), ColorFunction->Automatic, DrawArrowheads->True, DrawPoint->{}, DrawPointStyle -> {{Red,PointSize[Large]}}, PlotStyle->Automatic}; (*04-12-16*)

Options[DirParametricPlot3D]  = 
		{ArrowNumber->15, ArrowSize->Large, ColorFunction->Automatic, DrawArrowheads->True, DrawPoint->{}, DrawPointStyle -> {{Red,PointSize[Large]}}, PlotStyle->Automatic}; (*04-12-16*)

		
Options[DirPolarPlot] = Join[{PolarRay -> {}, PolarRayStyle -> Black},Options[DirParametricPlot]]//Sort;

Options[NewPolarPlot] ={PolarRay -> {}, PolarRayStyle -> Black};

Options[PolarPlotArea] = {ArrowSize->Medium,Between->Automatic,DrawArrowheads->False,FillingStyle->GrayLevel[0.8],Joined->True,LineStyle->Thickness[Tiny],PolarRay->{},PolarRayStyle->GrayLevel[0]};

Options[ParaPlotArea] = {ArrowSize->Medium,Between->Automatic,DrawArrowheads->False,FillingStyle->GrayLevel[0.8],Frame->False,Joined->True,LineStyle->Thickness[Tiny]};

Options[PlotVector] = {ArrowSize -> (*1*) Medium,  BoundVector -> False, DrawArrowheads->True, VectorStyle -> GrayLevel[0]};

Options[NewPlotVector]=Options[NewPlotVector3D]={ArrowSize->Medium,BoundVector->True,DrawArrowheads->True,VectorStyle->GrayLevel[0]};

Options[PlotProjection] = Options[PlotReflection] = 
	{ArrowSize->1,AspectRatio->Automatic,AxesOrigin->Automatic,BaseLength->1,BaseLineStyle->GrayLevel[0],BasePoint->{0,0},BaseVectorStyle->GrayLevel[0],DashedLine->True,DashedLineStyle->Dashing[{0.01,0.02}],DrawBaseLine->False,DrawBaseVector->False,PlotRange->All,PointStyle->PointSize[0.03],PrintDisplay->False,ProjectionStyle->RGBColor[1,0,0],VectorStyle->RGBColor[0,0,1]};
	
Options[PlotTV] = Options[PlotTangentVector] = Options[PolarPlotTangentVector] = Options[PlotTangentVector3D] = 
	{Acceleration->False,AccelerationVectorStyle->RGBColor[0,1,0],ArrowSize->Medium,Jerk->False,JerkStyle->RGBColor[0,0,1],Normalize->False,PointStyle->PointSize[Large],TangentVectorStyle->RGBColor[1,0,0],UseLimit->False,VectorStyle->{}};
	
Options[PlotTaylorPoly] = {PointStyle->AbsolutePointSize[7],PolyStyle->Automatic,PrintDisplay->False};

Options[PlotRegression] = {LineStyle->Dashing[{0.01,0.02}],PlotRange->All,PointStyle->PointSize[0.015],PrintDisplay->True,ShowDifference->False};

Options[PlotArcLengthApprox] = Options[PlotArcLengthApprox3D] = 
	{DrawGraph -> True, LineStyle -> Hue[0], PointStyle -> AbsolutePointSize[3], PrintDisplay -> True};

Options[LUSumPlot] = {Exact->False,Frame->False,DrawGraph->True,PolyStyle->GrayLevel[0],PointStyle->PointSize[Medium],FillingStyle->Directive[{Opacity[0.2], Hue[0.67, 0.6, 0.6]}],PrintDisplay->True,WorkingPrecision->MachinePrecision};

Options[PlotIntegralApprox] =
		{DrawPoint->{}, DrawPointStyle->AbsolutePointSize[Large], Exact->False, DrawGraph->True, PrintDisplay->True, PointStyle -> PointSize[Medium]};
		
Options[PlotIntegralApprox3D] =
		{CuboidStyle->Opacity[0.95],Exact->False,Frame->False,DrawGraph->True,DrawSurface->True,PlotStyle->Opacity[.7],PrintDisplay->True};
		
Options[IntegralApprox] =
		{Exact -> False, WorkingPrecision -> MachinePrecision};

Options[TrapError] = Options[MidpointError] = Options[SimpsonError] = {Exact -> False};


Options[CubicSpline] = {Exact->False, FirstDerivatives -> Automatic, SecondDerivatives -> {0,0}, WorkingPrecision->MachinePrecision};

Options[PlotCubicSpline] = Options[ParaPlotCubicSpline] = {Points -> True, PointStyle -> {PointSize[.017]}};

Options[ParaCubicSpline] = {Exact -> False, FirstDerivatives -> {{0, 0}, {0, 0}}, SecondDerivatives -> {{0, 0}, {0, 0}}, 
    WorkingPrecision -> MachinePrecision}; 

Options[ComplexPlot] = {XStyle -> Red, YStyle -> Blue}; 


(*************************************************************************)
Options[PlotPlane] = {};

Options[PlotDirectionalDerivative] = {ArrowSize->1,BaseVector->True,DirectionAngle->{},DirectionVector->{},DirectionVectorPosition->Automatic,DrawPlane->False,Mesh->None,PointStyle->PointSize[0.02],(*SectionStyle->{RGBColor[0.5,0,0.5],Thickness[0.01]},*)UseLimit->False,VectorStyle->{{Red,Thickness[0.006]}}};

Options[PlotSurfaceField] = Options[ParaPlotSurfaceField] = {ArrowSize -> Medium (*1*), PlotStyle->Opacity[.7], VectorPoints-> 6, VectorScale -> Automatic, VectorStyle -> Automatic};

Options[PlotContourSurface] = {ContourPlotPosition->Automatic,Contours->11,ContourStyle->Automatic};

Options[PlotVector3D] = {ArrowSize->1,AxesOrigin->Automatic,BoundVector->False,PlotRange->All,VectorStyle->Automatic};

Options[PlotProjection3D] = Options[PlotReflection3D] = {ArrowSize->1,BaseLength->1,BaseLineStyle->GrayLevel[0],BasePoint->{0,0,0},BaseVectorStyle->GrayLevel[0],DashedLine->True,DashedLineStyle->Dashing[{0.01,0.02}],DrawBaseLine->False,DrawBaseVectors->False,DrawPlane->True,PlaneSize->Automatic,PlaneStyle->{{Green,Opacity[.5]}},PlotRange->All,PointStyle->PointSize[0.02],PrintDisplay->False,ProjectionStyle->RGBColor[1,0,0],VectorStyle->RGBColor[0,0,1]};

Options[PlotCylinder] = Base -> {1,2};

Options[PlotSection] = 
{ColorFunctionSection->Automatic,ColorFunctionSurface->Automatic,CylinderBase->Automatic,CylinderRange->Automatic,CylinderStyle->Opacity[0.4],DrawCylinder->True,DrawPoint->{},DrawPointStyle->{{RGBColor[1,0,0],PointSize[Large]}},Mesh->Full,MeshStyle->GrayLevel[.45],OSection->{},OSectionStyle->{{RGBColor[1,0,0],Tube[.02]}},PlotPointsSection->75,PlotStyle->Opacity[0.7],SectionStyle->{{RGBColor[1,0,0],Tube[.02]}},Surface->True,VSection->{},VSectionStyle->{{RGBColor[1,0,0],Tube[.02]}},XSection->{},XSectionStyle->{{RGBColor[1,0,0],Tube[.02]}},YSection->{},YSectionStyle->{{RGBColor[1,0,0],Tube[.02]}},ZSection->{},ZSectionStyle->{{RGBColor[1,0,0],Tube[.02]}}, ZeroToNull -> True};

Options[ParaPlotSection] = 
{ColorFunctionSection->Automatic,ColorFunctionSurface->Automatic,CylinderBase->Automatic,CylinderRange->Automatic,CylinderStyle->Opacity[0.4],DrawCylinder->True,DrawPoint->{},DrawPointStyle->{{RGBColor[1,0,0],PointSize[Large]}},Mesh->Full,MeshStyle->GrayLevel[.45],OSection->{},OSectionStyle->{{RGBColor[1,0,0],Tube[.02]}},PlotPointsSection->75,PlotStyle->Opacity[0.8],SectionStyle->{RGBColor[1,0,0],Tube[.02]},SectionVariables->{},Surface->True,VSection->{},VSectionStyle->{{RGBColor[1,0,0],Tube[.02]}},XSection->{},XSectionStyle->{{RGBColor[1,0,0],Tube[.02]}},YSection->{},YSectionStyle->{{RGBColor[1,0,0],Tube[.02]}},ZSection->{},ZSectionStyle->{{RGBColor[1,0,0],Tube[.02]}}};

Options[ContourPlotSection] = 
 {DrawPoint->{},DrawPointStyle->{{RGBColor[1,0,0],PointSize[Large]}},DrawGradient->{},GradientStyle->{{GrayLevel[0],PointSize[Medium]}},OSection->{},OSectionStyle->{{RGBColor[1,0,0],Tube[.02]}},PlotPointsSection->75,SectionStyle->{RGBColor[1,0,0],Tube[.02]},VSection->{},VSectionStyle->{{RGBColor[1,0,0],Tube[.02]}},XSection->{},XSectionStyle->{{RGBColor[1,0,0],Tube[.02]}},YSection->{},YSectionStyle->{{RGBColor[1,0,0],Tube[.02]}},ZSection->{},ZSectionStyle->{{RGBColor[1,0,0],Tube[.02]}}};

Options[Plot3DArray] = Join[{ContourPlot->True,DensityPlot->False,DrawPoint->{},DrawPointStyle->{{RGBColor[1,0,0],PointSize[Large]}},DrawGradient->{},ImageSize->Automatic,Plot3D->True,PlotPointsContour->Automatic},Options[ContourPlotSection]];

Options[PlotTangentPlane] = Options[ParaPlotTangentPlane] = 
	{PlaneSize->1,PlotStyle->Opacity[0.7],PrintDisplay->False,TangentPlaneStyle->{{Red,Opacity[0.8]}},TangentPointStyle->PointSize[0.02],UseLimit->False};

Options[TangentPlane] = {UseLimit -> False};
	
Options[ParaTangentPlane] = {Normalize -> False, UseLimit -> False};

Options[PlotTaylorPoly3D] = {PointStyle -> PointSize[Large], PolyStyle -> {{RGBColor[0.368417, 0.506779, 0.709798],Opacity[.6]}}, PrintDisplay -> False};
				
Options[CylindricalPlot] = {RSection -> {}, RSectionStyle->{}, ThetaSection -> {}, ThetaSectionStyle->{}};

Options[SphericalPlot] = {Mesh -> Full, PhiSection -> {}, PhiSectionStyle -> {}, ThetaSection -> {}, ThetaSectionStyle -> {}};

Options[PlotRegression3D] = 
	{LineStyle->{},PlotRange->All,PointStyle->PointSize[0.017],ShowDifference->False, PlotStyle->Opacity[.8], PrintDisplay->True};

Options[PlotConstraint] = 
			{ConstraintStyle->{{White,Thick}},DrawPoint->{},DrawPointStyle->{{RGBColor[1,0,0],PointSize[Large]}},DrawGradient->{},GradientStyle->{{GrayLevel[0],PointSize[Medium]}},(*GradientPointStyle->PointSize[0.017],*)PlotPointsConstraint->Automatic};

Options[PlotVectorField] = Options[PlotVectorField3D] = {DrawPoint->{},DrawPointStyle->{{Red,PointSize[Medium]}},InitialValue->{},NDSolve->False,PlotInterval->{}};



Options[PlotGradientField] = Join[ Options[PlotVectorField],{ContourPlot->False, PlotPointsContour->Automatic}]//Sort;

Options[PlotSlopeField] = {AspectRatio->1,Direction->Right,DSolve->False,Euler->False,InitialValue->{},LinePoints->{15,15},LineScale->1,LineStyle->GrayLevel[0.5],NDSolve->False,Nullcline->False,NullclineStyle->Dashing[{0.02,0.02}],PlotStyle->Automatic,PointStyle->AbsolutePointSize[Medium],PrintDisplay->False,RungeKutta->False};

Options[PlotEuler] = Options[PlotRungeKutta] = {DSolve->False,Flag->Euler,DrawGraph->True,InitialValue->{},(*MeshStyle->GrayLevel[0],*)NDSolve->False,Orbit->False,PlotStyle->Automatic,PointStyle->AbsolutePointSize[3 (*Medium*)],PrintDisplay->False,ShowPlots->All,TimeState->False,WorkingPrecision->MachinePrecision};

Options[EulerMethod] = Options[RungeKutta] = {WorkingPrecision -> MachinePrecision};

Options[PlotGradient] = {ArrowSize->Medium,GradientStyle->GrayLevel[0],PointStyle->PointSize[Medium],UseLimit->False};

Options[PlotGradientMethod] = {Color->{Hue,0,0.8},Direction->1,EndSize->0.05,DrawGraph->True,PointStyle->Automatic,PrintDisplay->False,StartSize->0.007};

Options[PlotGradientMethodArray] = Join[Options[PlotGradientMethod],{DrawLines -> False, LineStyle -> {}, ShowPoints->True}]//Sort;
		
Options[ContourPlotGradientMethod] = {Color->{Hue,0,0.8},Direction->1,EndSize->0.03,DrawGraph->True,DrawLines->False,LineStyle->{},PointStyle->Automatic,PrintDisplay->False,ShowPoints->True,StartSize->0.005};

Options[GradientMethod]= {Direction -> 1, PrintDisplay->False, SameTest -> Automatic};

Options[PlotTNB] = {ArrowSize->Medium,BinormalVectorStyle->RGBColor[0,0,1],NormalVectorStyle->Darker[Green],PointStyle->PointSize[0.017],PrintDisplay->False,TangentVectorStyle->RGBColor[1,0,0],TNB->True,VectorStyle->{}};

Options[PlotTN] = Options[PlotTN3D] = {ArrowSize->Medium (*1*),NormalVectorStyle->RGBColor[0,1,0],PointStyle->PointSize[0.017],TangentVectorStyle->RGBColor[1,0,0],VectorStyle->{}};

Options[PlotAcceleration] = Options[PlotAcceleration3D] =
	 {AccelerationVectorStyle->Purple,ArrowSize->1,NormalVectorStyle->Darker[Green],PointStyle->PointSize[0.017],PrintDisplay->False,TangentVectorStyle->Red,TNProjections->False,VectorStyle->{}};

Options[PlotOsculatingCircle] = {CircleStyle->RGBColor[1,0,0],PointStyle->PointSize[0.017],RadiusStyle->Dashing[{0.01,0.02}]};

Options[PlotOsculatingCircle3D] = {CircleStyle->RGBColor[1,0,0],DrawPlane->False,PlaneStyle->Opacity[0.8],PointStyle->PointSize[0.017],RadiusStyle->Dashing[{0.01,0.02}]};

Options[TNB] = Options[TN] = Options[PrincipalTangent] = Options[PrincipalNormal] = Options[Binormal] = Options[Torsion] = Options[OsculatingCircle] = Options[OsculatingPlane] = Options[CenterOfCurvature] = Options[Curvature] = Options[TNComponents] = {Simplify -> True};
	
Options[DistancePointLine]=Options[DistancePointPlane]={PrintDisplay->False};

Options[IntegrationRegion3D] = {Faces->True, FaceMesh->None, FaceStyle->Opacity[1], Order -> dxdydz};

Options[TaylorPoly] = {UseSeries->False, UseLimit -> False};

Options[NewLimit] = {Direction -> Both};

Options[PlotBand] = {CurveStyle->{{Green,Tube[.02]}}, RegionStyle->{{Green,Opacity[0.7]}}};
(******************************************************************************)


Begin["`Private`"];




(* darwPoints coordinates points and pointstyle. Returns that list and the plotrange. *)
(*
drawPoint[point_?(VectorQ[#,NumericQ] || MatrixQ[#,NumericQ]&) ,opts___?OptionQ]:=
Module[{pts, ptstyle, plotpt},
	If[MatchQ[point, {}],
		plotpt = Point[{}],
		pts = Map[Take[#,2]&, If[Head[point[[1]]] === List, point, {point}]];
		ptstyle = setps5[PlotConstraint, DrawPointStyle, Length[pts], opts];
		plotpt = MapThread[Flatten[{#1,Point[#2]}]&,{ptstyle, pts}]]]
*)
checkbound[x0_,x1_,sectioneqns_] := Apply[Or, N[# + .001 <= x0||x1 <= # - .001]& /@ sectioneqns]


LUSumPlot[fun_,{x_,x0_,x1_,n_:1,m_:2}, lu_, regular_, opts___?OptionQ] := 
	Block[{nx0,nx1, f, graph, astyle, polystyle, esum, frame, print, dx = n/2, basepts, range, funvalues,
		baseintervals,lengths,maxlength,sumvalues,sum,list,list1,list2, list3, linedata, stylelinedata, pointdata, polypts, ptstyle,
		plotrange,  plotfun, axesorigin, minval, maxval, midpt, riem, simp, max, min, right, left, trap},
	If[Head[fun] === List && Length[fun] > 1, Return[Message[lusumplot::input,fun]]];


	{graph, polystyle, esum, frame, print} = 
		{DrawGraph, PolyStyle, Exact, Frame, PrintDisplay} 
					/. {opts} /. Options[LUSumPlot];

	If[esum && lu =!= reim, {nx0,nx1} = {x0,x1}, {nx0,nx1} = N[{x0,x1}]];

	f := Function[x, fun];

	If[regular === Regular,
		If[esum, dx = (x1-x0)/n, dx = N[(x1-x0)/n]];
		basepts = Range[nx0,nx1,dx];

		Which[
			lu === midpt,
				range = Drop[basepts + dx/2,-1];
					funvalues = Map[f,range],
			lu === riem,
				range = Partition[basepts,2,1];
					funvalues = Map[f,Map[Random[Real,N[#]]&,range]],
			lu === simp,
				range = Partition[basepts,3,2];
					funvalues = Map[f, basepts],
			lu === max,
				range = Range[nx0, nx1, dx];
				maxval = If[esum, MaxValue, NMaxValue];
					funvalues = Map[maxval[{f[x], #[[1]] <= x <= #[[2]]},x, WorkingPrecision -> $MachinePrecision,
						FilterRules[{opts},Options[NMaxValue]]//Evaluate(*, WorkingPrecision -> $MachinePrecision*)]&, Partition[range,2,1]],
			lu === min,
				range = Range[nx0, nx1, dx];
				minval = If[esum, MinValue, NMinValue];
					funvalues = Map[minval[{f[x], #[[1]] <= x <= #[[2]]},x, WorkingPrecision -> $MachinePrecision,
						FilterRules[{opts},Options[NMaxValue]]//Evaluate]&, Partition[range,2,1]],
			True,
				range = Range[nx0,nx1,dx/m]; 
					funvalues = Map[f, range] 
			],

		If[lu === simp && EvenQ[Length[regular]], Return[Message[SimpApprox::partition]]];
		
		basepts = If[Head[regular]===List, regular, Join[{nx0},Map[Random[Real,N[#]]&,Partition[Range[nx0,nx1,dx],2,1]],{nx1}]];

		baseintervals = Partition[basepts,2,1];
		lengths = Map[-Apply[Subtract,#]&,baseintervals];
		maxlength = Max[lengths];

		Switch[lu,
			midpt,
				range = Map[Apply[Plus,#]&,baseintervals]/2;
				funvalues = Map[f,range],
			riem,
				funvalues = Map[f,Map[Random[Real,N[#]]&,baseintervals]],
			max,
				funvalues = Map[NMaxValue[{f[x], #[[1]] <= x <= #[[2]]},x, WorkingPrecision -> $MachinePrecision,
					FilterRules[{opts},Options[NMaxValue]]//Evaluate(*, WorkingPrecision -> $MachinePrecision*)]&, baseintervals],
			min,
				funvalues = Map[NMinValue[{f[x], #[[1]] <= x <= #[[2]]},x, WorkingPrecision -> $MachinePrecision,
					FilterRules[{opts},Options[NMaxValue]]//Evaluate(*, WorkingPrecision -> $MachinePrecision*)]&, baseintervals],
			right | left | trap,
				funvalues = Map[f, basepts],
			simp,
				range = Partition[basepts,3,2];
				funvalues = Map[f, basepts],
			True,
				range = Join[Map[Drop[Range[Apply[Sequence,#],-Apply[Subtract,#]/m],
					    	-1] &,baseintervals],{x1}]//Flatten;
				funvalues = Map[f,range] ]
	];



				(* Test for singularities *)	

	If[Not[VectorQ[funvalues,NumberQ[N[#]]&]],Return @ Message[lusumplot::impint]];

	Which[			
			lu === simp, 
				sumvalues = Partition[funvalues,m+1,m];
				funvalues = Partition[
						    Transpose[{Flatten[range],Flatten[sumvalues]}],3],
			lu === trap, 
				sumvalues = If[regular =!= Regular,
								Drop[funvalues + RotateLeft[funvalues],-1]/2,
								{funvalues,-{Last[funvalues],First[funvalues]}/2}//Flatten],
			lu === left, sumvalues = funvalues = Drop[funvalues,-1],
			lu === right, sumvalues = funvalues = Drop[funvalues, 1],
			True, sumvalues = funvalues
		];


	sum = Which[
			regular =!= Regular, sumvalues.lengths,
			lu === trap, Apply[Plus,sumvalues] dx,
			lu === simp, Apply[Plus,sumvalues.{1,4,1}] dx/3,
			True, Apply[Plus, sumvalues] dx];

	If[regular =!= Regular && print, Print["Max Deltax = ", maxlength]];
	If[graph,
		If[esum, Print[sum]],
		Return @ sum];


	(* Prepare plot *)

	plotrange = PlotRange /. {opts} /. PlotRange->All;
	plotrange = (plotrange /. Automatic -> All);

	plotfun = PlotJump[{f[x]},{x,nx0,nx1}, opts, (*Evaluate[Sequence@@FilterRules[{opts}, Options[PlotJump]]],*) PlotRange -> plotrange];

	Which[		
	lu === trap,		
		list1 = Map[{#,0}&,Drop[basepts,-1]];
		list2 = Transpose[{Drop[basepts,-1],Drop[funvalues,-1]}];
		list3 = Transpose[{Drop[basepts, 1],Drop[funvalues,1]}];
		polypts = Join[Flatten[Transpose[{list1,list2,list3}],1],
					(*{{nx1,0},{nx0,0}}*)  {{Last[basepts],0},{First[basepts],0}}],

	lu === simp,
		list1 = Map[InterpolatingPolynomial[#,x]&,funvalues];
		list2 = Map[{x,#[[1]],#[[3]]}&,range];
		linedata = Replace[Drop[#,{2}]& /@ funvalues, {a_, b_} -> Line[{{a, b}, {a, 0}}], {2}];
		ptstyle = setps5[LUSumPlot, PointStyle, 1, opts];
		polypts = MapThread[Plot[#1,#2, Axes->None, AxesOrigin->{0,0},
				Filling -> Axis,
				PlotStyle -> polystyle,
				Sequence@@FilterRules[{opts},Options[Plot]]//Evaluate,
				Epilog-> Join[Flatten[{ptstyle, Point/@funvalues}],Flatten[{Dashing[Tiny],linedata}]] (*Join[Flatten[stylelinedata,1],pointdata]*)]&,{list1,list2}],
	True, 

		list1 = Map[{#,0}&,Drop[basepts,-1]];
		list2 = Transpose[{Drop[basepts,-1],funvalues}];
		list3 = Transpose[{Drop[basepts, 1],funvalues}];
		polypts = Join[Flatten[Transpose[{list1,list2,list3}],1],
					(*{{nx1,0},{nx0,0}}*)  {{Last[basepts],0},{First[basepts],0}}]
		];

		astyle = FillingStyle /. {opts} /. Options[LUSumPlot];
(*Print[polypts];*)


	If[lu =!= simp,
		Show[plotfun, 
			Graphics[{Flatten[{Opacity[.5], EdgeForm[Thin], astyle, Polygon[polypts]}](*, Line[polypts]*)}],  
				Sequence@@FilterRules[{opts}, Options[Graphics]], AxesOrigin->{Automatic,0}, PlotRange->All, PlotLabel -> If[esum || Not[print], None, StringForm["Area"] \[TildeEqual] N[sum]]],
	
(*Show[plotfun, 
			ListLinePlot[polypts, Evaluate[Sequence@@FilterRules[{opts},Options[ListLinePlot]]],PlotStyle->Hue[0.67, 0.6, 0.6],Filling->Axis,
					FillingStyle-> {1 -> {Axis, {Red, Green}}}],  
				Sequence@@FilterRules[{opts}, Options[Graphics]], AxesOrigin->{Automatic,0}, PlotRange->All, PlotLabel -> If[esum || Not[print], None, StringForm["Area"] \[TildeEqual] N[sum]]],*)
	
		Show[polypts, plotfun, Sequence@@FilterRules[{opts},Options[Graphics]], PlotRange->All, Axes->Automatic, 
				AspectRatio->1/GoldenRatio, AxesOrigin->Automatic, PlotLabel -> If[esum || Not[print], None, StringForm["Area"] \[TildeEqual] N[sum]]]]
];




PlotIntegralApprox[fun_, {x_,x0_,x1_}, opts___?OptionQ]:=
	Module[{type, partition, npartition, temp},

	If[FreeQ[{opts}, ApproxType], Return[Message[PIA::specifytype]]];
	{type, partition} = ApproxType /. {opts} /. Options[PlotIntegralApprox];

	If[VectorQ[partition, NumericQ],
		If[MatchQ[partition,{}], Return[Message[PIA::emptypartition]]];
		npartition = Sort[Union[partition, SameTest -> (N[#1] == N[#2] &)], (OrderedQ[{N[#1],N[#2]}]&)];
		If[Not[MatchQ[npartition, partition]], Message[PIA::notordered, npartition]];
		If[x0>First[npartition] || Last[npartition]>x1, Message[PIA::notpartition,x0,x1]; npartition = Select[npartition, x0<=#<=x1&]];
		If[Not[MemberQ[N[npartition], N[x0]]], temp = Flatten[{x0,npartition}], temp = npartition];
		If[Not[MemberQ[N[temp], N[x1]]], temp = Flatten[{temp,x1}]];
		If[MatchQ[temp,npartition], npartition = temp, Message[PIA::points, temp]; npartition = temp],
		If[MatchQ[Head[partition],Regular] && Not[And @@ (Positive[#] && IntegerQ[#] &) /@ (partition /. Regular[a__] -> {a})], Return[Message[PIA::regular]]]];

	Switch[
			ToString[type],
				"Upper", 		Which[
									MatchQ[partition, Regular[_Integer]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		max, 	Regular, 		opts],
									MatchQ[partition,MaxDeltax[_?NumericQ]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		max, 	Random, 		opts],
									VectorQ[npartition, NumericQ],	
										LUSumPlot[fun,{x,x0,x1}, 							max, 	npartition, 	opts],
									True, Return[Message[PIA::partition]]],

				
				"Lower", 		Which[
									MatchQ[partition, Regular[_Integer]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		min, 	Regular, 		opts],
									MatchQ[partition,MaxDeltax[_?NumericQ]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		min, 	Random, 		opts],
									VectorQ[npartition, NumericQ],	
										LUSumPlot[fun,{x,x0,x1}, 							min, 	npartition, 	opts],
									True, Return[Message[PIA::partition]]],

				
				"Right", 		Which[
									MatchQ[partition, Regular[_Integer]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		right, 	Regular, 		opts],
									MatchQ[partition,MaxDeltax[_?NumericQ]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		right, 	Random, 		opts],
									VectorQ[npartition, NumericQ],	
										LUSumPlot[fun,{x,x0,x1}, 							right, 	npartition, 	opts],
									True, Return[Message[PIA::partition]]],

				"Left", 		Which[
									MatchQ[partition, Regular[_Integer]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		left, 	Regular, 		opts],
									MatchQ[partition,MaxDeltax[_?NumericQ]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		left, 	Random, 		opts],
									VectorQ[partition, NumericQ],
										LUSumPlot[fun,{x,x0,x1}, 							left, 	npartition, 	opts],
									True, Return[Message[PIA::partition]]],

				"Midpoint",		Which[
									MatchQ[partition, Regular[_Integer]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		midpt, 	Regular, 		opts],
									MatchQ[partition,MaxDeltax[_?NumericQ]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		midpt, 	Random, 		opts],
									VectorQ[partition, NumericQ],
										LUSumPlot[fun,{x,x0,x1}, 							midpt, 	npartition, 	opts],
									True, Return[Message[PIA::partition]]],

				"Trap", 		Which[
									MatchQ[partition, Regular[_Integer]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		trap, 	Regular, 		opts],
									MatchQ[partition,MaxDeltax[_?NumericQ]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		trap, 	Random, 		opts],
									VectorQ[partition, NumericQ],	
										LUSumPlot[fun,{x,x0,x1}, 							trap, 	npartition, 	opts],
									True, Return[Message[PIA::partition]]],
									
				"Simpson", 		If[MatchQ[partition,Regular[_]] && EvenQ[First[partition]], 
									LUSumPlot[fun, {x,x0,x1,First[partition]}, 				simp, 	Regular, opts], 
										Return[Message[SimpApprox::even]]],
										
				"Riemann", 		Which[
									MatchQ[partition, Regular[_Integer]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		riem, 	Regular, 		opts],
									MatchQ[partition,MaxDeltax[_?NumericQ]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		riem, 	Random, 		opts],
									VectorQ[partition, NumericQ],	
										LUSumPlot[fun,{x,x0,x1}, 							riem, 	npartition, 	opts],
									True, Return[Message[PIA::partition]]],

				_,				Return[Message[PIA::type]]]
		];		
		
PlotIntegralApprox3D[fun_, {x_,x0_,x1_}, {y_,y0_,y1_}, opts___?OptionQ]:=
	Module[{type, partition, m, n},

	If[FreeQ[{opts}, ApproxType], Return[Message[PIA::specifytype]]];
	{type, partition} = ApproxType /. {opts} /. Options[PlotIntegralApprox3D];
	If[Not[MatchQ[partition, Regular[_,_] | Regular[_]]], Return[Message[PIA3D::notallowed]]]; 	
	{m, n} = {First[partition], Last[partition]};

	Switch[
			ToString[type],
				"Upper", 		pul3D[fun,{x,x0,x1,m},{y,y0,y1,n},Max,opts],

				"Lower", 		pul3D[fun,{x,x0,x1,m},{y,y0,y1,n},Min,opts],

				"Midpoint",		Module[{graph, surface, print, pstyle, criticals, dx = N[(x1 - x0)/m], dy = N[(y1 - y0)/n], cuboids, cstyle}, 
									{graph, surface, print, pstyle, cstyle} = {DrawGraph, DrawSurface, PrintDisplay, PlotStyle, CuboidStyle} /. {opts} /. Options[PlotIntegralApprox3D]; 
									criticals = Flatten[Table[fun, {x, x0 + dx/2, x1 - dx/2, dx}, {y, y0 + dy/2, y1 - dy/2, dy}], 1]; 
								pstyle = setps5[PlotIntegralApprox3D, PlotStyle, 1, opts];
								
								If[graph === True, 
									If[print === True, Print[Style[dx*dy*Plus @@ criticals,FontSize->12]]], 
									Return[dx*dy*Plus @@ criticals]]; 
    
								criticals = Partition[criticals, n]; 
								cuboids = Flatten[Table[Cuboid[{x0 + i*dx, y0 + j*dy, 0}, {x0 + (i + 1)*dx, y0 + (j + 1)*dy, criticals[[i + 1,j + 1]]}], {i, 0, m - 1}, {j, 0, n - 1}]]; 
								Show[Graphics3D[Flatten[{cstyle,cuboids}]], 
									If[surface, 
										Plot3D[fun, {x, x0, x1}, {y, y0, y1}, Evaluate[Sequence@@FilterRules[{opts},Options[Plot3D]]],
											(*PlotPoints -> {m + 1, n + 1}, MaxRecursion -> 0*) Mesh->{m-1,n-1}, PlotStyle->pstyle],
										Graphics3D[{}]],
									Sequence@@FilterRules[{opts},Options[Graphics3D]], Axes -> True, PlotRange -> All]
								],

				_,				Return[Message[PIA3D::type]]]
		];

pul3D[fun_,{x_,x0_,x1_,m_}, {y_,y0_,y1_,n_}, maxmin_, opts___?OptionQ] := 
	Module[{dx = N[(x1 - x0)/m], dy = N[(y1 - y0)/n], data, data1, data2, data3, data3p, pstyle, cstyle, graph, edge1, edge2},
	{cstyle, graph, surface, print} = 
		{CuboidStyle, DrawGraph, DrawSurface, PrintDisplay} /. {opts} /. Options[PlotIntegralApprox3D];
	pstyle = setps5[PlotIntegralApprox3D, PlotStyle, 1, opts];
	If[print,  
		 Print[If[maxmin === Max, Style[ul3D[fun, {x,x0,x1,m},{y,y0,y1,n},NMaxValue,opts],FontSize->12], Style[ul3D[fun, {x,x0,x1,m},{y,y0,y1,n},NMinValue,opts],FontSize->12] ]]];
	data = Table[{x,y,fun},{x,x0,x1,dx},{y,y0,y1,dy}];
	data1 = MakeMesh[data];
	data2 = Transpose/@data1;
	data3 = Map[{maxmin[Last[#]]}&,data2];
	data3p = Map[PadRight[#,4,#]&,data3];
	edge1 = Map[Flatten[{#[[1]][[{1,2}]],0}]&,data1];
	edge2 = Map[#[[3]]&,Transpose/@MapThread[ReplacePart[#1,3->#2]&,{data2,data3p}]];
	Show[Graphics3D[Flatten[{cstyle,MapThread[Cuboid[#1,#2]&,{edge1,edge2}]}]],
		(*Graphics3D[Flatten[{pstyle, Polygon/@data1}]],*)
		If[surface, 
				Plot3D[fun, {x, x0, x1}, {y, y0, y1}, Evaluate[Sequence@@FilterRules[{opts},Options[Plot3D]]],
											(*PlotPoints -> {m + 1, n + 1}, MaxRecursion -> 0*) Mesh->{m-1,n-1}, PlotStyle->pstyle],
				Graphics3D[{}]],
			Sequence@@FilterRules[{opts},Options[Graphics3D]],PlotRange->All, Axes->True]
];

ul3D[fun_,{x_,a_,b_,n_},{y_,c_,d_,m_}, type_, opts___?OptionQ]:=
	Module[{wp, na, nb, dx, nc, nd, dy},
	wp = WorkingPrecision/.{opts}/.Options[IntegralApprox];
	{na,nb,dx,nc,nd,dy} = N[{a,b,(b-a)/n,c,d,(d-c)/m}(*, wp*)];
	Total[Flatten[Outer[
		type[Flatten[{fun, #1[[1]] <= x <= #1[[2]] && #2[[1]] <= y <= #2[[2]]}], {x,y}, FilterRules[{opts}, Options[NMaxValue]]//Evaluate]&,
		Partition[Range[na,nb,dx],2,1],Partition[Range[nc,nd,dy],2,1],1]]] dx dy
]

IntegralApprox[fun_, {x_,x0_,x1_}, opts___?OptionQ] :=
	Module[{type, partition, wp, npartition, temp, esum, dx},
	If[FreeQ[{opts}, ApproxType], Return[Message[PIA::specifytype]]];
	{type, partition} = ApproxType /. {opts} /. Options[IntegralApprox];
	{wp, esum} = {WorkingPrecision, Exact} /. {opts} /. Options[IntegralApprox];
	If[VectorQ[partition, NumericQ],
		npartition = Sort[partition, Less];
		If[Not[MatchQ[npartition, partition]], Message[PIA::notordered,npartition]];
		If[Not[MatchQ[npartition, temp = Union[{x0},npartition,{x1}]]], 
			Message[PIA::points, temp]; npartition = temp],

			dx = If[esum, (x1 - x0)/First[partition], N[(x1 - x0)/First[partition],wp]]];   (* 8-20-17 Removed N *)

	Switch[
			ToString[type],
				"Upper", 		Which[ 
									MatchQ[partition, Regular[_Integer]],
										Total[Map[NMaxValue[{fun, #[[1]] <= x <= #[[2]]}, x, FilterRules[{opts},Options[NMaxValue]]//Evaluate] &, 
											Partition[Range[x0, x1, dx], 2, 1]]] dx,	
									VectorQ[partition, NumericQ] && x0<=Min[partition] && Max[partition]<=x1,	
										Map[NMaxValue[{fun,#[[1]]<=x<=#[[2]]}, x, FilterRules[{opts},Options[NMaxValue]]//Evaluate]&, Partition[npartition,2,1]].
											Drop[RotateLeft[npartition]-npartition, -1],
									True, Return[Message[IA::partition]]],
				"Lower", 		Which[
									MatchQ[partition, Regular[_Integer]],
										Total[Map[NMinValue[{fun, #[[1]] <= x <= #[[2]]}, x, FilterRules[{opts},Options[NMinValue]]//Evaluate] &, 
											Partition[Range[x0, x1, dx], 2, 1]]] dx,	
									VectorQ[partition, NumericQ] && x0<=Min[partition] && Max[partition]<=x1,	
										Map[NMinValue[{fun,#[[1]]<=x<=#[[2]]}, x, FilterRules[{opts},Options[NMinValue]]//Evaluate]&, Partition[npartition,2,1]].
											Drop[RotateLeft[npartition]-npartition, -1],
									True, Return[Message[IA::partition]]],
				"Right", 		Which[
									MatchQ[partition, Regular[_Integer]], 
										Sum[fun, {x,x0 + dx,x1,dx}] dx,
									VectorQ[partition, NumericQ] && x0<=Min[partition] && Max[partition]<=x1,								
										With[{f = Function[x, fun], spartition = Sort[N[partition], Less]},
											Map[f, Drop[spartition,1]].Drop[RotateLeft[spartition]-spartition,-1]],
									True, Return[Message[IA::partition]]],
				"Left", 		Which[
									MatchQ[partition, Regular[_Integer]], 
										Sum[fun, {x,x0,x1 - dx,dx}] dx,
									VectorQ[partition, NumericQ] && x0<=Min[partition] && Max[partition]<=x1,								
										With[{f = Function[x, fun], spartition = Sort[N[partition], Less]},
											Map[f, Drop[spartition,-1]].Drop[RotateLeft[spartition]-spartition,-1]],
									True, Return[Message[IA::partition]]],
				"Midpoint",		Which[
									MatchQ[partition, Regular[_Integer]],
										With[{f = Function[x,fun]},
											Total[Map[f, Drop[Range[x0, x1, dx] + dx/2, -1]]] dx],
									VectorQ[partition, NumericQ] && x0<=Min[partition] && Max[partition]<=x1,
										Module[{dxm, f = Function[x, fun], spartition = Sort[N[partition], Less]},
											dxm = Drop[RotateLeft[spartition]-spartition,-1];										
											Map[f,Drop[spartition,-1] + dxm/2].dxm],
									True, Return[Message[IA::partition]]],
				"Trap", 		Which[
									MatchQ[partition, Regular[_Integer]],
										Module[{data, f = Function[x,fun]}, 
											data = Map[f, Range[x0, x1, dx]];
											(Total[data] - (data[[1]]+data[[-1]])/2)*dx],
									VectorQ[partition, NumericQ] && x0<=Min[partition] && Max[partition]<=x1,
											Module[{data, f = Function[x,fun], spartition = Sort[partition, Less]},
											data = Map[f, spartition];
											(Drop[data + RotateLeft[data],-1]/2).Drop[RotateLeft[spartition]-spartition,-1]],
									True, Return[Message[IA::partition]]],
								
				"Simpson", 		If[EvenQ[First[partition]], 
									Module[{data, f = Function[x, fun]}, 
										data = f /@ Range[x0,x1,dx];
										(Plus@@(Plus@@({2,4}*#1)&)/@Partition[data,2]-data[[1]]+data[[-1]])*(dx/3)],
									Return[Message[SimpApprox::even]]],

				"Riemann", 		Which[
									MatchQ[partition, Regular[_Integer]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		riem, 	Regular, 		DrawGraph->False, opts],
									MatchQ[partition,MaxDeltax[_?NumericQ]],
										LUSumPlot[fun, {x,x0,x1,First[partition],1},		riem, 	Random, 		DrawGraph->False, opts],
									VectorQ[partition, NumericQ],	
										LUSumPlot[fun,{x,x0,x1}, 							riem, 	npartition, 	DrawGraph->False, opts],
									True, Return[Message[PIA::partition]]],

				_,				Return[Message[PIA::type]]]
		];



IntegralApprox[fun_, xrange_List, rest:_List.., opts___?OptionQ] :=
	Module[{type, partition, deltas},

	If[FreeQ[{opts}, ApproxType], Return[Message[PIA::specifytype]]];
	{type, partition} = ApproxType /. {opts} /. Options[IntegralApprox];
	If[Not[FreeQ[partition,MaxDeltax]], Return[Message[IA::partition]]];
	deltas = partition /. Regular[a__]:>{a};	
	If[Length[deltas]==1, deltas = First[deltas] Table[1,{Length[{xrange,rest}]}]];
	If[Not[Length[deltas]==Length[{xrange,rest}]],Return[Message[IA::incompatable]]];	
	If[MatchQ[ToString[type], "Simpson"] && Not[Apply[And, EvenQ[#]& /@ deltas]], Return[Message[SimpApprox::even]]];
	If[MatchQ[ToString[type], "Midpoint" | "Trap" | "Simpson"],
		Fold[IntegralApprox[#1, First[#2], ApproxType-> {type, Regular[Last[#2]]}, opts] &, fun, Reverse[Transpose[{{xrange, rest}, deltas}]]],
		Return[Message[IA2::type]]]
		];




PlotDiscreteIntegral[data_?MatrixQ,  opts___?OptionQ]:=
	Module[{type, graph, xdata, ydata, xmin, xmax, f, newdata, list1, list2},

	If[FreeQ[{opts}, ApproxType], Return[Message[PIA::specifytype]]];
	{type, graph, ptstyle}= {ApproxType, DrawGraph, PointStyle}/.{opts}/.Options[PlotIntegralApprox];
	{xdata, ydata} = Transpose[data];
	{xmin, xmax}={Min[xdata],Max[xdata]};

	Switch[
			ToString[type],

				"Right",        f = Interpolation[data,InterpolationOrder->0],			
				
				"Left",         ydata = Drop[Insert[ydata,ydata[[1]],1],-1];
								f = Interpolation[Transpose[{xdata,ydata}],InterpolationOrder->0],
								
				"Midpoint",		If[OddQ[Length[data]],
									newx=Take[xdata[[3;;Length[data];;2]]];
									newy=Take[ydata[[2;;Length[data];;2]]];
									newdata=Join[{data[[1]]},Transpose[{newx,newy}]];
									f = Interpolation[newdata,InterpolationOrder->0],
									Return[Message[PDI::midpt]]],
								
				"Upper",		list1=Map[{#[[1]],Max[#[[2]]]{1,1}}&,Transpose/@Partition[data,2,1]];
								sum = Total[Map[(-Subtract@@#[[1]]) #[[2,1]]&,list1]];
								list1=Transpose/@list1,
								
				"Lower",		list1=Map[{#[[1]],Min[#[[2]]]{1,1}}&,Transpose/@Partition[data,2,1]];
								sum = Total[Map[(-Subtract@@#[[1]]) #[[2,1]]&,list1]];
								list1=Transpose/@list1,

				"Trap", 		f = Interpolation[data,InterpolationOrder->1],
				
				"Simpson",	(*If[OddQ[Length[data]],
									newdata = Partition[data, 3, 2];
									list1 = Map[InterpolatingPolynomial[#, x] &, newdata];
									list2 = Map[#[[1]] <= x <= #[[3]] &, First /@ Transpose /@ newdata];
									f = Function[x,Piecewise[MapThread[{#1, #2} &, {list1, list2}]]],
									Return[Message[SimpApprox::partition]]],*)
							If[OddQ[Length[data]],
									newdata = Sort[data, #1[[1]] < #2[[1]] &];
									newdata = Partition[newdata, 3, 2];
									list1 = Map[InterpolatingPolynomial[#,x]&,newdata];
									list2 = Map[{x,#[[1]],#[[3]]}&,First /@ Transpose /@ newdata];
									sum = Plus @@ MapThread[NIntegrate[#1,#2]&,{list1,list2}];
									plot = MapThread[Plot[#1,#2, AxesOrigin->{0,0},Filling -> Axis,PlotLabel -> StringForm["Area"] \[TildeEqual] sum, PlotRange->All,
										Sequence@@FilterRules[{opts},Options[Plot]]//Evaluate,
										Epilog-> {AbsolutePointSize[6],Red, Point/@newdata}]&,{list1,list2}],
									Return[Message[SimpApprox::partition]]],

				"Spline",	   f = Interpolation[data,Method->"Spline"],

				_,				Return[Message[PDI::type]]];

prangedata = PlotRange /. AbsoluteOptions[ListPlot[data, Evaluate[Sequence@@FilterRules[{opts},Options[ListPlot]]],PlotStyle->ptstyle, PlotRange->All],PlotRange];

	If[graph,
		Which[
			MatchQ[ToString[type],"Simpson"], 
				Show[plot], 
			MatchQ[ToString[type],"Upper" | "Lower"],
				Show[ListLinePlot[list1, Evaluate[Sequence@@FilterRules[{opts},Options[ListLinePlot]]],PlotStyle->Hue[0.67, 0.6, 0.6],Filling->Axis,
					FillingStyle->Directive[{Opacity[0.2], Hue[0.67, 0.6, 0.6]}]],
					ListPlot[data, Evaluate[Sequence@@FilterRules[{opts},Options[ListPlot]]],PlotStyle->ptstyle],PlotRange->All,PlotLabel -> StringForm["Area"] \[TildeEqual] sum],
			True,
				Show[{
					Plot[f[x]//Evaluate,{x,xmin,xmax},Evaluate[Sequence@@FilterRules[{opts},Options[Plot]]], Filling -> Axis, AxesOrigin->{xmin,0},
						PlotLabel -> StringForm["Area"] \[TildeEqual] NIntegrate[f[x],{x,xmin,xmax}]],
					Graphics[{ptstyle, Point/@data},PlotRange->prangedata]}]],
		If[MatchQ[ToString[type],"Simpson" | "Upper" | "Lower"], 
			sum,
			NIntegrate[f[x],{x,xmin,xmax}]]]
		];

DiscreteIntegral[data_?MatrixQ, opts___?OptionQ]:=
	PlotDiscreteIntegral[data, DrawGraph->False, opts];
	

ImplicitD[eqn_Equal, ivar_, dvar_, n_Integer:1, opts___Rule] := 
   	Module[{consts, rrules, derivs}, 
	consts = FilterRules[{opts}, Options[ImplicitD]]/.{None -> {}}; 
	rrules = Complement[{opts}, consts]; 

	derivs = Last[Fold[With[{deqn = Dt[#1[[1]], ivar, consts]}, 

		{deqn, Flatten[{#1[[2]], Simplify[Solve[deqn //. #1[[2]], Dt[dvar, {ivar, #2}, consts]]]/. rrules }]}] &, 
       			{eqn, {{}}}, Range[1, n]]];
  	If[Not[rrules === {}], Simplify[derivs], derivs]];


DvecS[fun_,vars_]:= 
	Simplify[Cross @@ Transpose[JacobianMatrix[fun,vars]]];
	
DvecS[fun_,vars_,pt_List]:= 
	Simplify[Cross @@ Transpose[JacobianMatrix[fun,vars]]/.PointsToRules[pt,vars]];


IteratedLimit[fun_, rules__] := Fold[Limit, fun, Flatten[{rules}]];


SetPlotRange2[prange:{{_,_}..}]:=({Min[#1],Max[#1]}&)/@Flatten/@Transpose[prange];
SetPlotRange[prange:{{_, _, _}..}] := ({Min[#1], Max[#1]} & ) /@ Flatten /@ Transpose[prange];



PlotDirectionalDerivative[fun_, {x_, x0_, x1_}, {y_, y0_, y1_}, pt:{_,_}|{{_,_}..}, opts___?OptionQ] := 
  Module[{da,dv,dvposition, plane, vectorstyle, pointstyle, prange, fung, pts, 
		vecs, drawbasevecs, basevecs, prange1, vsect, xsect, ysect, surf, pts1, temp, slopes},
	{da, dv, dvposition, plane, drawbasevecs,mesh} = 
		{DirectionAngle, DirectionVector, DirectionVectorPosition, DrawPlane, BaseVector, Mesh} /. 
			{opts} /. Options[PlotDirectionalDerivative];

	If[Flatten[{da, dv}] == {}, Return[Message[nooption::direction]]]; 
	dv = Join[Normalize/@If[MatrixQ[dv],dv, {dv}], ({Cos[#],Sin[#]}& /@ Flatten[{da}])];
	dv = DeleteCases[dv,{}];

	{vectorstyle, pointstyle} = 
      		setps5[PlotDirectionalDerivative, { VectorStyle, PointStyle}, 
      				{Length[dv], Length[dv]}, opts]; 
				

	prange = PlotRange /. AbsoluteOptions[sur=Plot3D[fun,{x,x0,x1}, {y,y0,y1}, Sequence@@FilterRules[{opts},Options[Plot3D]]//Evaluate], PlotRange]; 

	pts1 = If[Head[pt[[1]]]===List, pt, {pt}]; 
	fung = Function[{x, y}, {x,y,fun}];
	pts = Apply[fung, pts1, {1}];

	vecs = Flatten[Outer[{#1, #1 + Flatten[{#2, 
			DirectionalDerivative[fun, {x, y}, Drop[#1, -1], #2,
				Sequence @@ FilterRules[{opts}, Options[DirectionalDerivative]]] // Evaluate}]} &, pts, dv, 1], 1];


If[Not[FreeQ[vecs, Indeterminate | Null]], 
		Message[pdd::nodd]; vecs = Select[vecs, MatrixQ[#, NumericQ] &]];	

	If[dvposition === Automatic, dvposition = prange[[3,1]]];
	basevecs = Flatten[Outer[{Flatten[{#1, dvposition}], Flatten[{#1, dvposition}] + #2}&, pts1, Flatten[{#,0}]& /@ dv, 1],1];

	prange1 = PlotRange /. AbsoluteOptions[Show[sur,Graphics3D[Line /@ Join[basevecs, vecs]], PlotRange->All], PlotRange];
	
	prange = SetPlotRange[{prange,prange1}];

	{vsect, xsect, ysect} = 
		{If[(temp = DeleteCases[dv, {0, _} | {_, 0}]) === {{}}, {}, 
			slopes = Flatten[(Divide @@ Reverse[#1] & ) /@ temp]; 
				Flatten[Outer[{#2, -1} . ({x, 0} - #1) & , pts1, slopes, 1]]], 
		If[MemberQ[dv, {0, _}], First /@ pts1, {}], 
		If[MemberQ[dv, {_, 0}], Last  /@ pts1, {}]};

	
	surf = PlotSection[fun,Flatten[{x, prange[[1]]}]//Evaluate, Flatten[{y, prange[[2]]}]//Evaluate,
		VSection -> vsect, XSection -> xsect, YSection -> ysect, 
		VSectionStyle -> vectorstyle, XSectionStyle -> vectorstyle, YSectionStyle -> vectorstyle,
		DrawCylinder -> plane, CylinderBase -> dvposition, PlotRange->prange, Mesh->mesh, opts, 
		BoxRatios -> Automatic]; 

	If[dvposition === Automatic, dvposition = prange[[3,1]]];
	basevecs = Map[ReplacePart[#, dvposition, 3]&, basevecs, {2}];

	vecs = PlotVector3D[If[drawbasevecs, Join[basevecs,vecs], vecs],
		PlotRange -> prange, VectorStyle -> vectorstyle, opts];

	Show[vecs, surf, Graphics3D[Flatten[{pointstyle, If[drawbasevecs,Point/@(Flatten[{#,dvposition}]& /@ pts1),{}], Point/@pts}]], 
		PlotRange -> prange, PlotRangePadding -> {Scaled[.01],Scaled[.01],Scaled[.04]},Sequence@@FilterRules[{opts},Options[Graphics3D]]//Evaluate]
];


(*New 11-13-2014 *)  (* Modified with Remove/RestorePiecewise  03-18-16 *)
DirectionalDerivative[fun_, vars_?VectorQ, pt_?VectorQ, dir_?VectorQ, opts___?OptionQ] := 
	Module[{limit = UseLimit /. {opts} /. Options[DirectionalDerivative]},
	RemovePiecewise[]; 
	deriv =	If[limit,
		f = Function[vars,fun];
		NewLimit[(f@@(pt + h dir/Norm[dir,Reals]) - f@@pt)/h, h->0],
        KPGrad[fun, vars, pt, opts] . Normalize[dir, Reals] ];
	RestorePiecewise[];
	deriv
		];


DistancePointLine[P_List, {A_List, B_List},opts___?OptionQ] := 
	Module[{pd, vecAP = P-A, proj },
	pd = PrintDisplay /. {opts} /. Options[DistancePointLine];
	proj = Projection[vecAP, B-A];
	If[pd, Print["The point on the line nearest the point ", P, " is ",A+proj]];
	Norm[vecAP - proj,Reals]];

DistancePointLine[pt_List, expr_ == d_, vars_List, opts___?OptionQ] := 
	DistancePointLine[pt, expr - d, vars, opts];
	
DistancePointLine[pt_List, expr_List, var_, opts___?OptionQ] := 
	Module[{vart = First[Flatten[{var}]], pp},
	pp = expr /. PointsToRules[{0,1}, vart];
	DistancePointLine[pt,pp,opts]];	


DistancePointLine[pt_List, expr_, vars_List, opts___?OptionQ] := 
	Module[{solve, pp},
	Off[Solve::svars];
	solve=Solve[expr == 0];
	On[Solve::svars];
	pp = vars /.solve[[1]]/. PointsToRules[{{0,0},{1,1}}, vars];
	DistancePointLine[pt,pp,opts]];


DistancePointPlane[pt_List, (exp_) == (d_), vars_List, opts___?OptionQ] := 
	DistancePointPlane[pt, exp - d, vars, opts];

DistancePointPlane[pt_List, fun_List, vars_List, opts___?OptionQ] := 
	Module[{pd,span,normal,pp,pn}, 
	pd = PrintDisplay /. {opts} /. Options[DistancePointPlane];
	span = Coefficient[fun,#]& /@ vars; 
	normal = NullSpace[span];
	If[Length[normal]>1, Return[Message[DistancePointPlane::nothplane]]];
	normal = First[normal];
	pp = First[fun /. PointsToRules[ZeroVector[Length[vars]], vars]];
	pn = Projection[pt - pp, normal,Dot]; 
	If[pd, Print["The point on the plane nearest the point ", pt," is ", pt - pn]]; 
	Norm[pn,Reals]];
  
DistancePointPlane[pt_List, exp_, vars_List, opts___?OptionQ] := 
	Module[{pd, normal, solve, pp, pn}, 
	pd = PrintDisplay /. {opts} /. Options[DistancePointPlane]; 
	normal = Coefficient[exp,#]& /@ vars;
	Off[Solve::svars]; 
	solve = Solve[exp == 0]; 
	On[Solve::svars]; 
	pp = First[vars /. solve[[1]] /. PointsToRules[ZeroVector[Length[vars]], vars]]; 
	pn = Projection[pt - pp, normal, Dot]; 
	If[pd, Print["The point on the plane nearest the point ", pt," is ", pt - pn]]; 
	Norm[pn,Reals]] /; PolynomialDegree[exp, vars] == 1
	
DistancePointPlane[point_?VectorQ, span_?MatrixQ, opts___?OptionQ] :=
	Module[{print, pt},
	print = PrintDisplay /. {opts} /. Options[DistancePointPlane];
	pt = point - ProjectVector[point, span];
	If[print,
		Print["The point on the plane nearest the point ", point, " is " , pt]];
	Norm[pt,Reals]
  ]


PrincipalTangent[fun_List, t_, opts___?OptionQ] := PrincipalTangent[fun, t, t, opts]

PrincipalTangent[fun_List, t_, pt_, opts___?OptionQ] := 
  Module[{simp, dfun = D[fun, t] /. t -> pt},
    simp = Simplify /. {opts} /. Options[PrincipalTangent];
    If[simp, dfun = Simplify[dfun];
      		dfun/PowerExpand[Sqrt[Simplify[dfun.dfun]]], 
      		dfun/Norm[dfun]]]

TNB[fun:{_, _, _}, t_, opts___?OptionQ] := 	TNB[fun, t, t, opts]
TNB[fun:{_, _, _}, t_, pt_, opts___?OptionQ] := 
	Module[{simp, dfun1, dfun2, cross, ncross, ndfun1},
	simp = Simplify /. {opts} /. Options[TNB];
	{dfun1,dfun2} = Rest[NestList[D[#,t]&,fun,2]] /. t -> pt;
	If[dfun2 == {0,0,0}, Return[Message[Curvature::curvaturezero,t,pt]]];
	If[simp, 
		cross = Simplify[Cross[dfun1,dfun2]];
		ndfun1 = PowerExpand[Sqrt[Simplify[dfun1.dfun1]]];
		ncross = PowerExpand[Sqrt[Simplify[cross.cross]]];
		{dfun1/ndfun1, Cross[cross,dfun1]/(ncross ndfun1), cross/ncross}//Simplify,
		cross = Cross[dfun1,dfun2];
		ndfun1 = Sqrt[dfun1.dfun1];
		ncross = Sqrt[cross.cross];
		{dfun1/ndfun1, Cross[cross,dfun1]/(ncross ndfun1), cross/ncross}]	] 
		

PrincipalNormal[fun:{_, _} | {_, _, _}, t_, rest___] := 
  With[{tnb = TN[fun, t, rest]}, If[tnb =!= Null, tnb[[2]], Return[]]]


Binormal[fun:{_, _} | {_, _, _}, t_, rest___] := 
  With[{tnb = TNB[fun, t, rest]}, If[tnb =!= Null, tnb[[3]], Return[]]]


TN[fun:{_,_}, t_, rest___] := With[{tnb = TNB[Append[fun,0], t, rest]}, If[tnb =!= Null, Map[Drop[#,-1]&,Drop[tnb,-1]], Return[]]]

TN[fun:{_,_,_},t_, rest___] := With[{tnb = TNB[fun, t, rest]}, If[tnb =!= Null, Drop[tnb, -1], Return[]]]



PlotTnb[fun_, {t_, t0_, t1_}, pts_, tnb_, acc_, tnbp_, opts___?OptionQ] := 
	Module[{npts = N[pts], len = Length[fun], print, asize, pplot, graphics, curve, 
			astyle, tstyle, nstyle, bstyle, ptstyle, vstyle, ps, fvals, data, accvals, prange, ptss = pts, tdata},
	{print, asize} = {PrintDisplay, ArrowSize} /. {opts} /. Options[PlotTNB];
	asize = If[Head[asize]===Symbol, asize, .0375 asize];

	Which[
		len == 2, 
			{pplot, graphics} = {ParametricPlot, Graphics}; curve = fun,
		len > 4, 
			Return[Message[ParametricPlot3D::ppfun, fun]],
		True, 
			curve = Take[fun, 3]; 
			{pplot, graphics} = {ParametricPlot3D, Graphics3D} ];
			
	fvals = Map[curve /. t-># &, npts]//Chop;

	data = If[acc === tnbp, Map[If[tnb, TNB, TN][curve, t, #, Simplify -> False]&, pts]];

	With[{pos = Flatten[Position[data,Null]]},
		If[Not[pos ==={}],	
			Message[NB::normalbinormal, t, pts[[pos]]]; npts = Delete[npts, List/@pos]; ptss = Delete[pts, List/@pos]; 
			data = Delete[data, List/@pos]; fvals = Delete[fvals, List/@pos]]
		];

	If[print,
		If[acc, 
			If[tnbp,
				Print["The principal tangent and principal normal at the point(s) ", 		
					curve/.PointsToRules[ptss,t]//Simplify, ":  "]; Print[data]], 
			Print["The principal tangent, principal normal, (and binormal) at the point(s) ", 		
				curve/.PointsToRules[ptss,t]//Simplify, ": "]; Print[data]]];
	If[acc, 
		accvals = (D[curve, {t, 2}] /. t -> #1 & ) /@ ptss; 
	If[print, 
		Print["Acceleration at time(s) t = ", ptss, ": "]; Print[accvals]]; 
	If[tnbp, 
		tdata = Flatten[MapThread[Outer[(#1 . #2) & , {#1}, #2, 1] & , {accvals, data}], 1]; 
      		If[print, 
			Print["The tangential and normal scalar components of the acceleration vector(s): "]; 
        		Print[tdata//Simplify]]; 
		data = MapThread[Append[#2 #3, #1] & , {accvals, data, tdata}], 
		data = List /@ accvals]]; 

	data = Chop[Flatten[MapThread[Outer[{#1, #1 + #2} & , {#1}, #2, 1] & , {fvals, data}], 2]]; 
	astyle = setps5[PlotAcceleration3D, AccelerationVectorStyle, Length[npts], opts];
	{tstyle, nstyle, bstyle, ptstyle} = 
		setps5[PlotTNB, {TangentVectorStyle, NormalVectorStyle, BinormalVectorStyle, PointStyle}, Length[npts], opts];


	If[Not[(vstyle = VectorStyle /. {opts} /. Options[PlotTNB]) === {}], 
		{astyle, tstyle, nstyle, bstyle} = 
			Map[Map[Flatten[Append[#, vstyle]]&, #]&, {astyle, tstyle, nstyle, bstyle}]];	

	ps = Flatten[Transpose[
		If[tnb, 
			If[acc, If[tnbp, {tstyle, nstyle, astyle}, {astyle}], {tstyle, nstyle, bstyle}],
			If[acc, If[tnbp, {tstyle, nstyle, astyle}, {astyle}], {tstyle, nstyle}]]
			],1];

	If[Length[Flatten[{prange}]]==1 && NumericQ[prange],
		If[pplot === ParametricPlot3D,
			prange = prange {{-1,1},{-1,1},{-1,1}},
			prange = prange {{-1,1},{-1,1}}]];

	Show[{
		pplot[fun//Evaluate, {t,t0,t1}, Sequence@@FilterRules[{opts}, Options[pplot]]//Evaluate], 
		graphics[MapThread[Flatten[{#1, Point[#2]}]&, {ptstyle, fvals}]],
		graphics[MapThread[Flatten[{#1, Arrowheads[asize], Arrow[#2]}]&, {ps, data}]]},
			Sequence@@FilterRules[{opts}, Options[graphics]], Axes -> Automatic, PlotRange->All]
	]


	
PlotTNB[fun_, {t_, t0_, t1_}, pts_, opts___?OptionQ] := 
	PlotTnb[fun, {t, t0, t1}, Flatten[{pts}], TNB /.{opts}/. Options[PlotTNB], False, False, opts] 

PlotTN[fun_, {t_, t0_, t1_}, pts_, opts___?OptionQ] := 
	PlotTN3D[fun, {t,t0,t1},pts,opts]

PlotTN3D[fun_, {t_, t0_, t1_}, pts_, opts___?OptionQ] := 
	If[VectorQ[Flatten[{pts}],NumericQ], PlotTnb[fun, {t, t0, t1}, Flatten[{pts}], False, False, False, opts],
	Message[plottn::numeric]]

PlotAcceleration[fun_, {t_, t0_, t1_}, pts_, opts___?OptionQ] := 
	PlotTnb[fun, {t, t0, t1}, Flatten[{pts}], False, True, TNProjections /.{opts}/. Options[PlotAcceleration], opts]  

PlotAcceleration3D[fun_, {t_, t0_, t1_}, pts_, opts___?OptionQ] := 
	PlotTnb[fun, {t, t0, t1}, Flatten[{pts}],False, True, TNProjections /.{opts}/. Options[PlotAcceleration3D], opts]


TNC[fun:{_, _, _}, t_, pt_, opts___?OptionQ] := 
	Module[{simp, dfun1, dfun2, dcross}, 
	simp = Simplify /. {opts} /. Simplify -> True; 
	{dfun1, dfun2} = Rest[NestList[D[#1, t] & , fun, 2]] /. t -> pt; 
	If[simp, 
		Sqrt[Simplify[
			With[	{cross = Expand[Cross[dfun1, dfun2], Trig -> True],
      			dot1 = Expand[dfun1.dfun1, Trig -> True],
      			dot2 = Expand[dfun1.dfun2, Trig -> True]}, 
			dcross = cross.cross;
			{dot2^2,dcross,dcross}/{dot1,dot1,dot1^3}]]], 
		Sqrt[With[{cross = Cross[dfun1, dfun2],dot1 = dfun1.dfun1, dot2 = dfun1.dfun2}, 
			dcross = cross.cross;
			{dot2^2,dcross,dcross}/{dot1,dot1,dot1^3}]]
		]
	]

Curvature[fun:{_,_,_}, t_, opts___?OptionQ] :=  
	Last[TNC[fun, t, t, opts]]
	
Curvature[fun:{_,_,_}, t_, pt_, opts___?OptionQ] := 
	Last[TNC[fun, t, pt, opts]]
	
Curvature[{f_,g_}, t_, opts___?OptionQ] := 
	Last[TNC[{f,g,0},t, t, opts]]	
	
Curvature[{f_,g_}, t_, pt_, opts___?OptionQ] := 
	Last[TNC[{f,g,0},t, pt, opts]]

TNComponents[fun:{_,_,_}, t_, opts___?OptionQ] :=  
	TNComponents[fun,t,t,opts]
	
TNComponents[fun:{_,_,_},t_,pt_,opts___?OptionQ]:= 
	Drop[TNC[fun,t,pt,opts],-1]
	
TNComponents[{f_,g_}, t_, opts___?OptionQ] := 
	TNComponents[{f,g,0},t, t, opts]	
	
TNComponents[{f_,g_}, t_, pt_, opts___?OptionQ] := 
	TNComponents[{f,g,0},t, pt, opts]




	
Torsion[f:{_,_,_}, t_, opts___?OptionQ] := 
	Torsion[f, t, t, opts]
	
Torsion[f:{_,_,_}, t_, pt_, opts___?OptionQ] := 
	Module[{simp, fp, fpp, fppp},
	simp = Simplify /. {opts} /. Options[Torsion];
	{fp,fpp,fppp} = Rest[NestList[D[#,t]&,f,3]] /. t -> pt;
	If[simp, 
		With[{cp = Expand[Cross[fp,fpp], Trig -> True]}, Simplify[cp.fppp/cp.cp]],
		With[{cp = Cross[fp,fpp]}, cp.fppp/cp.cp]]	]

OsculatingPlane[eqns:{_,_,_}, t_, {u_,v_}, pt_, opts___?OptionQ] := 
	(eqns /. t -> pt) + u PrincipalTangent[eqns, t, pt, opts] + v PrincipalNormal[eqns, t, pt, opts]

OsculatingPlane[eqns:{_,_,_}, t_, {u_,v_}, opts___?OptionQ] := 
	OsculatingPlane[eqns, t, {u,v}, t, opts]

OsculatingCircle[eqns:{_,_,_}, t_, u_, pt_, opts___?OptionQ] := 
	Module[{pnormal = PrincipalNormal[eqns, t, pt, opts], rho = 1/Curvature[eqns, t, pt, opts]},
	(eqns /. t -> pt) + rho pnormal + rho Cos[u] PrincipalTangent[eqns, t, pt, opts] + rho Sin[u] pnormal]

OsculatingCircle[eqns:{_,_,_}, t_, u_, opts___?OptionQ] := 
	OsculatingCircle[eqns, t, u, t, opts]
			
OsculatingCircle[eqns:{_,_}, t_, u_, pt_, opts___?OptionQ] := 
	Module[{rho = 1/Curvature[eqns, t, pt, opts]},
	(eqns /. t -> pt) + rho PrincipalNormal[eqns, t, pt, opts] + rho {Cos[u], Sin[u]}]

OsculatingCircle[eqns:{_,_}, t_, u_, opts___?OptionQ] := 
	OsculatingCircle[eqns, t, u, t, opts]

CenterOfCurvature[eqns_List, t_, pt_, opts___?OptionQ] :=
	(eqns/.t->pt) + PrincipalNormal[eqns,t,pt,opts]/Curvature[eqns,t,pt,opts]

CenterOfCurvature[eqns_List, t_, opts___?OptionQ] :=
	CenterOfCurvature[eqns, t, t, opts]

PlotOsculatingCircle[eqn:{_,_},{t_,t0_,t1_},pt_?(VectorQ[#,NumberQ[N[#]]&]&),opts___?OptionQ]:=
	Module[{cstyle, ptstyle, rstyle, radii, data, centers, circles },
(*	{radius, center} = {DrawRadius, Center} /. {opts} /. Options[PlotOsculatingCircle];*)
	{cstyle, ptstyle, rstyle} = 
		setps5[PlotOsculatingCircle, {CircleStyle, PointStyle, RadiusStyle}, Length[pt], opts];
	data = {eqn /. t -> #, 1/Curvature[eqn, t, #, Simplify -> False], 
				PrincipalNormal[eqn, t, #, Simplify -> False]}& /@ N[pt];

	centers = (#[[1]] + #[[2]] #[[3]]&) /@ data;
	circles = MapThread[Graphics[Flatten[{#1, Circle[#2, #3[[2]]]}]]&, {cstyle, centers, data}];
	radii =   If[True (*radius*), MapThread[Graphics[Flatten[{#1, #2, Line[{#3, #4[[1]]}]}]]&, {cstyle, rstyle, centers, data}],Graphics[{}]];
	centers = If[True (*center*), MapThread[Graphics[Flatten[{#1, #2, Point [#3]}]]&, {cstyle, ptstyle, centers}], Graphics[{}]];

	Show[
		ParametricPlot[eqn//Evaluate, {t, t0, t1},
			Sequence@@FilterRules[{opts}, Options[ParametricPlot]]//Evaluate], 
		circles, centers, radii, Sequence@@FilterRules[{opts}, Options[Graphics]], AspectRatio -> Automatic,PlotRange->All]
	]
	
PlotOsculatingCircle[eqn:{_,_}, {t_, t0_, t1_}, pt_?(NumberQ[N[#]]&), opts___?OptionQ] := 
	PlotOsculatingCircle[eqn, {t, t0, t1}, {pt}, opts]

PlotOsculatingCircle3D[eqns_List, {t_, t0_, t1_}, pt_?(VectorQ[#, NumberQ[N[#]]&]&), opts___?OptionQ] := 
	Module[{radius, center, cstyle, ptstyle, rstyle, data, centers, circles, radii, plane, eqn = Take[eqns,3], planestyle},	
	{radius, center,planestyle} = {DrawRadius, Center, PlaneStyle} /. {opts} /. Options[PlotOsculatingCircle3D];		
	{cstyle, ptstyle, rstyle, plane} = 
			setps5[PlotOsculatingCircle3D, {CircleStyle, PointStyle, RadiusStyle, DrawPlane}, Length[pt], opts];
	data = {
				(eqn /. t -> #), 
				1/Curvature[eqn, t, #, Simplify -> False], 
				PrincipalNormal[eqn, t, #, Simplify -> False],
				PrincipalTangent[eqn, t, #, Simplify -> False]}& /@ N[pt];
	
	centers = 	#[[1]] + #[[2]] #[[3]]& /@ data;
	circles = Flatten[{#[[1]] + #[[2]] #[[3]] + #[[2]] Cos[u] #[[4]] + #[[2]] Sin[u] #[[3]]}]& /@ data;
	circles = ParametricPlot3D[circles//Evaluate,{u,0,2Pi},PlotStyle->cstyle,
		  Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]]//Evaluate, PlotRange->All];
	radii = If[True (*radius*), Graphics3D[MapThread[Flatten[{#1, #2, Line[{#3, #4[[1]]}]}]&, {cstyle, rstyle, centers, data}]],Graphics3D[{}]];
	centers = If[True (*center*), Graphics3D[MapThread[(Flatten[{#1, #2, Point[#3]}]&) , {cstyle, ptstyle, centers}]],Graphics3D[{}]];

	plane = MapThread[If[Last[#1], 
			ParametricPlot3D[#2[[1]] + u #2[[4]] + v #2[[3]]//Evaluate, 
				{u,-#2[[2]], #2[[2]]},{v,0, 2 #2[[2]]}, (*PlotPoints -> 3, MaxRecursion->0,*)Mesh->None, PlotStyle->planestyle, PlotRange->All],
			Graphics3D[{}]]&, 	{plane, data}];

	Show[ 	circles, plane,
		ParametricPlot3D[eqns//Evaluate, {t, t0, t1}, 
			Evaluate[Sequence@@FilterRules[{opts},Options[ParametricPlot3D]]]], 
	  centers, radii]
	]
	
PlotOsculatingCircle3D[eqns_List, {t_, t0_, t1_}, pt_?(NumberQ[N[#]]&), opts___?OptionQ] := 
	PlotOsculatingCircle3D[eqns, {t, t0, t1}, {pt}, opts]


PlotTube[fun:{_, _, _}|{_, _, _, _}, rad_, {t_, t0_, t1_}, opts___?OptionQ] := 
	ParametricPlot3D[Evaluate[fun + rad*(Cos[u]*PrincipalNormal[fun, t, t,  Simplify -> False] + 
     				Sin[u]*Binormal[fun, t, t, Simplify -> False])], {t, t0, t1}, {u, 0, 2 Pi}, 
     		Evaluate[Sequence@@FilterRules[{opts},Options[ParametricPlot3D]]]]




PlotSurfaceField[fun_, {x_,x0_,x1_}, {y_,y0_,y1_}, field_List, vars_List, opts___?OptionQ] := 
	Module[{vs, vstyle, asize, funl, plot, data, vecs, vf = Function @@ {vars, field}, ppts},
	{vs, vstyle, asize, ppts} = 
		{VectorScale, VectorStyle, ArrowSize, VectorPoints} /. {opts} /. Options[PlotSurfaceField];
	ppts=Flatten[{ppts}]//N;
	asize = If[Head[asize]===Symbol, asize, .025 asize];
	funl = If[Head[fun] === List, fun, {x,y,fun}];
	pstyle = PlotStyle /. FilterRules[{opts}, {PlotStyle -> Opacity[.7]}]/.PlotStyle -> Opacity[.7];
	plot = MakePolygons[Table[funl,{x,x0,x1,(x1-x0)/First[ppts]},{y,y0,y1,(y1-y0)/Last[ppts]}]];

	data = plot/. Polygon[list_] :> Apply[Plus,list]/4;

	vecs = Apply[vf,#]& /@ data//Chop;

	vecs = If[vs === Automatic, vs = 1/Max[Norm /@ vecs],vs] vecs + data;
	
	data = Transpose[{data,vecs}];

	vstyle = setps5[PlotSurfaceField, VectorStyle,Length[data], opts];

	Show[Graphics3D[{pstyle,plot}], Graphics3D[Flatten[{vstyle, Arrowheads[asize],Arrow /@ data}]],
			Sequence@@FilterRules[{opts}, Options[Graphics3D]], PlotRange -> All,Axes->Automatic]
]

ParaPlotSurfaceField[fun_List,{r_,r0_,r1_},{t_,t0_,t1_}, field_List, vars_List, opts___?OptionQ] := 
	PlotSurfaceField[fun,{r,r0,r1},{t,t0,t1},field, vars, opts]


(*8/14/08: Removed WireFrame as an option*)
PlotContourSurface[fun_, {x_, x0_, x1_}, {y_, y0_, y1_}, opts___] := 
	Module[{bcp, plot, zmin, zmax, clines3D, contours, cstyle}, 
	{contours, cstyle, bcp} = {Contours, ContourStyle, ContourPlotPosition} /. {opts} /. Options[PlotContourSurface]; 
	contours = contours /. Automatic -> 11;

	clines3D = ContourPlot[fun, {x, x0, x1}, {y, y0, y1}, Mesh->None, Contours -> contours, ContourStyle -> cstyle, 
			Evaluate[Sequence @@ FilterRules[Flatten[{opts}], Options[ContourPlot]]]]; 
	contours = If[VectorQ[contours],{contours,0},contours];
	plot = Plot3D[fun, {x, x0, x1}, {y, y0, y1}, ClippingStyle -> None, Mesh -> contours, MeshFunctions -> {#3 & }, MeshStyle -> cstyle,
		Evaluate[Sequence @@ FilterRules[Flatten[{opts}], Options[Plot3D]]]]; 			
	{zmin, zmax} = Last[PlotRange /. AbsoluteOptions[plot, PlotRange]]; 

(*{zmin,zmax} = {NMinValue[{fun, x0 <=x<=x1, y0<=y<=y1},{x,y},PowerBehavior->Real],NMaxValue[{fun, x0 <=x<=x1, y0<=y<=y1},{x,y},PowerBehavior->Real]};*)
	If[bcp === Automatic, bcp = zmin - 0.75 (zmax - zmin)];
	clines3D = Replace[clines3D[[1]], {a_, b_} -> {a, b, bcp},{2}];
 
clines3D = DeleteCases[clines3D,{{},{},bcp},Infinity];  (*Added 3-10-17 Fixes an error caused by setting Contours to 1 or to a list containing a single number *)
	Show[
		plot, 
		Graphics3D[clines3D],
			PlotRange -> {Min[zmin,bcp], zmax}, 
			Sequence @@ FilterRules[Flatten[{opts}], Options[Graphics3D]]
		]
	]

PlotProperty[fun_List,rest__] := 
	If[Length[Flatten[{fun}]]==1, 
		PlotProperty[First[fun],rest],Return[]];		
	
PlotProperty[fun_,{x_,x0_,x1_},prop_, opts___?OptionQ] := 
	Module[{ps, newfun, fun1, pts, prange},
	fun1 = If[MatchQ[Head[fun],Abs],
		PiecewiseExpand[fun, Reals], fun];	

	ps = PlotStyle /. {opts} /. Options[Plot];
	psp = setps5[PlotProperty, PropertyStyle, 1, opts];
(*	ps = Join[Flatten[{ps}],psp];*)
	ps = {ps,psp};

	Switch[ToString[prop],
		"Positive", 	{newfun = fun1, pts = True, ps = Reverse[ps]},
		"Negative",		{newfun = fun1, pts = True},
		"Increasing", 	{newfun = D[fun1,x], pts = False, ps = Reverse[ps]},
		"Decreasing",	{newfun = D[fun1,x], pts = False},
		"ConcaveUp", 	{newfun = D[fun1,{x,2}], pts = False, ps = Reverse[ps]},
		"ConcaveDown",	{newfun = D[fun1,{x,2}], pts = False},
		_, Return @ Message[PlotProperty::invalid,prop]];
	
	prange = PlotRange/.AbsoluteOptions[PlotJump[fun1//Evaluate,{x,x0,x1},Evaluate[Sequence@@FilterRules[{opts},Options[PlotJump]]]],PlotRange];	

	If[Not[FreeQ[newfun, Derivative]],
		Return[Message[PD::noderiv]]];

Quiet[Show[
		PlotJump[ If[newfun >= 0, fun, Null]//Evaluate, {x, x0, x1},
              PlotStyle -> First[ps], ShowJumpPoint -> pts, opts, PlotRange-> prange],
		PlotJump[ If[newfun <= 0, fun, Null]//Evaluate, {x, x0, x1},
              PlotStyle -> Last[ps], ShowJumpPoint -> pts, opts, PlotRange-> prange],
			Evaluate[Sequence@@FilterRules[{opts},Options[Graphics]]]],{Power::infy,Infinity::indet}]
	]
	

	
(*
PlotProperty[fun_List,rest__] := 
	If[Length[Flatten[{fun}]]==1, 
		PlotProperty[First[fun],rest],Return[]];		
	
PlotProperty[fun_,{x_,x0_,x1_},prop_, opts___?OptionQ] := 
	Module[{ps, newfun, fun1, pts, prange},
	fun1 = If[MatchQ[Head[fun],Abs],
		PiecewiseExpand[fun, Assumptions->Element[x,Reals]], fun];	
	ps = PlotStyle /. {opts} /. Options[Plot];
	psp = setps5[PlotProperty, PropertyStyle, 1, opts];
(*	ps = Join[Flatten[{ps}],psp];*)
	ps = {ps,psp};

	Switch[ToString[prop],
		"Positive", 	{newfun = fun1, pts = True, ps = Reverse[ps]},
		"Negative",		{newfun = fun1, pts = True},
		"Increasing", 	{newfun = D[fun1,x], pts = False, ps = Reverse[ps]},
		"Decreasing",	{newfun = D[fun1,x], pts = False},
		"ConcaveUp", 	{newfun = D[fun1,{x,2}], pts = False, ps = Reverse[ps]},
		"ConcaveDown",	{newfun = D[fun1,{x,2}], pts = False},
		_, Return @ Message[PlotProperty::invalid,prop]];
	
	prange = PlotRange/.AbsoluteOptions[PlotJump[fun1,{x,x0,x1},Evaluate[Sequence@@FilterRules[{opts},Options[PlotJump]]]],PlotRange];	

	If[Not[FreeQ[newfun, Derivative]],
		Return[Message[PD::noderiv]]];

	Show[
		PlotJump[ If[newfun >= 0, fun1, Null], {x, x0, x1},
              PlotStyle -> First[ps], ShowJumpPoint -> pts, opts, PlotRange-> prange],
		PlotJump[ If[newfun <= 0, fun1, Null], {x, x0, x1},
              PlotStyle -> Last[ps], ShowJumpPoint -> pts, opts, PlotRange-> prange],
			Evaluate[Sequence@@FilterRules[{opts},Options[Graphics]]]]
	]
*)

PlotWeb[fun_,{x_,x0_,x1_},inval_,iter_Integer,drop_Integer:0,opts___?OptionQ] := 
	Block[{nx0 = N[x0], nx1 = N[x1], print, asize, ptstyle, pts, plot, lines},
	{print, asize} = {PrintDisplay, ArrowSize} /. {opts} /. Options[PlotWeb];
	pts = NestList[Function[x,fun],Nest[Function[x,fun],inval,drop]//Simplify,iter - drop]//Simplify;
	If[print, Print[pts]];

	pts = Chop[N[pts]];
	lines = Flatten[{inval, pts, Last[pts]}]//N;

	lines = Take[Nest[Partition[#,2,1]&, Partition[lines,2,1]//Flatten,2], {2,-2}];
	{nx0,nx1} = {Min[pts,nx0], Max[pts,nx1]};
	plot = Plot[{fun,x},{x,nx0,nx1}, 
				Sequence@@FilterRules[{opts}, Options[Plot]]//Evaluate, AspectRatio -> Automatic];
	lines = PlotVector[lines, Sequence@@AbsoluteOptions[plot, PlotRange], 
				ArrowSize -> asize, Sequence@@FilterRules[{opts}, Options[PlotVector]]//Evaluate];
	ptstyle = setps5[PlotWeb, PointStyle, 1, opts];
	Show[plot, lines, Graphics[{ptstyle, Point[{inval,inval}]}//Flatten],  Sequence@@FilterRules[{opts}, Options[Graphics]]]
	]


PlotOrbits[fun_,{x_,x0_},{k_,kmin_,kmax_,step_},iter_Integer,drop_Integer:0,opts___?OptionQ] := 
	Block[{ptstyle,colors,dk = (kmax - kmin),dc, kiter},
	{ptstyle,colors} = {PointStyle,Color} /. {opts} /. Options[PlotOrbits];
	dc = (colors[[3]] - colors[[2]])/dk;
	Show[Graphics[
		Table[
			{colors[[1]][(kiter - kmin) dc],ptstyle, Map[Point[{kiter,#}]&,
				NestList[(fun/.k -> kiter) /.x -> # &, 
					Nest[(fun/.k -> kiter) /.x -> # &,N[x0],drop],iter - drop]]} //Flatten,
		{kiter,kmin,kmax,step}]],
		Sequence@@FilterRules[{opts}, Options[Graphics]],Axes -> Automatic,AxesOrigin -> {kmin,0},PlotRange -> All]
	]

PlotBand[{fun1_List,fun2_List}, {t_,t0_,t1_}, opts___] := 
	Module[{rstyle, cstyle},
	{rstyle, cstyle} = setps5[PlotBand, {RegionStyle, CurveStyle}, {1,2}, opts];
	Show[{ParametricPlot3D[{fun1 ,fun2}//Evaluate, {t,t0,t1}, PlotStyle->cstyle, FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate],
		ParametricPlot3D[fun1 + d (fun2 - fun1)//Evaluate, {t,t0,t1}, {d,0,1}, PlotStyle->rstyle, FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate, Mesh->None]}]
		]
		


(* 10-05-17 Changed Block to Module and Put x in the Module *)
PlotJump[f_, {x_,a_,b_}, (opts___)?OptionQ]:=
	Module[{x, ptstyle, jump, jumps, jumps1, jumps2, len, asymptote, asymptotes, hastyle, astyle, hasymptote, hasymptotes, prangeha, 
		newf, subcurves, point, zerotonull,disconts, discontss, rcenters, lcenters, prange, centers, ymin, ymax, ptsize, dpts},
	If[a==b,Return[Message[Plot::plld,x,{x,a,b}]]];
	{jump, asymptote, astyle, hasymptote, hastyle, point, zerotonull} =
		{Jump, Asymptote, AsymptoteStyle, HorizontalAsymptote, HorizontalAsymptoteStyle, ShowJumpPoint, ZeroToNull}
			/.{opts}/.Options[PlotJump];
	{jump, asymptote, hasymptote} = {jump, asymptote, hasymptote} /. {None -> {}};
	
	prangeha = If[Not[MatchQ[Flatten[{hasymptote}],{}]], {(a+b)/2,#}&/@N[hasymptote], {}];  (*Added 12-28-16*)

	{asymptotes, hasymptotes, jumps} = Flatten/@{{asymptote}, {hasymptote}, {jump}};

	jumps1 = Sort[Union[asymptotes,jumps],Less];
	jumps2 = Select[jumps1,N[a]<=N[#1]<=N[b]&];
(*	If[Not[SubsetQ[jumps2,jumps1]],Message[InRange::notinrange,jumps1,{a,b}]];*)  (*01-07-16  Causes trouble with manipulations *)
	jumps2 = Intersection[jumps2,jumps];
	Off[Piecewise::pairs, NIntegrate::nlim];
(*	newf = Flatten[{If[Head[f]===Tooltip, ReleaseHold[Hold[f]/.Tooltip[rest_]:>Hold[rest]], Hold[f]]}];*)
	newf = Flatten[{If[Head[f]===Tooltip, ReleaseHold[Hold[f]/.Tooltip[rest_]:>Inactivate[rest]], Inactivate[f]]}];
	len = Length[Flatten[Activate[{newf/.x -> RandomReal[{a,b}]}]]];
	Off[Plot::exclul];
	newf = Map[
			If[Not[FreeQ[Activate[#],Piecewise]], 
				Activate[#]/.
					If[zerotonull,
						Piecewise[alist_List,0]->Piecewise[Join[alist,{{Null,True}}]],
						Piecewise[alist_List,0]->Piecewise[Join[alist,{{0.,True}}]]],#]&, newf];
								
(* 02-13-2012: Changed jumps2 to jumps1 *)
	subcurves = 
		If[Not[FreeQ[f, Integrate | NIntegrate | Convolve (*Spring_2014*), Infinity]], 
			zerotonull = False;
			Plot[f,{x,a,b},Exclusions->If[Not[jumps1==={}],(x==#1&)/@jumps1,Automatic],ExclusionsStyle->None,
				Evaluate[Sequence@@FilterRules[{opts},Options[Plot]]]]/.Line[{xs_,ys_}]->Line[{}],				
			Plot[Activate[f] /.
				If[zerotonull,
					Piecewise[alist_List, 0] :> Piecewise[Join[alist,{{Null,True}}]],
					Piecewise[alist_List, 0] :> Piecewise[Join[alist,{{0.,True}}]]]//Evaluate,
				{x,a,b}, Exclusions -> If[Not[jumps1 === {}],(x == #1&) /@ jumps1, Automatic],
				ExclusionsStyle->None,Sequence@@FilterRules[{opts},Options[Plot]]//Evaluate]/.Line[{xs_,ys_}]->Line[{}]];
	prange = PlotRange /. AbsoluteOptions[subcurves,PlotRange];  (*Added 12-28-16 *)

	On[Plot::exclul];
	If[Not[jumps2==={}] ,
		Off[Power::infy, Infinity::indet, GreaterEqual::nord, LessEqual::nord, Power::indet];

(*			disconts=({x,#1}&)/@ newf;*)
	
			disconts = Transpose[
							If[FreeQ[Activate[disconts], Piecewise | Abs | Surd, Infinity], 
								disconts=({x,#1}&)/@ newf;
								Activate[disconts  /. x -> #] & /@ jumps2,
								disconts=({x,#1}&)/@ PiecewiseExpand[Activate[newf],Reals];
								disconts /. x -> #  & /@ jumps]];	(*Revised 10-28-17*)

							
(*			disconts=Transpose[Activate[disconts /. x -> #] & /@ jumps2];  Reinstated the previous three lines 10-28-17*)

			If[Not[And@@MatrixQ/@disconts],
					disconts = Transpose[Map[Flatten[#,2]&,Map[Outer[List,{First[#]},Rest[#]]&,First[disconts]]]]];

			disconts = Replace[disconts, {_a, _?(Not[NumericQ[#]] &)} -> {a,Null}, {2}];

			lcenters = (Thread[{#1,Limit[Rationalize[Activate[newf],0],x->#1,Direction-> 1, PowerBehavior->Complex]}]&)/@DeleteCases[jumps2,a];  (*12-23-15 Need PowerBehavior->Complex to make Surd behave properly *)
			rcenters = (Thread[{#1,Limit[Rationalize[Activate[newf],0],x->#1,Direction->-1, PowerBehavior->Complex]}]&)/@DeleteCases[jumps2,b];  (*Removed Activate from newf *)
			centers = Flatten[{lcenters,rcenters},1];  (* 12-23-15 Removed RealPower *)

			centers = 	If[Not[And@@MatrixQ/@centers],
					Transpose[Map[Flatten[#,2]&,Map[Outer[List,{First[#]},Rest[#]]&,First/@centers]]],
					Transpose[centers]];						
			centers = Replace[centers, {_, _?(Not[NumericQ[#]] &)} -> {{},{}}, {2}];
			If[len==1,centers = Flatten[centers,1]],
			disconts=centers={{}}];

	If[Not[point], disconts = Table[{}(*{{}}*), {i,Length[disconts]}](* Changed list in Table 10-28-17*)(*; centers = Table[{{}}, {i,Length[centers]}]*)];

	{astyle, hastyle} = setps5[PlotJump,{AsymptoteStyle,HorizontalAsymptoteStyle},Length/@{asymptotes,hasymptotes}, opts];
	If[Not[hasymptotes==={}], hasymptotes = MapThread[Flatten[{#1,Line[{{a,#2},{b,#2}}]}]&, {hastyle,hasymptotes}]];


(*	discontss = Flatten[DeleteCases[disconts, {_, Null} | {_, Indeterminate} | {_, ComplexInfinity}, Infinity],1];*)
	discontss = Select[First[disconts],FreeQ[Null | Indeterminate | ComplexInfinity]]; (* Changed previous line to this 10-28-17*)

	prange = Last[setdp[PlotJump, discontss, prange, Evaluate[Sequence@@FilterRules[{opts},Options[setdp]]]]];


(*
	Off[Axes::axes, Ticks::ticks];
	prange = PlotRange /. AbsoluteOptions[
		Show[{
			(*Plot[f,{x,a,b},Evaluate[Sequence@@FilterRules[FilterRules[{opts},Options[Plot]],Except[PlotTheme]]]],*)
			subcurves/.Legended[data_,_]->data,
			Graphics[Map[Point,disconts,{2}]]//Evaluate,
			Graphics[hasymptotes]}(*,
				PlotRange -> Which[
						NumericQ[prange],{Full,{-prange,prange}},
						VectorQ[prange, NumericQ], {Full, prange} (* {{a,b}, prange}*),  (*12-28-16 *)
						prange === All, {Full, All} (*{{a,b},All}*),
						True, {Full, Automatic}(* PlotRange /. AbsoluteOptions[subcurves, PlotRange]*)]*)],PlotRange];(*Changed 11-27-16*)  (*Changed 12-28-16*)
	On[Axes::axes, Ticks::ticks];
*)


	If[Not[asymptotes === {}],
		{ymin,ymax} = prange[[2]];
		asymptotes = MapThread[Flatten[{#1,Line[{{#2,ymin},{#2,ymax}}]}]&, {astyle,asymptotes}]];

	disconts = DeleteCases[disconts,{{}, {}} .. | {{{}, {}} ..}];
	centers = DeleteCases[centers,{{}, {}} .. | {{{}, {}} ..}];
	If[MatrixQ[centers], centers = {centers}];

	ptstyle = setps5[PlotJump, PointStyle, len (*Length[First[disconts]*), opts]//Quiet;
	If[MemberQ[ptstyle,PointSize[_],Infinity],
		Message[PlotJump::ptsize]; 
		ptstyle = (ptstyle/.(PointSize[_]->AbsolutePointSize[7]))];	
		ptsize = Last/@(Cases[#, AbsolutePointSize[size_]->size, Infinity]& /@ ptstyle)/.{Tiny -> 1, Small -> 3, Medium -> 5, Large -> 7};
	ptstyle = DeleteCases[ptstyle, _AbsolutePointSize, Infinity];
	On[Piecewise::pairs, NIntegrate::nlim];

	dpts = DrawPoint /. {opts} /. Options[PlotJump];
	dpts = If[MatchQ[Head[dpts],List], dpts, {dpts}];

	If[MatchQ[Flatten[{dpts}],{}], 
			dpts = {},
			newnewf = ({x,#1}&)/@Flatten[Activate[newf]];  (*Removed ReleaseHold from newf *)(*7-4-16  Added[Activate]*)
			dpts = Sort[Map[If[Length[#] == 1, Apply[Sequence, Activate[newnewf/.x->Sequence@@#]], #] &, Map[Flatten[{#}] &, DeleteCases[dpts, {}]]]];(*Removed ReleaseHold from newnewf *)(*7-4-16 Added Activate *)

		{dpts, prange} = 
				If[And @@ (Map[Length[#] == 2 &, dpts]),
					setdp[PlotJump, dpts, prange, Evaluate[Sequence@@FilterRules[{opts},Options[setdp]]]],
					Message[DrawPoint::dpt2];{{},prange}]
		];

	Show[subcurves,
		If[Not[MatchQ[Flatten[centers],{}]],
			ListPlot[centers, Filling -> None,  Sequence@@FilterRules[{opts}, Options[ListPlot]], PlotRange->All,
				PlotMarkers -> 
					Map[Graphics[{Flatten[{AbsolutePointSize[#1], Point[{0,0}]}],Flatten[{GrayLevel[1], AbsolutePointSize[#1 - 2],Point[{0,0}]}]}]&, 
						ptsize[[1;;Length[centers]]]]],
			Graphics[{}]],
		Graphics[dpts],
		If[Not[MatchQ[Flatten[disconts],{}]],
			ListPlot[disconts,  Filling -> None, Sequence@@FilterRules[{opts}, Options[ListPlot]], PlotRange->All,
				PlotMarkers-> 
					MapThread[Graphics[Flatten[{AbsolutePointSize[#1], #2,  Point[{0,0}]}]]&, Map[Take[#, Length[disconts(*First[disconts]*)]]&, {ptsize, ptstyle}]]],
			Graphics[{}]],
		Graphics[{asymptotes,hasymptotes}],
				PlotRange->prange,  (*Changed 11-29-16*)
				Sequence@@FilterRules[{opts},Options[Graphics]],PlotRangePadding->Scaled[.05]]
]




(* Changed 3/5/09  *)(*Fixed issue with axes origin and plotrange 7/06/14*) (*Fixed issue with PlotRange 11-25-16 *)
PlotInverse[fun_, {x_, a_, b_}, (opts___)?OptionQ] :=
	Module[{istyle, plotfunction, plotline, plot1, plot2, min, max, line, lstyle},
	{plotfunction, plotline, lstyle} = {PlotFunction, PlotLine, LineStyle} /. {opts} /. Options[PlotInverse];

	If[((InverseStyle /. {opts}) === InverseStyle)  && Length[Flatten[{fun}]] > 1, 
		If[(PlotStyle /. {opts}) === PlotStyle, 
			istyle = Automatic,
			istyle = setps5[PlotInverse, InverseStyle, Length[Flatten[{fun}]],InverseStyle->(PlotStyle/.{opts})]],
		istyle = setps5[PlotInverse, InverseStyle, Length[Flatten[{fun}]], opts]];
prange = PlotRange /. {opts} /. Options[Plot];
Off[General::prng];  (* Changed to fix PlotRange bug in 11.1 11-29-16 *)
	plot1 = If[plotfunction, 
				PlotJump[fun, {x, a, b}, PlotRange -> prange, opts], 
				Graphics[{}]];
On[General::prng];
	If[plot1 === Null, Return[]];
	plot2 = PlotJump[fun, {x, a, b}, PlotStyle -> istyle, opts];
	plot2 = Graphics[GeometricTransformation[plot2[[1]], ReflectionTransform[{-1, 1}]]];

	{min, max} = {Min[#], Max[#]} &[PlotRange /. AbsoluteOptions[Show[plot1, plot2], PlotRange]];

	If[Not[plotfunction], plotline = False];
	line = If[plotline, Flatten[{lstyle, Line[{{min, min}, {max, max}}]}],{}];
	Show[plot1, plot2, Graphics[line], FilterRules[{opts},Options[Graphics]],
			PlotRange -> {All, PlotRange /. {opts} /. PlotRange -> All}, 
			AspectRatio -> Automatic, Axes -> Automatic]]



NPlotRoots[eqn_, x_, opts___]:=PlotRoots[eqn,x,opts,flag->numerical]

PlotRoots[eqn_, x_, opts___?OptionQ] := 
	Block[{pointstyle, print, roots, flag, numerical, points},
	{pointstyle, print} = {PointStyle, PrintDisplay} /. {opts} /. Options[PlotRoots];
	roots = If[(flag /. {opts}) === numerical, NSolve[eqn, x], Solve[eqn, x]] ;
	pointstyle = setps5[PlotRoots, PointStyle, Length[roots], opts];
	points = Point[{Re[x], Im[x]}]/.roots;
	If[print, Print[roots]];
	Show[Graphics[MapThread[Flatten[{#1, #2}]&,{pointstyle,points}]],
		Sequence@@FilterRules[{opts}, Options[Graphics]], AspectRatio -> Automatic, Axes -> Automatic]
]



PlotTangentLine[Tooltip[fun_],{t_,t0_,t1_},tanpts_(*?NumericQ*), opts___?OptionQ]:=	
	PlotTangentLine[fun, {t,t0,t1}, tanpts, opts]

PlotTangentLine[fun_,{t_,t0_,t1_},tanpts_(*?NumericQ*), opts___?OptionQ]:=	
	plotTangentLine[Flatten[{fun}], {t,t0,t1}, tanpts, opts]

(*All styles are passed to ParaPlotTangentLine *)	
plotTangentLine[fun_List, {t_, t0_, t1_}, tanpts_(*?NumericQ*), opts___?OptionQ] := 
	Module[{limit, funvalues, eqns, tpts = Rationalize[Flatten[{tanpts}],0]},
(*	limit = If[Not[FreeQ[fun, Piecewise | Abs]], True, False];  (*01-22-16 Added Abs *)*)
limit = UseLimit /. {opts} /. Options[TangentLine];
	If[PrintDisplay /. {opts} /. Options[PlotTangentLine],
		funvalues = MapThread[Thread[{#1,#2}]&,{tpts, fun/.PointsToRules[tpts,t]}];
		eqns = TangentLine[fun, t, tpts, UseLimit->limit,
			Evaluate[Sequence@@FilterRules[{opts},Join[Options[NewLimit],Options[TangentLine]]]]];
		MapThread[{Print["Point: ",#1], Print["Tangent line: y = ", #2]}&, {Flatten[funvalues,1], Flatten[eqns,1]}]];

	ParaPlotTangentLine[Thread[{t, fun}]//Evaluate, {t, t0, t1}, tpts, UseLimit->limit, 
			PrintDisplay->False, opts, AspectRatio->1/GoldenRatio, Normalize -> True]
		]


(* 03-15-2013  Does not support Tooltip*)	
ParaPlotTangentLine[Tooltip[fun_],{t_,t0_,t1_},tanpts_, opts___?OptionQ]:=	
	ParaPlotTangentLine[fun, {t,t0,t1}, tanpts, opts(*, Flag2 -> Tooltip*)]

ParaPlotTangentLine[fun_?VectorQ,{t_,t0_,t1_}, tanpts_, opts___?OptionQ]:=
	If[Length[fun]==2,ParaPlotTangentLine[{fun},{t,t0,t1},tanpts,opts], Message[PPTL::vector]]

ParaPlotTangentLine[fun_,{t_,t0_,t1_}, tanpts_, opts___?OptionQ]:=
	Module[{linelength, print, plot, tanptsl, prange, aratio, tlines, lstyle, ptsize, plotstyle, dpts, psdata},
	If[Head[fun] === Piecewise, Return[Message[Limit::piecewise]]];
	{linelength,print,prange, aratio} = {LineLength, PrintDisplay,PlotRange, AspectRatio} /. {opts} /. 
		Join[Options[ParaPlotTangentLine],Options[ParametricPlot]];
	If[MatchQ[prange, All], prange = Full];
	
	tanptsl = Flatten[Rationalize[{tanpts}]];
	gr = DrawGraph /. {opts} /. DrawGraph->True;

	If[MatchQ[gr, True],
		plot = ParametricPlot[fun,{t,t0,t1}, PlotRange->prange,
			Sequence@@FilterRules[{opts},Options[ParametricPlot]]//Evaluate];
		prange = PlotRange/.AbsoluteOptions[plot,PlotRange];
		aratio = AspectRatio/.AbsoluteOptions[plot,AspectRatio]];
		
	tlines = Transpose[ParaTangentLine[fun,t,tanptsl, Flag->True,
				Sequence@@FilterRules[{opts},Join[Options[ParaTangentLine],Options[Limit]]]//Evaluate]];

	If[print, 
		Print["Parametric equations of the tangent line(s) using ", t, " as the parameter are: "];  
		Print[tlines[[2]]/.{_, {0, 0}} -> Indeterminate]];

	plotstyle=setps5[ParametricPlot,PlotStyle,Length[fun],opts];
	{ptsize,lstyle}=setps5[ParaPlotTangentLine,{TangentPointStyle,TangentLineStyle},2 Length[tanptsl] Length[fun],opts];

	If[FreeQ[plotstyle,RGBColor[_,_,_] | Hue[_]],
		psdata = Darker[ColorData[97][#]&/@Range[ Length[fun]]];
		psdata = PadRight[psdata[[1;;Length[fun]]], 2 Length[tanptsl] Length[fun], psdata[[1;;Length[fun]]]];
		lstyle = MapThread[Flatten[{#1,#2}]&,{psdata,lstyle}],
		psdata = plotstyle;
		psdata = PadRight[psdata[[1;;Length[fun]]], 2 Length[tanptsl] Length[fun], psdata[[1;;Length[fun]]]];
		lstyle = MapThread[Flatten[{#1,#2}]&,{psdata,lstyle}]];
		
	lstyle = Flatten/@Transpose[{ptsize, lstyle}]; 
	lstyle = Flatten[Transpose[Partition[lstyle,Length[fun] Length[tanptsl]]],1];
	tlines = ListPlot[Flatten[Map[TanLines[First[#1],Last[#1], prange, aratio, 1.25 linelength]&,tlines[[1]]],2],
		Joined->{False,True}, PlotStyle->lstyle, (*PlotRange->All,*) Evaluate[Sequence @@ FilterRules[{opts},Options[ListPlot]]]];


	prange = SetPlotRange2[{prange,PlotRange /. AbsoluteOptions[tlines, PlotRange]}];

	dpts = DrawPoint /. {opts} /. Options[ParaPlotTangentLine];
	If[MatchQ[Flatten[dpts],{}], 
			dpts = {},
			dpts = If[MatchQ[Head[dpts[[1]]],List],dpts,{dpts}];
			{dpts, prange} = If[And @@ (Map[Length[#] == 2 &, dpts]),
				setdp[ParaPlotTangentLine, dpts, prange, Evaluate[Sequence@@FilterRules[{opts},Options[setdp]]]],
				Message[DrawPoint::dpt2];{{},prange}]
				];	
	Show[{If[gr, plot, Graphics[{}]], tlines, Graphics[dpts]}, 
			(*AspectRatio->aratio,*) Evaluate[FilterRules[{opts},Options[Graphics]]], PlotRangePadding->Scaled[.04], PlotRange -> prange]
]


	
ParaTangentLine[fun_, t_Symbol, tpts_?(VectorQ[#,NumericQ]&), opts___?OptionQ] := 
	Map[ParaTangentLine[fun, t, #, opts] &, tpts]

ParaTangentLine[fun_, {t_Symbol, t0_}, opts___?OptionQ] := 
	ParaTangentLine[fun, t, t0, opts]
		
ParaTangentLine[fun_, t_Symbol, t0_?NumericQ, opts___?OptionQ] := 
	Module[{normalize, limit, f := Function[t, fun], deriv, flag, pt}, 
	If[Head[fun] === Piecewise, Return[Message[Limit::piecewise]]];
	normalize = Normalize /. {opts} /. Options[ParaTangentLine];
	limit = UseLimit /. {opts} /. Options[ParaTangentLine];
(*	If[Not[FreeQ[fun, Piecewise]], limit = True];*)

	RemovePiecewise[];
	deriv = If[limit,
			NewLimit[ReleaseHold[Rationalize[(f[t0 + h] - f[t0]),0]/h], h->0, (*PowerBehavior->Complex, *)Sequence@@Evaluate[FilterRules[{opts},Options[Limit]]]]
			(*NewLimit[ReleaseHold[Hold[(f[t0 + h] - f[t0])/h]], h->0, Evaluate[Sequence@@FilterRules[{opts},Options[Limit]]]]*),
			(*f'[t0]]//Quiet;*)
			D[fun,t]/.t->t0//Quiet];
	RestorePiecewise[];
	deriv = (# /. {_, (DirectedInfinity[___] | Indeterminate | Null | Infinity)} -> {0, 0}&) /@ deriv;
	deriv = (# /. {(DirectedInfinity[___] | Indeterminate | Null | Infinity), _} -> {0, 0}&) /@ deriv;
	deriv = deriv /. Indeterminate -> {0,0};
	deriv = deriv /. {_, (DirectedInfinity[___] | Indeterminate | Null | Infinity)} -> {0, 0};
	deriv = deriv /. {(DirectedInfinity[___] | Indeterminate | Null | Infinity), _} -> {0, 0};

(* Adding option to return a list {dir vector, point} and the line, 07-21-2012 *)	


	flag = Flag /. {opts} /. Flag->False;
	If[flag, 
		pt = ReleaseHold[f[t0]];
		If[Not[FreeQ[deriv, Indeterminate]], deriv = {{0,0}}];
		{{deriv, pt}, pt + t deriv},
		If[normalize, deriv = If[VectorQ[deriv], Normalize[deriv], Map[Normalize[#]&, deriv]]];
		If[FreeQ[deriv, {0,0}], 
			deriv = ReleaseHold[f[t0]] + t deriv//Simplify, 
			Message[TanLines::SingularPoint,ReleaseHold[f[t0][[Flatten[Position[deriv,{0,0}]]]]]]; Indeterminate]]
	]

TangentLine[Tooltip[fun_],t_,tanpts_, opts___?OptionQ]:=	
	TangentLine[fun, t (*{t,t0,t1}*), tanpts, opts(*, Flag2 -> Tooltip*)]

TangentLine[fun_, t_, tpts_List, opts___?OptionQ] := 
	Map[TangentLine[fun, t, #, opts] &, Rationalize[tpts]]

TangentLine[fun_, {t_, t0_}, opts___?OptionQ] := 
	TangentLine[fun, t, t0, opts]
	
(*Added NewLimit*)	
TangentLine[fun_, t_, t0_?NumericQ, opts___?OptionQ] := 
	Module[{t00, limit, f := Function[t, fun], deriv},
	limit = UseLimit /. {opts} /. Options[TangentLine];
	If[Not[FreeQ[fun, Floor ]], limit = True]; (*Added 10-14-11*)
	t00 = Rationalize[t0];
	RemovePiecewise[];
	deriv = If[limit,
			NewLimit[ReleaseHold[Hold[Rationalize[(f[t00 + h] - f[t00]),0]/h]], h->0, 
				Evaluate[Sequence@@FilterRules[{opts},Options[Limit]]]]//ReleaseHold,
			D[fun,t]/.t->t0 (*RealPower[f'[t0]]  f'[t0] does not evaluate properly. *)]//Quiet;
	RestorePiecewise[];
	If[FreeQ[deriv, DirectedInfinity[___] | Indeterminate | Null | Infinity],
			ReleaseHold[f[t0]] + (t-t0) deriv//Simplify,
			Message[Limit::noderiv, t, t0]; deriv]
]

PolarTangentLine[fun_, {t_, t0_}, x_] := 
	PolarTangentLine[fun,t,t0,x]

PolarTangentLine[fun_, t_, t0_, x_] := 
	Module[{x0, y0},
	{x0, y0} = ({fun Cos[t], fun Sin[t]} /. t -> t0);
	y0 + (x - x0) Apply[Divide, Reverse[D[{fun Cos[t], fun Sin[t]}, t] /. t -> t0]]]

PlotDerivative[fun_, {x_,x0_,x1_}, opts___?OptionQ] :=  
	PlotDerivative[fun,{x,x0,x1}, 1, opts]
	
PlotDerivative[fun_, {x_,x0_,x1_},derivatives:_Integer?Positive | {_Integer?Positive...},opts___?OptionQ] :=
	Block[ {print, plotfun, derivs, dstyle, pstyle},
	newfun=Inactivate[fun];
	{print, zerotonull, plotfun} = {PrintDisplay, ZeroToNull, PlotFunction} /. {opts} /. Options[PlotDerivative];
	derivs = If[Head[derivatives] === List, Flatten[derivatives], Range[derivatives]];
	If[((DerivativeStyle /. {opts}) === DerivativeStyle)  && Length[Flatten[{fun}]] > 1, 
		If[(PlotStyle /. {opts}) === PlotStyle, 
			pstyle = {Automatic}; dstyle = {Automatic},
			dstyle = setps5[PlotDerivative, DerivativeStyle, Length[Flatten[{fun}]], DerivativeStyle->(PlotStyle/.{opts})]],
		pstyle = setps5[Plot, PlotStyle, Length[Flatten[{fun}]], opts];
		dstyle = setps5[PlotDerivative, DerivativeStyle, Length[derivs] Length[Flatten[{fun}]], opts]];
	derivs = Map[PiecewiseD[fun,{x,#}]&, derivs];
	If[print,
		Print["The function and specified derivatives are:"];
		Print[Flatten[{newfun,derivs}]]];  
	If[plotfun,
		PlotJump[Evaluate[Flatten[{newfun,derivs}]],{x,x0,x1}, PlotStyle->Join[pstyle,dstyle], opts],
		PlotJump[Evaluate[derivs],{x,x0,x1}, PlotStyle->dstyle, opts]]
	]
	
(*commented out on 10-12-17 
PlotDerivative[fun_, {x_,x0_,x1_},derivatives:_Integer?Positive | {_Integer?Positive...},opts___?OptionQ] :=
	Block[ {print, plotfun, derivs, dstyle, pstyle},
	{print, zerotonull, plotfun} = {PrintDisplay, ZeroToNull, PlotFunction} /. {opts} /. Options[PlotDerivative];
	derivs = If[Head[derivatives] === List, Flatten[derivatives], Range[derivatives]];
	If[((DerivativeStyle /. {opts}) === DerivativeStyle)  && Length[Flatten[{fun}]] > 1, 
		If[(PlotStyle /. {opts}) === PlotStyle, 
			pstyle = {Automatic}; dstyle = {Automatic},
			dstyle = setps5[PlotDerivative, DerivativeStyle, Length[Flatten[{fun}]], DerivativeStyle->(PlotStyle/.{opts})]],
		pstyle = setps5[Plot, PlotStyle, Length[Flatten[{fun}]], opts];
		dstyle = setps5[PlotDerivative, DerivativeStyle, Length[derivs] Length[Flatten[{fun}]], opts]];
	derivs = Map[PiecewiseD[fun,{x,#}]&, derivs];
	If[print,
		Print["The function and specified derivatives are:"];
		Print[Flatten[{fun,derivs//Simplify}]]];  (*Added Simplify 10-02-17*)
	If[plotfun,
		PlotJump[Flatten[{fun,derivs}],{x,x0,x1}, PlotStyle->Join[pstyle,dstyle], opts],
		PlotJump[derivs,{x,x0,x1}, PlotStyle->dstyle, opts]]
	]	
*)	
(* commented out on 09-03-17  
PlotDerivative[fun_, {x_,x0_,x1_},derivatives:_Integer?Positive | {_Integer?Positive...},opts___?OptionQ] :=
	Block[ {print, zerotonull, plotfun, derivs, dstyle, pstyle, newfun},
	{print, zerotonull, plotfun} = {PrintDisplay, ZeroToNull, PlotFunction} /. {opts} /. Options[PlotDerivative];
	derivs = If[Head[derivatives] === List, Flatten[derivatives], Range[derivatives]];
	Off[Piecewise::pairs];
	newfun = fun/.If[zerotonull,
						Piecewise[alist_List,0]->Piecewise[Join[alist,{{Null,True}}]],
						Piecewise[alist_List,0]->Piecewise[Join[alist,{{0.,True}}]]];

	On[Piecewise::pairs];
	If[((DerivativeStyle /. {opts}) === DerivativeStyle)  && Length[Flatten[{fun}]] > 1, 
		If[(PlotStyle /. {opts}) === PlotStyle, 
			pstyle = {Automatic}; dstyle = {Automatic},
			dstyle = setps5[PlotDerivative, DerivativeStyle, Length[Flatten[{fun}]], DerivativeStyle->(PlotStyle/.{opts})]],
		pstyle = setps5[Plot, PlotStyle, Length[Flatten[{fun}]], opts];
		dstyle = setps5[PlotDerivative, DerivativeStyle, Length[derivs] Length[Flatten[{fun}]], opts]];
	derivs = Map[PiecewiseD[newfun,{x,#}]&, derivs];
	If[print,
		Print["The function and specified derivatives are:"];
		Print[Flatten[{newfun,Simplify[derivs]}]]];
		If[plotfun,
			PlotJump[Flatten[{newfun,derivs}],{x,x0,x1}, PlotStyle->Join[pstyle,dstyle], opts],
			PlotJump[derivs,{x,x0,x1}, PlotStyle->dstyle, opts]]
	]	
*)

PiecewiseD[fun_, x_, opts___?OptionQ] :=
	Module[{newfun, zerotonull},
	zerotonull = ZeroToNull /. {opts} /. Options[PiecewiseD];
	Off[Piecewise::pairs];
	newfun = If[Head[fun]===Tooltip, fun/.Tooltip[rest_]->rest, fun];
	If[Head[newfun]===Piecewise,	
		newfun = PiecewiseExpand[newfun, Reals]/.If[zerotonull,
						Piecewise[alist_List,0]->Piecewise[Join[alist,{{{},True}}]],  (*Null replaced by {}  Modified 09-3-17 *)
						Piecewise[alist_List,0]->Piecewise[Join[alist,{{0.,True}}]]];
		On[Piecewise::pairs];
		D[newfun,x]/.({} | Indeterminate)->Null,
		Activate[D[Inactivate[fun],x]]]]
(*
PiecewiseD[Tooltip[fun_], rest__] := 
	PiecewiseD[fun, rest] 
	PiecewiseD[fun_, x_] := (Print[here2];
	Activate[D[Inactivate[fun],x]])
*)
(*  Commented out on 09-03-17
PiecewiseD[fun_Piecewise, x_, opts___?OptionQ] :=
	Module[{newfun, zerotonull,derivs,deriv},
	zerotonull = ZeroToNull /. {opts} /. Options[PiecewiseD];

	newfun = PiecewiseExpand[fun, Reals];

	Off[Piecewise::pairs];
	newfun = newfun/.If[zerotonull,
						Piecewise[alist_List,0]->Piecewise[Join[alist,{{{},True}}]],  (*Null replaced by {}  Modified 09-3-17 *)
						Piecewise[alist_List,0]->Piecewise[Join[alist,{{0.,True}}]]];

	(*derivs = D[newfun/.(Null->{}),x];*)  (*Modified 09-3-17 *)
	derivs = D[newfun,x];

	deriv = Simplify[derivs/.({} | Indeterminate)->Null]/.Piecewise[alist_List,0]->Piecewise[Join[alist,{{0.,True}}]];

	On[Piecewise::pairs];
	deriv]
*)

	
	
NewtonMethod[sys:{__Equal}, rest__] := NewtonMethod[sys/.lhs_ == rhs_ -> lhs-rhs, rest]
	
NewtonMethod[sys_List, {x_,x0_}, rest:_List.., (n_)?IntegerQ, opts___?OptionQ] := 
	Module[{wp, print, vars, initial, augmat}, 
	{wp, print} = {WorkingPrecision, PrintDisplay} /. {opts} /. Options[NewtonMethod]; 
	{vars, initial} = Transpose[{{x,x0},rest}]; 
	augmat = AppendColumn[Outer[D,sys,vars],-sys]//Evaluate; 
	If[print, 
		Print[N[#, wp]] ; Thread[vars -> Last[#]], 
		Thread[vars -> Last[#]]]&
			[Chop[NestList[N[#1 + Last[Transpose[RowReduce[augmat /. Thread[vars -> #1]]]], wp] & , 
       			initial, n], 10^(-wp)] ]
	]
	

NewtonMethod[lhs_ == rhs_, rest__]:= Return[Message[Newton::zeros]]

NewtonMethod[fun_, {t_, pt_?(NumberQ[N[#]]&)}, n_Integer, opts___?OptionQ] := 
	Module[{wp, print,f = Function[t, fun]}, 
	{wp, print} = {WorkingPrecision, PrintDisplay} /. {opts} /. Options[NewtonMethod]; 
	If[print, Print[N[#, wp]] ; {t-> Last[#]}, {t-> Last[#]}]&
		[NestList[N[Evaluate[#1 - f[#1]/Derivative[1][f][#1]], wp]& , pt, n] ]
	]

	
PlotNewton[lhs_ == rhs_, rest__]:=
	If[MatchQ[lhs,0]||MatchQ[rhs,0], PlotNewton[lhs-rhs, rest],Return[Message[Newton::zeros]]]

PlotNewton[fun_, {t_, t0_, t1_}, (pt_)?NumericQ, opts___?OptionQ] := 
	Module[{iter, graph, wp, f, intercepts, ptstyle, linestyle, anew, bnew, print}, 
	{iter, graph, wp, print} = {Iterations, DrawGraph, WorkingPrecision, PrintDisplay} /. {opts} /. Options[PlotNewton]; 
	Off[Power::infy, Infinity::indet]; 
	f = Function[t, fun]; 

    intercepts = NestList[N[Evaluate[#1 - f[#1]/Derivative[1][f][#1]], wp]& , pt, iter]; 

    On[Power::infy, Infinity::indet]; 
    If[ Not[VectorQ[intercepts, NumericQ]], Message[PlotNewton::zeroderiv]; Return[intercepts]]; 

    If[graph, 
    	intercepts = Chop[intercepts];
        If[Not[VectorQ[intercepts, Im[#1] == 0 & ]], Return[Message[PlotNewton::imaginary]]];
    	If[print,Print[TableForm[N[intercepts, wp]]]]; 
    	{ptstyle, linestyle} = setps5[PlotNewton, {PointStyle, LineStyle}, iter, opts]; 

    	{anew, bnew} = {Min[N[t0], intercepts], Max[N[t1], intercepts]}; 
    	intercepts = Partition[Flatten[({{#1, 0}, {#1, Chop[f[#1]]}} & ) /@ intercepts, 1], 3, 2]; 
      	Show[PlotJump[fun, {t, anew, bnew}, opts(*Added opts and Removed the rest 7-5-11, Evaluate[Sequence@@FilterRules[{opts}, {Options[PlotJump], Options[Plot]}]]*)],
       		Graphics[MapThread[Flatten[{#1, #3, Line[#2]}] & , {Table[Hue[(0.8/iter)*j], {j, 0, iter - 1}], intercepts, linestyle}]], 
       		Graphics[MapThread[Flatten[{#1, #3, Point /@ #2}] & , {Table[Hue[(0.8/iter)*j], {j, 0, iter - 1}], intercepts,ptstyle}]],
			FilterRules[{opts},Options[Graphics]]], 
	If[print, Print[TableForm[N[intercepts, wp]]]];
    {t -> Last[intercepts]}]
    ]

	
BisectionMethod[lhs_ == rhs_, rest__] := Return[Message[Newton::zeros]]

BisectionMethod[fun_, {x_, x1_, x2_}, n_, opts___] := 
	Module[{f , fx1, fx2, test, wp, wpp, print, fc, data}, 
	{wp, print} = {WorkingPrecision, PrintDisplay} /. {opts} /. Options[BisectionMethod];
	If[wp === MachinePrecision, 
		wpp = Ceiling[$MachinePrecision], 
		wpp = wp];
	f := N[Function[x, fun], wp];
	{fx1,fx2} = Chop[{f[x1],f[x2]}, 10^-wp];
	If[fx1 fx2 > 0, 
		Return[Message[BisectionMethod::oppositesign,fx1,fx2]], 
		Which[
			fx1 == 0,Return[{x -> x1}], 
			fx2 == 0, Return[{x -> x2}]];
		test[{a_, b_, fa_, fb_}] := 
			With[{c = (a + b)/2}, 
				fc = Chop[f[c],10^-wp];
				Which[
					fc == 0, {c,c,c,c}, 
					fa*fc < 0, {a, c, fa, fc}, 
					True,{c, b, fc, fb}]]];
	data = NestList[N[test[#], wp]&, {x1, x2, fx1, fx2}, n];
	If[print, Print[NumberForm[Part[#,{1,2}]& /@ data,wpp]]];
	{x -> Apply[Plus,Last[data][[{1,2}]]]/2}
	]

SecantMethod[lhs_ == rhs_, rest__] := Return[Message[Newton::zeros]]	

SecantMethod[fun_, {x_, x1_, x2_}, n_, opts___?OptionQ] := 
	Module[{f , fx1, fx2, test, wp, wpp, print, fc, data}, 
	{wp, print} = {WorkingPrecision, PrintDisplay} /. {opts} /. Options[BisectionMethod];
	If[wp === MachinePrecision, wpp = Ceiling[$MachinePrecision], wpp = wp];
	f := N[Function[x, fun],wp]; 
	{fx1,fx2} = Chop[{f[x1],f[x2]},10^-wp];
	If[fx1 fx2 > 0, 
		Return[Message[BisectionMethod::oppositesign,fx1,fx2]], 
		Which[fx1 == 0, Return[{x -> x1}], fx2 == 0, Return[{x -> x2}]];    
		test[{a_, b_, fa_, fb_}] := 
			With[{c = a - fa*((b - a)/(fb - fa))}, 
				fc = Chop[f[c],10^-wp]; 
				Which[fc == 0, Throw[{{c,c,c,c}}], fa*fc < 0, {a, c, fa, fc}, True,{c, b, fc, fb}]]];
	data = Catch[NestList[N[test[#], wp]&, N[{x1, x2, fx1, fx2}, wp], n]];
	If[print, Print[NumberForm[Part[#,{1,2}]& /@ data,wpp]]];
	{x -> Apply[Plus,Last[data][[{1,2}]]]/2}
]

(*Changed code for FillingStyle: 7-13-11*)
PlotIntegral[fun_List, rest__] := 
	Return[Message[PI::onefunction]]

PlotIntegral[fun_, {x_, x0_, x1_}, opts___?OptionQ] := 
	Module[{istyle, pstyle, fillingstyle, print, integral, singularity, newfun, nx0, nx1,endpts, data},
	print = PrintDisplay /. {opts} /. Options[PlotIntegral];
	{llimit,rlimit}={Limit[fun,x->x0,Direction->-1,PowerBehavior->Complex],Limit[fun,x->x1,Direction->1,PowerBehavior->Complex]};
	data = {Right, Left}[[Flatten[Position[Evaluate[{llimit,rlimit}]//N,_Real]]]];
	Which[
		data === {}, Return[Message[PlotIntegral::twosingularities]],
		Length[data]==1, singularity=First[data],
		Length[data]==2, singularity=Right
		];
	{pstyle,fillingstyle} = {PlotStyle, FillingStyle} /. {opts} /. Options[Plot];
	If[N[x0]>N[x1], {nx0,nx1} = {x1,x0}; newfun = -fun, {nx0,nx1} ={x0,x1}; newfun = fun];
	integral = Integrate[newfun, If[MatchQ[singularity, Right], {x, nx0, x}, {x,x,nx1}]//Evaluate, PowerBehavior->Complex,
				Sequence@@FilterRules[{opts},Options[Integrate]]//Evaluate, Assumptions -> {nx0 <= x <= nx1, Element[x,Reals]},
				GenerateConditions->False]//Simplify;(*12/22-17 added Simplify *)(*Removed 12-07-15*) 
	If[Not[FreeQ[integral, Integrate]], Return[Message[PlotIntegral::noint]]];
	If[print,
			If[MatchQ[singularity, Right], 
				Print["The integral of f(x) = ",fun," from ",x0," to ",x," is F(x) = ",integral ], 
				Print["The integral of f(x) = ",fun," from ",x," to ",x1," is F(x) = ",integral ]]];
	istyle = setps5[PlotIntegral, IntegralStyle, 1, opts];
	Show[PlotJump[{newfun(*,0*)}//Evaluate, {x, nx0, nx1}, Filling->Axis, PlotStyle -> pstyle, opts, FillingStyle-> {Yellow, Orange}],
	   PlotJump[integral//Evaluate, {x, nx0, nx1}, PlotStyle ->  istyle, opts], Sequence@@FilterRules[{opts},Options[Graphics]],PlotRange->Automatic]
	   ]



PlotNIntegral[fun_, {x_, x0_, x1_}, opts___?OptionQ] := 
	Module[{istyle, pstyle, print, integral, singularity, newfun, nx0, nx1,endpts,data},
	print = PrintDisplay /. {opts} /. Options[PlotIntegral];
	
	endpts=RealPower[fun/.PointsToRules[{x0,x1},x]];
	data={Right, Left}[[Flatten[Position[endpts // N, _Real]]]];
	Which[
		data === {}, Return[Message[PlotIntegral::twosingularities]],
		Length[data]==1, singularity=First[data],
		Length[data]==2, singularity=Right
		];
	
	pstyle = PlotStyle /. {opts} /. Options[Plot];
	If[N[x0]>N[x1], {nx0,nx1} = {x1,x0}; newfun = -fun, {nx0,nx1} ={x0,x1}; newfun = fun];
	If[MatchQ[singularity, Right], 
				integral = y[x] /. NDSolve[{y'[x] == newfun, y[nx0] == 0}, y[x], {x, nx0, nx1}],
				integral = -y[x] /. NDSolve[{y'[x] == newfun, y[nx1] == 0}, y[x], {x, nx0, nx1}]];

	istyle = setps5[PlotIntegral, IntegralStyle, 1, opts];	
	Show[PlotJump[newfun//Evaluate, {x, nx0, nx1}, Filling->{1->Axis}, PlotStyle -> pstyle, opts, FillingStyle->{Red, Yellow}],
	   Plot[integral//Evaluate, {x, nx0, nx1}, PlotStyle -> istyle, FilterRules[{opts},Options[Plot]]//Evaluate],
	   Sequence@@FilterRules[{opts},Options[Graphics]],PlotRange->Automatic]
	   ]


SurfaceOfRevolution[fun_,{x_,x0_,x1_}, trange___, opts___?OptionQ]:=
	Module[{orientation, linepoint, revolaxis, drawaxis, axislength, axeslabel, drawregion, axisstyle, curvestyle, drawvolume, rrange, funl, funll,  
		alabel, prange, delta, line, line2, bdry1, bdry2, vec1, vec2, side1, side2, side3, side4, mainplot, newcurve, facestyle, regionstyle},
If[Not[FreeQ[{opts},RevolutionAxis]], Message[obsolete::RevolutionAxis];Return[]];
	{orientation, linepoint, revolaxis, drawaxis, axislength, axeslabel, drawregion, axisstyle, drawvolume} = 
		{Orientation, LinePoint, AxisOfRevolution, DrawAxis, AxisLength, AxesLabel, DrawRegion, AxisStyle, DrawVolume} /. {opts} /. Options[SurfaceOfRevolution];
	{facestyle, curvestyle, regionstyle} = setps5[SurfaceOfRevolution, {FaceStyle, CurveStyle, RegionStyle}, 1, opts];
	rrange = If[MatchQ[{trange}, {}], {Theta, 0, 2 Pi}, ReplacePart[trange, 1->Theta]];
	revolaxis = Normalize[N[revolaxis]];
	If[Not[MatchQ[revolaxis,{0.,a_,0.}]] && MatchQ[orientation, Textbook],Message[surfofrevol::dui];orientation=false];
(*	If[MatchQ[revolaxis, {0.,0.,a_}], Return[Message[surfofrevol::notallowed]]];*)
	If[MatchQ[revolaxis, {0.,0.,a_}], drawaxis = False];  (*Added 02-21-17*)
	linepoint = If[Last[RowReduce[{revolaxis,linepoint}]] == {0,0,0}, {0,0,0},linepoint];
	funll = If[Head[fun]===List, fun, {fun}];
(*	If[Not[FreeQ[funll,Equal | Less | Greater]], Return[Message[ sor::inputerror]]];*)
	If[(drawvolume || drawregion) && Length[funll]>2, Message[surfofrevol::notallowed1]; drawvolume = False; drawregion = False];

	If[orientation === KnoxPackages`Calculus`Textbook,
		funl = {x,0,#}& /@ funll; 
		linepoint = {linepoint[[1]],0,0};
		revolaxis = {0,0,1}; 
		alabel = {Style["x",Medium,Blue],Style["z",Medium,Blue],Style["y",Medium,Blue]},
		funl = {x,#,0}& /@ funll;
		alabel = None(*{Style["x",Medium,Blue],Style["y",Medium,Blue],Style["z",Medium,Blue]}*)];

	funl = (# - linepoint)& /@ funl;

	mainplot = 	
		If[Norm[linepoint]==0,
			RevolutionPlot3D[funl//Evaluate,{x,x0,x1}, rrange//Evaluate, RevolutionAxis->revolaxis,
				Sequence@@FilterRules[{opts},Options[RevolutionPlot3D]]//Evaluate, (*PlotRange->All,*) AxesLabel->If[MatchQ[axeslabel,False | None],None,alabel]],
			Graphics3D[GeometricTransformation[RevolutionPlot3D[funl//Evaluate,{x,x0,x1}, rrange//Evaluate, RevolutionAxis->revolaxis,
				Sequence@@FilterRules[{opts},Options[RevolutionPlot3D]]//Evaluate][[1]],TranslationTransform[linepoint]],AxesLabel->If[MatchQ[axeslabel,False | None],None,alabel],
				Sequence@@FilterRules[{opts},Options[Graphics3D]]//Evaluate, Axes -> True, (*PlotRange->All,*) BoxRatios->Automatic]];
		prange = PlotRange /. AbsoluteOptions[ParametricPlot3D[funl,{x,x0,x1},FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate], PlotRange];

		delta = .15*axislength;

		Which[
			orientation === Textbook,
				{bdry1,bdry2} = {linepoint + (x0 - linepoint[[1]]){Cos[Theta],Sin[Theta],0}+{0,0,y}, linepoint + (x1 - linepoint[[1]]){Cos[Theta],Sin[Theta],0}+{0,0,y}};
				{vec1,vec2} = {{0,0,prange[[3,1]]},{0,0,prange[[3,2]]}},
			revolaxis == {0,1.,0}||revolaxis == {0,0,1.}, 
				{bdry1,bdry2} = {linepoint + (x0-linepoint[[1]]){Cos[-Theta],0,Sin[-Theta]}+{0,y,0}, linepoint + (x1-linepoint[[1]]){Cos[-Theta],0,Sin[-Theta]}+{0,y,0}};
				{vec1,vec2} = {{0,prange[[2,1]],0},{0,prange[[2,2]],0}},
			revolaxis == {1.,0,0},
				{bdry1,bdry2} = {{x0, (x-linepoint[[2]]) Cos[Theta], (x-linepoint[[2]]) Sin[Theta]} +linepoint, {x1, (x-linepoint[[2]]) Cos[Theta], (x-linepoint[[2]]) Sin[Theta]}+linepoint};
				{vec1,vec2} = {{x0,0,0},{x1,0,0}},
			True,
				{vec1,vec2} = (ProjectVector[#, {revolaxis}]&/@ Transpose[prange])
				];
		If[drawaxis, 
			line = Graphics3D[Flatten[{axisstyle,Line[{vec1 + linepoint - delta revolaxis, vec2 + linepoint + delta revolaxis}]}]],
			line = Graphics3D[{}]];

	Which[
		Length[funll]==2 (*&& (drawregion || Not[N[rrange]==N[{Theta,0,2 Pi}]]*), 
			If[drawregion && And @@ Map[MatrixQ[#, NumericQ[#] &] &,(funl/.{{x->x0},{x->x1}})], (*Changed 01-23-17*)
				line2 = Graphics3D[Flatten[{curvestyle,Line[(# + linepoint)&/@(funl/.x->x0)],Line[(# + linepoint)&/@(funl/.x->x1)]}]],
				line2 = Graphics3D[{}]];
			newcurve = 
				Plot[fun//Evaluate,{x,x0,x1}, PlotStyle->curvestyle, Filling->{1->{2}}, (*PlotRange->If[MatchQ[orientation,Textbook],prange[[{1,3}]],prange[[{1,2}]]],*)(* 01-17-17 Changed uncommenting of plotrange stuff *)
						FillingStyle-> Directive[Flatten[regionstyle]], Sequence@@FilterRules[{opts}, Options[PlotJump]]//Evaluate];
			newcurve = 
				Show[{Graphics3D[Replace[If[$VersionNumber >= 10.4, newcurve[[1,1]], newcurve[[1]]], (*Change from newcurve[[1]] to newcurve[[1,1]] required in 10.4 *)
					If[orientation === KnoxPackages`Calculus`Textbook, {a_, b_} -> {a,0,b}, {a_, b_} -> {a, b, 0}], {2}]],line, line2},
						Sequence@@FilterRules[{opts},Options[Graphics3D]]//Evaluate, PlotRange->All, Axes->True, AxesLabel->If[MatchQ[axeslabel,False | None],None,alabel]],
		Length[funll]==1 && drawregion, 	
			newcurve = 
				Show[{ParametricPlot3D[(# + linepoint)& /@ funl,{x,x0,x1},FilterRules[{opts}, Options[ParametricPlot3D]]//Evaluate, PlotStyle->curvestyle], line}, 
					Sequence@@FilterRules[{opts},Options[Graphics3D]]//Evaluate,PlotRange->All, AxesLabel->If[MatchQ[axeslabel,False | None],None,alabel]],
		True,
			newcurve = line2 = Graphics3D[{}]];

Off[Infinity::indet];
	If[Length[funll]==2 && drawvolume,  (*Changed 01-23-17*)
		Which[
			revolaxis == {0,1.,0}||revolaxis == {0,0,1.},
				side1 = If[MatchQ[Subtract@@(funl/.x->x0), {0,0,0}], Graphics3D[{}],
					If[And@@Map[VectorQ[#, NumericQ[#] &] &,(funl/.{x->x0})],
						ParametricPlot3D[bdry1//Evaluate, rrange//Evaluate,{y,Min[fun/.x->x0],Max[fun/.x->x0]}, PlotStyle->facestyle,
							Sequence@@FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate, Mesh->None],
						Graphics3D[{}]]];
				side2 = If[Subtract@@(funl/.x->x1) == {0,0,0}, Graphics3D[{}],
					If[And@@Map[VectorQ[#, NumericQ[#] &] &,(funl/.{x->x1})],
						ParametricPlot3D[bdry2//Evaluate,rrange//Evaluate,{y,Min[fun/.x->x1],Max[fun/.x->x1]}, PlotStyle->facestyle,
							Sequence@@FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate, Mesh->None], 
						Graphics3D[{}]]],
			revolaxis == {1.,0,0},
				side1 = If[MatchQ[Subtract@@(funl/.x->x0), {0,0,0}], Graphics3D[{}],
					If[And@@Map[VectorQ[#, NumericQ[#] &] &,(funl/.{x->x0})],
						ParametricPlot3D[bdry1//Evaluate, rrange//Evaluate, Flatten[{x,fun/.x->x0}]//Evaluate, PlotStyle->facestyle,
							Sequence@@FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate, Mesh->None],
						Graphics3D[{}]]];
				side2 = If[MatchQ[Subtract@@(funl/.x->x1), {0,0,0}], Graphics3D[{}],
					If[And@@Map[VectorQ[#, NumericQ[#] &] &,(funl/.{x->x1})],
						ParametricPlot3D[bdry2//Evaluate, rrange//Evaluate, Flatten[{x,(fun/.x->x1)}]//Evaluate,  PlotStyle->facestyle,
							Sequence@@FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate, Mesh->None], 
						Graphics3D[{}]]],
			True,
				side1 = If[MatchQ[Subtract@@(funl/.x->x0), {0,0,0}], Graphics3D[{}],
					If[And@@Map[VectorQ[#, NumericQ[#] &] &,(funl/.{x->x0})],
						Graphics3D[GeometricTransformation[
							RevolutionPlot3D[{x0,t,0}-linepoint//Evaluate,Flatten[{t,Sort[fun/.x->x0]}]//Evaluate,rrange//Evaluate, RevolutionAxis->revolaxis,
								PlotStyle->LightRed,
								Sequence@@FilterRules[{opts},Options[RevolutionPlot3D]]//Evaluate,Mesh->None][[1]],TranslationTransform[linepoint]]],
						Graphics3D[{}]]];
				side2 = If[MatchQ[Subtract@@(funl/.x->x1), {0,0,0}], Graphics3D[{}],
					If[And@@Map[VectorQ[#, NumericQ[#] &] &,(funl/.{x->x1})],
						Graphics3D[GeometricTransformation[
							RevolutionPlot3D[{x1,t,0}-linepoint//Evaluate,Flatten[{t,Sort[fun/.x->x1]}]//Evaluate, rrange//Evaluate, RevolutionAxis->revolaxis,PlotStyle->LightRed,
								Sequence@@FilterRules[{opts},Options[RevolutionPlot3D]]//Evaluate,Mesh->None][[1]],TranslationTransform[linepoint]]],
						Graphics3D[{}]]]
			];
		If[N[rrange]==N[{Theta,0,2 Pi}],
			side3 = side4 = Graphics3D[{}],
			side3 = Graphics3D[GeometricTransformation[newcurve[[1]], RotationTransform[trange[[2]], revolaxis, linepoint]]];
			side4 = Graphics3D[GeometricTransformation[newcurve[[1]], RotationTransform[trange[[3]], revolaxis, linepoint]]]],
		side1=side2=side3=side4=Graphics3D[{}]];
On[Infinity::indet];

(* In 11.3, GraphicsRow causes many problems so it has been commented out *)
	Which[
		drawregion && Length[funll]==2, 
			(*GraphicsRow[{newcurve, Show[{line, mainplot, side1, side2, side3, side4}, Sequence@@FilterRules[{opts},Options[Graphics3D]]//Evaluate, 
				Axes -> True, AxesLabel->If[MatchQ[axeslabel,False | None],None,alabel]]}, Sequence@@FilterRules[{opts},Options[GraphicsRow]]],*)
			{Show[newcurve], Show[{line, mainplot, side1, side2, side3, side4}, Sequence@@FilterRules[{opts},Options[Graphics3D]]//Evaluate, 
				Axes -> True, AxesLabel->If[MatchQ[axeslabel,False | None],None,alabel]]},	
		drawaxis,
			Show[{line, mainplot, side1, side2, side3, side4}, FilterRules[{opts},Options[Graphics3D]]//Evaluate, 
				Axes -> True, AxesLabel->If[MatchQ[axeslabel,False | None],None,alabel]],
		True,
			Show[{mainplot, side1, side2, side3, side4}, FilterRules[{opts},Options[Graphics3D]]//Evaluate, 
				Axes -> True, AxesLabel->If[MatchQ[axeslabel,False | None],None,alabel]]
			(*mainplot*)]
]


ParaSurfaceOfRevolution[fun_,{x_,x0_,x1_}, trange___, opts___?OptionQ]:=
	Module[{orientation, linepoint, revolaxis, drawaxis, axislength, axeslabel, drawregion, axisstyle, curvestyle, drawvolume, rrange, funl, funll,  
		alabel, prange, delta, line, line2, vec1, vec2, mainplot, newcurve, regionstyle},
	If[Not[FreeQ[{opts},RevolutionAxis]], Message[obsolete::RevolutionAxis];Return[]];
	{orientation, linepoint, revolaxis, drawaxis, axislength, axeslabel, drawregion, axisstyle, curvestyle, drawvolume, regionstyle} = 
		{Orientation, LinePoint, AxisOfRevolution, DrawAxis, AxisLength, AxesLabel, DrawRegion, AxisStyle, CurveStyle, DrawVolume, RegionStyle} /. {opts} /. Options[SurfaceOfRevolution];
	rrange = If[MatchQ[{trange}, {}], {Theta, 0, 2 Pi}, ReplacePart[trange, 1->Theta]];
	revolaxis = Normalize[N[revolaxis]];
	If[Not[MatchQ[revolaxis,{0.,a_,0.}]] && MatchQ[orientation, Textbook],Message[surfofrevol::dui];orientation=false];
(*	If[MatchQ[revolaxis, {0.,0.,a_}], Return[Message[surfofrevol::notallowed]]];*)
	If[MatchQ[revolaxis, {0.,0.,a_}], drawaxis = False];  (*Added 02-21-17*)
	linepoint = If[Last[RowReduce[{revolaxis,linepoint}]] == {0,0,0}, {0,0,0},linepoint];
	funll = If[Head[fun[[1]]]===List, fun, {fun}];

	If[drawvolume && Length[funll]>2, Message[surfofrevol::notallowed1]; drawvolume = False];
	If[orientation === Textbook,
		funl = Flatten[{#[[1]],0,#[[2]]}]& /@ funll;
		linepoint = {linepoint[[1]],0,0};
		revolaxis = {0,0,1}; 
		alabel = {Style["x",Medium,Blue],Style["z",Medium,Blue],Style["y",Medium,Blue]},
		funl = Flatten[{#,0}]& /@ funll;
		alabel = {Style["x",Medium,Blue],Style["y",Medium,Blue],Style["z",Medium,Blue]}];
	funl = (# - linepoint)& /@ funl;

	mainplot = 	
		If[Norm[linepoint]==0,
			RevolutionPlot3D[funl//Evaluate,{x,x0,x1}, rrange//Evaluate, RevolutionAxis->revolaxis,
				Sequence@@FilterRules[{opts},Options[RevolutionPlot3D]]//Evaluate, (*PlotRange->All,*) AxesLabel->If[MatchQ[axeslabel,False | None],None,alabel]],
			Graphics3D[GeometricTransformation[RevolutionPlot3D[funl//Evaluate,{x,x0,x1}, rrange//Evaluate, RevolutionAxis->revolaxis,
				Sequence@@FilterRules[{opts},Options[RevolutionPlot3D]]//Evaluate][[1]],TranslationTransform[linepoint]],AxesLabel->If[MatchQ[axeslabel,False | None],None,alabel],
				Sequence@@FilterRules[{opts},Options[Graphics3D]]//Evaluate, Axes -> True(*, PlotRange->All,*)]];
	
		prange = PlotRange/.AbsoluteOptions[ParametricPlot3D[funl,{x,x0,x1},Sequence@@FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate],PlotRange];

		delta= .15*axislength;
		Which[
			orientation === Textbook,
				{vec1,vec2} = {{0,0,prange[[3,1]]},{0,0,prange[[3,2]]}},
			revolaxis == {0,1.,0}||revolaxis == {0,0,1.}, 
				{vec1,vec2} = {{0,prange[[2,1]],0},{0,prange[[2,2]],0}},
			revolaxis == {1.,0,0},
				{vec1,vec2} = {{First[prange[[1]]],0,0},{Last[prange[[1]]],0,0}},
			True,
				{vec1,vec2} = (ProjectVector[#, {revolaxis}]&/@ Transpose[prange])
				];
		If[drawaxis, 
			line = Graphics3D[Flatten[{axisstyle,Line[{vec1 + linepoint - delta revolaxis, vec2 + linepoint + delta revolaxis}]}]],
			line = Graphics3D[{}]];
		
	closeregionstyle = setps5[PlotBand, CurveStyle, 2, opts];	
	closeregionstyle = DeleteCases[closeregionstyle, Tube[_],2];

	data=(# +  linepoint)& /@ funl;
	closeregion = Graphics3D[Flatten/@Transpose[{closeregionstyle,Line/@(data/.{{x->x0},{x->x1}})}]];

	If[drawregion && Length[funll]==2, (* Added length condition, 08-24-17. Changed ParametricPlot3D to PlotBand 02-23-17*)
		newcurve = Show[{PlotBand[Evaluate[(# + linepoint)& /@ funl],{x,x0,x1}, Sequence@@FilterRules[{opts}, Options[PlotBand]]//Evaluate], line,closeregion}, 
					Sequence@@FilterRules[{opts},Options[Graphics3D]]//Evaluate,PlotRange->All, AxesLabel->If[MatchQ[axeslabel,False | None],None,alabel]],
		newcurve = line2 = Graphics3D[{}]];	
	
	If[N[rrange]==N[{Theta,0,2 Pi}],
		side3 = side4 = Graphics3D[{}],
		side3 = Graphics3D[GeometricTransformation[newcurve[[1]], RotationTransform[trange[[2]], revolaxis, linepoint]]];
		side4 = Graphics3D[GeometricTransformation[newcurve[[1]], RotationTransform[trange[[3]], revolaxis, linepoint]]],
		side3 = side4 = Graphics3D[{}]];

(* Need to add code for side1 and side2 in addition side3 and side4; 02-24-17*)
(*
Print[data/.{{x->x0},{x->x1}}];
Print[side1 = Graphics3D[GeometricTransformation[
							RevolutionPlot3D[{Cos[2/3],t,0}-linepoint//Evaluate,{t,0.61837,1.23674},rrange//Evaluate, RevolutionAxis->revolaxis,
								PlotStyle->LightRed,
								Sequence@@FilterRules[{opts},Options[RevolutionPlot3D]]//Evaluate,Mesh->None][[1]],TranslationTransform[linepoint]],Axes->True]];
*)



	Which[
		drawregion && Length[funll]==2, (*08-24-17 Added length condition *)
			GraphicsRow[{newcurve, 	Show[{line, mainplot, (*side1,*) side3, side4}, Sequence@@FilterRules[{opts},Options[Graphics3D]]//Evaluate, Axes -> True]},
				Sequence@@FilterRules[{opts},Options[Graphics3D]]],
		drawaxis,
			Show[{line, mainplot, side3, side4}, Sequence@@FilterRules[{opts},Options[Graphics3D]]//Evaluate, Axes -> True],
		True,
			Show[{mainplot, side3, side4}]]
]
   

NewPolarPlot[r_, {t_,t0_,t1_}, opts___?OptionQ] := 
	polarPlot[r, {t,t0,t1}, PolarPlot, opts]

DirPolarPlot[r_, {t_,t0_,t1_}, opts___?OptionQ] := 
	polarPlot[r, {t,t0,t1}, DirParametricPlot, opts]

(*Editted 05-30-17*)
polarPlot[r_,{t_,t0_,t1_}, ptype_, opts___?OptionQ] := 
	Module[{t, rays, rstyles, rlist, funl, curve, lines},
	{rays, rstyles} = {PolarRay, PolarRayStyle} /. {opts} /. Options[DirPolarPlot];
	{rlist, rays, rstyles} = Map[Flatten[{#}]&,{r, rays, rstyles}];
	If[Not[VectorQ[rays,NumericQ]],Return[Message[PolarPlot::ray]]];

	funl = Map[# {Cos[t], Sin[t]}&, rlist];
	curve = Which[
		MatchQ[ptype,PolarPlot],
			ptype[rlist//Evaluate (*funl//Evaluate*),{t,t0,t1},  
				Sequence@@FilterRules[{opts},Options[PolarPlot]]//Evaluate, AspectRatio -> Automatic],
		MatchQ[ptype,DirParametricPlot],
			DirParametricPlot[funl//Evaluate,{t,t0,t1},
				Sequence@@FilterRules[{opts},Options[DirParametricPlot]]//Evaluate, AspectRatio -> Automatic],
		True,
			Return[]];

	If[rays =!= {},
		rstyles = setps5[DirPolarPlot, PolarRayStyle, Length[rays] Length[rlist], opts];
		lines = Flatten[Map[funl /. t -> #&, rays],1];
		lines = MapThread[Flatten[{#1,Line[{{0,0},#2 }]}]&,{rstyles, lines}],
		lines = {}];
	Show[curve, Graphics[lines], Sequence@@FilterRules[{opts}, Options[Graphics]], PlotRange->All ]
	]
	

ParaPlotArea[fun_, {t_,t0_,t1_}, opts___?OptionQ]:=
	ppArea[fun, {t,t0,t1}, ParametricPlot (*DirParametricPlot*), opts]

PolarPlotArea[fun_, {t_,t0_,t1_}, opts___?OptionQ]:=
	ppArea[Flatten[{fun}], {t,t0,t1}, PolarPlot (*DirPolarPlot*), opts]
	
ppArea[fun_, {t_,t0_,t1_}, ptype_, opts___?OptionQ]:=
	Module[{nt0 = N[t0], nt1 = N[t1], between, areastyle, linestyle, gr, joined, list,prange},
	{between, joined(*, asize, rays*)} = {Between, Joined(*, ArrowSize, PolarRay*)} /. {opts} /. Options[PolarPlotArea];
	between = If[between === Automatic, {{nt0, nt1}}, between//N];
	between = If[MatchQ[between, {{_,_}..}], between, {between}];
	{areastyle,linestyle} = setps5[ParaPlotArea, {FillingStyle,LineStyle}, Length[between], opts];

(*If[Not[VectorQ[Flatten[{rays}],NumericQ]],Return[Message[PolarPlot::ray]]];*)
	gr = ptype[fun,{t,nt0,nt1},
			Sequence@@FilterRules[{opts},Options[PolarPlot (*DirPolarPlot*)]]//Evaluate,
        	Sequence@@FilterRules[{opts},Options[ParametricPlot]]//Evaluate, PlotRange -> All(*, ArrowSize->asize*)](*//Normal*);

	list = Map[
		If[nt0 < #[[1]] || #[[2]] < nt1,
			Cases[ptype[fun,{t,#[[1]],#[[2]]}]//Normal,Line[n_] -> n,Infinity],
			Cases[gr//Normal,Line[n_] -> n,Infinity]  ]&, between];  (*Changed *)

	list = 
		If[MatchQ[ptype, DirPolarPlot | PolarPlot (*DirPolarPlot*)] && Length[Flatten[{fun}]]==1,
			Join[{{0,0}}, Flatten[#,1]]& /@ list,
			If[MatchQ[ptype, DirParametricPlot | ParametricPlot ] && Length[Flatten[{fun}]]==2,
				Map[
					If[(#[[1,1,1]] != First[Last[First[#]]]),
						Join[{{#[[1,1,1]],0}}, Flatten[#,1],{{First[Last[First[#]]],0}}],
						Flatten[#,1]]&, list],
				If[joined, Flatten[{#[[1]],Reverse[#[[2]]]},1]& /@ list, list]]];
	list = Show[
		Graphics[MapThread[Flatten[{EdgeForm[#1], #2,Polygon[#3]}]& , {linestyle, areastyle, list} ]], 	gr, 
			Sequence@@FilterRules[{opts},Options[Graphics]]//Evaluate, Axes -> True];
	If[Head[prange = PlotRange /. {opts} /. PlotRange -> All] === List && First[prange] === Full, 
		prange = {All, Last[prange]}];
	Show[list, PlotRange->prange, Sequence@@FilterRules[{opts},Options[Graphics]], AspectRatio -> Automatic,PlotRangePadding->Scaled[.04] ]
	]
	
(*  Commented out (*03-26-16*)
ppArea[fun_, {t_,t0_,t1_}, ptype_, opts___?OptionQ]:=
	Module[{nt0 = N[t0], nt1 = N[t1], between, areastyle, linestyle, gr, joined, list, prange, asize, rays},
	{between, joined, asize, rays} = {Between, Joined, ArrowSize, PolarRay} /. {opts} /. Options[PolarPlotArea];
	between = If[between === Automatic, {{nt0, nt1}}, between//N];
	between = If[MatchQ[between, {{_,_}..}], between, {between}];
	{areastyle,linestyle} = setps5[ParaPlotArea, {FillingStyle,LineStyle}, Length[between], opts];

	If[Not[VectorQ[Flatten[{rays}],NumericQ]],Return[Message[PolarPlot::ray]]];
	gr = ptype[fun,{t,nt0,nt1},(*PlotStyle->ColorData[97],*) 
			Sequence@@FilterRules[{opts},Options[DirPolarPlot]]//Evaluate,
        	Sequence@@FilterRules[{opts},Options[ParametricPlot]]//Evaluate, PlotRange -> All, 
        	DrawArrowheads -> False, ArrowSize->asize](*//Normal*);

	list = Map[
		If[nt0 < #[[1]] || #[[2]] < nt1,
			Cases[ptype[fun,{t,#[[1]],#[[2]]}]//Normal,Line[n_] -> n,Infinity],
			Cases[gr//Normal,Line[n_] -> n,Infinity]  ]&, between];  (*Changed *)


	list = 
		If[MatchQ[ptype, DirPolarPlot] && Length[Flatten[{fun}]]==1,
			Join[{{0,0}}, Flatten[#,1]]& /@ list,
			If[MatchQ[ptype, DirParametricPlot | ParametricPlot ] && Length[Flatten[{fun}]]==2,
				Map[
					If[(#[[1,1,1]] != First[Last[First[#]]]),
						Join[{{#[[1,1,1]],0}}, Flatten[#,1],{{First[Last[First[#]]],0}}],
						Flatten[#,1]]&, list],
				If[joined, Flatten[{#[[1]],Reverse[#[[2]]]},1]& /@ list, list]]];


	list = Show[
		Graphics[MapThread[Flatten[{EdgeForm[#1], #2,Polygon[#3]}]& , {linestyle, areastyle, list} ]], 	gr, 
			Sequence@@FilterRules[{opts},Options[Graphics]]//Evaluate, Axes -> True];
	If[Head[prange = PlotRange /. {opts} /. PlotRange -> All] === List && First[prange] === Full, 
		prange = {All, Last[prange]}];
	Show[list, PlotRange->prange, Sequence@@FilterRules[{opts},Options[Graphics]], AspectRatio -> Automatic,PlotRangePadding->Scaled[.04] ]
	]
*)



	
PlotArcLengthApprox[{f_,g_},{t_,tmin_,tmax_,m_},opts___?OptionQ] := 
	plotarclength[{f,g},{t,tmin,tmax,m},ParametricPlot,Graphics,opts]

PlotArcLengthApprox3D[{f_,g_,h_(*,style___*)},{t_,tmin_,tmax_,m_},opts___?OptionQ] := 
	plotarclength[{f,g,h(*,style*)},{t,tmin,tmax,m},ParametricPlot3D,Graphics3D,opts]
	
PlotArcLengthApprox[fun_,{t_,tmin_,tmax_,m_},opts___?OptionQ] := 
	plotarclength[{t,fun},{t,tmin,tmax,m},ParametricPlot,Graphics,opts]	
	
plotarclength[fun_,{t_,tmin_,tmax_,m_},plottype_,grtype_,opts___?OptionQ] := 
	Block[{graph,ls,ptstyle,funl,data,linelen,plot,pts,lines, print},
	{graph, print} = {DrawGraph, PrintDisplay} /. {opts} /. Options[PlotArcLengthApprox];
	{ptstyle, ls} = setps5[PlotArcLengthApprox, {PointStyle, LineStyle}, 1, opts];
	funl = If[Length[fun] == 4, Take[fun,3],fun];
	data = Table[funl,{t,tmin,tmax,(tmax - tmin)/m}//N//Evaluate];
	(*linelen = NArcLength[data];*)
	linelen = ArcLength[Line[data]];
	
	If[graph,
		If[print, Print[linelen]];
		plot = plottype[fun,{t,tmin,tmax},	Sequence@@FilterRules[{opts}, Options[plottype]]//Evaluate];
		pts = Flatten[{ptstyle,Point /@ data}];
		lines = Flatten[{ls,Line[data]}];
		Show[plot,grtype[{lines,pts}],
			Sequence@@FilterRules[{opts}, Options[grtype]]],
		linelen]
]



PlotVector[points : {{_, _}, {_, _}}?MatrixQ, opts___?OptionQ] :=
	Module[{bvec},
	bvec = BoundVector /. {opts} /. Options[PlotVector];
	If[bvec,
		PlotVector[Map[{{0,0},#}&, points], BoundVector -> False, opts],
		PlotVector[{points}, opts]]]

PlotVector[{a_,b_}?VectorQ, opts___] := 
	PlotVector[{{{0,0},{a,b}}}, BoundVector -> False, opts]

PlotVector[points_?MatrixQ, opts___?OptionQ] :=
	PlotVector[Map[{{0,0},#}&, points], BoundVector -> False, opts]
	
PlotVector[points : {{{_, _}, {_, _}} ..}, (opts___)?OptionQ] := 
	Module[{vs,  asize, bvec, length = Length[points], pts = points, aheads},
	{vs, asize, bvec, aheads} = {VectorStyle, ArrowSize, BoundVector, DrawArrowheads} /. {opts} /. Options[PlotVector]; 
	asize = If[Head[asize] === Symbol, asize, .0375*asize];
	If[bvec, 
		pts = Map[{#[[1]],#[[1]]+#[[2]]}&, points]
		];
	If[Length[Flatten[pts]] == 4 && length > 1, length = 1; pts = {pts}]; 
	vs = setps5[PlotVector, VectorStyle, length, opts];
	Show[
		If[aheads,
			MapThread[
				Graphics[Flatten[{#1, Arrowheads[asize], Arrow[#2]}]] &, {vs,  pts}], 
			MapThread[
				Graphics[Flatten[{#1, Line[#2]}]] &, {vs, pts}]],
   		Sequence@@FilterRules[{opts}, Options[Graphics]], Axes -> True,PlotRange->All]]		




NewPlotVector[vec : {_,_}?VectorQ, opts___?OptionQ] :=
	NewPlotVector[{{vec,{0,0}}}, opts]
	
(*NewPlotVector[dirpt : {{_, _},{_,_}}?MatrixQ, opts___?OptionQ] :=
	NewPlotVector[{dirpt}, opts]*)

NewPlotVector[vecs_?MatrixQ, opts___?OptionQ] :=
	NewPlotVector[Map[{#,{0,0}}&, vecs], opts]

NewPlotVector[dirpoints : {{{_, _}, {_, _}} ..}, (opts___)?OptionQ] := 
	Module[{vs,  asize, bvec, length = Length[dirpoints], pts = dirpoints, aheads},
	{vs, asize, bvec, aheads} = {VectorStyle, ArrowSize, BoundVector, DrawArrowheads} /. {opts} /. Options[NewPlotVector]; 
	asize = If[Head[asize] === Symbol, asize, .0375*asize];
	If[bvec, 
		pts = Map[{#[[2]],#[[1]]+#[[2]]}&, dirpoints]];
	If[Length[Flatten[pts]] == 4 && length > 1, length = 1; pts = {pts}]; 
	vs = setps5[PlotVector, VectorStyle, length, opts];
	Show[
		If[aheads,
			MapThread[
				Graphics[Flatten[{#1, Arrowheads[asize], Arrow[#2]}]] &, {vs, pts}], 
			MapThread[
				Graphics[Flatten[{#1, Line[#2]}]] &, {vs, pts}]],
   		Sequence@@FilterRules[{opts}, Options[Graphics]], Axes -> True,PlotRange->All]]
  
NewPlotVector[directions_?MatrixQ, initialpts_?MatrixQ, opts___?OptionQ] :=	
	If[Length[directions]==Length[initialpts],
		NewPlotVector[Transpose[{directions,initialpts}],opts],
		Return[Message[PV::wrongsize]]]
	
NewPlotVector[directions : {{_, _}...}?MatrixQ, initialpt_?VectorQ, opts___?OptionQ] :=	
	Module[{initialpts = Table[initialpt,Length[directions]]},
	NewPlotVector[Transpose[{directions,initialpts}],opts]]
	
(*NewPlotVector[vec_?VectorQ, initialpt_?VectorQ, opts___?OptionQ] :=	
	NewPlotVector[{vec},initialpt,opts]*)


NewPlotVector3D[vec : {_,_,_}?VectorQ, opts___?OptionQ] :=
	NewPlotVector3D[{{vec,{0,0,0}}}, opts]
	
NewPlotVector3D[dirpt : {{_,_,_},{_,_,_}}?MatrixQ, opts___?OptionQ] :=
	NewPlotVector3D[{dirpt}, opts]

NewPlotVector3D[vecs_?MatrixQ, opts___?OptionQ] :=
	NewPlotVector3D[Map[{#,{0,0,0}}&, vecs], opts]

NewPlotVector3D[dirpoints : {{{_,_,_}, {_,_,_}} ..}, (opts___)?OptionQ] := 
	Module[{vs,  asize, bvec, length = Length[dirpoints], pts = dirpoints, aheads},
	{vs, asize, bvec, aheads} = {VectorStyle, ArrowSize, BoundVector, DrawArrowheads} /. {opts} /. Options[NewPlotVector]; 
	asize = If[Head[asize] === Symbol, asize, .03*asize];
	
	If[bvec, 
		pts = Map[{#[[2]],#[[1]]+#[[2]]}&, dirpoints]];
	If[Length[Flatten[pts]] == 4 && length > 1, length = 1; pts = {pts}]; 
	vs = setps5[PlotVector, VectorStyle, length, opts];
	Show[
		If[aheads,
			MapThread[
				Graphics3D[Flatten[{#1, Arrowheads[asize], Arrow[#2]}]] &, {vs, pts}], 
			MapThread[
				Graphics3D[Flatten[{#1, Line[#2]}]] &, {vs, pts}]],
   		Sequence@@FilterRules[{opts}, Options[Graphics3D]], Axes -> True,PlotRange->All]]
  
NewPlotVector3D[vecs_?MatrixQ, initialpts_?MatrixQ, opts___?OptionQ] :=	
	If[Length[vecs]==Length[initialpts],
		NewPlotVector3D[Transpose[{vecs,initialpts}],opts],
		Return[Message[PV::wrongsize]]]
	
NewPlotVector3D[vecs_?MatrixQ, initialpt_?VectorQ, opts___?OptionQ] :=	
	Module[{initialpts = Table[initialpt,Length[vecs]]},
	NewPlotVector3D[Transpose[{vecs,initialpts}],opts]]
	
NewPlotVector3D[vec_?VectorQ, initialpt_?VectorQ, opts___?OptionQ] :=	
	NewPlotVector3D[{vec},initialpt,opts]	
	
	
	
	
	
	
PlotProjection[vec_?VectorQ, spanset:{_,_} | {{_,_}}, opts___] := 
	If[{Length[vec],Length[Flatten[spanset]]} != {2,2}, Return[Message[VectorSize::wrongsize,2]],
	PlotProjection[{vec}, spanset, opts]]

PlotProjection[vec_List, spanset:{_,_} | {{_,_}}, opts___] := 
	Module[{nvec = N[vec], vstyle, projstyle, dstyle, blength, bstyle, bpt, ptstyle, proj, plotrange, 
						aspectratio, asize, lines, aorigin, basis, print, projsize, baseline, drawbasevec, dline, blstyle},
	If[Union[Length /@ nvec, {Length[Flatten[spanset]]}] != {2}, Return[Message[VectorSize::wrongsize,2]]];
	{baseline, blength, bpt, aspectratio, asize, aorigin, drawbasevec, print, plotrange, dline} = 
		{DrawBaseLine, BaseLength, BasePoint, AspectRatio, ArrowSize, AxesOrigin, DrawBaseVector, PrintDisplay, PlotRange, DashedLine} 
						/. {opts} /. Options[PlotProjection];

	asize = If[Head[asize]===Symbol, asize, .055 asize];

	{vstyle, projstyle, dstyle, bstyle, blstyle, ptstyle} = 
		setps5[PlotProjection, {VectorStyle,ProjectionStyle,DashedLineStyle, BaseVectorStyle, BaseLineStyle, PointStyle}, 
			Length[nvec], opts];
	basis = Normalize[Flatten[{spanset}]];
	proj = Projection[#,basis]& /@ vec;
	
	If[print, Print["The projection(s) are ",proj]];

	projsize = Max[{Norm /@ proj,1}];
	
	baseline = If[baseline === False, 
		{}, (*If[drawbasevec,{bpt, bpt + Flatten[spanset]},{}],*)
		Flatten[{bpt - .35 projsize blength Normalize[#], bpt + # + .35 projsize blength Normalize[#]}& /@ proj,1]]; 

	
	aorigin = If[aorigin === Automatic, {}, Point[aorigin]];	
	lines = If[dline,Graphics[MapThread[Flatten[{#3, Line[{bpt + #1,bpt + #2}]}]&,{proj,nvec,dstyle} ]],Graphics[{}]];
	If[baseline === False,
		Show[lines,
			Graphics[MapThread[{Flatten[{vstyle, Arrowheads[asize], Arrow[{bpt, bpt + #2}]}],Flatten[{projstyle, Arrowheads[asize], Arrow[{bpt, bpt + #1}]}]}&,{proj,nvec}]],
			If[drawbasevec, 
				Graphics[{First[bstyle],Arrowheads[asize],Arrow/@{{bpt, bpt + Flatten[spanset]}}}],
				Graphics[{}]],		
			Graphics[Flatten[{ptstyle, Point[bpt]}]], 
				PlotRange -> plotrange, AspectRatio -> aspectratio, 
				Sequence@@FilterRules[{opts}, Options[Graphics]], Axes -> True],
		
		Show[Graphics[Flatten[{blstyle,Line[baseline]}]], lines,	
			Graphics[MapThread[{Flatten[{vstyle, Arrowheads[asize], Arrow[{bpt, bpt + #2}]}],Flatten[{projstyle, Arrowheads[asize], Arrow[{bpt, bpt + #1}]}]}&,{proj,nvec}]],
			If[drawbasevec, 
				Graphics[{First[bstyle],Arrowheads[asize],Arrow/@{{bpt, bpt + basis}}}],			
				Graphics[{}]],
			Graphics[Flatten[{ptstyle,Point[bpt]}]],
				PlotRange -> plotrange, AspectRatio -> aspectratio,  
			Sequence@@FilterRules[{opts}, Options[Graphics]], Axes -> True]
		]
	]	


PlotReflection[vec_?VectorQ, spanset:{_,_} | {{_,_}}, opts___] := 
	If[{Length[vec],Length[Flatten[spanset]]} != {2,2}, Return[Message[VectorSize::wrongsize,2]],
	PlotReflection[{vec}, Flatten[spanset], opts]]

PlotReflection[vec_List,spanset:{_,_} | {{_,_}}, opts___] := 
    PlotProjection[Join[vec,(2*Projection[#1, Flatten[spanset]] - #1 & ) /@ vec ], spanset, PrintDisplay->False, opts]


(* 9-16-13 Changed Plot to PlotJump *)
PlotTaylorPoly[fun_, {x_, x0_,x1_}, center_, degrees_, opts___?OptionQ] :=
	Block[{print, ptstyle, polystyle, deg, plot, serieslist},
	print = PrintDisplay /. {opts} /. Options[PlotTaylorPoly];
	If[Not[N[x0 <= center <= x1]], Message[InRange::notinrange, center,{x0,x1}]];
	deg = If[Head[degrees] === List, degrees, {degrees}];
	{ptstyle, polystyle} = setps5[PlotTaylorPoly, {PointStyle, PolyStyle}, {1, Length[deg]}, opts];	
	plot = Plot[fun, {x,x0,x1}, Sequence@@FilterRules[{opts}, Options[Plot]]//Evaluate];  (*08-03-17 Changed PlotJump to Plot *)
	serieslist = Map[TaylorPoly[fun, {x, center}, #, Sequence @@ FilterRules[{opts},Options[TaylorPoly]]//Evaluate]&, deg];
	If[MemberQ[serieslist, "Indeterminate", Infinity], Return[Message[PTP::notp,serieslist]]];
	If[print,
		Print["The function and its Taylor polynomial(s) of degree(s) ",degrees," centered at ",center," is (are):"];
		Print[Flatten[{fun,serieslist}]]];

	Show[
		plot,
		PlotJump[serieslist, {x,x0,x1}, PlotStyle -> polystyle, Sequence@@FilterRules[{opts}, Options[PlotJump]]//Evaluate, PowerBehavior->Real],
		Graphics[Flatten[{ptstyle, Point[{center, NewLimit[fun, x -> center]}]}]],
			Sequence@@FilterRules[{opts}, Options[Graphics]],
			Options[plot,PlotRange]]
	]/; NumberQ[N[center]]
	
 PlotTaylorPoly3D[fun_, {x_, x0_, x1_}, {y_, y0_, y1_}, {c1_, c2_}?VectorQ, deg_Integer?Positive, opts___Rule] := 
	Block[{print, ptstyle, tp, polystyle, plot, plotrange},
	{print, ptstyle, polystyle} = {PrintDisplay, PointStyle, PolyStyle} /. {opts} /. Options[PlotTaylorPoly3D];

	{polystyle, ptstyle} = setps5[PlotTaylorPoly3D, {PolyStyle, PointStyle}, 1, Sequence[PolyStyle -> {polystyle}, PointStyle -> {ptstyle}]];
	tp = TaylorPoly[fun,{x,c1},{y,c2}, deg, opts];

	If[MemberQ[{tp}, "Indeterminate", Infinity], Return[Message[PTP::notp,{tp}]]];
	If[print, Print[tp]];
	plot = Plot3D[fun, {x,x0,x1}, {y,y0,y1}, Sequence@@FilterRules[{opts},Options[Plot3D]]//Evaluate];
	plotrange = PlotRange /. AbsoluteOptions[plot, PlotRange];

	Show[
		plot, 
		Plot3D[tp, {x,x0,x1}, {y,y0,y1}, PlotStyle -> Reverse[polystyle], 
			Sequence@@FilterRules[{opts},Options[Plot3D]]//Evaluate, (*Mesh -> None,*) PlotRange->plotrange, ClippingStyle->None], 
		Graphics3D[Flatten[{ptstyle,Point[{c1,c2,Fold[Limit,fun,{x -> c1,y -> c2}]}]}]],
			Sequence@@FilterRules[{opts},Options[Graphics3D]]]
	]	

TaylorPoly[fun_, {x_, x0_, n_}, opts___?OptionQ] := 
	Normal[Series[fun, {x, x0, n}, PowerBehavior -> Complex]]

TaylorPoly[fun_, {x_, x0_}, n_, opts___?OptionQ] := 
	Normal[Series[fun, {x, x0, n}, PowerBehavior -> Complex]] 
	
TaylorPoly[fun_,{x_, x0_}, rest__List, order_, opts___?OptionQ]:=
	Module[{series, limit, vars, centers, orders, data},
	{series, limit} = {UseSeries, UseLimit}/.{opts}/.Options[TaylorPoly];
	If[MatchQ[Head[fun],Piecewise] || MatchQ[series, False],
		taylorPoly[fun, {x,x0}, rest, order, opts], 
		{vars,centers} = Transpose[{{x,x0}, rest}];
		orders = Map[Flatten[{#,order}]&,Join[{{x,x0},rest}]];
		data = Normal[Series[fun, Sequence@@orders]];
		Select[Expand[data], Apply[Plus, Exponent[#, vars]] <= order &]
		]
	]
	
taylorPoly[ fun_, vlists:{_,_},rest___List, order_?(IntegerQ[#] && Not[Negative[#]]&), opts___?OptionQ] :=
	Block[{limit, vars, centers, its, mostits, itlist, ts},
	If[Not[FreeQ[Flatten[{vlists,rest}],Subscript]], Return[Message[TaylorPoly::subscript]]];
	limit = UseLimit /. {opts} /. Options[TaylorPoly];
 	{vars,centers} = Transpose[{vlists, rest}];
 	its = Array[a, Length[vars]];
 	mostits = Drop[its,-1];
 	itlist = Append[ Map[{#,0,order}&, mostits], {Last[its],0, order - Plus @@ mostits} ];
	If[limit,
    		ts = Sum[Fold[NewLimit,D[fun, Sequence @@ Transpose[{vars, its}]],
							First[PointsToRules[centers,vars]]]*Times @@ ((vars - centers)^its/its!), 
								Evaluate[Sequence @@ itlist]];
			If[MemberQ[ts,Infinity | Null,Infinity],Return["Indeterminate"],ts],
			Sum[(D[fun, Sequence @@ Transpose[{vars, its}]] /.
    			First[PointsToRules[centers,vars]])*Times @@ ((vars - centers)^its/its!), 
     			Evaluate[Sequence @@ itlist]]]
     		]			
	
	
	
	
(* Needs work
TaylorPoly[fun_, {x_, x0_}, rest__List, order_, opts___?OptionQ] := 
	Module[{limit, vars, centers, orders, data}, 
	limit = UseLimit /. {opts} /. Options[TaylorPoly];
(*	If[MatchQ[Head[fun], Piecewise], limit = True];*)
	If[MatchQ[Head[fun], Piecewise] || limit ,
		{vars, centers} = Transpose[{{x, x0}, rest}];
		its = Array[a, Length[vars]];
		mostits = Drop[its, -1];
		itlist = Append[Map[{#, 0, order} &, mostits], {Last[its], 0, order - Plus @@ mostits}];
		If[limit,
			ts = Sum[Fold[NewLimit, D[fun, Sequence @@ Transpose[{vars, its}]], 
				First[PointsToRules[centers, vars]]]*Times @@ ((vars - centers)^its/its!),
					Evaluate[Sequence @@ itlist]];
			If[MemberQ[ts, Infinity | Null, Infinity], Return["Indeterminate"], ts], 
			Sum[(D[fun, Sequence @@ Transpose[{vars, its}]] /. First[PointsToRules[centers, vars]])*Times @@ ((vars - centers)^its/its!), 
				Evaluate[Sequence @@ itlist]]],
		{vars, centers} = Transpose[{{x, x0}, rest}]; 
		Print[limit];
		orders = Map[Flatten[{#, order}] &, Join[{{x, x0}, rest}]]; 
		data = Normal[Series[fun, Sequence @@ orders]]; 
		Select[Expand[data], Apply[Plus, Exponent[#, vars]] <= order &]]
	]			
*)

TrapError[fun_, {x_, a_, b_, n_}, opts___?OptionQ] :=
	Module[{evalue},
	evalue = Exact /. {opts} /. Options[TrapError];
	If[evalue,
		MaxValue[{Abs[D[fun, {x, 2}]], a <= x <= b}, x] (b - a)^3/(12 n^2),
		NMaxValue[{Abs[D[fun, {x, 2}]], a <= x <= b}, x] (b - a)^3/(12 n^2)]
	]
	
MidpointError[fun_, {x_, a_, b_, n_}, opts___?OptionQ] :=
	Module[{evalue},
	evalue = Exact /. {opts} /. Options[MidpointError];
	If[evalue,
		MaxValue[{Abs[D[fun, {x, 2}]], a <= x <= b}, x] (b - a)^3/(24 n^2),
		NMaxValue[{Abs[D[fun, {x, 2}]], a <= x <= b}, x] (b - a)^3/(24 n^2)]
	]
	
SimpsonError[fun_, {x_, a_, b_, n_}, opts___?OptionQ] :=
	Module[{evalue},
	If[OddQ[n],Return[Message[SimpError::even,n]]];
	evalue = Exact /. {opts} /. Options[SimpsonError];
	If[evalue,
		MaxValue[{Abs[D[fun, {x, 4}]], a <= x <= b}, x] (b - a)^5/(180 n^4),
		Simplify[NMaxValue[{Abs[D[fun, {x, 4}]], a <= x <= b}, x] (b - a)^5/(180 n^4),Assumptions->n>0]]
	]


PlotRegression[points:{__} ,fun_, var_Symbol | {var_Symbol}, opts___?OptionQ] :=
	Block[{ptstyle, linestyle, showdiff, prange, pts, graphpts, function, line, perline, print},
	{ptstyle, linestyle, showdiff, prange, print} = 
		{PointStyle, LineStyle, ShowDifference, PlotRange, PrintDisplay} /.{opts}/. Options[PlotRegression];
	pts = If[MatchQ[points,{{_,_}..}], points, Transpose[{Range[Length[points]], points}]];
	graphpts = Show[Graphics[Flatten[{ptstyle, Point /@ pts}]], PlotRange -> prange];
	prange = PlotRange/.AbsoluteOptions[graphpts,PlotRange];

	function = Fit[points, fun, var];
	If[print,
		Print["The least-squares fit is: "];
		Print[function]];

	line = Plot[function//Evaluate, {var, prange[[1]]}//Flatten//Evaluate, 
			Sequence@@FilterRules[{opts}, Options[Plot]]//Evaluate];
	perline = If[ showdiff, 
		Graphics[Flatten[{linestyle, Line /@ Transpose[
			{pts, Map[ {#, function/.{var -> #}}&, Transpose[pts][[1]]]}]}]],
		Graphics[{}]];
    			
	Show[line, graphpts, perline, Sequence@@FilterRules[{opts}, Options[Graphics]], AspectRatio -> Automatic, 
		PlotRange -> All] 
	]
(**********************************************************************)






PlotPlane[eqn:_Equal | {__Equal}, {x_, x0_, x1_}, {y_, y0_, y1_}, opts___?OptionQ] := 
	Module[{vars,zvar,eqnl,funz,surf,eqn1,ysects,xsects,vsects, cstyle}, 

(*	If[Not[And @@ (PolynomialQ[#,{x,y}]& /@ Flatten[{eqn}])],Return @ Message[PlotPlane::notpoly]];
	If[Max @@ (PolynomialDegree[#,{x,y}]& /@ Flatten[{eqn}]) > 1,Return @ Message[PlotPlane::polydegree]];
*)	
	vars = Variables[N[eqn /. (a_) == (b_) -> a - b]]; 
	zvar = Complement[vars, {x, y}]; 
	eqnl = Flatten[{eqn}];
	If[zvar === {}, 
		{funz, surf, eqn1} = {{x}, False, eqnl},
		zvar = First[zvar]; 
		{funz, surf} = {Select[eqnl, MemberQ[#1, zvar, Infinity] & ], True};
		eqn1 = Complement[eqnl, funz]; 
		funz = Flatten[(zvar /. Solve[#1, zvar] & ) /@ funz]]; 
	ysects = Complement[eqn1, Select[eqn1, MemberQ[#1, x, Infinity] & ]]; 
	xsects = Complement[eqn1, Select[eqn1, MemberQ[#1, y, Infinity] & ]]; 
    vsects = Complement[eqn1, Join[xsects, ysects]]; 
    {xsects, ysects, vsects} = Flatten /@ 
    	{(x /. Solve[#1, x] & ) /@ xsects, 
		 (y /. Solve[#1, y] & ) /@ ysects, 
       	 (y == First[y /. Solve[#1, y]] & ) /@ vsects}; 
	cstyle = PlotStyle /. {opts}/.Options[ParaPlotSection];
	ParaPlotSection[{x,y,#1}& /@ funz, {x,x0,x1},{y,y0,y1},
		SectionVariables->{x,y}, XSection -> xsects,
		VSection -> vsects, YSection -> ysects, Surface -> surf, CylinderStyle -> cstyle, opts,Mesh->None, Axes->Automatic]]

PlotPlane[{},{x_,x0_,x1_},{y_,y0_,y1_},opts__Rule] :=
	PlotPlane[x,{x,x0,x1},{y,y0,y1}, Surface -> False, SectionVariables -> {x,y}, opts]

PlotPlane[fun_,{x_,x0_,x1_},{y_,y0_,y1_},opts___?OptionQ] :=
	Block[{x,y,funl = Flatten[{fun}]},

	If[Not[And @@ (PolynomialQ[#,{x,y}]& /@ funl)],Return @ Message[PlotPlane::notpoly]];
	If[Max @@ (PolynomialDegree[#,{x,y}]& /@ funl) > 1,Return @ Message[PlotPlane::polydegree]];
	ParaPlotSection[Map[{x,y,#}&,funl]//Evaluate,{x,x0,x1},{y,y0,y1},
		SectionVariables -> {x,y}, opts]  ]


Plot3DArray[fun_, {x_,xmin_,xmax_}, {y_,ymin_,ymax_}, gun___List, trange___List, opts___?OptionQ] :=
	Block[{tickstyle,p3D, cp, dp, pptsc},
	tickstyle = TicksStyle /. {opts} /. TicksStyle -> Automatic;
	{p3D, cp, dp, pptsc, imsize} =
		 {Plot3D, ContourPlot, DensityPlot, PlotPointsContour, ImageSize} /. {opts} /. Options[Plot3DArray];
	Show[GraphicsGrid[{DeleteCases[
		{If[p3D,
			PlotSection[fun,{x, xmin, xmax},{y, ymin, ymax}, gun, trange, opts]],
		If[cp,
			ContourPlotSection[fun,{x, xmin, xmax},{y, ymin, ymax}, gun, trange, PlotPoints->pptsc, FrameTicksStyle->tickstyle, opts]],
		If[dp,
			DensityPlot[fun,{x, xmin, xmax}, {y, ymin, ymax}, Sequence@@FilterRules[{opts}, Options[DensityPlot]]//Evaluate]]},
		Null]}, PlotRange -> Automatic, Sequence@@FilterRules[{opts}, Options[GraphicsGrid]], ImageSize->imsize]]  (*04-19-16  Set ImageSize *)
]



DirParametricPlot[fun_, {t_, t0_, t1_}, opts___?OptionQ] := 
	DPP[fun,{t,t0,t1},2,opts]//Quiet
	
DirParametricPlot3D[fun_, {t_, t0_, t1_}, opts___?OptionQ] := 
	DPP[fun,{t,t0,t1},3,opts]//Quiet


	
DPP[fun_, {t_, t0_, t1_}, flag_, opts___?OptionQ] := 
	Module[{x,y,z,funl, fp, aheads, asize, anumber, color, pp, gr, vars, arrow, data, dt = N[t1 - t0], 
		ps, psdata, dpts, plotfun, plotrange}, 
	funl = ReleaseHold[Hold[fun] /. {Integrate -> NIntegrate, Tooltip[a_]->a}]//Quiet;
	If[VectorQ[funl],funl = ReleaseHold[{Hold[funl]}]];
	If[Head[funl] === Piecewise, funl = {funl}];
	If[MatrixQ[First[funl]], funl = Flatten[funl,1]];
	If[Not[MatrixQ[funl/.t->t0]] & , Return[Message[dpp::improperinput]]];

	Switch[flag,
			2, 
				pp = ParametricPlot; 
				gr = Graphics;
				vars = {x,y};
				arrow = (Arrow[#]&),
			3, 
				pp = ParametricPlot3D;
				gr = Graphics3D;
				vars = {x,y,z};
				arrow = (Arrow[#]&)];

	color = ColorFunction /. {opts} /. Options[DirParametricPlot];
	If[MatchQ[color, Automatic], color = Function[Flatten[{vars,t}]//Evaluate, Hue[.8 t]]];

	{aheads, (*asize,*) anumber} = 
		{DrawArrowheads, (*ArrowSize,*) ArrowNumber} /. {opts} /. Options[DirParametricPlot];		
	If[anumber <= 0, aheads = False];
(*	If[MatchQ[flag,2],
		{ps,asize} = setps5[DirParametricPlot, {PlotStyle, ArrowSize}, Length[funl], opts],
		{ps,asize} = setps5[DirParametricPlot3D, {PlotStyle, ArrowSize}, Length[funl], opts]];
*)

	ps = setps5[DirParametricPlot, PlotStyle, Length[funl], opts];
		
	If[MatchQ[flag,2],
		asize = ArrowSize /. {opts} /. Options[DirParametricPlot];
		asize = If[Head[asize]===Symbol, asize, .1 asize] ,
		asize = ArrowSize /. {opts} /. Options[DirParametricPlot3D];
		asize = If[Head[asize]===Symbol, asize, .03 asize]];

	If[Not[FreeQ[ps, RGBColor[_,_,_] | Hue[_] | GrayLevel[_]]], (*Message[dpp::pscolor];*) color = None];

	psdata = {
			RGBColor[0.368417, 0.506779, 0.709798], RGBColor[0.880722, 0.611041, 0.142051], RGBColor[0.560181, 0.691569, 0.194885], 
			RGBColor[0.922526, 0.385626, 0.209179], RGBColor[0.528488, 0.470624, 0.701351], RGBColor[0.772079, 0.431554, 0.102387], 
			RGBColor[0.363898, 0.618501, 0.782349], RGBColor[1, 0.75, 0], RGBColor[0.647624, 0.37816, 0.614037], RGBColor[0.571589, 0.586483, 0.], 
			RGBColor[0.915, 0.3325, 0.2125], RGBColor[0.40082222609352647`, 0.5220066643438841, 0.85], 
			RGBColor[0.9728288904374106, 0.621644452187053, 0.07336199581899142], RGBColor[0.736782672705901, 0.358, 0.5030266573755369], 
			RGBColor[0.28026441037696703`, 0.715, 0.4292089322474965]
			};
	ps = MapThread[Flatten[{#1,#2}]&,{psdata[[1;;Length[funl]]],ps}];

	aheads = 
		If[aheads,
			fp = D[funl, t];
			If[FreeQ[fp, Derivative, Infinity], 			
				If[color === None, 
					ps1 = DeleteCases[ ps, Thickness[_]|Tube[_], Infinity]; (* allowing Thickness or Tube in Arrowheads causes a display problem *)
					data = Table[
						arrow /@ Transpose[{funl, funl + .001 Map[Normalize[#, Reals] &, fp]}//Chop], {t, t0 + dt/anumber, t1, dt/(anumber+.0001)}];
					data = Map[MapThread[Flatten[{Arrowheads[Last[Flatten[{#2}]]],#1,#3}]&,{ps1,asize,#}]&,data],				
					data = Table[
						{color[Sequence@@Flatten[{vars,(t - t0)/dt}]], Arrowheads[Last[Flatten[{asize}]]], 
							arrow /@ Transpose[{funl, funl + .001 Map[Normalize[#, Reals] &, fp]}//Chop]}, 
										{t, t0 + dt/anumber, t1, dt/(anumber + .0001)}]]//Quiet; 
			Select[data, FreeQ[#, Indeterminate, Infinity] &, Infinity],
			Message[dpp::noderiv];{}],{}];

	plotfun = pp[funl//Evaluate, {t, t0, t1}, ColorFunction -> color, PlotStyle -> ps,
				Sequence @@ FilterRules[{opts}, Options[pp]] // Evaluate];


	plotrange = PlotRange /. AbsoluteOptions[plotfun,PlotRange];
	dpts = DrawPoint /. {opts} /. Options[DirParametricPlot3D];
	If[VectorQ[dpts,NumericQ]&& Not[MatchQ[dpts,{}]], dpts = {dpts}];
	{dpts, plotrange} = setdp[DirParametricPlot, dpts, plotrange, opts];
	If[flag == 2, dpts = Graphics[dpts], dpts = Graphics3D[dpts]];

	Show[	
			plotfun, 
			gr[aheads], dpts, PlotRange-> plotrange,
				Sequence@@FilterRules[{opts},Options[gr]]//Evaluate, PlotRangePadding->Scaled[.04]]
	]








PlotVector3D[points : {{_, _, _}, {_, _, _}}?MatrixQ, opts___?OptionQ] :=
	Module[{bvec},
	bvec = BoundVector /. {opts} /. Options[PlotVector3D];
	If[bvec === True,
		PlotVector3D[Map[{{0,0,0},#}&, points], BoundVector -> False, opts],
		PlotVector3D[{points}, opts]]]

PlotVector3D[{a_,b_,c_}?VectorQ, opts___] := PlotVector3D[{{{0,0,0},{a,b,c}}}, BoundVector -> False, opts]

PlotVector3D[points_?MatrixQ, opts___?OptionQ] :=
	PlotVector3D[Map[{{0,0,0},#}&, points], BoundVector -> False, opts]
	
PlotVector3D[points : {{_, _, _}, {_, _, _}} | {{{_, _, _}, {_, _, _}} ..}, (opts___)?OptionQ] := 
   Module[{vs, asize, bvec, length = Length[points], pts = points}, 
    {vs, asize, bvec} = {VectorStyle, ArrowSize, BoundVector} /. {opts} /. Options[PlotVector3D]; 
	asize = If[Head[asize]===Symbol, asize, .035 asize];	
    If[bvec, pts = ({#1[[1]], #1[[1]] + #1[[2]]} & ) /@ points]; 
	If[Length[Flatten[pts]] == 6 && length > 1, length = 1; pts = {pts}]; 
    vs = setps5[PlotVector3D, VectorStyle, length, opts]; 
Graphics3D[MapThread[
		Flatten[{#1, Arrowheads[asize], Arrow[#2]}] & , {vs, pts}], 
			Sequence @@ FilterRules[{opts}, Options[Graphics3D]], Axes -> True, PlotRange->All]]






PlotProjection3D[vec_?VectorQ, spanset_List, opts___] := 
	If[Length[vec] != 3, Return[Message[VectorSize::wrongsize,3]],
		PlotProjection3D[{vec},spanset, opts]]

PlotProjection3D[vec_List,spanset_List,opts___] := 
	Module[{proj, bpt, basis, baseline, bline, blength, bstyle, ptstyle, vstyle, projstyle, dstyle, 
			plane, ppts, psize, plotrange, asize, drawbasevecs, print, blstyle, projsize, lines},
	basis = If[Length[spanset] === 3 && NumberQ[N[spanset[[1]]]], {spanset}, spanset, dline];
	If[Union[Flatten[{Length /@ basis, Length /@ vec}]] != {3}, Return[Message[VectorSize::wrongsize,3]]];	
	{baseline, blength, bpt, plane, ppts, psize, plotrange, asize, drawbasevecs, print, dline} = 
		{DrawBaseLine, BaseLength, BasePoint, DrawPlane, PlotPoints, PlaneSize, PlotRange, 
			ArrowSize, DrawBaseVectors, PrintDisplay, DashedLine} 
							/. {opts} /. Options[PlotProjection3D];
	asize = If[Head[asize]===Symbol, asize, .04 asize];	

	basis = If[Length[spanset] === 3 && NumberQ[N[spanset[[1]]]], {spanset}, spanset];
	{vstyle, projstyle, dstyle, bstyle, blstyle, ptstyle} = 
		setps5[PlotProjection3D,
			{VectorStyle, ProjectionStyle, DashedLineStyle, BaseVectorStyle, BaseLineStyle, PointStyle}, 
				Length[vec],opts];
				
	planestyle = setps5[PlotProjection3D, PlaneStyle, 1, opts];

	basis = Orthogonalize[basis];
	proj = Map[ProjectVectorON[#, basis]&,vec];
	If[print, Print["The projection(s) are: ", proj]];
	projsize = Max[Norm /@ proj];

	bline = 
		Which[
			baseline === False && Length[basis] == 1, {{bpt, bpt + Flatten[basis]}},
			Length[basis] == 1, 
				{bpt - .25 blength projsize blength Normalize[#], bpt + # + .25 blength projsize blength Normalize[#]}& /@ proj,
			baseline === False, {bpt, bpt + #}& /@ (Normalize /@ spanset),
			baseline === True, Return @ Message[PlotProjection3D::wrongop]];

		
	lines = If[dline, 
			Graphics3D[MapThread[Flatten /@ {{dstyle, Line[{bpt + #1, bpt + #2}]}, 
				If[baseline || Not[plane], {blstyle, Line /@ bline},
					If[drawbasevecs, {bstyle, Line /@ bline},{}]],
				{ptstyle, Point[bpt]}}&,  {proj, vec}],
				Axes->Automatic],
			Graphics3D[{}]];

	If[Length[basis] == 1,
		Show[lines,
			Graphics3D[MapThread[{Flatten[{vstyle, Arrowheads[asize], Arrow[{bpt, bpt + #2}]}],Flatten[{projstyle, Arrowheads[asize], Arrow[{bpt, bpt + #1}]}]}&,{proj,vec}]],
			If[drawbasevecs, 
				If[Not[baseline], 
					Graphics3D[Flatten[{First[bstyle],Arrowheads[asize],Arrow/@bline}]],
					Graphics3D[{First[bstyle],Arrowheads[asize],Arrow/@{{bpt, bpt + First[basis]}}}]],
				Graphics3D[{}]],		
			Sequence@@FilterRules[{opts},Options[Graphics3D]]//Evaluate, Axes -> Automatic],

	 	If[plane,
	 		If[psize === Automatic, psize =  projsize, psize = psize projsize];
			plane = ParametricPlot3D[(bpt + Plus@@({a,b} basis))//Evaluate,
							{a, - psize, psize},{b, - psize, psize}, PlotStyle -> planestyle,
							Sequence@@FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate, Mesh->None],
			plane = Graphics3D[{}];
			drawbasevecs = True];
			
		Show[lines, plane,
			Graphics3D[MapThread[
				{Flatten[{vstyle, Arrowheads[asize], Arrow[{bpt, bpt + #2}]}],
				Flatten[{projstyle, Arrowheads[asize], Arrow[{bpt, bpt + #1}]}]}&,{proj,vec}]],
			If[drawbasevecs,
				Graphics3D[MapThread[{
					Flatten[{vstyle, Arrowheads[asize], Arrow[{bpt, bpt + #2}]}],
					Flatten[{projstyle, Arrowheads[asize], Arrow[{bpt, bpt + #1}]}],
					Flatten[{Arrowheads[asize],Arrow/@ bline}]}&,{proj,vec}]],
				Graphics3D[{}]],
				Sequence@@FilterRules[{opts},Options[Graphics3D]], Axes -> Automatic]
		]
	]


PlotReflection3D[(vec_)?VectorQ, spanset_List, opts___] := 
	If[Length[vec] != 3, Return[Message[VectorSize::wrongsize, 3]], 
		PlotReflection3D[{vec}, spanset, opts]]
 
PlotReflection3D[vec_?MatrixQ, spanset_List, opts___] := 
	PlotProjection3D[Join[vec,(2*ProjectVector[#1, spanset] - #1 & ) /@ vec ], spanset, PrintDisplay->False, opts]

(* needs to be upgraded to handle piecewise functions*)
PlotTV[fun:{_,_}|{_,_,_}(*fun_List*), {t_, t0_, t1_}, pts_List, (opts___)?OptionQ] := 
	Module[{npts = Flatten[{N[pts]}], len = Length[fun], normalize, pplot, graphics,  curve, asize, acc, tstyle, astyle, 
				vstyle, fvals, ptstyle, data, jerk, jstyle, limit, pvector, tastyle}, 
 
	Which[
   		len == 2, 
			{pplot, graphics, pvector} = {DirParametricPlot, Graphics, PlotVector}; 
			curve = PiecewiseExpand[fun,Assumptions->Element[t,Reals]], 
   		len > 4, 
			Return[Message[ParametricPlot3D::ppfun, fun]], 
   		True, 
   			curve = PiecewiseExpand[fun (*Take[fun, 3]*),Assumptions->Element[t,Reals]]; 
			{pplot, graphics, pvector} = {DirParametricPlot3D, Graphics3D, PlotVector3D}]; 

	{acc, normalize, jerk, limit} = {Acceleration, Normalize, Jerk, UseLimit} /. {opts} /. Options[PlotTV]; 

	{tstyle, astyle, ptstyle, vstyle, jstyle} = 
		setps5[PlotTV, {TangentVectorStyle, AccelerationVectorStyle, PointStyle, VectorStyle, JerkStyle}, Length[npts], opts]; 

	tstyle = Flatten[#,1]& /@ Transpose[{tstyle, vstyle}]; 
	astyle = Flatten[#,1]& /@ Transpose[{astyle, vstyle}]; 
	jstyle = Flatten[#,1]& /@ Transpose[{jstyle, vstyle}]; 
	fvals = (curve /. t -> #1 & ) /@ npts ;

	Which[
		jerk,
			tastyle = Flatten[ Transpose[{tstyle, astyle, jstyle}],1]; 
			If[limit,
				data = NewLimit[Rest[TableDerivatives[fun,t,3]],t->#]& /@ npts, 
				data = {D[curve, t], D[curve, {t, 2}], D[curve, {t, 3}]}/. t->#& /@ npts],
		acc, 
			tastyle = Flatten[ Transpose[{tstyle, astyle}],1]; 
			If[limit,
				data = NewLimit[Rest[TableDerivatives[fun,t,2]], t->#]& /@ npts, 
				data = {D[curve, t], D[curve, {t, 2}]}/. t->#& /@ npts],
		True,
			tastyle = tstyle; 
			If[limit,
				data = NewLimit[Rest[TableDerivatives[fun,t,1]], t -> #] &  /@ npts,
				data = {D[curve, t]}/. t -> #1 &  /@ npts];
			If[normalize, data = Map[Normalize, data, {2}]]];
			
	If[Not[VectorQ[Flatten[data], NumericQ]], Return[Message[Limit::noderiv1]]];
	data = Chop[Flatten[MapThread[Outer[{#1, #1 + #2} & , {#1}, #2, 1] & , {fvals, data}], 2]]; 

	Show[
		pplot[Evaluate[fun], {t, t0, t1}, Evaluate[Sequence @@ FilterRules[Flatten[{opts}], Options[pplot]]], DrawArrowheads->False, 
			ColorFunction->None, PlotStyle->Black], 
		pvector[data, VectorStyle->tastyle, (*ArrowSize->asize*) Sequence@@FilterRules[{opts},Options[pvector]]//Evaluate],
		graphics[MapThread[Flatten[{#1, #2, Point[#3]}] & , {tstyle, ptstyle, fvals}]],
		Sequence @@ FilterRules[Flatten[{opts}], Options[graphics]], Axes -> Automatic,PlotRange->All,PlotRangePadding->Scaled[.04]]]

PlotTangentVector[fun:{{_,_}..}, {t_, t0_, t1_}, pts_, opts___?OptionQ] := 
	Module[{ps},
	ps = setps5[Plot, PlotStyle, Length[fun], opts];
	Show[DeleteCases[MapThread[PlotTV[#1, {t, t0, t1}, Flatten[{pts}],  PlotStyle -> #2, opts]&, {fun, ps}], Null]]
]	(*7-5-11 Added DeleteCases *)
		
PlotTangentVector[fun_?VectorQ, {t_, t0_, t1_}, pts_, opts___?OptionQ] := 
	If[Length[fun]==2, PlotTV[fun, {t, t0, t1}, Flatten[{pts}], opts], Return[Message[PTV::length2]]]
(*
PlotTangentVector3D[fun:{{_,_,_}..}, {t_, t0_, t1_}, pts_, opts___?OptionQ] := 
	Show[Map[PlotTV[#, {t, t0, t1}, Flatten[{pts}], opts]&,fun]]	
	
PlotTangentVector3D[fun:{_,_,_}, {t_, t0_, t1_}, pts_, opts___?OptionQ] := 
	PlotTV[fun//Evaluate, {t, t0, t1}, Flatten[{pts}], opts]	 	
*)

PlotTangentVector3D[fun_, {t_, t0_, t1_}, pts_, opts___?OptionQ] := 
	If[Length[Flatten[{fun//Evaluate}]]==3,
		PlotTV[fun, {t, t0, t1}, Flatten[{pts}], opts],
		Show[Map[PlotTV[#, {t, t0, t1}, Flatten[{pts}], opts]&,fun]]
	]

PolarPlotTangentVector[r_, {t_, tmin_, tmax_}, t0_, opts___] := 
	PlotTangentVector[{r Cos[t], r Sin[t]}, {t, tmin, tmax}, t0, opts]	



PlotCylinder[fun_List,{t_,t0_,t1_},{z_,zmin_,zmax_},opts___?OptionQ] :=
	Module[{funl},
	funl = If[Head[fun[[1]]] === List,fun,{fun}];	
	ParametricPlot3D[fun,{t,t0,t1},{z,zmin,zmax},
			Sequence@@FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate,Mesh->None]
	]


PlotCylinder[fun_List,{t_,tmin_,tmax_},opts___?OptionQ] := 
	Module[{funl, base, pos},
	funl = If[Head[fun[[1]]] === List,fun,{fun}];
	base = Base /. {opts} /. Options[PlotCylinder];
	pos = First[Complement[{1,2,3},base]];
	funl = Map[ReplacePart[#,d #[[pos]],pos]&, funl];
	ParametricPlot3D[funl//Evaluate,{t,tmin,tmax},{d,0,1},Sequence@@FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate,Mesh->None]
	]
	
	


PlotSection[fun_, {x_, x0_, x1_}, {y_, y0_, y1_}, psect___, opts___?OptionQ]:=
	Module[{funl=Flatten[{fun}], surface, point, point2, point3, cbase, plot, plotrange, crange, pptsect, xrange1, yrange1, zrange1}, 

	funl = If[FreeQ[Hold[Unevaluated[fun]], RealPower] && FreeQ[Unevaluated[opts],RealPower],
		funl, Return[Message[RealPower::notallowed]]];
	pstyle = setps5[PlotSection, PlotStyle, Length[funl], opts];
	{surface, cfsurface} = {Surface, ColorFunctionSurface}/. {opts}/. Options[PlotSection]; 
	cfsurface = (Flatten[{cfsurface}]/.{}->None); (*Required because ColorFunction->{} causes the kernel to quit. *)
	If[Length[cfsurface]>1,Message[PlotSection::colorfunctionsurface]; cfsurface=First[cfsurface]];
	plot = Plot3D[funl, {x, x0, x1}, {y, y0, y1}, PlotStyle->pstyle, Evaluate[Sequence@@FilterRules[{opts}, Options[Plot3D]]],ColorFunction->First[cfsurface]];	

	{point, cbase, crange, pptsect} = 
		{DrawPoint, CylinderBase, CylinderRange, PlotPointsSection}/. {opts}/. Options[PlotSection];

	If[Not[FreeQ[plot, Null]], Return[]];
	plotrange = PlotRange/. AbsoluteOptions[plot, PlotRange];
	{xrange1, yrange1, zrange1} = plotrange; 	
(* Get plot range of o-sections *)
	osection = OSection /. {opts} /. Options[PlotSection];	
	If[MatchQ[crange,All] && Not[MatchQ[osection, {}]],  (*01-31-17  Replaced Options by AbsoluteOptions *)
		crange = Last[PlotRange/.AbsoluteOptions[ParametricPlot3D[Evaluate[({x, y, #1}&)/@osection], {x, x0, x1}, {y, y0, y1}, PlotPoints->Max[pptsect], 
				PlotRange -> crange, Evaluate[Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]]]],PlotRange]]];

	
	cbase = If[VectorQ[Flatten[{cbase}], NumericQ], Flatten[{cbase}], {}]; 
	Which[
		MatchQ[crange, Automatic], cbase = Min[cbase, First[zrange1]]; crange = {}, 
		MatchQ[crange, {a_?NumericQ, Automatic}], cbase = First[crange]; crange = {}, 
		MatchQ[crange, Reverse], cbase = Max[cbase, Last[zrange1]]; crange = {}, 
		MatchQ[crange, {a_?NumericQ, b_?NumericQ}], cbase = First[crange], 
		True, cbase = {}; crange = {}]; 

	plotrange = With[{data = DeleteCases[{Transpose[plotrange], ({x0, y0, #1}&)/@Flatten[{cbase, crange}]}, {{}}]}, ({Min[#1], Max[#1]}&)/@Transpose[Join@@data]]; 

	If[!point==={}, 
		point = N[If[Head[point[[1]]]===List, point, {point}]]; 
		point2 = Select[point, Length[#1]==2&]; 
		point3 = Select[point, Length[#1]==3&]; 
		If[!point2==={}, 
			point2 = If[MemberQ[point2, _->_, \[Infinity]], point2, PointsToRules[point2, {x, y}]]; 
			point2 = Flatten[(ReleaseHold[{x, y, #1}/. point2]&)/@funl, 1]]; 
		point = Join[point2, point3]; 
		If[MatrixQ[First[point]], point = Flatten[point, 1]]; 
		{point, plotrange} = setdp[PlotSection, point, plotrange, opts], 
		point={}]; 

	pptsect=If[FreeQ[pptsect, Automatic], pptsect, 35];(*{Max[{75,pptsect}],2}, {75,2}];*)

	Show[
			If[surface, plot, Graphics3D[{}]], 
			Graphics3D[point], 
			Map[plotsection[#, {x, x0, x1}, {y, y0, y1}, psect, PlotRange -> plotrange, CylinderBase -> cbase, 
				PlotPointsSection -> pptsect, opts]&,funl], (*Changed ranges from x00 to x0, ... 04-18-16 *)
		PlotRange -> plotrange, Sequence@@FilterRules[{opts}, Options[Graphics3D]], Axes->True, PlotRangePadding-> Scaled[.03(*.01*)](*Changed on 12-23-16*)]]


			
plotsection[fun_, {x_, x0_, x1_}, {y_, y0_, y1_}, gun_, {t_, t0_, t1_}, opts___?OptionQ]:=
	Module[{f, g, gunl, plotrange, crange, cfsection, zmin, zmax, plotsur, cyl, pptsect, len2, sstyle, cstyle, csstyle, curve2, plen2, ln2}, 
	gunl = Switch[Length[Flatten[{gun}]], 1, Flatten[{gun}], 2, {Flatten[{gun}]}, _, gun];
	gunl = If[And @@ (Length[# /. t -> t0] == 2 & /@ gunl), gunl,  Message[PlotSection::pw]; Return[plotsection[fun, {x, x0, x1}, {y, y0, y1}, opts]]];

	f = Function[{x,y}, fun];
	plotrange=PlotRange/. {opts};
	{zmin, zmax}=plotrange[[3]];
	{cyl, cbase, crange, pptsect, cfsection}=
		{DrawCylinder, CylinderBase, CylinderRange, PlotPointsSection, ColorFunctionSection}/. {opts}/. Options[PlotSection];

	Which[
		MatchQ[crange, Automatic], g=f, 
		MatchQ[crange, {_?NumericQ, Automatic}], g=f;zmin=First[crange], 
		VectorQ[crange, NumericQ], g:=Function@@{{x, y}, crange[[2]]};zmin=crange[[1]], 
		MatchQ[crange, Reverse], {zmin, zmax}=Reverse[{zmin, zmax}];g=f, 
		True, g:=Function@@{{x, y}, zmax}];

	ln2=Length[gunl];

	{sstyle, cstyle, cfsection}=setps5[PlotSection, {SectionStyle, CylinderStyle, ColorFunctionSection}, Length[ gunl], opts];

	cfsection = (cfsection/.{}->None); (*Required because ColorFunction->{} causes the kernel to quit. *)


	If[Not[FreeQ[cstyle, RGBColor[_, _, _]|Hue[_] | GrayLevel[_]]], Message[color::cstyle]];
	cstyle=DeleteCases[cstyle, RGBColor[_, _, _]|Hue[_] | GrayLevel[_] |Tube[_]|Thickness[_], {2}];

	If[Not[FreeQ[sstyle, Opacity[_]|Thickness[_]]], Message[notopt::sstyle]];
	sstyle=DeleteCases[sstyle, Opacity[_], {2}];	


	csstyle=DeleteCases[Flatten/@Transpose[{sstyle, cstyle}], Tube[_]|Thickness[_], {2}];
	
	If[Not[MatchQ[len2, {}]],

		curve2 = MapThread[ParametricPlot3D[{#1, f@@#1}//ReleaseHold, {t, t0, t1}, PlotPoints->Max[pptsect], PlotStyle->#2, 
				ColorFunction->#3, Exclusions->None, Evaluate[Sequence@@FilterRules[Flatten[{opts}], Options[ParametricPlot3D]]]]&, 
				{gunl, sstyle, cfsection}];

		If[cyl, 
			plen2=MapThread[ParametricPlot3D[{#1, d(g@@#1-zmin)+zmin}//ReleaseHold, {t, t0, t1}, {d, 0, 1}, PlotPoints->pptsect, 
				Mesh->None, Exclusions->None, PlotStyle->#2, Evaluate[Sequence@@FilterRules[Flatten[{opts}], Options[ParametricPlot3D]]]]&, 
					{gunl, csstyle}], 
			plen2=Graphics3D[{}]], 
		curve2=plen2=Graphics3D[{}]];

	plotsur = plotsection[fun, {x, x0, x1}, {y, y0, y1}, PlotRange->plotrange, PlotPointsSection->pptsect, SectionStyle->{}, opts];
	{plotsur, plen2, curve2}]
			
			

plotsection[fun_, {x_, x0_, x1_}, {y_, y0_, y1_}, opts___?OptionQ]:=
	Module[{f, f1, xsection, ysection, zsection, vsection, osection, xrange1, yrange1, zrange1, xplane, zrad, 
		xcurve, yplane, ycurve, crange, cstyle, vxfun, vyfun, xlen, ylen, zlen, olen, vlen, xstyle, ystyle, zstyle, ostyle, vstyle, cyl, plotrange, 
		zmin, zmax, pptsect, orad, zplane, zcurve, oplane, ocurve, vsection1, vsect2, vxsect, vysect, vxplane, vyplane}, 
	{xsection, ysection, zsection, vsection, osection, cyl, crange, pptsect}=
		{XSection, YSection, ZSection, VSection, OSection, DrawCylinder, CylinderRange, PlotPointsSection}/. {opts}/. Options[PlotSection];
	{xsection, ysection, vsection, zsection, osection}=Flatten/@{{xsection}, {ysection}, {vsection}, {zsection}, {osection}};
	If[!VectorQ[N[xsection\[Union]ysection\[Union]zsection], NumberQ], Message[section::notnumber];
		{xsection, ysection, zsection}=(Select[#1, NumericQ[#1]&]&)/@{xsection, ysection, zsection}];

	{xlen, ylen, zlen, olen, vlen}=Length/@{xsection, ysection, zsection, osection, vsection};
	{xstyle, ystyle, zstyle, ostyle, vstyle, cstyle}=
		MapThread[setps5[PlotSection, #1, #2, opts]&, {{XSectionStyle, YSectionStyle, ZSectionStyle, OSectionStyle, VSectionStyle, CylinderStyle}, 
			{xlen, ylen, zlen, olen, vlen, 1}}];


	If[Not[FreeQ[{xstyle, ystyle, zstyle, ostyle, vstyle},Opacity[_],Infinity]],Message[opacity::xyzvosection]];
		{xstyle, ystyle, zstyle, ostyle, vstyle} = Map[DeleteCases[#, Opacity[_],{2}]&,{xstyle, ystyle, zstyle, ostyle, vstyle}];


	plotrange=PlotRange/. {opts};


	{xrange1, yrange1, zrange1}=plotrange;
	{zmin, zmax}=zrange1;

	Which[
		MatchQ[crange, Automatic], f:=Function[{x, y}, fun];f1:=Function[{x, y}, fun](*;crange={}*), 
		MatchQ[crange, {_?NumericQ, Automatic}], f:=Function[{x, y}, fun];f1:=Function[{x, y}, fun];zmin=First[crange], 
		MatchQ[crange, Reverse], f:=Function[{x, y}, fun];f1:=Function[{x, y}, fun];{zmin, zmax}=Reverse[{zmin, zmax}]; crange = Automatic, 
		VectorQ[crange, NumericQ], f:=Function[{x, y}, zmax];f1:=Function[{x, y}, fun];{zmin, zmax}=crange, 
		True, f:=Function[{x, y}, zmax];f1:=Function[{x, y}, fun]];

	
	If[Or@@Apply[checkbound[#1, #2, #3]&&#3=!={}&, 
		{{Sequence@@xrange1, xsection}, {Sequence@@yrange1, ysection}, {Sequence@@zrange1, zsection}}, {1}], Message[section::outrange]];

	If[Not[MatchQ[xsection, {}]], 
		If[cyl===True, 
			xplane=ParametricPlot3D[Evaluate[({#1, y, d (f[#1, y]-zmin)+zmin}&)/@xsection], {y, y0, y1}, {d, 0, 1}, 
				Mesh->None, PlotPoints->pptsect, Exclusions->None, PlotStyle->(Flatten[{cstyle, #1}]&)/@(xstyle/. Tube[_]->{}), 
				Evaluate[Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]]]], 
			xplane=Graphics3D[{}]];
		xcurve=ParametricPlot3D[Evaluate[({#1, y, f1[#1, y]}&)/@xsection], {y, y0, y1}, PlotPoints->Max[pptsect], 
			Exclusions->None, PlotStyle->xstyle, Mesh->None, Evaluate[Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]]]], 
		xplane=xcurve=Graphics3D[{}]];

	If[Not[MatchQ[ysection, {}]], 
		If[cyl===True,
			yplane=ParametricPlot3D[Evaluate[({x, #1, d (f[x, #1]-zmin)+zmin}&)/@ysection], {x, x0, x1}, {d, 0, 1}, 
				Mesh->None, PlotPoints->pptsect, Exclusions->None, PlotStyle->(Flatten[{cstyle, #1}]&)/@(ystyle/. Tube[_]->{}), 
				Evaluate[Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]]]], 
			yplane=Graphics3D[{}]];
		ycurve=ParametricPlot3D[Evaluate[({x, #1, (f1[x, #1]-zmin)+zmin}&)/@ysection], {x, x0, x1}, PlotPoints->Max[pptsect], PlotStyle->ystyle, Exclusions->None, Mesh->None, 
			Evaluate[Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]]]], 
		yplane=ycurve=Graphics3D[{}]];
	{zrad, orad}=Map[Cases[#1, Tube[r_]->r, \[Infinity]]&, {zstyle, ostyle}, {2}];

	If[Not[MatchQ[zsection, {}]], 
		If[cyl===True,
			zplane=ParametricPlot3D[Evaluate[({x, y, #1}&)/@zsection], {x, x0, x1}, {y, y0, y1}, PlotPoints->3, (*MaxRecursion->0, *)Mesh->None, Exclusions->None, 
				PlotStyle->(Flatten[{cstyle, #1}]&)/@(zstyle/. Tube[_]->{}), Evaluate[Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]]]], 
		zplane=Graphics3D[{}]];
		zcurve=Quiet[MapThread[ContourPlot[fun==#1, {x, x0, x1}, {y, y0, y1}, PlotPoints->Max[pptsect], 
				Evaluate[Sequence@@FilterRules[{opts}, Options[ContourPlot]]], ContourLabels->None, ContourStyle->#2]&, {zsection, zstyle}]];
		zcurve=MapThread[If[#1[[1]]=!={}, Graphics3D[GraphicsComplex[#1[[1, 1]]/. {a_, b_}:>Flatten[{a, b, f1@@{a, b}}], #1[[1, 2]]]]/. 
				If[!Flatten[{#2}]==={}, Line[pts_, rest___]:>Tube[pts, First[#2], rest], {}], zcurve=Graphics3D[{}]]&, {zcurve, zrad}], 
		zplane=zcurve=Graphics3D[{}]];

	If[Not[MatchQ[osection, {}]],
		osection = osection/.z_==a_->a;
		oplane=ParametricPlot3D[Evaluate[({x, y, #1}&)/@osection], {x, x0, x1}, {y, y0, y1}, Mesh->None, Exclusions->None, PlotPoints->pptsect, 
				PlotStyle->(Flatten[{cstyle, #1}]&)/@(ostyle/. Tube[_]->{}), PlotRange -> crange, Evaluate[Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]]]];

		(*If[cyl===True,				
			oplane=ParametricPlot3D[Evaluate[({x, y, #1}&)/@osection], {x, x0, x1}, {y, y0, y1}, Mesh->None, Exclusions->None, PlotPoints->pptsect, 
				PlotStyle->(Flatten[{cstyle, #1}]&)/@(ostyle/. Tube[_]->{}), PlotRange -> crange, Evaluate[Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]]]], 
			oplane=Graphics3D[{}]]; 6-20-17*)
		ocurve=Quiet[MapThread[ContourPlot[fun==#1, {x, x0, x1}, {y, y0, y1}, PlotPoints->Max[pptsect], 
			Evaluate[Sequence@@FilterRules[{opts}, Options[ContourPlot]]], ContourLabels->None, ContourStyle->#2]&, {osection, ostyle}]];
		ocurve=MapThread[If[#1[[1]]=!={}, Graphics3D[GraphicsComplex[#1[[1, 1]]/. {a_, b_}:>Flatten[{a, b, f1@@{a, b}}], #1[[1, 2]]]]/. If[!Flatten[{#2}]==={}, Line[pts_, rest___]:>Tube[pts, First[#2], rest], {}], ocurve=Graphics3D[{}]]&, {ocurve, orad}], 
		oplane=ocurve=Graphics3D[{}]];


	If[Not[MatchQ[vsection, {}]], 
		vsection1 = Join[Cases[vsection, y==a_|a_==y|x==a_|a_==x->a], Select[vsection, Head[#1]=!=Equal&]];
		If[Length[vsection]=!=Length[vsection1], Message[vsect::badinput];vsection = vsection1, vsection = vsection1];
		vxfun=Select[vsection, MemberQ[{#1}, x, \[Infinity]] && Not[MemberQ[{#1}, y, \[Infinity]]]&];
		vyfun=Select[vsection, MemberQ[{#1}, y, \[Infinity]] && Not[MemberQ[{#1}, x, \[Infinity]]]&];
		vsect2 = Join[vxfun, vyfun];
		If[Not[Length[vsect2]==Length[vsection]], Print[Message[vsect::badinput]]];

		If[Not[MatchQ[vxfun, {}]], 
			vxsect = MapThread[ParametricPlot3D[ReleaseHold[{x, #1, f1[x, #1]}], {x, x0, x1}, PlotPoints->Max[pptsect], 
				PlotStyle->#2,
				Exclusions->None, Mesh->None, Evaluate[Sequence@@FilterRules[Flatten[{opts}], Options[ParametricPlot3D]]]]&, 
					{vxfun, vstyle[[1;;Length[vxfun]]]}];
			If[MatchQ[cyl, True], 
				vxplane = MapThread[ParametricPlot3D[ReleaseHold[{x, #1, d (f[x, #1]-zmin)+zmin}], {x, x0, x1}, {d, 0, 1}, PlotPoints->pptsect, 
					PlotStyle->Flatten[{cstyle, (#2/. Tube[_]->{})}],
					Exclusions->None, Mesh->None, Evaluate[Sequence@@FilterRules[Flatten[{opts}],Options[ParametricPlot3D]]]]&, 
						{vxfun, vstyle[[1;;Length[vxfun]]]/.Tube[_]->{}}], 
				vxplane = Graphics3D[{}]], 
			vxsect = vxplane = Graphics3D[{}]];

		If[Not[MatchQ[vyfun, {}]], 
			vysect=MapThread[ParametricPlot3D[ReleaseHold[{#1, y, f1[#1, y]}], {y, y0, y1}, PlotPoints->Max[pptsect], 
				PlotStyle->#2, Exclusions->None, Mesh->None, Evaluate[Sequence@@FilterRules[Flatten[{opts}],Options[ParametricPlot3D]]]]&, 
					{vyfun, vstyle[[Length[vxfun]+1;;Length[vsect2]]]}];
			If[MatchQ[cyl, True], 
				vyplane= MapThread[ParametricPlot3D[ReleaseHold[{#1, y, d (f[#1, y]-zmin)+zmin}], {y, y0, y1}, {d, 0, 1}, PlotPoints->pptsect,
					PlotStyle->Flatten[{cstyle, (#2/. Tube[_]->{})}],
					Exclusions->None, Mesh->None, Evaluate[Sequence@@FilterRules[Flatten[{opts}], Options[ParametricPlot3D]]]]&, 
						{vyfun, vstyle[[Length[vxfun]+1;;Length[vsect2]]]/.Tube[_]->{}}], 
				vyplane=Graphics3D[{}]], 
		vysect=vyplane=Graphics3D[{}]], 
	vxsect=vysect=vxplane=vyplane=Graphics3D[{}]];

	{xplane, xcurve, yplane, ycurve, zplane, zcurve, oplane, ocurve, vxsect, vxplane, vysect, vyplane}]    

 


	
ParaPlotSection[fun_, {u_, u0_, u1_}, {v_, v0_, v1_}, gun_, {t_,t0_,t1_}, opts___?OptionQ] :=
	Module[{funl, gunl, surface, pstyle, plot, plotrange, cbase, crange, zmin, zmax, plotsur, len2, len3, cyl, pptsect, cfsection, 
		xrange, yrange, plotrangepass, sstyle, cstyle, srad, curve3, cfun, plotcyl,crangeorig},

	funl = If[FreeQ[Hold[Unevaluated[fun]], RealPower] && FreeQ[Unevaluated[opts],RealPower] && FreeQ[Unevaluated[gun],RealPower],
		If[MatrixQ[fun], fun, {fun}], Return[Message[RealPower::notallowed]]];
	gunl = If[MatchQ[Head[First[gun]],List], gun, {gun}];
	{pstyle,cfsurface} = setps5[PlotSection, {PlotStyle, ColorFunctionSurface}, Length[funl], opts];
	cfsurface = (cfsurface/.{}->None); (*Required because ColorFunction->{} causes the kernel to quit. *)
	plot = Show[MapThread[ParametricPlot3D[#1, {u, u0, u1}, {v, v0, v1}, PlotStyle-> #2, ColorFunction->#3, 
			Evaluate[Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]]]]&,{funl,pstyle,cfsurface}]];			
			
	If[Not[FreeQ[plot, Null]], Return[]];
	plotrange = PlotRange /. AbsoluteOptions[plot,PlotRange]; 

	plotsur = ParaPlotSection[funl, {u, u0, u1}, {v, v0, v1}, Surface -> False, PlotRange -> plotrange, opts, Flag -> False];

	If[MatchQ[plotsur,Null],Return[]];  (*07-06-15 *)
	len3 = Select[gunl,(Length[#]==3&)];
	len2 = Complement[gunl, len3];	

	sorder = Map[Position[gunl, #] & , {len2, len3}, {2}];

	{cyl, pptsect, cfsection} = 
		{DrawCylinder, PlotPointsSection, ColorFunctionSection}
			 /. {opts} /. Options[ParaPlotSection];
			 
(*New 09-01-2014 *)
	{sstyle, cstyle, cfsection}=setps5[PlotSection, {SectionStyle, CylinderStyle, ColorFunctionSection}, Length[ gunl], opts];
	cfsection = (cfsection/.{}->None); (*Required because ColorFunction->{} causes the kernel to quit. *)

(* End New *)	
	If[Not[FreeQ[cstyle, RGBColor[_, _, _]|Hue[_] | GrayLevel[_]]], Message[color::cstyle]];
	cstyle=DeleteCases[cstyle, RGBColor[_, _, _]|Hue[_] | GrayLevel[_] |Tube[_]|Thickness[_], {2}];
(* New 08-15-2014*)
	If[Not[FreeQ[sstyle, Opacity[_]]], Message[opacity::sstyle]];
	sstyle=DeleteCases[sstyle, Opacity[_], {2}];	
(* End New *)	
	csstyle=DeleteCases[Flatten/@Transpose[{sstyle, cstyle}], Tube[_]|Thickness[_], {2}];
	{sstyle,csstyle} = {sstyle[[Flatten[sorder]]],csstyle[[Flatten[sorder]]]};			 
			 
			 
If[pptsect === Automatic, pptsect =75];
(*	If[pptsect === Automatic, pptsect = {25, 2} (* 15 *)];
	pptsect = Flatten[{pptsect}];
	pptsect = If[Length[pptsect] == 2, pptsect, {pptsect[[1]], 2}];
*)
	{crange, cbase, plotrangepass, plotrange} = plotsur[[-4;;-1]];
	{xrange, yrange, {zmin, zmax}} = plotrangepass (*Last[plotsur]*);
	crangeorig = CylinderRange /. {opts} /. Options[ParaPlotSection];
	If[MatchQ[crangeorig,Reverse], {zmin,zmax}=Reverse[{zmin,zmax}]];


	If[Not[len2 === {}], len2 = Map[Join[#1,{zmax}]&, len2]];
(*	{sstyle, cstyle} = setps5[PlotSection, {SectionStyle, CylinderStyle}, Length[len3], opts];*)
	srad = Map[Cases[#, Tube[r_] -> r, Infinity] &, sstyle];
(*	cstyle = DeleteCases[cstyle, Hue[__] | RGBColor[_,_,_] | GrayLevel[__],Infinity];*)
(*	cstyle = Flatten/@Transpose[{sstyle, cstyle}]; (*Force sections to have the same style as cylinders *)*)

	If[Not[len3 === {}],	
		curve3 = 
			MapThread[ParametricPlot3D[#1//Evaluate, {t, t0, t1}, PlotPoints->pptsect, PlotStyle->#2, 
				ColorFunction->#3, Exclusions->None, Evaluate[Sequence@@FilterRules[Flatten[{opts}], Options[ParametricPlot3D]]]]&, 
				{len3, sstyle, cfsection}],
  		curve3 = Graphics3D[{}]];

	If[cyl === True, 
		(*cfun = Join[ReplacePart[#,d (zmax - zmin) + zmin, 3] & /@ len2, ReplacePart[#,d (#[[3]] - zmin) + zmin, 3] & /@len3];*)
		cfun = If[MatchQ[crangeorig,All] || VectorQ[crangeorig,NumericQ], 
			Join[ReplacePart[#,d (zmax - zmin) + zmin, 3] & /@ len2, ReplacePart[#,d (zmax - zmin) + zmin, 3] & /@len3],
			Join[ReplacePart[#,d (zmax - zmin) + zmin, 3] & /@ len2, ReplacePart[#,d (#[[3]] - zmin) + zmin, 3] & /@len3]];
		Off[ParametricPlot3D::exclul];

		plotcyl = ParametricPlot3D[cfun//Evaluate(*ReleaseHold*) (*RlacePart[#,d (#[[3]] - zmin) + zmin, 3] & /@ Flatten[{len2,len3},1]//Evaluate,*),
				{t,t0,t1},{d,0,1}, PlotPoints -> pptsect, Mesh -> None, PlotStyle -> (csstyle/.Tube[r_]->{}), PlotRange->plotrangepass,
				 ColorFunction->Automatic, Sequence@@FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate],
		plotcyl = Graphics3D[{}] ];
		On[ParametricPlot3D::exclul];


	surface = Surface /. {opts} /. Options[PlotSection]; 
	Show[If[surface, plot, Graphics3D[{}]], Drop[plotsur,-4], plotcyl, curve3, 
				Sequence @@ FilterRules[{opts}, Options[Graphics3D]], Axes->True, PlotRange->All]]




ParaPlotSection[fun_, {u_,u0_,u1_}, {v_,v0_,v1_}, opts___?OptionQ] :=
	Block[{cbase, crange, cstyle, cyl, flag, funl, ofuns, olen, orad, osecs, osection, ostyle, plot, plotrange, plotrangepass, point, point2, 
		point3, pstyle, rules, surface, vars, vlen, vrad, vsection, vsection1, vstyle, vxfun, vxplots, vyfun, vyplots, x, xlen, xplane, 
		xrad, xrange, xrange1, xsection, xstyle, y, ylen, yplane, yrad, yrange, yrange1, ysection, ystyle, z, zlen, zmax, zmin, zrad, zrange, zrange1, 
		zsecs, zsection, zstyle,  xx,  yy},

	funl = If[FreeQ[Hold[Unevaluated[fun]], RealPower] && FreeQ[Unevaluated[opts],RealPower],
		If[MatrixQ[fun], fun, {fun}], Return[Message[RealPower::notallowed]]];

	{xsection, ysection, zsection, vsection, osection, vars, surface, cyl, cbase, crange, point} = 
		{XSection, YSection, ZSection, VSection, OSection, SectionVariables, Surface, DrawCylinder, CylinderBase, 
				CylinderRange, DrawPoint}  
				/. {opts} /. Options[ParaPlotSection];

	{xsection, ysection, vsection, zsection, osection} = 
 		Flatten/@{{xsection},{ysection},{vsection},{zsection},{osection}};
		
 	If[Not[VectorQ[N[Union[xsection,ysection,zsection]],NumberQ]],Return[Message[section::notnumber]]];
	
	{xlen, ylen, zlen, olen, vlen} = Length /@ {xsection, ysection, zsection, osection, vsection}; 
	{xstyle, ystyle, zstyle, ostyle, vstyle, cstyle} = 
		MapThread[setps5[ParaPlotSection, #1, #2, opts] & , 
 			{{XSectionStyle, YSectionStyle, ZSectionStyle, OSectionStyle, VSectionStyle, CylinderStyle}, {xlen, ylen, zlen, olen, vlen, 1}}];
	If[Not[FreeQ[{xstyle, ystyle, zstyle, ostyle, vstyle},Opacity[_],Infinity]],Message[opacity::xyzvosection]];
		{xstyle, ystyle, zstyle, ostyle, vstyle} = Map[DeleteCases[#, Opacity[_],{2}]&,{xstyle, ystyle, zstyle, ostyle, vstyle}];	

	flag = Flag /. {opts} /. Flag->True;
	
	{pstyle,cfsurface} = setps5[PlotSection, {PlotStyle, ColorFunctionSurface}, Length[funl], opts];
	cfsurface = (cfsurface/.{}->None); (*Required because ColorFunction->{} causes the kernel to quit. *)

	If[flag,	
		plot = Show[MapThread[ParametricPlot3D[#1//Evaluate, {u, u0, u1}, {v, v0, v1}, PlotStyle-> #2, ColorFunction->#3, 
			Evaluate[Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]]]]&,{funl,pstyle,cfsurface}]];			
		If[Not[FreeQ[plot, Null]], Return[]];
		plotrange = PlotRange /. AbsoluteOptions[plot,PlotRange],
		plotrange = PlotRange /. {opts}]; 

	{xrange1, yrange1, zrange1} = plotrange;
	{zmin,zmax} = zrange1;

	cbase = If[VectorQ[Flatten[{cbase}], NumericQ], Flatten[{cbase}], {}]; 

	Which[
		MatchQ[crange, Automatic], 
				If[MatchQ[cbase,{}], cbase = zmin; crange = {},
					cbase = First[cbase]; crange = {};
					zrange1 = Which[
								zrange1[[1]]<cbase<zrange1[[2]],{cbase,zrange1[[2]]},
								cbase<=zrange1[[1]],{cbase,zrange1[[2]]},
								cbase >= zrange1[[2]],{zrange1[[1]],cbase},
								True, zrange1]]; 
				plotrangepass = {xrange1,yrange1,zrange1}, 
		MatchQ[crange, {a_?NumericQ, Automatic}], cbase = First[crange]; crange = {}; 
				zrange1 = Which[zrange1[[1]]<=cbase<=zrange1[[2]],{cbase,zrange1[[2]]},
								cbase<=zrange1[[1]],{cbase,zrange1[[2]]},
								cbase >= zrange1[[2]],{zrange1[[1]],cbase},
								True, zrange1]; 
				plotrangepass = {xrange1,yrange1,zrange1}, 
		MatchQ[crange, Reverse], cbase = Max[{cbase,zrange1}]; crange = {}; {zmin,zmax} = Reverse[zrange1];
				zrange1 = Which[zrange1[[1]]<cbase<zrange1[[2]],{cbase,zrange1[[2]]},
								cbase<=zrange1[[1]],{cbase,zrange1[[2]]},
								cbase >= zrange1[[2]],{zrange1[[1]],cbase},
								True, zrange1]; 
								plotrangepass = {xrange1,yrange1,zrange1}, 
		MatchQ[crange, {a_?NumericQ, b_?NumericQ}], 
				zrange1 = Sort[crange]; cbase = First[zrange1]; crange = {};
								plotrangepass = {xrange1,yrange1,zrange1}, 
		True, 
				cbase = {}; crange = {}; plotrangepass = plotrange]; 

	plotrange = With[{data = DeleteCases[{Transpose[plotrange], ({0, 0, #1}&)/@Flatten[{cbase, crange}]}, {{}}]}, ({Min[#1], Max[#1]}&)/@Transpose[Join@@data]]; 

	If[!point==={}, 
		point = N[If[Head[point[[1]]]===List, point, {point}]]; 
		point2 = Select[point, Length[#1]==2&]; 
		point3 = Select[point, Length[#1]==3&]; 
		If[!point2==={}, 
			point2 = If[MemberQ[point2, _->_, \[Infinity]], point2, PointsToRules[point2, {u, v}]]; 
			point2 = Flatten[(ReleaseHold[#1/. point2]&)/@funl, 1]]; 
		point = Join[point2, point3]; 
		If[MatrixQ[First[point]], point = Flatten[point, 1]]; 
		{point, plotrange} = setdp[ParaPlotSection, point, plotrange, opts], 
		point={}]; 

	{xrange,yrange,zrange} = Flatten/@{{x,xrange1},{y,yrange1},{z,zrange1}};	

	If[Or @@ Apply[ checkbound[#1,#2,#3] && (#3 =!= {})&,
		{{Sequence@@xrange1,xsection},{Sequence@@yrange1,ysection},{Sequence@@zrange1,zsection}},{1}],
		Message[section::outrange]]; 

	{xrad, yrad, zrad, vrad, orad} = Map[Cases[#, Tube[r_] -> r, Infinity] &, {xstyle, ystyle, zstyle, vstyle, ostyle} ,{2}];

	If[xsection =!= {},
		xplane = ParametricPlot3D[Evaluate[{#,y,z}& /@ xsection], Evaluate[yrange], Evaluate[zrange],
					(*PlotPoints -> 2, MaxRecursion->0,*) Mesh->None, 
					PlotStyle -> Map[Flatten[{cstyle, #}]&, (xstyle /. Tube[_]->{})]],
		xplane = Graphics3D[{}]];

	If[ysection =!= {}, 
		yplane = ParametricPlot3D[Evaluate[{x,#,z}& /@  ysection], Evaluate[xrange],Evaluate[zrange],
					(*PlotPoints -> 2, MaxRecursion->0,*)  Mesh->None,
					PlotStyle -> Map[Flatten[{cstyle,#}]&, (ystyle /. Tube[_]->{})]],
		yplane = Graphics3D[{}]];

 	If[zsection =!= {},
 		zsecs = ParametricPlot3D[Evaluate[{x,y,#}& /@ zsection],Evaluate[xrange],Evaluate[yrange],
					(*PlotPoints -> 2, MaxRecursion->0,*)  Mesh -> None,
					PlotStyle -> Map[Flatten[{cstyle,#}]&, (zstyle /. Tube[_]->{})]],
 		zsecs = Graphics3D[{}]];
	
 	If[vsection =!= {},
		If[Length[vars] != 2, Return[Message[section::wrongnum]]];
		If[Not[MemberQ[vsection,_Equal,Infinity]], Return[Message[section::equation]]];
		{x,y} = Flatten[{vars}];
		vxfun = If[(rules = Cases[vsection, y == (a_) | a_ == y -> {y -> a}]) =!= {}, {x, y, z} /. rules, {}];
		vsection1 = DeleteCases[vsection,y == (a_) | a_ == y ];
		vyfun = If[(rules = Cases[vsection1, x == (a_) | a_ == x -> {x -> a}]) =!= {}, {x, y, z} /. rules, {}];

		vxplots = If[vxfun =!= {},
			ParametricPlot3D[vxfun, Evaluate[xrange],Evaluate[zrange], Mesh -> None,
				PlotStyle -> (Map[Flatten[{cstyle,#}]&, (vstyle /. Tube[_]->{})][[1;;Length[vxfun]]])],
			Graphics3D[{}]];
		vyplots = If[vyfun =!= {},
			ParametricPlot3D[vyfun, Evaluate[yrange],Evaluate[zrange], Mesh -> None,
				PlotStyle -> (Map[Flatten[{cstyle,#}]&, (vstyle /. Tube[_]->{})][[Length[vxfun]+1;;]])],
			Graphics3D[{}]],
		vxplots = vyplots = Graphics3D[{}]];

	If[osection =!= {}, 
		If[Length[vars] != 2, Return[Message[section::wrongnum]]];
		{x,y} = Flatten[{vars}];
		ofuns = If[(rules = Cases[osection, _ == (a_) | a_ == _ -> {z -> a}]) =!= {}, {x, y, z} /. rules, {x, y, #}& /@ osection];
		If[Intersection[Variables[N[Last[ofuns]]],vars] === {},Return[Message[section::wrongvars]]];
			osecs = ParametricPlot3D[Evaluate[ofuns],Evaluate[xrange],
					Evaluate[yrange], Mesh -> None, 
					PlotStyle -> Map[Flatten[{cstyle,#}]&, (ostyle /. {Tube[_]->{}, Opacity[_]->{}})]],
 			osecs = Graphics3D[{}]];	
	
	If[flag, 
		Show[If[surface,plot, Graphics3D[{}]], xplane, yplane, zsecs, vxplots, vyplots, osecs, Graphics3D[point], 
			Sequence@@FilterRules[{opts}, Options[Graphics3D]]//Evaluate, PlotRange->plotrange,PlotRangePadding->Scaled[.03]],
		{{xplane, yplane, zsecs, vxplots, vyplots, osecs, Graphics3D[point]}, crange, cbase, plotrangepass, plotrange}]
]



ContourPlotSection[fun_, {x_, x0_, x1_}, {y_, y0_, y1_}, gun_List, {t_,t0_,t1_},  opts___?OptionQ] := 
	Module[{gunl, ps, ppsection},
	gunl = If[Head[gun[[1]]] === List, gun, {gun}];
	ps = setps5[ContourPlotSection, SectionStyle, Length[gunl], Sequence@@({opts} /. Tube[r_] -> Thickness[r/3])];
	ppsection = PlotPointsSection /. {opts} /. Options[ContourPlotSection];
	Show[	ContourPlotSection[fun, {x,x0,x1}, {y,y0,y1}, opts],
		ParametricPlot[gunl//Evaluate,{t,t0,t1}, PlotStyle -> ps, PlotPoints -> ppsection,
			Sequence@@FilterRules[{opts},Options[ParametricPlot]]//Evaluate]]
	]


ContourPlotSection[fun_, {x_, x0_, x1_}, {y_, y0_, y1_}, opts___?OptionQ] := 
	Module[{xsect, ysect, zsect, vsect, osect, ppsection, xstyle, ystyle, zstyle, vstyle, ostyle,constraint,dpts},
	{xsect, ysect, zsect, vsect, osect, ppsection} = 
 		{XSection, YSection, ZSection, VSection, OSection, PlotPointsSection} /. {opts} /. Options[ContourPlotSection];
 	{xsect, ysect, zsect, vsect, osect} = Flatten /@ {{xsect}, {ysect}, {zsect}, {vsect}, {osect}};
 	{xstyle, ystyle, zstyle, vstyle, ostyle} = MapThread[setps5[ContourPlotSection, #1, Length[#2], opts]& , 
		{{XSectionStyle,YSectionStyle,ZSectionStyle,VSectionStyle,OSectionStyle},{xsect,ysect,zsect,vsect,osect}}]/.Tube[r_]->Thickness[r/3] ;

	osect = osect /. z_ == a_ -> a;
 	xsect = If[xsect =!= {}, Map[x == #& , xsect], {}];
 	ysect = If[ysect =!= {}, Map[y == #& , ysect], {}];
	vsect = If[vsect =!= {}, Map[If[FreeQ[#,Equal],var = If[FreeQ[#,x],y,x];First[Complement[{x,y},{var}]] == #,#]&, vsect],{}];
	zsect = If[zsect =!= {}, Map[fun == #&, zsect],{}];
	
	osect = If[osect =!= {}, Map[fun == #&, osect],{}];

	constraint = Flatten[{xsect, ysect, zsect, vsect, osect}];

	dpts = DrawPoint /. {opts} /. Options[PlotSection];

	If[Not[MatchQ[dpts,{}]],
		If[VectorQ[dpts], dpts = {dpts}];

		dpts = Map[If[Length[#] >= 2, Take[#,2]]&,dpts];

		dpts = Select[dpts, (Length[#]>1&)];

		If[MatchQ[Head[dpts[[1,1]]], Rule], dpts =  RulesToPoints[dpts, {x,y}]]];

	point = setdp[ContourPlotSection, dpts, {{x0,x1},{y0,y1}},opts];

	{{x00,x11},{y00,y11}} = Map[{Min[#], Max[#]} &, Transpose[{point[[2]],{{x0,x1},{y0,y1}}}]];
	{{x00,x11},{y00,y11}} = 1.05{{x00,x11},{y00,y11}};

	If[MatchQ[constraint ,{}],
		PlotConstraint[fun,{x,x00,x11},{y,y00,y11}, Mesh->None, ParaConstraint -> {}, opts],
		(*	Show[ContourPlot[fun,{x,x00,x11},{y,y00,y11},Sequence@@FilterRules[{opts},Options[ContourPlot]]//Evaluate, AspectRatio->Automatic],
			Graphics[point[[1]]](*,PlotRange->point[[2]]*),Sequence@@FilterRules[{opts},Options[ContourPlot]]//Evaluate],*)
		PlotConstraint[fun,{x,x00,x11},{y,y00,y11}, constraint, ConstraintStyle -> Join[xstyle, ystyle, zstyle, vstyle, ostyle], 
			PlotPointsConstraint -> ppsection, Mesh->None, opts]]
	]


PlotConstraint[fun_, {x_, x0_, x1_}, {y_, y0_, y1_}, constraint_List, {t_, t0_, t1_}, otherconstraints___, opts___?OptionQ] := 
	PlotConstraint[fun,{x,x0,x1},{y,y0,y1}, otherconstraints, ParaConstraint -> constraint, PPRange -> {t,t0,t1}, opts]


(*12-23-11: added opts to Grad, Changed Gradient->True to DrawGradient -> list of points and added GradientStyle as option. DrawPoint remains the same. *)	
PlotConstraint[fun_,{x_,x0_,x1_},{y_,y0_,y1_},  cconstraint___(*cconstraint_: {_Equal..} | _Equal*), opts___?OptionQ] :=
	Module[{constraint, paraconstraint, pprange, ppconstraint, point, gradient, pointstyle, 
		contourplot, constraintstyle, x00 = x0, x11 = x1, y00 = y0, y11 = y1, asize, grads, plotpt, plotgr, prgrads, eqn, pplot, gradstyle},

	paraconstraint = ParaConstraint /. {opts} /. ParaConstraint -> {};
	paraconstraint = If[Length[Flatten[{paraconstraint}]]==2,{paraconstraint},paraconstraint];
	constraint = Map[If[Head[#] === Equal, #, Simplify[# == 0]] &, Flatten[{cconstraint}]];  

	{pprange, ppconstraint, point, gradient} = 
		{PPRange, PlotPointsConstraint, DrawPoint, DrawGradient} 
			/. {opts} /. Options[PlotConstraint];


	constraintstyle = setps5[PlotConstraint, ConstraintStyle, Length[Join[paraconstraint,constraint]], opts];	

(*	If[gradient && point === {}, Message[grad::nopoint]];*)
		If[point == {},
		plotpt = Graphics[{}],
		point = If[MemberQ[point, _ -> _, Infinity], {x, y} /. point, point];
		point = Map[Take[#,2]&, If[Head[point[[1]]] === List, point, {point}]];
		pointstyle = setps5[PlotConstraint, DrawPointStyle, Length[point], opts];
		plotpt = Graphics[MapThread[Flatten[{#1,Point[#2]}]&,{pointstyle, point}]]];
(*		{{x00,x11},{y00,y11}} = {Min[#], Max[#]}& /@ Transpose[Join[point,{{x0,y0},{x1,y1}}]];*)
	If[gradient == {},
		plotgr = Graphics[{}];
		grads=prgrads = {},
		gradient = If[Head[First[gradient]]===List, gradient,{gradient}];
		asize = ArrowSize /. {opts} /. Options[PlotGradient];	
		asize = If[Head[asize]===Symbol, asize, .035 asize];
		Off[Power::infy,Infinity::indet];
		grads = N[{#,# + KPGrad[fun,{x,y}, #, PowerBehavior->Complex, opts]}]& /@ gradient;
		If[Not[FreeQ[grads, Indeterminate]], 
			Message[plotconstr::nograd];
			grads=DeleteCases[grads, {Indeterminate, _} | {_, Indeterminate}, Infinity]];
		On[Power::infy,Infinity::indet];
		If[FreeQ[grads, Null], 
			gradstyle = setps5[PlotConstraint, GradientStyle, Length[gradient], opts];
			plotgr = Graphics[MapThread[Flatten[{#1, Point[#2], Arrowheads[asize],Arrow[#3]}]&, 
					{(*gradptstyle,*)gradstyle, gradient,  grads}]];
			prgrads={Min[#], Max[#]}& /@ Flatten[grads,1],	
			(*prgrads={Min[#], Max[#]}& /@ Transpose[Flatten[Join[{{x0,x1},{y0,y1}},grads],1]],*)
			plotgr = Graphics[{}]; prgrads = {};Message[KPGrad::nograd]]];
	{{x00,x11},{y00,y11}} = {Min[#], Max[#]}& /@ Transpose[Join[{{x0,y0},{x1,y1}},Flatten[grads,1], point]];
	{{x00,x11},{y00,y11}} = {1.05{x00,x11},1.05{y00,y11}};

(*	If[Length[Flatten[{prange}]] > 2, Return[Message[PlotRange::cprng, prange]]];*)
	contourplot = ContourPlot[fun,{x,x00,x11},{y,y00,y11},Sequence@@FilterRules[{opts},Options[ContourPlot]]//Evaluate, 
		AspectRatio -> Automatic];
	
	If[constraint =!= {}, 
		eqn =  ContourPlot[constraint//Evaluate,{x,x00,x11},{y,y00,y11}, PlotPoints -> ppconstraint, ContourStyle -> constraintstyle[[(Length[paraconstraint]+1);;]], 
						Evaluate[Sequence@@FilterRules[{opts}, Options[ContourPlot]]], AspectRatio->Automatic, PlotRange->All],
		eqn = Graphics[{}]];

	If[paraconstraint =!= {},
		pplot = ParametricPlot[paraconstraint//Evaluate, pprange//Evaluate, PlotStyle -> constraintstyle[[;;Length[paraconstraint]]],
			PlotPoints -> ppconstraint, Sequence@@FilterRules[{opts},Options[ParametricPlot]]//Evaluate],
		pplot = Graphics[{}]]; 	
	Show[ contourplot, eqn, pplot, plotpt, plotgr(*, PlotRange->All*)]
	]


ParaTangentPlane[fun_, {s_, t_}, tpts_?MatrixQ, opts___?OptionQ] := 
	Map[ParaTangentPlane[fun, {s, t}, #, opts] &, tpts]

(* 12-16-11: Removed Limit and Flag *)
ParaTangentPlane[fun_?VectorQ, {s_, t_}, t0_?VectorQ, opts___?OptionQ] := 
	Module[{normalize, deriv}, 
	normalize = Normalize /. {opts} /. Options[ParaTangentPlane];
	If[Head[fun]===Piecewise,Return[Message[Limit::piecewise]]];

	deriv = JacobianMatrix[fun, {s,t}, t0, (*PowerBehavior->Complex,*) opts]//Transpose//Quiet;  (*04-08-16*)

	If[FreeQ[deriv, {0,0,0} | DirectedInfinity[___] | Indeterminate | Null] (* && MatrixQ[deriv,NumericQ]*), (*Commented out on 11-07-17 *)
			If[normalize, deriv = Map[Normalize[#, Reals]&, deriv]];
			Function[{s,t}, fun] @@ t0+ {s,t}.deriv//Simplify,
			Message[TangentPlane::singpt, Function[{s,t}, fun] @@ t0, {s,t} == t0]]
			]

ParaPlotTangentPlane[Tooltip[fun_], {u_, u0_, u1_}, {v_, v0_, v1_}, tanpts_List, opts___?OptionQ] := 
	Block[{Flag2},
	ParaPlotTangentPlane[fun, {u,u0,u1},{v,v0,v1}, tanpts, opts, Flag2->Tooltip]]
	
ParaPlotTangentPlane[fun_?MatrixQ, {x_,x0_,x1_}, {y_,y0_,y1_}, tanpts_List, opts___?OptionQ] :=
	Module[{pstyle, tptstyle, tplstyle},
	{tplstyle, tptstyle, pstyle} = setps5[ParaPlotTangentPlane, {TangentPlaneStyle, TangentPointStyle, PlotStyle}, 15, opts];
	tplstyle = MapThread[Flatten[{#1, #2}] &,{ColorData[97][#] & /@ Swap[Range[15], 1, 2], tplstyle}][[1;;Length[fun]]];
	tptstyle = MapThread[Flatten[{#1, #2}] &,{ColorData[97][#] & /@ Swap[Range[15], 1, 2], tptstyle}][[1;;Length[fun]]];
	pstyle = MapThread[Flatten[{#1, #2}] &,{ColorData[97][#] & /@ Swap[Range[15], 1, 2], pstyle}][[1;;Length[fun]]];
	Show[MapThread[ParaPlotTangentPlane[#1,{x,x0,x1},{y,y0,y1}, tanpts, PlotStyle->{#2}, TangentPointStyle-> {#3}, 
			TangentPlaneStyle -> {#4}, opts]& , {fun, pstyle, tptstyle, tplstyle}]]
	]

(* 12-16-11: Removed Limit *)	(* Added newfun to fix problem with interaction of Surd and Abs on 03-18-16 -- What was the problem?? *)
ParaPlotTangentPlane[fun_, {u_, u0_, u1_}, {v_, v0_, v1_}, tanpts_List, opts___?OptionQ] := 
	Module[{print, tpts, planesize, tpstyle, ptstyle, graph, funvalues, showpts, funlins, plot, plotfun, ranges, ps}, 
	{print, planesize} = {PrintDisplay, PlaneSize} 
			/. {opts} /. Options[ParaPlotTangentPlane]; 
	tpts = If[Head[tanpts[[1]]] === List, tanpts, {tanpts}]; 
	If[MemberQ[planesize, 0], Return[Message[WrongSize::size]]]; 
(*newfun = fun /. Abs[stuff_]:>PiecewiseExpand[Abs[stuff],Element[{u,v},Reals]];	  (*04-08-16 *)

Print[newfun];*)


	If[print, Print[ParaTangentPlane[fun (*newfun//Evaluate*),{u,v},tpts, opts]]];

	funlins = ParaTangentPlane[fun (*newfun//Evaluate*),{u,v},tpts, Normalize -> True, opts];


	funlins = Map[If[(FreeQ[#, {_,{0,0,0}} | {{0,0,0},_} | Infinity | DirectedInfinity[___] | Indeterminate | Null](* || flag*)),
			#, Print["No tangent plane."];Null]&, funlins];
	funvalues = Function[{u, v}, fun (*newfun//Evaluate*)] @@ #& /@ tpts; 

	ptstyle = setps5[ParaPlotTangentPlane,TangentPointStyle,Length[tpts],opts];	


	showpts = Graphics3D[Flatten/@MapThread[Flatten[{#1, Point[#2]}]&, {ptstyle, N[funvalues]}], 
			FilterRules[{opts}, Options[Graphics3D]], PlotRange->All];

	If[MatchQ[funlins, {Null} | {}], Return[]];
	{tpts, funlins} = Transpose[Select[Transpose[{tpts,funlins}], 
			FreeQ[#, DirectedInfinity[___]|Indeterminate| Null, Infinity] && (MemberQ[#,u|v,Infinity]&)]];


	{tpstyle, ptstyle, planesize, ps} = 
		setps5[ParaPlotTangentPlane, {TangentPlaneStyle, TangentPointStyle, PlaneSize, PlotStyle}, 
				{Length[funlins],Length[tpts],Length[tpts],1}, opts];

	{plot, plotfun} = If[(Flag3 /. {opts}) === Plot3D, {Plot3D, Last[fun]}, {ParametricPlot3D, fun}];

	graph = plot[If[(Flag2 /. {opts})===Tooltip, Tooltip[plotfun], plotfun]//Evaluate, {u, u0, u1}, {v, v0, v1}, PlotStyle->ps, 
		Evaluate[Sequence@@FilterRules[{opts},Options[plot]]], BoxRatios -> Automatic]; 

	ranges = Max[Abs[Apply[Subtract, Drop[PlotRange /. AbsoluteOptions[graph, PlotRange], -1], {1}]]]; 

	Show[
		graph, 
		MapThread[ParametricPlot3D[Evaluate[#1],
			Flatten[{u, Last[#3] ranges {-.1,.1}}]//Evaluate,Flatten[{v, Last[#3] ranges {-.1,.1}}]//Evaluate, 
				PlotStyle -> #2, Mesh->None]&, 
				{N[funlins],  tpstyle, planesize}], (*Made PlaneSize smaller 11-27-16*)
		showpts]

	]


TangentPlane[fun_, {x_, y_}, tpts_?MatrixQ, opts___?OptionQ] := 
	(	If[Length[Flatten[{fun}]]>1, Return[Message[Command::onefunction]]];
	Map[TangentPlane[fun, {x, y}, #, opts] &, tpts])

(*TangentPlane[Tooltip[fun_], rest__]:=
(Message[Command::notooltip]; TangentPlane[fun, rest])*)
	
TangentPlane[fun_, {x_, y_}, {x0_,y0_}, opts___?OptionQ] := 
	Module[{deriv}, 
	If[Length[Flatten[{fun}]]>1, Return[Message[Command::onefunction]]];
	deriv = KPGrad[fun, {x,y}, {x0,y0}, (*PowerBehavior->Complex,*) opts]//Quiet;  (*Commented out: 04-08-16 *)
	If[FreeQ[deriv, DirectedInfinity[___] | Indeterminate | Null],
		Function[{x,y}, fun][x0,y0] + ({x,y}-{x0,y0}).deriv//Chop,
		Message[KPGrad::nograd]; deriv]
	]

PlotTangentPlane[Tooltip[fun_], {x_,x0_,x1_}, {y_,y0_,y1_}, tanpts_List, opts___?OptionQ] :=
	PlotTangentPlane[fun,{x,x0,x1},{y,y0,y1},tanpts,opts, Flag2->Tooltip, Flag3 -> Plot3D]
	
PlotTangentPlane[fun_List, {x_,x0_,x1_}, {y_,y0_,y1_}, tanpts_List, opts___?OptionQ] :=
	Module[{pstyle, tptstyle, tplstyle, clippingstyle},
	{tplstyle, tptstyle, pstyle} = setps5[PlotTangentPlane, {TangentPlaneStyle, TangentPointStyle, PlotStyle}, 15, opts];
	clippingstyle = setps5[Plot3D, ClippingStyle, Length[fun], opts]/.{None}->None;
	tplstyle = MapThread[Flatten[{#1, #2}] &,{ColorData[97][#] & /@ Swap[Range[15], 1, 2], tplstyle}][[1;;Length[fun]]];
	tptstyle = MapThread[Flatten[{#1, #2}] &,{ColorData[97][#] & /@ Swap[Range[15], 1, 2], tptstyle}][[1;;Length[fun]]];
	pstyle = MapThread[Flatten[{#1, #2}] &,{ColorData[97][#] & /@ Swap[Range[15], 1, 2], pstyle}][[1;;Length[fun]]];
	Show[MapThread[PlotTangentPlane[#1,{x,x0,x1},{y,y0,y1},tanpts, PlotStyle -> {#2}, TangentPointStyle-> {#3}, 
			TangentPlaneStyle -> {#4}, ClippingStyle -> #5, opts, Flag3 -> Plot3D]& , 
		{fun, pstyle, tptstyle, tplstyle, clippingstyle}]]
	]
	
PlotTangentPlane[fun_, {x_,x0_,x1_}, {y_,y0_,y1_}, tanpts_List, opts___?OptionQ] :=
	Module[{tpts, funvalues, eqns},

	tpts = If[Head[tanpts[[1]]] === List, tanpts, {tanpts}]; 
	tpts = Map[#[[1;;2]]&,tpts];
	If[PrintDisplay /. {opts} /. Options[PlotTangentPlane],
		funvalues = Function[{x,y}, {x,y,fun}]@@@tpts;
		eqns = TangentPlane[fun,{x,y},tpts, Normalize -> False, opts];  (*Made first arguement a list *)
		MapThread[
			{Print["Point: ",#1], Print["Plane: z = ", #2]}&, {funvalues, eqns}]];
	ParaPlotTangentPlane[{x, y, fun}, {x, x0, x1}, {y, y0, y1}, tpts, PrintDisplay -> False, (*Flag -> True,*) opts, Flag3 -> Plot3D]
	]


PlotRungeKutta[anything__, opts___?OptionQ] := PlotEuler[anything, Flag -> RungeKutta, opts]
RungeKutta[anything__, opts___?OptionQ] := EulerMethod[anything, Flag -> RungeKutta, DrawGraph->False, opts];
(*
PlotRungeKutta5[anything__, opts___?OptionQ] := PlotEuler[anything, Flag -> RungeKutta5, opts]; 
RungeKutta5[anything__, opts___?OptionQ] := EulerMethod[anything, Flag -> RungeKutta5, DrawGraph->False, opts];
*)

EulerMethod[eqn_, vars_, {t_, t0_, t1_, dt_:Automatic}, opts___?OptionQ] := 
	PlotEuler[eqn, vars, {t, t0, t1, dt}, Flag->Euler, DrawGraph->False, opts]
(*
EulerMethod[eqn_, vars_, {t_, t0_, t1_, dt_:Automatic}, opts___?OptionQ] := 
	Module[{wp, table, ifun, wpp, header, iter, dtt, data}, 
(*	{wp, table} = {WorkingPrecision, Table} /. {opts} /. Options[EulerMethod];*)
	wp = WorkingPrecision /. {opts} /. Options[EulerMethod];
	If[wp === MachinePrecision, wpp = $MachinePrecision, wpp = wp];  (* 08-14-17   Removed Ceiling from wpp*)
	dtt = N[If[dt === Automatic, (t1-t0)/20, dt],wpp];
	ifun = PlotEuler[eqn, vars, {t, t0, t1, dtt}, DrawGraph -> False, PrintDisplay->False, opts];
	If[ifun === Null, Return[]];

	iter = {t, t0, t1, dtt};
	If[Last[iter] < 0, 
			iter = Swap[iter,2,3]];
	Quiet[
			Check[data = Table[N[Flatten[{N[t], (vars /. ifun)}],wpp], iter//Evaluate], 
                Message[EulerMethod::dmval],
                InterpolatingFunction::dmval],
           InterpolatingFunction::dmval];
	data
(*
	header = PadRight[{t}, Length[Flatten@ifun]+1, RotateRight[vars,1]];
	If[table,		
		TableForm[data,
			FilterRules[{opts},Options[TableForm]]//Evaluate,
			TableHeadings -> {None, header}, TableAlignments -> Left],
    	data]*)
    ]
*)

PlotEuler[eqn_, vars:{_Symbol[_Symbol]..}, {t_, tmin_, tmax_, dt_:Automatic}, opts___?OptionQ] := 
	Module[{iv, graph, data1, ivg, oorts, len, ps, ptstyle, psdata, shpls, data}, 
	{iv, graph} = {InitialValue, DrawGraph} /. {opts} /. Options[PlotEuler]; 
	data1 = DeleteCases[eqn, _[(b_)?(NumberQ[N[#1]] & )] == (c_) | (c_) == _[(b_)?(NumberQ[N[#1]] & )]]; 
	ivg = Complement[eqn,Flatten[{data1}]];
   	If[VectorQ[Flatten[iv], NumberQ[N[#]] & ], iv = Map[Thread[Head[First[vars]][tmin] == #] &, iv]];
	If[Not[ivg === {}], iv = Join[iv, If[Length[vars]==1, List /@ ivg, {ivg}]]];
	If[Length[vars] == Length[Flatten[{iv}]], len = Length[vars];iv = {iv}, len = Length[Flatten[iv]]];
	oorts = (Orbit || TimeState) /. {opts} /. Options[PlotEuler];
	len = Length[iv]*Length[vars];

	shpls = ShowPlots /. {opts} /. Options[PlotEuler];
	shpls = If[shpls === All, vars, vars[[Flatten[(Position[vars, #1] & ) /@ Flatten[{shpls}]]]]]; 	
	If[UnsameQ[shpls, NewUnsortedUnion[shpls]],Return[Message[ShowPlots::repeat]]];

	psdata = {Hue[0.67, 0.6, 0.6], Hue[0.906, 0.6, 0.6], Hue[0.142, 0.6, 0.6], Hue[0.378, 0.6, 0.6]};
	psdata = If[len <= 4, psdata[[1;;len]], PadRight[psdata, len, psdata]];

	ps = setps5[PlotEuler,  PlotStyle, len, opts];
	ps = MapThread[Flatten[{#1,#2}]&,{psdata,ps}];
	ps = Partition[ps, Length[shpls]];
	ps = ps[[1;;Length[iv]]];	
	
	ptstyle = setps5[PlotEuler,  PointStyle, If[oorts === True, Length[iv], len], opts];
	ptstyle = If[oorts === True, ptstyle, Partition[ptstyle, Length[shpls]]];
	ptstyle = ptstyle[[1;;Length[iv]]];
	If[MemberQ[ptstyle, PointSize[_],Infinity], 
			Message[PlotJump::ptsize]; 
			ptstyle = (ptstyle /. (PointSize[_] -> AbsolutePointSize[7]))];	

	data = MapThread[Ploteuler[Flatten[{data1, #1}], vars, {t, tmin, tmax, dt}, 
		InitialValue -> {}, PlotStyle -> #2, PointStyle -> {#3}, opts(*, MeshStyle -> Black(*#2*)*)]&, {iv, ps, ptstyle}];

	If[Not[FreeQ[data,$Failed]],Return[]];
	If[graph === True, 
		Show[data, Sequence@@FilterRules[{opts},Options[Graphics]]],
		(*Flatten /@*) Flatten[data,1]]   (*08-14-17 *)
	] 

PlotEuler[eqn_, vars:(_Symbol[_Symbol]), {t_,t0_,t1_,dt_:Automatic}, opts___?OptionQ] :=
	PlotEuler[Flatten[{eqn}], {vars}, {t, t0, t1, dt}, opts]

PlotEuler[list:{_Equal..}, vars:{_Symbol..}, {t_,t0_,t1_,dt_:Automatic}, opts___?OptionQ] := 
	PlotEuler[list, #[t]& /@ vars, {t,t0,t1,dt}, opts]

PlotEuler[list:{_Equal..}, var_Symbol, {t_,t0_,t1_,dt_:Automatic}, opts___?OptionQ] := 
	PlotEuler[list, {var[t]}, {t,t0,t1,dt}, opts]
	

Ploteuler[list:{_Equal..}, vars:{_Symbol[_Symbol]..}, {t_, t0_, t1_, dt_}, opts___?OptionQ] := 
	Module[{print, graph, shpls, dsolve, ndsolve, flag, orbit, timestate, ptstyle, slist, f, vals, startt, data, Iter,
		eplot, graphics = Graphics, wp, dtt, t11, derivs}, 
(*If[wp === MachinePrecision, wpp = Ceiling[$MachinePrecision], wpp = wp];   (*Restored   Removed Ceiling  08-14-17 *)*)

	If[t0 > t1, Message[PlotEuler::order]; Return[$Failed]];
	{print, graph, shpls, dsolve, ndsolve, flag, orbit, timestate, wp} = 
     		{PrintDisplay, DrawGraph, ShowPlots, DSolve, NDSolve, Flag, Orbit, TimeState, WorkingPrecision} /. 
				{opts} /. Options[PlotEuler]; 

	ptstyle = PointStyle /. {opts};
	derivs = D[vars, t]; 

	If[Not[Length[list] == 2 Length[vars]], Message[PlotEuler::eqnsvars]; Return[$Failed]];
	If[Or @@ (FreeQ[list, #1] & ) /@ derivs, Message[PlotEuler::badvar]; Return[$Failed]]; 
	If[Not[Plus @@ OrderODE[list, vars, t]== Length[vars]],Message[PlotEuler::degree]; Return[$Failed]];

	Off[Solve::svars, Solve::ifun]; 
	slist = Thread[derivs == (derivs /. Flatten[(Solve[#1, derivs] & ) /@ list])]; 
	On[Solve::svars, Solve::ifun]; 

	f = Function[Evaluate[Join[{t}, Head /@ vars]], 
      		Evaluate[Flatten[Cases[slist, Derivative[1][_][t] == b_ -> b] /. PointsToRules[Head /@ vars, vars]]]]; 

	vals = Flatten[(Cases[list, #1[(b_)?(NumberQ[N[#1]] & )] == c_ | c_ == #1[(b_)?(NumberQ[N[#1]] & )] -> {c, b}] & ) /@ Head /@ vars, 1]; 
	If[vals === {}, Return[Message[InitialValue::noiv]], {vals, startt} = Transpose[vals]]; 

	If[Not[Length[Union[startt]] == 1], Return[Message[PlotEuler::initialcond, t, startt]], startt = First[startt]]; 
	If[N[startt > t1 || startt < t0], Return[Message[PlotEuler::initialval, t, startt, {t, t0, t1}]]]; 
	If[t1-t0 == 0, Return[Message[Plot::plld,t,{t,t0,t1}]]];

    dtt = If[dt === Automatic, N[(t1 - t0)/20,wp] (*(t1 - t0)/(10*Floor[t1 - t0])*), N[dt,wp]];   (*Removed   Added N[,wpp]  08-14-17 *)
	t11 = If[dtt < 0, t0, t1]; 




    Switch[flag, 
     	Euler, 
			Iter[{tt_, yy_List}] := 
				{tt, yy} + dtt*{1, f @@ Flatten[{tt, yy}]}, 
     	RungeKutta, 
			Iter[{tt_, yy_List}] := 
				Module[{k1, k2, k3, k4}, 
     				k1 = f @@ Flatten[{tt, yy}]*dtt; 
        			k2 = f @@ Flatten[{tt + dtt/2, yy + k1/2}]*dtt; 
        			k3 = f @@ Flatten[{tt + dtt/2, yy + k2/2}]*dtt; 
        			k4 = f @@ Flatten[{tt + dtt, yy + k3}]*dtt; 
        			{tt, yy} + {dtt, (k1 + 2*k2 + 2*k3 + k4)/6}]];

	data = Flatten /@ NestList[N[Iter[#], wp]& , {startt, vals}, Ceiling[(t11 - startt)/dtt]];   (* 08/14/17 Changed wpp to wp *)
	data =   (data[[All,{1, #1}]] & ) /@ Range[2,1+Length[vals]];
If[Not[graph] && MatchQ[flag,  Euler | RungeKutta (*| RungeKuppa5*)], 
	data,
	If[print, 
		If[orbit, 
			Print[{vars,Transpose[Last /@ Transpose /@data]}],
			If[timestate, 
				Print[{Flatten[{vars,t}], MapThread[RotateLeft[Flatten[{#1,#2[[2]]}]]&, data]}],
				Print[Transpose[{({t, #1} & ) /@ vars, N[#,wp]&/@data}]]]  (* 08-15-17  Changed wpp to wp *) (*Changed NumberForm to N *)
				]];
	If[ Not[graph],

		MapThread[{#1->Interpolation[#2,InterpolationOrder->1][t]}&, {vars,data}],
		If[orbit, 
			Which[
				Length[vars] == 2,
					eplot = DirParametricPlot[Evaluate[Interpolation[#,InterpolationOrder->1][t]& /@ data], {t, startt (*t0*), t11 (*t1*)}, 
								DrawPoint -> Transpose[Last /@ Transpose /@data], DrawPointStyle->ptstyle, opts, DrawArrowheads->False, PlotPoints->Floor[(t11-startt)/.1]],
    			Length[vars] == 3,
					eplot = DirParametricPlot3D[Evaluate[Interpolation[#,InterpolationOrder->1][t]& /@ data], {t, startt (*t0*), t11 (*t1*)}, 
								DrawPoint -> Transpose[Last /@ Transpose /@data], DrawPointStyle->ptstyle, opts, DrawArrowheads->False, PlotPoints->Floor[(t11-startt)/.1]];

					graphics = Graphics3D,	
      			True, 
					Message[Orbit::orderode, Length[vars]]; Return[$Failed]],
			If[timestate,
				If[Not[Length[vars]==2], Return[Message[TimeState::orderode, Length[vars]]]];
				eplot = DirParametricPlot3D[Evaluate[Flatten[{Interpolation[#, InterpolationOrder -> 1][t] & /@ data, t}]],{t, t0, t11 (*t1*)},  
							DrawPoint -> MapThread[RotateLeft[Flatten[{#1,#2[[2]]}]]&, data], opts, DrawArrowheads->False];
				graphics = Graphics3D,
				shpls = If[shpls === All, Range[Length[vars]], Flatten[(Position[vars, #1] & ) /@ Flatten[{shpls}]]];

				eplot =	ListLinePlot[data[[shpls]], 
							PlotMarkers->Graphics[Flatten[{#, Point[{0,0}]}]& /@ ptstyle],
							Sequence@@FilterRules[{opts},Options[ListLinePlot]]//Evaluate
						]
				]
			];

	Show[
		eplot,
		If[dsolve === True, 
        		PlotDSolve[list, vars, {t, t0, t1},  InitialValue -> {}, PrintDisplay -> False, opts(*, PlotPoints->Floor[(t1-t0)/Abs[dtt]]*)], 
				graphics[{}]], 
		If[ndsolve === True, 
				PlotNDSolve[list, vars, {t, t0, t1},  InitialValue -> {}, PrintDisplay -> False, opts(*, PlotPoints->Floor[(t1-t0)/Abs[dtt]]*)], 
				graphics[{}]], 
		Sequence@@FilterRules[{opts}, Options[graphics]],PlotRange->Automatic]
		]
	]]	


 Plotdsolve[list:{_Equal..}, vars:{_[_Symbol]..}, {t_, tmin_, tmax_}, opts___?OptionQ] := 
  	Module[{flag, iv, shpls, print, orbit, data1, ivg, ivt, esoln, dplot, timestate, graph, ordersys, test}, 
  	flag = Flag /. {opts}; 

  	{iv, shpls, print, orbit, timestate, graph} = 
		{InitialValue, ShowPlots, PrintDisplay, Orbit, TimeState, DrawGraph} 
  			/. {opts} /. Options[PlotDSolve]; 

  	data1 = DeleteCases[list, _[(b_)?(NumberQ[N[#1]] & )] == c_ | c_ == _[(b_)?(NumberQ[N[#1]] & )]];
	
	ordersys = Plus @@ Max /@ (Outer[OrderODE[#1, #2, t] & , list, vars] /. -Infinity -> 0);

	If[ordersys == Length[Flatten[{iv}]], iv = {Flatten[{iv}]}];

	iv = If[(ivg = Complement[list,data1]) === {},iv, Join[iv,{ivg}]];

 	If[Not[MatchQ[iv, {}]], 
						
		esoln = (flag[Flatten[{data1, #1}], vars, If[flag === DSolve, t, {t, tmin, tmax}],
					Sequence@@FilterRules[{opts},Options[flag]]//Evaluate] & ) /@ iv;
		]; 

	If[shpls === All, shpls = vars, shpls = Flatten[{shpls}]]; 
	If[UnsameQ[shpls, NewUnsortedUnion[shpls]],Return[Message[ShowPlots::repeat]]];

	ivt = Flatten[Map[# /. (a_[b_]==c_) -> b&, Table[Map[Cases[iv,#[a_]==b_,Infinity]&,Head/@shpls], {Length[iv]} ]],1];

	If[Not[Apply[And, tmin <= # <= tmax& /@ ivt]],Return[]];

  	If[esoln === {}, Return[Message[DSolve::nosoln]]]; 
	esoln = Flatten[esoln,1];

  	If[FreeQ[esoln, flag], 
		If[Not[graph], Return[esoln]];
		If[print, If[flag === DSolve, Print[esoln], Message[PlotNDSolve::noprint]]]; 
		If[orbit && timestate, Message[plotdsolve::orbitST]; timestate = False];
		If[orbit || timestate,
			Which[
				Length[vars] == 1,
					Switch[OrderODE[First[list],First[vars],t],
						2, 	esoln = Flatten /@ ({#,D[#,t]}&/@esoln);
							If[timestate, 
								shpls = Flatten[{vars,D[vars,t],t}]; dplot = DirParametricPlot3D,
								shpls = Flatten[{vars,D[vars,t]}]; dplot = DirParametricPlot],
						3, 	esoln = Flatten /@ ({#,D[#,t],D[#,{t,2}]}&/@esoln); 
							shpls = Flatten[{vars,D[vars,t],D[vars,{t,2}]}]; dplot = DirParametricPlot3D,
						_, Return[If[orbit, 
								Message[Orbit::orderode, OrderODE[First[list], First[vars], t]], 
								Message[TimeState::orderode, OrderODE[First[list], First[vars], t]]]]],
				Length[vars] == 2,
					If[timestate,
						If[Not[(test=Plus@@OrderODE[list, vars, t])==2], Return[Message[TimeState::orderode, test]]];
						shpls = Flatten[{vars,t}]; dplot = DirParametricPlot3D,
						
						Switch[Plus@@(test = OrderODE[list, vars, t]),
							2, 
								dplot = DirParametricPlot; shpls = vars,
							3, 
								dplot = DirParametricPlot3D;
								esoln = Sort[#, Position[vars,First[#1]][[1,1]] <= Position[vars,First[#2]][[1,1]]& ] & /@ esoln;
								If[test[[1]]==1, 
										shpls = Flatten[{vars, D[vars[[2]],t]}];
										esoln = Flatten[{#,D[#[[2]],t]}]&/@esoln,
										shpls = Insert[vars, D[vars[[1]],t],2];
										esoln = {#[[1]],D[#[[1]],t],#[[2]]}&/@esoln;

									],
							_, Return[Message[Orbit::orderode, Plus@@test]]
							]
						],
				Length[vars] == 3,
					If[timestate===True, Return[Message[TimeState::orderode, Length[vars]]]];
					If[(test = Plus@@OrderODE[list, vars, t])>3, Return[Message[Orbit::orderode, test]]];
					shpls = vars;
					dplot = DirParametricPlot3D,	
      			True, 
					Return[If[orbit, 
								Message[Orbit::orderode, Length[vars]], 
								Message[TimeState::orderode, Length[vars]]]]
				];

On[NDSolve::mxst, NDSolve::ndsz, NDSolve::nderr, InterpolatingFunction::dmval]; (* Added Off and On on 7-1-2012*)
			dplot[shpls /. esoln//Evaluate, {t, tmin, tmax}, opts, DrawArrowheads -> False],			(*Added Evaluate on 6-5-09*)		

			PlotJump[Evaluate[Flatten[shpls /. #& /@ esoln]], {t, tmin, tmax}, opts]],
            Message[dsolve::nosoln]]
	]

 

PlotDSolve[eqn_, y_[t_]|{y_[t_]}, {t_, tmin_, tmax_}, opts___?OptionQ] := 
	Module[{iv}, 
	iv = InitialValue /. {opts} /. Options[PlotDSolve];
	iv = If[Head[iv] === List, iv, {iv}];
	If[Not[iv == {}] && (VectorQ[iv, NumericQ] || MatrixQ[iv, NumericQ]), Return[Message[PlotDSolve::wrongiv]],
		Plotdsolve[Flatten[{eqn}], {y[t]}, {t, tmin, tmax}, InitialValue -> iv, Flag -> DSolve, opts]]];

PlotDSolve[list:{_Equal..}, vars:{_[_Symbol]..},  {t_,tmin_,tmax_}, opts___?OptionQ] :=
	Plotdsolve[list, vars, {t,tmin,tmax}, Flag -> DSolve, opts]
	
PlotDSolve[list:{_Equal..}, vars_List, {t_,t0_,t1_}, opts___?OptionQ] := 
	PlotDSolve[list, #[t]& /@ vars, {t,t0,t1}, opts]

PlotDSolve[eqn_, var_, {t_,t0_,t1_}, opts___?OptionQ] := 
	PlotDSolve[eqn, var[t], {t,t0,t1}, opts]


PlotNDSolve[eqn_, y_[t_]|{y_[t_]}, {t_, tmin_, tmax_}, opts___?OptionQ] := 
	Module[{iv}, 
	iv = InitialValue /. {opts} /. Options[PlotDSolve];
	iv = If[Head[iv] === List, iv, {iv}];
	If[Not[iv == {}] && (VectorQ[iv, NumericQ] || MatrixQ[iv, NumericQ]), Return[Message[PlotDSolve::wrongiv]],
		Plotdsolve[Flatten[{eqn}], {y[t]}, {t, tmin, tmax}, InitialValue -> iv, Flag -> NDSolve, opts]]];

PlotNDSolve[list:{_Equal..}, vars:{(_[_Symbol])..},  {t_,tmin_,tmax_}, opts___?OptionQ] :=
	Plotdsolve[list, vars, {t,tmin,tmax}, Flag -> NDSolve, opts]
	
PlotNDSolve[list:{_Equal..}, vars_List, {t_,t0_,t1_}, opts___?OptionQ] := 
	Plotdsolve[list, #[t]& /@ vars, {t,t0,t1}, Flag -> NDSolve, opts]

PlotNDSolve[eqn_, var_, {t_,t0_,t1_}, opts___?OptionQ] := 
	PlotNDSolve[eqn, var[t], {t,t0,t1}, opts]


PlotSlopeField[deqn_, {x_,x0_,x1_}, {y_,y0_,y1_}, opts___?OptionQ] := 
	Module[{data1, euler, rk, iv, dsolve, print,ls, de, dx, dy, ps, 
			nullcline, nullc, nullstyle, dir, ptstyle,k, linescale, aratio, ndsolve, lpts, esolnE, esolnD, esolnRK, esolnRK5, valpts, vals, sfield}, 
	{euler, estyle, rk, rkstyle, iv, dsolve, ndsolve, print, lpts, linescale, ls, nullc, nullstyle, dir, aratio} = 
		{Euler, EulerStyle, RungeKutta, RungeKuttaStyle, InitialValue, DSolve, NDSolve, PrintDisplay, LinePoints, LineScale, LineStyle, Nullcline, 
			NullclineStyle, Direction, AspectRatio} /.  {opts} /.  Options[PlotSlopeField];

	data1 = DeleteCases[Flatten[{deqn}],(y[b_?(NumberQ[N[#]]&)] == c_ | c_ == y[b_?(NumberQ[N[#]]&)])];

	If[Not[OrderODE[First[data1],y[x],x]==1], Return[Message[PlotSlopeField::order,First[data1]]]];

	de = First[Flatten[Solve[data1, y'[x]]/.Rule -> Equal]]; 

	lpts = If[Length[Flatten[{lpts}]] == 2, lpts, {lpts, lpts}]; 

	{dx, dy} = {x1 - x0, y1 - y0}/lpts//N;
  	Off[Power::infy, Infinity::indet];  

	If[euler || rk || dsolve || ndsolve, 
		iv = If[Head[iv] === List, iv, Flatten[{iv}]]; 
		If[VectorQ[iv, NumberQ[N[#]]&], iv =  (y[x0] == #) & /@ iv];
		If[MatrixQ[iv, NumberQ[N[#]]&], iv =  (y[#[[1]]] == #[[2]]) & /@ iv];
		iv = If[data1 === deqn, iv, Join[iv, Complement[Flatten[{deqn}],data1]] ]; 
		If[iv === {}, Return[Message[InitialValue::noiv]]]; 
		ps = setps5[PlotSlopeField, PlotStyle, Length[iv], opts]; 
		ptstyle = setps5[PlotSlopeField, PointStyle, Length[iv], opts]; 
		If[euler,
			esolnE = Table[MapThread[
				Ploteuler[Flatten[{de, #1}], {y[x]}, {x, x0, x1, k}, 
					PlotStyle -> #2, LineStyle -> #2, PointStyle -> {Flatten[{#2,#3}]}, 
					InitialValue -> {}, (*new*) PlotRange->{{x0,x1},{y0,y1}}, opts]&, {iv, ps, ptstyle}],
				Switch[dir, (*Right*) 1,{k,dx,dx}, (*Left*) -1,{k,-dx,-dx},
							_,{k,-dx,dx,2dx}]//Evaluate],
			esolnE = Graphics[{}]];
		If[rk,
			esolnRK = Table[MapThread[
				PlotEuler[Flatten[{de, #1}], {y[x]}, {x, x0, x1, k}, 
					PlotStyle -> #2, LineStyle -> #2, PointStyle -> {Flatten[{#2,#3}]}, 
					InitialValue -> {}, (*new*) PlotRange->{{x0,x1},{y0,y1}}, Flag->RungeKutta, opts]&, {iv, ps, ptstyle}],
				Switch[dir, (*Right*) 1,{k,dx,dx}, (*Left*) -1,{k,-dx,-dx},
							_,{k,-dx,dx,2dx}]//Evaluate],
			esolnRK = Graphics[{}]];


		If[dsolve || ndsolve, 
			esolnD = Plotdsolve[{de}, {y[x]}, {x,x0,x1}, Flag -> If[dsolve,DSolve, NDSolve],
				InitialValue -> iv, PlotRange->{{x0,x1},{y0,y1}}, opts],
			esolnD = Graphics[{}] ],
		esolnE = esolnRK = esolnRK5 = esolnD = Graphics[{}]];

	nullstyle = setps5[PlotSlopeField, NullclineStyle, Max[1,Length[iv]], opts];

	Switch[nullc,
		False, nullcline = Graphics[{}], 
		Automatic | True, 
			nullcline = NSolve[de[[2]]==0,y[x]]; (*Possibly change Solve to NSolve in this line and the next*)
			
			If[FreeQ[nullcline,NSolve] && nullcline =!= {}, 
				nullcline = Plot[y[x]/.nullcline//Evaluate,{x,x0,x1}, 
					PlotStyle -> nullstyle, Sequence@@FilterRules[{opts}, Options[Plot]]//Evaluate],
				nullcline = Graphics[{}];
				Message[PlotSlopeField::nonullcline]
				],
		_, nullcline = Plot[nullc//Evaluate,{x,x0,x1},  
					PlotStyle -> nullstyle, Sequence@@FilterRules[{opts}, Options[Plot]]//Evaluate] ];

  	On[Power::infy, Infinity::indet];  	
Off[Axes::axes, Ticks::ticks];	
	aratio = AspectRatio /. AbsoluteOptions[Show[esolnE, esolnD, nullcline, PlotRange->{{x0,x1},{y0,y1}}, AspectRatio -> aratio],AspectRatio];		
On[Axes::axes, Ticks::ticks];
  	valpts = Flatten[Table[{{x, y}, {1,de[[2]]}/Norm[{1,de[[2]]}]/.  y[x] -> y}//Evaluate, {x, x0,  x1, dx}, {y, y0, y1, dy}], 1];

  	valpts =  Select[valpts,  VectorQ[#1[[2]], NumericQ[#1] && Im[#1] == 0 & ] (*&& minsize < Norm[#1[[2]]] < maxsize*) & ];  
  	If[valpts === {},  Return[Message[PlotVectorField::maxsize, maxsize]]]; 
  	{valpts,  vals} = Transpose[valpts];  			
	sfield = SlopeLines[Transpose[{valpts, linescale*vals + valpts}], {{x0,x1},{y0, y1}}, aratio, ls, linescale];					

	Show[Graphics[sfield], esolnE, esolnRK, esolnD, nullcline, PlotRange->{{x0,x1},{y0,y1}},  AspectRatio -> aratio,
		Sequence@@FilterRules[{opts},Options[Graphics]], Frame -> True, Axes->False, PlotRangeClipping->True] 
	] 


PlotVectorField[{fx_, fy_}?VectorQ, {x_, x0_, x1_}, {y_, y0_, y1_}, (opts___)? OptionQ] :=  
	Module[{iv, pinterval,  ndsolve,  dpts, dptstyle, solnplot, gx, gy, soln},  
	{iv, pinterval, ndsolve, dpts, dptstyle} =  
			{InitialValue, PlotInterval, NDSolve, DrawPoint,  PointStyle} /. 
     			{opts} /. Options[PlotVectorField]; 
If[Not[FreeQ[{opts},DSolve]],Return[Message[PlotVectorField::dsolve]]];  			

If[Not[iv == {}] && (VectorQ[iv, NumericQ] || MatrixQ[iv, NumericQ]), Return[Message[PlotDSolve::wrongiv]]];
  	If[ndsolve === True && (iv === {} || pinterval === {}), Return[Message[PlotVectorField::ndsolve]]]; 
  	If[ndsolve,
   		Switch[Length[pinterval],
     			2, t=Unique[]; pinterval = Flatten[{t, pinterval}], 
     			3, t = First[pinterval],
     			_, Return[Message[PlotVectorField::ndsolve]]]; 
   		];

    If[ndsolve, 
   		{gx, gy} = {Function[{x, y}, fx],  Function[{x, y}, fy]}; 
		soln = (NDSolve[ 
			Flatten[{Derivative[1][x][t] == gx[x[t], y[t]], Derivative[1][y][t] == gy[x[t], y[t]], #1}], {x[t], y[t]}, pinterval] & ) /@ iv;  
		solnplot = DirParametricPlot[Evaluate[Flatten /@ ({x[t], y[t]} /. soln)], Evaluate[pinterval], 
     				Sequence @@ FilterRules[{opts}, {Options[ParametricPlot], Options[DirParametricPlot]}] // Evaluate, ArrowSize->Medium, DrawArrowheads -> False], 
   		solnplot = (*Graphics[{}]*) DirParametricPlot[{},{t,0,1},Evaluate[Sequence@@FilterRules[{opts},Options[DirParametricPlot]]]]];  

    Show[	VectorPlot[{fx, fy}, {x, x0, x1}, {y, y0, y1}, Evaluate[Sequence @@ FilterRules[Flatten[{opts}], Options[VectorPlot]]]], 
			solnplot, 
   		Sequence @@ FilterRules[Flatten[{opts}], Options[Graphics]](*, PlotRangePadding -> .03*)]
]

PlotVectorField3D[{fx_,fy_,fz_}?VectorQ, {x_,x0_,x1_}, {y_,y0_,y1_}, {z_,z0_,z1_}, opts___?OptionQ] :=
	Module[{iv, pinterval,  ndsolve, solnplot, gx, gy, gz, soln},  
	{iv, pinterval, ndsolve} =  
			{InitialValue, PlotInterval, NDSolve} /. 
     			{opts} /. Options[PlotVectorField]; 

If[Not[iv == {}] && (VectorQ[iv, NumericQ] || MatrixQ[iv, NumericQ]), Return[Message[PlotDSolve::wrongiv]]];
  	If[ndsolve === True && (iv === {} || pinterval === {}), Return[Message[PlotVectorField::ndsolve]]]; 
  	If[ndsolve,
   		Switch[Length[pinterval],
     			2, t=Unique[]; pinterval = Flatten[{t, pinterval}], 
     			3, t = First[pinterval],
     			_, Return[Message[PlotVectorField::ndsolve]]]; 
   		];

    If[ndsolve, 
   	{gx,gy,gz} = {Function[{x,y,z},fx],Function[{x,y,z},fy],Function[{x,y,z},fz]};

		soln = Map[NDSolve[Flatten[{x'[t] == gx[x[t],y[t],z[t]], y'[t] == gy[x[t],y[t],z[t]], z'[t] == gz[x[t],y[t],z[t]], # }], 
			{x[t],y[t],z[t]}, pinterval]&, iv];
		solnplot = DirParametricPlot3D[Flatten /@ ({x[t],y[t],z[t]}/.soln), pinterval//Evaluate, 
				Sequence@@FilterRules[{opts}, {Options[ParametricPlot3D],Options[DirParametricPlot3D]}]//Evaluate],
   		solnplot = (*Graphics3D[{}]*) DirParametricPlot3D[{},{t,0,1},Evaluate[Sequence@@FilterRules[{opts},Options[DirParametricPlot]]]]]; 
  
	Show[
		VectorPlot3D[{fx, fy, fz}, {x, x0, x1}, {y, y0, y1}, {z, z0, z1},Evaluate[Sequence @@ FilterRules[Flatten[{opts}], Options[VectorPlot3D]]]], 
		solnplot, 
			Sequence @@ FilterRules[Flatten[{opts}], Options[Graphics3D]](*, PlotRangePadding -> .03*)]
]

PlotGradientField[fun_, {x_, x0_, x1_}, {y_, y0_, y1_}, opts___]:=
	Module[{cplot,ppc, pvf},
	{cplot,ppc} = {ContourPlot,PlotPointsContour} /. {opts} /. Options[PlotGradientField];
	pvf = PlotVectorField[KPGrad[fun,{x,y}],{x,x0,x1},{y,y0,y1}, opts];

	Show[
		If[cplot,
			ContourPlot[fun, {x,x0,x1}, {y,y0,y1}, 
				PlotPoints->ppc, Sequence@@FilterRules[{opts}, Options[ContourPlot]]//Evaluate],
			Graphics[{},Options[pvf]]],
		pvf, 
			Sequence@@FilterRules[{opts},Options[Graphics]]]
	]


CylindricalPlot[polarf_, {r_,r0_,r1_}, {t_,t0_,t1_}, opts___?OptionQ] :=
	Module[{funl,picture,tsection,rsection, rstyle, tstyle, bdry, tsect, rsect},
	{tsection,rsection} = 
		{ThetaSection,RSection} /. {opts} /. Options[CylindricalPlot];
	{funl,tsection,rsection} = Map[Flatten,{{polarf},{tsection},{rsection}}];
	{rstyle,tstyle} = setps5[CylindricalPlot,{RSectionStyle,ThetaSectionStyle},Length/@{rsection,tsection},opts];

	funl = {r Cos[t],r Sin[t],#}& /@ funl;
	picture = ParametricPlot3D[funl,{r,r0,r1}, {t,t0,t1}, 
			Sequence@@FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate, Mesh->None];
	funl = Switch[Length[funl], 1, Join[{{r Cos[t], r Sin[t], 0}}, funl], 2, funl, _, Partition[funl,2,1]];
	If[tsection != {},
		bdry = Map[funl/.t -> #&,tsection];
		tsect = MapThread[PlotBand[#1, {r, Max[0, r0], r1}, CurveStyle->{#2}, RegionStyle -> {#2}]&, {bdry, tstyle}],  
		tsect = {}];	
	If[rsection != {},
		bdry = Map[funl/.r -> #&, rsection];
		rsect = MapThread[PlotBand[#1, {t,t0,t1}, CurveStyle->{#2}, RegionStyle -> {#2}]&, {bdry, rstyle}], 
		rsect = {}];
	Show[picture,tsect,rsect,Sequence@@FilterRules[{opts},Options[Graphics3D]]]]


CylindricalPlot[fun_List, {t_,t0_,t1_}, opts___?OptionQ] := 
	Module[{funl},
	funl = If[Head[fun[[1]]] === List, fun, {fun}];
	ParametricPlot3D[Evaluate[Map[
			{#[[1]] Cos[#[[2]]], #[[1]] Sin[#[[2]]],#[[3]]}&, funl]], {t,t0,t1}, opts]]


DirCylindricalPlot[fun_List, {t_,t0_,t1_}, opts___?OptionQ] := 
	Module[{funl},
	funl = If[Head[fun[[1]]] === List, fun, {fun}];
	DirParametricPlot3D[Evaluate[Map[
			{#[[1]] Cos[#[[2]]], #[[1]] Sin[#[[2]]], #[[3]]}&, funl]], {t,t0,t1}, opts]]

SphericalPlot[rho_, {p_,p0_,p1_}, {t_,t0_,t1_}, opts___?OptionQ] := 
	Module[{plotstyle, tsection, psection, rhol, bdry, picture, mesh, pstyle, tstyle, tsect, psect},
	{tsection, psection, mesh} = 
		{ThetaSection, PhiSection, Mesh} /. {opts} /. Options[SphericalPlot];
	rhol = # {Sin[p] Cos[t], Sin[p] Sin[t], Cos[p]}& /@ Flatten[{rho}];
	{tsection, psection, mesh} = Flatten/@{{tsection}, {psection}, {mesh}};
	{pstyle, tstyle, mesh} = setps5[SphericalPlot, {PhiSectionStyle, ThetaSectionStyle, Mesh}, Length/@{psection, tsection, rhol}, opts];	
	plotstyle = setps5[ParametricPlot3D, PlotStyle, Length[rhol], opts];
	plotstyle = MapThread[Flatten[{#1, #2}] &,{ColorData[97][#] & /@ If[Length[rhol]===1,{2},Swap[Range[Length[rhol]], 1, 2]], plotstyle}][[1;;Length[rhol]]];
	picture = MapThread[ParametricPlot3D[#1, {p,p0,p1}, {t,t0,t1}, Mesh -> Last[#2], PlotStyle->#3, 
			Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]]//Evaluate]&,{rhol, mesh, plotstyle}];
	rhol = Switch[Length[rhol], 1, Join[{{0,0,0}}, rhol], 2, rhol, _, Partition[rhol,2,1]];

	If[tsection != {},
		bdry = Map[rhol/.t -> #&, tsection];
		tsect = MapThread[PlotBand[#1, {p,p0,p1}, CurveStyle->{#2}, RegionStyle->{#2}]&, {bdry, tstyle}],
		tsect = {}];
	If[psection != {},
		bdry = Map[rhol/.p -> #&, psection];
		psect = MapThread[PlotBand[#1, {t,t0,t1}, CurveStyle->{#2}, RegionStyle -> {#2}]&, {bdry, pstyle}],
		psect = {}];
	Show[picture, tsect, psect, Sequence@@FilterRules[Evaluate[FilterRules[{opts}, Options[Graphics3D]]], Except[AxesLabel->_]]]]

	
SphericalPlot[fun_,{t_,t0_,t1_},opts___?OptionQ] := 
	Module[{funl},
	funl = If[Head[fun[[1]]] === List,fun,{fun}];	
	ParametricPlot3D[Evaluate[
		{#[[1]] Sin[#[[2]]] Cos[#[[3]]],#[[1]] Sin[#[[2]]] Sin[#[[3]]],#[[1]] Cos[#[[2]]]}& 
				/@ funl],{t,t0,t1}, Evaluate[Sequence@@FilterRules[{opts}, Options[ParametricPlot3D]]]]]


DirSphericalPlot[fun_, {t_,t0_,t1_}, opts___?OptionQ] := 
	Module[{funl},
	funl = If[Head[fun[[1]]] === List, fun, {fun}];
	DirParametricPlot3D[Evaluate[
		{#[[1]] Sin[#[[2]]] Cos[#[[3]]],#[[1]] Sin[#[[2]]] Sin[#[[3]]],#[[1]] Cos[#[[2]]]}& 
				/@ funl],{t,t0,t1}, opts]
	]


LevelCurve[fun__, {x_,x0_,x1_}, {y_,y0_,y1_}, {z_,z0__}, opts___?OptionQ] := 
	ImplicitPlot[Flatten[Transpose[Outer[#1 == #2&, Flatten[{fun}], {z0}]]], {x,x0,x1}, {y,y0,y1}, opts]

(*12-23-11 Changed npts back to pts, removed N from grads *)
PlotGradient[fun_, {x_,x0_,x1_}, {y_,y0_,y1_}, pts_, opts___?OptionQ] := 
	Module[{npts, asize, ptstyle, gstyle, grads, x00, x11, y00, y11, plot},
	npts = If[Head[pts[[1]]] === List, pts, {pts}];	
	asize = ArrowSize /. {opts} /. Options[PlotGradient];
	asize = If[Head[asize]===Symbol, asize, .035 asize];	
	{gstyle, ptstyle} = setps5[PlotGradient, {GradientStyle, PointStyle}, Length[npts], opts];
	Off[Power::infy, Infinity::indet];
	grads = {#, # + KPGrad[fun, {x,y}, #, opts]}& /@ npts;
	On[Power::infy, Infinity::indet];
	If[Not[FreeQ[grads, Null | Indeterminate]], Print[Last[Transpose[grads]]]; Return[Message[KPGrad::nograd]]];

	plot = Graphics[MapThread[Map[Flatten, {{#1, Line[#2]}, {#4, #1, Point[#3]}}]&, 
			{gstyle, grads, npts, ptstyle}]];		

	{{x00,x11}, {y00,y11}} = {Min[#], Max[#]}& /@ Transpose[{{{x0,x1}, {y0,y1}}, PlotRange /. AbsoluteOptions[plot, PlotRange]}];
	
	plot = Show[
			Graphics[ContourPlot[fun, {x,x00,x11}, {y,y00,y11}, 
				Sequence@@FilterRules[{opts}, Options[ContourPlot]]//Evaluate, AspectRatio -> Automatic]],
			plot, Graphics[MapThread[Flatten[{#1, Arrowheads[asize], Arrow[#2]}]&, {gstyle, grads}]]]
	]



PlotRegression3D[points:{{_,_,_}...}, fun_List, {var1_,var2_}, opts___?OptionQ]:=
	Module[{ptstyle, linestyle, showdiff, prange, pts, function, plotfun, perline, lstyle},
	{pstyle, ptstyle, lstyle, showdiff, prange, print} = 
		{PlotStyle, PointStyle, LineStyle, ShowDifference, PlotRange, PrintDisplay}
			/.{opts}/.Options[PlotRegression3D];

	{ptstyle, linestyle} = Map[setps5[PlotRegression3D, #, 1, Sequence[
					PointStyle -> {ptstyle}, LineStyle -> {lstyle}]]&, {PointStyle, LineStyle}];
	pts = Graphics3D[Flatten[{ptstyle, Point /@ points}], PlotRange->prange];

	prange = PlotRange /. AbsoluteOptions[pts, PlotRange];

	function = Fit[points, fun, {var1,var2}];
	If[print,
		Print["The least-squares fit is: "];
		Print[function]];
	


	plotfun = Plot3D[function//Evaluate,
		{var1, prange[[1,1]], prange[[1,2]]}, {var2, prange[[2,1]], prange[[2,2]]},
			Sequence@@FilterRules[{opts}, Options[Plot3D]]//Evaluate, PlotStyle->pstyle];

	perline = If[ showdiff, 
		Graphics3D[Flatten[{linestyle, Map[Line, Transpose[{points,
			Map[ {#[[1]],#[[2]], function/.{var1 -> #[[1]], var2 -> #[[2]]} }&, points]}]]}]],
		Graphics3D[{}]];
	     			
	Show[pts, plotfun, perline, Sequence@@FilterRules[{opts}, Options[Graphics3D]],
			Axes -> True] 
	]


(*5-9-00: Changed RealPower[points] and RealPower[rules] to points and rules *)

SelectReal[{}]:={}

SelectReal[points:{__}] := Select[points, zerotest[][Im[N[#]]] &]

SelectReal[rules_List, vars_] := Select[rules, (zerotest[][Im[vars /. N[#]]] || Length[#] < Length[vars]) &]
	
SelectReal[rules_, vars_] := Select[{ToRules[LogicalExpand[rules]]}, (zerotest[][Im[vars /. N[#]]] || Length[#] < Length[vars]) &]	


TableDerivatives[fun_, vars_, order_] :=
	Module[{vlist, its, l, iteration, start, stop, ord},
	{start, stop} = If[Head[order] === List, {First[order], First[order]}, {0, order}];
  	vlist = Flatten[{vars}];
	l = Length[vlist] - 1;
	its = Array[i,l];
	If[l == 0, 
		Table[D[fun, {vlist[[1]], ord}], {ord, start, stop}],		
		iteration = Join[{{ord, start, stop}},
			Transpose[{its, ord+FoldList[Subtract, 0, Drop[its,-1]], Table[0,{l}], Table[-1,{l}]}]];
		Table[D[fun, Sequence@@Transpose[{vlist, Join[its, {ord - Plus@@its}]}]],
			Evaluate[Sequence@@iteration]]//Flatten]
	]  


FindRoots[eqn_List, ipts:{{{_Symbol,_}..}..}, opts___?OptionQ]:= 
	Module[{optval = FilterRules[{opts}, Options[FindRoot]]},
	Map[FindRoot[Flatten[Thread /@ eqn]//Evaluate, Evaluate[Sequence @@ #], Evaluate[optval]]&, ipts]]

FindRoots[eqn_List, ipts:{{_Symbol,_}..}, opts___?OptionQ]:= 
	FindRoot[Evaluate[Flatten[Thread /@ eqn]], Evaluate[Sequence @@ ipts], opts]

FindRoots[eqn_, ipts:{_Symbol,_}, opts___?OptionQ]:= 
	FindRoot[eqn, ipts, opts]

FindRoots[lhs_ == rhs_, {x_,xmin_,xmax_}, opts___?OptionQ] := 
	Module[{plot, data, endpts}, 
    	plot = Plot[lhs - rhs, {x, xmin, xmax}, Evaluate[Sequence@@FilterRules[{opts},Options[Plot]]]]; 
    	data = Cases[plot, Line[{a__}] -> a, Infinity];  
    	data = Take[#,1]& /@ First /@ Transpose /@ 
			Select[Split[data, (Chop[Last[#1] Last[#2]] <= 0 & )], 
				Length[#1] >= 2 & ];
	endpts = If[Chop[lhs - rhs/.x->#] == 0,{{x->#}},{}]& /@ {xmin,xmax};
	Union[Flatten[{First[endpts],(FindRoot[lhs == rhs, {x, #1}//Flatten, 
    		Evaluate[Sequence@@FilterRules[{opts}, Options[FindRoot]]]] & ) /@ data, Last[endpts]},1]//N]
    ]


FindRoots[eqn_, vars_, ipts_, opts___?OptionQ] := 
	Block[{varss, iptslist, pts},
	varss = Flatten[{vars}];
	iptslist = If[Length[varss]==1 || Head[ipts[[1]]]===List, ipts, {ipts}];
	pts = If[Length[varss] == 1 && VectorQ[iptslist, NumberQ[N[#]]&], Partition[iptslist,1], iptslist]; 
	If[VectorQ[pts[[1]],NumberQ[N[#]]&],
			pts = Map[Transpose[{varss,#}]&,pts],
			If[Head[pts[[1,1]]] === Rule, 
				pts = Map[Apply[List,#,1]&,pts],
				If[Length[varss] == 1,
					pts = Partition[pts,1]]] ];
	Map[FindRoot[eqn, #, Sequence@@FilterRules[{opts},Options[FindRoot]]//Evaluate]&, pts]
	]


FindCriticalPoints[fun_, vars_, ipts_?VectorQ, opts___?OptionQ] :=
	FindRoots[Grad[fun,Flatten[{vars}]], vars, ipts, opts]

FindCriticalPoints[fun_, vars_, ipts_, opts___?OptionQ] :=
	FindRoots[Grad[fun,Flatten[{vars}]], vars, ipts, opts]
	
FindCriticalPoints[fun_, vars_, opts___?OptionQ] :=
	Solve[Simplify[Grad[fun,vars]]== 0, vars, opts(*, VerifySolutions->True*)]//Union


ClassifyCriticalPoints[fun_,vars:{__Symbol},cpt:{__List}, opts___?OptionQ] :=
	Module[{cptlist,gradvalues, pos, data, test},
	If[MatchQ[Flatten[cpt],{}],Return[Message[ClassifyCriticalPoints::notempty]]];
	cptlist = If[Head[cpt[[1,1]]] =!= Rule, PointsToRules[cpt,vars],cpt];
	cptlist = Join[NewUnsortedUnion[SelectReal[cptlist,vars]],Select[cptlist,Length[#] < Length[vars]&]];

	(*    Test critical points    *)

	gradvalues = Grad[fun,vars] /. cptlist//Quiet;
	pos = Position[ Map[ Chop[PowerExpand[Chop[N[#1]]]] === ZeroVector[Length[#1]]& , gradvalues], False];
	If[pos != {},
		If[Length[pos] == Length[cptlist], 
			Return @ Message[ClassifyCriticalPoints::notcritpt,cptlist[[Flatten[pos]]]],
			Message[ClassifyCriticalPoints::notcritpt,cptlist[[Flatten[pos]]]];
			cptlist = Delete[cptlist,pos]]];
(*	data = Chop[PowerExpand[Chop[Eigenvalues /@ (HessianMatrix[fun,vars,opts] /. Chop[N[cptlist]])]]];*)
	data = Map[HessianMatrix[fun,vars,#,opts]&, vars/.cptlist];

	pos = Position[FreeQ[#,Infinity | Indeterminate]&/@data,False];

	If[Flatten[pos]!={}, Message[ClassifyCriticalPoints::unabletoeval]];
	data = Delete[data,pos];

	data = Chop[PowerExpand[Chop[Eigenvalues /@ data]]];
(*	data = Chop[PowerExpand[Chop[Eigenvalues /@ Map[HessianMatrix[fun,vars,#,opts]&, Chop[N[vars/.cptlist]]]]]];*)

cptlist = Delete[cptlist, pos];

	test = Which[Times @@ # == 0, "Inconclusive", And @@ Positive[#], "LocalMinimum", 
			And @@ Negative[#],  "LocalMaximum", True, "Saddle"]&;
	Transpose[{ test /@ data, cptlist }]
	]


ClassifyCriticalPoints[fun_,vars:{__Symbol},cpt_List, opts___?OptionQ] := 
	ClassifyCriticalPoints[fun,vars,{cpt}, opts]

ClassifyCriticalPoints[fun_,var: _Symbol | {_Symbol}, cpt:{__List}, opts___?OptionQ] := 
	ClassifyCriticalPoints[fun,Flatten[{var}],cpt, opts]

ClassifyCriticalPoints[fun_,var: _Symbol | {_Symbol},cpt:{__} | _, opts___?OptionQ] := 
	ClassifyCriticalPoints[fun,Flatten[{var}],List /@ Flatten[{cpt}], opts]



KPGrad[fun_,vars_] := 
	If[VectorQ[vars], 
		Grad[fun, vars],
		Return[Message[Grad::nocoord,vars]]]
		
KPGrad[fun_, vars_, (opts__)?OptionQ] := 
	If[VectorQ[vars], 
		KPGrad[fun, vars, vars, opts],
		Return[Message[Grad::nocoord,vars]]]

(* Added Remove/RestorePiecewise  03-18-16 *)
KPGrad[fun_, (vars_)?VectorQ, pt_?(VectorQ), (opts___)?OptionQ] := 
	Module[{limit, len = Length[vars],rpt}, 
	limit = UseLimit /. {opts} /. Options[KPGrad]; 
	RemovePiecewise[];
	grad = 
		If[limit, (*12-9-11 next line changed*)
			f = Function[vars, fun];
			rpt = Rationalize[pt,0];  (*Added 11-28-16*)
			NewLimit[(f @@ (rpt + h*UnitVector[len, #1]) - f @@ rpt)/h& /@ Range[len], h -> 0,
				Evaluate[Sequence@@FilterRules[{opts},Options[Limit]]]],
			First[Grad[fun, vars]/.PointsToRules[pt,vars]]];
	RestorePiecewise[];
	grad
	]
	
KPGrad[fun_, (vars_)?VectorQ, pts_?(MatrixQ), (opts___)?OptionQ] := 
	KPGrad[fun, vars, #, opts]& /@ pts
	
KPGrad[fun_, vars_, chart_] := 
	If[VectorQ[vars], 
		Grad[fun, vars, chart],
		Return[Message[Grad::nocoord,vars]]]
		


	
JacobianMatrix[fun_List, vars_] := 
	If[VectorQ[vars], 
		Grad[fun, vars],
		Return[Message[Grad::nocoord,vars]]]		

JacobianMatrix[fun_List?VectorQ, vars_List?VectorQ, pt_List?VectorQ, opts___?OptionQ] := 
	KPGrad[fun, vars, pt, opts]
		
(* 12-16-11: added this template *)		
JacobianMatrix[fun_, (vars_)?VectorQ, pts_?(MatrixQ), (opts___)?OptionQ] := 
	JacobianMatrix[fun, vars, #, opts]& /@ pts
	
JacobianMatrix[fun_List, vars_, chart_] := 
	If[VectorQ[vars], 
		Grad[fun, vars, chart],
		Return[Message[Grad::nocoord,vars]]]

JacobianDet[fun_List, vars_List] := 
	Det[JacobianMatrix[fun, vars]]
	
JacobianDet[fun_List, vars_List, pt_?VectorQ, opts___?OptionQ] := 
	Det[JacobianMatrix[fun, vars, pt, opts]]
	
JacobianDet[fun_List, vars_List, pts_?MatrixQ, opts___?OptionQ] := 
	Det/@JacobianMatrix[fun, vars, pts, opts]

	
KPDiv[fun_List, vars_List] := Div[fun,vars]
(*	Inner[ D, fun, vars, Plus ]*)

KPDiv[fun_List, vars_List, (opts__)?OptionQ] := 
	If[VectorQ[vars], 
		KPDiv[fun, vars, vars, opts],
		Return[Message[Div::bdmtrc,vars]]]

KPDiv[fun_List, vars_List, pt_?(VectorQ), opts___?OptionQ]:=
	Tr[Map[KPGrad[#1, vars, pt, opts] &, fun]]
	
KPDiv[fun_List, vars_List, pts:{__List}, opts___?OptionQ]:=
	KPDiv[fun, vars, #, opts]& /@ pts

KPDiv[fun_List, vars_, chart_] := 
	If[VectorQ[vars],
		Div[fun,vars,chart],
		Return[Message[Div::nocoord, vars]]]	
	
(* Changed 11-27-17 *)	
HessianMatrix[fun_,vars_List] := 
	D[fun,{vars,2}]

HessianMatrix[fun_,vars_List, pt_?VectorQ] := 
	First[D[fun,{vars,2}]/.PointsToRules[pt,vars]]

HessianMatrix[fun_, vars_List, pt_?VectorQ, opts___?OptionQ] := 
	Module[{limit, gradf, gradv, hessian, len = Length[vars]},
	limit = UseLimit /. {opts} /. Options[HessianMatrix];
	If[limit,
		gradf = Function[vars, Evaluate[Grad[fun, vars]]];
		gradv = KPGrad[fun, vars, pt, opts]//Evaluate;
		If[FreeQ[gradv, Indeterminate],
			hessian = (NewLimit[(gradf @@ (pt + h*UnitVector[len, #1]) - gradv)/h, h -> 0] &) /@ Range[len];
			If[FreeQ[hessian,Indeterminate(*Null*)],Transpose[hessian],Return[Indeterminate](*Null*)],
			Return[Indeterminate]],
		HessianMatrix[fun, vars, pt]
	]]


KPLaplacian[fun_, vars_] := 	
	If[VectorQ[vars], 
		Laplacian[fun, vars],
		Return[Message[Laplacian::nocoord,vars]]]
		
KPLaplacian[fun_, vars_List, opts___?OptionQ]:=
	If[FreeQ[{opts},UseLimit], 
		Laplacian[fun, vars],
		KPLaplacian[fun, vars, vars, opts]
		]

KPLaplacian[fun_, vars_List, pt_List, opts___?OptionQ]:=
	Module[{limit, hessian},
	limit = UseLimit /. {opts} /. Options[KPLaplacian];
	hessian = HessianMatrix[fun, vars, pt, UseLimit->limit];
	If[FreeQ[hessian,Null], Tr[hessian],Indeterminate]
	]

KPLaplacian[fun_, vars_List, pts:{__List}, opts___?OptionQ]:=
	KPLaplacian[fun, vars, #, opts]& /@ pts
	
KPLaplacian[fun_List, vars_List, pt_List, opts___?OptionQ]:= 
	KPLaplacian[#, vars, pt, opts]& /@ fun
	
KPLaplacian[fun_, vars_, chart_]:= Laplacian[fun, vars, chart]
	
	
Potential[fun_List, vars_List] := 
	Module[{f, test}, 
	test = Switch[
		Length[fun], 
		2, Simplify[Div[{fun[[2]], -fun[[1]]}, vars]] === 0, 
        3, Simplify[Curl[fun, vars]] === {0, 0, 0}, 
		_, False]; 
    If[Not[test], Return[Message[Potential::notconservative]]]; 
    (f @@ vars)/.First[DSolve[Thread[Grad[f @@ vars, vars] == fun], f @@ vars, vars]]
	]


AlternatingSum[list_] := 
	Module[{parity = Length[list], sum},
	sum = Sum[list[[2 i - 1]] - list[[2 i]],{i,1,Floor[parity/2]}];
	If[OddQ[parity], sum + Last[list],sum]
	]
	
	

     
(* Added opts to command and Grad; Removed SelectReal from lines calculating pts.*)
(*Restored SelectReal *)
(*PowerBehavior->Complex must be added to Solve because if the option is called by ImplicitPlot, expr will involve an expression involving Surd which Solve cannot solve. *)
tangentlines[expr_, x_, var_, t_, tanpts_List, tanstyle_, ptstyle_, pstyle_, yrange_, opts___Rule] := 
	Module[{pts}, 
	Off[ReplaceAll::"reps", Solve::ratnz];
Off[PowerBehavior::notoption];
	pts = If[Length[#] == 1, 
			Sequence @@ Flatten /@ ({#1, Flatten[{var}]} /. Solve[(expr /. x -> #1) == 0, var, PowerBehavior->Complex]/.C[1]->0),
			If[First[expr/.PointsToRules[#,{x,var}]]==0,
				#,
				{#1[[1]], var} /. FindRoot[(expr /. x -> #1[[1]]) == 0, {var, #1[[2]]}]]] &  /@ (Flatten[{#}] & /@tanpts); 
On[PowerBehavior::notoption];

	pts = Select[pts,(Length[Flatten[{#}]]==2 && FreeQ[#,var]&)];

	pts = SelectReal[pts]; 

	If[Length[Dimensions[pts]]==1,pts={pts}];
(* 11/10/11 Added Chop *)
	pts = Select[Flatten/@ Chop[pts], (First[yrange] <= Chop[Last[N[#]]] <= Last[yrange]&)];

	On[ReplaceAll::"reps", Solve::ratnz];
	If[ Not[MatrixQ[pts, NumberQ[N[#1]] & ]], 
		Message[ImplicitPlot::ptfail]; 
			Return[{$Failed, $Failed, $Failed, $Failed,$Failed}]]; 
		
	{
	pts, 
	MapThread[#1 + t*{1, -1}*Reverse[#2] & ,
       {pts, Simplify[Normalize[#, Reals]& /@ (KPGrad[expr, {x, var}, #, opts]  &  /@ pts)]}],
    Table[tanstyle,{Length[pts]}],Table[ptstyle,{Length[pts]}],Table[pstyle,{Length[pts]}]
	}
	] 




	
	
iplot::iplotpoints = "DrawTangentLine requires that both the x and y ranges of the plot be sepcified. ";
(*
ImplicitPlot[eqns : (_Equal | _Tooltip | {__Equal}), {x_, x0_, x1_}, opts___?OptionQ] :=
	Block[{neweqn, var, pts, pr, neweqns},
(*If[MemberQ[{opts}, DrawTangentLine, Infinity], Return[Message[iplot::iplotpoints]]];*)
	neweqns = eqns; (*eqns /. Tooltip[rest_]->rest;*)
  	var = Union[Select[Cases[neweqns, _Symbol,Infinity],  (Not[NumberQ[N[#]] || # === x ]&)]];
	If[Unequal[Length[var], 1], Message[PlotEquation::var, eqns, x]; Return[$Failed]];
(*Off[PowerBehavior::notoption];*)
	pts = Solve[#, var, Reals(*, PowerBehavior->Complex*)]& /@Flatten[{neweqns}];  (*Removed PowerBehavior->Complex 11-23-16 *)
(*On[PowerBehavior::notoption];*)
	pts = Select[pts,(Length[Flatten[{#}]]==2 && FreeQ[#,var]&)];
	pts = SelectReal[pts];
	If[Head[pts] === Solve, Message[PlotEquation::sfail, eqns, x]; pts = $Failed];
	pr = (PlotRange /. {opts} /. Options[ImplicitPlot])/.All->Full;
 	pr = (PlotRange/.AbsoluteOptions[PlotJump[Evaluate[Flatten[var /. pts]],{x,x0,x1}, PlotRange->pr (*, opts, PlotRange->Full*)],PlotRange])[[2]];
Print[pr];
	ImplicitPlot[neweqns, {x, x0, x1}, {var, pr+.001*{-1,1} Abs[pr]} // Flatten//Evaluate, opts(*, Epilog->{PointSize[.02],Point/@jpts}*)]
	]
*)	

ImplicitPlot[eqns:(_Equal | _Tooltip | {__Equal}), {x_, x0_, x1_}, {y_, y0_, y1_}, opts___?OptionQ] := 
	Module[{dpts, dptstyle, pstyle},
	dpts = DrawPoint /. {opts} /. Options[ImplicitPlot];
	Which[
				MatchQ[dpts, {}], dpts = {},
				MatrixQ[dpts,NumericQ]&&MatchQ[Dimensions[dpts],{_,2}], dpts = dpts, 
				VectorQ[dpts, NumericQ] && Length[dpts]==2, dpts = {dpts},
				True, Message[point::drawpt]; dpts = {}];
	dptstyle = setps5[ImplicitPlot, DrawPointStyle, Length[dpts], opts];
	pstyle = PlotStyle /. {opts} /. Options[ImplicitPlot];
	Show[
		ContourPlot[eqns /. Tooltip[rest_]->rest//Evaluate, {x, x0, x1}, {y, y0, y1}, ContourStyle -> pstyle, 
			Evaluate[Sequence@@FilterRules[{opts}, Options[ContourPlot]]]],
		Graphics[MapThread[Flatten[{#1, Point[#2]}]&,{dptstyle, dpts}]]]
	]


(* condition set so that it only fires if opts contains DrawTangentLine *)
ImplicitPlot[eqns:(_Equal | _Tooltip | {__Equal}), {x_, x0_, x1_}, {y_, y0_, y1_}, opts___?OptionQ] := 
	Module[{dpts, dptstyle, expr, prange, lsize, tanpts, print, pts, lines, tanstyle, tanstyle1, ptstyle, ptstyle1, pstyle, iplot, yrange, ptline}, 
	dpts = DrawPoint /. {opts} /. Options[ImplicitPlot];
	prange = PlotRange /. {opts} /. PlotRange->{{x0,x1},{y0,y1}};
	If[MemberQ[Flatten[{prange}],All|Full], prange = {{x0,x1},{y0,y1}}];

	Which[
				MatchQ[dpts, {}], dpts = {},
				MatrixQ[dpts,NumericQ] && MatchQ[Dimensions[dpts],{_,2}], dpts = dpts, 
				VectorQ[dpts, NumericQ] && Length[dpts]==2, dpts = {dpts},
				True, Message[point::drawpt]; dpts = {}];
	dptstyle = setps5[ImplicitPlot, DrawPointStyle, Length[dpts], opts];
	pstyle = PlotStyle /. {opts} /. Options[ImplicitPlot];
	expr = Flatten[{eqns /. Tooltip[rest_] -> rest}];
	iplot = ContourPlot[expr//Evaluate, {x, x0, x1}, {y, y0, y1}, ContourStyle->pstyle, PlotRange->prange, (*Changed 10-11-17 *)
				Sequence@@FilterRules[{opts},Options[ContourPlot]]//Evaluate];

	expr = Apply[Subtract, Flatten[{eqns /. Tooltip[rest_] -> rest}], 1];
	{lsize, tanpts, print} = {LineLength, DrawTangentLine, PrintDisplay} /. {opts} /. Options[ImplicitPlot]; 
	tanpts = If[Head[tanpts] === List, tanpts, {tanpts}];
(*	yrange = Abs[Subtract @@ (PlotRange /. AbsoluteOptions[iplot, PlotRange])[[2]]]; (*Changed 11-25-16*) *)
	yrange = y1-y0;

	{tanstyle1, ptstyle1, pstyle} = setps5[ImplicitPlot, {TangentLineStyle, TangentPointStyle,PlotStyle}, Length[Flatten[{expr}]], opts];	
(*	If[Length[Flatten[{expr}]]>1, ptstyle1 = Cases[#, PointSize[_]] & /@ ptstyle1];*)
ptstyle1 = Cases[#, PointSize[_]] & /@ ptstyle1;
	{pts, lines, tanstyle, ptstyle,pstyle} = (*Changed t to x  11-23-16*)
		Transpose[MapThread[tangentlines[#1, x, y, x(*t*), tanpts, #2, #3, #4, {y0,y1}(*(PlotRange /. AbsoluteOptions[iplot, PlotRange])[[2]]*), opts] &,
				{expr, ptstyle1, tanstyle1, pstyle}]]//Simplify;

	If[MemberQ[pts,$Failed],Return[]];

	If[print, Print[TableForm[(Transpose/@Transpose[{pts,lines}])[[1]], TableDepth -> 2, TableHeadings -> {None, {"TangentPoint", "TangentLine"}, None}]]]; 
	If[Flatten[{lines}]=={},Return[]];

	ptline = Transpose/@Transpose[{pts, lines, ptstyle, tanstyle, pstyle}];
	ptline = Map[Select[#, FreeQ[#, Null | Indeterminate] &] &, ptline, 1];

	lines = Map[#[[2]]&, ptline,{2}];
	data = Map[#[[{5,4,3}]]&,ptline,{2}];

	Show[
		iplot, 
	If[Length[Flatten[expr]]==1,
		ParametricPlot[Flatten[lines,1], {x, -0.2 lsize yrange, 0.2 lsize yrange}, PlotRange->prange,
				PlotStyle -> Flatten[data,1],Sequence@@FilterRules[{opts},Options[ParametricPlot]]//Evaluate],
		Show[MapThread[ParametricPlot[Evaluate[#1], {x, -0.2 lsize yrange, 0.2 lsize yrange}, 
				PlotStyle -> #2, PlotRange->prange,
      			Sequence@@FilterRules[{opts},Options[ParametricPlot]]//Evaluate]&, {lines,data}]]],
      	ListPlot[pts,
      			PlotStyle -> MapThread[Flatten[{#1,#2,#3}]&, {ptstyle1,  pstyle, tanstyle1}]],
      	Graphics[MapThread[Flatten[{#1, Point[#2]}]&,{dptstyle, dpts}]],
     		PlotRange->prange, Sequence@@FilterRules[{opts},Options[Graphics]]]
	]/; MemberQ[{opts}, DrawTangentLine, Infinity]


PlotEquation[eqns:(_Equal | _Tooltip | {__Equal}), {x_, x0_, x1_}, {y_, y0_, y1_}, opts___?OptionQ] := 
	Module[{dpts, dptstyle},
	dpts = DrawPoint /. {opts} /. Options[PlotEquation];
	Which[
				MatchQ[dpts, {}], dpts = {},
				MatrixQ[dpts,NumericQ]&&MatchQ[Dimensions[dpts],{_,2}], dpts = dpts, 
				VectorQ[dpts, NumericQ] && Length[dpts]==2, dpts = {dpts},
				True, Message[point::drawpt]; dpts = {}];
	dptstyle = setps5[PlotEquation, DrawPointStyle, Length[dpts], opts];	
	Show[
		ImplicitPlot[eqns, {x, x0, x1}, {y, y0, y1}, 
			ContourLabels-> If[Head[eqns] === Tooltip, Automatic, None], opts, Frame -> False, Axes -> True],
		Graphics[MapThread[Flatten[{#1, Point[#2]}]&,{dptstyle, dpts}]]]
	]

PlotEquation[fun_, anything___] :=
	Return[Message[PlotEquation::eqn, fun]]

	
PlotEquation3D[eqns:(_Equal | _Tooltip | {__Equal}), {x_, xmin_, xmax_}, {y_, ymin_, ymax_}, {z_, zmin_, zmax_}, opts___?OptionQ] := 
	Module[{dpts, dptstyle, pstyle},
	If[MemberQ[{opts}, Point, Infinity], Return[Message[PE3::pt]]];
	dpts = DrawPoint /. {opts} /. Options[PlotEquation];
	Which[
				MatchQ[dpts, {}], dpts = {},
				MatrixQ[dpts,NumericQ]&&MatchQ[Dimensions[dpts],{_,3}], dpts = dpts, 
				VectorQ[dpts, NumericQ] && Length[dpts]==3, dpts = {dpts},
				True, Message[point::drawpt3D]; dpts = {}];
	dptstyle = setps5[PlotEquation, DrawPointStyle, Length[dpts], opts];

	{dptstyle, pstyle} = setps5[PlotEquation3D, {DrawPointStyle, PlotStyle}, {Length[dpts], Length[Flatten[{eqns}]]}, opts];

	Show[
		{ContourPlot3D[eqns, {x, xmin, xmax}, {y, ymin, ymax}, {z, zmin, zmax}, ContourStyle -> pstyle, 
			Evaluate[Sequence@@FilterRules[{opts}, Options[ContourPlot3D]]], BoxRatios->Automatic],
		Graphics3D[MapThread[Flatten[{#1, Point[#2]}]&,{dptstyle, dpts}]]}]]

PlotEquation3D[fun_, anything___] :=
	Return[Message[PlotEquation3D::eqn, fun]]

	


	
(* The following template is for backwards compatability *)
PlotGradientMethodArray[fun_, {x_,x0_,x1_}, {y_,y0_,y1_}, ivalue_?VectorQ, iters_?(IntegerQ[#]&&Positive[#]&), step_?NumericQ, opts___?OptionQ] :=
	Return[Message[syntax::changed]]


PlotGradientMethodArray[fun_,{x_,x0_,x1_}, {y_,y0_,y1_}, ivalue_?(VectorQ[#,NumericQ]&), step_?NumericQ, iters_?(IntegerQ[#]&&Positive[#]&), opts___?OptionQ] :=
	Module[{lines, spts, colorcommand, startcolor, endcolor, ptstyle, lstyle, flag, graph, print, 
				startsize, endsize, color, pts, ptsize, direction, gradf, points, points3D},
	{lines, spts, {colorcommand, startcolor, endcolor}, ptstyle, lstyle, flag, graph, print, startsize, endsize, direction} = 
				{DrawLines, ShowPoints, Color, PointStyle, LineStyle, Flag, DrawGraph, PrintDisplay, StartSize, EndSize, Direction} /. 
				{opts} /. Options[PlotGradientMethodArray];
	If[MatchQ[spts,False], lines = True];
	If[Not[direction === 1 || direction === -1], Return[Message[GradientMethod::dir]]];
	
	gradf = Function[{x,y},Grad[fun,{x,y}]//Evaluate];
	pts = NestList[(# + direction step gradf@@#)&, ivalue, iters];

	If[graph,
		If[print, Print[pts], Print[Last[pts]]];	
		color = If[endcolor == startcolor,
				Table[colorcommand[startcolor],{iters + 1}],
				Table[colorcommand[n],{n,startcolor,endcolor,(endcolor - startcolor)/iters}]];	
		ptsize = If[endsize == startsize,
				Table[PointSize[startsize],{iters + 1}],
				Table[PointSize[n],{n,startsize,endsize,(endsize - startsize)/iters}]];	
		{ptstyle, lstyle} = setps5[PlotGradientMethodArray, {PointStyle, LineStyle}, iters + 1, opts];
		points = MapThread[Flatten[{#1,#2,#3,#4}]&, {color, ptsize, ptstyle, (Point /@ pts)}];
		points3D = MapThread[Flatten[{#1,#2,#3,#4}]&, {color, ptsize, ptstyle, (Point /@ ({x,y,fun}/.PointsToRules[pts,{x,y}]))}];

		lines = If[lines, 
					MapThread[Flatten[{#1,#2,#3}]&, {Drop[color,-1], Drop[lstyle,-1], (Line /@ Partition[pts,2,1])}],
					{}];

		Switch[flag,
			"contour",
				ContourPlot[fun,{x,x0,x1}, {y,y0,y1},
					Epilog -> If[spts, {points,lines},{lines}], Sequence@@FilterRules[{opts}, Options[ContourPlot]]//Evaluate, 
					AspectRatio -> Automatic],
			"surface",
				(*PlotSection[fun,{x,x0,x1},{y,y0,y1}, DrawPoint -> pts,
					PointStyle -> Transpose[{color, ptsize, ptstyle}], opts],*)
				Show[PlotSection[fun,{x,x0,x1},{y,y0,y1}, opts],Graphics3D[points3D]],
			_,
				Show[GraphicsRow[
					{ContourPlot[fun,{x,x0,x1}, {y,y0,y1},
						Epilog -> If[spts, {points,lines},{lines}], Sequence@@FilterRules[{opts}, Options[ContourPlot]]//Evaluate, AspectRatio -> Automatic],
					Show[PlotSection[fun,{x,x0,x1},{y,y0,y1}, opts],Graphics3D[points3D]]}
						]]
			],
		If[print, Print[pts], Print[Last[pts]]]]
	]	
	
ContourPlotGradientMethod[fun_, {x_,x0_,x1_}, {y_,y0_,y1_}, ivalue_?(VectorQ[#,NumericQ]&), step_, iters_, opts___?OptionQ] :=
	PlotGradientMethodArray[fun, {x,x0,x1}, {y,y0,y1}, ivalue, step, iters, Flag -> "contour", opts]
	
PlotGradientMethod[fun_, {x_,x0_,x1_}, {y_,y0_,y1_}, ivalue_?(VectorQ[#,NumericQ]&), step_, iters_, opts___?OptionQ] :=
	PlotGradientMethodArray[fun, {x,x0,x1}, {y,y0,y1}, ivalue, step, iters, Flag -> "surface", opts]

GradientMethod[fun_, {x_,x0_?NumericQ}, {y_,y0_?NumericQ}, stepsize_?NumericQ, n___?(IntegerQ[#]&&Positive[#]&), opts___?OptionQ]:=
	Module[{print, gradf = Function[{x,y}, Grad[fun, {x,y}]//Evaluate], direction},
	{direction, print} = {Direction, PrintDisplay} /. {opts} /. Options[GradientMethod];
	If[Not[direction === 1 || direction === -1], Return[Message[GradientMethod::dir]]];
	If[print,
		FixedPointList[(# + direction*stepsize*gradf@@#)&, N[{x0,y0}], n, FilterRules[{opts}, Options[FixedPointList]]],
		FixedPoint[(# + direction*stepsize*gradf@@#)&, N[{x0,y0}], n, FilterRules[{opts},Options[FixedPoint]]]]
	]

WronskianMat[fun_?VectorQ, x_] := 
	NestList[D[#1, x] & , fun, Length[fun] - 1]

WronskianDet[fun_?VectorQ, x_]:= 
	Simplify[Det[WronskianMat[fun,x]]]



CubicSpline[data_, opts___] := 
	Module[{ndata = data, n = Length[data],fd, sd, exact, wp, vech, vecb, fdt, sdt, firstv, lastv, vecu, vecv, vecz, vecA, vecB, vecC}, 
	{fd, sd, exact, wp} = {FirstDerivatives, SecondDerivatives, Exact, WorkingPrecision} /. {opts} /. Options[CubicSpline];
	If[!exact,ndata = N[data, wp]];
	{vech, vecb} = (Drop[Subtract @@ #1, -1] & ) /@ ({RotateLeft[#1], #1} & ) /@ Transpose[ndata]; 
	vecb = (vecb/vech); 
	{fdt,sdt} = {MemberQ[{opts}, FirstDerivatives,2],MemberQ[{opts}, SecondDerivatives,2]};
	If[fdt && sdt, Message[CubicSpline::firstandsecondderivs];Return[]];
	If[fdt, 

		If[fd === Automatic, 
			{firstv,lastv} = DividedDifferenceTable/@{data[[Range[4]]],data[[Range[n-3,n]]]};
			{firstv,lastv} = {6 vech[[1]] ((vech[[1]]+vech[[2]]) firstv[[4,1]]-firstv[[3,1]]),
							  6 vech[[n-1]](lastv[[3,2]]+(vech[[n-1]]+vech[[n-2]])lastv[[4,1]])}];

		vecu = FoldList[(2*(vech[[#2]] + vech[[#2-1]]) - vech[[#2-1]]^2/#1 )& , 2vech[[1]], Range[2, n-1 ]]//Simplify; 
		vecu = Join[vecu,{2 Last[vech] - Last[vech]^2/Last[vecu]}];
		If[Not[fd === Automatic], firstv = 6 vecb[[1]]-6 fd[[1]]];
		vecv = FoldList[6(vecb[[#2]] - vecb[[#2 - 1]]) - vech[[#2 - 1]] #1/vecu[[#2-1 ]] & , firstv, Range[2, n - 1]]; 
		If[Not[fd === Automatic], lastv = 6 fd[[2]] - 6 Last[vecb] - vech[[n-1]] vecv[[n-1]]/vecu[[n-1]]];
		vecv = Join[vecv, {lastv}];
		vecz = Reverse[FoldList[(vecv[[#2]] - vech[[#2]]*#1)/vecu[[#2]] & , Last[vecv]/Last[vecu], Range[n - 1, 1, -1]]],

		vecu = FoldList[(2*(vech[[#2]] + vech[[#2-1]]) - (vech[[#2-1]]^2)/#1 &) , 2*(vech[[1]] + vech[[2]]), Range[3, n - 1]]; 
		vecv = FoldList[6(vecb[[#2]] - vecb[[#2 - 1]]) - vech[[#2 - 1]] #1/vecu[[#2-2 ]] & , 
			6(vecb[[2]] - vecb[[1]]) - vech[[1]] sd[[1]], Range[3, n - 1]]; 
		vecz = Flatten[{sd[[1]], Reverse[FoldList[(vecv[[#2-1]] - vech[[#2]]*#1)/vecu[[#2-1]] & , sd[[2]], Range[n - 1, 2, -1]]]}]];
	{vecA, vecB, vecC} = 
    		{1/6 (Drop[RotateLeft[vecz]-vecz,-1]/vech), 
       		 1/2 Drop[vecz,-1], 
		 -vech Rest[vecz]/6 - vech[[Range[n-1]]] vecz[[Range[n-1]]]/3 +  vecb}; 

	CubicSplineFunction[{ndata[[1,1]],ndata[[n,1]]}, ndata, vecA, vecB, vecC]
	]


PlotCubicSpline[data_, opts___?OptionQ] :=
	Module[{x, points, ptstyle},
	{points,ptstyle} = {Points,PointStyle} /. {opts} /. Options[PlotCubicSpline];
	Plot[CubicSpline[data, opts][x]//Evaluate,{x,First[data][[1]],Last[data][[1]]}, Sequence@@FilterRules[{opts}, Options[Plot]]//Evaluate,
		Evaluate[If[points, Epilog->Flatten[{PointStyle/.Options[PlotCubicSpline],ptstyle,Point/@data}], Epilog->{}]]]
	]

Format[CubicSplineFunction[{min_,max_},b__]]:= SequenceForm["CubicSplineFunction[", {min, max}," ,<>]"]
CubicSplineFunction[{min_,max_}, ndata_, vecA_, vecB_, vecC_][val_] := 
    Piecewise[
		Table[
			{ndata[[i,2]] + (val - ndata[[i,1]])*(vecC[[i]] + (val - ndata[[i,1]])*(vecB[[i]] + (val - ndata[[i,1]])*vecA[[i]])), ndata[[i,1]] <= val <= ndata[[i + 1,1]]}, 
		{i, 1, Length[ndata] - 1}]
	]


ParaCubicSpline[data_, opts___] := 
	Module[{n = Length[data], fd, sd, derivs}, 
	{fd, sd} = {MemberQ[{opts}, FirstDerivatives, 2], MemberQ[{opts}, SecondDerivatives, 2]}; 
	If[fd && sd, Return[Message[CubicSpline1::firstandsecondderivs]]]; 
	derivs = If[fd, FirstDerivatives /. {opts}, SecondDerivatives /. {opts} /. Options[ParaCubicSpline]]; 
	If[Not[MatrixQ[derivs]], Return[Message[ParaCubicSpline::vectorderivs]]]; 
	ParaCubicSplineFunction[MapThread[CubicSpline[#1, #2, opts] & , 
		{(Transpose[{Range[n], #1}] & ) /@ Transpose[data], 
		Evaluate[
			If[fd, (FirstDerivatives -> #1 & ) /@ Transpose[derivs], (SecondDerivatives -> #1 & ) /@ Transpose[derivs]]]}]]
	]

ParaCubicSplineFunction[{CS1_, CS2_}][val_] := 
	{CS1[val], CS2[val]}

ParaPlotCubicSpline[data_, opts___?OptionQ] := 
	Module[{x, points, ptstyle}, 
	{points, ptstyle} = {Points, PointStyle} /. {opts} /. Options[PlotCubicSpline]; 
	ParametricPlot[ParaCubicSpline[data, opts][x]//Evaluate, {x, 1, Length[data]}, 
		Sequence@@FilterRules[{opts}, Options[ParametricPlot]]//Evaluate, 
		Evaluate[If[points, Epilog -> Flatten[{ptstyle, PointStyle /. Options[PlotCubicSpline], Point /@ data}], 
		Epilog -> {}]]]
	]/;ParaCubicSpline[data,opts] =!= Null	 	


DividedDifferenceTable[(data_)?MatrixQ, opts___?OptionQ] := 
	DividedDifferenceTable[data,Length[data], opts]

DividedDifferenceTable[(data_)?MatrixQ, m_?IntegerQ, opts___] := 
	Module[{vecx,wp},
	wp = WorkingPrecision /. {opts} /. WorkingPrecision -> MachinePrecision;
	vecx = With[{tmp = First[Transpose[N[data,wp]]]}, 
		(Drop[tmp, #1] - Drop[tmp, -#1] & ) /@ Range[m]]; 
	N[FoldList[(Drop[#1, 1] - Drop[#1, -1])/vecx[[#2]] & , Last[Transpose[N[data,wp]]], Range[m-1]],wp]
	]





makeCurves[fun_, linerange_, varrange_, (opts___)?OptionQ] := 
	With[{lines = Table[{Re[fun], Im[fun]}, linerange]}, ParametricPlot[lines, varrange, opts]]


ComplexPlot[fun_, {x_, x0_, x1_, dx_}, {y_, y0_, y1_, dy_}, (opts___)?OptionQ] := 
	Module[{ppopts, xstyle, ystyle}, 
	ppopts = FilterRules[{opts}, Options[ParametricPlot]]; 
	{xstyle, ystyle} = {XStyle, YStyle} /. {opts} /. Options[ComplexPlot]; 
    Show[makeCurves[fun, {y, y0, y1, dy}, {x, x0, x1}, PlotStyle -> xstyle, ppopts], 
		makeCurves[fun, {x, x0, x1, dx}, {y, y0, y1}, PlotStyle -> ystyle, ppopts], 
		Sequence@@FilterRules[{opts}, Options[Graphics]],  
		AspectRatio -> Automatic, PlotRange -> All]]
		


(*New version: 07-26-2012*)	


NewLimit[expr_, x_ -> x0_,  opts___?OptionQ] := 
	Module[{rl, ll, dir}, 
	dir = Direction /. {opts} /. Options[NewLimit];
(* 04-27-17  PowerBehavior->Complex is required since the change to Surd *)

	If[dir === Both,
		rl = Limit[expr, x -> x0, Direction -> -1, PowerBehavior->Complex, Evaluate[Sequence@@FilterRules[{opts},Options[Limit]]]]; 
		ll = Limit[expr, x -> x0, Direction ->  1, PowerBehavior->Complex, Evaluate[Sequence@@FilterRules[{opts},Options[Limit]]]]; 
		Off[Infinity::indet];

		If[FreeQ[{rl,ll},Limit],
			If[Length[Flatten[{expr}]]>1, 
				MapThread[
					Which[
						MatchQ[Simplify[#1 - #2], 0], #1, (* Added this on 09-30-15 *)
						(And @@ Flatten[{NumericQ /@ Flatten[{#1}], NumericQ /@ Flatten[{#2}]}]) && (Chop[Norm[N[#1 - #2]]] == 0), #1,
						(* #1 == #2, #1, *)  (* Added this on 09-30-15 *)
						(And @@ Flatten[{NumericQ /@ Flatten[{#1}], NumericQ /@ Flatten[{#2}]}]) && #1 == #2, #1, 
						True, Message[NewLimit::nolimit, rl, ll]; Indeterminate (*Table[Indeterminate,{i,1,Length[Flatten[{expr}]]}]*)]&, {rl, ll}],
					Which[
						MatchQ[Simplify[rl - ll], 0], rl, (* Added this on 09-30-15 *)
						And @@ Flatten[{NumericQ /@ Flatten[{rl}], NumericQ /@ Flatten[{ll}]}] && (Chop[Norm[Simplify[rl - ll]]] == 0), rl, 
						And @@ Flatten[{NumericQ /@ Flatten[{rl}], NumericQ /@ Flatten[{ll}]}] && rl == ll, rl, 
						rl === ll, rl,   (* Added this on 09-30-15 *)
						True, Message[NewLimit::nolimit, rl, ll]; Indeterminate (*Null*)]],
			Indeterminate],
	On[Infinity::indet];
		Limit[expr, x -> x0, Evaluate[Sequence@@FilterRules[{opts},Options[Limit]]]]]

	]

NewNLimit[expr_, x_ -> x0_,  opts___?OptionQ] := 
	Module[{rl, ll, dir}, 
	dir = Direction /. {opts} /. Options[NewLimit];
(*	nexpr=If[FreeQ[expr,Abs],
		expr,
		expr /. Abs[junk_]->Hold[RealAbs[{junk}]]];*)
	If[dir === Both,
		rl = NLimit[expr, x -> x0, Direction -> -1, Sequence@@FilterRules[{opts},Options[Limit]//Evaluate]](*//Quiet*); 
		ll = NLimit[expr, x -> x0, Direction -> 1 , Sequence@@FilterRules[{opts},Options[Limit]//Evaluate]](*//Quiet*); 

	If[FreeQ[{rl,ll},Limit],
		If[Length[Flatten[{expr}]]>1, 
			MapThread[
				Which[
					#1 == #2, #1,
					And @@ Flatten[{NumericQ /@ Flatten[{#1}], NumericQ /@ Flatten[{#2}]}] && #1 == #2, #1, 
					And @@ Flatten[{NumericQ /@ Flatten[{#1}], NumericQ /@ Flatten[{#2}]}] && (Chop[Norm[#1 - #2]] == 0), #1, 
					True, Message[NewLimit::nolimit, rl, ll]; Indeterminate (*Table[Indeterminate,{i,1,Length[Flatten[{expr}]]}]*)]&, {rl, ll}],
		
			Which[
				And @@ Flatten[{NumericQ /@ Flatten[{rl}], NumericQ /@ Flatten[{ll}]}] && (Chop[Norm[rl - ll]] == 0), rl, 
				And @@ Flatten[{NumericQ /@ Flatten[{rl}], NumericQ /@ Flatten[{ll}]}] && rl == ll, rl, 
				rl === ll, rl,
				True, Message[NewLimit::nolimit, rl, ll]; Indeterminate (*Null*)]],
	Indeterminate],
					
	NLimit[expr, x -> x0, Sequence@@FilterRules[{opts},Options[Limit]//Evaluate]]]
	]

IntegrationRegion3D[{x_,a_,b_},{y_,f_,g_},{z_,F_,G_},opts___?OptionQ]:=
	Module[{xx, yy, faces, facestyle, facemesh},
	xx = a + u (b-a);
	yy = f + v (g-f)/.x->xx;
	{faces, facestyle,facemesh} = {Faces, FaceStyle,FaceMesh}/.{opts}/.Options[IntegrationRegion3D];
	Show[{
		ParametricPlot3D[Evaluate[{{x, y, F},{x, y, G}}/.{x->xx, y->yy}],{u,0,1},{v,0,1},
			FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate],
		If[faces,
			ParametricPlot3D[Evaluate[
				{{x, f, F + v (G - F)}/.{x->xx, y->(f/.x->xx)},
				{x, g, F + v (G - F)}/.{x->xx, y->(g/.x->xx)},
				{x,f + u (g - f),F + v (G-F)}/.{x->b, y->(f + u (g - f))/.x->b},
				{x,f + u (g-f),F + v (G - F)}/.{x->a, y->(f + u (g - f))/.x->a}}],{u,0,1},{v,0,1},PlotStyle->facestyle,Mesh->facemesh,
					FilterRules[{opts},Options[ParametricPlot3D]]//Evaluate],
			Graphics3D[{}]]},
		AxesLabel->{x,y,z}]
		]


End[];

SetAttributes[{
	ArrowSize, Asymptote, AsymptoteStyle, BaseLength, BaseLineStyle,
	BasePoint, BaseVectorStyle, Between, BoundVector, Color, 
	ConstraintStyle, ContourPlotPosition, CylinderRange, DashedLineStyle, 
	DerivativeStyle, DiscreteIntegral, DrawArrowheads, DrawAxis, 
	DrawBaseLine, DrawCylinder, DrawLines, DrawPlane, Euler, DrawGraph, InitialValue, 
	IntegralStyle, InverseStyle, Jump, Jumps, LineLength, LinePoint, 
	LineStyle, Orientation, OSection, PhiSection, PlaneSize, 
	PlotDiscreteIntegral, PlotPointsSection, Points, PointStyle, 
	PolarRay, PolarRayStyle, PolyStyle, ProjectionStyle, (*Region, *)
	RSection, SectionVariables, ShowDifference, Surface, 
	TangentLineStyle, TangentPointStyle, TangentVectorStyle, 
	ThetaSection, VectorScale, VectorStyle, VSection, VSectionStyle, 
	XSection, XSectionStyle, YSection, YSectionStyle, 
	ZSection, ZSectionStyle}, 
	Protected];

SetAttributes[{ApproxType,
	AlternatingSum,   Binormal, BisectionMethod, 
	CenterOfCurvature, ClassifyCriticalPoints, ComplexPlot, CubicSpline, CubicSplineFunction, Curvature, DirCylindricalPlot, DirSphericalPlot, 
	DistancePointLine, 
	DistancePointPlane, KPDiv, DividedDifferenceTable, DvecS, 
	FindCriticalPoints, FindRoots, (*HessianMatrix,*) 
	IteratedLimit, KPLaplacian, LevelCurve, makeCurves, (*NewLimit,*) NewtonMethod, 
	OsculatingCircle, OsculatingPlane, 
	ParaCubicSpline, ParaCubicSplineFunction, ParaPlotCubicSpline, 
	ParaPlotSurfaceField, PlotBand, PlotCubicSpline, 
	PlotEuler, PlotFunction, PlotGradientField, 
	PlotOrbits, PlotOsculatingCircle, 
	PlotOsculatingCircle3D, 
	PlotProjection, PlotProjection3D, PlotReflection, PlotReflection3D, 
	PlotRungeKutta, (*PlotRungeKutta5,*) 
	PlotSurfaceField, PlotTN3D, PlotTnb, PlotTV, 
	PlotVector, PlotVector3D, PlotVectorField, PlotVectorField3D, PolarTangentLine, 
	Potential, PrincipalNormal, PrincipalTangent, RungeKutta, (*RungeKutta5,*) 
	SecantMethod, SelectReal, TableDerivatives, TN, TNB, Torsion, WronskianDet, WronskianMat, 
	(*RightApprox, LeftApprox,  UpperApprox, LowerApprox, UpperApprox3D, LowerApprox3D,*)
	IntegrationRegion3D(*, LagrangeInterpolationFunction*), PlotEquation, PlotEquation3D}, 
		{Protected (* ReadProtected, Locked*)}];
  
SetAttributes[{(*08-19-2012*) PlotSection, ParaPlotSection, (*TangentLine, 08-28-2012 *)
	ContourPlotGradientMethod, CylindricalPlot, DirParametricPlot, 
	DirParametricPlot3D, DirPolarPlot,  KPGrad, ImplicitD, JacobianDet, 
	ParaPlotArea, 
	ParaPlotTangentLine, ParaSurfaceOfRevolution, PiecewiseD,  
	PlotAcceleration, PlotAcceleration3D, PlotArcLengthApprox, 
	PlotArcLengthApprox3D, PlotContourSurface, PlotCylinder, 
	PlotDerivative, (*PlotEquation,*) PlotGradient, PlotGradientMethod, 
	PlotGradientMethodArray, PlotIntegral, PlotInverse, PlotJump, 
	JacobianMatrix, HessianMatrix,	(*NewLimit, *)
	PlotNewton, PlotPlane, 
	PlotProperty, PlotRoots, (*PlotTangentLine, *)
	PlotTangentVector, PlotTangentVector3D, PlotTaylorPoly, 
	PlotTaylorPoly3D, PlotTN, PlotTNB,  
	PlotTube, PlotWeb, PolarPlotArea, 
	PolarPlotTangentVector, SphericalPlot, 
	SurfaceOfRevolution, TaylorPoly},
		{HoldAll, (*ReadProtected, Locked*) Protected}];
	
SetAttributes[RealAbs, HoldAll];

SetAttributes[{LUSumPlot, PlotIntegralApprox, ParaTangentLine,	(*PlotJump, 08-24-2012 *)
	(*(* Added 7-4-11 *) NewLimit,*)
	TrapError, MidpointError, SimpsonError,
	ContourPlotSection, DirectionalDerivative, ParaPlotTangentPlane, 
	ParaTangentPlane, Plot3DArray, PlotConstraint, 
	PlotDirectionalDerivative, (*PlotSection, 08-19-2912*) PlotSlopeField, 
	PlotTangentPlane, TangentPlane, PlotTangentLine, TangentLine, 
	IntegralApprox}, 
		{HoldFirst, (*ReadProtected, Locked*) Protected}];	

SetAttributes[NewLimit,{Listable,Protected}]; (*Added 08-18-2012*)

SetAttributes[{PlotRegression, PlotRegression3D}, 
		{HoldRest, (*ReadProtected, Locked*) Protected}];	

EndPackage[];

