(* ::Package:: *)

(* Context:  KnoxPackages`QuadricsNew` *)  

(* Mathematica Version: 11.1 *)

(* Author:  Original version by Robby S. Villegas. Substantionally revised by Sanjay Nath and Dennis M. Schneider *)

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


BeginPackage["KnoxPackages`QuadricsNew`",
		{"KnoxPackages`Calculus`", "KnoxPackages`LinearAlgebra`", "KnoxPackages`CommonFunctions`"}]

 
 (* options, usage messages, types list,phrase definitions *)
(*Removed WireFrame,WireFrameStyle on 4/1/2015*)
Options[PlotQuadricSurface] = {PrintDisplay->False, DrawAxes->False, PlotStyle->Opacity[.8],
	PrimitivesStyle->{}, PlotPoints->Automatic, QuadricID->True, QuadricPlot->True,
	QuadricView->False}//Sort;
Options[PlotConicSection] = {ConicID->True, ConicPlot->True,
	PrintDisplay->False,DrawAxes->False, 
	PrimitivesStyle->{Thickness[.0125], Dashing[{0.03, 0.03}],PointSize[0.04]}}//Sort;


PlotQuadricSurface::usage = "PlotQuadricSurface[poly, {x, y, z}] takes a polynomial in three or fewer of the variables {x, y, z}, of degree 2 or less, and identifies the quadric surface (degenerate or not) determined by poly = 0, producing a graph.  Option QuadricPlot->(True or False) decides plot or no plot (default=True). Rotation and translation are performed if needed, and the translated form and canonical form sans translations are displayed in a Print Cell if requested by PrintDisplay->True. After evaluation, the global variable QuadricID[] will contain a
data-structure containing the type of quadric, its canonical equation, and its coordinate system.  With option QuadricID->False, only the type is stored. When poly has mixed terms and requires a rotated coordinate system, preferred axial variables for the new system can be specified as PlotQuadricSurface[poly,
{x, y, z}, {u, v, w}]. Without specifying axial variables the system will choose unique axial variables. For a list of all quadric types,  see QuadricTypes.";

PlotConicSection::usage = "PlotConicSection[poly, {x, y}] takes a polynomial in two or fewer of the variables {x, y}, of degree 2 or less, and identifies the conic section (degenerate or not) determined by poly = 0, producing a graph.  Option ConicPlot->(True or False) decides plot or no plot.  Rotation and translation are performed if needed, and the translate form and canonical form sans translations can be displayed in a Print Cell, using the option  PrintDisplay->True. After evaluation, the global variable ConicID[] will contain a data-structure containing the type of conic, its canonical equation, and its coordinate system.  With option ConicID->False, only the type is stored. When poly has mixed terms and requires a rotated coordinate system, preferred axial variables for the new system can be specified as PlotConicSection[poly,{x, y}, {u, v}] or will be uniquely determined by the program. For a list of all conic types, see ConicTypes.  Also takes PlotStyle as an option.";

NPlotQuadricSurface::usage = "NPlotQuadricSurface is the same function as PlotQuadricSurface except that it converts all numbers in the input to floating-point approximations and uses these in all the computations.  This results in a vast decrease in time-consumption for polynomials requiring diagonalization, and simplifications will occupy negligible time.  However, accumulated numerical errors may result in small holes in some graphs.";

NPlotConicSection::usage = "NPlotConicSection is the same function as PlotConicSection except that it converts all numbers in the input to floating-point approximations and uses these in all the computations.  This results in a vast decrease in time-consumption for polynomials requiring diagonalization, and simplifications will occupy negligible time.";

QuadricID::usage = "QuadricID[] is a global variable which is set every time PlotQuadricSurface is executed.  It identifies the quadric surface most recently analyzed.  Sans empty brackets, QuadricID is the option which specifies whether or not full information should be stored in the variable.  If QuadricID->False, QuadricID[] is assigned to the symbol for the type of quadric.  If QuadricID->True, QuadricID[] will contain a data-structure type[canon, axialvars, coordmatrix, origin] identifying the quadric surface.  In this expression, the head (type) is the symbol for the kind of quadric surface, canon represents the canonical equation as a list of the variables and associated parameters,
axialvars is the list of variables for the coordinate axes, coordmatrix is the list of basis vectors for the (transformed) axes, and origin gives the translations with respect to the system given by coordmatrix.  The first element, canon, has a different template for each quadric surface, so see info for the particular type.";

ConicID::usage = "ConicID[] is a global variable which is set every time PlotConicSection is executed.  It identifies the conic section most recently analyzed.  Sans empty brackets, ConicID is the option which specifies whether or not full information should be stored in the variable.  If ConicID->False, ConicID[] is assigned to the symbol for the type of conic.  If ConicID->True, ConicID[] will contain a data-structure type[canon, axialvars, coordmatrix, origin] identifying the conic section.  In this expression, the head--type--is the symbol for the kind of conic section, canon represents the canonical equation as a list of the variables and associated parameters, axialvars is the list of variables for the coordinate axes, coordmatrix is the list of basis vectors for the (transformed) axes, and origin gives the translations with respect to the system given by coordmatrix.  The first element, canon, has a different template for each conic section, so see info for the particular type.";

QuadricPlot::usage = "QuadricPlot is the option of PlotQuadricSurface whose Boolean setting specifies whether or not graphics should be output.  It is also the function to which PlotQuadricSurface sends the quadric ID information for plotting.  See GraphQuadric.  Though normally called as a subroutine by the main program, QuadricPlot is accessible to the user who is familiar with the data-structures by which PlotQuadricSurface represents quadrics. The user can send any desired quadric information directly to the plotting function as QuadricPlot[quadric ID].";

ConicPlot::usage = "ConicPlot is the option of PlotConicSection whose Boolean setting specifies whether or not graphics should be output.  It is also the function to which PlotConicSection sends the conic ID information for plotting.  See GraphConic.  Though normally called as a subroutine by the main program, ConicPlot is accessible to the user who is familiar with the data-structures by which PlotConicSection represents conics.  The user can send any desired conic information directly to the plotting function as conicplot[conic ID].";

QuadricView::usage = "QuadricView is used with the Show command to display the quadric surface generated by PlotQuadricSurface with Mathematica's defaults of vertical orientation and viewer vantage point but measured with respect to the rotated and/or translated coordinate system instead of the standard coordinate system.  E.g., Show[%, QuadricView[QuadricID[]] ].";

(*PrintDisplay::usage = "PrintDisplay is an option of both PlotQuadricSurface and PlotConicSection.  PrintDisplay->True prints the polynomial with squares completed, the canonical form of the equation, and the eigenvalues and eigenvectors.";
*)
DrawAxes::usage = "DrawAxes is an option of PlotQuadricSurface and PlotConicSection which decides whether color-coded axes will be drawn to orient the graphics output in the diagonalized coordinate system. Its settings are 'True' and 'False'.";

PrimitivesStyle::usage = "PrimitivesStyle is an option of PlotQuadricSurface which can be set to a list of graphics 
directives to be applied to the primitives Point, Line, or Polygon used in graphing the degenerate cases. In PlotConicSection the option PrimitivesStyle affects all types except ellipses, hyperbolas, and parabolas, which take the option PlotStyle instead.";

GraphQuadric::usage = "GraphQuadric is the function containing, under the various quadric types as tags, the mathematical formulae and graphics expressions for the various quadrics of PlotQuadricSurface. It is subordinate to QuadricPlot which sets up several temporary functions that are Absolutely necessary for each GraphQuadric routine.  Thus, it is not an independent, directly usable function for the user.";

GraphConic::usage = "GraphConic is the function containing, under the various conic types as tags, the mathematical formulae and graphics expressions for the various conics of PlotConicsection. It is subordinate to conicplot which sets 
up several temporary functions that are Absolutely necessary for each GraphConic routine.  Thus, it is not an independent, directly usable function for the user.";

QuadricTypes::usage = "This tag contains the list of symbols designating the various types of quadric sets classified by PlotQuadricSurface.  You can request an information message on each of the types by name.";

QuadricTypes =
{EmptySet, Universe, CoordinatePlane1, VerticalPlane, ObliquePlane, CoordinatePlane2, ParallelPlanes, ParabolicCylinder,
ObliqueParabolicCylinder, CoordinateAxis, IntersectingPlanes, EllipticCylinder, HyperbolicCylinder, EllipticParaboloid,
HyperbolicParaboloid, Origin, EllipticCone, Ellipsoid1, hyperboloid1, hyperboloid2} ;

QuadricPhrase::usage = "QuadricPhrase[type] gives a short phrase describing the geometric quadric set represented by the symbol type, which must be a member of the list QuadricTypes.  For example, the phrase for hyperboloid1 is \"a hyperboloid of one sheet\".";

QuadricPhrase[Universe] ^= "the entire coordinate space.";
QuadricPhrase[EmptySet] ^= "the empty set.";
QuadricPhrase[CoordinatePlane1] ^= "a coordinate plane.";
QuadricPhrase[VerticalPlane] ^= "a plane perpendicular to a coordinate plane.";
QuadricPhrase[ObliquePlane] ^= "a plane oblique to all coordinate planes.";
QuadricPhrase[CoordinatePlane2] ^= "a coordinate plane.";
QuadricPhrase[ParallelPlanes] ^= "a pair of planes equidistant from and parallel to a coordinate plane";
QuadricPhrase[ParabolicCylinder] ^= "a parabolic cylinder with cylindrical axis concurrent with a coordinate axis.";
QuadricPhrase[ObliqueParabolicCylinder] ^= "a parabolic cylinder with cylindrical axis oblique to all coordinate axes.";
QuadricPhrase[CoordinateAxis] ^= "a coordinate axis.";
QuadricPhrase[IntersectingPlanes] ^= "a pair of intersecting planes perpendicular to a coordinate plane.";
QuadricPhrase[EllipticCylinder] ^= "an elliptic cylinder.";
QuadricPhrase[HyperbolicCylinder] ^= "a hyperbolic cylinder.";
QuadricPhrase[EllipticParaboloid] ^= "an elliptic paraboloid.";
QuadricPhrase[HyperbolicParaboloid] ^= "a hyperbolic paraboloid.";
QuadricPhrase[Origin] ^= "the origin of the coordinate space.";
QuadricPhrase[EllipticCone] ^= "an elliptic cone.";
QuadricPhrase[Ellipsoid1] ^= "an ellipsoid.";
QuadricPhrase[hyperboloid1] ^= "a hyperboloid of one sheet.";
QuadricPhrase[hyperboloid2] ^= "a hyperboloid of two sheets.";

ConicTypes::usage = "This tag contains the list of symbols designating the various types of conic sections classified by PlotConicSection.  You can request an information message on each of the types by name.";

ConicTypes = {EmptySet, Universe, CoordinateAxis1, ObliqueLine, CoordinateAxis2, ParallelLines, Parabola, Origin, Ellipse,
	IntersectingLines, Hyperbola} ;

ConicPhrase::usage = "ConicPhrase[type] gives a short phrase describing the geometric conic set represented by the symbol type, which must be a member of the list ConicTypes.  For example, the phrase for ParallelLines is \"a pair of lines equidistant from and parallel to a coordinate axis.\".";

ConicPhrase[Universe] ^= "the entire coordinate plane.";
ConicPhrase[EmptySet] ^= "the empty set.";
ConicPhrase[CoordinateAxis1] ^= "a coordinate axis.";
ConicPhrase[ObliqueLine] ^= "a line oblique to both coordinate axes.";
ConicPhrase[CoordinateAxis2] ^= "a coordinate axis.";
ConicPhrase[ParallelLines] ^= "a pair of lines equidistant from and parallel to a coordinate axis.";
ConicPhrase[Parabola] ^= "a parabola.";
ConicPhrase[Origin] ^= "the origin of the coordinate plane.";
ConicPhrase[IntersectingLines] ^= "a pair of intersecting lines.";
ConicPhrase[Ellipse] ^= "an ellipse.";
ConicPhrase[Hyperbola] ^= "a hyperbola.";

                   (* THE TYPES FOR QS AND CS *)
	   
Universe::usage = "Universe is the PlotQuadricSurface and PlotConicSection type denoting the entire coordinate space, 
and is returned when the input reduces to the form 0 = 0, with all variables free. In QuadricID[] and ConicID[], the
equation 0 = 0 is represented as the empty list.";

EmptySet::usage = "This is the PlotQuadricSurface and PlotConicSection type denoting the empty set. Such occurs in PlotQuadricSurface when the reduced equation has one of four forms:  d = 0 (where d is a nonzero number), u^2/a^2 = -1, u^2/a^2 + v^2/b^2 = -1, or u^2/a^2 + v^2/b^2 + w^2/c^2 = -1.  The parameters for these in the Empty-object assigned to QuadricID[] are, by degree:  (0) {d}; (1) {u, a}; (2) {{u, a}, {v, b}}; and (3) {{u, a}, {v, b}, {w, c}}. It occurs in PlotConicSection when the reduced equation has one of three forms: d = 0 (where d is a nonzero number), u^2/a^2 = -1, or u^2/a^2 + v^2/b^2 = -1.  The respective lists representing these canonical forms in ConicID[] are, by degree:  (0) {d}; (1) {u, a}; and (2) {{u, a}, {v, b}}.";

CoordinatePlane1::usage = "This symbol denotes the type returned by PlotQuadricSurface when the input reduces to an equation containing a single linear variable set to zero, with two variables free:  u = 0, in variables {u, v, w}, {v, w} being free.  This represents the vw-plane in uvw-space.  The parameter list for this form in QuadricID[] is {u, {v, w}}.";

VerticalPlane::usage = "This PlotQuadricSurface symbol identifies a quadric set which is a single plane perpendicular to one coordinate plane. In general, this is the solution set of the reduced equation:  a u + b v = 0, in the variables {u, v, w}, with w free.  In QuadricID[], this takes the form {{u, a}, {v, b}, w}.";

ObliquePlane::usage = "This PlotQuadricSurface type identifies the quadric set whose reduced equation is:  a u + b v + c w = 0, in the variables {u, v, w}.  The solution set is a plane oblique to all three coordinate planes.  QuadricID[] lists the parameters as {{u, a}, {v, b}, {w, c}}.";

CoordinatePlane2::usage = "The PlotQuadricSurface type for input that reduces to the equation u^2 = 0, in variables {u, v, w}, with {v, w} free.  As for CoordinatePlane1, the solution set is the vw-plane; the only difference is that the bound variable is quadratic instead of linear.  The equation is represented in QuadricID[] as {u, {v, w}}.";

ParallelPlanes::usage = "PlotQuadricSurface's symbol for the quadric set consisting of two planes parallel to a coordinate plane, described by the reduced equation:  u^2 = d, in the variables {u, v, w}, with {v, w} free, and d > 0.  This equation is represented in QuadricID[] as {{u, Sqrt[d]}, {v, w}}.";

ParabolicCylinder::usage = "This symbol represents the PlotQuadricSurface type for a reduced equation of the form 
v = m u^2, in variables {v, u, w}, with w being free.  The axis of the cylinder is perpendicular to the uv-plane. This 
reduced equation is represented in QuadricID[] as {v, {u, m}, w}.";

ObliqueParabolicCylinder::usage = "This represents the type returned by PlotQuadricSurface when the reduced equation is:  a u^2 + b v + c w = 0, in variables {u, v, w}.  The axis of the parabolic cylinder is oblique to the coordinate planes. For this equation, the parameters of QuadricID[] are in the form {{u, a}, {v, b}, {w, c}}.";

CoordinateAxis::usage = "The symbol representing the PlotQuadricSurface type returned when the standardized 
equation for the input has the form u^2/a^2 + v^2/b^2 = 0, where u and v are variables, and w is free. Geometrically, 
the solution set is the w-axis, after translations. This equation is represented in QuadricID as {{u, a}, {v, b}, w}.";

IntersectingPlanes::usage = "This PlotQAuadricSurface symbol denotes a quadric set consisting of two intersecting planes, determined by the reduced equation:  u^2/a^2 = v^2/b^2, in variables {u, v, w}, where w is free.  In QuadricID[], this equation is formatted as the list {{u, a}, {v, b}, w}.";

EllipticCylinder::usage = "The symbol which represents the type of PlotQuadricSurface whose standardized equation is:  u^2/a^2 + v^2/b^2 = 1, in variables {u, v, w}, with w free.  QuadricID[] contains this equation in the form {{u, a}, {v, b}, w}.";

HyperbolicCylinder::usage = "This is the symbol representing the PlotQuadricSurface type described by the standardized equation:  u^2/a^2 - v^2/b^2 = 1, in variables {u, v, w}, with w free.  This equation is in QuadricID[] as the list {{u, a}, {v, b}, w}.";

EllipticParaboloid::usage = "This symbol represents the elliptic paraboloid, the quadric surface whose standardized equation a la PlotQuadricSurface is u^2/a^2 + v^2/b^2 = s w, where {u, v, w} are the variables, and s is a sign, \[PlusMinus]1.  This equation in QuadricID[] has the form {{u, a}, {v, b}, {w, s}}.";

HyperbolicParaboloid::usage = "This symbol denotes the hyperbolic paraboloid, as classified by PlotQuadricSurface when the input reduces to u^2/a^2 - v^2/b^2 = w, in the variables {u, v, w}.  This type's equation is given in QuadricID[] as {{u, a}, {v, b}, w}.";

Origin::usage = "This symbol denotes the quadric set consisting of a single point, which PlotQuadricSurface identifies from the standardized form:  u^2/a^2 + v^2/b^2 + w^2/c^2 = 0, in variables {u, v, w}.  This equation is listed in QuadricID[] as {{u, a}, {v, b}, {w, c}}.";

EllipticCone::usage = "This symbol denotes the quadric surface known as the elliptic cone, and is returned by PlotQuadricSurface when the input's reduced form is:  u^2/a^2 + v^2/b^2 = w^2, in the variables {u, v, w}.  This form is in QuadricID[] as {{u, a}, {v, b}, w}.";

Ellipsoid1::usage = "This symbol identifies an ellipsoid, the PlotQuadricSurface type represented by the reduced equation:  u^2/a^2 + v^2/b^2 + w^2/c^2 = 1, in variables {u, v, w}.  The equation is represented in QuadricID[] as {{u, a}, {v, b}, {w, c}}.";

hyperboloid1::usage = "This symbol is used by PlotQuadricSurface for the hyperboloid of one sheet, the quadric surface represented by the reduced equation u^2/a^2 + v^2/b^2 = w^2/c^2 + 1, in the variables {u, v, w}.  In QuadricID[], this equation has the form {{u, a}, {v, b}, {w, c}}.";

hyperboloid2::usage = "The symbol used by PlotQuadricSurface for the hyperboloid of two sheets, the quadric surface whose standardized equation is u^2/a^2 + v^2/b^2 = w^2/c^2 - 1, in the variables {u, v, w}.  QuadricID[] represents the equation as the list {{u, a}, {v, b}, {w, c}}.";

CoordinateAxis1::usage = "CoordinateAxis1 is the PlotConicSection type for the solution set of a reduced equation where one linear variable is present and set to zero:  u = 0, in variables {u, v}, with v free, which determines the v-axis in the uv-plane.  This equation is represented in ConicID[] as {u, {v}}.";

ObliqueLine::usage = "ObliqueLine is the PlotConicSection type for a reduced equation of a u + b v = 0, in variables {u, v}, which determines a line oblique to both the u-axis and the v-axis. ConicID[] lists this equation as {{u, a}, {v, b}}.";

CoordinateAxis2::usage = "CoordinateAxis2 is the PlotConicSection type for a reduced equation of the form u^2 = 0, in variables {u, v}, with v free.  The only difference between this and CoordinateAxis1 is in the reduced equation; here the bound variable is quadratic.  The equation is represented in ConicID[] as {u, {v}}.";

ParallelLines::usage = "ParallelLines is the PlotConicSection type for a reduced equation of the form u^2 = d, d > 0, in variables {u, v} with v free, which determines a pair of lines equidistant from and parallel to the v-axis.  This equation is represented in ConicID[] as {{u, p}, {v}}, where p = Sqrt[d].";

Parabola::usage = "Parabola is the PlotConicSection type for a reduced equation of the form v = m u^2, in variables {u, v}, which determines a parabola with directrix parallel to the u-axis.  This is represented in ConicID[] as {v, {u, m}}.";

IntersectingLines::usage = "IntersectingLines is the PlotConicSection type for the reduced equation u^2/a^2 = v^2/b^2, in variables {u, v}, which determines a pair of lines intersecting at the origin of the uv-plane.  In ConicID[] this equation is listed as {{u, a}, {v, b}}.";

Ellipse::usage = "Ellipse is the PlotConicSection type for the reduced equation u^2/a^2 + v^2/b^2 = 1, in variables {u, v}, which determines an ellipse.  This equation is in ConicID[] in the form {{u, a}, {v, b}}.";

Hyperbola::usage = "Hyperbola is the PlotConicSection type for the reduced equation u^2/a^2 - v^2/b^2 = 1, in variables {u, v}, which determines a hyperbola.  In ConicID[], this is expressed as {{u, a}, {v, b}}.";




(* Error messages *) 

QSCS::noplot = "QuadricPlot does not return a plot for
quadric type `1`." ;

QSCS::varslist = "The specified or assumed `1` variables `2` are not a list of distinct symbols.";

QSCS::varlists = "The original variables `1` and new variables `2` are lists of unequal lengths.";

QSCS::numbervars = "The list of variables `2` has the wrong number of variables.";

QSCS::complexinput = "The input expression `1` contains
complex numbers; only real polynomials are allowed.";

QSCS::polyvars = "The input `1` is not a polynomial
in the specified or assumed variables `2`; its variables are
`3`.";

QSCS::notpoly = "The input expression `1` is not a
polynomial in the specified or assumed variables `2`.";

QSCS::polydegree = "The input polynomial `1` has degree
higher than 2 in the variables `2`.";

QSCS::complexdiag = "Sorry, the diagonalization
resulted in complex numbers in the quadratic or the coordinate
basis.  ConicSection cannot continue.";

QSCS::invalidopts = "The given options `1` are invalid
and cannot be passed as valid graphics options.";

QSCS::invalidbool = "The options `1` are invalid; the
settings must be Boolean values:  True or False.";

QSCS::varsconflict = "There is a conflict between the
original and new variables (explicitly specified or assumed). 
The two sets of variables have `1` in common, when they should be
disjoint.";


QSCS::nonvars = "The specified `1` variables
`2` are not legal variables; they are write-protected or raw
objects in Mathematica.  Be sure your variables don't already
have assigned values, such as numbers, strings, algebraic
expressions, or the like.";





Begin["`Private`"]

                  
                      (* Auxiliary Functions *)
		      
		      
(****************** New Templates *********************)
(*
PlotConicSection[rhs_ == lhs_, rest__] := PlotConicSection[rhs - lhs, rest];

NPlotConicSection[rhs_ == lhs_, rest__] := NPlotConicSection[rhs - lhs, rest];
*)
	      
(*Used in Mma_v10   
BooleanQ[expr_] := MatchQ[expr, True | False]
*)


mixedQ[poly_, varslist_] :=
	With[{anyvar = Alternatives @@ varslist},
		MemberQ[poly, _.(v1:anyvar)*(v2:anyvar) /; Not[v1 === v2],{0, 1} ]]

VariableQ[expr_] := Check[Set[expr, 0]; Unset[expr]; True,False, Set::write, Set::setraw, Set::wrsym]

(* Bug in Mathematica:  "RepeatedNull", or "...", would not accept the
    empty list in the second template for SignCount below, so a separate
    definition for the particular value "{}" has to be entered. *)

SignCount[{}] = Thread @ { {-1, 0, +1}, 0 }

SignCount[list:{ (_Integer | _Rational | _Real).. }] :=
	Block[ {signslist = Sign[list]},
  	Map[{#, Count[signslist, #]}&, {-1, 0, 1}] //Return]

symbnumQ = NumberQ[N[#]]& ;
Attributes[translation] = Listable;
translation[_. (var_ + h_.)^_. + _., var_] := -h;
translation[poly_, var_] := 0 /; FreeQ[poly, var];

sqrform[ {{u_, a_}, {v_, b_}}, -1 ] :=
  With[{sqru = sqrform[{u, a}], sqrv = sqrform[{v, b}]},HoldForm[sqru - sqrv]];
sqrform[ pairs:{{_, _}..} ] := Plus @@ Map[sqrform, pairs];
sqrform[{num_, denom_}] = HoldForm[num^2 / denom^2];
eqntemplate = "`1`  =  `2`";
freetemplate = StringJoin[eqntemplate, ".    `3`"];


printeqn[lhs_, rhs_] := Print @
  StringForm[eqntemplate, lhs, rhs]
  
printeqn[lhs_, rhs_, freevars_List] :=
Module[{frees = Length[freevars], freestr},
  Which[
    frees == 1,
      freestr = SequenceForm[freevars[[1]], " is free."],
    frees == 2,
      freestr = SequenceForm[freevars[[1]], " and ",
        freevars[[2]], " are free."]
  ];
  Print @ StringForm[freetemplate, lhs, rhs, freestr]]



PrintPhrase[phrase_,type_] :=
CompoundExpression[
  Print[" "];
  Print @ ToString @ SequenceForm["The equation determines ",phrase[type], "  After translation, the simplified quadric equation is:"];
  Print[" "]]
quotes = str_String :> InputForm[str] ;



    (* Auxillary Functions for plotting routines *)

symbnum3vec = Thread @ PatternTest[{_, _, _}, symbnumQ] ;

Parallelogram3D[dirs:{symbnum3vec, symbnum3vec}] :=
  Polygon @ Map[Dot[#, dirs]&, corners]

Parallelogram3D[dirs:{symbnum3vec, symbnum3vec},origin:symbnum3vec] :=
  Polygon @ Map[(origin + #)&, Dot[#, dirs]& /@ corners ]

corners = { {1, 1}, {-1, 1}, {-1, -1}, {1, -1} } ;

QuadricView[quadricID:type_[canon_, axialvars_, coordmatrix_,origin_] ] :=
Sequence[
  ViewVertical->
    Options[Graphics3D, ViewVertical][[1, 2]] . coordmatrix,
  ViewPoint->
    Options[Graphics3D, ViewPoint][[1, 2]] . coordmatrix]

(*setQSaxes[newaxes_,picrange_,picboxrat_,colors_] :=  Thread @ {Thread @ {Map[SurfaceColor[#]&,colors],
	                              Arrowhead3D[newaxes,picrange,picboxrat,{{},{},{}}]},
	                             Thickness[0],colors,Map[Line[#]&,newaxes]};*)

setQSaxes1[newaxes_, colors_] := 
 MapThread[{Arrowheads[Medium], #1, Arrow[#2]} &, {colors, newaxes}]
								 
(*setCSaxes[newaxes_,picrange_,aspect_,colors_,opts___] :=  Thread @ {Thread @ {colors,
	                              Arrowhead[newaxes,picrange,aspect,{{},{}},opts]},
	                             Thickness[0],colors,Map[Line[#]&,newaxes]};*)
setCSaxes1[newaxes_, colors_] := 
 MapThread[{Arrowheads[Medium], #1, Arrow[#2]} &, {colors, newaxes}]
				     
QuadOpts = First /@ Options[PlotQuadricSurface];
QSGraphOpts = Join[{PlotStyle, PrimitivesStyle, DrawAxes},First /@ Options[ParametricPlot3D]];

QSValidOpts = Union[QuadOpts, QSGraphOpts];

 (******************************************************************)
 (*************** BEGIN QUADRIC SURFACE PORTION ********************)
 (******************************************************************)
 
               (* MAIN PROGRAM PlotQuadricSurface: *)

			   
PlotConicSection[rhs_ == lhs_, rest__] := PlotConicSection[rhs - lhs, rest];
PlotQuadricSurface[rhs_ == lhs_, rest__] := PlotQuadricSurface[rhs - lhs, rest];

NPlotConicSection[rhs_ == lhs_, rest__] := NPlotConicSection[rhs - lhs, rest];
NPlotQuadricSurface[rhs_ == lhs_, rest__] := NPlotQuadricSurface[rhs - lhs, rest];

			   
PlotQuadricSurface[inputpoly_, inputvars_List, opts___?OptionQ] :=	
 Module[{newvars = Map[Unique[ToString[#]]&, inputvars]},
  PlotQuadricSurface[inputpoly, inputvars, newvars, opts]]   

  
PlotQuadricSurface[inputpoly_, inputvars_List, newvars_List, opts___?OptionQ]:=
Module[{    (* Testing the input for validity *)

       inputpolyN = Chop @ Expand[1.*inputpoly], origvars,
       QSopts, plotopts,
            (* preparation, analysis, display translate form *)
       rotateQ, printQ, poly, coordmatrix, axialvars,
       printeqnQ, drawaxes, quadvars, linvars, freevars,
       q, l, quadcoeffs, lincoeffs, quadsigns, linsigns,
       qsc, completedsqrs, rsimp, translateform,
       mindistpt, lincoeffs1ToStr, standardlin, d, dsign, origin,
       rotstr, anylin, unitsrules, printform,
       type, canon, quadricid,nquadricid,
            (* quadric ID construction, display canonical *)
       a, b, c, m, coeffsorder, varsorder, s, lonesign,
       conevarpos, conevar, conecoeff, ellipsevarspos,
       ellipsevars, ellipsecoeffs, canoncoeffs, canonsigns,
       axisvarpos, axisvar, axiscoeff,
            (* Decisions concerning final output *)
       ID, plot, axeslabels ,QSboolopts, invalidbool, display, translstr
      },
              (* Error-checking for input *)
	If[Not[FreeQ[inputpolyN, Complex]], Return @
    	Message[QSCS::complexinput, inputpoly] ];

	If[Not[MatchQ[inputvars, vars:{_, _, _} /; UnsameQ @@ vars]],
    	Return @ Message[QSCS::numbervars, "original",inputvars] ];

	With[{commonvars = Intersection[inputvars, newvars]},
        If[Not[commonvars === {}], Return @
          Message[QSCS::varsconflict, commonvars] ]  ];

	origvars = Variables[inputpolyN];

	If[Not[PolynomialQ[inputpolyN, inputvars]], Return @
		Message[QSCS::notpoly, inputpoly /. quotes,origvars /. quotes] ];
	If[Not[SubsetQ[inputvars, origvars ]], Return @
    	Message[QSCS::polyvars, inputpoly /. quotes,inputvars /. quotes, origvars /. quotes] ];
	If[PolynomialDegree[inputpolyN, inputvars] > 2, Return @
    	Message[QSCS::polydegree, inputpoly /. quotes,origvars /. quotes] ];
		
	With[{optnames = First /@ {opts} },
		If[Not[SubsetQ[QSValidOpts, optnames]], Return @
			Message[QSCS::invalidopts, Complement[optnames,QSValidOpts]] ]  ];

	QSopts = Sequence @@ Select[{opts}, MemberQ[QuadOpts, First[#] ]& ];
	QSboolopts = ( Thread @ Rule[#, # /. {QSopts} /. Options[PlotQuadricSurface] ] )& @ {QuadricID, QuadricPlot};
	invalidbool = Select[QSboolopts, (Not[BooleanQ[Last[#]]])& ];
	If[Not[invalidbool === {}], Return @ Message[QSCS::invalidbool, invalidbool] ];
            (* Separate the options to be passed *)
	display = PrintDisplay /. {QSopts} /. Options[PlotQuadricSurface];
	With[{memberQ = Not @ MemberQ[{True, False}, display]},
    	If[memberQ, Return @ Message[QSCS::invalidbool, display] ]];
	If[display === False, 
		printeqnQ = Hold; printQ = Hold, printeqnQ = printeqn;printQ = CompoundExpression];
	plotopts = Sequence @@ Select[{opts}, MemberQ[QSGraphOpts,First[#] ]& ];
	drawaxes = DrawAxes /. {plotopts} /. Options[PlotQuadricSurface];
  	If[Not[BooleanQ[drawaxes]], Return @ Message[QSCS::invalidbool, DrawAxes -> drawaxes] ];
	rotateQ = mixedQ[inputpolyN, inputvars];

            (* Determine coordinate basis and polynomial therein. *)

(*	{poly, coordmatrix} = Chop @ If[Not[rotateQ],
			axialvars = inputvars;
      			{Simplify[inputpoly], IdentityMatrix[3]},
			DiagQuad[inputpoly,inputvars,axialvars = newvars][[{1,3}]]//Simplify];*)
			
{poly, coordmatrix} = DiagQuad[inputpoly,inputvars,axialvars = newvars][[{1,3}]]//Simplify;

              (* Begin analyzing quadric form *)

  quadvars = Select[axialvars, (Exponent[poly, #] == 2)&];


  linvars = Select[Complement[axialvars, quadvars],
    (Exponent[poly, #] == 1)&];

  freevars = Complement[axialvars, quadvars ~Union~ linvars];

  {q, l} = Length /@ {quadvars, linvars};

  quadcoeffs = Simplify @ Map[Coefficient[poly, #, 2]&, quadvars];

  lincoeffs = Simplify @ Map[Coefficient[poly, #, 1]&, linvars];

  {quadsigns, linsigns} = Sign /@
    Chop[N @ {quadcoeffs, lincoeffs}];
  {qsc[-1], qsc[0], qsc[+1]} = Last /@ SignCount[quadsigns];
  completedsqrs = CompleteSquare[poly, axialvars]//Chop;


	rsimp = If[AtomQ[completedsqrs],0, 
		Select[completedsqrs,FreeQ[#,Alternatives[Sequence @@ axialvars]]& ]];

(*********** Changed Norm from being numerical**********)
  translateform = Chop @
    If[l == 0, completedsqrs, (* else: *)
      mindistpt = Simplify[(-rsimp/Norm[lincoeffs,Reals]^2) lincoeffs];
      lincoeffs1ToStr =
        Map[Replace[#, {1 -> "1", -1 -> -"1"}]&, lincoeffs];
          (* Coefficients of +- 1 will not hold variable and  *)
          (* translation constant together in parentheses,  *)
          (* like (z - 4), but strings cannot be discarded.  *)
      standardlin = lincoeffs1ToStr . (linvars - mindistpt);
      completedsqrs - rsimp - (lincoeffs . linvars) + standardlin
    ];
	
  d = If[l == 0, rsimp, 0];
  dsign = Sign[N[d]];
  origin = translation[translateform, axialvars];
  
        (* Prepare display of polynomial with translations *)
  rotstr = If[rotateQ, ", after rotation,", ""];
  translstr = If[l == 0, ":",
    " and translations for linear variables proportionate to 
    coefficients for constant of 0:"];
  printQ[
    Print[" "],
    Print @ StringJoin["Simplified equation", rotstr,
      " with squares completed", translstr]
  ];

 

printform =
    If[l == 0, translateform, (* else: *)
      anylin = Alternatives @@ linvars;
      unitsrules =
        {"1" ((var:anylin) + h_) :> SequenceForm["(", var + h, ")"],
         ("1" var:anylin) :> var};
      translateform /. unitsrules
    ];
(*
Print[{"printform",printform}];

Print[{"translateform",translateform}];
*)
printform = translateform;
  printQ[
    Print[" "],
    Print[printform, "  =  ", 0]
  ];

                (* CLASSIFICATION OF THE QUADRIC *)
  Which[
  q == 0,
    Which[
    l == 0, If[dsign == 0, type = Universe, type = EmptySet],
    l == 1, type = CoordinatePlane1,
    l == 2, type = VerticalPlane,
    l == 3, type = ObliquePlane
         ],
  q == 1,
    Which[
    l == 0, Which[dsign == 0, type = CoordinatePlane2,
                  dsign == -quadsigns[[1]], type = ParallelPlanes,
                  dsign == quadsigns[[1]], type = EmptySet
                  ],
    l == 1, type = ParabolicCylinder,
    l == 2, type = ObliqueParabolicCylinder
         ],
  q == 2,
    Which[
    l == 0, Which[dsign == 0,
                    If[Not[Equal @@ quadsigns],
                       type = IntersectingPlanes,
                       type = CoordinateAxis],
                  Equal @@ quadsigns,
                    If[dsign == quadsigns[[1]],
                       type = EmptySet,
                       type = EllipticCylinder],
                  Not[Equal @@ quadsigns],
                    type = HyperbolicCylinder
                 ],
    l == 1, If[Equal @@ quadsigns,
               type = EllipticParaboloid,
               type = HyperbolicParaboloid]
         ],
  q == 3,
    Which[dsign == 0, If[Equal @@ quadsigns,
                         type = Origin,
                         type = EllipticCone],
          dsign != 0,
            Which[Equal @@ quadsigns,
                    If[quadsigns[[1]] == dsign,
                       type = EmptySet,
                       type = Ellipsoid1],
                  Not[Equal @@ quadsigns],
                    Which[qsc[dsign] == 1, type = hyperboloid1,
                          qsc[dsign] == 2, type = hyperboloid2
                          ]
                 ]
         ]
  ];   (* :end Which[] *)
            (* DISPLAY CANONICAL EQUATION AND CONSTRUCT  *)
            (* REPRESENTATION FOR QUADRIC ID.           *)


  If[display === True, PrintPhrase[QuadricPhrase,type]];
  Which[
  type === Universe,
    If[display === True,
      Print["0 = 0.  All variables are free."] ];
    canon = {},
  type === EmptySet,
    Which[
    q == 0,
      printeqnQ[d, 0];
      canon = {d},
    q == 1,
      d = -d/quadcoeffs[[1]] //Simplify;
      printeqnQ[quadvars[[1]]^2, d];
      canon = {{quadvars[[1]], d}, freevars},
    q == 2,
      {a, b} = 1 / Sqrt[quadcoeffs/d] //Simplify;
      printeqnQ[sqrform @ Transpose[{quadvars, {a, b}}], -1 ];
      canon = Transpose[{quadvars, {a, b}}] ~Join~ freevars,
    q == 3,
      {a, b, c} = 1 / Sqrt[quadcoeffs/d] //Simplify;
      printeqnQ[sqrform @ Transpose[{quadvars, {a, b, c}}], -1];
      canon = Transpose[{quadvars, {a, b, c}}]
    ],
  type === CoordinatePlane1,
    printeqnQ[linvars[[1]], 0, freevars];
    canon = Join[linvars, {freevars}],
  type === VerticalPlane,
    printeqnQ[lincoeffs . linvars + d, 0, freevars];
    canon = Transpose[{linvars, lincoeffs}] ~Join~ freevars,
  type === ObliquePlane,
    printeqnQ[lincoeffs . linvars, 0];
    canon = Transpose[{linvars, lincoeffs}],
  type === CoordinatePlane2,
    printeqnQ[quadvars[[1]]^2, 0, freevars];
    canon = Join[quadvars, {freevars}],
  type === ParallelPlanes,
    d = Sqrt[ -d/quadcoeffs[[1]] ] //Simplify;
    printeqnQ[quadvars[[1]]^2, d^2, freevars];
    canon = {Join[quadvars, {d}], freevars},
  type === ParabolicCylinder,
    m = -quadcoeffs[[1]] / lincoeffs[[1]] //Simplify;
    printeqnQ[linvars[[1]], m quadvars[[1]]^2, freevars];
    canon = linvars ~Join~ {Join[quadvars, {m}]} ~Join~ freevars,

  type === ObliqueParabolicCylinder,
    printeqnQ[quadcoeffs . (quadvars^2) + lincoeffs . linvars, 0];
    canon = {Join[quadvars, quadcoeffs]} ~Join~
      Transpose[{linvars, lincoeffs}],

  type === CoordinateAxis,
    {a, b} = Sqrt @ Abs[1/quadcoeffs] //Simplify;
    printeqnQ[sqrform @ Transpose[{quadvars, {a, b}}], 0,
      freevars];
    canon = Transpose[{quadvars, {a, b}}] ~Join~ freevars,
  type === IntersectingPlanes,
    {a, b} = 1/Sqrt @ Abs[quadcoeffs] //Simplify;
    printeqnQ @@ Join[
      Map[sqrform, Transpose[{quadvars, {a, b}}]], {freevars}];
    canon = Transpose[{quadvars, {a, b}}] ~Join~ freevars,
  type === EllipticCylinder,
    {a, b} = Sqrt[-d/quadcoeffs] //Simplify;
    printeqnQ[sqrform @ Transpose[{quadvars, {a, b}}], 1,
      freevars];
    canon = Transpose[{quadvars, {a, b}}] ~Join~ freevars,
  type === HyperbolicCylinder,
    {coeffsorder, varsorder} = Transpose @
      Sort[ Transpose[{quadcoeffs/(-d), quadvars}],
        OrderedQ[N @ {#2, #1}]& ];
    {a, b} = 1 / Sqrt @ Abs[coeffsorder] //Simplify;
    printeqnQ[sqrform[Transpose[{varsorder, {a, b}}], -1], 1,
      freevars];
    canon = Transpose[{varsorder, {a, b}}] ~Join~ freevars,
  type === EllipticParaboloid,
    {a, b} = Sqrt @ Abs[lincoeffs[[1]]/quadcoeffs] //Simplify;
    s = -quadsigns[[1]]*linsigns[[1]];
    printeqnQ[sqrform @ Transpose[{quadvars, {a, b}}],
      s linvars[[1]] ];
    canon = Transpose[{quadvars, {a, b}}] ~Join~
      {Join[linvars, {s}]},
  type === HyperbolicParaboloid,
    {coeffsorder, varsorder} = Transpose @
      Sort[ Transpose[{-quadcoeffs/lincoeffs[[1]], quadvars}],
        OrderedQ[N @ {#2, #1}]& ];
    {a, b} = 1 / Sqrt @ Abs[coeffsorder] //Simplify;
    printeqnQ[sqrform[Transpose[{varsorder, {a, b}}], -1],
      linvars[[1]] ];
    canon = Transpose[{varsorder, {a, b}}] ~Join~ linvars,
  type === Origin,
    {a, b, c} = Sqrt @ Abs[1/quadcoeffs] //Simplify;
    printeqnQ[sqrform @ Transpose[{quadvars, {a, b, c}}], 0];
    canon = Transpose[{quadvars, {a, b, c}}],
  type === EllipticCone,
    lonesign = Switch[{qsc[-1], qsc[+1]},
      {1, 2}, -1,  {2, 1}, +1];
    conevarpos = Position[quadsigns, lonesign][[1, 1]];
    {conevar, conecoeff} =
      {quadvars[[conevarpos]], quadcoeffs[[conevarpos]]};
    ellipsevarspos = Complement[{1, 2, 3}, {conevarpos}];
    {ellipsevars, ellipsecoeffs} =
      {quadvars[[ellipsevarspos]], quadcoeffs[[ellipsevarspos]]};
    {a, b} = 1 / Sqrt[-ellipsecoeffs/conecoeff] //Simplify;
    printeqnQ[sqrform @ Transpose[{ellipsevars, {a, b}}],
      conevar^2 ];
    canon = Transpose[{ellipsevars, {a, b}}] ~Join~ {conevar},
  type === Ellipsoid1,
    {a, b, c} = Sqrt[-d / quadcoeffs] //Simplify;
    printeqnQ[sqrform @ Transpose[{quadvars, {a, b, c}}], 1 ];
    canon = Transpose[{quadvars, {a, b, c} }],
  type === hyperboloid1,
    canoncoeffs = quadcoeffs / (-d);
    canonsigns = Sign[N @ canoncoeffs];
    axisvarpos = Position[canonsigns, -1][[1, 1]];
    {axisvar, axiscoeff} =
      {quadvars[[axisvarpos]], canoncoeffs[[axisvarpos]]};
    c = Sqrt[1 / (-axiscoeff)] //Simplify;
    ellipsevarspos = Complement[{1, 2, 3}, {axisvarpos}];
    {ellipsevars, ellipsecoeffs} = 
      {quadvars[[ellipsevarspos]], canoncoeffs[[ellipsevarspos]]};
    {a, b} = Sqrt[1 / ellipsecoeffs] //Simplify;
    printeqnQ[sqrform @ Transpose[{ellipsevars, {a, b}}],
      With[{sqrvar = sqrform[{axisvar, c}]},
        HoldForm[sqrvar + 1] ]  ];
    canon = Transpose[{ellipsevars, {a, b}}] ~Join~
      {{axisvar, c}},
  type === hyperboloid2,
    canoncoeffs = quadcoeffs / d;
    canonsigns = Sign[N @ canoncoeffs];
    axisvarpos = Position[canonsigns, -1][[1, 1]];
    {axisvar, axiscoeff} =
      {quadvars[[axisvarpos]], canoncoeffs[[axisvarpos]]};
    c = Sqrt[1 / (-axiscoeff)] //Simplify;
    ellipsevarspos = Complement[{1, 2, 3}, {axisvarpos}];
    {ellipsevars, ellipsecoeffs} =
      {quadvars[[ellipsevarspos]], canoncoeffs[[ellipsevarspos]]};
    {a, b} = Sqrt[1 / ellipsecoeffs] //Simplify;
    printeqnQ[sqrform @ Transpose[{ellipsevars, {a, b}}],
      With[{sqrvar = sqrform[{axisvar, c}]},
        HoldForm[sqrvar - 1] ]
    ];
    canon = Transpose[{ellipsevars, {a, b}}] ~Join~
      {{axisvar, c}}
  ]; (* :end Which[] *)
          (* Print out coordinate basis and translations: *)
  If[display === True,
    Print[" "];
    Print["Basis vectors of axes:"]; Print[" "];
    Print["     ", coordmatrix];
    Print[" "];
    Print["Translations in the above:"]; Print[" "];
    Print["     ", origin]
  ];
          (* Assemble quadric ID data-structure: *)
  quadricid = type[canon, axialvars, coordmatrix, origin];
          (* Determine output:  graphics, data, symbol *)
  {ID, plot} = {QuadricID, QuadricPlot} /. {QSopts} /.
    Options[PlotQuadricSurface];

  nquadricid = N[quadricid];
  QuadricID[] = If[ID, quadricid, type];
  If[plot, 
    axeslabels = Map[StringInsert["  ", #, 2]&,
      ToString /@ inputvars];
    plotopts = Sequence @@ Append[{plotopts},
      AxesLabel->axeslabels];
    QuadricPlot[nquadricid, plotopts],
    (* else: *)
    QuadricID[]
  ]  (* End output decisions *)
]                  (* End QS main Module *)




NPlotQuadricSurface[inputpoly_,rest___] :=
  PlotQuadricSurface[Expand[1.*inputpoly ], rest]

   (* QuadricPlot, the commanding program of the plotting subroutines: *)


QuadricPlot[quadricID:type_[canon_, axialvars_, coordmatrix_,origin_], plotopts___] :=
Block[{pos, axis, axes, anyvar = Alternatives @@ axialvars, rgb,
        primssetting, primsStyle, styledaxes, drawaxes},
  If[type === EmptySet || type === Universe,
    Message[QSCS::noplot, type]; Return[quadricID] ];
  pos[v:anyvar] := Position[axialvars, v][[1, 1]];
  pos[vars:{anyvar..}] := Map[pos, vars];
  axis[v:anyvar] := Part[coordmatrix, pos[v] ];
  axes[vars:{anyvar..}] := Map[axis, vars];
  rgb[v:anyvar] := RGBColor @@ Part[IdentityMatrix[3], pos[v]];
  rgb[vars:{anyvar..}] := Map[rgb, vars];
  primssetting = PrimitivesStyle /. {plotopts} /. Options[PlotQuadricSurface];
  primssetting = Flatten[{primssetting}];

  If[!MatchQ[primssetting, PrimitivesStyle | {}],
    primsStyle = Join[{Thickness[1/80], PointSize[0.04]}, primssetting],
    primsStyle = {Thickness[1/80], PointSize[0.04]}
  ];
  drawaxes = DrawAxes /. {plotopts} /. Options[PlotQuadricSurface];
  If[!drawaxes, styledaxes = {} ];
  GraphQuadric[quadricID, plotopts]
]

                (* The plotting functions, by quadric type *)


CoordinatePlane1 /:
GraphQuadric[
		CoordinatePlane1[{u_, {v_, w_}}, axialvars_, coordmatrix_,origin_], plotopts___] :=
	Module[{planedirs, center = origin . coordmatrix, newaxes, pic, styledaxes, opacity, relendpts},
	planedirs = axes[{v, w}];
	opacity = PlotStyle /. {plotopts} /. PlotStyle->{};
	pic = Graphics3D[{opacity,Join[primsStyle,{Parallelogram3D[planedirs, center]}]}];
	If[drawaxes,
		relendpts = Map[{-axis[#], +axis[#]} &, {u, v, w}];
		newaxes =  Map[(center + #)&, relendpts, {2}];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
	Show[pic,Graphics3D[styledaxes],
		Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All ]
]


VerticalPlane /:
GraphQuadric[
		VerticalPlane[{{u_, a_}, {v_, b_}, w_}, axialvars_,coordmatrix_, origin_], 
	plotopts___] :=
	Module[{planedirs,linedir,vertdir,center = origin .coordmatrix,relendpts,
		newaxes, pic, styledaxes,opacity},
	planedirs = axes[{u, v}];
	linedir = Normalize[{-b, a} . planedirs];
	vertdir = axis[w];
	opacity = PlotStyle /. {plotopts} /. PlotStyle->{};
	pic = Graphics3D[{opacity,Join[primsStyle,{Parallelogram3D[{linedir, vertdir}, center]}]}];
	If[drawaxes,
		relendpts = Map[{-axis[#], +axis[#]} &, {u, v, w}];
		newaxes = Map[(center + #)&, relendpts, {2}];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
	Show[pic,Graphics3D[styledaxes],
		Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All]
]

ObliquePlane /:
GraphQuadric[
		ObliquePlane[{{u_, a_}, {v_, b_}, {w_, c_}}, axialvars_,coordmatrix_, origin_], 
	plotopts___] :=
	Module[{planedirs,center=origin.coordmatrix,relendpts,newaxes,
		pic, styledaxes, opacity},
	planedirs = Map[Normalize, {{-b, a} . axes[{u, v}],{-c, a} . axes[{u, w}]} ];
	opacity = PlotStyle /. {plotopts} /. PlotStyle->{};
	pic = Graphics3D[{opacity,Join[primsStyle,{Parallelogram3D[planedirs, center]}]}];
	If[drawaxes,
		relendpts = Map[{-axis[#], +axis[#]} &, {u, v, w}];
		newaxes = Map[(center + #)&, relendpts, {2}];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
	Show[pic,Graphics3D[styledaxes],
    	Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All]
]

CoordinatePlane2 /:
GraphQuadric[
		CoordinatePlane2[{u_, {v_, w_}}, axialvars_, coordmatrix_,origin_], 
	plotopts___] :=
	Module[{planedirs,center = origin . coordmatrix,relendpts, newaxes, pic, styledaxes, opacity},
	planedirs = axes[{v, w}];
	opacity = PlotStyle /. {plotopts} /. PlotStyle->{};
	pic = Graphics3D[{opacity,Join[primsStyle,{Parallelogram3D[planedirs, center]}]}];
	If[drawaxes,
		relendpts = Map[{-axis[#], +axis[#]} &, {u, v, w} ];
		newaxes = Map[(center + #)&, relendpts, {2}];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
	Show[pic,Graphics3D[styledaxes], 
		Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All]
]


ParallelPlanes /:
GraphQuadric[
		ParallelPlanes[{{u_, d_}, {v_, w_}}, axialvars_,coordmatrix_, origin_], plotopts___] :=
	Module[{planedirs,normaldir,center1,center2,center = origin . coordmatrix,
		newaxes, pic, styledaxes, opacity},
	planedirs = axes[{v, w}];
	normaldir = axis[u];
	center1 = center - d*normaldir;
	center2 = center + d*normaldir;
	opacity = PlotStyle /. {plotopts} /. PlotStyle->{};
	pic = Graphics3D[{opacity,Join[primsStyle,{Parallelogram3D[(3d)planedirs, center1],
                        Parallelogram3D[(3d)planedirs, center2]}]}];

	If[drawaxes, 
		newaxes = MapThread[{center - d #1 axis[#2],center + d #1 axis[#2]} &, {{5/4, 3, 3}, {u, v, w}}];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
	Show[pic,Graphics3D[styledaxes],
		FilterRules[{plotopts},Options[Graphics3D]], Axes->Automatic,PlotRange->All]
]


ParabolicCylinder /:
GraphQuadric[
		ParabolicCylinder[{v_, {u_, m_}, w_}, axialvars_,coordmatrix_, origin_], 
	plotopts___] :=
	Module[{parabcyl, r, k, h, uu, ww,msign, vendpts, vline, axisv,
		uwendpts, uwlines,newaxes, center, pic, styledaxes},

	parabcyl[uu_, ww_] := (m uu^2) axis[v] + ww axis[w] +
		uu axis[u] + origin . coordmatrix;
	r = Which [Abs[N @ m] >= 1, 1, Abs[N @ m] < 1, Sqrt @ Abs[1/m] ];
	k = 1; h = k (r + Abs[m] r^2)/2;
	pic = ParametricPlot3D[Evaluate @ parabcyl[uu, ww], {uu, -r, r}, {ww, -h, h}, 
			Sequence@@FilterRules[{plotopts},Options[ParametricPlot3D]]//Evaluate];

	If[drawaxes, 
		msign = Sign @ N[m];
		vendpts = center + Abs[m] r^2 Sort[msign {-1/4, 5/4}] axisv /.
			{center -> origin . coordmatrix, axisv -> axis[v]} ;
		vline = vendpts;
		uwendpts = center + 5/4 {-1, +1} #1 axis[#2];
		uwlines = MapThread[Evaluate @ uwendpts & /. center -> origin . coordmatrix, { {r, h}, {u, w} } ];
		newaxes = {vline,Sequence @@ uwlines};
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
	Show[
		Graphics3D[styledaxes],pic,
			Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All]
]

ObliqueParabolicCylinder /:
GraphQuadric[
  ObliqueParabolicCylinder[{{u_, a_}, {v_, b_}, {w_, c_}},
  axialvars_, coordmatrix_, origin_], plotopts___] :=
Module[{m, n, depvar, oblcyl, k, r, h, uu, vv, msign, order,
	w0, w1, axesranges, center, endpts, newaxes, pic, styledaxes},

  {m, n} = -{a, b} / c;
  depvar[uu_, vv_] := m uu^2 + n vv;
  oblcyl[uu_, vv_] := depvar[uu, vv] axis[w] + vv axis[v] +
    uu axis[u] + origin . coordmatrix;
  k = 1;  (* Multiplier on average of indep. and dep. *)
          (* variable ranges for parabola, to yield    *)
          (* range of cylindrical axis variable.      *)
  {r, h} = Which[
    Abs[N @ m] >= 1, {1, k (Abs[m] + 1)/2},
    Abs[N @ m] < 1, {Sqrt @ Abs[1/m], k (Sqrt @ Abs[1/m] + 1)/2}  ];
  pic = ParametricPlot3D[Evaluate @ oblcyl[uu, vv], {uu, -r, r}, {vv, -h, h}, 
			Evaluate @ FilterRules[{plotopts},Options[ParametricPlot3D]]];

  If[drawaxes, 
		msign = Sign @ N[m];
		order = Switch[msign, +1, Identity, -1, Reverse];
		{w0, w1} = order @ {-msign Abs[n] h, m r^2 + msign Abs[n] h};
		axesranges = Transpose[{{-r, r}, {-h, h}, {w0, w1}}];
		endpts = center + {#1, #2} axis[#3];
		newaxes = MapThread[Evaluate @ endpts & /. center-> origin . coordmatrix, Join[axesranges, {{u, v, w}}] ];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
  Show[
    Graphics3D[styledaxes],pic,
    Sequence@@FilterRules[{plotopts}, Options[Graphics3D]],Axes->Automatic, PlotRange->All
  ]
]

CoordinateAxis /:
GraphQuadric[
  CoordinateAxis[{{u_, a_}, {v_, b_}, w_}, axialvars_,
  coordmatrix_, origin_], plotopts___] :=
Module[{endpts,relendpts, newaxes,pic, styledaxes},
  endpts = Map[(origin . coordmatrix + # axis[w])&, {-1, 1}];
  pic = Graphics3D[Join[primsStyle,{Line[endpts]}]];
  If[drawaxes, 
		relendpts = Map[{-axis[#], +axis[#]}&, {u, v, w}];
		newaxes =  Map[(origin . coordmatrix + #)&, relendpts, {2}];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
  Show[Graphics3D[styledaxes],pic,
    Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All
  ]]


IntersectingPlanes /:
GraphQuadric[
  IntersectingPlanes[{{u_, a_}, {v_, b_}, w_}, axialvars_, coordmatrix_, origin_], plotopts___] :=
Module[{horiz1, horiz2, vert, center,relendpts, newaxes, pic, styledaxes, opacity},
  vert = axis[w];
  center = origin . coordmatrix;
  {horiz1, horiz2} = Map[Normalize,{{-a, b}, {a, b}} . axes[{u, v}] ];
  opacity = PlotStyle /. {plotopts} /. PlotStyle->{};
  pic = Graphics3D[{opacity,Join[primsStyle, {Parallelogram3D[{horiz1, vert}, center],Parallelogram3D[{horiz2, vert}, center]} ]}];
  If[drawaxes, 
		relendpts = Map[{-axis[#], +axis[#]}&, {u, v, w}];
		newaxes = Map[(center + #)&, relendpts, {2}];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
  Show[pic,Graphics3D[styledaxes],
    Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All
  ]
]


EllipticCylinder /:
GraphQuadric[
  EllipticCylinder[{{u_, a_}, {v_, b_}, w_}, axialvars_, coordmatrix_, origin_], plotopts___] :=
Module[{ellipse, h, ellcyl, th, ff, endpts, center, newaxes, pic, styledaxes},
  ellipse[th_] :=  ( {a, b}*{Cos[th], Sin[th]} ) . axes[{u, v}];
  ellcyl[ff_, th_] := ff axis[w] + ellipse[th] + origin . coordmatrix;
  k = 1; h = k (a + b)/2;
    	(* Cylindrical axis length is k times *)
    	(* average of semi-axis lengths.      *)
	pic = ParametricPlot3D[Evaluate @ ellcyl[ff, th], {th, 0, 2Pi}, {ff, -h, h}, 
			FilterRules[{plotopts},Options[ParametricPlot3D]]//Evaluate];

  If[drawaxes, 
		endpts = center + {-1, +1} #1 axis[#2];
		newaxes = MapThread[Evaluate @ endpts & /. center->origin . coordmatrix, { {a+1/3, b+1/3, h+1/2}, {u, v, w} }];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
  Show[
    Graphics3D[styledaxes],pic, 
    Sequence@@FilterRules[{plotopts}, Options[Graphics3D]],Axes->Automatic,PlotRange->All
  ]
]


HyperbolicCylinder /:
GraphQuadric[
  HyperbolicCylinder[{{u_, a_}, {v_, b_}, w_}, axialvars_,
  coordmatrix_, origin_], plotopts___] :=
Module[{t, h1, h2, hypcyl1, hypcyl2, m, r, n, h, ff,center, endpts, newaxes, pic, styledaxes},
  h1[t_] := ( {a, b}*{Cosh[t], Sinh[t]} ) . axes[{u, v}];
  h2[t_] := ( {a, b}*{-Cosh[t], Sinh[t]} ) . axes[{u, v}];
  hypcyl1[ff_, t_] := ff axis[w] + h1[t] + origin . coordmatrix;
  hypcyl2[ff_, t_] := ff axis[w] + h2[t] + origin . coordmatrix;
  m = 3; r = ArcCosh[m];  (* the domain of the hyp parameter *)
  n = 1;
  h = n (m a + Sqrt[m^2 - 1] b)/2;
    (* cylindrical axis range of n times the  *)
    (* average of the rectangular sides.      *)
  pic =  ParametricPlot3D[Evaluate[{hypcyl1[ff, t], hypcyl2[ff, t]}],  {t, -r, r}, {ff, -h, h}, 
			Evaluate @  FilterRules[{plotopts},Options[ParametricPlot3D]]];
  If[drawaxes, 
			endpts = center + {-1, +1} #1 #2 axis[#3];
			newaxes = MapThread[Evaluate @ endpts & /. center -> origin . coordmatrix,{ {1, 1, 5/4}, {m a, Sqrt[m^2 - 1] b, h}, {u, v, w} } ];
			styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
  Show[
    Graphics3D[styledaxes],pic,
    Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All
  ]
]


EllipticParaboloid /:
GraphQuadric[
  EllipticParaboloid[{{u_, a_}, {v_, b_}, {w_, s_}}, axialvars_,
  coordmatrix_, origin_], plotopts___] :=
Module[{ellipse, th, ellparab, w0, w1,winterval, wline, axisw,
        center, uvlines, newaxes, pic, styledaxes},
  ellipse[ww_, th_] := (Sqrt[s ww]*{a, b}*{Cos[th], Sin[th]}) .
    axes[{u, v}];
  ellparab[ww_, th_] := ww axis[w] + ellipse[ww, th] +
    origin . coordmatrix;
  {w0, w1} = Sort[s*{0, 1}];
  pic = ParametricPlot3D[Evaluate @ ellparab[ww, th], {th, 0, 2Pi},
      {ww, w0, w1}, Sequence@@FilterRules[{plotopts}, Options[ParametricPlot3D]]//Evaluate];
  If[drawaxes, 
			winterval = Sort[s*{-1/4, 5/4}];
			wline = center + winterval * axisw /.
			{axisw->axis[w], center-> origin . coordmatrix};
			uvendpts = center + {-1, +1} #1 axis[#2];
			uvlines = MapThread[Evaluate @ uvendpts & /. center-> origin . coordmatrix, { {a, b}, {u, v} }];
			newaxes = uvlines ~Join~ {wline};
			styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
  Show[
    Graphics3D[styledaxes],pic,
    Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All
  ]
]

HyperbolicParaboloid /:
GraphQuadric[
  HyperbolicParaboloid[{{u_, a_}, {v_, b_}, w_}, axialvars_,
  coordmatrix_, origin_], plotopts___] :=
Module[{depvar, hypparab,endpts, center, newaxes, pic, styledaxes},
  depvar[uu_, vv_] := uu^2/a^2 - vv^2/b^2;
  hypparab[uu_, vv_] := {uu, vv} . axes[{u, v}] +
    depvar[uu, vv] axis[w] + origin . coordmatrix;
  pic =  ParametricPlot3D[Evaluate @ hypparab[uu, vv],
      {uu, -2 a, 2 a}, {vv, -2 b, 2 b}, 
      Sequence@@FilterRules[{plotopts}, Options[ParametricPlot3D]]//Evaluate];

  If[drawaxes, 
		endpts = center + {-1, +1} #1 axis[#2];
		newaxes = MapThread[Evaluate @ endpts & /. center-> origin . coordmatrix, { {2 a, 2 b, 4}, {u, v, w} } ];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
  Show[
    Graphics3D[styledaxes],pic,
    Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All
  ]
]

Origin /:
GraphQuadric[
  Origin[{{u_, a_}, {v_, b_}, {w_, c_}}, axialvars_,
  coordmatrix_, origin_], plotopts___] :=
Module[{newaxes,styledaxes, pic, relendpts},
  pic = Graphics3D[Join[primsStyle, {Point[origin . coordmatrix]}]];
  If[drawaxes, 
		relendpts = Map[{-axis[#], +axis[#]}&, {u, v, w}];
		newaxes = Map[(origin . coordmatrix + #)&,relendpts, {2}];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
  Show[pic,Graphics3D[styledaxes],
    Sequence@@FilterRules[{plotopts}, Options[Graphics3D]],Axes->Automatic, PlotRange->All
  ]]


EllipticCone /:
GraphQuadric[
  EllipticCone[{{u_, a_}, {v_, b_}, w_}, axialvars_, coordmatrix_,
  origin_], plotopts___] :=
Module[{ellipse, ellcone, ww,th,endpts, center, newaxes, pic, styledaxes}, 

  ellipse[ww_, th_] := (ww*{a, b}*{Cos[th], Sin[th]}) . axes[{u, v}];
  ellcone[ww_, th_] := ww axis[w] + ellipse[ww, th] +
    origin . coordmatrix;
  pic =  ParametricPlot3D[Evaluate @ ellcone[ww, th], {th, 0, 2Pi}, {ww, -1, 1}, 
			Evaluate @ FilterRules[{plotopts},Options[ParametricPlot3D]]];
  If[drawaxes, 
		endpts = center + {-1, +1} #1 #2 axis[#3];
		newaxes = MapThread[Evaluate @ endpts & /. center-> origin . coordmatrix, { {1, 1, 5/4}, {a, b, 1}, {u, v, w} }];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
  Show[
    Graphics3D[styledaxes],pic,
    Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All
  ]
]

Ellipsoid1 /:
GraphQuadric[
  Ellipsoid1[{{u_, a_}, {v_, b_}, {w_, c_}}, axialvars_,
  coordmatrix_, origin_], plotopts___] :=
Module[{ax, bx, th, ellipse, ellipsoid, ww,endpts, center, newaxes, pic, styledaxes},
  ax[ww_] := a Sqrt[1 - ww^2/c^2];
  bx[ww_] := b Sqrt[1 - ww^2/c^2];
  ellipse[ww_, th_] :=
    ({ax[ww], bx[ww]}*{Cos[th], Sin[th]}) . axes[{u, v}];
  ellipsoid[ww_, th_] := ww axis[w] + ellipse[ww, th] + origin . coordmatrix;
  pic = ParametricPlot3D[Evaluate @ ellipsoid[ww, th], {th, 0, 2Pi},{ww, -c, c},  
  	Sequence@@FilterRules[{plotopts}, Options[ParametricPlot3D]]//Evaluate];
  If[drawaxes, 
		endpts = center + {-1, +1} #1 axis[#2];
		newaxes = MapThread[Evaluate @ endpts & /. center-> origin . coordmatrix, { {a + 1/4, b + 1/4, c + 1/4}, {u, v, w} }];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
  Show[
    Graphics3D[styledaxes],pic,
    Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All
  ]
]

hyperboloid1 /:
GraphQuadric[
  hyperboloid1[{{u_, a_}, {v_, b_}, {w_, c_}}, axialvars_,
  coordmatrix_, origin_], plotopts___] :=
Module[{ax, bx, ellipse, hypsheet, th, ww,endpts, newaxes,center, pic, styledaxes},
  ax[ww_] := a Sqrt[ww^2/c^2 + 1];
  bx[ww_] := b Sqrt[ww^2/c^2 + 1];
  ellipse[ww_, th_] :=
    ({ax[ww], bx[ww]}*{Cos[th], Sin[th]}) . axes[{u, v}];
  hypsheet[ww_, th_] := origin . coordmatrix + ww axis[w] + ellipse[ww, th];
  pic = ParametricPlot3D[Evaluate @ hypsheet[ww, th],
      {th, 0, 2Pi}, {ww, -Sqrt[3]c, Sqrt[3]c}, 
       Sequence@@FilterRules[{plotopts}, Options[ParametricPlot3D]]//Evaluate];
  If[drawaxes, 
		endpts = center + {-1, +1} #1 #2 axis[#3];
		newaxes = MapThread[Evaluate @ endpts & /. center-> origin . coordmatrix, {{1, 1, 5/4}, {2 a, 2 b, Sqrt[3] c},{u, v, w}} ];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
  Show[
    pic,Graphics3D[styledaxes],
    Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All
  ]
]

hyperboloid2 /:
GraphQuadric[
  hyperboloid2[{{u_, a_}, {v_, b_}, {w_, c_}}, axialvars_,
    coordmatrix_, origin_], plotopts___] :=
Module[{ellipse, hypsheet1, hypsheet2, ww, th,endpts,newaxes, center, pic, styledaxes}, 
  ax[ww_] := a Sqrt[ww^2/c^2 - 1];
  bx[ww_] := b Sqrt[ww^2/c^2 - 1];
  ellipse[ww_, th_] :=
    (({ax[ww], bx[ww]}*{Cos[th], Sin[th]}) . axes[{u, v}]); 
  hypsheet1[ww_, th_] := (ellipse[ww, th] + ww axis[w] + origin . coordmatrix);
  hypsheet2[ww_, th_] := (ellipse[ww, th] - ww axis[w] + origin . coordmatrix);

  pic = ParametricPlot3D[Evaluate[
  		{Re[hypsheet1[ww, th]], Re[hypsheet2[ww, th]]}], {th, 0, 2 Pi},{ww,c,Sqrt[5] c},  
			Sequence@@FilterRules[{plotopts}, Options[ParametricPlot3D]]//Evaluate, PlotPoints->{15,9}]; 
  If[drawaxes, 
		endpts = center + {-1, +1} #1 axis[#2];
		newaxes = MapThread[Evaluate @ endpts & /. center-> origin . coordmatrix, {{2 a, 2 b, Sqrt[5] c}, {u, v, w}} ];
		styledaxes = setQSaxes1[newaxes,rgb[{u,v,w}]],styledaxes = {}];
  Show[
  	pic,Graphics3D[styledaxes],
	Sequence@@FilterRules[{plotopts}, Options[Graphics3D]], Axes->Automatic,PlotRange->All
  ]
]

(*******************************************************************)
(********************BEGIN CONIC SECTION PORTION********************)
(*******************************************************************)

ConicOpts = First /@ Options[PlotConicSection];
CSGraphOpts = Join[Union @@ Map[First, Options /@ {ParametricPlot,Graphics}, {2}],{PrimitivesStyle}];

CSValidOpts = Union[ConicOpts, CSGraphOpts];

(* This is the default style for the types CoordinateAxis1,CoordinateAxis2,ObliqueLine,
   Origin, ParrallelLines, and IntersectingLines. It is held here because if held
   in the Option[PlotConicSection], it would also affect the parabolas, ellipses, 
   and hyperbolas, which we do not want. Any user specified PlotStyle will have
   preference over these primitives *)


PlotConicSection[inputpoly_, inputvars_, opts:(_Rule | _RuleDelayed)...] :=
Module[{newvars = Map[Unique[ToString[#]]&,inputvars]},
  PlotConicSection[inputpoly,inputvars,newvars,opts]]



PlotConicSection[inputpoly_, inputvars_, newvars_, opts:(_Rule | _RuleDelayed)...] :=
Module[{inputpolyN = Expand[1.*inputpoly //N], CSopts, plotopts,display,
				origvars, rotateQ, printQ, CSboolopts, invalidbool,
				axialvars, poly, coordmatrix,
				quadvars, linvars, freevars, q, l,
				quadcoeffs, lincoeffs, quadsigns, linsigns, completedsqrs,
				rsimp, translateform, mindistpt, lincoeffs1ToStr,
				standardlin, d, dsign, origin,
				params, a, b, m, coeffsorder, varsorder, ID, plot,nconicid,conicid,axesopt,printform, rotstr,
				translstr, type, axeslabels},
	If[!FreeQ[inputpolyN, Complex], Return @
		Message[QSCS::complexinput, inputpoly] ];
	If[Not[MatchQ[inputvars, vars:{_, _} /; UnsameQ @@ vars]],
    	Return @ Message[QSCS::numbervars, "original",inputvars] ];
If[Not[Length[newvars]==Length[inputvars]], Return@Message[QSCS::varlists, inputvars, newvars]];


	origvars = Variables[inputpolyN];
	If[!PolynomialQ[inputpolyN, inputvars], Return @
		Message[QSCS::notpoly, inputpoly /. quotes,origvars /. quotes] ];
	If[Not[SubsetQ[inputvars, origvars]], Return @
		Message[QSCS::polyvars, inputpoly /. quotes,inputvars /. quotes, origvars /. quotes] ];
	If[PolynomialDegree[inputpolyN, inputvars] > 2, Return @
		Message[QSCS::polydegree, inputpoly /. quotes,origvars /. quotes] ];
(*	If[!MatchQ[newvars, vars:{_, _} /; UnsameQ @@ vars],
				Return @ Message[QSCS::varslist, "new",newvars] ];*)
	With[{commonvars = Intersection[inputvars, newvars]},
		If[commonvars =!= {}, Return @ Message[QSCS::varsconflict, commonvars] ]];

				(* Separate out and check PlotConicSection options: *)
	With[{optnames = First /@ {opts} },
		If[Not[SubsetQ[CSValidOpts, optnames]], Return @
			Message[QSCS::invalidopts, Complement[optnames,
				CSValidOpts]] ]  ];
	CSopts = Sequence @@ Select[{opts}, MemberQ[ConicOpts,
		First[#] ]& ];
	CSboolopts = Thread @ (Rule[#, # /. {CSopts} /.
		Options[PlotConicSection]]&) @ {ConicID, ConicPlot, PrintDisplay,DrawAxes};
	invalidbool = Select[CSboolopts, !BooleanQ[Last[#]] & ];
	If[invalidbool =!= {}, Return @ Message[QSCS::invalidbool,
		invalidbool] ];
				(* Separate out graphics options to be passed: *)
	plotopts = Sequence @@ Select[{opts}, MemberQ[CSValidOpts,First[#] ]& ];

display = PrintDisplay /. {opts} /. Options[PlotConicSection];
	printQ = If[PrintDisplay /. {opts} /. Options[PlotConicSection],CompoundExpression, Hold];

	rotateQ = mixedQ[inputpolyN, inputvars];
	{poly, coordmatrix} = Chop @ If[!rotateQ,
		axialvars = inputvars; {Simplify[inputpoly], IdentityMatrix[2]},
		DiagQuad[inputpoly,inputvars, axialvars = newvars][[{1,3}]]//Simplify];

						(* Begin analyzing quadratic form *)

	quadvars = Select[axialvars, (Exponent[poly, #] == 2)& ];
	linvars = Select[Complement[axialvars, quadvars],(Exponent[poly, #] == 1)& ];
	freevars = Complement[axialvars, Union[quadvars, linvars] ];
	{q, l} = Length /@ {quadvars, linvars};
	quadcoeffs = Simplify @ Map[Coefficient[poly, #, 2]&, quadvars];
	lincoeffs = Simplify @ Map[Coefficient[poly, #, 1]&, linvars];
	quadsigns = Sign @ Chop[N @ quadcoeffs];
	linsigns = Sign @ Chop[N @ lincoeffs];
	completedsqrs = CompleteSquare[poly, axialvars]//Chop;


	rsimp = If[AtomQ[completedsqrs],
		0,
		Select[completedsqrs,FreeQ[#,Alternatives[Sequence @@ axialvars]]& ]];

	translateform = Chop @ If[l == 0, completedsqrs,
		(* linear variables absorb constant term as translations: *)
			mindistpt = Simplify[-rsimp/Norm[lincoeffs(*//N*)]^2 lincoeffs];
			lincoeffs1ToStr =
				Map[Replace[#, {1 -> "1", -1 -> -"1"}]&, lincoeffs];
					(* Coefficients of +-1 will not hold variable and	*)
					(* translation constant together in parentheses,	*)
					(* like (z - 4), but strings cannot be discarded.	*)
			standardlin = lincoeffs1ToStr . (linvars - mindistpt);
			completedsqrs - rsimp - lincoeffs.linvars + standardlin
	];

	
	d = If[l == 0, rsimp, 0];
	dsign = Sign @ N[d];
	origin = translation[translateform, axialvars];

	rotstr = If[rotateQ, ", after rotation,"  ,  ""];
	translstr = If[l == 0, ":",
		" and translations for linear variables proportionate to coefficients for constant of 0:"];
	printQ[
		Print[" "],
		Print @ StringJoin["Simplified equation", rotstr," with squares completed", translstr]
	];
(*	printform = If[l == 0, translateform,
		(* Else, control display of linear terms: *)
		anylin = Alternatives @@ linvars;
		unitsrules =
			{"1" ((var:anylin) + h_) :> SequenceForm["(", var + h, ")"],
			 ("1" var:anylin) :> var };
		translateform /. unitsrules  ];
*)
	printform = translateform;	
	printQ[
		Print[" "],
		Print[printform, "  =  ", 0]
	];
	

							(* CLASSIFICATION OF CONIC SECTION *)
	Which[
		q == 0,
			Which[l == 0, If[dsign == 0, type = Universe, type = EmptySet],
						l == 1, type = CoordinateAxis1,
						l == 2, type = ObliqueLine
					 ],
		q == 1,
			Which[l == 0, Which[dsign == 0, type = CoordinateAxis2,
													dsign == quadsigns[[1]], type = EmptySet,
													dsign == -quadsigns[[1]], type =
														ParallelLines
												 ],
						l == 1, type = Parabola
					 ],
		q == 2,
			Which[dsign == 0, Which[Equal @@ quadsigns, type = Origin,
													Unequal @@ quadsigns, type =
														IntersectingLines
												 ],
						dsign != 0, Which[Unequal @@ quadsigns, type = Hyperbola,
													Equal @@ quadsigns,
														If[dsign == quadsigns[[1]],
															type = EmptySet,
															type = Ellipse
															]
												 ]
					 ]
			 ]; (* end Classification Which[] *)
						(* DISPLAY CANONICAL EQUATION AND CONSTRUCT *)
						(* ITS REPRESENTATION FOR CONIC ID DATA			*)
	printQ @ PrintPhrase[ConicPhrase,type];
	Which[
	type === Universe,
		printQ @ Print["0 = 0.  All variables are free."];
		params = {},
	type === EmptySet,
		Which[
		q == 0,
			printQ @ printeqn[d, 0];
			params = {d},
		q == 1,
			d = -d/quadcoeffs[[1]] //Simplify;
			printQ @ printeqn[quadvars[[1]]^2, d];
			params = {{quadvars[[1]], d}, freevars},
		q == 2,
			{a, b} = 1 / Sqrt[quadcoeffs/d] //Simplify;
			printQ @ printeqn[sqrform @ Transpose[{quadvars, {a, b}}], -1];
			params = Transpose[{quadvars, {a, b}}] ~Join~ freevars
		],
	type === CoordinateAxis1,
		printQ @ printeqn[linvars[[1]], 0, freevars];
		params = Join[linvars, {freevars}],
	type === ObliqueLine,
		printQ @ printeqn[lincoeffs . linvars, 0];
		params = Transpose[{linvars, lincoeffs}],
	type === CoordinateAxis2,
		printQ @ printeqn[quadvars[[1]]^2, 0];
		params = Join[quadvars, {freevars}],
	type === ParallelLines,
		d = Sqrt[-d/quadcoeffs[[1]] ] //Simplify;
		printQ @ printeqn[quadvars[[1]]^2, d^2, freevars];
		params = {{quadvars[[1]], d}, freevars},
	type === Parabola,
		m = Simplify @@ (-quadcoeffs/lincoeffs);
		printQ @ printeqn[linvars[[1]], m quadvars[[1]]^2 ];
		params = linvars ~Join~ {Append[quadvars, m]},
	type === Origin,
		{a, b} = Sqrt @ Abs[1/quadcoeffs] //Simplify;
		printQ @ printeqn[sqrform @ Transpose[{quadvars, {a, b}}], 0];
		params = Transpose[{quadvars, {a, b}}],
	type === IntersectingLines,
		{a, b} = Sqrt @ Abs[1/quadcoeffs] //Simplify;
		printQ @ printeqn @@ sqrform[ Transpose[{quadvars, {a, b}}] ];
		params = Transpose[{quadvars, {a, b}}],
	type === Ellipse,
		{a, b} = Simplify @ Sqrt[-d/quadcoeffs];
		printQ @ printeqn[sqrform @ Transpose[{quadvars, {a, b}}], 1];
		params = Transpose[{quadvars, {a, b}}],
	type === Hyperbola,
		{coeffsorder, varsorder} = Transpose @
			Sort[ Transpose[{-quadcoeffs/d, quadvars}],
				OrderedQ[N @ {#2, #1}]& ];
		{a, b} = Sqrt @ Abs[1 / coeffsorder] //Simplify;
		printQ @ printeqn[sqrform[ Transpose[{varsorder, {a, b}}], -1 ], 1];
		params = Transpose[{varsorder, {a, b}}]
	]; 

 If[display === True,
    Print[" "];
    Print["Basis vectors of axes:"]; Print[" "];
    Print["     ", coordmatrix];
    Print[" "];
    Print["Translations in the above:"]; Print[" "];
    Print["     ", origin]
  ];


			(* end canonical-equation-representation Which[] *)
					(* Assemble ConicID[] data-structure *)
	conicid = type[params, axialvars, coordmatrix, origin];
					(* Determine output:  graphics, data, symbol *)
	{ID, plot} = {ConicID, ConicPlot} /. {CSopts} /.
		Options[PlotConicSection];
	ConicID[] = If[ID, conicid, type];
	nconicid = N[conicid];
	If[plot,axeslabels = Map[StringInsert["  ", #, 2]&,
			ToString /@ inputvars];
		plotopts = Sequence @@ Append[{plotopts}, AxesLabel->axeslabels];
		axesopt = DrawAxes /. CSboolopts /. Options[PlotConicSection];
		conicplot[nconicid, axesopt,plotopts],
		(* Else, no plot, just the symbol or data: *)
		ConicID[]
	]	 (* :End of output decision *)
] 			(* END PlotConicSection MAIN MODULE *)



NPlotConicSection[inputpoly_, rest___] :=
	PlotConicSection[Expand[1.*inputpoly //N], rest]

conicplot[conicID:type_[params_, axialvars_, coordmatrix_, origin_], axesopt_,plotopts___] :=
Block[{anyvar = Alternatives @@ axialvars, pos, axis, axes, rgb},
	If[type === EmptySet || type === Universe,
		Message[QSCS::noplot, type]; Return[conicID] ];
	pos[v:anyvar] := Position[axialvars, v][[1, 1]];
	pos[vars:{anyvar..}] := Map[pos, vars];
	axis[v:anyvar] := Part[coordmatrix, pos[v] ];
	axes[vars:{anyvar..}] := Map[axis, vars];
(*	rgb[v:anyvar] := Switch[pos[v],
		1, RGBColor[1, 0, 0],
		2, RGBColor[0, 0, 1]  ];*)
rgb[v:anyvar] := RGBColor @@ IdentityMatrix[3][[pos[v]]];
	rgb[vars:{anyvar..}] := Map[rgb, vars];
	drawaxes = axesopt;
        GraphConic[conicID, plotopts]
]

CoordinateAxis1 /:
GraphConic[CoordinateAxis1[{u_, {v_}}, axialvars_, coordmatrix_,
	origin_], plotopts___] :=
Module[{endpts, axesends, center, axisv, newaxes, styledaxes, pic, style},
	endpts = center + {-1, +1}*axisv /. {center -> origin . coordmatrix, axisv -> axis[v]};
    style = PrimitivesStyle /. {plotopts} /. Options[PlotConicSection];
	style = Flatten[{style}];
	pic = Graphics[{{Apply[Sequence,style],Line[endpts]} }];
	If[drawaxes,
		axesends = center + {-1, +1} axis[#];
		newaxes = Map[(Evaluate @ axesends)& /. center -> origin . coordmatrix, {u, v}];
		styledaxes = setCSaxes1[newaxes,rgb[{u,v}]],styledaxes = {}];
	Show[pic,Graphics[styledaxes],Sequence@@FilterRules[{plotopts}, Options[Graphics]], Axes->Automatic,
		AspectRatio->Automatic, PlotRange->All
	]
]

ObliqueLine /:
GraphConic[ObliqueLine[{{u_, a_}, {v_, b_}}, axialvars_,
	coordmatrix_, origin_], plotopts___] :=
Module[{linedir, endpts, axesends, center, newaxes, styledaxes, pic, style},
	linedir = Normalize[{-b, a}];
	endpts = Map[(origin . coordmatrix + # linedir)&, {-1, +1}];
	style = PrimitivesStyle /. {plotopts} /. Options[PlotConicSection];
	style = Flatten[{style}];
	pic = Graphics[
		{{Apply[Sequence,style],Line[endpts]} },
		Sequence@@FilterRules[{plotopts}, Options[Graphics]], Axes->Automatic,
		AspectRatio->Automatic, PlotRange->All];
	If[drawaxes,
		axesends = center + {-1, +1} axis[#];
		newaxes = Map[(Evaluate @ axesends)& /. center ->origin . coordmatrix, {u, v}];
		styledaxes = setCSaxes1[newaxes,rgb[{u,v}]],styledaxes = {}];
	Show[pic,Graphics[styledaxes]]
]

CoordinateAxis2 /:
GraphConic[CoordinateAxis2[{u_, {v_}}, axialvars_, coordmatrix_,
	origin_], plotopts___] :=
Module[{endpts, axesends, center, newaxes, styledaxes, pic, style},
	endpts = center + {-1, +1}*axisv /. {center -> origin . coordmatrix, axisv -> axis[v]};
	style = PrimitivesStyle /. {plotopts} /. Options[PlotConicSection];
	style = Flatten[{style}];
	pic = Graphics[{ {Apply[Sequence,style],Line[endpts]} }];
	If[drawaxes,
		axesends = center + {-1, +1} axis[#];
		newaxes = Map[(Evaluate @ axesends)& /. center -> origin . coordmatrix, {u, v}];
		styledaxes = setCSaxes1[newaxes,rgb[{u,v}]],styledaxes = {}];
	Show[pic,Graphics[styledaxes],Sequence@@FilterRules[{plotopts}, Options[Graphics]], Axes->Automatic,
		AspectRatio->Automatic, PlotRange->All
	]
]

ParallelLines /:
GraphConic[ParallelLines[{{u_, p_}, {v_}}, axialvars_,
	coordmatrix_, origin_], plotopts___] :=
Module[{lines, axesends, center, newaxes, styledaxes, pic, style},
	lines = Line /@ Map[origin . coordmatrix + # . axes[{u, v}] &,
	Outer[List, {-p, p}, {-2p, 2p}], {2}];
	style = PrimitivesStyle /. {plotopts} /. Options[PlotConicSection];
	style = Flatten[{style}];
	pic = Graphics[{{Apply[Sequence,style],lines} },
		Sequence@@FilterRules[{plotopts}, Options[Graphics]], Axes->Automatic,
		AspectRatio->Automatic, PlotRange->All
	];
	If[drawaxes,
		axesends = center + {-1, +1} 5/4 #1 axis[#2];
		newaxes = MapThread[(Evaluate @ axesends)& /. center -> origin . coordmatrix, {{p, 2p}, {u, v}}];
		styledaxes = setCSaxes1[newaxes,rgb[{u,v}]],styledaxes = {}];
	Show[pic,Graphics[styledaxes]]
]

Parabola /:
GraphConic[Parabola[{v_, {u_, m_}}, axialvars_, coordmatrix_,
	origin_], plotopts___] :=
Module[{r, uu, parab, uaxis, center, axisu, vaxis, axisv,
				styledaxes, pic, newaxes},
	r = With[{mAbs = Abs[N @ m]},
		Which[mAbs <= 1, Sqrt[1/mAbs], mAbs > 1, 1]  ];
	parab[uu_] := {uu, m uu^2} . axes[{u, v}] + origin . coordmatrix;
	pic = ParametricPlot[Evaluate @ parab[uu], {uu, -r, r},
			Sequence@@FilterRules[{plotopts}, Options[ParametricPlot]] //Evaluate,
			AspectRatio->Automatic];
	If[drawaxes,
		uaxis = center + {-r, r} axisu /. {axisu -> axis[u],
		center -> origin . coordmatrix};
		vaxis = center + Sort[m r^2 {-1/4, 1}] axisv /.{axisv -> axis[v], center -> origin . coordmatrix};
		newaxes = {uaxis,vaxis};
		styledaxes = setCSaxes1[newaxes,rgb[{u,v}]],styledaxes = {}];
	Show[pic,
		Graphics[styledaxes],
		Sequence@@FilterRules[{plotopts}, Options[Graphics]], PlotRange->All
	]
]

Origin /:
GraphConic[Origin[{{u_, a_}, {v_, b_}}, axialvars_, coordmatrix_,
	origin_], plotopts___] :=
Module[{axesends, newaxes, styledaxes, pic, style},
	style = PrimitivesStyle /. {plotopts} /. Options[PlotConicSection];
	style = Flatten[{style}];
	pic = Graphics[{ {Apply[Sequence,style], Point[origin . coordmatrix]}},
		Sequence@@FilterRules[{plotopts}, Options[Graphics]], Axes->Automatic,
		AspectRatio->Automatic, PlotRange->All];
	If[drawaxes,
		axesends = center + {-1, +1} axis[#];
		newaxes = Map[(Evaluate @ axesends)& /. center -> origin . coordmatrix, {u, v}];
		styledaxes = setCSaxes1[newaxes,rgb[{u,v}]],styledaxes = {}];
	Show[pic,Graphics[styledaxes]]
]

IntersectingLines /:
GraphConic[IntersectingLines[{{u_, a_}, {v_, b_}}, axialvars_,
	coordmatrix_, origin_], plotopts___] :=
Module[{dir1, dir2, endpts, center, lines, axesends, newaxes,
				styledaxes, pic, style},
	{dir1, dir2} = Normalize /@ { {-a, b}, {-a, -b} };
	endpts = center + # {-1, +1};
	lines = Map[Evaluate @ Line[endpts] & /. center -> origin. coordmatrix, {dir1, dir2}];
    style = PrimitivesStyle /. {plotopts} /. Options[PlotConicSection];
	style = Flatten[{style}];
	pic = Graphics[{ {Apply[Sequence,style],lines} },
		Sequence@@FilterRules[{plotopts}, Options[Graphics]], Axes->Automatic,
		AspectRatio->Automatic, PlotRange->All];
	If[drawaxes,
		axesends = center + {-1, +1} axis[#];
		newaxes = Map[(Evaluate @ axesends)& /. center -> origin . coordmatrix, {u, v}];
		styledaxes = setCSaxes1[newaxes,rgb[{u,v}]],styledaxes = {}];
	Show[pic,Graphics[styledaxes]]
]



Ellipse /:
GraphConic[Ellipse[{{u_, a_}, {v_, b_}}, axialvars_, coordmatrix_,origin_], plotopts___] :=
Module[{th, ellipse, center, axesends, newaxes, styledaxes, pic},
	ellipse[th_] := ({a, b}*{Cos[th], Sin[th]}) . axes[{u, v}] + origin . coordmatrix;
	pic = ParametricPlot[Evaluate @ ellipse[th], {th, 0, 2Pi},
			Sequence@@FilterRules[{plotopts}, Options[ParametricPlot]]//Evaluate,
			AspectRatio->Automatic];
	If[drawaxes,
		axesends = center + {-1, 1}  #1 axis[#2];
		newaxes = MapThread[Evaluate @ axesends & /. center -> origin . coordmatrix, {{a+1/3, b+1/3}, {u, v}} ];
		styledaxes = setCSaxes1[newaxes,rgb[{u,v}]],styledaxes = {}];
	Show[pic, Graphics[styledaxes],
		Sequence@@FilterRules[{plotopts}, Options[Graphics]]//Evaluate, PlotRange->All
	]
]

Hyperbola /:
GraphConic[Hyperbola[{{u_, a_}, {v_, b_}}, axialvars_, coordmatrix_, origin_], plotopts___] :=
Module[{hyp1, hyp2, t, p, r, center, axesends, newaxes, styledaxes, pic},
	hyp1[t_] := ({a, b}*{Cosh[t], Sinh[t]}) . axes[{u, v}] + origin . coordmatrix;
	hyp2[t_] := ({a, b}*{-Cosh[t], Sinh[t]}) . axes[{u, v}] + origin . coordmatrix;
	p = 3; 
	r = ArcCosh[p];
	pic = ParametricPlot[Evaluate[{hyp1[t], hyp2[t]}], {t, -r, r},
			Sequence@@FilterRules[{plotopts}, Options[ParametricPlot]] //Evaluate,
			AspectRatio->Automatic];
	If[drawaxes,
		axesends = center + {-1, +1} #1 axis[#2];
		newaxes = MapThread[Evaluate @ axesends & /. center -> origin . coordmatrix, {{p a, b Sqrt[p^2 - 1]}, {u, v}} ];
		styledaxes = setCSaxes1[newaxes,rgb[{u,v}]],styledaxes = {}];
	Show[pic,Graphics[styledaxes],Sequence@@FilterRules[{plotopts}, Options[Graphics]],
	      PlotRange->All
	]
]



End[]

SetAttributes[{ConicPhrase, GraphConic, GraphQuadric, NPlotConicSection, NPlotQuadricSurface, 
  PlotConicSection, PlotQuadricSurface, QuadricPhrase, QuadricPlot, QuadricView},
  {ReadProtected, Protected, Locked}];



EndPackage[]
