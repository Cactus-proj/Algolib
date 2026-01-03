# Copyright (C) 1991--2010 by INRIA.
#
# This file is part of Algolib.
#
# Algolib is free software: you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# Algolib is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with Algolib.  If not, see
# <http://www.gnu.org/licenses/>.

###    -*-Maple-*-
###
###    Title: 	MultiSeries
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact email: 	Bruno.Salvy@inria.fr
###
### Description: This module manipulates multiseries expansions.
###   The main entry point for (advanced) users is the multiseries function.
###   Interface functions provided to users who are used to the current
###   behaviour of series/asympt/limit are
###
###		newseries	asympt	limit	taylor and LeadingTerm
###
###  In this version, the main functionality is there, but no attempt at
###  efficiency has been made.

$include <src/multiseries.mi>

MultiSeries:=module()

option package, 
       ALGOCOPYRIGHT,
       `Copyright (c) Maplesoft, a division of Waterloo Maple Inc. 2005`;

description
       "a package for manipulating multiseries expansions";

    export
        _pexports,
	init,
	multiseries,
	limit,
	asympt,
	series,
	taylor,
	LeadingTerm,
#	TypeForest,
	AddFunction,
	FunctionSupported,
	GetFunction,
	RemoveFunction,
	SeriesInfo,
	oldseries;

    global
	SERIES ,
	t_SERIES,
	`diff/SERIES`,
#	`diff/_var`,
#	`evalc/_var`,
	`print/SERIES` ,
	`print/_var`,
#	`signum/_var`,
#	`Re/_var`,
#	`Im/_var`,
	`type/SCALE`,
	`type/SERIES`,
	`type/ScaleVar`,
	`type/MultiSeries:-taylor`,
	`type/MultiSeries:-series`,
	`type/t_SERIES`,
	_var;

    local
	`multiseries/2polynom`,
	`multiseries/AddBigO` ,
	`multiseries/AddDoit` ,
	`multiseries/AddSeries` ,
	`multiseries/AddSeries/basic`,
	`multiseries/AddSeries/findbigo`,
	`multiseries/AddSeries/hardexponents`,
	`multiseries/CommonType`,
	`multiseries/CompareExponent` ,
	`multiseries/ComparisonFunction` ,
	`multiseries/ComposeClassical`,
	`multiseries/Convert2SERIES`,
	`multiseries/D`,
	`multiseries/D/recurse`,
	`multiseries/DominantTerm`,
	`multiseries/ExpExtend`,
	`multiseries/Expr4Series2Expr`,
	`multiseries/GAMMAaux`,
	`multiseries/InfinitePart` ,
	`multiseries/IsOnBranchCut`,
	`multiseries/LnExtend`,
	`multiseries/MulBranch`,
	`multiseries/MulClassical`,
	`multiseries/MulDoit`,
	`multiseries/MulKaratsuba`,
	`multiseries/MulSeriesCst`,
	`multiseries/MulSeriesCst/basic`,
	`multiseries/Psiaux`,
	`multiseries/RealImPart`,
	`multiseries/Series2BigO/cleanup`,
	`multiseries/Series2BigO` ,
	`multiseries/Series2Expr`,
	`multiseries/Series2sumofprod`,
	`multiseries/Testzero`,
	`multiseries/WhatType`,
	`multiseries/add` ,
	`multiseries/analytic`,
	`multiseries/analytic/recurse`,
	`multiseries/analytic/recurseonseries`,
	`multiseries/ancestors`,
	`multiseries/cast`,
	`multiseries/cleanremember`,
	`multiseries/compose/makebigo`,
	`multiseries/compose`,
	`multiseries/doit`,
	`multiseries/diffexprin_var`,
	`multiseries/eval`,
	`multiseries/fastest`,
	`multiseries/function`,
	`multiseries/function/arctanunivar`,
	`multiseries/function/AiryAiunivar`,
	`multiseries/function/AiryBiunivar`,
	`multiseries/function/EllipticEunivar`,
	`multiseries/function/EllipticF2ndarg`,
	`multiseries/function/erfcunivar`,
	`multiseries/function/LegendreP3arg`,
	`multiseries/function/LegendreQ3arg`,
	`multiseries/getoneterm`,
	`multiseries/homogenize`,
	`multiseries/ishiddenzero`,
	`multiseries/lastchancetestzero` ,
	`multiseries/leadterm` ,
	`multiseries/limit`,
	`multiseries/lnGAMMAaux`,
	`multiseries/locate` ,
	`multiseries/mul`,
	`multiseries/multiseries`,
	`multiseries/newseries`,
	`multiseries/newseries/remember`,
        _x,
	`multiseries/newseries/recurse`,
	`multiseries/pow`,
	`multiseries/power`,
	`multiseries/run/cleanup`,
	`multiseries/run`,
	`multiseries/sign`,
	`multiseries/splitmultiseries`,
	`multiseries/truncate`,
	jsc_94,
	lncoeffdom,
	minwithcomp,
	newvar,
	series2SERIES,
	subsoreval,
	TypeForest,
	##oldseries, # used in newseries to output a series data-structure,
		   # and also useful in convert/series and expand/bigprod
	`multiseries/addMS`,
	`multiseries/mulMS`,
	newscale;

# This was suggested by James McCarron.
# It is useful for those parts of the code that use
# type(.,series) instead of type(.,'series').
init:=proc()
local oldwarnlevel;
   # suppress warning when doing with(MultiSeries) twice
   oldwarnlevel := interface('warnlevel'=0);
   TypeTools:-AddType( 'series', proc(e) type( e, ':-series' ) end );
   interface('warnlevel'=oldwarnlevel);
end:

$include <arithmetic/src/add.mm>
$include <arithmetic/src/compose.mm>
$include <arithmetic/src/diff.mm>
$include <arithmetic/src/homogenize.mm>
$include <arithmetic/src/mul.mm>
$include <arithmetic/src/power.mm>
$include <conversion/src/series2SERIES.mm>
$include <conversion/src/Series2Expr.mm>
$include <conversion/src/Series2Polynom.mm>
$include <foresttype/src/CommonType.mm>
$include <foresttype/src/TypeForest.mm>
$include <functions/src/abs.mm>
$include <functions/src/analytic.mm>
$include <functions/src/conjugate.mm>
$include <functions/src/csgn.mm>
$include <functions/src/diff.mm>
$include <functions/src/Dirac.mm>
$include <functions/src/exp.mm>
$include <functions/src/factorial.mm>
$include <functions/src/floor.mm>
$include <functions/src/GAMMA.mm>
$include <functions/src/Heaviside.mm>
$include <functions/src/hypergeom.mm>
$include <functions/src/leadterm.mm>
$include <functions/src/ln.mm>
$include <functions/src/LambertW.mm>
$include <functions/src/min.mm>
$include <functions/src/O.mm>
$include <functions/src/piecewise.mm>
$include <functions/src/polylog.mm>
$include <functions/src/Re.mm>
$include <functions/src/signum.mm>
$include <functions/src/_var.mm>
$include <functions/src/Zeta.mm>
$include <limit/src/DominantTerm.mm>
$include <limit/src/getoneterm.mm>
$include <limit/src/InfinitePart.mm>
$include <limit/src/leadterm.mm>
$include <limit/src/limit.mm>
$include <limit/src/splitmultiseries.mm>
$include <new/src/newleadterm.mm>
$include <new/src/newasympt.mm>
$include <new/src/newlimit.mm>
$include <new/src/newseries.mm>
$include <new/src/newtaylor.mm>
$include <kernel/src/doit.mm>
$include <kernel/src/Expr2Series.mm>
$include <kernel/src/multiseries.mm>
$include <kernel/src/newscale.mm>
###$include <kernel/src/run.mm>
$include <kernel/src/series.mm>
$include <kernel/src/Series2BigO.mm>
$include <tools/src/CompareExponent.mm>
$include <tools/src/ComparisonFunction.mm>
$include <tools/src/fastest.mm>
$include <tools/src/ishiddenzero.mm>
$include <tools/src/locate.mm>
$include <tools/src/print.mm>
$include <tools/src/RealImPart.mm>
$include <tools/src/sign.mm>
$include <tools/src/subsoreval.mm>
$include <tools/src/Testzero.mm>
$include <tools/src/truncate.mm>
$include <tools/src/WhatType.mm>
$include <type/src/ScaleType.mm>
$include <type/src/ScaleVar.mm>
$include <type/src/SeriesType.mm>
$include <type/src/series.mm>
$include <SeriesInfo/src/SeriesInfo.mm>

### Holonomic functions
$include <functions/holonomic/src/Shared.mm>
$include <functions/holonomic/src/AiryAi.mm>
$include <functions/holonomic/src/AiryBi.mm>
$include <functions/holonomic/src/AngerJ.mm>
$include <functions/holonomic/src/BesselI.mm>
$include <functions/holonomic/src/BesselJ.mm>
$include <functions/holonomic/src/BesselK.mm>
$include <functions/holonomic/src/BesselY.mm>
$include <functions/holonomic/src/ChebyshevT.mm>
$include <functions/holonomic/src/ChebyshevU.mm>
$include <functions/holonomic/src/Chi.mm>
$include <functions/holonomic/src/Ci.mm>
$include <functions/holonomic/src/CoulombF.mm>
$include <functions/holonomic/src/CylinderD.mm>
$include <functions/holonomic/src/CylinderU.mm>
$include <functions/holonomic/src/CylinderV.mm>
$include <functions/holonomic/src/DAiryAi.mm>
$include <functions/holonomic/src/DAiryBi.mm>
$include <functions/holonomic/src/Ei.mm>
$include <functions/holonomic/src/EllipticCE.mm>
$include <functions/holonomic/src/EllipticCK.mm>
$include <functions/holonomic/src/EllipticE.mm>
$include <functions/holonomic/src/EllipticEfirstarg.mm>
$include <functions/holonomic/src/EllipticE2ndarg.mm>
$include <functions/holonomic/src/EllipticF.mm>
$include <functions/holonomic/src/EllipticFfirstarg.mm>
$include <functions/holonomic/src/EllipticK.mm>
$include <functions/holonomic/src/GegenbauerC.mm>
$include <functions/holonomic/src/HermiteH.mm>
$include <functions/holonomic/src/JacobiP.mm>
$include <functions/holonomic/src/KummerM.mm>
$include <functions/holonomic/src/KummerU.mm>
$include <functions/holonomic/src/LaguerreL.mm>
$include <functions/holonomic/src/LegendreP.mm>
$include <functions/holonomic/src/LegendreQ.mm>
$include <functions/holonomic/src/LommelS1.mm>
$include <functions/holonomic/src/LommelS2.mm>
$include <functions/holonomic/src/Shi.mm>
$include <functions/holonomic/src/Si.mm>
$include <functions/holonomic/src/Ssi.mm>
$include <functions/holonomic/src/StruveH.mm>
$include <functions/holonomic/src/StruveL.mm>
$include <functions/holonomic/src/WeberE.mm>
$include <functions/holonomic/src/WhittakerM.mm>
$include <functions/holonomic/src/WhittakerW.mm>
$include <functions/holonomic/src/arccos.mm>
$include <functions/holonomic/src/arccosh.mm>
$include <functions/holonomic/src/arccot.mm>
$include <functions/holonomic/src/arccoth.mm>
$include <functions/holonomic/src/arccsc.mm>
$include <functions/holonomic/src/arccsch.mm>
$include <functions/holonomic/src/arcsec.mm>
$include <functions/holonomic/src/arcsech.mm>
$include <functions/holonomic/src/arcsin.mm>
$include <functions/holonomic/src/arcsinh.mm>
$include <functions/holonomic/src/arctan.mm>
$include <functions/holonomic/src/arctanh.mm>
$include <functions/holonomic/src/cos.mm>
$include <functions/holonomic/src/cosh.mm>
$include <functions/holonomic/src/dawson.mm>
$include <functions/holonomic/src/dilog.mm>
$include <functions/holonomic/src/erf.mm>
$include <functions/holonomic/src/erfc.mm>
$include <functions/holonomic/src/erfcbivar.mm>
$include <functions/holonomic/src/erfi.mm>
$include <functions/holonomic/src/sin.mm>
$include <functions/holonomic/src/sinh.mm>

## Other functions that are generated more or less automatically
$include <functions/other/src/Beta.mm>
$include <functions/other/src/EllipticNome.mm>
$include <functions/other/src/FresnelC.mm>
$include <functions/other/src/FresnelS.mm>
$include <functions/other/src/Fresnelf.mm>
$include <functions/other/src/Fresnelg.mm>
$include <functions/other/src/Hankel.mm>
$include <functions/other/src/Kelvin.mm>
$include <functions/other/src/Li.mm>
$include <functions/other/src/bernoulli.mm>
$include <functions/other/src/binomial.mm>
$include <functions/other/src/cot.mm>
$include <functions/other/src/coth.mm>
$include <functions/other/src/csc.mm>
$include <functions/other/src/csch.mm>
$include <functions/other/src/euler.mm>
$include <functions/other/src/factorial.mm>
$include <functions/other/src/harmonic.mm>
$include <functions/other/src/pochhammer.mm>
$include <functions/other/src/sec.mm>
$include <functions/other/src/sech.mm>
$include <functions/other/src/tan.mm>
$include <functions/other/src/tanh.mm>

## Should be loaded after the holonomic
## Ei, arctan, AiryAi, AiryBi, DAiryAi, DAiryBi
$include <functions/src/arctan_bivar.mm>
$include <functions/src/Airy_bivar.mm>
$include <functions/src/EllipticE_bivar.mm>
$include <functions/src/EllipticF_bivar.mm>
$include <functions/src/Ei_bivar.mm>
$include <functions/src/LegendreP_bivar.mm>
$include <functions/src/LegendreQ_bivar.mm>
$include <functions/src/erfc_bivar.mm>
$include <functions/holonomic/src/isonbranchcut.mm>

$include <tools/src/AddFunction.mm>
$include <kernel/src/run.mm>

# Define the package exports
_pexports := () -> [ op( { exports( MultiSeries ) } minus
{ ':-_pexports', ':-init',
':-oldseries'#,:-`multiseries/function`
} ) ];


end module:


#savelib('MultiSeries','SERIES','t_SERIES','`diff/SERIES`','`print/SERIES`','`print/_var`','`type/SCALE`','`type/SERIES`','`type/ScaleVar`','`type/MultiSeries:-taylor`','`type/MultiSeries:-series`','`type/t_SERIES`','_var'):
