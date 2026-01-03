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
###    Title: 	EllipticE
###    Created:	Oct 2004
###    Author: 	Bruno Salvy
###    Contact: 	Bruno.Salvy@inria.fr
###
### This is loaded by MultiSeries.mpl after EllipticE.mm.
### It redefines `multiseries/function`[EllipticE] so as to deal with 
### the bivariate case as well. The code in the non-list part is
### that of the original `multiseries/function`[EllipticE].
###

`multiseries/function/EllipticEunivar`:=eval(FUNCTIONTABLE['EllipticE']):

FUNCTIONTABLE['EllipticE']:=proc (expr, scale, var, order)
option ALGOCOPYRIGHT;
    if not type(expr,list) then # one argument case
	`multiseries/function/EllipticEunivar`(args)
    elif nops(expr)<>2 then error "wrong number of arguments, EllipticE"
    elif expr[2]=0 or not has(op(EXPR4SERIES,expr[2]),SCALEVARIABLE) then
    	# exchange arguments, since EllipticEfirstarg expects parameter first
    	# do not let the closed form escape
    	eval(FUNCTIONTABLE['EllipticEfirstarg']([expr[2],expr[1]],args[2..-1]),
    		'EllipticEfirstarg'=proc(a,x) 'EllipticE'(x,a) end)
    elif expr[1]=0 or not has(op(EXPR4SERIES,expr[1]),SCALEVARIABLE) then
	subs('EllipticE2ndarg'='EllipticE',
	    FUNCTIONTABLE['EllipticE2ndarg'](args))
    else # assume analytic 
    	ANALYTIC('EllipticE',args)
    fi
end:
