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
###    Title: 	EllipticF
###    Created:	Oct 2004
###    Author: 	Bruno Salvy
###    Contact: 	Bruno.Salvy@inria.fr
###
### This is loaded by MultiSeries.mpl after EllipticF.mm.
### It redefines `multiseries/function`[EllipticF] so as to deal with 
### the case when the parameter moves as well.
###

`multiseries/function/EllipticF2ndarg`:=eval(FUNCTIONTABLE['EllipticF']):

FUNCTIONTABLE['EllipticF']:=proc (expr, scale, var, order)
option ALGOCOPYRIGHT;
    if not type(expr,list) or nops(expr)<>2 then
	error "wrong number of arguments, EllipticF"
    elif expr[1]=0 or not has(op(EXPR4SERIES,expr[1]),SCALEVARIABLE) then
	`multiseries/function/EllipticF2ndarg`(args)
    elif expr[2]=0 or not has(op(EXPR4SERIES,expr[2]),SCALEVARIABLE) then
    	# exchange arguments, since EllipticFfirstarg expects parameter first
    	# do not let the closed form escape
    	eval(FUNCTIONTABLE['EllipticFfirstarg']([expr[2],expr[1]],args[2..-1]),
    		'EllipticFfirstarg'=proc(a,x) 'EllipticF'(x,a) end)
    else # assume analytic 
    	ANALYTIC('EllipticF',args)
    fi
end:
