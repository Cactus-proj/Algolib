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
###    Title: 	LegendreP
###    Created:	Oct 04
###    Author: 	Bruno Salvy
###    Contact: 	Bruno.Salvy@inria.fr
###
### This is loaded by MultiSeries.mpl after LegendreP.mm.
### It redefines `multiseries/function`[LegendreP] so as to deal with 
### the bivariate case as well.
###

`multiseries/function/LegendreP3arg`:=eval(FUNCTIONTABLE['LegendreP']);

FUNCTIONTABLE['LegendreP']:=proc(expr,scale,var,order)
option ALGOCOPYRIGHT;
    if not type(expr,list) then error "expected 2 or 3 arguments, got 1"
    elif nops(expr)=3 then
    	`multiseries/function/LegendreP3arg`(args)
    elif nops(expr)=2 then
    	`multiseries/function/LegendreP3arg`([expr[1],0,expr[2]],args[2..-1])
    else error "expected 2 or 3 arguments, got %1",nops(expr)
    fi
end: