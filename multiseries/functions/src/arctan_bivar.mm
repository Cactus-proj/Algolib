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
###    Title: 	arctan
###    Created:	Nov 2003
###    Author: 	Bruno Salvy & Ludovic Meunier
###    Contact: 	Bruno.Salvy@inria.fr
###
### This is loaded by MultiSeries.mpl after arctan.mm.
### It redefines `multiseries/function`[arctan] so as to deal with 
### the bivariate case as well. The code in the non-list part is
### that of the original `multiseries/function`[arctan].
###

`multiseries/function/arctanunivar`:=eval(FUNCTIONTABLE['arctan']):

FUNCTIONTABLE['arctan']:=proc (expr, scale, var, order)
local x,y;
option ALGOCOPYRIGHT;
    if not type(expr,list) then # one argument case
	`multiseries/function/arctanunivar`(args)
    else
	#                             x + I y
	#    arctan(y, x) = - I ln(------------)
	#                            2    2 1/2
	#                          (x  + y )
	y:=expr[1];
	x:=expr[2];
	if y=0 then 0 
	else RUN(-I*'ln'((x+I*y)/(x^2+y^2)^(1/2)),args[2..-1]) fi
    fi
end:
