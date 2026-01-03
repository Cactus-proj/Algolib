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
###    Title: 	erfc
###    Created:	Oct 04
###    Author: 	Bruno Salvy
###    Contact: 	Bruno.Salvy@inria.fr
###
### This is loaded by MultiSeries.mpl after erfc.mm.
### It redefines `multiseries/function`[erfc] so as to deal with 
### the bivariate case as well.
###

`multiseries/function/erfcunivar`:=eval(FUNCTIONTABLE['erfc']);

FUNCTIONTABLE['erfc']:=proc(expr,scale,var,order)
option ALGOCOPYRIGHT;
    if not type(expr,list) then # one argument case
    	`multiseries/function/erfcunivar`(args)
    elif nops(expr)<>2 then error "wrong number of arguments in erfc"
    else
    	# do not let a closed form in terms of erfcbivar escape
    	subs(erfcbivar=erfc,FUNCTIONTABLE['erfcbivar'](args))
    fi
end: