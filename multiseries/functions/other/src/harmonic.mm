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
###    Title: 	harmonic numbers
###    Created:	Oct 2004
###    Author: 	Bruno Salvy
###    Contact: 	Bruno.Salvy@inria.fr
###
#------------------------------------------------------------------------------
### Name                  : `multiseries/function`[harmonic]
### Input                 :
###                          expr   a SERIES data-structure or a list 
###                                 SERIES (for bivariate case)
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :
###                         multiseries expansion of harmonic(expr)
###

FUNCTIONTABLE['harmonic'] := proc ( expr, scale, var, ord ) :: SERIES :
option ALGOCOPYRIGHT;

    ASSERT( expr::{'SERIES',identical(0),list({'SERIES',identical(0)})}
	and scale :: 'SCALE' and var::list('ScaleVar') and ord::nonnegint);
    
    if expr=0 then return 0 fi;
    if not type(expr,list) then # classical harmonic numbers 
	ADDDOIT(FUNCTIONTABLE['Psi'](ADDDOIT(1,expr),args[2..-1]),gamma)
    elif type(expr,list) and nops(expr)=2 then
	ADDDOIT(FUNCTIONTABLE['Zeta'](expr[1],args[2..-1]),
	    MULSERIESCST(FUNCTIONTABLE['Zeta']([0,expr[1],ADDDOIT(1,expr[2])],args[2..-1]),
		-1,'integer'))
     else error "harmonic expects 1 or 2 arguments, got %1",nops(expr)
     end if 

end proc:	                     # `multiseries/function`[harmonic]

