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
###    Title: 	factorial
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
#------------------------------------------------------------------------------
### Name                  : `multiseries/function`[factorial]
### Input                 :
###                          expr   a SERIES data-structure or a list 
###                                 SERIES (for Incomplete Gamma functions)
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :
###                         multiseries expansion of expr!

FUNCTIONTABLE['factorial'] := proc ( expr, scale, var, ord ) :: SERIES :
option ALGOCOPYRIGHT;
    ASSERT( expr::{'SERIES',identical(0)} and scale :: 'SCALE' and
               var::list('ScaleVar') and ord::nonnegint);
    FUNCTIONTABLE['GAMMA'](ADDDOIT(expr,CONVERT2SERIES(1,scale,var,false)),
	args[2..-1])
end:
