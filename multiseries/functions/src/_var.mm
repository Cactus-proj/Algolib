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
###    Title: 	_var
###    Created:	Oct 2003
###    Author: 	Bruno Salvy
###    Contact: 	Bruno.Salvy@inria.fr
###
### Description: expansion of _var
### This is used when series is invoked in run through series/fct. and 
### a new series is required by this series/fct.
#------------------------------------------------------------------------------
### Name                  : `multiseries/function`[_var]
### Input                 :
###			     
###			     lexpr list consisting of
###			     	oldvar SERIES for name
###			     	pt     SERIES for expansion point
###                          	expr   SERIES for the actual _var
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :  expansion of this _var in scale
###

FUNCTIONTABLE[_var] := proc(lexpr, scale, var, ord) :: SERIES:
option ALGOCOPYRIGHT;

    ASSERT( lexpr::list({'SERIES',identical(0)}) and scale :: 'SCALE' and
                 var :: list('ScaleVar') and ord :: nonnegint);
    lexpr[3]

end proc:                        # `multiseries/function`[_var]
#------------------------------------------------------------------------------


