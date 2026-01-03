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
###    Title:   `multiseries/function`[Dirac]
###    Created: Aug 04
###    Author:  Bruno Salvy
###    Contact: Bruno.Salvy@inria.fr
###
### Input                 : 
###                         ser       a SERIES data-structure or 0
###				      or a list of such
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                : 0
`multiseries/function`[Dirac]:=proc(ser,scale,var,ord)
option ALGOCOPYRIGHT; 
	0
end proc: # `multiseries/function`[Dirac]
