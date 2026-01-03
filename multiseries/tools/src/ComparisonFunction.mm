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
###    Title: 	`multiseries/ComparisonFunction`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 : 
###                          typeexpon    a type in TypeForest
### Output                : 
###                          the associated comparison function 
###

`multiseries/ComparisonFunction` := proc(typeexpon) :: procedure ;
option ALGOCOPYRIGHT; 
    ASSERT(typeexpon :: type);
         if typeexpon='algebraic'
         then return `multiseries/CompareExponent`
         elif type(typeexpon[`<`],'procedure')
         then return typeexpon[`<`]
         else return `<`
         end if 

end proc :                                  # `multiseries/ComparisonFunction`
#----------------------------------------------------------------------------

