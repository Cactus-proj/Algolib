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
###    Title: 	`type/ScaleVar`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 : 	expr	any non-NULL expression
### Output                : 	boolean
### Description           : type-checking function
###
###                         In a SCALE data-structure, elements of the
###                         asymptotic basis are represented by ScaleVar
###                         data-structure
### References            :
###                          see `type/SCALE`
###

`type/ScaleVar` := indexed :          # `type/ScaleVar`

#------------------------------------------------------------------------------
### Name                  : `signum/_var`
### Description           :
###                         Suppose that we have created a SCALE data-structure
###                         and that _var(0) represents a variable x tending
###                         to 0.
###
###                         We assume that _var(0) is positive.
###
###                         Hence, evalc(ln(_var(0))) should return
###                         ln(_var(0)) and not
###                         ln(| _var(0) |) + I (1/2 - 1/2 signum(_var(0))) Pi
###
###                         The procedure `signum/_var` gives this result.

# `signum/_var`:=proc() option ALGOCOPYRIGHT; 1 end proc:
#----------------------------------------------------------------------------

