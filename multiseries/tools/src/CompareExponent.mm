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
###    Title: 	`multiseries/CompareExponent`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###				expo1
###				expo2
###
### Output                : boolean true if expo1 < expo2 false otherwise
### Description           :
###				this procedure is used when the exponents
###				of a multiseries are non-numeric, 
###                             and only then.
###
###				Note that false may mean "I don't know".
###
### Other                 :
###                             Currently, this procedure is not exported
###				and thus cannot be modified by users.
###				This should probably be changed.

`multiseries/CompareExponent` := proc(expo1,expo2) :: boolean ;
option ALGOCOPYRIGHT; 
local sig;
    # default definition
    # evalb(signum(expo1-expo2)=-1)
    # modified BS Aug. 03: signum(infinity-a) does not work when a is a symbol
    sig:=signum(expo1-expo2);
    if has(sig,signum) then error "need to determine the sign of",expo1-expo2 fi;
    evalb(sig=-1 or expo2=infinity)

end proc:                                       # `multiseries/CompareExponent`
#----------------------------------------------------------------------------

