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
###    Title: 	`multiseries/Testzero`
###    Created:	Oct 2003
###    Author: 	Bruno Salvy
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###                         expr   an expression
### Output                :
###                         boolean telling whether 0 has been recognized 
### Description           :
###			    invoke Testzero, but helps it in case the user
###			    has modified the standard one and it does not
###			    expect the expressions created by multiseries.
###			    This happens for instance when the user has
###			    modified Normalizer to become
###			    proc(a) `mod/Expand`(a, p) end
###

`multiseries/Testzero`:=proc (expr,scale)
option ALGOCOPYRIGHT;
local orig;
    try
	Testzero(expr);
    catch:
	orig:=subs(SCALEVARIABLE=op(SCALEVARIABLE),expr);
	try
	    Testzero(orig)
	catch:
	    evalb(Normalizer(numer(normal(orig)))=0)
	end try;
    end try;
end: # `multiseries/Testzero`

## This is supposedly expensive and called only when 
## not knowing whether we have a zero would result in an ERROR.
## 
# opt is 'symbolic', it should be used only when the expression
# is already known (by previous expansions) to be very likely to be 0.
`multiseries/lastchancetestzero`:=proc(expr,opt)
option ALGOCOPYRIGHT, remember;
    evalb(expr=0 or 		# By increasing order of 
    	Testzero(expr)=true or  # cost.
    	simplify(args)=0)
end:
