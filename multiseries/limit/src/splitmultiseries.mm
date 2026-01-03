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
###    Title: 	`multiseries/splitmultiseries`
###    Created:	Oct 2003
###    Author: 	Bruno Salvy
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###			     expr   a SERIES
###                          scale  a SCALE 
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which expr 
###                                 is expanded.
### Output                :
###                          a list of 3 expressions
###                              - the part of the expansion expr which 
###                                tends to infinity;
###                              - its constant part (wrt var[-1]);
###                              - the part which tend to 0.
### Description           : same as InfinitePart, but no reexpansion.
###
`multiseries/splitmultiseries`:=proc (expr)
local infinitepart,constantpart,zeropart, i, j, v, lexp, lcoeffs,comp,expr4res,typeexpon;
    if expr=0 then return 0,0,0 fi;
    # Compute the infinite part of expr
    typeexpon := op(TYPEEXPON,expr);
    comp := COMPARISONFUNCTION(typeexpon);
    if comp(op(EXPONBIGO,expr),0) then 
	return expr,0,0 fi;# all terms tend to infinity
    v:=op(EXPANVAR,expr);
    lexp:=op(LISTEXPON,expr);
    lcoeffs:=op(LISTCOEFF,expr);
    i := LOCATE(lexp,0,comp); # infinite from 1 to i
    if i=0 then infinitepart:=0;expr4res:=0
    else
	if op(TYPECOEFF,expr)<>'t_SERIES' then 
	     expr4res:=add(lcoeffs[j]*v^lexp[j],j=1..i)
	else expr4res:=add(op(EXPR4SERIES,lcoeffs[j])*v^lexp[j],j=1..i)
	fi;
	infinitepart:=subsop(LISTCOEFF=lcoeffs[1..i],
		    LISTEXPON=lexp[1..i],COEFFBIGO=0,EXPONBIGO=infinity,
		EXPR4SERIES=expr4res,expr)
    fi;
    if i=nops(lexp) or comp(0,lexp[i+1]) then
	constantpart:=0;
	zeropart:=subsop(LISTCOEFF=lcoeffs[i+1..-1],
	    LISTEXPON=lexp[i+1..-1],
	    EXPR4SERIES=op(EXPR4SERIES,expr)-expr4res,expr)
    else 
	constantpart:=lcoeffs[i+1];
	zeropart:=subsop(LISTCOEFF=lcoeffs[i+2..-1],
	    LISTEXPON=lexp[i+2..-1],
    	    EXPR4SERIES=op(EXPR4SERIES,expr)-expr4res-
	    `if`(type(constantpart,'SERIES'),op(EXPR4SERIES,constantpart),
		constantpart),expr)
    fi;
    infinitepart,constantpart,zeropart
end: # `multiseries/splitmultiseries`
