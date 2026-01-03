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
###    Title: 	`multiseries/2polynom`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 : 	ser 	a SERIES data-structure
### Output                : 	a polynomial
### Description           :
###                         this procedure drops the order term
###                         of the SERIES data-structure ser and returns the
###                         associated maple expression
###			    the next routine does not drop the order term
###
 
`multiseries/2polynom` := proc( ser )
option ALGOCOPYRIGHT;
    `multiseries/Series2sumofprod`(ser,true)
end proc :		                               # `multiseries/2polynom`
#----------------------------------------------------------------------------

`multiseries/Series2sumofprod` := proc( ser, removebigo )
option ALGOCOPYRIGHT;
local lcoeff, bigo, expon, v, i :: integer ;
    ASSERT( ser :: 'SERIES' );
    lcoeff := op(LISTCOEFF,ser);
    expon := op(LISTEXPON,ser);
## This should not be done here, since Series2sumofprod is called by Series2BigO 
## which is called by compose.
##    v:=op([EXPANVAR,1],ser);
    v:=op(EXPANVAR,ser);
    if op(TYPECOEFF,ser)='t_SERIES' then
	    lcoeff := map(procname,lcoeff,removebigo)
    elif has(lcoeff,SCALEVARIABLE) and has(lcoeff,exp) then
	    lcoeff := map(convert,lcoeff,trig)
    fi;
    bigo:=op(COEFFBIGO,ser);
    if type(bigo,'SERIES') then bigo:=procname(bigo,removebigo)
    elif removebigo then bigo:=0
    elif bigo<>0 then bigo:=O(bigo*v^op(EXPONBIGO,ser))
    fi;
    add(lcoeff[i]*v^expon[i],i=1..nops(lcoeff))+bigo
end proc :		                 # `multiseries/Series2sumofprod`
#----------------------------------------------------------------------------

