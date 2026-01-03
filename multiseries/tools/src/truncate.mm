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
###    Title: 	`multiseries/truncate`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###			     ser    a SERIES data-structure
###			     valbigo a valuation (of type op(TYPEEXPON,ser))
###                          comp   the comparison function for this type
### Output                :
###			     1. the truncated part of ser up to valbigo
###			     2. the coefficient of order valbigo
###

`multiseries/truncate`:=proc (ser, valbigo, comp)
option ALGOCOPYRIGHT; 
local i, lexpons, res1;
    ASSERT(ser::{'SERIES',identical(0)});
    if ser=0 then 0,0
    else
	lexpons:=op(LISTEXPON,ser);
	i:=`multiseries/locate`(lexpons,valbigo,comp);
	if i = 0 then res1:=0
	else
	    res1:=subsop(LISTEXPON=lexpons[1..i],
		LISTCOEFF=[op([LISTCOEFF,1..i],ser)],
		COEFFBIGO=0,EXPONBIGO=infinity,ser)
	fi;
	if i<nops(lexpons) then
	    if comp(valbigo,lexpons[i+1]) then res1,0
	    else res1,op([LISTCOEFF,i+1],ser) fi
	elif comp(valbigo,op(EXPONBIGO,ser)) then res1,0
	elif valbigo=op(EXPONBIGO,ser) or 
	    comp(op(EXPONBIGO,ser),valbigo) or 
	    Testzero(valbigo-op(EXPONBIGO,ser)) then res1,op(COEFFBIGO,ser)
	else
	    error "need to determine the sign of %1",
	    valbigo-op(EXPONBIGO,ser)
	fi
    fi
end: # `multiseries/truncate`
#----------------------------------------------------------------------------

