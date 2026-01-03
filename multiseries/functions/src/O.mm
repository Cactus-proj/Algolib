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
###    Title: 	`multiseries/function`[0]
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###			     expr   a SERIES data-structure
###                          scale  a SCALE data-structure
### Output                :
###                          the multiseries expansion of O(expr)
###			     Note that EXPR4SERIES is not changed
###

FUNCTIONTABLE['O'] := proc( expr, scale) :: {SERIES,identical(0)}:
option ALGOCOPYRIGHT;
local res;
    ASSERT(expr :: {'SERIES',identical(0)} and scale :: 'SCALE');
    if expr=0 then return 0 fi;
    if op(LISTEXPON,expr)=[] then res:=expr
    else res:=subsop(LISTCOEFF=[],LISTEXPON=[],
		    COEFFBIGO=op([LISTCOEFF,1],expr),
		    EXPONBIGO=op([LISTEXPON,1],expr),expr)
    fi;
    if type(op(COEFFBIGO,res),'SERIES') then
	res:=applyop(procname,COEFFBIGO,res,scale)
    fi;
    applyop(SERIES2BIGO,COEFFBIGO,res,scale)
end: # `multiseries/function`[O]

