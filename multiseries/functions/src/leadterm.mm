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
###    Title: 	`multiseries/function`[leadterm]
###    Created:	Oct 2004
###    Author: 	Bruno Salvy
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###			     expr   a SERIES data-structure
###                          scale  a SCALE data-structure
### Output                :
###                          this is not the same as MultiSeries:-leadterm
###			     which should be preferred in programming.
###			     This one is here to emulate the old
###			     series('leadterm'())

FUNCTIONTABLE['leadterm'] := proc( expr, scale) :: {SERIES,identical(0)}:
option ALGOCOPYRIGHT;
local res;
    ASSERT(expr :: {'SERIES',identical(0)} and scale :: 'SCALE');
    if expr=0 then return 0 fi;
    res:=`multiseries/getoneterm`(expr,scale,[SCALEVARIABLE]);
    if type(res,'SERIES') then 
    	res:=subsop(COEFFBIGO=0,EXPONBIGO=infinity,res);
    	subsop(EXPR4SERIES=CONVERT2POLYNOM(res),res)
    else res
    fi
end: # `multiseries/function`[leadterm]

