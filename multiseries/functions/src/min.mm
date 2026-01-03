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

##    -*-Maple-*-
##
##    Title: 	FUNCTIONTABLE[min,max]
##    Created:	Tue Oct 28 21:01:01 2003
##    Author: 	Bruno Salvy
##		<Bruno.Salvy@inria.fr>
##

FUNCTIONTABLE['min']:=proc (expr,scale,var,ord)
local res, i, sgn;
option ALGOCOPYRIGHT;
    if not type(expr,list) then expr # only one argument
    else
	res:=expr[1];
	for i from 2 to nops(expr) do
	    if expr[i]=0 then sgn:=SIGN(res)
	    else sgn:=SIGN(ADDDOIT(res,MULSERIESCST(expr[i],-1,integer))) fi;
	    if has(sgn,'signum') then error "unable to compute series"
	    elif sgn=1 then res:=expr[i]
	    fi
	od;
	res
    fi
end: # FUNCTIONTABLE['min']

FUNCTIONTABLE['max']:=proc (expr,scale,var,ord)
local res, i, sgn;
option ALGOCOPYRIGHT;
    if not type(expr,list) then expr # only one argument
    else
	res:=expr[1];
	for i from 2 to nops(expr) do
	    if expr[i]=0 then sgn:=SIGN(res)
	    else sgn:=SIGN(ADDDOIT(res,MULSERIESCST(expr[i],-1,integer))) fi;
	    if has(sgn,'signum') then error "unable to compute series"
	    elif sgn=-1 then res:=expr[i]
	    fi
	od;
	res
    fi
end: # FUNCTIONTABLE['max']
