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
###    Title: 	`multiseries/Series2BigO`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###				expr	a maple expression or a SERIES
###				scale	a SCALE data-structure 
### Output                :
###                          the encoding of expr considered as the 
###                          coefficient of a big O, using the same structure
###			     as expr (i.e., SERIES --> SERIES in the same
###				variables; nonSERIES --> nonSERIES).
### Description           :
###                          if expr is a constant w.r.t. the elements in the
###                          asymptotic basis scale['list'], or if it is 
###                          bounded then this procedure returns 1.
###
###


`multiseries/Series2BigO` := proc( expr, scale )
option ALGOCOPYRIGHT;
local res;
    ASSERT(scale::'SCALE');
    if type(expr,'SERIES') then
	if op(COEFFBIGO,expr)<>0 then applyop(procname,COEFFBIGO,expr,scale)
	elif op(LISTEXPON,expr)=[0] then
	    if op(EXPANVAR,expr)=SCALELIST[1] then # oscillatory coefficient
		res:=1
	    else res:=procname(op(op(LISTCOEFF,expr)),scale) fi;
	    subsop(LISTCOEFF=[],LISTEXPON=[],COEFFBIGO=res,EXPONBIGO=0,expr)
	elif op(LISTEXPON,expr)=[] then subsop(COEFFBIGO=1,expr)
	else
	    subsop(
	    [LISTCOEFF,-1]=NULL,
	    [LISTEXPON,-1]=NULL,
	    EXPONBIGO=op([LISTEXPON,-1],expr),
	    COEFFBIGO=procname(op([LISTCOEFF,-1],expr),scale),expr)
	fi
    elif has(expr,SCALEVARNAME) then
	res:=`multiseries/Series2BigO/cleanup`(
	    RUN(expr,scale,SCALELIST,0),scale);
	if type(res,'SERIES') then `multiseries/2polynom`(res,scale)
	else res fi
    else 1 
    end if
end proc :                                          # `multiseries/Series2BigO`
#------------------------------------------------------------------------------

# temporary: this should be moved elsewhere and shared with parts of leadterm

`multiseries/Series2BigO/cleanup`:=proc(ser,scale)
option ALGOCOPYRIGHT;
local res;
    if not type(ser,'SERIES') then
	    if has(ser,SCALELIST) then return ser else return 1 fi
    elif op(COEFFBIGO,ser)=0 and op(EXPONBIGO,ser)=infinity and 
	    op(LISTEXPON,ser)=[0] then return procname(op([LISTCOEFF,1],ser),scale)
    elif op(LISTEXPON,ser)=[] then
	    res:=subsop(LISTCOEFF=[procname(op(COEFFBIGO,ser),scale)],
	        LISTEXPON=[op(EXPONBIGO,ser)],COEFFBIGO=0,EXPONBIGO=infinity,ser);
    else res:=subsop(LISTCOEFF=[procname(op([LISTCOEFF,1],ser),scale)],
	    LISTEXPON=[op([LISTEXPON,1],ser)],ser);
    fi;
    if not type(op([LISTCOEFF,1],res),op(TYPECOEFF,res)) then
	    res:=subsop(TYPECOEFF=WHATTYPE(op([LISTCOEFF,1],res)),res)
    else res
    fi
end:


