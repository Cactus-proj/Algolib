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

# Input: a series for which LIMIT has recognized that the limit is 0.
# Output: true or false
# This tries not to waste too much time with TESTZERO, but spots a few obvious
# cases, using the fact that TESTZERO has been used in LIMIT.

`multiseries/ishiddenzero`:=proc(expr,scale,var)
local lexps, i;
    if op(EXPONBIGO,expr)<>infinity then false
    else
	lexps:=op(LISTEXPON,expr);
	if member(op(TYPEEXPON,expr),['integer','rational','float']) then
	    for i to nops(lexps) while lexps[i]<0 do od
	else
	    for i to nops(lexps) while signum(lexps[i])=-1 do od
	fi;
    	evalb(i>nops(lexps) or 
    	    i=nops(lexps) and lexps[i]=0 
    	    and (op(TYPECOEFF,expr)<>'t_SERIES' and 
    	    	  not has(op([LISTCOEFF,i],expr),SCALEVARIABLE)
    	           or (op(TYPECOEFF,expr)='t_SERIES'
		    and procname(op([LISTCOEFF,i],expr),scale,var))))
    fi
end:
