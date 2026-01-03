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
##    Title: 	FUNCTIONTABLE[hypergeom]
##    Created:	Tue Oct 28 22:32:22 2003
##    Author: 	Bruno Salvy
##		<Bruno.Salvy@inria.fr>
##

FUNCTIONTABLE[hypergeom]:=proc (expr,scale,var,ord) :: SERIES;
local s, lup, ldown, L, cofs, alpha, i, beta, tmpfunc;
options ALGOCOPYRIGHT;
    ASSERT(expr::list and expr[1]::list and expr[2]::list
	and expr[3]::{'SERIES',identical(0)});
    lup:=expr[1];
    ldown:=expr[2];
    if has([lup,ldown],SCALEVARNAME) then error "unable to compute series" fi;
    s:=expr[3];
    if s=0 then return 0 fi;
    L:=LIMIT(s,scale,var);
    if L=0 then
    	if ISHIDDENZERO(s,scale,var) then
    	    CONVERT2SERIES(1,scale,var,true)
    	else
	    cofs[0]:=1;
	    for i to ord-1 while cofs[i-1]<>0 do
		cofs[i]:=cofs[i-1]*mul(alpha+i-1,alpha=lup)/mul(beta+i-1,beta=ldown)/i;
	    od;
	    if cofs[i-1]=0 then # terminating case
		COMPOSE('SERIES'(scale,[seq(cofs[i],i=0..i-2)],0,algebraic,[$0..i-2],
		    infinity,integer,SCALEVARIABLE,'hypergeom'(lup,ldown,SCALEVARIABLE)),
		    s,args[2..-1])
	    else
		COMPOSE('SERIES'(scale,[seq(cofs[i],i=0..ord-1)],1,algebraic,[$0..ord-1],
		    ord,integer,SCALEVARIABLE,'hypergeom'(lup,ldown,SCALEVARIABLE)),
		    s,args[2..-1])
	    fi
	fi
    elif has(L,infinity) then error "unable to compute series"
    else # assume not a singularity (should be improved)
    	eval(ANALYTIC(tmpfunc,s,args[2..-1]),
    		# this subs seems to be necessary even in Maple 10
    		tmpfunc=subs([_LUP=lup,_LDOWN=ldown],
    			proc(x)hypergeom(_LUP,_LDOWN,x) end))
    fi

end: # FUNCTIONTABLE[hypergeom]
