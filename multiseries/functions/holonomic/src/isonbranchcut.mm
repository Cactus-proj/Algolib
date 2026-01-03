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

#########################################################
### Description: test whether a point belongs to a segment. 
### Argument sequence:
#	seg::[{iRootOf,signed_infinity},{iRootOf,signed_infinity}]
#	pt::SERIES
### Output format::boolean
############################################################
`multiseries/IsOnBranchCut` := proc (seg, pt, scale, var)
local lim,alpha,to_test,s;
option ALGOCOPYRIGHT;

    lim:=LIMIT(args[2..-1]);
    if has(seg[1],'infinity') and has(seg[2],'infinity') then
	return evalb(has(lim,infinity) or 
		pt=0 or member(signum(pt),map(signum,seg)))
    elif has(seg,infinity) then
	alpha:=signum(op(select(has,seg,infinity)));
	if has(lim,infinity) then
#	    return evalb(member(signum(Re(subs(infinity=1,lim)/alpha)),[0,1]))
##	    return evalb(signum(Re(subs(infinity=1,lim)/alpha))<>-1)
        to_test:=signum(subs(infinity=1,lim))/alpha;
        return evalb(to_test=1 or has(to_test,signum))
	fi;
	to_test:=lim-op(remove(has,seg,infinity))
    elif has(lim,infinity) then return false
    else
    	alpha:=signum(seg[2]-seg[1]);
    	to_test:=lim-seg[1]
    fi;
    # check if it's on the half-line
    s:=signum(0,to_test,0);
    if s=0 then
	evalb(LIMIT(FUNCTIONTABLE['signum'](ADDDOIT(pt,-lim),
		args[3..4],1),args[3..-1])=alpha)
    elif s<>alpha then false 
    elif has(seg,infinity) then true
    else # check if it's on the segment
	s:=signum(0,seg[2]-lim,0);
	if s=0 then
	    evalb(LIMIT(FUNCTIONTABLE['signum'](ADDDOIT(pt,-lim),args[3..4],1),
		args[3..-1])=-alpha)
	else evalb(s=alpha)
	fi
    fi
end;	# `multiseries/IsOnBranchcut`

