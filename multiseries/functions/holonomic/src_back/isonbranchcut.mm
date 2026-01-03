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
`multiseries/IsOnBranchCut` := proc (seg, pt)
local lim,alpha,to_test,s;
option ALGOCOPYRIGHT;

    lim:=LIMIT(pt);
    if has(lim,infinity) then # infinity has to be an endpoint,
    			      # with the right argument
	return member(signum(lim),map(signum,select(has,seg,infinity)))
    elif has(seg[1],'infinity') and has(seg[2],'infinity') then
	return evalb(pt=0 or member(signum(pt),map(signum,seg)))
    elif has(seg,infinity) then
	alpha:=signum(op(select(has,seg,infinity)));
	to_test:=lim-op(remove(has,seg,infinity))
    else
    	alpha:=signum(seg[2]-seg[1]);
    	to_test:=lim-seg[1]
    fi;
    # check if it's on the half-line
    s:=signum(0,to_test,0);
    if s=0 then evalb(LIMIT(signum(ADDDOIT(pt,-lim)))=alpha)
    elif s<>alpha then false 
    elif has(seg,infinity) then true
    else # check if it's on the segment
	s:=signum(0,seg[2]-lim,0);
	if s=0 then evalb(LIMIT(signum(ADDDOIT(pt,-lim)))=-alpha)
	else evalb(s=alpha)
	fi
    fi
end;	# `multiseries/IsOnBranchcut`

