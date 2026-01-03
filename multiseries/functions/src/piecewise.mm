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

`multiseries/function`[piecewise]:=proc(lser,scale,var,ord)
local i, rel, sig, alldecided;
    alldecided:=true;
    for i by 2 to nops(lser) do
	rel:=lser[i];
	if type(rel,{`<`,`<=`}) then
	    sig:=signum(op(2,rel)-op(1,rel));
	    if sig=1 then return RUN(lser[i+1],args[2..-1])
	    elif sig<>-1 then alldecided:=false
	    fi
#	elif is(rel) then return RUN(lser[i+1],args[2..-1])
#	else alldecided:=false
	fi
    od;
    if alldecided then
	if irem(nops(lser),2)=0 then 0
	else RUN(lser[-1],args[2..-1])
	fi
    else
    	RUN(thaw(convert('piecewise'(
    	    op(applyop(freeze,{seq(2*i,i=1..iquo(nops(lser),2)),nops(lser)},
    		lser))),'Heaviside')),args[2..-1])
    fi
end:
