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
###    Title:   `multiseries/function`[Heaviside]
###    Created: Oct 2003
###    Author:  Bruno Salvy
###    Contact:         Bruno.Salvy@inria.fr
###
### Input                 : 
###                         ser       a SERIES data-structure or 0
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                : 
###                         
###                        Heaviside(ser)=1 if ser>0, 0 otherwise.
###			   ser is assumed to be real
###
`multiseries/function`[Heaviside]:=proc(ser,scale,var,ord)::{SERIES,identical(0)};
local L, s;
option ALGOCOPYRIGHT; 
    ASSERT(ser::{'SERIES',identical(0)});
    if ser=0 then 0
    else
	L:=LIMIT(ser,scale,var);
	if L=0 then s:=SIGN(ser) else s:=signum(L) fi;
	if s=-1 then 0
	elif s=1 then CONVERT2SERIES(1,scale,var,false)
	elif L<>0 then CONVERT2SERIES(Heaviside(L),scale,var,false)
	else CONVERT2SERIES(s,scale,var,false)
	fi
    fi
end proc: # `multiseries/function`[Heaviside]
