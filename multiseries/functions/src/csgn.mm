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
###    Title:   `multiseries/function`[csgn]
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
###                        corresponding csgn
###
### The difficult part of the work is done in RealImPart.

`multiseries/function`[csgn]:=proc(ser,scale,var,ord)::{SERIES,identical(0)};
local real,im, X;
option ALGOCOPYRIGHT; 
    ASSERT(ser::{'SERIES',identical(0),list({'SERIES',identical(0)})});
        
    if type(ser,list) then
    	if nops(ser)=3 then
    	    if ser[2]=0 then return CONVERT2SERIES(ser[3],args[2..-1])
    	    else return procname(ser[2],args[2..-1]) fi
    	elif nops(ser)<>2 then
    	    error "wrong number of arguments in csgn, %1",nops(ser)
    	else X:=ser[2]
    	fi
    else X:=ser
    fi;
    REALIMPART(X,real,im) ;
    if nops(ser)=2 then
    	if real<>0 then 0
    	else error "undefined"
    	fi
    elif real=0 then CONVERT2SERIES(SIGN(im),scale,var,false)
    else CONVERT2SERIES(SIGN(real),scale,var,false)
    fi
end proc: # `multiseries/function`[csgn]
