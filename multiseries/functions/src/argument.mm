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
###    Title:   `multiseries/function`[argument]
###    Created: June 2006
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
###                        corresponding argument
###
### The difficult part of the work is done in RealImPart.

`multiseries/function`[argument]:=proc(ser,scale,var,ord)::{SERIES,identical(0)};
local real,im;
option ALGOCOPYRIGHT; 
    ASSERT(ser::{'SERIES',identical(0),list({'SERIES',identical(0)})});
        
    if ser=0 then return 0 fi;
	REALIMPART(ser,real,im) ;
	if im=0 then
	    if SIGN(real)=-1 then CONVERT2SERIES(Pi,scale,var,false)
	    else 0
	    fi
	else
	    `multiseries/function`[arctan](MUL(im,POWER(real,-1,args[2..-1])),args[2..-1])
	fi
end proc: # `multiseries/function`[argument]
