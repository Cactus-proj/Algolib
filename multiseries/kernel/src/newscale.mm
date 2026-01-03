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
###    Title: 	newscale
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 : varname	name of a variable
###                         pt          a complex number or infinity
###
### Output                :     the scale associated to x tending to pt
### Description           :	construct an object of type SCALE
### Side Effects          : the scale is a local table whose name escapes
### Other                 : A global function name _var is used to represent
###				variables tending to 0.
###
###
### Todo		  : Create a utility to print the scale

newscale := proc (var :: name, pt ) :: SCALE ;
option ALGOCOPYRIGHT, remember; 
local v, Scale, sgn, transl;# SCALE will escape

    if pt=0 then transl:=var
    elif has(pt,var) then error "invalid point"
    elif has(pt,infinity) then sgn:=signum(pt); transl:=sgn/var
    else transl:=1-var/pt
    fi;

    Scale['point']             :=       pt;
    Scale['varname']	       := 	var;
    v			       :=	newvar(transl);
    Scale['variable'] 	       := 	v;
    Scale[list]	               :=	[v];
    # Do not remove the quotes on the rhs.
    Scale['log'][v]	       := 	1/'log'(1/v); 

    if pt=0 then Scale['back'] :=	v
    elif has(pt,infinity) then Scale['back']:=sgn/v
    else Scale['back']	       := 	pt*(1-v)
    fi;

    Scale

end proc :                                                           # newscale
#----------------------------------------------------------------------------

`print/_var`:=proc(var,pt,transl) option ALGOCOPYRIGHT; transl end:
     
#`evalc/_var`:=proc()option ALGOCOPYRIGHT;  `evalc/split`(_var(args),0) end:
#
#`diff/_var`:=proc(var,pt,transl,newvar)option ALGOCOPYRIGHT; 
#    diff(transl,newvar)
#end:
#
#`Re/_var`:=proc() _var(args) end:
#`Im/_var`:=proc() 0 end:

newvar:=proc(transl)
global `property/object`;
        # assume(res>0);
        # This is better wrt the evaluation rules
        `property/object`[_var][transl]:=RealRange(Open(0),10^(-100)); # magic constant
        # it would be nice to put the information about diff/_var in
        # diff's remember table, but it is sometimes gc'd.
        _var[transl];
end:
