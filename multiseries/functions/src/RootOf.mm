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
###    Title: 	RootOf
###    Created:	Aug 2004
###    Author: 	Bruno Salvy
###    Contact: 	Bruno.Salvy@inria.fr
###
### Description: computation of the RootOf of a multiseries with a
### simple enough closed form.
### This is also one of the 3 places (with ln & exp) where the scale can be
### extended.
### References            :
### @Article{SaSh99,
###   author =	 {Bruno Salvy and John Shackell},
###   title =	 {Symbolic Asymptotics: Multiseries of Inverse Functions},
###  journal =	 {Journal of Symbolic Computation},
###  year =	 1999,
###  volume = 27,
###  number = 6,
###  month = jun,
###  pages = "543--563",
###  askreprint = {Bruno.Salvy@inria.fr}
### }
#------------------------------------------------------------------------------
### Name                  : `multiseries/function`[RootOf]
### Input                 :
###			     expr   a maple expression
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                : 	
###                             the expansion of RootOf(expr) in the
###				(possibly extended) scale associated to expr
### Description           :
###
### We are computing RootOf(f(x,_Z)) as a series in x, the
### variable in the scale.
###
### In this version, only inversion is considered, meaning that 
### f(x,_Z) has to be of the form x-g(_Z).


FUNCTIONTABLE['RootOf'] := proc(expr, scale, var, ord) :: SERIES:
option ALGOCOPYRIGHT;

local g, y, inverses, limpt, lengthlim, i;

    ASSERT( scale :: 'SCALE' and var :: list('ScaleVar') and ord :: nonnegint);
    
    if not type(f,linear(SCALEVARNAME)) then
    	error "non-linear case not implemented yet" fi;
    g:=subs(_Z=y,-coeff(f,SCALEVARNAME,0)/coeff(f,SCALEVARNAME,1));
    # The first step is to determine the limit of _Z and create the scale
    # for _Z tending to this limit.
    # Use solve to get a branch of g tending to the limit of SCALEVAR
    limpt:=op(2,SCALEVARIABLE);
    if has(limpt,infinity) then inverses:=solve(1/g,y)
    else inverses:=solve(g-limpt,y)
    fi;
    if inverses=NULL then # it could still be infinity
    	if limit(g,y=infinity)=limpt then # Found it!
    	    inverses := infinity
    	else error "unable to compute limit"
    	fi
    elif nops([inverses])>1 then # select one
    	inverses:=[inverses];
    	if member(0,inverses) then inverses:=0
    	else 
    	    lengthlim:=map(length,inverses);
    	    member(min(op(lengthlim)),lengthlim,'i');
    	    inverses:=inverses[i]
    	fi
    fi;
    # reduce all cases to inversion at infinity
    if limpt=infinity then
    	`multiseries/inverse`(multiseries(g,y=inverses,1),args[2..nargs])
    elif limpt=0 then 
    	POWER(`multiseries/inverse`(multiseries(1/g,y=inverses,1),
    		args[2..nargs]),-1,args[2..nargs])
    else
    	ADD(RUN(limpt,args[2..nargs]),
    		POWER(`multiseries/inverse`(multiseries(
    			1/(g-limpt),y=inverses,1),args[2..nargs]),
    			-1,args[2..nargs]))
    fi
end proc:                        # `multiseries/function`[RootOf]
#------------------------------------------------------------------------------
### Name                  : `multiseries/inverse`
### Input                 :
###			     expr   a SERIES expansion tending to infinity
###			     origscale the scale for the inverse
###                          origvar    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis origscale
###                          ord    a nonnegative integer
### Output                : 	
###                             the expansion of the solution of expr=var in the
###				(possibly extended) origscale
### Description           :
# 

`multiseries/inverse`:=proc(expr,origscale,origvar,ord)
local dt, scale, rest;
    scale:=op(THESCALE,expr);
    dt:=DOMINANTTERM(expr,scale,SCALELIST, rest);
    # By construction, dt tends to infinity
    if op(LISTEXPON,dt)<>[0] then # it is < 0 
    	# depends on whether there is a slower scale element in expr or not
    	member(
    else # leading term not in this scale
    fi
end:
