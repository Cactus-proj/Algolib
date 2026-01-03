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
###    Title: 	`multiseries/sign`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###                          f      a SERIES data-structure
###                          equal  (optional) boolean telling whether signum(0)=1
### Output                :  
###                         the sign of the ``leading term'' of f
### Description           :
###                         this procedure is used in exp when we need 
###                         to know the sign of the limit of f 
### Implementation        : call `multiseries/leadterm` and signum
###

`multiseries/sign` := proc (f, equal)
option ALGOCOPYRIGHT; 
local res, scale, sigres, tmpres, dummyreal;
    ASSERT(f::{'SERIES',identical(0)});

    if f=0 then return f end if;

    res := LEADTERM(f) :
    
    if res=0 then 0 
    else
#        while type(res,SERIES) do res:=op([LISTCOEFF,1],res) od;
#        if has(res,_var) then res:=`multiseries/Expr4Series2Expr`(res,op(THESCALE,f)) fi;
#        signum(res)
#	#while op(TYPECOEFF,res)='t_SERIES' do res := op([LISTCOEFF,1],res) od;
#	#signum(op([LISTCOEFF,1],res))
    	while op(TYPECOEFF,res)='t_SERIES' do res := op([LISTCOEFF,1],res) od;
    	res:=op([LISTCOEFF,1],res);                
    	scale:=op(THESCALE,f);
    	if has(res,SCALEVARIABLE) then
    	    if has(res,'exp') then # this helps signum
    	        res:=convert(res,'trig')
    	    fi;
    	    tmpres:=subs(SCALEVARIABLE=dummyreal,res)
    	else tmpres:=res
    	fi;
	if nargs=2 and equal then sigres:=signum(0,tmpres,1) assuming dummyreal::real
	elif has(res,SCALEVARIABLE) then sigres:=signum(tmpres) assuming dummyreal::real
	else sigres:=signum(tmpres)
        fi;
        if has(sigres,dummyreal) then sigres:=subs(dummyreal=SCALEVARIABLE,sigres) fi;
        sigres
    fi
end proc :                                                 # `multiseries/sign`
#----------------------------------------------------------------------------

