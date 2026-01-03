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
###    Title: 	Ei
###    Created:	Oct 2003
###    Author: 	Bruno Salvy & Ludovic Meunier
###    Contact: 	Bruno.Salvy@inria.fr
###
### This is loaded by MultiSeries.mpl after Ei.mm.
### It redefines `multiseries/function`[Ei] so as to deal with 
### the bivariate case as well. The code in the non-list part is
### copy-pasted from that of the original `multiseries/function`[Ei], 
### except that Ei has a strange "no branch-cut" that need special handling.
###
FUNCTIONTABLE['Ei']:=proc (expr, scale, var, order)
option ALGOCOPYRIGHT;
local lim, argseq, res, am1, mam1, zam1   ;
    if not type(expr,list) then # one argument case
	    if expr = 0 then return CONVERT2SERIES(Ei(0), scale, var, false) fi;
    	lim := LIMIT(expr, scale, var);
    	if lim = undefined then error "unable to compute series"
    	elif has(lim, infinity) then lim := infinity
    	end if;
    	if lim = infinity then argseq :=
    	    POWER(expr, -1, scale, var, order), scale, var, order
    	else argseq := ADDDOIT(expr,CONVERT2SERIES(
    	    -lim, scale, var, false)), scale, var, order
    	end if;
    	if lim = 0 then
    	    res := `multiseries/function`[Ei, 0](argseq);
    	    ##### no branch cut for Ei.
    	    if SIGN(expr)=-1 then res:=ADDDOIT(res,-I*Pi)
    	    else res
    	    fi
    	elif lim = infinity then res :=
    	    `multiseries/function`[Ei, infinity, -Pi .. Pi](argseq,
    	    [op(1, [argseq]), scale, var, order])
    	else res := eval(
    	    `multiseries/function`[Ei, _AnyFiniteOrdinaryPoint](argseq),
    	    _AnyFiniteOrdinaryPoint = lim)
    	end if;
    	subsop(9 = 'Ei'(op(9, expr)), res)
    else
	## z^(a-1)*GAMMA(1-a,z) -- first version
    	am1:=ADDDOIT(expr[1],-1);
    	if am1<>0 then mam1:=MULSERIESCST(am1,-1,'integer') else mam1:=0 fi;
    	zam1:=EXP(MULDOIT(am1,LN(expr[2],args[2..-1])),args[2..-1]);
    	res:=subsop(EXPR4SERIES='Ei'(op(map2(op,EXPR4SERIES,expr))),
    	    MULDOIT(zam1,FUNCTIONTABLE['GAMMA']([mam1,expr[2]],args[2..-1])));
    fi
end proc;
