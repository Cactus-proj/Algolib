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
###    Title: 	`multiseries/analytic`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###                          fct    a function assumed to be analytic
###			     expr   a SERIES data-structure not tending to 
###					infinity
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :
###                          the power multiseries expansion of f(expr)
###                          w.r.t. the variable var
### Description           :
###                          Compute a multiseries expansion using derivatives
###                          in the standard way.
###			     This also applies in the multivariate case,
###			     There, the difficulty is that we still need
###			     COMPOSE to handle indefinite cancellation.
### Note: In the multivariate case, a lot of this code is common with
### `multiseries/D`. However, I do not see how to avoid this duplication. 
### BS. Oct. 05.

`multiseries/analytic`:=proc(fct,expr,scale,var,ord) :: {SERIES,identical(0)}:
option ALGOCOPYRIGHT;

local L, i :: integer, largs, nbargs, ser2, x, argseq, lvars;
#    ASSERT(expr :: {'SERIES',identical(0),list({'SERIES',identical(0)})}
#	and scale :: 'SCALE' and var :: list('ScaleVar') and ord :: nonnegint);
# The first assertion is not necessarily satisfied, if one of the arguments of
# fct is not algebraic.
    ASSERT(scale :: 'SCALE' and var :: list('ScaleVar') and ord :: nonnegint);

    if not type(expr,list) then largs:=[expr] else largs:=expr fi;
    nbargs:=nops(largs);

    # first, check the limit of expr
    for i to nbargs do
	if largs[i]=0 then L[i]:=0
	elif type(largs[i],'SERIES') then
	    L[i]:=LIMIT(largs[i],scale,var);
	    if L[i]=undefined then
		error "unable to compute multiseries; undefined limit for %1",
		    `multiseries/Series2Expr`(largs[i],scale)
	    fi
	fi
    od;

    for i to nbargs do
	if assigned(L[i]) then
	    if has(L[i],infinity) then ser2[i]:=POWER(largs[i],-1,args[3..-1])
	    elif L[i]=0 then ser2[i] := largs[i]
	    else ser2[i]:=ADDDOIT(largs[i],-L[i])
	    fi
	else ser2[i]:=NULL
	fi
    od;

    # then recurse on the number of variables (note that this code has
    # exponential complexity in the number of variables and should be 
    # improved).
#    return `multiseries/analytic/recurse`(fct,largs,[seq(L[i],i=1..nbargs)],
#	[seq(ser2[i],i=1..nbargs)],scale,var,ord);
    argseq:=seq(`if`(assigned(L[i]),x[i],largs[i]),i=1..nbargs);
    lvars:=[seq(`if`(assigned(L[i]),x[i],NULL),i=1..nbargs)];
    return `multiseries/analytic/recurse`(
	unapply(fct(argseq),lvars),
	[seq(`if`(assigned(L[i]),largs[i],NULL),i=1..nbargs)],
	[seq(`if`(assigned(L[i]),L[i],NULL),i=1..nbargs)],
	[seq(ser2[i],i=1..nbargs)],scale,var,ord)
end :                                                  # `multiseries/analytic`
#------------------------------------------------------------------------------

#`multiseries/analytic/recurse`
#Input:
#	fct	a procedure (result of an unapply)
#	largs	a list of (SERIES or 0), the arguments
#	L	list of limits of largs
#	ser2	a list of SERIES ready for substitution:
#		at 0 ser2[i]=largs[i]
#		at L ser2[i]=largs[i]-L
#		at infty ser2[i]=1/largs[i]
#	scale, var, ord: as usual
#Output:
#	the composition.
#Description: this is done by recursing on the number of variables
#	and applying Taylor expansion (using diff) in the univariate case.
#	COMPOSE is used to deal with indefinite cancellation.
#
`multiseries/analytic/recurse`:=proc(fct,largs,L,ser2,scale,var,ord) :: {SERIES,identical(0)}:
option ALGOCOPYRIGHT;
local coef :: {table,list}, ser1 :: {SERIES,identical(0)}, u, v, i :: integer, argsf, lcoef, nbargs, res;

    nbargs:=nops(L);

    # Compute the ``power series'' expansion of fct(v) at L 
    # using derivatives

    argsf:=[seq(u[i],i=1..nbargs-1),v];
    if has(L[-1],infinity) then argsf:=subsop(-1=1/v,argsf) fi;
    coef[0]:=fct(op(argsf));
    if ser2[-1]<>0 then
	for i to ord+1 do coef[i]:=diff(coef[i-1],v)/i od;
	lcoef:=[seq(coef[i],i=0..ord+1)]
    else lcoef:=[coef[0]] fi;
    if has(L[-1],infinity) then
	lcoef:=subs(v=1/v,lcoef);
	argsf:=subsop(-1=v,argsf)
    fi;

    if has(lcoef,'diff') then lcoef := map(convert,lcoef,'D') fi;

    if not has(L[-1],infinity) then
	try lcoef:=eval(lcoef,v=L[-1]);
	catch:
	    error "the function %1 or one of its derivatives has a singularity at %2, but is not known to multiseries. See ?multiseries/function to extend the knowledge of multiseries.",fct(op(argsf)),v=L[-1];
	end try
    elif nbargs=1 then lcoef:=map(Limit,subs(v=_X,lcoef),_X=L[-1])
    else 
	error "multivariate analytic function not handled at v=infinity",
	    fct(op(argsf));
    fi;

    # encode this power series as a SERIES data-structure
    if ser2[-1]<>0 then
	ser1 := ADDDOIT(seq(MULDOIT(lcoef[i],
	    RUN(SCALEVARIABLE^(i-1),# args[5..-1]
	    	scale,[SCALEVARIABLE],i
	    )),i=1..nops(lcoef)));
	ser1:= subsop(
	    COEFFBIGO=1,
	    EXPONBIGO=nops(lcoef),
	    EXPR4SERIES=
	    fct(op(argsf[1..-2]),`if`(not has(L[-1],infinity),
		L[-1]+scale['variable'],1/scale['variable'])),ser1);
	# compute the composition ser1(ser2)
	if ser1=0 then return 0
	else res:=COMPOSE(ser1,ser2[-1],args[5..-1])
	fi
    else
	res:=CONVERT2SERIES(lcoef[1],scale,var,true)
    fi;
    if nbargs=1 or res=0 then res
    else
	`multiseries/analytic/recurseonseries`(res,argsf,args[2..-1])
    fi
end : # `multiseries/analytic/recurse`

# `multiseries/analytic/recurseonseries`
#Input: 
#	S	the result of univariate expansion on the last coordinate
#	argsf	the names that have been used for the coordinates
#	largs,L,ser2,scale,var,ord same as for analytic/recurse
#Output: the result of composition
#	Since part of the result has already been computed, this has to go
#	inside the SERIES, invoke analytic/recurse on the coefficients,
#	and recombine the results.
`multiseries/analytic/recurseonseries`:=proc(S,argsf,largs,L,ser2,scale,var,ord)
local lcoef, res, i, v, bigO;
    # Now recurse:
    ### Several problems:
    # 1. the O() term has to be dealt with
    # 2. the LISTCOEFF itself may contain SERIES,
    #    so we have to recurse inside them.
    if S=0 then return 0 fi;
    if op(TYPECOEFF,S)='t_SERIES' then 
	lcoef:=map(procname,op(LISTCOEFF,S),args[2..-1]);
	if op(COEFFBIGO,S)=0 then bigO:=0
	else bigO:=procname(op(COEFFBIGO,S),args[2..-1]) fi;
    else
	lcoef:=map(`multiseries/analytic/recurse`,
	    map(unapply,op(LISTCOEFF,S),argsf[1..-2]),
	    largs[1..-2],L[1..-2],ser2[1..-2],args[6..-1]);
	if op(COEFFBIGO,S)=0 then bigO:=0
	else 
	    bigO:=`multiseries/analytic/recurse`(
		unapply(op(COEFFBIGO,S),argsf[1..-2]),
		largs[1..-2],L[1..-2],ser2[1..-2],args[6..-1])
	fi
    fi;
    v:=op(EXPANVAR,S);
    res:=ADDDOIT(seq(MULDOIT(lcoef[i],
	RUN(v^(op([LISTEXPON,i],S)),scale,var,ord)),
	i=1..nops(op(LISTEXPON,S))));
    res:=ADDDOIT(res,MULDOIT(bigO,
	subsop(LISTCOEFF=[],LISTEXPON=[],COEFFBIGO=1,S)));
    subsop(EXPR4SERIES=subs(
	[seq(argsf[i]=`if`(ser2[i]=0,L[i],L[i]+op(EXPR4SERIES,ser2[i])),
	i=1..nops(argsf))],op(EXPR4SERIES,S)),res)
end: #`multiseries/analytic/recurseonseries`
