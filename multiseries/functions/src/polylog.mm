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

##    -*-Maple-*-
##
##    Title: 	FUNCTIONTABLE[polylog]
##    Created:	Thu Oct 30 07:30:05 2003
##    Author: 	Bruno Salvy
##		<Bruno.Salvy@inria.fr>
##
### Input                 :
###                          expr   a list of SERIES
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer

FUNCTIONTABLE['polylog']:=proc(expr,scale,var,ord)::SERIES:
option ALGOCOPYRIGHT;
local a, zlim, i,j, res, bern, lnexpr1, B,A, k, L, pow,tosubs, neword;
    if expr[2]=0 then return 0 fi;
    if expr[1]=0 then a:=0
    elif op(LISTEXPON,expr[1])=[0] and op(EXPONBIGO,expr[1])=infinity then
	a:=CONVERT2POLYNOM(expr[1]); # a does not depend on z
    else a:=expr[1]
    fi;
    if a=0 then return
	MULDOIT(expr[2],POWER(ADDDOIT(1,
	    MULSERIESCST(expr[2],-1,'integer')),-1,args[2..-1]))
    elif a=1 then return
	LN(POWER(ADDDOIT(1,MULSERIESCST(expr[2],-1,'integer')),
	    -1,args[2..-1]),args[2..-1])
    elif a=2 then return	
	FUNCTIONTABLE['dilog'](ADDDOIT(1,
	    MULSERIESCST(expr[2],-1,'integer')),args[2..-1])
    fi;
    zlim:=LIMIT(expr[2],scale,var);
    if zlim=0 then
    	if ISHIDDENZERO(expr[2]) then return 0 fi;
	if not type(a,'SERIES') then
	    COMPOSE('SERIES'(scale,[seq(1/i^a,i=1..ord-1)],1,
		`if`(type(a,integer),'rational','algebraic'),
		[$1..ord-1],ord,'integer',SCALEVARIABLE,
		'polylog'(a,SCALEVARIABLE)),expr[2],args[2..-1])
	else
	    pow[0]:=1;
	    for i to ord do pow[i]:=MULDOIT(pow[i-1],expr[2]) od;
	    L:=ADD([seq(MULDOIT(pow[i],EXP(MULSERIESCST(expr[1],
		-ln(i),'algebraic'),args[2..-1])),i=1..ord-1)]);
	    ADDDOIT(L,SERIES2BIGO(
		MULDOIT(pow[ord],EXP(MULSERIESCST(expr[1],-ln(ord),
		    'algebraic'),args[2..-1])),scale))
	fi
    elif zlim=1 or Testzero(zlim-1) then
	if not type(a,integer) then
	    error "unable to compute expansion of polylog at 1" fi;
	tosubs:=MULSERIESCST(ADDDOIT(expr[2],-1),-1,'integer');
	# First, we compute the series of polylog(1-u,a) at u=0.
	neword:=max(1,ord);
	B:=[0$neword];  # coefficient of 1
	A:=[-1,0$neword-1]; # coefficient of ln(u)
	for i from 2 to a do
	    B:=[seq(add(B[k],k=1..j),j=1..neword)];
	    A:=[seq(add(A[k],k=1..j),j=1..neword)];
	    B:=[Zeta(i),seq(A[k]/k^2-B[k]/k,k=1..neword-1)];
	    A:=[0,seq(-A[k]/k,k=1..neword-1)]
	od;
	# the EXPR4SERIES should not escape
	B:='SERIES'(scale,B,1,'algebraic',[$0..neword-1],neword,'integer',
		SCALEVARIABLE,`multiseries/nonloginpolylog`(SCALEVARIABLE));
	if a<=ord+1 then 
	    A:='SERIES'(scale,A[a..neword],1,'rational',[$a-1..neword-1],neword,
		'integer',SCALEVARIABLE,
		`multiseries/loginpolylog`(SCALEVARIABLE));
	else A:='SERIES'(scale,[],1,'rational',[],a-1,'integer',
		SCALEVARIABLE,`multiseries/loginpolylog`(SCALEVARIABLE))
	fi;
	subsop(EXPR4SERIES='polylog'(a,op(EXPR4SERIES,expr[2])),
	    ADDDOIT(COMPOSE(B,tosubs,args[2..-1]),
		MULDOIT(COMPOSE(A,tosubs,args[2..-1]),
		    LN(tosubs,args[2..-1]))))
    elif has(zlim,infinity) then
	if not type(a,integer) then
	    error "unable to compute expansion of polylog at infinity" fi;
	res:=procname(subsop(2=POWER(expr[2],-1,args[2..-1]),expr),
	    args[2..-1]);
	res:=MULSERIESCST(res,-(-1)^a,'integer');
	bern:=RUN(bernoulli(a,1/SCALEVARIABLE)*SCALEVARIABLE^a,args[2..-1]);
	lnexpr1:=MULSERIESCST(LN(expr[2],args[2..-1]),1/2/I/Pi,'algebraic');
	lnexpr1:=ADDDOIT(lnexpr1,(1+csgn(I*subs(infinity=1,zlim)))/2);
	bern:=COMPOSE(bern,POWER(lnexpr1,-1,args[2..-1]),args[2..-1]);
	subsop(EXPR4SERIES='polylog'(a,op(EXPR4SERIES,expr[2])),
	    ADDDOIT(res,MUL([bern,POWER(lnexpr1,a,args[2..-1]),
	    -(2*Pi*I)^a/a!])))
    else ANALYTIC(polylog,args)
    end if
end proc;

