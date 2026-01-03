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
###    Title: 	LambertW
###    Created:	Sep 2004
###    Author: 	Bruno Salvy
###    Contact: 	Bruno.Salvy@inria.fr
###
#------------------------------------------------------------------------------
### Name                  : `multiseries/function`[LambertW]
### Input                 :
###                          expr   a SERIES data-structure or a list 
###                                 SERIES (for Incomplete Gamma functions)
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :
###                         multiseries expansion of LambertW(expr)
###
### Ultimately, this should be handled directly by `multiseries/RootOf`.
### Currently, this is just a MultiSeries version of the old series/LambertW.

FUNCTIONTABLE['LambertW'] := proc ( expr, scale, var, ord ) :: SERIES :
option ALGOCOPYRIGHT;

local pt, ind, L, closedform, s, y, dominant, res, closeddom, genpt, final,i;

    ASSERT( expr::{'SERIES',identical(0),list({'SERIES',identical(0)})}
	and scale :: 'SCALE' and var::list('ScaleVar') and ord::nonnegint);
    
    if not type(expr,list) then pt:=expr; ind:=0  # classical LambertW function
    elif nops(expr)=2 then pt:=expr[2]; ind:=`multiseries/Series2Expr`(expr[1])
    else error "LambertW expects 1 or 2 arguments, got %1",nops(expr)
    fi;
    if not type(ind,'integer') then
	error "cannot expand LambertW with first argument %1",ind
    fi;

    if pt=0 then
    	if ind=0 then return 0 else error "singularity encountered" fi
    fi;

    L := LIMIT(pt,scale,var) :
    if L=undefined then error "unable to compute series"
    elif has(L,infinity) then
	# Asymptotic expansion -- equation exp(u)u=expr
	closedform:='LambertW'(`if`(ind<>0,ind,NULL),op(EXPR4SERIES,pt));
	dominant:=DOMINANTTERM(pt,scale,var,res); # res is assigned
#	dominant:=LEADTERM(pt);
#	if dominant=pt then res:=0
#	else res:=ADDDOIT(pt,MULSERIESCST(dominant,-1,integer))
#	fi;
	closeddom:='LambertW'(`if`(ind<>0,ind,NULL),
	    op(EXPR4SERIES,dominant));
	if res<>0 then
	    # The idea is W(x(1+t))=W(x)+sum(r_k(W(x))t^k,k=1..infinity),
	    # where r_k is a simple rational function.
	    L := subs('LambertW'(genpt)=closeddom,
		genpt=op(EXPR4SERIES,dominant),applyop(normal,LISTCOEFF,
		    ADDDOIT(
			procname('SERIES'(scale,[genpt,genpt],0,'algebraic',
			    [0,1],infinity,'integer',SCALEVARIABLE,
				genpt*(1+SCALEVARIABLE)),
			    scale,[SCALEVARIABLE],ord),
			'SERIES'(scale,[-'LambertW'(genpt)],0,'algebraic',
			    [0],infinity,'integer',SCALEVARIABLE,
			    -'LambertW'(genpt)))));
	    res:=MULDOIT(res,POWER(dominant,-1,args[2..-1]));
	    res:=subsop(EXPR4SERIES=closedform-closeddom,COMPOSE(L,res,args[2..-1]))
	fi;
	# Expand the leading term if necessary
	if op([LISTEXPON,1],dominant)=0 and nops(var)>1 then
	    dominant:=procname(dominant,scale,var[1..-2],ord);
	elif op([LISTEXPON,1],dominant)=0 or 
	    op(EXPANVAR,pt)=SCALELIST[1] or 
	    1/SCALELOG[op(EXPANVAR,pt)]<>
		FASTEST([var[1],1/SCALELOG[op(EXPANVAR,pt)]],scale) then
	    # it is necessary to introduce a new log in the scale if it's not
	    # there yet, so that limit works when given this closed form later.
	    if op(EXPANVAR,pt)=SCALELIST[1] then
	    	`multiseries/LnExtend`(scale)
	    fi;
	    dominant:=CONVERT2SERIES(closeddom,scale,var,true)
	else
	    dominant:=subsop(EXPR4SERIES=closeddom,
		jsc_94(-1,ind,dominant,args[2..-1]))
	fi;
	final:=dominant;
	if res<>0 then 
#	    for i to nops(op(LISTCOEFF,res)) do
#		final:=ADDDOIT(final,MULDOIT(RUN(subs(closeddom=dominant,
#		    op([LISTCOEFF,i],res)),args[2..-1]),
#			subsop(LISTCOEFF=[1],LISTEXPON=[i],COEFFBIGO=0,res)))
#	    od;
#	    final:=ADDDOIT(final,
#		subsop(LISTCOEFF=[],LISTEXPON=[],res))
	    final:=ADDDOIT(final,res)
	fi;
	subsop(EXPR4SERIES=closedform,final)
    elif L=0 then
	if ind=0 then
	    COMPOSE('SERIES'(scale,[seq((-1)^(i+1)*i^(i-1)/i!,i=1..ord)],
	    	1,'rational',[$1..ord],ord+1,'integer',scale['variable'],
	    	'LambertW'(scale['variable'])),args[1..-1])
	else # Branch -- equation exp(u)/u=1/pt
	    closedform:='LambertW'(ind,op(EXPR4SERIES,pt));
	    if var[-1]=FASTEST([var[-1],op(EXPANVAR,pt)],scale) then
	    	# it is necessary to introduce a new log in the scale if it's not
	    	# there yet, so that limit works when given this closed form later.
	    	if op(EXPANVAR,pt)=SCALELIST[1] then
	    	    `multiseries/LnExtend`(scale)
		fi;
		CONVERT2SERIES(closedform,scale,var,true)
	    else
		subsop(EXPR4SERIES=closedform,
		MULSERIESCST(jsc_94(1,`if`(ind>0,-ind,-ind-1),
	    	    MULSERIESCST(POWER(pt,-1,args[2..-1]),-1,integer),
			args[2..-1]),
		-1,integer))
	    fi
	fi
    elif ind=0 and (L=-exp(-1) or TESTZERO(L+exp(-1))=true) then # singularity
	Order:=max(2,ord+1);
	s:=series2SERIES(solve(
	    MultiSeries:-series((2+2*(y-1)*'exp'(y))^(1/2),y)=SCALEVARIABLE,y),
	    args[2..-1]);
	ADDDOIT(-1,COMPOSE(
	    subsop(EXPR4SERIES=1+LambertW(exp(-1)*SCALEVARIABLE^2-exp(-1)),s),
	    POWER(MULSERIESCST(ADDDOIT(expr,-L),2*'exp'(1),algebraic),
		1/2,args[2..-1]),args[2..-1]))
    else # analytic behaviour
	ANALYTIC(LambertW,args)
    fi
end proc:	                     # `multiseries/function`['LambertW']

# A special case of my 1994 paper in the Journal of Symbolic Computation,
# with a minor change to handle the index of the branches.
# It is assumed that expr tends to infinity.
jsc_94:=proc(alpha,indbranch,expr,scale,var,ord)
local logexpr, loglog, u, P, i, j, oneoverlog, loglogpow, oneoverlogpow, oldres, res, new;
    logexpr:=ADDDOIT(LN(args[3..-1]),indbranch*2*I*Pi);
    loglog:=LN(logexpr,args[4..-1]);
    res:=ADDDOIT(logexpr,MULSERIESCST(loglog,alpha,integer));
    if ord>1 then
    	oneoverlog:=POWER(logexpr,-1,args[4..-1]);
    	oneoverlogpow:=1;
    	loglogpow[0]:=1;
	P:=alpha*u
    fi;
    for i to ord-1 do
    	oneoverlogpow:=MULDOIT(oneoverlogpow,oneoverlog);
    	loglogpow[i]:=MULDOIT(loglogpow[i-1],loglog);
    	oldres:=res;
        # Compute the polynomials. There are several ways to do this:
        # use the closed form with Stirling numbers;
        # use the recurrence for Stirling numbers;
        # or simply this:
	if i=ord-1 then new:=loglogpow[i]
	else
	    P:=alpha*(diff(P,u)-(i-1)*P);
	    P:=add(coeff(P,u,j)*u^(j+1)/(j+1),j=0..i);
	    new:=ADDDOIT(
    		seq(MULSERIESCST(loglogpow[j],coeff(P,u,j),rational),j=1..i))
	fi;
    	res:=ADDDOIT(res,MULDOIT(oneoverlogpow,new));
    	if res=oldres then break fi
    od;
    # This adds a O() on the last term
    `multiseries/run/cleanup`(res,args[4..-1])
end:
