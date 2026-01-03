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
###    Title: 	`multiseries/run`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###			     expr   a maple expression
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :
###                          the power multiseries expansion of expr
###                          w.r.t. the variable var
###
### Description           :  This is where the dag traversal is done.
###                          The nonzero constants are encoded as constants 
###                          SERIES w.r.t. var 
### Error Conditions      :
###                         if expr is a function with two arguments
###                         if the multiseries expansion of expr is not
###                         implemented
###

`multiseries/run` :=  proc( expr, scale, var, ord ) :: {SERIES,identical(0)} :
option remember,ALGOCOPYRIGHT; # dag traversal; the remember table is cleaned in multiseries
local res, fcn, largs, procfcn, vname, i, difforder;
    ASSERT(scale :: 'SCALE' and
	var :: list('ScaleVar') and ord :: nonnegint);
    if expr=0 then return 0
    elif expr = SCALEVARNAME then
	res := procname(SCALEBACK,args[2..-1])
    elif type(expr,`+`)
    then res := `multiseries/add`(map(procname,[op(expr)],args[2..-1]))
    elif type(expr,`*`)
    then res := `multiseries/mul`(map(procname,[op(expr)],args[2..-1]))
    elif type(expr,`^`) then
        if not has(op(2,expr),{SCALEVARNAME,SCALEVARIABLE}) then
            res := `multiseries/pow`( procname(op(1,expr),args[2..-1]),
	                                op(2,expr),args[2..-1])
	elif not has(op(1,expr),{SCALEVARNAME,SCALEVARIABLE}) then
	    # Allow ln to simplify things like ln(1/2) -> -ln(2)
	    # See asympt(2*(s+1)*binomial(2*s+2,s+1)*(1/2)^(2*s+2),s,1)
	    res := procname('exp'(op(2,expr)*ln(op(1,expr))),args[2..-1])
        else res := procname('exp'(op(2,expr)*'ln'(op(1,expr))),args[2..-1])
        end if
    elif type(expr,':-series') then res:=series2SERIES(args)
    elif type(expr,'function') then
	# There are a number of special cases that are treated first.
	# In a future version, there should be another table,
	# SPECFUNCTIONTABLE, for those functions for which the arguments
	# should not go through run.
	if op(0,expr)=_var and member( expr, SCALELIST) then
	    res:=CONVERT2SERIES('SERIES'(scale,[1],0,'integer',[1],infinity,
		'integer',expr,expr),scale,var,true)
	elif op(0,expr)=O then # special case for O() must be done before
	    procfcn := FUNCTIONTABLE['O'];
	    res:=procfcn( # the SERIES case to catch O(1)
	         procname(op(1,expr),scale,var,0),args[2..-1]);
	    if res<>0 then res:=applyop(O,EXPR4SERIES,res) fi
	elif not has(expr,{SCALEVARIABLE,SCALEVARNAME,'leadterm'}) then 
	    res:=CONVERT2SERIES(expr,scale,var,true)
	# This is now handled by `multiseries/function`[leadterm]
	# that mimicks more closely the old behaviour.
	#elif op(0,expr)='leadterm' then # support for old series(leadterm())
	   #res:=LEADTERM(procname(op(1,expr),scale,var,1));
	elif type(op(0,expr),function) and has(op(0,expr),D) then
	    if op([0,0],expr)=D then difforder:=[1]
	    elif type(op([0,0],expr),indexed) then
		difforder:=[op(op([0,0],expr))]
	    elif type(op([0,0],expr),function) and op([0,0,0],expr)=`@@` then
		if type(op([0,0,1],expr),indexed) then
		    difforder:=[(op(op([0,0,1],expr)))$op([0,0,2],expr)]
		else
		    difforder:=[1$op([0,0,2],expr)]
		fi
	    else error "unable to compute series"
	    fi;
	    res:=`multiseries/D`(op([0,1],expr),difforder,[op(expr)],args[2..-1])
	elif type(expr,'SERIES')  then
	    # This is now caught earlier.
	    # or not has(expr,{op(SCALELIST),SCALEVARNAME}) then 
	    res:=CONVERT2SERIES(expr,scale,var,true)
	elif op(0,expr)='eval' then # an attempt at supporting eval
	    res:=`multiseries/eval`(op(expr),args[2..-1])
	else
	    fcn:=op(0,expr);
	    procfcn := FUNCTIONTABLE[fcn] ;
	    if not type(procfcn,'procedure') and 
	        #### a temporary patch to accomodate series/.
		    type(`series/` || fcn, procedure) then
		    procfcn:=`series/`||fcn;
	        Order:=max(ord,3); # cannot give order 0 to series
		    # procfcn expects its second argument tending to 0
#		    largs:=subs(scale['varname']=
#		    subs(scale['variable']=vname,scale['back']),[op(expr)]);
            ## Use _var as the variable name, so that some knowledge of it can be used by signum
            vname:=_var;
		    largs:=subs(SCALEVARIABLE=vname,[op(expr)]);
		    res:=procfcn(op(largs),vname);
    		# Special case for RootOf or Mathieu that need various _Env to be setup
    		if res=FAIL then
    		    res:=oldseries(fcn(op(largs)),vname)
    		fi;
    		# series/RootOf returns FAIL
    		if res=FAIL then error "FAIL"
    		# avoid an infinite loop with series/LambertW
    		elif res=fcn(op(largs)) or 
    		    type(res,':-series') and op(1,res)=fcn(op(largs)) then
    		    error "unable to compute series"
    		fi;
    		res:=subs(vname=SCALEVARIABLE,res);
    		if type(res,specfunc(anything,'PuiseuxSeries')) then
    		    res:=op(res) fi;
    		res:=procname(res,args[2..-1])
	    else
    		for i to nops(expr) do
    		    if type(op(i,expr),algebraic) then
    			 largs[i]:=procname(op(i,expr),args[2..-1])
    		    else largs[i]:=op(i,expr)
    		    fi
    		od;
    		if nops(expr)=1 then largs:=largs[1]
    		else largs:=[seq(largs[i],i=1..nops(expr))] fi;
    		if type(procfcn,'procedure') then
    		    res:=procfcn(largs,args[2..-1])
    		else res:=`multiseries/analytic`(fcn,largs,args[2..-1])
    		fi
	    fi
	fi
    elif type(expr,algebraic) then
	res := CONVERT2SERIES(expr,scale,var,true)
    else error "invalid type",expr
    end if :

    `multiseries/run/cleanup`(res,args[2..-1])

end proc :                                                  # `multiseries/run`
#------------------------------------------------------------------------------

# Make sure that we do not return more than what was asked for.
`multiseries/run/cleanup`:=proc(res,scale,var,ord)
option ALGOCOPYRIGHT; 
local bigo, tmp, lord;
    if ord=0 then lord:=1 else lord:=ord fi; # avoids infinite loops
    if var=[] or not type(res,'SERIES') then res
    elif op(EXPANVAR,res)<>var[-1] then # the result uses a faster elt of scale
	tmp:=procname(res,scale,[op(EXPANVAR,res)],1);
	if op(LISTEXPON,tmp)<>[] then
	    applyop(procname,[LISTCOEFF,1],tmp,scale,var,ord)
	else tmp
	fi
    else
	if nops(op(LISTCOEFF,res))<=lord then tmp:=res
	else
	    bigo:=SERIES2BIGO(op([LISTCOEFF,lord+1],res),scale);
	    if op(TYPECOEFF,res)='t_SERIES' and not type(bigo,'SERIES') then
		bigo:='SERIES'(scale,[],bigo,WHATTYPE(bigo),[],0,'integer',
		    op([LISTCOEFF,1,EXPANVAR],res),bigo) fi;
	    tmp:=subsop(LISTCOEFF = [op([LISTCOEFF,1..lord],res)],
		   COEFFBIGO = bigo,
		   LISTEXPON = [op([LISTEXPON,1..lord],res)],
		   EXPONBIGO = op([LISTEXPON,lord+1],res)   , res )
	fi;
	subsop(LISTCOEFF=map(procname,op(LISTCOEFF,tmp),
	    scale,var[1..-2],ord),tmp)
    fi
end: # `multiseries/run/cleanup`

#------------------------------------------------------------------------------
# This is only used by RUN when it encounters an unevaluated eval

`multiseries/eval`:=proc(f,eqn,scale,var,ord)
option ALGOCOPYRIGHT;
local res;
    if not has(eqn,SCALEVARIABLE) then # does this happen?
	RUN(eval(RUN(f,args[3..-1]),eqn),args[3..-1])
    elif nops(eqn)>1 
	or not type(f,function) 
	or has(op([1,1],eqn),SCALEVARIABLE)
	or not type(op(1,f),function)
	or nops(f)<>2 
	then error "unable to compute series for eval"
    elif op(0,f)='diff' or op(0,f)='Diff' and op([1,1],eqn)=op(2,f) then
	# rely on multiseries/D
	`multiseries/D`(unapply(op(1,f),op(2,f)),[1],[op([1,2],eqn)],args[3..-1])
    else # this is where support should be added for int,RootOf,piecewise...
	res:=RUN(subsop(
	    1=RUN(subs(eqn,op(1,f)),args[3..-1]),
	    2=subs(op([1,1],eqn)=SCALEVARIABLE,op(2,f)),f),args[3..-1]);
	if op(0,f)='diff' or op(0,f)='Diff' then # chain rule
	    MULDOIT(res,POWER(
		RUN('diff'(op([1,2],eqn),SCALEVARIABLE),args[3..-1]),-1,args[3..-1]))
	else res
	fi
    fi
end:

# Input: 
#	f		function
#	difforder	list of indices of variables wrt one differentiates
#	expr		list of arguments
#	scale,var,ord	as usual
# Output:
#	expansion of D[op(difforder)](f)(op(expr))
# Note: When difforder is empty, this is simply doing the 
# composition of f with expr.
`multiseries/D`:=proc(f,difforder,expr,scale,var,ord)
option ALGOCOPYRIGHT;
local L, ser2, argsf, ser1, sc0, v, i, nbargs, vars;
    # some of the following is common with multiseries/analytic
    ## Expand each argument wrt scale and get its limit
    ser2:=map(RUN,expr,args[4..-1]);
    L:=map(LIMIT,ser2,scale,var);
    if member('undefined',L,i) then error "undefined limit for %1",
	`multiseries/Series2Expr`(expr[i],scale) fi;
    nbargs:=nops(ser2);
    ## Normalize so that each of the arguments tends to 0
    for i to nbargs do
	if has(L[i],infinity) then ser1[i]:=POWER(ser2[i],-1,args[4..-1]); argsf[i]:=1/v[i]
	elif L[i]<>0 then ser1[i]:=ADDDOIT(ser2[i],-L[i]); argsf[i]:=L[i]+v[i]
	elif ser2[i]=0 or ISHIDDENZERO(ser2[i],scale,var) then ser1[i]:=0; argsf[i]:=0
        else ser1[i]:=ser2[i]; argsf[i]:=v[i]
	fi
    od;
    ## Create a scales for intermediate computations
    vars:=[seq(v[i],i=1..nbargs)];
    sc0:=map(newscale,vars,0);
    ## Perform the computation recursively on each argument
    `multiseries/D/recurse`(f(seq(argsf[i],i=1..nbargs)),
	difforder,[seq(ser1[i],i=1..nbargs)],sc0,vars,scale,var,ord)
end: # `multiseries/D`

`multiseries/D/recurse`:=proc (f,difforder,ser,sc0,v,scale,var,ord)
option ALGOCOPYRIGHT;
local res, dorder, narg, nbdiff;
    narg:=nops(v);
    dorder:=subs(narg=NULL,difforder);
    nbdiff:=nops(difforder)-nops(dorder);
    if ser[-1]=0 then res:=subs(v[-1]=0,f)
    else
	res:=RUN(f,sc0[-1],[eval(sc0[-1]['variable'],2)],ord+nbdiff);
	res:=diff(res,[v[-1]$nbdiff]);
	res:=COMPOSE(res,ser[-1],args[6..-1])
    fi;
    if narg=1 then res
    else
	`multiseries/D/recurse`(res,dorder,subsop(-1=NULL,ser),
	    subsop(-1=NULL,sc0),subsop(-1=NULL,v),args[6..-1])
    fi
end: # `multiseries/D/recurse`
