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
###    Title: 	`multiseries/doit`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###                          expr   an expression
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer (or infinity)
###                          flag   'exact_order' (optional)
###
### Output                :
###                         the multiseries expansion of expr w.r.t. the
###                         variables specified in var at order ord
### Description           :
###			    This is the place where the order and the flag
###			    exact_order are dealt with. This can be done by
###                         increasing the number of coefficients used in the
###                         computation and/or expanding the expr w.r.t. an
###                         element in the asymptotic basis that is not 
###                         specified in var, when necessary.
### Implementation        : 
###                         option remember 
### Error Conditions      : "multiseries encountered a difficulty in expanding
###			    "the expression: %1.\n"
###			    "If this expression is not identically 0, try 
###			    "increasing the order of expansion.\n"
###			    "Otherwise, try adapting Testzero to your needs, "
###			    "see ?Testzero for more information.",
### Global Variables      : the scale may be extended
### Other                 : the remember table is cleaned in multiseries
###
$define NBITER 3
# NBITER is the default number of times that the expansion 
# order is doubled in `multiseries/doit` before the use
# of another variable of expansion.
## 2 is not sufficient for some of the Maple tests.
$define DEFAULTORDER 20
# DEFAULTORDER is used as the initial value of order when Order=infinity.
# This does not mean that order won't be increased till the result is found,
# but this makes it possible to compute series(1/(1-x)+O(x^100),x,infinity)
# when Order is infinity.

`multiseries/doit` := proc ( expr, scale, var, ord, flag) :: algebraic :
option remember, ALGOCOPYRIGHT;
global `multiseries/possiblezero`;
local res      :: { SERIES, algebraic } , maxexpon :: algebraic             ,
      nbcoeff  :: integer,oldnbcoeff    , nbiter   :: integer               ,
      i               , j               ,
      lcoeff   :: table                 , lexpon   :: table		    ,
      lcoeffs  :: list			, lexpons  :: list		    ,
      tmp                 , thetype  :: type                  ,
      lord::nonnegint, minnbcoeff,bigo,gres,closed,enough,tmpres,expbigo;

    ASSERT( expr  :: algebraic and scale :: 'SCALE' and
            var :: list('ScaleVar') and ord :: {nonnegint,identical(infinity)}
	    and (nargs=4 or flag  :: identical('exact_order')));

    if var=[] then return expr
    elif not type(expr,'SERIES') and 
	not has(expr,O) and # avoids expensive useless calls to Testzero
	TESTZERO(expr,scale) then return 0
    end if :

    if ord<>infinity then lord:=ord
    elif Order<>infinity then 
        lord:=max(Order,
            op(map2(op,2,map(op,indets(expr,specfunc(`^`,O))))), # if O(x^k) is in the expression
            op(map2(op,-1,select(has,indets(expr,':-series'),O(1))))) # old series in there
    else lord:= DEFAULTORDER
    fi;

    res      :=    expr   :
    nbcoeff  :=     0     :
    maxexpon := -infinity :
    enough   := false;
    closed   := 0;

    # number of coefficients that must be computed. 
    if nargs=5 or ord=0 # member('exact_order',[args])
    then minnbcoeff := ord
    elif ord<>infinity then minnbcoeff := 1
    else minnbcoeff:=infinity
    end if :

    for nbiter do # first, iterate on the order in intermediate computations

        if type(gres,'SERIES') then 
    		res := op(EXPR4SERIES,gres);
    		if has(res,O) then res:=expr fi # when old series are in the input
	    end if :

        gres := RUN( res, scale, var[-1..-1], lord ) :

	    if gres = 0 then return 0 fi;

    	# Because the scale may have been extended, the expansion can be in 
    	# a faster element of the scale (but not in a slower one).
    	res:=gres;
    	while op(EXPANVAR,res)<>var[-1] and op(LISTEXPON,res)<>[] do
    	    res:=op([LISTCOEFF,1],res) od;

    	if not member(maxexpon,op(LISTEXPON,res),'i') then
    	    i := 0 : thetype := op(TYPECOEFF,res) :
    	fi;
    	if op(LISTEXPON,res)<>[] then maxexpon := op([LISTEXPON,-1],res) fi;

    	# Prevent a possible infinite loop
    	if op(LISTEXPON,res)=[0] and nops(var)>1 and assigned(
    	    `multiseries/possiblezero`[expr,scale,var[1..-2],args[4..-1]]) then
    	    `multiseries/possiblezero`[op([LISTCOEFF,1],res),scale,var[1..-2],
    		args[4..-1]]:=true
    	fi;

    	oldnbcoeff:=nbcoeff;
        for j from i+1 to nops(op(LISTCOEFF,res)) while nbcoeff < ord do
    	    tmp:=procname(op([LISTCOEFF,j],res),scale,var[1..-2],args[4..-1]);
    	    if tmp=0 then next fi;
    	    if type(tmp,'SERIES') then
    		if op(LISTEXPON,tmp)=[] then next fi;
    	    elif nargs=5 and LASTCHANCETESTZERO(tmp) then next  # exact_order
    	    fi;
    	    nbcoeff         := nbcoeff+1               :
    	    lcoeff[nbcoeff] := tmp                  :
    	    lexpon[nbcoeff] := op([LISTEXPON,j],res) :
        end do :
    	lcoeffs:=[seq(lcoeff[i],i=1..nbcoeff)];
    	lexpons:=[seq(lexpon[i],i=1..nbcoeff)];

    	# test if enough coefficients have been computed
    	if nbiter>1 and nbcoeff=oldnbcoeff and has(op(EXPR4SERIES,res),O) then
    	    enough:=true
    	elif op(EXPONBIGO,res)=infinity then
    	    if nbcoeff=0 and ord<>0 then
        	    if gres=res then return 0
        	    else
        	        procname(op(EXPR4SERIES,res),args[2..-1]):=0;
        	        nbiter:=2*NBITER; # so that the coefficient after res is computed
        	    fi
            else enough:=true
            fi
    	elif ord=0 or nbcoeff<>0 then
    	    if nbcoeff >= minnbcoeff then
    		enough:=true
    	    else
    		tmpres := subs(res='SERIES'(scale,lcoeffs,1,thetype,lexpons,
    		    op(EXPONBIGO,res),op(TYPEEXPON,res),op(EXPANVAR,res),
    		    op(EXPR4SERIES,res)),gres);
    		if lcoeffs<>[] and type(lcoeffs[1],'SERIES') then
    		    tmpres:=subsop(TYPECOEFF='t_SERIES',tmpres) fi;
    		closed:=CONVERT2POLYNOM(tmpres);
    		if TESTZERO(`multiseries/Series2Expr`(gres) - closed,scale)then
    		    gres:=subsop(EXPONBIGO=infinity,COEFFBIGO=0,gres);
    		    enough:=true fi
    	    fi
    	fi;

    	if enough then # Construct O-term and return result
    	    if j<=nops(op(LISTCOEFF,res)) then
    		    bigo:=procname(op([LISTCOEFF,j],res),scale,var[1..-2],0);
        		# the exact coeff might have been 0, but there are more terms,
        		# thus it would be wrong to return 0
        		if bigo=0 then bigo:=procname(1,scale,var[1..-2],0) fi;
        		expbigo:=op([LISTEXPON,j],res)
    	    else
        		bigo:=procname(op(COEFFBIGO,res),scale,var[1..-2],0);
        		expbigo:=op(EXPONBIGO,res);
    	    fi;
    	    if bigo<>0 then bigo:=SERIES2BIGO(bigo,scale) fi;
    	    # determine coefficients type
    	    if thetype<>'t_SERIES' then
    		    thetype := COMMONTYPE(map(WHATTYPE,{op(lcoeffs),bigo})) fi;
    	    return subs(res=SERIES(scale,lcoeffs,bigo,thetype,lexpons,
    		    expbigo,op(TYPEEXPON,res),op(EXPANVAR,res),
    		        op(EXPR4SERIES,res)),gres)
        elif nbiter<NBITER or var[-1] = scale['list'][-1] then
    	    if (nbiter=2*NBITER and ord<>infinity)
    		or (nbiter=NBITER-1 and assigned(`multiseries/possiblezero`[args]))
    		then
        		# Last chance before the error. We are ready to spend some
        		# time now recognizing a 0.
        		## It is safe to use "symbolic" here, since the expression
        		## is known to be likely to be 0.
        		if LASTCHANCETESTZERO(
        			`multiseries/Expr4Series2Expr`(expr,scale)-closed,
        			    'symbolic') assuming op(SCALEVARIABLE)::RealRange(Open(0),10^(-100)) # magic constant
        		    then return subsop(EXPONBIGO=infinity,COEFFBIGO=0,gres)
        		fi;
        		error "multiseries encountered a difficulty in expanding "
        		    "the expression: %1.\n"
        		"If this expression is not identically 0, try increasing the "
        		"order of expansion.\n"
        		"Otherwise, try adapting Testzero to your needs, "
        		"see ?Testzero for more information.",
        		`multiseries/Expr4Series2Expr`(expr,scale)-closed
    	    fi;
    	    lord:=max(6,2*lord); # increase order
     	    userinfo(1,`multiseries/doit`,res," is expanded at order",lord);
    	else 
    	    # find the next element in the asymptotic basis
    	    member(var[-1],scale['list'],'j') ;
    	    # j is used in the recursive call below
    	    break
    	end if
    end do :		

    # If increasing the order failed, then use a faster element of the scale
    `multiseries/possiblezero`[args]:=true;
    `multiseries/possiblezero`[op(EXPR4SERIES,gres),args[2..-1]]:=true;
    res:=procname(op(EXPR4SERIES,gres),scale,[op(var),scale['list'][j+1]],
	args[4..-1]);
    # Then truncate the coefficients
    res:=`multiseries/run/cleanup`(res,scale,var,ord);
    # it may be possible to express the result in the original variables
    if res<>0 and op(EXPANVAR,res)<>var[-1] and op(LISTEXPON,res)=[0] and
	op(COEFFBIGO,res)=0 and op(EXPONBIGO,res)=infinity then
	res:=op([LISTCOEFF,1],res)
    fi;
    res   

end proc :                                                 # `multiseries/doit`
#------------------------------------------------------------------------------


