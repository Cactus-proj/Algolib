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
###    Title: 	`diff/SERIES`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###       * the ops of a SERIES S
###		- a scale			scale       :: SCALE
###		- list of coefficients		lcoeffs     :: list
###		- coefficient of O-term		coeffO      :: algebraic
###		- type of coefficients 	      	typeCoeff   :: type
###		- list of exponents		lexpons     :: list
###		- O-term			exponO      :: algebraic
###		- type of exponents		typeExpon   :: type
###		- expansion variable		expanVar   :: algebraic
###		- the expression being expanded expr4SERIES :: algebraic
###	   * var::name : the variable wrt which the SERIES is differentiated
###
### Output	: multiseries expansion of diff(S,var)


`diff/SERIES`:=proc (scale, lcoeffs, coeffO, typecoeff, lexpons, exponO, typeexpon, expanvar, expr4series, var)
option ALGOCOPYRIGHT;
local diffcoeff, i, nbcoeffs, pow, res, diffO, reslcoeffs, expr,llexpons,llcoeffs;
    if var<>SCALEVARNAME then # differentiating wrt a parameter
    	if not has(expr4series,var) then 0
    	else SERIES(scale, map(diff,lcoeffs,var),coeffO,typecoeff,lexpons,
    	    exponO,typeexpon,expanvar,diff(expr4series,var))
    	fi
    else
    	# coefficient by coefficient
    	nbcoeffs:=nops(lcoeffs);
    	llexpons:=lexpons;
    	llcoeffs:=lcoeffs;
    	if member(0,lexpons,'i') then
    	    llexpons:=subsop(i=NULL,llexpons); 
    	    llcoeffs:=subsop(i=NULL,llcoeffs);
    	    nbcoeffs:=nbcoeffs-1
    	fi;
    	if typecoeff<>'t_SERIES' then
    	    reslcoeffs:=[seq(llcoeffs[i]*llexpons[i],i=1..nbcoeffs)]
    	else
    	    reslcoeffs:=[seq(MULSERIESCST(llcoeffs[i],llexpons[i],typeexpon),
    		i=1..nbcoeffs)]
    	fi;
    	res:=SERIES(scale,reslcoeffs,
    	    coeffO,typecoeff,[seq(llexpons[i]-1,i=1..nbcoeffs)],exponO-1,
    		typeexpon,expanvar,expr4series); # EXPR4SERIES fixed below
    	if res=0 then return 0 fi;
    	# exponent of O() term
    	if not type(exponO,numeric) and exponO<>infinity then
    	    if op(TYPECOEFF,res)<>'t_SERIES' then
    		res:=subsop(COEFFBIGO=exponO*op(COEFFBIGO,res),res)
    	    else res:=applyop(MULSERIESCST,COEFFBIGO,res,exponO,typeexpon)
    	    fi
    	fi;
    	# multiply by the derivative of the expansion variable wrt var
    	res:=MULDOIT(res,
    		`multiseries/diffexprin_var`(expanvar,scale,var,nbcoeffs));
    	# differentiate the coefficients if needed
    	if typecoeff<>'t_SERIES' and has(lcoeffs,SCALELIST) then
    	    diffcoeff:=map(`multiseries/diffexprin_var`,lcoeffs,
    		scale,var,nbcoeffs)
    	else diffcoeff:=map(diff,lcoeffs,var)
    	fi;
    	# differentiate coefficient of O() term
    	if has(coeffO,SCALELIST) then
    	    if typecoeff<>'t_SERIES' then
    		diffO:=SERIES2BIGO(`multiseries/diffexprin_var`(coeffO,
    		    scale,var,nbcoeffs),scale)
    	    else diffO:=SERIES2BIGO(diff(coeffO,var),scale) fi;
    	    res:=ADDDOIT(res,
    		MULDOIT(diffO,'SERIES'(scale,[],1,integer,[],exponO,
    		    typeexpon,expanvar,expanvar^exponO)))
    	fi;
    	# construct sum
    	for i to nbcoeffs do
    	    if diffcoeff[i]<>0 then
    		    pow:='SERIES'(scale,[1],0,integer,[lexpons[i]],infinity,
    			    typeexpon,expanvar,expanvar^lexpons[i]);
        		if type(diffcoeff[i],'SERIES') then
        		    res:=ADDDOIT(res,MULDOIT(diffcoeff[i],pow))
        		else res:=ADDDOIT(res,MULSERIESCST(pow,diffcoeff[i],
        		    WHATTYPE(diffcoeff[i])))
        		fi
        	fi
    	od;
    	# fix EXPR4SERIES
    	if op(LISTEXPON,res)=[] and op(COEFFBIGO,res)=0 and 
    	    op(EXPONBIGO,res)=infinity then 0
    	else
    #	    subsop(EXPR4SERIES=subs(var=SCALEBACK,
    #		diff(`multiseries/Expr4Series2Expr`(expr4series,scale),var)),
    #		res)
    	    # can't use subs in case of multiple diff
    #	    subsop(EXPR4SERIES='diff'(`multiseries/Expr4Series2Expr`(
    	    expr:=`multiseries/Expr4Series2Expr`(expr4series,scale);
    	    if type(expr,specfunc(anything,eval)) and
    	    	type(op(2,expr),set) and
    	    	op([2,1,1],expr)=op([2,1,2],expr) then
    	    	expr:=op(1,expr)
    	    fi;
        	    subsop(EXPR4SERIES=eval(diff(expr,SCALEVARNAME),SCALEVARNAME=SCALEVARIABLE),res)
    	fi
    fi
end: # `diff/SERIES`

`multiseries/diffexprin_var`:=proc (expr,scale,var,ord)
option ALGOCOPYRIGHT;
local fvar, lvar, i2, i1, v, vvar;
    v:=SCALEVARIABLE;
    fvar:=FASTEST([v,op(select(has,indets(expr,indexed),SCALEVARNAME))],scale);
    member(v,SCALELIST,'i1');
    member(fvar,SCALELIST,'i2');
    lvar:=[op(i1..i2,SCALELIST)];
#    RUN(diff(`multiseries/Expr4Series2Expr`(expr,scale),var),scale,lvar,ord)
    RUN(subs([seq(diff(vvar,var)=subs(var=SCALEVARIABLE,diff(op(vvar),var)),vvar=SCALELIST)],diff(expr,var)),
        scale,lvar,ord) 
end: # `multiseries/diffexprin_var`

