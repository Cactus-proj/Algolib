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
###    Title: 	multiseries
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
###
### Name                  : multiseries
### Input                 :
###                          expr   an expression
###                          V      a variable, an equality or a SCALE
###                          ord    the number of coefficients used during the
###                                 computation
###			     varlist a list of variables in the scale (when V
###				    is a scale)
###                          a flag can be specified
###                              if 'exact_order' is specified, the
###                              multiseries expansion should have exactly ord
###                              coefficients
### Output                :
###                          the multiseries expansion of expr w.r.t. x.
### Description: 	     Top-level function.
###                          If ord is not specified, Order is assumed to
###                          be the expansion order
### Implementation        : call `multiseries/doit`
### Environment Variables : Order 
### Side Effects          : The scale may be extended during this operation.
###

multiseries:=proc ( expr :: algebraic , V :: { name,name=algebraic,SCALE })
    try
	    return `multiseries/multiseries`(args)
    catch:
	    error
    finally
	    `multiseries/cleanremember`()
    end try;
end:

`multiseries/multiseries` := proc (expr,V)
option ALGOCOPYRIGHT; 
local scale  :: SCALE, lexpr :: algebraic, order, flag, varlist , ord,
	oldsers, SERs, s, dummy, i, inds:

    if not assigned(_EnvBranchCut) then _EnvBranchCut:=true fi;

    lexpr := expr ;

    if type(V,'SCALE') then
    	scale := V;
    	varlist:=op(select(type,[args],'list'));
    	if varlist=NULL then varlist:=SCALELIST
    	elif remove(member,varlist,SCALELIST)<>[] then
    	    error "invalid argument",varlist
    	fi;
    	# When the variable occurs outside of _var, it should be substituted
    	inds:=indets(lexpr,specindex(anything,_var));
    	lexpr:=subs([seq(inds[i]=dummy[i],i=1..nops(inds))],lexpr);
    	lexpr:=subs(SCALEVARNAME=SCALEBACK,lexpr);
    	lexpr:=subs([seq(dummy[i]=inds[i],i=1..nops(inds))],lexpr)
    else
    	# for efficiency reasons, we don't let series be transformed
    	# into polynomials by subs
    	oldsers:=indets(expr,':-series');
	    SERs:=indets(expr,specfunc(anything,SERIES));
    	if oldsers<>{} then lexpr:=subs([seq(s=freeze(s),s=oldsers)],lexpr) fi;
        if SERs<>{} then lexpr:=subs([seq(s=freeze(s),s=SERs)],lexpr) fi;
    	if type(V,name) then
    	    scale:=newscale(V,0);
    	    lexpr:=subs(V=SCALEBACK,lexpr)
    	else
    	    scale:=newscale(op(V)); # type `=`
    	    lexpr:=subs(op(1,V)=SCALEBACK,lexpr)
    	fi;
    	if oldsers<>{} or SERs<>{} then lexpr:=thaw(lexpr) fi;
    	# since newscale has option remember, SCALELIST might be too large here
    	# varlist:=SCALELIST
    	varlist:=[SCALEVARIABLE]
    fi;
	
    ord:=select(type,[args[3..nargs]],nonnegint);
    if ord=[] then order := Order
    elif nops(ord)=1 then order := op(ord)
    else error "wrong number of integer arguments:",ord
    end if :

    if member(infinity,[args[3..nargs]]) then
	    if ord=[] then order:=infinity
	    else error "wrong number of order arguments:",op(ord),infinity
	    fi
    fi;

    if member('exact_order',[args])
    then flag :='exact_order'
    else flag := NULL
    end if :

    DOIT( lexpr, scale, varlist, order, flag ) :

end proc :                            # multiseries/multiseries
#----------------------------------------------------------------------------

`multiseries/cleanremember`:=proc()
    ## clean up the remember tables of multiseries/doit
    ## do not use forget since
    ## 1) it is not efficient
    ## 2) it suppresses option trace
    subsop(4=NULL,op('`multiseries/doit`'));
    subsop(4=NULL,op('`multiseries/run`'));
    subsop(4=NULL,op('`multiseries/CommonType`'));
    subsop(4=NULL,op('`multiseries/lastchancetestzero`'));
    unassign('`multiseries/possiblezero`')
end:
