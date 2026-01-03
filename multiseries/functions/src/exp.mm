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
###    Title: 	exp
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Description: computation of the exponential of a multiseries
### This is also one of the two places (with ln) where the scale can be
### extended.
#------------------------------------------------------------------------------
### Name                  : `multiseries/ExpExtend`
### Input                 :
###				lterm
###				scale	the corresponding scale
###                             i       a nonnegative integer that represents
###                                     the position of the new variable
###                                     in the scale
###                             sgn     a sgn
###                             L       a limit (see the code of 
###                                     `multiseries/exp`)
### Output                :
###				the function introduced in the basis of
###				the scale (see type ScaleVar in scale.mpl)
### Side Effects          : scale is modified
###

`multiseries/ExpExtend` := proc ( lterm, scale, i, sgn, L) :: ScaleVar :
option ALGOCOPYRIGHT;
local newVar :: ScaleVar, transl, basis :: list(ScaleVar), j;
    ASSERT(scale::'SCALE' and i::nonnegint);
    basis	       := 		SCALELIST			;

    transl:=[seq(j=op(1,j),j=basis)];

    if type(sgn*lterm,`+`) then
	transl:=1/mul(exp(subs(transl,j)),j=sgn*lterm)
    else transl:=1/exp(normal(subs(transl,sgn*lterm)))
    fi;

    ## strange evaluation in Maple. We need one more evaluation because
    ## scale['list'] is more or less global and otherwise we cannot
    ## ensure that member(newVar,scale['list']) will return true.
    
#    newVar             :=   eval(subsop(3=transl,basis[1]));
    newVar             :=   newvar(transl);
    scale[log][newVar] :=   sgn*lterm			;

    if L<>0
    then basis := [op(1..i-1,basis),newVar,op(i..nops(basis),basis)]   
    else basis := [op(1..i-2,basis),newVar,op(i-1..nops(basis),basis)] 
    end if ;

    scale['list'] := basis :

    newVar

end proc:	                                      # `multiseries/ExpExtend`
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
### Name                  : `multiseries/function`[exp]
### Input                 :
###			     expr   a SERIES data-structure
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                : 	
###                             the expansion of exp(expr) in the
###				(possibly extended) scale associated to expr
### Description           :
### References            : Richardson, Salvy, Shackell, van der Hoeven,
###			    ISSAC'96.
###

FUNCTIONTABLE['exp'] := proc(expr, scale, var, ord) :: SERIES:
option ALGOCOPYRIGHT;

local 	newexpr, s, i, ConstantPart, 
        sgn, # signum can return unevaluated ##:: {identical(1),identical(-1)},
        L :: algebraic , G :: SERIES, RealPart,
	ImPart , zeroPart :: {SERIES,identical(0)}, 
	lvar :: list(ScaleVar),
	expon, indssgn;

    ASSERT( expr::{'SERIES',identical(0)} and scale :: 'SCALE' and
                 var :: list('ScaleVar') and ord :: nonnegint);

    G := 'SERIES'(scale,[1],0,'integer',[0],'infinity','integer',var[-1],1) :

    if expr = 0 then return G end if :

    s := INFINITEPART(expr,args[2..-1]) : # Infinite part returns a list
    ConstantPart := s[2] :
    zeroPart     := s[3] :
    REALIMPART(s[1],'RealPart','ImPart') :

    L := LIMIT(RealPart,scale,var) :

    while has(L,infinity) do

	sgn:=signum(L);
	if not member(sgn,[-1,1]) then 
	   # Give it a last chance:
	   expon:=op([LISTCOEFF,1],RealPart);
	   if not has(expon,SCALELIST) then
		RealPart:=MULSERIESCST(RealPart,1/expon,'algebraic');
		L := LIMIT(RealPart,scale,var);
		sgn:=signum(L)
	   fi
	else expon:=1
	fi;
	if not member(sgn,[-1,1]) then
	    indssgn:=indets(sgn,specfunc(anything,signum));
	    if indssgn<>{} then
	        error "need to determine the sign of %1",op([1,1],indssgn)
        else
	        error "need to determine %1",sgn 
        fi
	fi;

        # Check whether the real part of expr is asymptotic to the
        # logarithm of an element in the asymptotic basis. 
        # The asymptotics of the log of the slowest element in the basis
        # (scale['list'][1]) cannot be expressed w.r.t. the basis.

        for i from 2 to nops(SCALELIST) while L=infinity or L=-infinity do
	    # limit(RealPart/scale[log][scale['list'][i]])
            L := LIMIT(MULDOIT(RealPart,POWER(RUN(eval(
		scale[log][scale['list'][i]],1),
                                              args[2..-1]),-1,args[2..-1])),
	               scale,var)
        end do :

        if L=0 or L=infinity or L=-infinity and i=nops(SCALELIST)+1
        then # a new exponential has to be introduced in the scale
             # EXPSCALE (`multiseries/ExpExtend`) returns the function 
             # introduced in the basis of the scale
             # G := G*newvar
             G := MUL([POWER(RUN(EXPSCALE(op(EXPR4SERIES,RealPart),
		                              scale,i,sgn,L),
		                 args[2..-1]),-sgn*expon,args[2..-1]),G]);
	     RealPart:=0
        else # iterate
	
             #G := G*SCALELIST[i-1]^(-L)
              G := MUL([POWER(RUN(SCALELIST[i-1],args[2..-1]),
	                      -L*expon,args[2..-1]),G]);

             RealPart := ADD([MULDOIT(RUN(eval(scale[log][scale['list'][i-1]],1),
	                      args[2..-1]),-L),RealPart]) :
	     if expon<>1 and RealPart<>0 then
	     	RealPart:=MULSERIESCST(RealPart,expon,'algebraic') fi
        end if:
        L := LIMIT(RealPart,scale,var)
    end do:

    if L=undefined then error "unable to compute multiseries" fi;

    # handle the infinite imaginary part 
    if ImPart<>0 
    then # insert an oscillating coefficient	     
	 G := MULDOIT(G,'SERIES'(scale,[exp(I*op(EXPR4SERIES,ImPart))],
				    0,'algebraic',[0],'infinity','integer',
			  op(EXPANVAR,ImPart),exp(I*op(EXPR4SERIES,ImPart))))
    end if : 

    # handle the "constant" part recursively
    if type(ConstantPart,'SERIES') or var[1]<>SCALELIST[1] then
	if var[-1]=FASTEST([op(EXPANVAR,expr),var[-1]],scale) then
	    lvar:=var[1..-2]
	else lvar:=var fi;
	if lvar=[] then
	    member(var[-1],SCALELIST,'i');
	    if i>1 then lvar:=[SCALELIST[i-1]]
	    else lvar:=[SCALELIST[1]] fi;# ex. sin(1/x+1/exp(x)),x=infinity,0
	    ConstantPart:=RUN(ConstantPart,scale,lvar,1)
	fi;
	ConstantPart:=procname(ConstantPart,scale,lvar,ord);
	if nops(var)=1 then # rewrite in the desired variables
	    ConstantPart:=CONVERT2SERIES(ConstantPart,scale,var,true)
	fi
    else ConstantPart:=exp(ConstantPart) fi;

    if zeroPart = 0 then newexpr := 1 
    else
	L:='SERIES'(scale,[seq(1/(i!),i=0..ord)],1,'rational',[$0..ord],ord+1,
	    'integer',SCALEVARIABLE,exp(SCALEVARIABLE));
	newexpr := subsop( EXPR4SERIES = exp(op(EXPR4SERIES,zeroPart)),
	                    COMPOSE(L,zeroPart,args[2..-1])            ) 
    fi;
	      
    MUL([ConstantPart,G,newexpr])
end proc:                        # `multiseries/function`[exp]
#------------------------------------------------------------------------------


