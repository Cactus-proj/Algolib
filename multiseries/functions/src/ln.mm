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
###    Title: 	ln
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Description: expansion of the ln of a multiseries. This is also one of 
### the two places (with exp) where the scale can be extended.
#------------------------------------------------------------------------------
### Name                  : `multiseries/LnExtend`
### Input                 :
###				scale	a scale
### Output                : 	nothing
### Description           :
###                         this procedure adds a new element in the asymptotic
###                         basis. Hence, 
### Side Effects          : scale is modified
###

`multiseries/LnExtend` := proc ( scale ) 
option ALGOCOPYRIGHT;
local newT, T;
    ASSERT(scale::'SCALE');
    # prepend ln(T[1]) to scale
    T 		    		:= 		SCALELIST		   ;
    newT	    		:=              newvar(1/ln(1/op(T[1])));
    T				:= 		[newT,op(T)]		   ;
    scale['list']	        := 		T		           ;
    scale['log'][T[2]]		:=		1/newT			   ;
    NULL    
end proc :                                            # `multiseries/LnExtend`
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
### Name                  : `multiseries/function`[ln]
### Input                 :
###                          expr   a SERIES data-structure
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :
###                           the multiseries expansion of ln(expr) in the
###			      (possibly extended) scale associated to expr
### Description           : Use the following
###                        ln(expr) = ln(L) + ln(1+res) with res := (expr-L)/L.
###                        where
###                        ln(1+res) is computed using a composition and
###                        ln(L) is computed recursively.
###			   Plus, when lim(L)<=0, the direction of approach 
###			   to the branch cut is taken into account.

FUNCTIONTABLE[ln]:= proc(expr, scale, var, ord ) :: {SERIES,identical(0)}:
option ALGOCOPYRIGHT;
local Dominant :: SERIES, coeffdom,
      L :: {SERIES,identical(0)}, inires, res,
	i, v::ScaleVar, expo, s::integer, j, im, closed, sgnlead, sgnpb, jumpfactor;

    ASSERT(expr::'SERIES' and scale::'SCALE'
	   and var::list('ScaleVar') and ord :: nonnegint );

    # First compute the dominant term
    # For example if expr is multiseries(ln(x),x)
    # it will be the asympotic element v:=ln(x) considered as a
    # constant w.r.t. x and encoded in a multiseries.
    Dominant := DOMINANTTERM(expr,scale,var,'inires') :

    # we have ln(expr) = ln(Dominant) + ln(1+inires/Dominant)
    # The computation of the second log is done by composition
    if inires=0 then res:=0 # check if inires is not 0
    else # use the usual expansion of log(1+var)
	L := 'SERIES'(scale,[seq((-1)^(s+1)/s,s=1..ord)],(-1)^(ord+2)/(ord+1),
	   'rational',[seq(s,s=1..ord)],ord+1,'integer',#var[-1],ln(1+var[-1])):
	   SCALEVARIABLE,'ln'(1+SCALEVARIABLE));
	res:=MULDOIT(inires,POWER(Dominant,-1,args[2..-1]));
	closed:=op(EXPR4SERIES,res);
	###	This is not necessary any longer. signum now knows about _var
	# We need to help signum, it will be invoked by evalc, that is
	# needed for later branch cut decisions.
	# However, we cannot just tell signum that
	## signum(1+op(EXPR4SERIES,res)):=1;
	# because evalc modifies it before invoking signum.
	### 
	# signum(evalc(1+closed)):=1;
	###
	res := subsop(EXPR4SERIES='ln'(1+closed),COMPOSE(L,res,args[2..-1])) 
    end if :

    # now, we compute ln(Dominant)
    # First, deal with the exponent of its leading variable.
    expo:=op([LISTEXPON,1],Dominant);
    v:=op(EXPANVAR,Dominant);
    if expo<>0 then
	if v=SCALELIST[1] then `multiseries/LnExtend`(scale) fi;
	res:=ADDDOIT(res,MULSERIESCST(RUN(SCALELOG[v],args[2..-1]),
	    -expo,WHATTYPE(expo)))
    fi;

    # Then, deal with the corresponding coefficient
    coeffdom:=op([LISTCOEFF,1],Dominant);
    if type(coeffdom,'SERIES') then
    	if var[-1]<>v then # v can only be faster
    	    ADDDOIT(res,procname(coeffdom,args[2..nargs]))
    	else ADDDOIT(res,procname(coeffdom,scale,var[1..-2],ord)) fi
    else
        if _EnvBranchCut then
        	# careful with the branch cut
        	sgnlead:=SIGN(Dominant);
        	if has(sgnlead,'signum') then # This is our last chance before 
        	    # throwing an error. It catches 
        	    # series(ln(GAMMA((exp(w)*exp(-w)-1)*c0/x+(exp(w)*exp(-w)-1)*c1+c2*x+1)), x, 3)
        	    sgnlead:=simplify(sgnlead)
    	    fi;
        	if sgnlead=-1 or has(sgnlead,'signum') then
        	    ###REALIMPART(expr,'re','im');
        	    ###if im<>0 and SIGN(im)=-1 then res:=ADDDOIT(res,-2*I*Pi) fi
#        	    im := SIGN(Im(expr),true);
        	    im := SIGN(Im(inires),true);
        	    if sgnlead<>-1 then # it has signum
#        	        if im=-1 or has(im,signum) then
#                        sgnpb:=map(op,indets(sgnlead,specfunc(anything,signum)));
#                        error "need to determine the sign of", op(1,sgnpb)
#                    fi
                   if im = -1 or has(im,signum) then # possible jump depending on coeffdom
                       jumpfactor:=ADDDOIT(lncoeffdom(coeffdom,scale,v,var,ord),
                           lncoeffdom(1/coeffdom,scale,v,var,ord));
                       res:=ADDDOIT(res,MULSERIESCST(jumpfactor,(im-1)/2,`if`(im=-1,integer,algebraic)))
                   fi
                else 
        	        if im=-1 then res:=ADDDOIT(res,-2*I*Pi)
        	        elif has(im,signum) then res := ADDDOIT(res,(im-1)*I*Pi)
                    fi
        	    fi
        	fi
	    fi;
	
        ADDDOIT(res,lncoeffdom(coeffdom,scale,v,var,ord))
    fi
end proc :                                                  # `multiseries/ln`
#------------------------------------------------------------------------------

lncoeffdom := proc(coeffdom,scale,v,var,ord)
local i, j, lvar;
	if v=SCALELIST[1] then
	    CONVERT2SERIES(ln(coeffdom),scale,var,false)
	else
	    member(v,SCALELIST,'i');
	    member(var[1],SCALELIST,'j');
	    if j<i then lvar:=SCALELIST[j..i-1]
	    else lvar:=[SCALELIST[i-1]] fi;
	    CONVERT2SERIES(LN(
		    RUN(coeffdom,scale,lvar,1),scale,lvar,ord),scale,var,true)
	fi
end proc: # lncoeffdom
