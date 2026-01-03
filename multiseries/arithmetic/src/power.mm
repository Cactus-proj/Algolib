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
###    Title: 	`multiseries/pow`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Description: powering routines. These are
###	`multiseries/pow`	top-level function, any exponent
###	`multiseries/power`	fixed exponent case
###
#------------------------------------------------------------------------------
### Name                  : `multiseries/pow`
### Input                 :
###			     expr   a SERIES data-structure or 0
###			     pwr	
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :
###			     the SERIES data-structure encoding expr^pwr
###

`multiseries/pow`:=proc( expr, pwr, scale, var, ord) :: {SERIES,identical(0)} ;
option ALGOCOPYRIGHT;
local expo;
    ASSERT(expr ::{'SERIES',identical(0)} and scale :: 'SCALE' and
                           var :: list('ScaleVar') and ord :: nonnegint);
    if pwr = 1 then expr 
    elif pwr = 0 then
	'SERIES'(scale,[1],0,'integer',[0],'infinity','integer',var[-1],1) 
    elif pwr = -1 then
	if expr=0 then error "singularity encountered"
	else `multiseries/power`(args) fi
    elif pwr = 2 then 
    	MULDOIT(expr,expr,args[3..-1])
    else 
	if expr=0 then
	    expo:=Re(pwr);
	    if signum(expo)=-1 then error "singularity encountered"
	    else 0 fi
#  The following 3 lines had been commented out by mistake.
#  They are necessary for things like series(x^I+2,x);
        elif has(op(EXPR4SERIES,expr),SCALEVARIABLE) then
	    expo:=Im(pwr);
	    if expo<>0 and type(expo,numeric) # this avoids (1+x)^a splitting a into Re(a)+I*Im(a)
		then MULDOIT(procname(expr,Re(pwr),args[3..-1]),
			EXP(MUL([I,expo,LN(expr,args[3..-1])]),args[3..-1]))
		     # the above computation should be done without any call to run
	    else `multiseries/power`(args)
	    fi
        else `multiseries/power`(args)
        end if
    end if

end proc :                                                  # `multiseries/pow`
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
### Name                  : `multiseries/power`
### Input                 : 
###                          expr   a SERIES data-structure
###                          pwr	
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :  
###                           the SERIES data-structure encoding expr^pwr
### Description           :
###
### if expr is a multiseries that encodes a constant, 
### we consider the coefficient directly.
###
### We compute a multiseries expansion of expr^pwr as follow:
### first, we determine U s.t. T(1+U) is a multiseries expansion
### of expr. Then, we compute recursively T^pwr and we use the
### classical composition to compute a multiseries expansion 
### of (1+U)^pwr.
###
###

`multiseries/power` := proc( expr, pwr, scale, var, ord, onbranchcut):: {SERIES,identical(0)} :
option ALGOCOPYRIGHT;
local   lcoeff, lexpon, res, i, k, s, dom, brcut, im, nbcoeffs, bigo;

    ASSERT( expr :: 'SERIES' and pwr :: algebraic and scale :: 'SCALE' and
                           var :: list('ScaleVar') and ord :: nonnegint );

    # if expr encodes exactly a monomial wrt its fastest var---------------

    if nops(op(LISTCOEFF,expr))=1 and op(EXPONBIGO,expr)=infinity
       and op(COEFFBIGO,expr)=0 then
    	if op(TYPECOEFF,expr)='t_SERIES' then
    	    s := procname(op([LISTCOEFF,1],expr),args[2..-1]) ;
            i := 't_SERIES'; 
        elif type(op(TYPECOEFF,expr)[`^`],procedure) then
	        s := op(TYPECOEFF,expr)[`^`](op([LISTCOEFF,1],expr),pwr) :
            i := WHATTYPE(s) 
	    else
	        s := op([LISTCOEFF,1],expr)^pwr :
            i := WHATTYPE(s) 
        end if :
    
	    if nargs=6 then onbranchcut:=evalb(signum(op([LISTCOEFF,1],expr))=-1)
	    fi;

	    if op(LISTEXPON,expr)<>[0] then
	        subsop( LISTCOEFF = [s], TYPECOEFF = i,
		        TYPEEXPON = COMMONTYPE({ WHATTYPE(pwr),op(TYPEEXPON,expr) }),
                LISTEXPON   = [pwr*op([LISTEXPON,1],expr)],
                EXPR4SERIES = op(EXPR4SERIES,expr)^pwr,  expr	) 
	    else
	        subsop( LISTCOEFF = [s], TYPECOEFF = i,
                 EXPR4SERIES = op(EXPR4SERIES,expr)^pwr,  expr	) 
	    fi

    elif op(LISTCOEFF,expr)=[] and
	    COMPARISONFUNCTION(COMMONTYPE({op(TYPEEXPON,expr),
	    WHATTYPE(pwr)}))(0,pwr) then

    	# expr is a O()-term  ------------------------------------------------
    	lexpon:=op(EXPONBIGO,expr)*pwr;
    	lcoeff:=op(COEFFBIGO,expr);
    	if type(lcoeff,'SERIES') then
	        lcoeff:=POWER(lcoeff,args[2..-1])
	    else lcoeff:=lcoeff^pwr
	    fi;
	    subsop(EXPONBIGO=lexpon,COEFFBIGO=lcoeff,
	        EXPR4SERIES=op(EXPR4SERIES,expr)^pwr,expr)
    else # otherwise --------------------------------------------------------- 

        # First, take the dominant term T of the multiseries
        try
            dom := DOMINANTTERM(expr,scale,var,'res') 
        catch "unable to compute series":
            # this may happen because a O() term was in the input
            if signum(pwr)=1 then
                bigo:=op(COEFFBIGO,expr);
                if op(TYPECOEFF,expr)='t_SERIES' then
                    bigo:=procname(bigo,args[2..-1])
                else bigo:=bigo^pwr
                fi;
                return subsop( EXPR4SERIES=op(EXPR4SERIES,expr)^pwr,
                    LISTCOEFF=[],LISTEXPON=[],COEFFBIGO=bigo,
                    EXPONBIGO=op(EXPONBIGO,expr)*pwr,expr)
            else
                error "unable to compute series"
            fi
        end try;

	    # DOMINANTTERM can sometimes recognize 0.
	    if dom = 0 then return 0^pwr fi;

        # Hence, expr = T + res. Compute res/T
        res := MULDOIT(res,POWER(dom,-1,args[3..-1])) :

        # Now, we compute the multiseries associated to (1+res/T)^pwr
        s[0] := 1 : s[1] := pwr : 
	    if type(pwr,nonnegint) and pwr<ord+1 then nbcoeffs:=pwr
	    else nbcoeffs:=ord+1 fi;
        for k from 2 to nbcoeffs do s[k]:=(pwr-(k-1))*s[k-1]/k od;

    	if nbcoeffs>ord then
    	    s:='SERIES'(scale,[seq(s[k],k=0..ord)],s[ord+1],WHATTYPE(pwr),
    		[$0..ord],ord+1,'integer',SCALEVARIABLE,
    		(1+SCALEVARIABLE)^pwr)
    	else
    	    s:='SERIES'(scale,[seq(s[k],k=0..nbcoeffs)],0,integer,
    		[$0..nbcoeffs],infinity,'integer',SCALEVARIABLE,
    		(1+SCALEVARIABLE)^pwr)
    	fi;

        res := COMPOSE(s,res,scale,var,ord) :

        # output --------------------------------------------------------------
        # Last, we compute the product T^pwr*(1+res/T)^pwr
   
    	# careful with the branch cut:
    	if type(pwr,integer) or nargs=6 or not _EnvBranchCut then dom:=POWER(dom,pwr,args[3..-1])
    	else
    	    dom:=POWER(dom,pwr,args[3..-1],'brcut');
    	    if brcut then 
    		#REALIMPART(expr,'re','im');
    		#im:=DOMINANTTERM(im,scale,var);
    		#while type(im,'SERIES') do im:=op([LISTCOEFF,1],im) od;
    		#if im<>0 and signum(im)=-1 then
    		#    dom:=MULDOIT(dom,exp(-2*I*Pi*pwr)) fi
    		im:=SIGN(Im(expr),true);
    		if im<>0 and im=-1 then dom:=MULDOIT(dom,exp(-2*I*Pi*pwr))
    		elif type(im,specfunc(anything,signum)) or
    		     type(-im,specfunc(anything,signum)) then
    		    # error "need to determine %1",im
    		    dom:=MULDOIT(dom,exp((im-1)*I*Pi*pwr))
    		fi
    	    fi
    	fi;

        res:=subsop( EXPR4SERIES=op(EXPR4SERIES,expr)^pwr,
                MULDOIT(res,dom))	


    end if 
			     
end proc :                                                # `multiseries/power`
#------------------------------------------------------------------------------

