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
###    Title: 	newlimit
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Description: interface to multiseries for the computation of limits.
### The input is the same as limit, but directional limits are not handled.
###
### Modified to handle directional limits. BS Sep 03.
### This version is keeping the old limit's defaults: by default, newlimit
### is from the right at the origin and from the origin elsewhere, including
### at infinity, where "from the origin" means along the positive real axis. 

limit:=proc (F::{algebraic,`=`}, eq::name=algebraic, 
    dir::{identical(left),identical(right),
	identical(real),identical(complex),identical(fromtheorigin)}:='real', {hint := true})
local s, scale, x, pt, u, f, lim, lim2, res;
global limit; # This is in order to return unevaluated limits in terms of
	      # the "global" limit, so that if the user has done
	      # limit:=MultiSeries:-limit, 
	      # the result he gets has 'limit' in it, not 'MultiSeries:-limit'.
	      # This is important for the code that tests has(result,'limit').

    if type(F,`=`) then return map(procname,F,args[2..-1]) fi;

    # Some of the following code is duplicated in multiseries and in newscale, but
    # I do not see how to avoid this. BS. Sep 03.
    x:=op(1,eq); pt:=op(2,eq);
    # eval is used instead of subs because of cases like diff when x
    # appears both as a  variable and a symbol
    if pt=0 then
	if dir='left' then f:=subsoreval(F,x=-u)
	else f:=subs(x=u,F)
	fi
    elif has(pt,infinity) then f:=subsoreval(F,x=signum(pt)/u)
    elif dir='left' then f:=subsoreval(F,x=pt-u)
    elif dir='right' or dir='real' then f:=subsoreval(F,x=pt+u)
    else f:=subsoreval(F,x=pt*(1-u))
    fi;
    try 
	s:=`multiseries/multiseries`(f,u,1);
	if s=0 then # cannot return 0 now because of functions
	    lim:=0  # that are 0 on the right but not on the left
	else
	    scale:=op(THESCALE,s);
	    lim:=LIMIT(s,scale,scale[list])
	fi;
	if has(pt,infinity) and nargs=2 or dir='right' or dir='left' 
		or dir='fromtheorigin' then
	    res:=lim
	elif dir='complex' then
	    # The following could be considered wrong on branch cuts.
	    # I do not know how to fix this in a simple way. Of course,
	    # one could flag branch cuts everywhere in the code, but I 
	    # wonder whether this is appropriate for complex where people 
	    # might be thinking of their point as being on the Riemann 
	    # surface.
	    # Note that the old limit does
	    #> limit(ln(x),x=-1,complex);   
	    #                             Pi I
	    #> limit(arctan(x),x=infinity,complex);  
	    #                             Pi
	    #                            ----
	    #                             2
	    # which is consistent with the following:
	    if s=0 then res:=0
	    elif nops(scale[list])>1 then res:=undefined
	    else res:=lim fi
	else # real
	    # it is not sufficient to check whether nops(scale[list])=1
	    # because the point might lie on a branch cut.
	    f:=subsoreval(f,u=-u);
	    # inelegant hack for limit(diff(f(u),u),u)
	    if hastype(f,specfunc(anything,eval)) then res:=lim
	    else
		s:=`multiseries/multiseries`(f,u,1);
		if s=0 then lim2:=0
		else
		    scale:=op(THESCALE,s);
		    lim2:=LIMIT(s,scale,scale[list])
		fi;
		if lim2=lim or LASTCHANCETESTZERO(lim2-lim) then res:=lim
		elif type(lim2-lim,'constant') then res:=undefined
		else res:='limit'(args) fi # because it could be defined for some values of the params
	    fi
	fi;
    catch "need to determine the sign of":
        if hint then
    	    userinfo(1,'hints',StringTools:-FormatMessage(lastexception[2..-1]));
    	    return 'limit'(args)
    	else
    	    error
	fi
    catch 
	"multiseries encountered a difficulty in expanding",
	"multivariate analytic functions not handled at infinity":
	if hint then
    	        userinfo(1,'hints',StringTools:-FormatMessage(lastexception[2..-1]))
    	fi;
        ## Commented out at WMI's request.
        ##	limit(args):='limit'(args); # avoid recomputing this
    	return 'limit'(args)
    catch "singularity encountered":
	error;
    catch :
        ## See above.
        ##	limit(args):='limit'(args); # avoid recomputing this
    	return 'limit'(args)
    finally
	    `multiseries/cleanremember`()
    end try;
    res
end: # newlimit

