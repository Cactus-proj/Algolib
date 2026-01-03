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
###    Title: 	`multiseries/limit`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###			     ser     a  SERIES data-structure or 0
###			     scale   a  scale
###                          var     a  list of ScaleVar data-structure
### Output                :  
###                          the limit of ser
### Description           :
###                          if the dominant part of ser encodes a bigOterm
###			     that does not tend to 0,
###                          we compute another multiseries expansion.
###
###                          otherwise, we check (recursively) the exponent 
###                          of the dominant term and, if it is zero, we
###                          analyse its coefficient.
###
###                          if the coefficient is a maple expression that
###                          contains some elements of the asymptotic basis,
###                          we compute a multiseries expansion of this 
###                          coefficient
###
###

`multiseries/limit` := proc ( ser, scale, var )
option ALGOCOPYRIGHT;
local comp :: procedure, i, rec, lead, simpler:

    ASSERT(ser::{'SERIES',identical(0)} and scale::'SCALE' and 
	var::list('ScaleVar'));

    if ser = 0 then return 0 end if :
        
    if not assigned(_EnvBranchCut) then _EnvBranchCut:=true fi;
     
    comp  := COMPARISONFUNCTION(op(TYPEEXPON,ser)) :

    if op(LISTCOEFF,ser)=[] then
    	if comp(0,op(EXPONBIGO,ser)) then return 0
    	elif comp(op(EXPONBIGO,ser),0) or 
    	    op(TYPECOEFF,ser)<>'t_SERIES' and 
    		(op(EXPONBIGO,ser)=0 or LASTCHANCETESTZERO(op(EXPONBIGO,ser))) then 
    	    return procname(`multiseries/getoneterm`(
    		op(EXPR4SERIES,ser),scale,[var[-1]]),args[2..-1])
    	elif (op(EXPONBIGO,ser)=0 or LASTCHANCETESTZERO(op(EXPONBIGO,ser))) then 
    	    lead:=procname(op(COEFFBIGO,ser),args[2..-1]);
    	    if lead=0 or has(lead,infinity) then return lead
    	    else
    		return procname(`multiseries/getoneterm`(
    		    op(EXPR4SERIES,ser),scale,[var[-1]]),args[2..-1])
    	    fi
    	else error "need to determine the sign of %1",op(EXPONBIGO,ser)
	fi
    elif comp(0,op([LISTEXPON,1],ser)) then 0
    # we assume that op([LISTCOEFF,1],ser)<>0 (Testzero is used in DOIT)
    elif comp(op([LISTEXPON,1],ser),0) then # recurse to get sign 
    	# It is not sufficient to use
    	#    rec:=SIGN(ser);
    	# since during the computation of this sign, it may happen that
    	# the first coefficient is recognized to be 0, and then the correct
    	# sign of the rest will be incorrectly multiplied by infinity.
    	lead:=LEADTERM(ser);
    	if lead=0 then return 0 fi;
    	if not comp(op([LISTEXPON,1],lead),0) then 
    	    return procname(lead,scale,var) fi;
    	rec:=SIGN(lead);
    	if rec=0 then return 0 fi; # should not happen anymore
    	###if has(rec,SCALELIST) then undefined
        if has(rec,SCALELIST) or has(rec,SCALEVARNAME) then undefined
    	else rec*infinity
    	fi
    elif op([LISTEXPON,1],ser)=0 or LASTCHANCETESTZERO(op([LISTEXPON,1],ser)) then
    	# op([LISTEXPON,1],ser) is equal to 0
    	if op(TYPECOEFF,ser)='t_SERIES' then
    	    procname(op([LISTCOEFF,1],ser),args[2..-1]) 
    	elif has(op([LISTCOEFF,1],ser),SCALELIST) then
    	    # Check that it's really there: I do not see how to avoid simplify
    	    # here to get limit(GAMMA(a+x)/GAMMA(x)*exp(-a*log(x)),x=infinity)
    	    simpler:=simplify(op([LISTCOEFF,1],ser));
    	    if has(simpler,SCALEVARNAME) then # did not save anything
    		    member(op(EXPANVAR,ser),SCALELIST,'i') :		    
    	    	if i=1 then 'undefined' # oscillatory case
    	    	else procname(`multiseries/getoneterm`(op([LISTCOEFF,1],ser),
    			    scale,SCALELIST[1..i-1]),args[2..-1])
    		    fi
    	    elif TESTZERO(simpler) then 0
    	    else simpler
    	    fi
    	elif TESTZERO(op([LISTCOEFF,1],ser),scale) then 0
    	else op([LISTCOEFF,1],ser)
    	fi
    else error "need to determine the sign of %1", op([LISTEXPON,1],ser)
    end if 
     
end proc :                                                # `multiseries/limit`
#----------------------------------------------------------------------------

