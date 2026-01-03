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
###    Title: 	`multiseries/DominantTerm`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###				ser    a SERIES data-structure
###				scale  a SCALE data-structure 
###                             var    a list of ScaleVar data-structure
###                                    this list contains elements in the 
###                                    asymptotic basis w.r.t. which the expr 
###                                    is expanded.
###				rest (optional) a name
### Output                :
###				The leading part of ser encoded by a SERIES 
###                             data-structure, rest is assigned the rest of
###				the series ser, so that ser=result+rest.
### Description           :
###                        This procedure returns the dominant part of a SERIES
###                         data-structure. If the coefficient of this dominant
###			    part coefficient contains an element of
###                         the asymptotic basis that is not in var, the 
###                         coefficient is not reexpanded. It is returned as 
###			    a maple expression.
###			   If ser does not contain enough terms to extract
###			    a dominant term from, then the exact expression is 
###			    reexpanded.
### 

`multiseries/DominantTerm`:=proc(ser, scale, var, rest)::{SERIES,identical(0)}:
option ALGOCOPYRIGHT;
     
local coeff, expon, lcoeffs :: list, nbcoeffs::integer, 
      i :: integer, tocheck, recurse::boolean, expr4coeff, prerest ;

    ASSERT(ser::{'SERIES',identical(0)} and scale::'SCALE' and 
	var::list('ScaleVar') and nargs = 3 or rest::name);

    if ser = 0 then if nargs=4 then rest:=0 end if; return 0 end if;

    recurse  := evalb(op(TYPECOEFF,ser)='t_SERIES');
    lcoeffs  := op(LISTCOEFF,ser) :
    nbcoeffs := nops(lcoeffs)     :

    for i to nbcoeffs do
	tocheck:=lcoeffs[i];
	if recurse then
	    if procname(tocheck,scale,var)<>0 then coeff:=tocheck; break fi
	elif LASTCHANCETESTZERO(tocheck)<>true then coeff := tocheck; break
	end if
    od;

    if i = nbcoeffs+1 then # no dominant term found, reexpand
     return procname(`multiseries/getoneterm`(
	op(EXPR4SERIES,ser),scale,var),args[2..-1])
    end if;

    expon := op([LISTEXPON,i],ser) :

    if type(coeff,'SERIES') 
    then expr4coeff := op(EXPR4SERIES,coeff)
	    else expr4coeff := coeff
    end if:

    if nargs=4 
    then tocheck := op(EXPR4SERIES,ser)-expr4coeff*op(EXPANVAR,ser)^expon :
         if tocheck = 0 
         then rest := 0 
         else prerest:=subsop(LISTCOEFF   = lcoeffs[i+1..nbcoeffs],
		           LISTEXPON   = [op([LISTEXPON,i+1..nbcoeffs],ser)],
		           EXPR4SERIES = tocheck, ser);
	      if op(LISTEXPON,prerest)=[] and op(COEFFBIGO,prerest)=0 and
		op(EXPONBIGO,prerest)=infinity then rest:=0 
	      else rest:=prerest fi
         end if
    end if;

    subsop( LISTCOEFF = [coeff], LISTEXPON = [expon], EXPONBIGO = 'infinity',
	    COEFFBIGO=0, EXPR4SERIES = expr4coeff*op(EXPANVAR,ser)^expon, ser )

end proc :                                         # `multiseries/DominantTerm`
#------------------------------------------------------------------------------
