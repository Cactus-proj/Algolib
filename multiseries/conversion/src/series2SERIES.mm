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
###    Title: 	`multiseries/series2SERIES`
###    Created:	Aug 200
###    Author: 	Bruno Salvy
###    Contact: Bruno.Salvy@inria.fr
###
### Input                 :
###			     s	    a maple "old" series
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
### Output                :
###                          the series in the SERIES format
###				(but no nice closed form, obviously)
###
### Description           :  
### Error Conditions      : None
###

series2SERIES:=proc(expr,scale,var,ord)
local i, listcoeffs, listexpons, coefbigo, exponbigo, maxind, typecoeffs, closedform, varres;
#    if op(0,expr)=SCALEVARIABLE or op(0,expr)=SCALEVARNAME then # Get the basic case fast:
     if member(op(0,expr),SCALELIST) or op(0,expr)=SCALEVARNAME-SCALEPOINT then
    	if op(-2,expr)=O(1) then
	        coefbigo:=1;
	        exponbigo:=op(-1,expr);
	        maxind:=iquo(nops(expr),2)-1;
	    else
	        coefbigo:=0;
	        exponbigo:=infinity;
	        maxind:=iquo(nops(expr),2);
	    fi;
	    closedform:=convert(expr,polynom)+
			`if`(exponbigo<>infinity,O(op(0,expr)^exponbigo),0);
	    listcoeffs:=[seq(op(2*i-1,expr),i=1..maxind)];
	    if maxind=0 then typecoeffs:='integer'
	    else typecoeffs:=COMMONTYPE(map(WHATTYPE,{op(listcoeffs)}))
        fi;
        listexpons:=[seq(op(2*i,expr),i=1..maxind)];
        if op(0,expr)=SCALEVARNAME-SCALEPOINT then
            varres:=SCALEVARIABLE;
            closedform:=subs(SCALEVARNAME=SCALEBACK,closedform);
            if has(listcoeffs,SCALEVARNAME) then return RUN(closedform,args[2..-1]) fi;
                if SCALEPOINT<>0 then
                    listcoeffs:=[seq(listcoeffs[i]*(-SCALEPOINT)^listexpons[i],i=1..maxind)]
                fi
            else 
                varres:=op(0,expr);
            	    if has(listcoeffs,SCALEVARNAME) then
            	        listcoeffs:=map(RUN,listcoeffs,scale,var,ord);
            	        typecoeffs:=t_SERIES;
                fi
            fi;
            	CONVERT2SERIES('SERIES'(scale,listcoeffs,coefbigo,
    	    typecoeffs,
    		listexpons,exponbigo,'integer',varres,
    		closedform),scale,var,true)
	# added type(op(0,expr),name) to catch series(z^2+z^3,z^2+z^3) that was sent
	# by combstruct[gfsolve]
	elif not type(op(0,expr),linear) then
		procname(MultiSeries:-oldseries(expr,SCALEVARNAME=SCALEPOINT,ord),args[2..-1])
    elif not has(expr,{SCALEVARIABLE,SCALEVARNAME}) then
    	CONVERT2SERIES(expr,scale,var,true)
    elif has (expr,SCALEVARIABLE) then # it's somewhere in the coefficients
	    RUN(add(op(2*i-1,expr)*op(0,expr)^op(2*i,expr),
    	    i=1..iquo(nops(expr),2)),args[2..-1])
    else
	    RUN(subs(SCALEVARNAME=SCALEBACK,add(op(2*i-1,expr)*op(0,expr)^op(2*i,expr),
    	    i=1..iquo(nops(expr),2))),args[2..-1])
    fi
end: