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

##    -*-Maple-*-
##
##    Title: 	`multiseries/function`[floor,ceil,frac,round,trunc]
##    Created:	Tue Oct 28 10:32:19 2003
##    Author: 	Bruno Salvy and Ha Le
##		<Bruno.Salvy@inria.fr> <Ha.Le@inria.fr>
##
## Description: compute floor(series).

FUNCTIONTABLE['floor']:=proc( expr,scale,var,ord ) :: {identical(0),SERIES};
option ALGOCOPYRIGHT;
   local bigO, typeexpon, comp, i1, res, k, co, v, sgn, expr4res, j;

   ASSERT( expr::{'identical'(0),'SERIES',list({identical(0),'SERIES'})} and 
           scale :: 'SCALE' and
           var::list('ScaleVar') and ord::nonnegint);

   if expr = 0 then return 0 end if;

   if type(expr,'SERIES') then   

	bigO := op(EXPONBIGO,expr);
	if bigO <= 0 then # all terms tend to infinity
	    return applyop(floor,EXPR4SERIES,expr) fi;

	# Compute the infinite part of expr
	typeexpon := op(TYPEEXPON,expr);
	comp := COMPARISONFUNCTION(typeexpon);
	i1 := LOCATE(op(LISTEXPON,expr),0,comp); 
	if i1<>0 then
	    if member(0,op(LISTEXPON,expr)) then i1:=i1+1 fi;
	    v:=op(EXPANVAR,expr);
	    if op(TYPECOEFF,expr)<>'t_SERIES' then 
		 expr4res:=add(op([LISTCOEFF,j],expr)
		    *v^op([LISTEXPON,j],expr),j=1..i1)
	    else
		expr4res:=add(op([LISTCOEFF,j,EXPR4SERIES],expr)
		    *v^op([LISTEXPON,j],expr),j=1..i1)
	    fi;
	    return ADDDOIT(
		subsop(LISTCOEFF=[op([LISTCOEFF,1..i1],expr)],
			LISTEXPON=[op([LISTEXPON,1..i1],expr)],
		    COEFFBIGO=0,EXPONBIGO=infinity,
		    EXPR4SERIES=expr4res,expr),
		'SERIES'(scale,[-'frac'(expr4res)],0,algebraic,[0],
		    infinity,integer,v,-'frac'(expr4res)))
	fi;
	# no infinite part.
	res:=0;
	if member(0,op(LISTEXPON,expr),'k') then # Constant part
	    co := op([LISTCOEFF,k],expr);
	    if op(TYPECOEFF,expr) = 't_SERIES' then
		return CONVERT2SERIES(procname(co,args[2..-1]),scale,var,true)
	    fi;
	    res := floor(co);
	    if res<>co then return CONVERT2SERIES(res,scale,var,true) fi;
	end if;
	# need sign of the zero part:
	if op(TYPECOEFF,expr)='t_SERIES' then
	    sgn:=SIGN(op([LISTCOEFF,i1+1],expr))
	else sgn:=signum(op([LISTCOEFF,i1+1],expr)) fi;
	if sgn=-1 or sgn=0 then res:=res-1 fi;
	CONVERT2SERIES(res,scale,var,true)
    elif type(expr,list) and nops(expr)=2 then
	error "not implemented yet"

    else error "wrong arguments to the floor function"

   end if;
end proc:
#------------------------------------------------------------------------------


#FUNCTIONTABLE['ceil']:=proc( expr,scale,var,ord ) :: {identical(0),SERIES};
#end:
