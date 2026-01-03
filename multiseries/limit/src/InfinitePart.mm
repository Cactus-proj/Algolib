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
###    Title: 	`multiseries/InfinitePart`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###			     expr   a SERIES
###                          scale  a SCALE 
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which expr 
###                                 is expanded.
###                          ConstantPart a name
###                          zeroPart     a name
###
### Output                :
###                          a list of 3 expressions
###                              - the part of the expansion expr which 
###                                tends to infinity;
###                              - its constant part (wrt var[-1]);
###                              - the part which tend to 0.
### Description           : when necessary, the expression is re-expanded.
###

`multiseries/InfinitePart` := proc( input, scale, var):: list :
option ALGOCOPYRIGHT;
local comp, newser, ord, i, lexp, lcoeffs, v, constantpart, zeropart, infinitepart, expr4res, j, lvar;
    
    ASSERT( input :: 'SERIES' and scale :: 'SCALE' and var::list('ScaleVar'));

    comp := COMPARISONFUNCTION(op(TYPEEXPON,input)) :

    newser:=input;
    lvar:=var;
    v:=op(EXPANVAR,input);
    # make the O() term small enough by reexpanding if necessary
    for ord from nops(op(LISTEXPON,input))+1 
	while comp(op(EXPONBIGO,newser),0) 
#	    not comp(0,op(EXPONBIGO,newser)) do
	    or ((op(EXPONBIGO,newser)=0 or LASTCHANCETESTZERO(op(EXPONBIGO,newser)))
	    	and not has(op(EXPR4SERIES,newser),O)) do
	if v<>lvar[-1] and v=FASTEST([lvar[-1],v],scale) then
	    member(v,SCALELIST,'j'); lvar:=SCALELIST[1..j] fi;
	newser:=RUN(op(EXPR4SERIES,input),scale,lvar,ord);
	if newser=0 then return [0,0,0] fi; # Hidden zero.
	v:=op(EXPANVAR,newser)
    od;
    if not has(op(EXPR4SERIES,newser),O)
    	and not comp(0,op(EXPONBIGO,newser)) then 
	error "need to determine the sign of %1",op(EXPONBIGO,newser) fi;
    # At this stage the O() term has exponent larger than 0.
    lexp:=op(LISTEXPON,newser);
    lcoeffs:=op(LISTCOEFF,newser);
    i:=`multiseries/locate`(lexp,0,comp); # infinite from 1 to i.
    if op(TYPECOEFF,newser)<>'t_SERIES' then 
	 expr4res:=add(lcoeffs[j]*v^lexp[j],j=1..i)
    else expr4res:=add(op(EXPR4SERIES,lcoeffs[j])*v^lexp[j],j=1..i)
    fi;
    # might not be a legitimate SERIES: could encode 0; but that's ok
    infinitepart:=subsop(LISTCOEFF=lcoeffs[1..i],LISTEXPON=lexp[1..i],
	    COEFFBIGO=0,EXPONBIGO=infinity,EXPR4SERIES=expr4res,newser);

    if i=nops(lexp) or comp(0,lexp[i+1]) then
	constantpart:=0;
	zeropart:=subsop(LISTCOEFF=lcoeffs[i+1..-1],
	    LISTEXPON=lexp[i+1..-1],
	    EXPR4SERIES=op(EXPR4SERIES,newser)-expr4res,newser)
    else 
	constantpart:=lcoeffs[i+1];
	zeropart:=subsop(LISTCOEFF=lcoeffs[i+2..-1],
	    LISTEXPON=lexp[i+2..-1],
    	    EXPR4SERIES=op(EXPR4SERIES,newser)-expr4res-
	    `if`(type(constantpart,'SERIES'),op(EXPR4SERIES,constantpart),
		constantpart),newser)
    fi;

    # fix infinitepart
    if infinitepart<>0 and op(LISTEXPON,infinitepart)=[] then
	infinitepart:=0 fi;
    # fix zeropart
    if type(zeropart,'SERIES') and op(LISTEXPON,zeropart)=[] then
	if op(COEFFBIGO,zeropart)=0 and op(EXPONBIGO,zeropart)=infinity then 
	    zeropart:=0
	elif op(TYPECOEFF,zeropart)='t_SERIES' and 
	    not type(op(COEFFBIGO,zeropart),'SERIES') then
	    zeropart:=subsop(TYPECOEFF=
		WHATTYPE(op(COEFFBIGO,zeropart)),zeropart)
	fi
    fi;
    
    [infinitepart,constantpart,zeropart];
    
end proc :                                         # `multiseries/InfinitePart`
#------------------------------------------------------------------------------
 
