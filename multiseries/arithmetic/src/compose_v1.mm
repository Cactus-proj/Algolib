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
###    Title: 	`multiseries/compose`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###			     ser1   a SERIES data-structure
###			     ser2   a SERIES data-structure or 0
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :
###				the multiseries expansion of ser1(ser2)
###				at order ord w.r.t. to the scale associated
###				to ser2.
### Description           :
###                          It is assumed that ser2's limit is zero 
###                          and that ser1 represents a plain power series 
###			     with positive (possibly non-rational) exponents 
###
###                          this procedure could take 0 or a big O term 
###                          (encoded as a SERIES data structure) as input.
### References            :
### Todo: the management of the O() term should be more efficient.

`multiseries/compose`:=proc(ser1,ser2,scale,var,ord)::{SERIES,identical(0)} :
option ALGOCOPYRIGHT;
local res :: {SERIES,identical(0)}, bigo, scale1, genpt, actpt, fcn, genser1, v, rest, first, v0, sc0;
    ASSERT( ser1 :: {'SERIES',identical(0)}
        and ser2 :: {'SERIES',identical(0)} and
            scale :: 'SCALE' and var :: list('ScaleVar') and ord :: nonnegint);
    if ser1=0 then return 0 fi;
    scale1:=op(THESCALE,ser1);
    if ser2=0 then return CONVERT2SERIES(LIMIT(ser1,scale1,var),scale,var,true)
    elif op(EXPR4SERIES,ser2)=SCALEVARIABLE and scale1=scale then
    	return ser1 # composition with identity
    elif op(LISTEXPON,ser2)<>[] and op([LISTEXPON,1],ser2)=0 then
	# This is the place where f(x+small) is replaced when needed by
	# f(x)+D(f)(x)*small+...
	# This is only invoked when f behaves nicely (i.e. its growth
	# is at most in scale[scalevar]).
	v:=op(EXPANVAR,ser1);
	fcn:=op(EXPR4SERIES,ser1);
	actpt:=op([LISTCOEFF,1],ser2);
	if op(TYPECOEFF,ser2)='t_SERIES' then actpt:=op(EXPR4SERIES,actpt) fi;
	rest:=subsop([LISTCOEFF,1]=NULL,[LISTEXPON,1]=NULL,
		    EXPR4SERIES=op(EXPR4SERIES,ser2)-actpt,ser2);
	if op(LISTEXPON,rest)=[] and op(COEFFBIGO,rest)=0 and 
	    op(EXPONBIGO,rest)=infinity then rest:=0 fi;
	if op(TYPECOEFF,ser2)='t_SERIES' then
	    first:=procname(ser1,op([LISTCOEFF,1],ser2),args[3..-1])
	else first:=CONVERT2SERIES(eval(fcn,v=actpt),scale,var,false) fi;
	if rest=0 then res:=first
	else
	    sc0:=newscale(v0,0);
	    genser1:=RUN(subs(v=genpt+v0,fcn)-subs(v=genpt,fcn),sc0,
		[eval(sc0['variable'],1)],ord);
	    # Since the point is generic, genser1 is a Taylor series.
	    # check for cases where leading 0 coefficient has not been 
	    # recognized:
	    if op(LISTEXPON,genser1)<>[] and op([LISTEXPON,1],genser1)=0 then
		genser1:=subsop([LISTCOEFF,1]=NULL,[LISTEXPON,1]=NULL,genser1)
	    fi;
	    res:=ADDDOIT(first,
		procname(subs(genpt=actpt,genser1),rest,args[3..-1]))
	fi
    else
	if op(LISTEXPON,ser1)=[] then res:=0
	else res := `multiseries/ComposeClassical`(ser1,ser2,args[3..-1]) fi;
	if op(EXPONBIGO,ser1)<>infinity then # big O term
	    # order 0 might work below, try later.
	    bigo:=POWER(ser2,op(EXPONBIGO,ser1),scale,var,1); 
	    if bigo<>0 then
		if op(COEFFBIGO,ser1)<>0 then
		    bigo:=MULDOIT(bigo,op(COEFFBIGO,ser1)) fi;
		# here, the O()-term means that the coefficients that would 
		# have been added if this was not in a O() belong to O() terms,
		# recursively, whence the need to get their coefficient right.
		bigo:=`multiseries/compose/makebigo`(bigo,scale);
		res:=ADDDOIT(res,bigo)
	    else return CONVERT2SERIES(LIMIT(ser1,scale1,var),scale,var,true)
	    fi
	fi;
    fi;
    subsop(EXPR4SERIES=subs(op(EXPANVAR,ser1)=op(EXPR4SERIES,ser2),
	op(EXPR4SERIES,ser1)),res)
end proc :                                             # `multiseries/compose`
#------------------------------------------------------------------------------

# This function is here temporarily. It should be moved somewhere else.
`multiseries/compose/makebigo`:=proc (ser,scale)
local res;
    ASSERT(ser::'SERIES' and scale::'SCALE');
    if op(TYPECOEFF,ser)='t_SERIES' then
	res:=subsop(LISTCOEFF=map(procname,op(LISTCOEFF,ser),scale),ser);
	if op(COEFFBIGO,ser)=0 and op(EXPONBIGO,ser)=infinity then
	    res:=subsop([LISTCOEFF,-1]=NULL,
			[LISTEXPON,-1]=NULL,
			COEFFBIGO=op([LISTCOEFF,-1],res),
			EXPONBIGO=op([LISTEXPON,-1],res),res)
	fi
    elif op(LISTCOEFF,ser)<>[] then
	res:=subsop(LISTCOEFF=[],LISTEXPON=[],
	    EXPONBIGO=op([LISTEXPON,1],ser),ser);
	res:=subsop(COEFFBIGO=SERIES2BIGO(op([LISTCOEFF,1],ser),scale),res)
    else res:=ser
    fi;
    res
end: # `multiseries/compose/makebigo`

#------------------------------------------------------------------------------
### Name                  : `multiseries/ComposeClassical`
### Input                 :
###                          ser1   a SERIES data-structure
###                          ser2   a SERIES data-structure
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :
###				the multiseries expansion of ser1(ser2)
###				at order ord w.r.t. to the scale associated
###				to ser2.
###
### This version avoids unnecessary O() computations. 

`multiseries/ComposeClassical`:=proc(ser1, ser2, scale, var, ord )::SERIES:
option ALGOCOPYRIGHT;
local expon, res, pow, i, lcoeffs, valbigo, comp, bigo, firstexp, Ores, lser2, prevexp;

    ASSERT( ser1 :: 'SERIES' and ser2 :: 'SERIES' and scale :: 'SCALE' and
	var :: list('ScaleVar') and ord :: nonnegint
	and op(LISTEXPON,ser1)<>[]);

    expon:=op(LISTEXPON,ser1);
    lcoeffs:=op(LISTCOEFF,ser1);

    # Locate O term 
    valbigo:=op(EXPONBIGO,ser2);
    if valbigo<>infinity then	# (x^a+O(x^b))^c=x^ac(1+O(x^(b-a)))
	if expon[1]<>0 then firstexp:=expon[1]
	elif nops(expon)>1 then firstexp:=expon[2]
	elif op(EXPONBIGO,ser1)<>infinity then firstexp:=op(EXPONBIGO,ser1)
	fi;
	if op(LISTEXPON,ser2)=[] then valbigo:=firstexp*valbigo
	else valbigo:=op([LISTEXPON,1],ser2)*(firstexp-1)+valbigo fi;
    fi;

    # Main loop 
    comp:=COMPARISONFUNCTION(
	COMMONTYPE({op(TYPEEXPON,ser2),op(TYPEEXPON,ser1)}));
    res:=0; Ores:=0; pow:=1; prevexp:=-infinity; lser2:=ser2;
    for i to nops(expon) do
	if expon[i]=prevexp+1 then pow:=MULDOIT(pow,lser2)
	else pow:=POWER(lser2,expon[i],args[3..-1]) fi;
	pow,bigo:=TRUNCATE(pow,valbigo,comp);
	if expon[i]=firstexp then # Happens only once 
	    if op(LISTEXPON,ser2)=[] then lser2:=0
	    else lser2:=subsop(COEFFBIGO=0,EXPONBIGO=infinity,ser2)
	    fi
	fi;
	res[i]:=MULDOIT(pow,lcoeffs[i]);
	Ores[i]:=MULDOIT(bigo,lcoeffs[i]);
	prevexp:=expon[i]
    od;
    Ores:=ADDDOIT(seq(Ores[i],i=1..nops(expon)));
    ADDDOIT(seq(res[i],i=1..nops(expon)),subsop(COEFFBIGO=Ores,EXPONBIGO=valbigo,
	LISTCOEFF=[],LISTEXPON=[],EXPR4SERIES=op(EXPANVAR,ser2)^valbigo*Ores,
	    ser2))

end proc :                            # `multiseries/ComposeClassical`


