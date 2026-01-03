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
###    Title: 	add
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Description: The addition routines. These are
###	`multiseries/add`:	input is a list whose elements are to be added
###	`multiseries/AddDoit`:	input are two such elements
###	`multiseries/AddSeries`: input are two SERIES in the same variable
###	`multiseries/AddBigO`:	adds a O() to a SERIES.
###
### This should be modified. comp should return in {1,-1,0,FAIL}.
#------------------------------------------------------------------------------
### Name                  : `multiseries/add`
### Input                 :
###				expr    a list
### Output                :
###				the sum of elements in expr
### Description           :
###                          this procedure is just an iterative call to
###                          `multiseries/AddDoit`

`multiseries/add` := proc( expr ) :: {SERIES,identical(0)} ;
option ALGOCOPYRIGHT;

local res, i :: integer ;

    ASSERT(expr::list);
    res := expr[1]   :
    for i from 2 to nops(expr) do res := ADDDOIT(res,expr[i]) end do :
    res 

end proc :                                                  # `multiseries/add`

#------------------------------------------------------------------------------
### Name                  : `multiseries/AddDoit`
### Input                 :
###				expr1   an expression    
###				expr2   an expression 
### Output                :
###				expr1 + expr2 (of type SERIES if necessary)
### Description           :
###                         this procedure checks the type of expr1 and expr2
###                         and calls the suitable addition procedure.
###                         

`multiseries/AddDoit` := proc ( expr1, expr2 )

local ser1 :: SERIES, ser2 :: SERIES :

    if expr2 = 0 then expr1    
    elif expr1 = 0 then expr2    
    elif not type(expr1,'SERIES') then
	if not type(expr2,'SERIES') then expr1+expr2
	else `multiseries/AddSeries`(CONVERT2SERIES(expr1,op(THESCALE,expr2),
	    [op(EXPANVAR,expr2)],false),expr2)
	fi
    elif not type(expr2,'SERIES') then
	`multiseries/AddSeries`(expr1,CONVERT2SERIES(expr2,op(THESCALE,expr1),
	    [op(EXPANVAR,expr1)],false))
    else `multiseries/AddSeries`(`multiseries/homogenize`(expr1,expr2))
    fi

end proc :                                              # `multiseries/AddDoit`

#------------------------------------------------------------------------------
### Name                  : `multiseries/AddSeries`
### Input                 :
###                          ser1     a SERIES data-structure
###                          ser2     a SERIES data-structure
### Output                :
###			     ser1+ser2 encoded as a SERIES data-structure
### Description           :
###                         this procedure performs the addition of two
###                         SERIES data-structure
### Todo		  :
###                             This procedure is designed for general case.
###                             The efficiency for the most usual cases
###                             must be improved in a forthcoming version and
###				part (or all) of this procedure should be 
###				implemented in the kernel

`multiseries/AddSeries` := proc( ser1, ser2)::{SERIES,identical(0)}:
option ALGOCOPYRIGHT;

local typeexpon, typecoeff, lexpon, lcoeff, bigoterm, bigO;

    ASSERT(ser1::'SERIES' and ser2::'SERIES');

    # Determine the coefficients type ----------------------------------------
    typecoeff := COMMONTYPE({op(TYPECOEFF,ser1),op(TYPECOEFF,ser2)}) :

    # Determine the exponents type -------------------------------------------
    typeexpon := COMMONTYPE({op(TYPEEXPON,ser1),op(TYPEEXPON,ser2)}) :

    # Special case where things need to be fast:
    if member(typeexpon,[integer,rational,float]) or
	COMPARISONFUNCTION(typeexpon)=`<` then
    	if not type(typecoeff[`+`],procedure) then
	    lcoeff,lexpon,bigO:=`multiseries/AddSeries/basic`(ser1,ser2)
	else
	    lcoeff,lexpon,bigO:=`multiseries/AddSeries/basic`(
	    	ser1,ser2, typecoeff[`+`])
	fi;
	if bigO=infinity then bigoterm:=0 else bigoterm:=1 fi
    else
    	lcoeff,bigoterm,lexpon,bigO:=
    	    `multiseries/AddSeries/hardexponents`(ser1,ser2,
    	    	COMPARISONFUNCTION(typeexpon),typecoeff)
    fi;
    SERIES(op(THESCALE,ser1),lcoeff,bigoterm,typecoeff,lexpon,bigO,typeexpon,
    	op(EXPANVAR,ser1),op(EXPR4SERIES,ser1)+op(EXPR4SERIES,ser2))
end proc :                                            # `multiseries/AddSeries`

#------------------------------------------------------------------------------
### Name                  : `multiseries/AddBigO`
### Input                 :
###				ser1  the COEFFBIGO field of a SERIES
###                             ser2  a maple expression
###                             scale a SCALE data-structure 
### Output                : 
###				The COEFFBIGO field of a SERIES whose O() term
###				is O(ser1+ser2). 
###                             This can be a SERIES data-structure, whose bigo
###				term only encodes the O() term.
###				Example: n+O(1) + ln(n) --> n+O(ln(n)+1)
`multiseries/AddBigO` := proc(ser1,ser2,scale)
local bigo;
option ALGOCOPYRIGHT;

    ASSERT(scale::'SCALE');
    if ser2 = 0 then ser1
    elif ser1 = 0 then SERIES2BIGO(ser2,scale)
    elif type(ser1,'SERIES') then
	bigo:=ADDDOIT(ser1,ser2); # ser1 has a O-term
	if bigo=0 then # cancellation occurred because of the closed forms
	    ser1
	else bigo
	fi
    # now ser1 is a monomial 
    elif not type(ser2,'SERIES') then
	SERIES2BIGO(ser1+SERIES2BIGO(ser2,scale),scale)
    else # at this stage, ser2 is a SERIES and we just add O(1) to it.
	`multiseries/AddSeries`(ser2,
	    subsop(LISTEXPON=[],LISTCOEFF=[],COEFFBIGO=ser1,EXPONBIGO=0,ser2))
    fi
end proc :                                              # `multiseries/AddBigO`
#------------------------------------------------------------------------------
### Name                  : `multiseries/AddSeries/basic`
### Input                 :
###                          ser1     a SERIES data-structure
###                          ser2     a SERIES data-structure
### Output                :
###			     ser1+ser2 encoded as a SERIES data-structure
### Description           :
###                         this procedure performs the addition of two
###                         SERIES data-structure in the case when the
###			    exponents are compared with "<", "<=",...
###			    and the coefficients are added with "+".
###		
###
`multiseries/AddSeries/basic`:=proc(ser1,ser2)
local exps, cofs, co1, co2, bigo, i, j1, j2, n1, n2, exp1, exp2, last, j1max, j2max;
	exp1:=op(LISTEXPON,ser1); exp2:=op(LISTEXPON,ser2);
	n1:=nops(exp1); n2:=nops(exp2);
	# exponent of O() term
	j1:=op(EXPONBIGO,ser1);
	j2:=op(EXPONBIGO,ser2);
	if j1<=j2 then
	    bigo:=j1; j1max:=n1;
	    for j2max from n2 by -1 to 1 while exp2[j2max]>=bigo do od
	else
	    bigo:=j2; j2max:=n2;
	    for j1max from n1 by -1 to 1 while exp1[j1max]>=bigo do od
	fi;
	co1:=op(LISTCOEFF,ser1); co2:=op(LISTCOEFF,ser2);
	# mergesort with removal of duplicates
	j1:=1; j2:=1;i:=0;
	if n1<>0 and n2<>0 then
	     last:=min(exp1[n1],exp2[n2]);
	     for i do
	     	if exp1[j1]=exp2[j2] then
	     		exps[i]:=exp1[j1];
	     		cofs[i]:=co1[j1]+co2[j2];
	     		j1:=j1+1; j2:=j2+1
	     	elif exp1[j1]<exp2[j2] then
	     		exps[i]:=exp1[j1];
	     		cofs[i]:=co1[j1];
	     		j1:=j1+1
	     	else # exp1[j1]>exp2[j2]
	     		exps[i]:=exp2[j2];
	     		cofs[i]:=co2[j2];
	     		j2:=j2+1
	     	fi;
	     	if exps[i]=last then break fi
	     od
	fi;
	if j1>n1 then
	    cofs:=[seq(cofs[j1],j1=1..i),op(j2..j2max,co2)];
	    exps:=[seq(exps[j1],j1=1..i),op(j2..j2max,exp2)]
	elif j2>n2 then
	    cofs:=[seq(cofs[j2],j2=1..i),op(j1..j1max,co1)];
	    exps:=[seq(exps[j2],j2=1..i),op(j1..j1max,exp1)]
	fi;
	cofs,exps,bigo
end:

#------------------------------------------------------------------------------
`multiseries/AddSeries/basic`:=proc(ser1,ser2,addcoeff)
local exps, cofs, co, ex, ser, bigo, indmax, i;
    	# Find exponent of O() term
	# inline this call to simpl/min
    	# bigo:=min(op(EXPONBIGO,ser1),op(EXPONBIGO,ser2));
    	if op(EXPONBIGO,ser1)<=op(EXPONBIGO,ser2) then
    	    bigo:=op(EXPONBIGO,ser1)
    	else bigo:=op(EXPONBIGO,ser2) fi;
	# Merge sorted lists of exponents with removal of duplicates
	exps:=sort([op({op(op(LISTEXPON,ser1)),op(op(LISTEXPON,ser2))})]);
	# this could/should be done by dichotomy
	for indmax from nops(exps) by -1 to 1 while bigo<=exps[indmax] do od;
	# truncate exps if needed
	if indmax<nops(exps) then exps:=exps[1..indmax] fi;
	## create the list of coefficients
	for i in exps do cofs[i]:=0 od;
	for ser in [ser1,ser2] do # add into it
	    co:=op(LISTCOEFF,ser);
	    ex:=op(LISTEXPON,ser);
	    # same dichotomy as above
	    for indmax from nops(ex) by -1 to 1 while bigo<=ex[indmax] do od;
	    if nargs=3 then
	        for i to indmax do cofs[ex[i]]:=addcoeff(cofs[ex[i]],co[i]) od
	    else
	    	for i to indmax do cofs[ex[i]]:=cofs[ex[i]]+co[i] od
	    fi
	od;
        [seq(cofs[i],i=exps)],exps,bigo
end:

`multiseries/AddSeries/hardexponents`:=proc(ser1,ser2,comp,typecoeff)
local n1, n2, exp1, exp2, j1, j2, bigO, bigOterm, co1, co2, cofs, exps, i;

    exp1 := op(LISTEXPON,ser1) :
    exp2 := op(LISTEXPON,ser2) :
    # Determine the ``truncation order'' ---------------------------------
    n1:=op(EXPONBIGO,ser1);
    n2:=op(EXPONBIGO,ser2);
    if n1=n2 then # cheapest test first
    	bigO:=n1; n1:=nops(exp1); n2:=nops(exp2);
    	bigOterm:=`multiseries/AddBigO`(op(COEFFBIGO,ser1),op(COEFFBIGO,ser2),op(THESCALE,ser1))
    elif comp(n1,n2) then
	bigO := n1;
	n1:=nops(exp1);
	# this statement assumes a syntactical equality of the exponents
	if member(bigO,exp2,'n2') then
	   bigOterm := `multiseries/AddBigO`(
	       op(COEFFBIGO,ser1),exp2[n2],op(THESCALE,ser1));
	   n2:=n2-1
	else
	   bigOterm := op(COEFFBIGO,ser1);
	   n2:= LOCATE(exp2,bigO,comp)
	fi
    elif comp(n2,n1) then # other way round
	bigO := n2;
	n2:=nops(exp2);
	# this statement assumes a syntactical equality
	if member(bigO,exp1,'n1') then
	    bigOterm := `multiseries/AddBigO`(
		op(COEFFBIGO,ser2),exp1[n1],op(THESCALE,ser1)) ;
	    n1:=n1-1
	else
	    bigOterm := op(COEFFBIGO,ser2);
	    n1 := LOCATE(exp1,bigO,comp)
	fi
    elif # other equality cases
	Testzero(n2-n1)=true  or
	# 3. last chance 
	signum(0,n2-n1,0)=0
	then
    	bigO:=n1; n1:=nops(exp1); n2:=nops(exp2);
    	bigOterm:=`multiseries/AddBigO`(op(COEFFBIGO,ser1),op(COEFFBIGO,ser2),op(THESCALE,ser1))
    else # cannot compare
	error "need to determine the sign of %1",n2-n1
    end if :	
    # Add coefficients (merge sort with duplicates)
    co1 := op(LISTCOEFF,ser1); co2:=op(LISTCOEFF,ser2);
    # small optimisation:
    if exp1=exp2 then
    	exps:=exp1;
    	if not type(typecoeff[`+`],procedure) then
    	    cofs:=co1+co2
    	else cofs:=[seq(typecoeff[`+`](co1[i],co2[i]),i=1..nops(co1))]
    	fi
    else
	j1:=1; j2:=1; i:=0;
	if n1<>0 and n2<>0 then
	    for i do
		if exp1[j1]=exp2[j2] then
		    exps[i]:=exp1[j1];
		    if type(typecoeff[`+`],procedure) then
		        cofs[i]:=typecoeff[`+`](co1[j1],co2[j2])
		    else cofs[i]:=co1[j1]+co2[j2] fi;
		    j1:=j1+1; j2:=j2+1;
		    if j1>n1 or j2>n2 then break fi
		elif comp(exp1[j1],exp2[j2]) then
		    exps[i]:=exp1[j1];
		    cofs[i]:=co1[j1];
		    j1:=j1+1;
		    if j1>n1 then break fi
		elif comp(exp2[j2],exp1[j1]) then # exp1[j1]>exp2[j2]
		    exps[i]:=exp2[j2];
		    cofs[i]:=co2[j2];
		    j2:=j2+1;
		    if j2>n2 then break fi
		elif Testzero(exp1[j1]-exp2[j2])=true or 
		    signum(0,exp1[j1]-exp2[j2],0)=0 then # other equality cases
		    exps[i]:=exp1[j1];
		    if type(typecoeff[`+`],procedure) then
		        cofs[i]:=typecoeff[`+`](co1[j1],co2[j2])
		    else cofs[i]:=co1[j1]+co2[j2] fi;
		    j1:=j1+1; j2:=j2+1;
		    if j1>n1 or j2>n2 then break fi
		else error "need to determine the sign of %1",exp1[j1]-exp2[j2]
		fi
	    od
	fi;
	if j1>n1 then
	    cofs:=[seq(cofs[j1],j1=1..i),op(j2..n2,co2)];
	    exps:=[seq(exps[j1],j1=1..i),op(j2..n2,exp2)]
	elif j2>n2 then
	    cofs:=[seq(cofs[j2],j2=1..i),op(j1..n1,co1)];
	    exps:=[seq(exps[j2],j2=1..i),op(j1..n1,exp1)]
	fi
    fi;
    cofs,bigOterm,exps,bigO
end:
