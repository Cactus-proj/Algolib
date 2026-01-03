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
###	`multiseries/AddDoit`:	input are elements to be added
###	`multiseries/AddBigO`:	adds a O() to a SERIES.
###
###   This should be modified. comp should return in {1,-1,0,FAIL}.
###
### Modification: B.S. Aug 04
### The code is reorganized: AddDoit takes as input an arbitrary number
### of SERIES/0 and deals with homogenizing and extracting common types.
###
#------------------------------------------------------------------------------
### Name                  : `multiseries/add`
### Input                 :
###				expr    a list
### Output                :
###				the sum of elements in expr
### Description           :
###                          this procedure is obsolete. It is just a call to
###                          `multiseries/AddDoit`

`multiseries/add` := proc( expr ) :: {SERIES,identical(0)} ;
option ALGOCOPYRIGHT;
   ADDDOIT(op(expr))
end proc :                                                  # `multiseries/add`

#------------------------------------------------------------------------------
### Name                  : `multiseries/addMS`
### Input                 :
###				SERIES or constants
### Output                :
###				their sum (of type SERIES if necessary)
###            or FAIL
### Description           :
###                         this is an externally compiled C routine
###                         performing the addition if:
###                         - all arguments are SERIES or 0
###                         - all arguments are homogeneous
###                         - all exponents are integers
###                         Otherwise, it returns FAIL
###                         This procedure redefines itself on first 
###                         invocation.
###                         
`multiseries/addMS` := proc()
   unprotect(procname);
   assign(procname, define_external("addMS", 'MAPLE', 'LIB'=ExternalCalling:-ExternalLibraryName("mseries")));
   protect(procname);
   procname(args)
end proc:

#------------------------------------------------------------------------------
### Name                  : `multiseries/AddDoit`
### Input                 :
###				SERIES or constants
### Output                :
###				their sum (of type SERIES if necessary)
### Description           :
###                         this procedure checks the types of its args
###                         and calls the suitable addition procedure.
###                         
## The aim is to be fast when all arguments are SERIES with the same
## variable and typecoeff, and in particular when there are only 2 such args.
`multiseries/AddDoit` := proc ()
option ALGOCOPYRIGHT;
local L, notser, typecoeff, typeexpon, lcoeff, lexpon, bigO, bigoterm, expr4series, oterm, bigos, scale;
# uncomment the following four lines to enable the kernel extension 
# code for addition
#    L := `multiseries/addMS`(args);
#    if L<>FAIL then
#       return L;
#    end if:
    L,notser:=selectremove(type,[args],'SERIES');
    if notser<>[] then
    	notser:=convert(notser,`+`);
    	if L=[] then return notser
    	elif notser<>0 then
    	    L:=[op(L),
    	    	CONVERT2SERIES(notser,op([1,THESCALE],L),
    	    	    [op([1,EXPANVAR],L)],false)]
    	fi
    fi;
    if nops(L)<=1 then if L=[] then 0 else op(L) fi
    else
    	L:=[`multiseries/homogenize`(op(L))];
	    typecoeff := op([1,TYPECOEFF],L);
    	# Determine the exponents type
    	if nops(map2(op,TYPEEXPON,{op(L)}))<>1 then
    	    typeexpon := COMMONTYPE(map2(op,TYPEEXPON,{op(L)}))
    	else typeexpon:=op([1,TYPEEXPON],L)
    	fi;
        scale:=op([1,THESCALE],L);
    	# Special case where things need to be fast:
    	if member(typeexpon,[integer,rational,float]) or
                COMPARISONFUNCTION(typeexpon)=`<` then
    	    if not type(typecoeff[`+`],procedure) then
                lcoeff,lexpon,bigO:=`multiseries/AddSeries/basic`(L)
            else
        	    lcoeff,lexpon,bigO:=`multiseries/AddSeries/basic`(
        	        L, typecoeff[`+`])
            fi;
            if bigO=infinity then bigoterm:=0
            elif typecoeff='t_SERIES' or 
                typecoeff='algebraic' and
                    (has(map2(op,LISTCOEFF,L),SCALELIST) or has(map2(op,COEFFBIGO,L),SCALELIST)) then
            	bigoterm:=`multiseries/AddSeries/findbigo`(L,bigO)
            else bigoterm:=1
            fi
    	else
    	    lcoeff,bigoterm,lexpon,bigO:=
    	    	`multiseries/AddSeries/hardexponents`(L,
    	    COMPARISONFUNCTION(typeexpon),typecoeff)
    	fi;                                                                       
        expr4series:=map2(op,EXPR4SERIES,L);
        if has(expr4series,O) then # the input contained old type series
    	    bigos:=indets(expr4series,specfunc(anything,O));
            expr4series:=# convert(eval(expr4series,O=0),`+`)+ %% much too expensive (see test g14)
    	        subs([seq(oterm=0,oterm=bigos)],convert(expr4series,`+`))+
                    convert(bigos,`+`)
        else expr4series:=convert(expr4series,`+`)
        fi;
    	SERIES(scale,lcoeff,bigoterm,typecoeff,lexpon,bigO,
    	    typeexpon,op([1,EXPANVAR],L),expr4series)
    fi
end proc :                                              # `multiseries/AddDoit`

`multiseries/AddSeries/findbigo`:=proc(sers,bigoexpon)
local res, ser, i;
    res:=0;
    for ser in sers do
    	if op(EXPONBIGO,ser)=bigoexpon then
    	    res:=ADDBIGO(op(COEFFBIGO,ser),res,op(THESCALE,ser))
    	elif member(bigoexpon,op(LISTEXPON,ser),'i') then
    	    res:=ADDBIGO(res,op([LISTCOEFF,i],ser),op(THESCALE,ser))
    	fi
    od;
    res
end:

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
###                          sers    a list of SERIES data-structure
### Output                :
###			     their sum encoded as a SERIES data-structure
### Description           :
###                         this procedure performs the addition of
###                         SERIES data-structure in the case when the
###			    exponents are compared with "<", "<=",...
###			    and the coefficients are added with "+".
###		
###
`multiseries/AddSeries/basic`:=proc(sers,addcoeff)
option ALGOCOPYRIGHT;
local exps, cofs, co, ex, ser, bigo, indmax, i;
    	# Find exponent of O() term
    	bigo:=min(op(map2(op,EXPONBIGO,sers)));
	# Merge sorted lists of exponents with removal of duplicates
	exps:=sort([op({op(map(op,map2(op,LISTEXPON,sers)))})]);
	# this could/should be done by dichotomy
	for indmax from nops(exps) by -1 to 1 while bigo<=exps[indmax] do od;
	# truncate exps if needed
	if indmax<nops(exps) then exps:=exps[1..indmax] fi;
	## create the list of coefficients
	for i in exps do cofs[i]:=0 od;
	for ser in sers do # add into it
	    co:=op(LISTCOEFF,ser);
	    ex:=op(LISTEXPON,ser);
	    # same dichotomy as above
	    for indmax from nops(ex) by -1 to 1 while bigo<=ex[indmax] do od;
	    if nargs=2 then
	        for i to indmax do cofs[ex[i]]:=addcoeff(cofs[ex[i]],co[i]) od
	    else
	    	for i to indmax do cofs[ex[i]]:=cofs[ex[i]]+co[i] od
	    fi
	od;
        [seq(cofs[i],i=exps)],exps,bigo
end:

minwithcomp:=proc(mini,L,comp)
local a,b;
    if L=[] then mini
    elif member(L[1],mini) then mini # cheapest test first
    else
    	a:=L[1]; b:=mini[1];
	if comp(a,b) then {a}
	elif comp(b,a) then mini
	elif Testzero(a-b)=true or signum(0,a-b,0)=0 then mini union {a}
	else error "need to determine the sign of %1",a-b
	fi
    fi
end:

`multiseries/AddSeries/hardexponents`:=proc(sers,comp,typecoeff)
option ALGOCOPYRIGHT;
local bigO, bigOterm, cofs, exps, i, sbigO, scale, tmp,ser, n, lexps, co;
    scale:=op([1,THESCALE],sers);
    # Find exponent of O() term
    sbigO:=minwithcomp([op([1,EXPONBIGO],sers)],
    	map2(op,EXPONBIGO,subsop(1=NULL,sers)),comp);
    # Select the smallest one
    tmp:=map(length,[op(sbigO)]);
    member(min(op(tmp)),tmp,'i');
    bigO:=sbigO[i]; bigOterm:=0;
    # Find truncation points in series as well as coefficient of O()
    for ser in sers do
    	if member(op(EXPONBIGO,ser),sbigO) then
    	    n[ser]:=nops(op(LISTEXPON,ser));
    	    bigOterm:=`multiseries/AddBigO`(op(COEFFBIGO,ser),bigOterm,scale)
    	else
    	    lexps:=op(LISTEXPON,ser);
    	    n[ser]:=LOCATE(lexps,bigO,comp);
    	    if n[ser]<nops(lexps) and member(lexps[n[ser]+1],sbigO) then
    	    	bigOterm:=`multiseries/AddBigO`(op([LISTCOEFF,n[ser]+1],ser),
    	    	    bigOterm,scale)
    	    fi
    	fi
    od;
    # Add coefficients
    exps:={op(map(op,map2(op,LISTEXPON,sers)))};
    for i in exps do cofs[i]:=0 od;
    for ser in sers do
    	lexps:=op(LISTEXPON,ser);
    	co:=op(LISTCOEFF,ser);
    	if type(typecoeff[`+`],procedure) then
	    for i to n[ser] do
	    	cofs[lexps[i]]:=typecoeff[`+`](cofs[lexps[i]],co[i])
	    od
	else
	    for i to n[ser] do cofs[lexps[i]]:=cofs[lexps[i]]+co[i] od
    	fi
    od;
    # Remove duplicate exponents and add corresponding coefficients
    exps:=select(proc(u) assigned(cofs[u]) end,exps);
    try 
    	exps:=sort([op(exps)],comp);
    catch: 
    	error "unable to sort exponents",exps
    end try;
    [seq(cofs[i],i=exps)],bigOterm,exps,bigO
end:
