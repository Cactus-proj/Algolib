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
###    Title: 	mul
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Description: The multiplication routines. These are
### 	`multiseries/mul`	input is a list whose elements 
###					are to be multiplied
###	`multiseries/MulDoit`:	input are two such elements
###	`multiseries/MulSeriesCst`: input are a SERIES and a constant
###	`multiseries/MulClassical`: naive multiplication.
###
### Currently only the naive multiplication algorithm is implemented.
###
#------------------------------------------------------------------------------
### Name                  : `multiseries/mul`
### Input                 : 
###                          expr    a list of SERIES or 0 
###                                            or 1 (because foo^0=1)
###                          ord     a nonnegative integer
### Output                : 
###                          the multiseries expansion of the product
###                          expr[1]*..*expr[-1] or 0
### Description           :
###                          this procedure is an iterative call to
###                          `multiseries/MulDoit`
###

`multiseries/mul` := proc( expr ) :: {SERIES,identical(0)} ;
option ALGOCOPYRIGHT;
local res, i :: integer :
    ASSERT(expr::list);
    if member(0,expr) then 0 
    else res := expr[1] :
         for i from 2 to nops(expr) do 
             res := MULDOIT(res,expr[i]) 
         end do;
	 res
    end if 

end proc:                                                   # `multiseries/mul`

#------------------------------------------------------------------------------
### Name                  : `multiseries/MulDoit`
### Input                 :
###				expr1  
###				expr2 
### Output                :
###				expr1 * expr2 (of type SERIES if necessary)
### Description           :
###                         this procedure checks the type of expr1 and expr2
###                         and calls the suitable multiplication procedure.
###

`multiseries/MulDoit` := proc ( expr1, expr2 )

    # Homogenize the input ----------------------------------------------------
    if expr2 = 0 or expr1 = 0 then 0
    elif expr2 = 1 then expr1
    elif expr1 = 1 then expr2
    elif not type(expr1,'SERIES') then
	if type(expr2,'SERIES') then MULSERIESCST(expr2,expr1,WHATTYPE(expr1))
	else expr1*expr2
	fi
    elif not type(expr2,'SERIES') then MULSERIESCST(expr1,expr2,WHATTYPE(expr2))
    else
	# for the coefficients computation, we use another procedure 
	# in order to make possible the use of several multiplication 
	# algorithms.
        `multiseries/MulBranch`(`multiseries/homogenize`(expr1,expr2))
    end if :

end proc:                                              # `multiseries/MulDoit`
#------------------------------------------------------------------------------

# Branch between Karatsuba & naive
`multiseries/MulBranch`:=proc(ser1,ser2) 
local i, lexpon1, lexpon2, lcoeff2, n1, nbslices, extra, res;
    if ser1=0 or ser2=0 then 0 
    elif nops(op(LISTEXPON,ser1))>nops(op(LISTEXPON,ser2)) then
    	procname(ser2,ser1)
    # Note: another test based on length instead of nops
    # should be used for series with integer coefficients
    # so that small series with huge coefficients can be multiplied
    # that way
    elif nops(op(LISTCOEFF,ser1))<KARATHRESHHOLD then
       	`multiseries/MulClassical`(ser1,ser2)
    elif nops(op(LISTEXPON,ser1))=nops(op(LISTEXPON,ser2)) then
    	subsop(EXPR4SERIES=op(EXPR4SERIES,ser1)*op(EXPR4SERIES,ser2),
		`multiseries/MulKaratsuba`(ser1,ser2))
    else # slice ser2 if necessary
    	lexpon1:=op(LISTEXPON,ser1);
    	lcoeff2:=op(LISTCOEFF,ser2);
    	lexpon2:=op(LISTEXPON,ser2);
    	n1:=nops(lexpon1);
    	nbslices:=iquo(nops(lexpon2),n1,extra);
    	for i to nbslices do
    	    res[i]:=`multiseries/MulKaratsuba`(ser1,
    	    	subsop(LISTEXPON=lexpon2[n1*(i-1)+1..n1*i],
    	    	       LISTCOEFF=lcoeff2[n1*(i-1)+1..n1*i],
    	    	       COEFFBIGO=0,
    	    	       EXPONBIGO=infinity,ser2))
    	od;
    	# extra bit
    	if extra<>0 then
    	    extra:=procname(subsop(LISTEXPON=lexpon2[-extra..-1],
    	    	LISTCOEFF=lcoeff2[-extra..-1],ser2),ser1)
    	else # compute O()
    	    extra:=`multiseries/MulClassical`(
    	    	subsop(LISTEXPON=[],LISTCOEFF=[],ser2),ser1)
    	fi;
    	subsop(EXPR4SERIES=op(EXPR4SERIES,ser1)*op(EXPR4SERIES,ser2),
	    ADDDOIT(extra,seq(res[i],i=1..nbslices)))
    fi
end:

#------------------------------------------------------------------------------
### Name                  : `multiseries/MulSeriesCst`
### Input                 :
###                          expr1  a SERIES data-structure
###                          expr2  a maple expression. If expr1 is expanded
###                                 w.r.t. var, expr2 does not contains var.
###                          type2  the type of expr2
### Output                :
###                          the SERIES expr2*expr1
### Description           :
###                          expr1 could be O(x) 
###                          (for example the multiplication of a constant 
###                           with O(x)=SERIES(SCALE,[],1,integer,[],
###                                             infinity,integer,x,EXPR)
###                           is allowed)
###
`multiseries/MulSeriesCst` := proc(expr1, expr2, type2)::SERIES;
option ALGOCOPYRIGHT;
local lcoeff :: list, typecoeff :: type, i , coeffbigo;

    ASSERT(expr1::'SERIES' and type2::type and expr2<>0);

#    if expr2 = 0 then 0 
#    else
	typecoeff := op(TYPECOEFF,expr1) :
	if typecoeff<>type2 then typecoeff:=COMMONTYPE({typecoeff,type2}) fi;
	if type(typecoeff[`*`],procedure)
	then lcoeff := map(typecoeff[`*`],op(LISTCOEFF,expr1),expr2)
	else lcoeff := [seq(i*expr2,i=op(LISTCOEFF,expr1))]
	end if :

	coeffbigo:=op(COEFFBIGO,expr1);
	if typecoeff='t_SERIES' and coeffbigo<>0 then
	    coeffbigo:=MULDOIT(coeffbigo,expr2)
	fi;
	subsop( LISTCOEFF   = lcoeff                      ,
            TYPECOEFF   = typecoeff		      ,
            EXPR4SERIES = expr2*op(EXPR4SERIES,expr1) ,
	    COEFFBIGO   = coeffbigo		      ,
            expr1 			              )
#    fi

end proc:                                        # `multiseries/MulSeriesCst`

`multiseries/MulSeriesCst/basic` := proc(expr1, expr2)::SERIES;
option ALGOCOPYRIGHT;
local i;

    ASSERT(expr1::'SERIES' and expr2<>0);

    subsop( LISTCOEFF   = [seq(i*expr2,i=op(LISTCOEFF,expr1))],
            expr1 			              )

end proc:                                        # `multiseries/MulSeriesCst`

#------------------------------------------------------------------------------
### Name                  : `multiseries/MulClassical`
### Input                 :
###                          ser1     a SERIES
###                          ser2     a SERIES
### Output		ser1*ser2	encoded as a SERIES data-structure
### To do		improve efficiency of O() computation
###
`multiseries/MulClassical` := proc( ser1, ser2)
option ALGOCOPYRIGHT;
local lexpon1 :: list , lexpon2 :: list, lcoeff1, lcoeff2 :: list, typeexpon,
      j1, j2, bigO, typecoeff :: type, bigO2, expo, res, bigO1, var, co;
    ASSERT( ser1 :: 'SERIES' and ser2 :: 'SERIES');
    if nops(op(LISTEXPON,ser1))>nops(op(LISTEXPON,ser2)) then
    	return procname(ser2,ser1)
    fi;
    typecoeff := COMMONTYPE({op(TYPECOEFF,ser1),op(TYPECOEFF,ser2)}) :
    typeexpon := COMMONTYPE({op(TYPEEXPON,ser1),op(TYPEEXPON,ser2)}) :
    lexpon1:=op(LISTEXPON,ser1);
    lcoeff1:=op(LISTCOEFF,ser1);
    lcoeff2:=op(LISTCOEFF,ser2);
    lexpon2:=op(LISTEXPON,ser2);
    bigO2:=op(EXPONBIGO,ser2);
    var:=op(EXPANVAR,ser2);
    if type(typecoeff[`*`],procedure) then
	res:=ADDDOIT(seq(MULSERIESCST(
	    subsop(LISTEXPON=[seq(j2+lexpon1[j1],j2=lexpon2)],
	    	TYPEEXPON=typeexpon,
		`if`(bigO2<>infinity,EXPONBIGO=bigO2+lexpon1[j1],NULL),
		  ser2),lcoeff1[j1],typecoeff),j1=1..nops(lexpon1)))
    else
	res:=ADDDOIT(seq(
	    subsop(
	    	LISTCOEFF=[seq(j2*lcoeff1[j1],j2=lcoeff2)],
		LISTEXPON=[seq(j2+lexpon1[j1],j2=lexpon2)],
	    	TYPEEXPON=typeexpon,
		`if`(bigO2<>infinity,EXPONBIGO=bigO2+lexpon1[j1],NULL),
		   ser2),j1=1..nops(lexpon1)))
    fi;
    expo:=op(EXPONBIGO,ser1);
    if expo<>infinity then
	bigO1:=op(COEFFBIGO,ser1);
	if lexpon2<>[] then
	    expo:=expo+lexpon2[1];
	    co:=op([LISTCOEFF,1],ser2)
	elif bigO2<>infinity then
	    expo:=expo+bigO2; 
	    co:=op(COEFFBIGO,ser2)
	else return 0; # lexpon2=[] and bigO2=infinity ==> ser2 = 0 !
	fi;
	if not type(typecoeff[`*`],procedure) then bigO:=bigO1*co
	else bigO:=typecoeff[`*`](bigO1,co) fi;
	res:=ADDDOIT(subsop(LISTEXPON=[],LISTCOEFF=[],EXPONBIGO=expo,
	    COEFFBIGO=bigO,EXPR4SERIES=var^expo,ser2),res)
    fi;
    if res<>0 then
    	subsop(EXPR4SERIES=op(EXPR4SERIES,ser1)*op(EXPR4SERIES,ser2),res)
    else 0 
    fi
    
end proc :                                         # `multiseries/MulClassical`

#------------------------------------------------------------------------------
# Multiply 2 series with the same number of terms by
# Karatsuba's algorithm
`multiseries/MulKaratsuba`:=proc(ser1,ser2)
local lexpon1, lexpon2, a1, a2, lcoeff1, lcoeff2, half1, half2, ser11, ser12, ser21,
ser22, i, middle, res;
    lexpon1:=op(LISTEXPON,ser1);
    lexpon2:=op(LISTEXPON,ser2);
    userinfo(5,MultiSeries,"using Karatsuba's multiplication with length",
    	nops(lexpon1),nops(lexpon2));
    a1:=lexpon1[1];
    a2:=lexpon2[1];
    if a1<>0 then lexpon1:=[seq(i-a1,i=lexpon1)] fi;
    if a2<>0 then lexpon2:=[seq(i-a2,i=lexpon2)] fi;
    # find "middle"
    half1:=iquo(nops(lexpon1),2)+1;
    middle:=lexpon1[half1];
    # could be improved:
    for half2 to nops(lexpon2) while lexpon2[half2]<middle do od;
    if half2>nops(lexpon2) then
    	half2:=iquo(nops(lexpon2),2)+1;
    	middle:=lexpon2[half2];
	for half1 to nops(lexpon1) while lexpon1[half1]<middle do od;
    fi;
    lcoeff1:=op(LISTCOEFF,ser1);
    lcoeff2:=op(LISTCOEFF,ser2);
    ser11:=subsop(LISTCOEFF=lcoeff1[1..half1-1],
    	LISTEXPON=lexpon1[1..half1-1],
    	COEFFBIGO=0,EXPONBIGO=infinity,ser1);
    ser21:=subsop(LISTCOEFF=lcoeff2[1..half2-1],
    	LISTEXPON=lexpon2[1..half2-1],
    	COEFFBIGO=0,EXPONBIGO=infinity,ser2);
    ser12:=subsop(LISTCOEFF=lcoeff1[half1..-1],
    		LISTEXPON=[seq(lexpon1[i]-middle,i=half1..nops(lcoeff1))],
    		`if`(op(EXPONBIGO,ser1)<>infinity,
    			EXPONBIGO=op(EXPONBIGO,ser1)-middle,NULL),ser1);
    ser22:=subsop(LISTCOEFF=lcoeff2[half2..-1],
    		LISTEXPON=[seq(lexpon2[i]-middle,i=half2..nops(lcoeff2))],
    		`if`(op(EXPONBIGO,ser2)<>infinity,
    			EXPONBIGO=op(EXPONBIGO,ser2)-middle,NULL),ser2);
    res:=`multiseries/MulBranch`(
    	ADDDOIT(ser11,MULSERIESCST(ser12,-1,op(TYPECOEFF,ser1))),
    	ADDDOIT(ser22,MULSERIESCST(ser21,-1,op(TYPECOEFF,ser2))));
    ser11:=`multiseries/MulBranch`(ser11,ser21);
    ser22:=`multiseries/MulBranch`(ser12,ser22);
    res:=ADDDOIT(
        ser11,
    	subsop(LISTEXPON=[seq(i+2*middle,i=op(LISTEXPON,ser22))],
	    `if`(op(EXPONBIGO,ser2)<>infinity,
	    	EXPONBIGO=op(EXPONBIGO,ser2)+2*middle,NULL),
	    ser22),
	`if`(res<>0,
	subsop(LISTEXPON=[seq(i+middle,i=op(LISTEXPON,res))],
	    `if`(op(EXPONBIGO,res)<>infinity,
	    	EXPONBIGO=op(EXPONBIGO,res)+middle,NULL),
	    res),0),
	subsop(LISTEXPON=[seq(i+middle,i=op(LISTEXPON,ser11))],ser11),
	subsop(LISTEXPON=[seq(i+middle,i=op(LISTEXPON,ser22))],
	    `if`(op(EXPONBIGO,ser22)<>infinity,
	    	EXPONBIGO=op(EXPONBIGO,ser22)+middle,NULL),
	    ser22));
    if a1+a2=0 then res
    else
    	a1:=a1+a2;
    	subsop(LISTEXPON=[seq(i+a1,i=op(LISTEXPON,res))],
    		`if`(op(EXPONBIGO,res)<>infinity,
    			EXPONBIGO=op(EXPONBIGO,res)+a1,NULL),
    		res)
    fi    
end:

