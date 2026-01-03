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
    if member(0,expr) 
    then 0 
    else res := expr[1] :
         for i from 2 to nops(expr) do 
             res := MULDOIT(res,expr[i]) 
         end do 
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
        `multiseries/MulClassical`(`multiseries/homogenize`(expr1,expr2))
    end if :

end proc:                                              # `multiseries/MulDoit`
#------------------------------------------------------------------------------

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
`multiseries/MulSeriesCst` := proc(expr1, expr2, type2)::{SERIES,identical(0)}:
option ALGOCOPYRIGHT;
local lcoeff :: list, typecoeff :: type, i , coeffbigo;

    ASSERT(expr1::'SERIES' and type2::type);

    if expr2 = 0 then return 0 end if :

    typecoeff := op(TYPECOEFF,expr1) :

    if typecoeff<>type2 then typecoeff:=COMMONTYPE({typecoeff,type2}) end if :

    if type(typecoeff[`*`],procedure)
    then lcoeff := map(typecoeff[`*`],op(LISTCOEFF,expr1),expr2) 
    else lcoeff := [seq(i*expr2,i=op(LISTCOEFF,expr1))]
    end if :

    coeffbigo:=op(COEFFBIGO,expr1);
    if typecoeff='t_SERIES' and coeffbigo<>0 then
	coeffbigo:=MULDOIT(coeffbigo,expr2) fi;

    subsop( LISTCOEFF   = lcoeff                      ,
            TYPECOEFF   = typecoeff		      ,
            EXPR4SERIES = expr2*op(EXPR4SERIES,expr1) ,
	    COEFFBIGO   = coeffbigo		      ,
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

`multiseries/MulClassical` := proc( ser1, ser2) :: SERIES :
option ALGOCOPYRIGHT;
local lexpon1 :: list , lexpon2 :: list, lcoeff1 :: list, typeexpon,
      j1, j2, bigO, typecoeff :: type, bigO2, expo, res, bigO1, expr2, var, co;
    ASSERT( ser1 :: 'SERIES' and ser2 :: 'SERIES');
    typecoeff := COMMONTYPE({op(TYPECOEFF,ser1),op(TYPECOEFF,ser2)}) :
    typeexpon := COMMONTYPE({op(TYPEEXPON,ser1),op(TYPEEXPON,ser2)}) :
    res:=0;
    lexpon1:=op(LISTEXPON,ser1);
    lcoeff1:=op(LISTCOEFF,ser1);
    lexpon2:=op(LISTEXPON,ser2);
    bigO2:=op(EXPONBIGO,ser2);
    expr2:=op(EXPR4SERIES,ser2);
    var:=op(EXPANVAR,ser2);
    for j1 to nops(lexpon1) do
	expo:=lexpon1[j1];
	res:=ADDDOIT(res,MULSERIESCST(
	    subsop(LISTEXPON=[seq(j2+expo,j2=lexpon2)],TYPEEXPON=typeexpon,
		   `if`(bigO2<>infinity,EXPONBIGO=bigO2+expo,NULL),
		   EXPR4SERIES=expr2*var^expo,ser2),lcoeff1[j1],typecoeff))
    od;
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

    subsop(EXPR4SERIES=op(EXPR4SERIES,ser1)*op(EXPR4SERIES,ser2),res)
    
end proc :                                         # `multiseries/MulClassical`
#------------------------------------------------------------------------------

