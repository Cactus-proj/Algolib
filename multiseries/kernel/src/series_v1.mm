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
###    Title: 	SERIES -- the SERIES data-structure
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###
###		      -	a scale 	              scale       :: SCALE
###		      -	list of coefficients 	      lcoeffs     :: list
###			(sparse representation)
###		      -	coefficient of O-term         coeffO      :: algebraic
###		      -	type of coefficients 	      typeCoeff   :: type
###		      - list of exponents 	      lexpons     :: list
###			(same length as lcoeffs)
###		      -	O-term		              exponO      :: algebraic
###		      -	type of exponents 	      typeExpon   :: type
###		      -	expansion variable             expanVar   :: algebraic
###		      -	the expression being expanded expr4SERIES :: algebraic
###
### Output                :
###                          0 or a SERIES data-structure
### Description           :
###                          the zero terms are suppressed from the 
###                          coefficients list (and the associated exponents).
###
###                          When all coefficients are SERIES that represent
###                          constants, convert them to constants.
###
###			     otherwise, all the coefficients are expressed as
###			     SERIES in the same expansion variable.
###
###			     Note on the O-term:
###				. the exponent of the O-term is stored in a 
###				separate field. This exponent is larger than
###				the largest element of the list of exponents;
###				. exact expressions are represented with a
###				O-term with 0 coefficient and infinity for 
###				exponent;
###				. when the coefficient of the O-term is a 
###				SERIES, the series has a non-zero O-term, which
###				is the only term contributing to the
###				mathematical O-term. Thus, the expansion
###				being represented is
###				    sum(lcoeffs[i]*expanVar^lexpons[i])+
###				    expanVar^exponO*O(coeffO),
###				where O(coeffO) is interpreted recursively.
###
### Implementation        : could be moved into the kernel
### Error Conditions      : if a field of the SERIES data-structure is missing
###

SERIES := proc(	scale       :: SCALE	 , lcoeffs   :: list	 ,
                coeffO      :: algebraic , typeCoeff :: type	 ,
                lexpons     :: list	 , exponO    :: algebraic ,
                typeExpon   :: type	 , expanVar  :: algebraic ,
                expr4SERIES :: algebraic ) :: { SERIES, identical(0)} ;
option ALGOCOPYRIGHT; 
local i, expons, coeffs, typecoeff, fast , ser, const, fastO, ind;
    if expr4SERIES = 0 then 0
    else
	coeffs:=lcoeffs;
	expons:=lexpons;
	# suppress zero terms
	if member(0,coeffs) or member(0.,coeffs) then
	    for i to nops(coeffs) do
		if coeffs[i]=0 or coeffs[i]=0. then ind[i]:=i=NULL
		else ind[i]:=NULL
		fi
	    od;
	    ind:=seq(ind[i],i=1..nops(coeffs));
	    expons:=subsop(ind,expons);
	    coeffs:=subsop(ind,coeffs)
	fi;
	typecoeff := typeCoeff :
	# rewrite the coefficients if necessary
	if coeffs<>[] and typeCoeff='t_SERIES' then
	    if type(coeffO,'SERIES') then fastO:=op(EXPANVAR,coeffO)
	    else fastO:=NULL fi;
	    ser,const:=selectremove(type,coeffs,'SERIES');
	    fast := map2(op,EXPANVAR,{op(ser)});
	    if fast={} and fastO=NULL then # all the coefficients are constant
		typecoeff := COMMONTYPE(map(WHATTYPE,{op(const)}))
	    else
		typecoeff := 't_SERIES';
		if nops(fast)<>1 or fastO<>op(fast) or const<>[] then
		    fast      := FASTEST([op(fast),fastO],scale) :
		    coeffs := map(CONVERT2SERIES,coeffs,scale,[fast],false)
		fi
	    end if 
	end if :
        # Output 
	if coeffs=[] and coeffO=0 and exponO=infinity then 0
	else
	    'SERIES'( scale,coeffs,coeffO,typecoeff,expons,
	          exponO,typeExpon,expanVar,expr4SERIES )
	fi
    end if

end proc :			                                       # SERIES
#------------------------------------------------------------------------------

