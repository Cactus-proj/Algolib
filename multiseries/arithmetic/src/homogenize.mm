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
###    Title: 	`multiseries/homogenize`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 : 
###                          expr1   a SERIES data-structure
###                          expr2   a SERIES data-structure
### Output                : 
###                          SERIES 
###                          data-structures that are expanded w.r.t. the same 
###                          element in the asymptotic basis
### Error Conditions      :
###                         if expr1 and expr2 are two SERIES expressed 
###                         in two different scales
### Other                 :
###                         in this procedure, a SERIES data-structures like
###                         SERIES(scale,[],1,integer,[],infinity,v1,v2)
###                         can be converted in a bigO term
###                         SERIES(scale,[1],0,integer,[infinity],
###                                infinity,v1,v1^infinity) 
###

#`multiseries/homogenize` := proc(expr1, expr2)
#option ALGOCOPYRIGHT;
#local scale::SCALE, var1 :: ScaleVar, var2 :: ScaleVar , typecoeff::type, t1, t2;
#
#    ASSERT(expr1 :: 'SERIES' and expr2 :: 'SERIES');
#
#    if op(THESCALE,expr1)<>op(THESCALE,expr2) then
#	error "multiseries with different scales not implemented yet"
#    elif op(EXPANVAR,expr1)<>op(EXPANVAR,expr2) then
#	 # we must find the ``fastest'' element between
#	 # op(EXPANVAR,expr1) and op(EXPANVAR,expr2).
#	 # The multiseries expansion w.r.t. the slowest element
#	 # is ``cast'' in a multiseries expansion w.r.t. the fastest one.
#	scale := op(THESCALE,expr1);
#	var1 := op(EXPANVAR,expr1) ;
#	var2 := op(EXPANVAR,expr2) ;
#	if FASTEST([var1,var2],scale) = var1
#	then expr1,'SERIES'(scale,[expr2],0,'t_SERIES',[0],'infinity',
#			      'integer',var1,op(EXPR4SERIES,expr2))
#	else expr2,'SERIES'(scale,[expr1],0,'t_SERIES',[0],'infinity',
#			     'integer', var2,op(EXPR4SERIES,expr1))
#    	fi
#    elif op(TYPECOEFF,expr1)<>op(TYPECOEFF,expr2) then
#	# if expr1 and expr2 are two SERIES expanded w.r.t. the same
#	# element in the asymptotic basis, homogenize the coefficient types
#	    t1:=op(TYPECOEFF,expr1); t2:=op(TYPECOEFF,expr2);
#	    typecoeff := COMMONTYPE({t1,t2});
#	    if typecoeff=t1 then expr1,`multiseries/cast`(expr2,typecoeff)
#	    else `multiseries/cast`(expr1,typecoeff),expr2
#	    fi
#    else expr1,expr2
#    fi
#
#end proc :                                           # `multiseries/homogenize`

`multiseries/homogenize` := proc()
option ALGOCOPYRIGHT;
local Sargs, scale::SCALE, var :: ScaleVar, res , typecoeff::type, tser, ser, 
      tmp;

    Sargs:={args};
    ASSERT(Sargs :: set('SERIES'));

    if nops(map2(op,THESCALE,Sargs))<>1 then
	error "multiseries with different scales not implemented yet"
    elif nops(map2(op,EXPANVAR,Sargs))<>1 then
	 # we must find the ``fastest'' element between the
	 # op(EXPANVAR,args)
	 # The multiseries expansions w.r.t. the slower elements
	 # are ``cast'' in a multiseries expansion w.r.t. the fastest one.
	scale := op(THESCALE,args[1]);
	var:=FASTEST(map2(op,EXPANVAR,Sargs),scale);
	for ser in args do
	    if op(EXPANVAR,ser)=var then res[ser]:=ser
	    else res[ser]:='SERIES'(scale,[ser],0,'t_SERIES',[0],'infinity',
			      'integer',var,op(EXPR4SERIES,ser))
	    end if
	end do;
	return procname(seq(res[ser],ser=args)) # once more for TYPECOEFF
    end if:

    tmp := map2(op,TYPECOEFF,Sargs):
    if nops(tmp)<>1 then
	# SERIES expanded w.r.t. the same
	# element in the asymptotic basis, homogenize the coefficient types
	typecoeff := COMMONTYPE(tmp);
#	for ser in args do
#	    tser:=op(TYPECOEFF,ser);
#	    if tser=typecoeff then res[ser]:=ser
#	    else res[ser]:=`multiseries/cast`(ser,typecoeff)
#	    end if
#	end do;
#	seq(res[ser],ser=args)
        # This is uglier but more memory efficient
        seq(`if`(op(TYPECOEFF,ser)=typecoeff,
                 ser,
                 `if`(op(TYPECOEFF,ser)='algebraic' and typecoeff='float',
                      subsop(TYPECOEFF=typecoeff,
                             LISTCOEFF=evalf(op(LISTCOEFF,ser)),ser),
                      subsop(TYPECOEFF=typecoeff,ser))),
            ser=args);
    else 
        args
    end if

end proc :                                           # `multiseries/homogenize`


#----------------------------------------------------------------------------

### Input                 : 
###                          ser   	a SERIES data-structure
###                          typein    	the type of its coefficients
###                          typeout    the type they should have on output
### Output                : 
###                          ser with its list of coefficients possibly 
###				modified.
### Other		  :  in this version only algebraic -> float
###			        is handled specially. This is needed by 
###				evalf/int.
###		 	     in the future integer mod p should also be treated
###				here.
### A more general mechanism should be designed. For instance, the table
### TypeForest could store the cast operators.
###
`multiseries/cast`:=proc (ser, typeout)
option ALGOCOPYRIGHT;
    if op(TYPECOEFF,ser)='algebraic' and typeout='float' then
	subsop(TYPECOEFF=typeout,LISTCOEFF=evalf(op(LISTCOEFF,ser)),ser)
    else 
        subsop(TYPECOEFF=typeout,ser)
    end if
end proc: # `multiseries/cast`
