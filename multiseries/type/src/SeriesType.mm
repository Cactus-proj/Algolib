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
###    Title: 	type/SERIES and t_SERIES
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###			    	expr 	any non-NULL expression
### Output                :     boolean    
### Description           :
###				type-checking function
###
###				The type SERIES is a function
###
###			SERIES(		scale		,
###					lcoeffs		,
###					coeffO		,
###					typeCoeff	,
###					lexpons		,
###					exponO		,
###					typeExpon	,
###					expanVar	,
###					Expr4Series	)
###
###			    	where the fields are
###
###		      -	a scale                       scale       :: scale
###		      -	list of coefficients 	      lcoeffs     :: list
###			(sparse representation)
###		      -	coefficient of O-term         coeffO      :: algebraic
###		      -	type of coefficients 	      typeCoeff   :: type
###		      - list of exponents             lexpons     :: list
###			(same length as lcoeffs)
###		      -	O-term		              exponO      :: algebraic
###		      -	type of exponents             typeExpon   :: type
###		      -	expansion variable            expanVar    :: algebraic
###		      -	the expression being expanded expr4SERIES :: algebraic
###
### References            :
### Date                  : Tue Oct  2 16:50:58 MEST 2001
### Last modified         :
### Other                 : 	see series.mpl
### 

## This is for debugging purposes only
#`type/SERIES`:= proc(f) :: boolean ;
#    type(f,specfunc(anything,SERIES))
#	and type([ op(f) ],
#	   [SCALE,list,algebraic,type,list,algebraic,type,algebraic,algebraic])
#	and type(op(LISTCOEFF,f),list(op(TYPECOEFF,f)))
#	and type(op(LISTEXPON,f),list(op(TYPEEXPON,f)))
#	and not has(op(EXPR4SERIES,f),'SERIES')
#end proc:


# The ``light'' version below is sufficient since SERIES is an actual
# function that type-checks its arguments
#`type/SERIES`:= proc(f) :: boolean ;
#option ALGOCOPYRIGHT; 
##    type(f,specfunc(anything,'SERIES'))
#    type(f,function) and op(0,f)='SERIES'
#end proc :			                                # `type/SERIES`
`type/SERIES` := 'specfunc'('anything','SERIES'):

#----------------------------------------------------------------------------

#------------------------------------------------------------------------------
### Name                  : t_SERIES
### Description           :
###				t_SERIES is a table that stores the available 
###                             operations associated to the SERIES 
###                             data-structure
### Last modified         :
### Other                 : a user could define his own maple data-structure
###                         and the associated arithmetic operations in a
###                         similar way
###
###                         Unfortunately, the procedure SERIES 
###                         (see series.mpl)and the table t_SERIES cannot 
###                         share the same name. Thus, we must define a new
###                         type checking function `type/t_SERIES`.
###
t_SERIES[`+`] := `multiseries/AddDoit` :
t_SERIES[`*`] := `multiseries/MulDoit` :
t_SERIES[`^`] := `multiseries/power` :
`type/t_SERIES`:= op(`type/SERIES`):    # `type/t_SERIES`
#----------------------------------------------------------------------------


