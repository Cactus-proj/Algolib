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
###    Title: 	`multiseries/WhatType`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###                          expr	a maple expression
### Output                :
###                          the type of expr as defined in TypeForest
### Description           :
###                          if the type of expr is not recognized, it is
###                          assumed to be 'algebraic'.
###			     This can be extended by defining a `gettype/.`
###			     procedure, but this is not documented (yet).

`multiseries/WhatType` := proc( expr ) :: type ;
option ALGOCOPYRIGHT; 
local n ;

   if type(expr,'integer')
   then 'integer'
   elif type(expr,'rational')
   then 'rational'
   elif type(expr,'float')
   then 'float'
   elif type(expr,'SERIES')
   then 't_SERIES'
   elif type(expr,function)
   then n := cat(`gettype/`,op(0,expr)) ;
        if type(n,procedure) 
	then n(op(expr))
        else 'algebraic'
	end if
   else 'algebraic'	
   end if 

end proc :                                             # `multiseries/WhatType`
#----------------------------------------------------------------------------

