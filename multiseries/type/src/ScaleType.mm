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
###    Title: 	`type/SCALE`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 : 	expr	any non-NULL expression
### Output                : 	boolean
### Description           : type-checking function
###
###				for an expression expr and a variable var,
###				a SCALE is a table with entries :
###
###			      varname	the name of the variable var
###				 list	the ordered list of the variables
###					_vari(j) (the slowest variables
###					come first)
###				index	the maximal index in the _vari(j)'s
###					of the scale
###			     variable	the first variable _vari(0): it
###					corresponds to expr
###				  log	a table which gives the logarithm
###					of the _vari(j)'s in terms of the
###					 _vari(j)'s (log[_vari(j)] is
###					log(1/_vari(j))).
###				 back	var in terms of _vari(0).
###
### References            : Richardson, Salvy, Shackell & van der Hoeven.
###			    ISSAC'96.
### 				An asymptotic scale is a finite ordered set
###				{t1,...,tm} of positive exp-log functions
###				tending to 0 such that
###				log ti=o(log ti+1), for i=1,...,m-1.
###
### Other                 :
###                          this procedure should be simplified.
###                          In fact, it is too expensive.
###

# This type-checking procedure should be something like
#`type/SCALE`:=proc( expr ) :: boolean ;
#
#    type(expr,table)
#    and type(expr['varname']    , algebraic   )
#    and type(expr['list']       , list        )
#    and type(expr['index']      , nonnegint   )
#    and type(expr['variable']   , algebraic   )
#    and type(expr['log']        , table       )
#    and type(expr['back']       , algebraic   )
#end proc :                                                      # `type/scale`
#

`type/SCALE` := proc ( expr ) :: boolean :
option ALGOCOPYRIGHT; 
    type(expr,table)  and
    {indices(expr)}={['variable'],['back'],['varname'],['log'],['list'],['point']}
end proc :                                                      # `type/scale`
#----------------------------------------------------------------------------




