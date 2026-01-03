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
###    Title: 	`multiseries/function`[diff]
###    Created:	Oct 2003
###    Author: 	Bruno Salvy
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###			     lexpr list consisting of
###                            expr a SERIES data-structure
###			       name SERIES for the variable wrt
###					 one differentiates
###				    or list of variable names
###                          scale  a SCALE data-structure
###			     var    list of variables in the scale
###			     ord    order.
### Output                :
###                          the multiseries expansion of diff(expr,name)
###

FUNCTIONTABLE['diff'] := proc( lexpr, scale,var,ord) :: {SERIES,identical(0)}:
option ALGOCOPYRIGHT;
local expr,lvars,vname;
    expr:=lexpr[1];
    lvars:=lexpr[2];
    if lvars=[] then expr
    elif type(lvars,list) then vname:=lvars
    elif op(EXPR4SERIES,lvars)=SCALEBACK then vname:=SCALEVARNAME
    else vname:=op(EXPR4SERIES,lvars) fi;
    diff(expr,vname) # calls diff/SERIES
end: # `multiseries/function`['diff']

FUNCTIONTABLE['Diff']:=FUNCTIONTABLE['diff']:
