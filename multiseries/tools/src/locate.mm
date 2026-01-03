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
###    Title: 	`multiseries/locate`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input : a list of exponents, an expression (a bigO) and a comparison
###         procedure
### Output : the index of the last element in the list which is < expr;
###	     0 if none are.
### Todo   : improve efficiency
### Description: 
### This is used in `multiseries/AddSeries` when the
### truncation order is determined.
### It should be moved outside of multiseries.


`multiseries/locate` := proc(TheList::list, expr, comp::procedure)::integer:
option ALGOCOPYRIGHT;

local j :: integer :

## old version
#    for j from nops(TheList) to 1 by -1 while not comp(TheList[j],expr) do od;
#    j
## new version (BS, Aug 2003)
     for j from nops(TheList) to 1 by -1 while comp(expr,TheList[j]) do od;
     if j=0 then 0
     elif comp(TheList[j],expr) then j
     elif TheList[j]=expr or Testzero(TheList[j]-expr) then j-1
     else error "need to determine the sign of %1",expr-TheList[j]
     fi

end proc : # `multiseries/locate`
