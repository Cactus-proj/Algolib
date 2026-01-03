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
###    Title: 	`multiseries/getoneterm`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###			     expr    an expression coming from an EXPR4SERIES
###			     scale   a  scale
###                          var     a  list of ScaleVar data-structure
### Output                :  the first term of the expansion of expr
###			     with respect to var.
### Comment: we cannot use DOIT with order 1 directly here because we want
### to provide the user with some control via the variable Order.

`multiseries/getoneterm`:=proc(expr,scale,var)
local res;
option ALGOCOPYRIGHT;
    try
	res:=DOIT(expr,scale,var,1,'exact_order');
    catch "multiseries encountered a difficulty in expanding":
	res:=DOIT(expr,scale,var,Order,'exact_order');
    end try;
    if res<>0 and op(LISTCOEFF,res)=[] then# this happens with O() in the input
	# give it another chance, first
	res:=DOIT(expr,scale,var,Order,'exact_order');
	if res<>0 and op(LISTCOEFF,res)=[] then # didn't work
	    if op(COEFFBIGO,res)=0 and op(EXPONBIGO,res)=infinity then res:=0 
	    else
	        error "unable to compute series"
	    fi
	fi
    fi;
    res
end:	# `multiseries/getoneterm` 

