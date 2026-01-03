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
###    Title: 	Series2Expr
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###				ser 	a SERIES data-structure
### Output                :
###				an equivalent expression as a function of 
###                             op(THESCALE,ser)[varname]
### Description           :
###                         this procedure allows to retreive the exact
###                         expression stored in op(EXPR4SERIES,ser)
###                         where elements in the asymptotic basis (_vari(j))
###                         are replaced by their exact expression.
###
###
###

`multiseries/Series2Expr` := proc (s)
option ALGOCOPYRIGHT;
    ASSERT(s :: {'SERIES',identical(0)});
    if s=0 then 0 else
	`multiseries/Expr4Series2Expr`(op(EXPR4SERIES,s),op(THESCALE,s))
    fi
end proc:                                   # `multiseries/Series2Expr`

#----------------------------------------------------------------------------
`multiseries/Expr4Series2Expr` := proc (expr,scale)
option ALGOCOPYRIGHT;
local i ;
    subs([seq(i=op(i),i=eval(scale['list'],1))],expr)
end:

