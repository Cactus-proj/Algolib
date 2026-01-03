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
###    Title: 	`multiseries/convert2SERIES`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###                          expr	a SERIES or a coefficient
###                          scale	a SCALE data-structure
###                          var        a list(ScaleVar) data-structure
###			     recurse	a boolean determining whether only
###					the variables in var should be used
###					in the resulting SERIES or slowest
###					variables used in expr (when it is a
###					series) can appear. 
### Output                :
###                          A multiseries with respect to the variables
###			     in var that encodes expr.
### Description           :
###                          If expr is not a series, it is supposed to
###			     be a constant w.r.t var.
###
###			     When recurse is true, expansions in variables 
###			     slower than var[1] are replaced by their
###			     EXPR4SERIES field.
###
###                          If the type of expr is not recognized, it is
###                          assumed to be 'algebraic'.
###
###			     If var=[] the expression itself is returned.
###
###

`multiseries/Convert2SERIES`:=proc(expr, scale, var, recurse)
option ALGOCOPYRIGHT;
local v, res, newvar;
    ASSERT(scale::'SCALE' and var::list('ScaleVar') and recurse::boolean);
    if expr = 0 then return 0 fi;
    if var=[] then
	if type(expr,'SERIES') then return op(EXPR4SERIES,expr)
	else return expr fi
    fi;
    newvar:=var;
    res:=expr;
    if type(expr,'SERIES') then
	v:=op(EXPANVAR,expr);
	if v = var[-1] then newvar:=subsop(-1=NULL,var)
	elif var[-1] = FASTEST([v,var[-1]],scale) then
	    res:='SERIES'( scale,[expr],0,'t_SERIES',[0],'infinity',
                      'integer', var[-1], op(EXPR4SERIES, expr) );
	    newvar:=subsop(-1=NULL,var)
        end if 
    # This breaks too many things (like sum(.,k=0..infinity) )
    # elif has(expr,infinity) then error "invalid input, %1", expr
    else res:='SERIES'(scale,[expr],0,WHATTYPE(expr),[0],'infinity',
                  'integer',var[-1],expr); newvar:=subsop(-1=NULL,var)
    end if;

    if newvar<>[] or (recurse and 
	    member(op(TYPECOEFF,res),{algebraic,'t_SERIES'})) then
	res:=subsop(LISTCOEFF=map(procname,op(LISTCOEFF,res),scale,newvar,
	    recurse),res);
	if newvar<>[] then res:=subsop(TYPECOEFF='t_SERIES',res)
	else res:=subsop(TYPECOEFF=WHATTYPE(op(EXPR4SERIES,res)),res) fi;
    fi;

    res

end proc :                                       # `multiseries/Convert2SERIES`
#------------------------------------------------------------------------------

