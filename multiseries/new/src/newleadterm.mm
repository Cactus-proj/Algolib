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
###    Title: 	LeadingTerm
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Description: interface to multiseries that returns the leading term of
### an expression at a point.

LeadingTerm:=proc (f,eqn::{name=algebraic,name})
local lead, res, var;
    if not assigned(_EnvBranchCut) then _EnvBranchCut:=true fi;
    try
	try
	    res:=`multiseries/multiseries`(args,1,'exact_order');
	catch "multiseries encountered a difficulty in expanding":
	    res:=`multiseries/multiseries`(args,Order,'exact_order');
	end try;
	lead:=LEADTERM(res);
	if lead = 0 then return 0
	else
	    res:=`multiseries/Expr4Series2Expr`(
		    CONVERT2POLYNOM(lead),op(THESCALE,lead));
		if has(res,_var) then 
		    return subs([seq(var=op(var),var=indets(res,specindex(anything,_var)))],res)
		else return res
		fi
	fi
    catch: error
    finally
	`multiseries/cleanremember`()
    end try;
end: # LeadingTerm

