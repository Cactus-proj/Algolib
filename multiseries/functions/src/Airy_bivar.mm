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
###    Title: 	Airy
###    Created:	Jan 2004
###    Author: 	Bruno Salvy & Ludovic Meunier
###    Contact: 	Bruno.Salvy@inria.fr
###
### This is loaded by MultiSeries.mpl after AiryAi.mm, AiryBi.mm, DAiryAi.mm,
### DAiryBi.mm.
### It redefines `multiseries/function`[AiryAi] & 
### `multiseries/function`[AiryBi] so as to deal with 
### the bivariate case as well. 
###

`multiseries/function/AiryAiunivar`:=eval(FUNCTIONTABLE[AiryAi]):
`multiseries/function/AiryBiunivar`:=eval(FUNCTIONTABLE[AiryBi]):

FUNCTIONTABLE['AiryAi']:=proc (expr, scale, var, order)
local k, f, x, xx;
option ALGOCOPYRIGHT;
    if not type(expr,list) then # one argument case
	`multiseries/function/AiryAiunivar`(args)
    else
	k:=expr[1];
	if k=0 then procname(expr[2],args[2..-1])
	else k:=`multiseries/Series2Expr`(k)
	fi;
	if not type(k,nonnegint) then
	    error "Cannot expand AiryAi with first argument %1",k fi;
	if k=1 then FUNCTIONTABLE[`D(AiryAi)`](expr[2],args[2..-1])
	else # rewrite in terms of Ai and Ai'
	    f:=collect(eval(AiryAi(k,x)),AiryAi);
	    xx:=expr[2];
	    RUN(subs(x=xx,coeff(f,AiryAi(x)))*
		FUNCTIONTABLE[AiryAi](xx,args[2..-1])+
		    coeff(f,AiryAi(1,x))*
    		FUNCTIONTABLE[`D(AiryAi)`](xx,args[2..-1]),args[2..-1])
	fi
    fi
end:

FUNCTIONTABLE['AiryBi']:=proc (expr, scale, var, order)
local k, f, x, xx;
option ALGOCOPYRIGHT;
    if not type(expr,list) then # one argument case
	`multiseries/function/AiryBiunivar`(args)
    else
	k:=expr[1];
	if k=0 then procname(expr[2],args[2..-1])
	else k:=`multiseries/Series2Expr`(k)
	fi;
	if not type(k,nonnegint) then
	    error "Cannot expand AiryBi with first argument %1",k fi;
	if k=1 then FUNCTIONTABLE[`D(AiryBi)`](expr[2],args[2..-1])
	else # rewrite in terms of Ai and Ai'
	    f:=collect(eval(AiryBi(k,x)),AiryBi);
	    xx:=expr[2];
	    RUN(subs(x=xx,coeff(f,AiryBi(x)))*
		FUNCTIONTABLE[AiryBi](xx,args[2..-1])+
		    coeff(f,AiryBi(1,x))*
    		FUNCTIONTABLE[`D(AiryBi)`](xx,args[2..-1]),args[2..-1])
	fi
    fi
end:
