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

##    -*-Maple-*-
##
##    Title: 	`multiseries/function`[floor,ceil,frac,round,trunc]
##    Created:	Tue Oct 28 10:32:19 2003
##    Author: 	Bruno Salvy and Ha Le
##		<Bruno.Salvy@inria.fr> <Ha.Le@inria.fr>
##

FUNCTIONTABLE['floor']:=proc( expr,scale,var,ord ) :: {identical(0),SERIES};
option ALGOCOPYRIGHT;
local inf,const,zero,res, expr4res, sgn;

    ASSERT( expr::{'identical'(0),'SERIES',list({identical(0),'SERIES'})} and 
           scale :: 'SCALE' and
           var::list('ScaleVar') and ord::nonnegint);

    if type(expr,list) or expr=0 then return 0 fi;

    inf,const,zero:=`multiseries/splitmultiseries`(args);

    if inf<>0 then
		if op(EXPONBIGO,inf)<>infinity then applyop(floor,EXPR4SERIES,inf)
		else
		    res:=ADDDOIT(inf,const);
		    expr4res:=-'frac'(op(EXPR4SERIES,res));
		    if SIGN(inf)=-1 then expr4res:=expr4res-1 fi;
		    ADDDOIT(res,CONVERT2SERIES(expr4res,scale,var,true))
		fi
    else
    	if type(const,'SERIES') then 
    	    CONVERT2SERIES(procname(const,args[2..-1]),scale,var,true)
    	else
    	    res := floor(const);
    	    if not TESTZERO(res-const) then CONVERT2SERIES(res,scale,var,true)
    	    else
        		# need sign of the zero part:
        		sgn:=SIGN(zero,true);
        		if sgn=-1 or sgn=0 then res:=res-1 fi;
        		CONVERT2SERIES(res,scale,var,true)
    	    fi
    	fi
    fi
end proc:

#------------------------------------------------------------------------------

FUNCTIONTABLE['ceil']:=proc( expr,scale,var,ord ) :: {identical(0),SERIES};
option ALGOCOPYRIGHT;
local fl;
    if type(expr,list) then 0
    else
	fl:=FUNCTIONTABLE['floor'](MULSERIESCST(expr,-1,integer),args[2..-1]);
	if fl=0 then 0
	else MULSERIESCST(fl,-1,integer)
	fi
    fi
end:

#------------------------------------------------------------------------------

FUNCTIONTABLE['trunc']:=proc( expr,scale,var,ord ) :: {identical(0),SERIES};
option ALGOCOPYRIGHT;
local inf,const,zero,res, expr4res, sgn;

    ASSERT( expr::{'identical'(0),'SERIES',list({identical(0),'SERIES'})} and 
           scale :: 'SCALE' and
           var::list('ScaleVar') and ord::nonnegint);

    if type(expr,list) or expr=0 then return 0 fi;

    inf,const,zero:=`multiseries/splitmultiseries`(args);

    if inf<>0 then
	if op(EXPONBIGO,inf)<>infinity then applyop(trunc,EXPR4SERIES,inf)
	else
	    res:=ADDDOIT(inf,const);
	    expr4res:=-'frac'(op(EXPR4SERIES,res));
	    ADDDOIT(res,CONVERT2SERIES(expr4res,scale,var,true))
	fi
    else
	if type(const,'SERIES') then 
	    CONVERT2SERIES(procname(const,args[2..-1]),scale,var,true)
	else
	    res := trunc(const);
	    if not TESTZERO(res-const) then CONVERT2SERIES(res,scale,var,true)
	    elif res<>0 then
		# need sign of the zero part:
		sgn:=SIGN(zero,true);
#		if sgn=-1 or sgn=0 then res:=res-1 fi;
		if sgn<>0 and signum(0,res,1)<>sgn then res:=res+sgn fi;
		CONVERT2SERIES(res,scale,var,true)
	    else 0
	    fi
	fi
    fi
end proc:

#------------------------------------------------------------------------------

FUNCTIONTABLE['frac']:=proc( expr,scale,var,ord ) :: {identical(0),SERIES};
option ALGOCOPYRIGHT;
local inf,const,zero;

    ASSERT( expr::{'identical'(0),'SERIES',list({identical(0),'SERIES'})} and 
           scale :: 'SCALE' and
           var::list('ScaleVar') and ord::nonnegint);

    if type(expr,list) then
	if ADDDOIT(expr[1],-1)=0 then return CONVERT2SERIES(1,args[2..-1])
	else return 0 fi
    fi;
    if expr=0 then return 0 fi;

    inf,const,zero:=`multiseries/splitmultiseries`(args);

    if inf<>0 then const:=ADDDOIT(const,'frac'(op(EXPR4SERIES,inf))) fi;
    if type(const,'SERIES') then 
	ADDDOIT(zero,
		CONVERT2SERIES(procname(const,args[2..-1]),scale,var,true))
    else ADDDOIT(frac(const),zero)
    fi
end proc:

#------------------------------------------------------------------------------

FUNCTIONTABLE['round']:=proc( expr,scale,var,ord ) :: {identical(0),SERIES};
option ALGOCOPYRIGHT;
    if type(expr,list) then 0
    else FUNCTIONTABLE['floor'](ADDDOIT(expr,1/2),args[2..-1])
    fi
end:

