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
###    Title: 	newseries
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Description: Interface to multiseries, with the same syntax as series.
### Moreover, the result of newseries will be returned as a series data
### structure when series would do it. 
### Ultimately, we want to allow the user typing 
### 	series:=newseries;
###
### This oldseries is necessary to create the series data structure below.

oldseries:=eval(:-series):

# Implement a "smart" remember mechanism:
# - react to a change of one of the environment variables Order or Testzero
# - independent of the series variable (should speed up evalf/Int)
#   by remembering in terms of the dummy variable MultiSeries:-x
series := proc(expr::algebraic, eqn::{name=algebraic,name},
               n::{nonnegint,identical(infinity)}:=Order)
local res, xx, x;
   xx := op(1,eqn):
   x := _x: # this is the local MultiSeries:-_x
   res := `multiseries/newseries/remember`(subs(xx=x,expr), subs(xx=x,eqn),
                                           n, eval(Testzero,1)):
   if res<>FAIL then
      subs(x=xx,res)
   else
      res := `multiseries/newseries`(args):
      `multiseries/newseries/remember`(subs(xx=x,expr), subs(xx=x,eqn),
                                       n, eval(Testzero,1)) := subs(xx=x,res):
      res
   end if:
end proc:

`multiseries/newseries/remember` := proc(expr, eqn, n, tz)
option cache;
   FAIL
end proc:

`multiseries/newseries` := proc(expr, eqn, n)
local s, hasbigo;    
    if # ( type(eqn,name) and type(expr,polynom(algebraic,eqn)) ) or
       # Laurent polynomials are treated here:
	((type(eqn,name) and degree(expr,eqn)<>FAIL) or
       	 ( type(eqn,name=algebraic) and type(expr,polynom(algebraic,op(1,eqn))) ) or
         ( type(expr,':-series') ) or
         ( type(expr,{`+`,`*`}) and
           type([op(expr)],list(`MultiSeries:-series`)) or
	 type(expr,`MultiSeries:-series`^posint) ) )
	# added "not has(eqn,infinity)" to avoid an infinite loop
	# in series(y^2+1,y=infinity)
        and not has(expr,'leadterm') and not has(eqn,infinity)
	 then

	 return oldseries(args)
    end if;

    # change the variable to prevent it being evaluated later
    # e.g. G:=sin(theta); newseries('G','G',infinity);
#    if type(eqn,`=`) then globx:=op(1,eqn) else globx:=eqn fi;
#    s:=multiseries(op(subs(globx=x,[args])));
    s:=multiseries(args);
    if s=0 then 0
#    else subs(x=globx,`multiseries/newseries/recurse`(
#	s,op(THESCALE,s),'hasbigo',not has(eqn,infinity)))
    else `multiseries/newseries/recurse`(
	s,op(THESCALE,s),'hasbigo',not has(eqn,infinity))
    fi
end: # newseries

`multiseries/newseries/recurse`:=proc(s,scale,hasbigo,useseries)
local fact, res, i, lexpons, lcoeffs, var, x, coeffbigo, tmpvar, coeffvar, oldlcoeffs;
    ASSERT(type(s,'SERIES') and type(hasbigo,name)
	    and type(useseries,boolean));
#    if eval(op(EXPANVAR,s))<>scale['variable'] then
    if op(EXPANVAR,s)<>SCALEVARIABLE then
    	if op(LISTEXPON,s)=[] then res:=0
    	else
    	    fact:=op(EXPANVAR,s)^op([LISTEXPON,1],s);
    	    # remove terms that are not related to the dominant behaviour wrt x
    	    res:=procname(op([LISTCOEFF,1],s),scale,hasbigo,
    		useseries and evalb(fact=1))
    		    *`multiseries/Expr4Series2Expr`(fact,scale);
    	    if not eval(hasbigo) and useseries and fact=1 then
    		    res:=convert(res,polynom) fi;
    	fi;
    	# assigned(hasbigo) added BS. Sep 04.
    	# fixes series(O(exp(1/x)),x,3);
        if assigned(hasbigo) and eval(hasbigo) then res
	    elif nops(op(LISTEXPON,s))>1 then
	        res+procname(subsop(LISTCOEFF=[op([LISTCOEFF,2..-1],s)],
		    LISTEXPON=[op([LISTEXPON,2..-1],s)],s),scale,'hasbigo',false)
	    elif op(COEFFBIGO,s)<>0 then
	        if op(TYPECOEFF,s)='t_SERIES' then
		        coeffbigo:=procname(op(COEFFBIGO,s),scale,'hasbigo',false)
	        else
		        coeffbigo:=O(`multiseries/Expr4Series2Expr`(
		            op(COEFFBIGO,s),scale))
	        fi;
	        hasbigo:=true;
	        res+coeffbigo*`multiseries/Expr4Series2Expr`(
		        op(EXPANVAR,s)^op(EXPONBIGO,s),scale)
	    else res
	    fi
    elif useseries and op(TYPECOEFF,s)<>'t_SERIES' 
        and (op(TYPEEXPON,s)=integer 
	    # some simplifications may have got unnoticed
	        or type(op(LISTEXPON,s),list(integer)))
	    and type(op(EXPONBIGO,s),{integer,identical(infinity)}) then
	    lexpons:=op(LISTEXPON,s);
	    var:=`multiseries/Expr4Series2Expr`(SCALEVARIABLE,scale);
	    lcoeffs:=map(Normalizer,	## This is a TEMPORARY change to be
	        map(`multiseries/Expr4Series2Expr`,## closer to the old series.
	        op(LISTCOEFF,s),scale));
	    if has(lcoeffs,var) and has(lcoeffs,exp) and has(indets(lcoeffs,specfunc(anything,'exp')),I) then
	        oldlcoeffs:=lcoeffs;
	        lcoeffs:=map(convert,lcoeffs,trig);
	        if lcoeffs<>oldlcoeffs then lcoeffs:=map(expand,lcoeffs) fi
	    fi;	
	    coeffbigo:=`multiseries/Expr4Series2Expr`(op(COEFFBIGO,s),scale);
	    hasbigo:=evalb(coeffbigo<>0);
	    if scale[point]=0 then coeffvar:=1
	    else coeffvar:=-1/scale[point] fi;
	    var:=expand(var/coeffvar);
	    # need a special case because .3^0 = 1. <> 1 and
	    # this breaks signum
	    if lexpons=[0] then 
	        subs([x=lcoeffs[1],tmpvar=var],oldseries(x+
	    	    `if`(coeffbigo<>0,O(1)*coeffbigo*tmpvar^op(EXPONBIGO,s),0),
	    	    tmpvar,infinity))
	    elif coeffbigo=0 then
	        subs([seq(x[i]=lcoeffs[i],i=1..nops(lcoeffs)),tmpvar=var],
	            oldseries(add(x[i]*coeffvar^lexpons[i]*tmpvar^lexpons[i],
        		i=1..nops(lcoeffs)),tmpvar,infinity))
	    else
            subs([seq(x[i]=lcoeffs[i],i=1..nops(lcoeffs)),tmpvar=var,O(1)=O(coeffbigo)],
	            oldseries(add(x[i]*coeffvar^lexpons[i]*tmpvar^lexpons[i],
        		i=1..nops(lcoeffs))+O(1)*tmpvar^op(EXPONBIGO,s),
        	    tmpvar,infinity))
	    fi
    else
	    hasbigo:=evalb(op(COEFFBIGO,s)<>0);
	    lcoeffs:=map(Normalizer,	## Same as above
	        map(`multiseries/Expr4Series2Expr`,
	        op(LISTCOEFF,s),scale));
	    if has(lcoeffs,SCALEVARNAME) and has(lcoeffs,exp)  and has(indets(lcoeffs,specfunc(anything,'exp')),I) then
	        oldlcoeffs:=lcoeffs;
	        lcoeffs:=map(convert,lcoeffs,trig);
	        if lcoeffs<>oldlcoeffs then lcoeffs:=map(expand,lcoeffs) fi
	    fi;	
	    `multiseries/Expr4Series2Expr`(`multiseries/Series2sumofprod`(
	        subsop(LISTCOEFF=lcoeffs,s),false),scale)
    fi
end: # `multiseries/newseries/recurse`
