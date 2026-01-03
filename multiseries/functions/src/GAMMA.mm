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
###    Title: 	GAMMA and related functions
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
#------------------------------------------------------------------------------
### Name                  : `multiseries/function`[GAMMA]
### Input                 :
###                          expr   a SERIES data-structure or a list 
###                                 SERIES (for Incomplete Gamma functions)
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :
###                         multiseries expansion of GAMMA(expr)
### References            :
###                          Abramowitz and Stegun
### Error Conditions      :
###                         if expr is not a valid argument for GAMMA or 
###                         Incomplete GAMMA function (too many arguments)
###			    Or if GAMMA is evaluated at an exact 0.
###

FUNCTIONTABLE['GAMMA'] := proc ( expr, scale, var, ord ) :: SERIES :
option ALGOCOPYRIGHT;

local L, i :: integer, a, zlim, pow, n, s,res, pole, im, jump, gammaa, sinpe, sig;

    ASSERT( expr::{'SERIES',identical(0),list({'SERIES',identical(0)})}
	and scale :: 'SCALE' and var::list('ScaleVar') and ord::nonnegint);
    
    if expr=0 then error "singularity encountered, GAMMA(0)" fi;
    if not type(expr,list) then # classical GAMMA function 

	L := LIMIT(expr,scale,var) :
	if L=undefined then error "unable to compute multiseries"
	elif has(L,infinity) then
	    # Stirling's formula can be used if |arg expr|<Pi
	    # hence, we have to check the sign of L

	    if signum(L)=-1 then
		# apply reflection formula (A&S 6.1.17)
		return MULDOIT(RUN(Pi/sin(Pi*expr),args[2..-1]),
				POWER(procname(ADDDOIT(1,
					    MULSERIESCST(expr,-1,'integer')),
					      args[2..-1]),-1,args[2..-1]))
	    end if :

	    EXP(FUNCTIONTABLE['lnGAMMA'](args),args[2..-1])

	elif L=0 then # 0 is a simple pole of GAMMA
	     # use the recurrence formula (A&S 6.1.15)
	     if ISHIDDENZERO(expr,scale,var) then
	     	error "singularity encountered, GAMMA(0)" fi;
	     MULDOIT(procname(ADDDOIT(expr,1),args[2..-1]),
		     POWER(expr,-1,args[2..-1]))
	else
	    pole:=false;
	    if type(L,float) and frac(L)=0 and signum(L)<0 then pole:=true
	    elif is(L,'integer') then
		s:=signum(L);
		pole:=evalb(s=-1);
		if has(s,signum) then error "need to determine the sign of %1",L fi
	    fi;
	    if pole then
		# again a simple pole 
		# this time, we use the reflection formula (A&S 6.1.17)
		# GAMMA(1-expr)GAMMA(expr)=Pi*csc(Pi*expr)
		sinpe:=RUN('sin'(Pi*expr),args[2..-1]);
		if op(LISTEXPON,sinpe)<>[] and op([LISTEXPON,1],sinpe)=0 then
		    sinpe:=subsop(LISTCOEFF=subsop(1=NULL,op(LISTCOEFF,sinpe)),
			LISTEXPON=subsop(1=NULL,op(LISTEXPON,sinpe)),sinpe)
		fi;
		RUN(Pi/sinpe/'GAMMA'(1-expr),args[2..-1])
	    else # we treat GAMMA as an analytic function
		ANALYTIC(GAMMA,args[1..-1])
	    fi
	fi

     elif type(expr,list) and nops(expr)=2 then
	# incomplete GAMMA function (ref.: A&S 6.5)
	# THIS CODE SHOULD BE REORGANIZED.
 	zlim:=LIMIT(expr[2],scale,var);
	if zlim=0 then
	    if ISHIDDENZERO(expr[2],scale,var) then # it's not incomplete
		return procname(expr[1],args[2..-1])
	    fi;
	    if expr[1]=0 or
		(op(LISTEXPON,expr[1])=[0] and op(EXPONBIGO,expr[1])=infinity)
	    then
		if expr[1]=0 then a:=0
		else a:=CONVERT2POLYNOM(expr[1]) fi; # a does not depend on z
		if not is(a,nonposint) then  # easy case of 6.5.29
		    res:=ADDDOIT(GAMMA(a),MULSERIESCST(MULDOIT(
			POWER(expr[2],a,args[2..-1]),
			COMPOSE('SERIES'(scale,[seq((-1)^i/i!/(a+i),i=0..ord-1)],
			    1,WHATTYPE(a),[$0..ord-1],ord,'integer',
			    scale['variable'],
			    hypergeom([a],[a+1],-scale['variable'])),expr[2],
			    args[2..-1])),-1,'integer'));
		    ## Branch cut:
		    if not is(a,integer) and
			SIGN(DOMINANTTERM(expr[2],scale,var))=-1 then
			im:=SIGN(Im(expr[2]),true);
			if im<>0 and im<>1 then
			    # GAMMA(a)*(exp(-2*I*Pi*a)-1)
			    jump:=FUNCTIONTABLE['GAMMA'](a,args[2..-1]);
			    jump:=MULDOIT(jump,ADDDOIT(EXP(MULSERIESCST(a,
				    -2*I*Pi,'algebraic'),args[2..-1]),-1))
			fi;
			if im=-1 then res:=ADDDOIT(res,jump)
			elif has(im,signum) then # signum still in im
			    res:=ADDDOIT(res,MULSERIESCST(jump,(1-im)/2,'algebraic'))
			fi
		    fi;
		    subsop(EXPR4SERIES='GAMMA'(a,op(EXPR4SERIES,expr[2])),res)
		else # A&S 6.5.19 nonposint
		    n:=-a; # makes it easier to read formulae
		    s:=min(n-1,ord);
		    if has(s,min) then
			error "need to determine the sign of %1",n-1-ord
		    fi;
		    if a=0 then res:=0
		    elif s<>n-1 then
			res:='SERIES'(scale,[seq((-1)^i*(n-1-i)!/n!,
			    i=0..ord-1)],1,rational,[$0..ord-1],ord,'integer',
			    scale['variable'],
			    add((-1)^i*(n-1-i)!/n!*scale['variable']^i,
				i=0..ord-1)+O(scale['variable']^ord))
		    else
			res:='SERIES'(scale,[seq((-1)^i*(n-1-i)!/n!,
			    i=0..n-1)],0,rational,[$0..n-1],infinity,'integer',
			    scale['variable'],
			    add((-1)^i*(n-1-i)!/n!*scale['variable']^i,
				i=0..n-1))
		    fi;
		    if a<>0 then
		    res:=MUL([POWER(expr[2],-n,args[2..-1]),
			EXP(MULSERIESCST(expr[2],-1,'integer'),args[2..-1]),
			COMPOSE(res,expr[2],args[2..-1])])
		    fi;
		    if s=n-1 then
			res:=ADDDOIT(res,MULSERIESCST(
			    FUNCTIONTABLE['Ei'](
				MULSERIESCST(expr[2],-1,'integer'),
				    scale,var,ord-(n-1)),
			    -(-1)^n/n!,'rational'))
		    fi;
		    # Branch cut:
		    if SIGN(DOMINANTTERM(expr[2],scale,var))=-1 then
			im:=SIGN(Im(expr[2]),true);
			if im<>0 and im<>1 then
			    jump:=(-1)^(n-1)*2*I*Pi/(n)!
			fi;
			if im=-1 then res:=ADDDOIT(res,jump)
			elif has(im,signum) then res:=ADDDOIT(res,jump*(1-im)/2)
			fi
		    fi;
		    subsop(EXPR4SERIES=GAMMA(a,op(EXPR4SERIES,expr[2])),res)
		fi
	    else # AS 6.5.29 again, but with more computation
		pow[0]  := 1 :
		for i to ord do
		 pow[i] := MULDOIT(pow[i-1],expr[2])
		end do :
		L:=RUN(add((-1)^(i+1)*pow[i]/i!/(i+expr[1]),i=0..ord),
		    args[2..-1]);
		# then, we retrieve GAMMA(expr[1],expr[2])
		res:=ADDDOIT(procname(expr[1],args[2..-1]), # GAMMA(a)
			MULDOIT(EXP(MULDOIT(expr[1],LN(expr[2],args[2..-1])),
				      args[2..-1]),L));
		## Branch cut (same as above):
		if SIGN(DOMINANTTERM(expr[2],scale,var))=-1 then
		    im:=SIGN(Im(expr[2]),true);
		    if im<>0 and im<>1 then
			# GAMMA(a)*(exp(-2*I*Pi*a)-1)
			jump:=FUNCTIONTABLE['GAMMA'](expr[1],args[2..-1]);
			jump:=MULDOIT(jump,ADDDOIT(EXP(MULSERIESCST(expr[1],
				-2*I*Pi,'algebraic'),args[2..-1]),-1))
		    fi;
		    if im=-1 then res:=ADDDOIT(res,jump)
		    elif has(im,signum) then # signum still in im
			res:=ADDDOIT(res,MULSERIESCST(jump,(1-im)/2,'algebraic'))
		    fi
		fi;
		subsop(EXPR4SERIES=GAMMA(op(map2(op,EXPR4SERIES,expr))),res)
	    fi
	elif not has(zlim,infinity) then # analytic
	    res:=ANALYTIC(GAMMA,args);
	    # Branch cut (again, the same).
	    # Three cases depending on a.
	    if expr[1]<>0 and
		    (op(LISTEXPON,expr[1])=[0] and op(EXPONBIGO,expr[1])=infinity)
		then a:=CONVERT2POLYNOM(expr[1])
	    elif expr[1]=0 then a:=0 
	    fi;
	    if assigned(a) and is(a,nonposint) then
    		sig:=SIGN(DOMINANTTERM(expr[2],scale,var));
    		if sig=-1 or has(sig,signum) then 
                sig:=(1-sig)/2;
        		im:=SIGN(Im(expr[2]),true);
        		if im<>0 and im<>1 then
        		    jump:=sig*(-1)^(-a)*2*I*Pi/(-a)!
        		fi;
        		if im=-1 and jump<>0 then res:=ADDDOIT(res,jump)
        		elif has(im,signum) then # signum still in im
        		    res:=ADDDOIT(res,jump*(1-im)/2)
        		fi
            fi
	    elif (not assigned(a) or not is(a,posint)) then
    		sig:=SIGN(DOMINANTTERM(expr[2],scale,var));
    		if sig=-1 or has(sig,signum) then
    		    sig:=(1-sig)/2;
        		im:=SIGN(Im(expr[2]),true);	    
        		if im<>0 and im<>1 then
        		    # (GAMMA(a,z)-GAMMA(a))*(exp(-2*I*Pi*a)-1)
        		    gammaa:=FUNCTIONTABLE['GAMMA'](expr[1],args[2..-1]);
        		    jump:=ADDDOIT(res,MULSERIESCST(gammaa,-1,'integer'));
        		    jump:=MULDOIT(jump,ADDDOIT(EXP(MULSERIESCST(expr[1],
        			    -2*I*Pi,'algebraic'),args[2..-1]),-1));
        			jump:=MULSERIESCST(jump,sig,`if`(type(sig,integer),integer,algebraic))
        		fi;
        		if im=-1 then res:=ADDDOIT(res,jump)
        		elif has(im,signum) then # signum still in im
        		    res:=ADDDOIT(res,MULSERIESCST(jump,(1-im)/2,'algebraic'))
        		fi
            fi
	    fi;
	    if a=0 then
		    subsop(EXPR4SERIES='GAMMA'(0,op(EXPR4SERIES,expr[2])),res)
	    else
		    subsop(EXPR4SERIES='GAMMA'(op(map2(op,EXPR4SERIES,expr))),res)
	    fi
	elif expr[1]=0 or 
	    op(LISTEXPON,expr[1])=[0] and op(EXPONBIGO,expr[1])=infinity then
	    # asymptotic expansion A&S 6.5.32
	    if expr[1]=0 then a:=0 else a:=CONVERT2POLYNOM(expr[1]) fi;
	    subsop(EXPR4SERIES='GAMMA'(a,op(EXPR4SERIES,expr[2])),
		MUL([EXP(MULSERIESCST(expr[2],-1,'integer'),args[2..-1]),
		     POWER(expr[2],a-1,args[2..-1]),
		     FUNCTIONTABLE[`multiseries/GAMMAaux`](a,
			POWER(expr[2],-1,args[2..-1]),args[2..-1])]))
	else error "unable to compute series";
	fi

     else error "GAMMA expects 1 or 2 arguments, got %1",nops(expr)
     end if 

end proc:	                     # `multiseries/function`['GAMMA']
#------------------------------------------------------------------------------

FUNCTIONTABLE['lnGAMMA'] := proc ( expr, scale, var, ord) :: SERIES :
option ALGOCOPYRIGHT;
local res,coef, L;
    ASSERT(expr::'SERIES' and scale :: 'SCALE' and var::list('ScaleVar') and
	ord::nonnegint);
    L:=LIMIT(expr,scale,var);
    if L=undefined then error "unable to compute multiseries"
    elif has(L,infinity) then
	# asymptotic expansion of ln(GAMMA(expr)) (A&S 6.1.41)
	subsop(EXPR4SERIES=lnGAMMA(op(EXPR4SERIES,expr)),
	  ADD([MULDOIT(ADDDOIT(expr,-1/2),LN(expr,args[2..-1])),
	    MULSERIESCST(expr,-1,'integer'),ln(2*Pi)/2,
	    # Compose expr with a SERIES involving Bernoulli numbers.
	    FUNCTIONTABLE[`multiseries/lnGAMMAaux`](
		POWER(expr,-1,args[2..-1]),args[2..-1])]))
    else # compose with GAMMA
	res:=subsop(EXPR4SERIES=lnGAMMA(op(EXPR4SERIES,expr)),
	    LN(FUNCTIONTABLE['GAMMA'](args),args[2..-1]));
	if op(LISTCOEFF,res)<>[] and
	    typematch(op([LISTCOEFF,1],res),'ln'('GAMMA'(coef::anything))) then
	    # normal is used here to avoid GAMMA entering the coefficients,
	    # which used to end up in an infinite loop
	    res:=applyop(normal,LISTCOEFF,
		subsop([LISTCOEFF,1]=lnGAMMA(coef),res))
	fi;
	res
    fi
end proc:	                     # `multiseries/function`['lnGAMMA']
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
### Name                  : `multiseries/function`[Psi]
### Input                 : 
###                          expr   a SERIES data-structure or a list 
###                                 SERIES (for polygamma functions)
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :
###                         multiseries expansion of Psi(expr)
### References            :
###                          Abramowitz and Stegun
###

FUNCTIONTABLE['Psi'] := proc ( expr, scale, var, ord) :: SERIES :
option ALGOCOPYRIGHT;
local L, ind, todiff, tosubs, pt, v, res, sc0, v0, tmpvar;

    ASSERT( expr::{'SERIES',identical(0),list({'SERIES',identical(0)})}
	and scale::'SCALE' and var::list('ScaleVar') and ord::nonnegint );

    if expr=0 then error "singularity encountered: Psi(0)" fi;
    if type(expr,'SERIES') then # classical Psi function 
	L := LIMIT(expr,scale,var) :
	if L=undefined then error "unable to compute multiseries"
	elif has(L,infinity) then
	    # We use A&S 6.3.18. It can be used if |arg expr|<Pi
	    # hence, we have to check the sign of L

	    if signum(L)=-1 then # reflection formula (A&S 6.3.7)
		ADDDOIT(RUN(-Pi/'tan'(Pi*expr),args[2..-1]),
			procname(MULSERIESCST(expr,-1,'integer'),args[2..-1]))
	    else
		subsop(EXPR4SERIES='Psi'(op(EXPR4SERIES,expr)),
		   ADD([LN(expr,args[2..-1]),
		    # compose expr with a SERIES involving Bernoulli's number
		   FUNCTIONTABLE[`multiseries/Psiaux`](
		    POWER(expr,-1,args[2..-1]),args[2..-1])]))
	    fi
	elif L=0 then # recurrence formula (A&S 6.3.5)
	    if ISHIDDENZERO(expr,scale,var) then 
	    	 error "singularity encountered: Psi(0)" fi;
	    ADDDOIT(procname(ADDDOIT(1,expr),args[2..-1]),
		 MULSERIESCST(POWER(expr,-1,args[2..-1]),-1,'integer'))
	elif (is(L,'integer') or (type(L,float) and frac(L)=0)) and signum(L)<0
	then # reflection formula (A&S 6.3.7)
	    L := ADDDOIT(1,MULSERIESCST(expr,-1,'integer')) ;
	    ADDDOIT(RUN(-Pi/tan(Pi*expr),args[2..-1]),
		   procname(L,args[2..-1]))
	else # treat Psi as an analytic function
	    ANALYTIC(Psi,args[1..-1])
	end if
    else # derivatives of Psi
	ind:=`multiseries/Series2Expr`(expr[1]);
	if not type(ind,nonnegint) then # rewrite in terms of 
		# Hurwitz zeta.
	    return RUN((Zeta(1,ind+1,expr[2])+(gamma+Psi(-ind))*Zeta(0,ind+1,expr[2]))/GAMMA(-ind),args[2..-1])
	fi;
	tosubs:=expr[2];
	L:=LIMIT(tosubs,scale,var); 
	if L=undefined then error "unable to compute multiseries" fi;
	sc0:=newscale(v0,0);
	v:=sc0['variable'];
	if has(L,infinity) then
	    if signum(L)=-1 then
		return ADDDOIT(RUN(subs(tmpvar=tosubs,
		    diff(Pi/tan(-Pi*tmpvar),[tmpvar$ind])),args[2..-1]),
		    MULSERIESCST(procname(applyop(MULSERIESCST,2,expr,-1,'integer'),
			args[2..-1]),(-1)^ind,'integer'))
	    else
		tosubs:=POWER(tosubs,-1,args[2..-1]);
		pt:='SERIES'(sc0,[1],0,algebraic,[-1],infinity,'integer',v,1/v)
	    fi
	else
	    tosubs:=ADDDOIT(tosubs,-L);
	    # same here 
	    pt:=SERIES(sc0,[L,1],0,algebraic,[0,1],infinity,'integer',v,L+v)
	fi;
	todiff:=procname(pt,sc0,[v],max(ord,1)); # max to get rid of ln(x)
	# Except for a possible log, this should be a power series which
	# can be given as a first argument to COMPOSE once it has been
	# converted into a power series.
	# We cannot use diff/SERIES directly: the result depends on scale['varname'],
	# since it is also differentiated (chain rule). The same problem occurs in Zeta.
	if not has(L,infinity) then 
	    res:=diff(todiff,[v0$ind])
	else 
	    res:=todiff;
	    to ind do 
		res:=diff(res,v0);
		res:=MULDOIT(res,SERIES(sc0,[-1],0,'integer',[2],infinity,'integer',v,-v^2))
	    od
	fi;
	COMPOSE(res,tosubs,args[2..-1])
    end if
end proc :                                 # `multiseries/function`[Psi]
#------------------------------------------------------------------------------

`multiseries/lnGAMMAaux`:=proc(x)
option ALGOCOPYRIGHT;
    lnGAMMA(1/x)+ln(x)/x+1/x-ln(2*Pi)/2-1/2*ln(x)
end:

# It is assumed that expr tends to 0
FUNCTIONTABLE[`multiseries/lnGAMMAaux`]:=proc(expr,scale,var,ord)::SERIES:
option ALGOCOPYRIGHT;
local i;
    ASSERT(expr::'SERIES' and scale::'SCALE' and var::list('ScaleVar')
	and ord::nonnegint);
    ASSERT(LIMIT(expr,scale,var)=0,"unexpected limit in argument");
    COMPOSE('SERIES'(scale,[seq(bernoulli(2*i)/(2*i)/(2*i-1),i=1..ord)],
	1,'rational',[seq(2*i-1,i=1..ord)],2*ord+1,'integer',
	scale['variable'],`multiseries/lnGAMMAaux`(scale['variable'])),args)
end:

`multiseries/Psiaux`:=proc(x) 
option ALGOCOPYRIGHT;
    Psi(1/x)+ln(x) 
end:

FUNCTIONTABLE[`multiseries/Psiaux`]:=proc(expr,scale,var,ord)::SERIES:
option ALGOCOPYRIGHT;
local i;
    ASSERT(expr::'SERIES' and scale::'SCALE' and var::list('ScaleVar')
	and ord::nonnegint);
    ASSERT(LIMIT(expr,scale,var)=0,"unexpected limit in argument");
    COMPOSE('SERIES'(scale,[-1/2,seq(-bernoulli(2*i)/(2*i),i=1..ord)],1,
	'rational',[1,2*i$i=1..ord],2*(ord+1),'integer',scale['variable'],
	`multiseries/Psiaux`(scale['variable'])),args)
end:

`multiseries/GAMMAaux`:=proc(a,z)
option ALGOCOPYRIGHT;
    GAMMA(a,z)/z^(a-1)/exp(-z)
end:

FUNCTIONTABLE[`multiseries/GAMMAaux`]:=proc(a,expr,scale,var,ord)::SERIES:
local i, j;
    ASSERT(expr::'SERIES' and scale::'SCALE' and var::list('ScaleVar')
	and ord::nonnegint);
    ASSERT(LIMIT(expr,scale,var)=0,"unexpected limit in argument");
    COMPOSE('SERIES'(scale,[seq(mul(a-j,j=1..i),i=0..ord-1)],1,
	WHATTYPE(a),[$0..ord-1],ord,'integer',SCALEVARIABLE,
	`multiseries/GAMMAaux`(a,SCALEVARIABLE)),args[2..-1])
end:
