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

## Input:	pol		a polynomial in one variable x with coefficients of type rational ()
##                              or complex(rational)
##		x		the variable
## Output:	a list of those roots of pol that have the smallest modulus
##		
## Description: special case for degree 1 or 2 where the polynomial is solved symbolically,
## a combination of algebraic and numerical computation otherwise.

infsolvepoly := proc (p, x)
local sol, i, deg, abssol, sorted, nbmini, candidates, j, cand2, n, q, y, sqf, sqf2, pol;
	pol:=collect(p,x);
	deg:=degree(pol,x);
	if deg=0 then return [] fi;
	if deg=1 then return [solve(pol,x)] fi;
	sol:=[fsolve(pol, x, complex)];
	n:=nops(sol);
	abssol:=map(abs,sol);
	sorted:=sort(abssol);
	for nbmini from 2 to nops(sorted) while sorted[nbmini]-sorted[1]<Float(1,2-Digits) do od;
	nbmini:=nbmini-1;

	# Easy case with only one root of minimal modulus
	if nbmini=1 then 
		member(sorted[1],abssol,'i');
		return `infsolvepoly/found`([sol[i]],pol,x,deg)
	fi;

	# Deal with multiple factors
	q:=gcd(pol,diff(pol,x)); 
        if q<>1 then
                if type(q,'polynom'('rational',x)) then divide(pol,q,'q')
                else q:=quo(pol,q,x) fi;
                return procname(q,x)
        fi;

	candidates:=[seq(sol[i],i=select(proc(k) abssol[k]<=sorted[nbmini] end,[$1..n]))];

	# Polynomial with non-real coefficients
	if not type(pol,polynom(realcons)) then
		pol:=primpart(pol,x);
		if not type(pol,polynom(realcons)) then 
			q:=collect(evalc(Re(pol)^2+Im(pol)^2),x,expand);
			return select(`infsolvepoly/isroot`,procname(q,x),pol,x)
		else return procname(pol,x)
		fi
	fi;

	# Special cases for nbmini=2 and opposite or conjugate roots
	if nbmini=2 and Im(candidates[1])+Im(candidates[2])=0 then
		if Im(candidates[1])=0 then
			q:=gcd(pol,eval(pol,x=-x));
			if `infsolvepoly/isroot`(candidates[1],q,x) then
				return `infsolvepoly/found`(candidates,q,x,degree(q,x))
			fi
		else return `infsolvepoly/found`(candidates,pol,x,deg)
		fi
	fi;

	# Compute a polynomial whose roots are pairwise products of roots of p
	q:=resultant(subs(x=y,p),numer(subs(x=x/y,p)),y);
	# This polynomial should have a smallest real root sorted[1]^2 of multiplicity nbmini
	sqf:=select(proc(t)t[2]=nbmini end,sqrfree(q,x)[2]);
	if sqf<>[] and `infsolvepoly/isroot`(sorted[1]^2,sqf[1][1],x) then
		`infsolvepoly/found`(candidates,pol,x,deg) 
	else
		Digits:=2*Digits;
		procname(pol,x) # not the right multiplicity, refine
    fi 
end: # infsolvepoly

## Input:
##		lroots	list of numerical roots
##		pol		polynomial
##		x		variable
##		deg		degree(pol,x)
## Output:
##		list of symbolic values (either explicit for deg 2, or using RootOf)
## Description:
##  This could be improved by factoring pol and returning the RootOf's for the appropriate
##  factors only.
`infsolvepoly/found` := proc(lroots,pol,x,deg)
local sol, fsol;
	if deg>2 then 
		map2(RootOf,subs(x=_Z,pol),lroots)
	else
		sol:=[solve(pol,x)];
		if nops(lroots)=2 then sol
		else
			fsol:=evalf(sol);
			if abs(fsol[1]-lroots[1])<abs(fsol[2]-lroots[1]) then
				[sol[1]]
			else
				[sol[2]]
			fi
		fi
	fi
end proc: # `infsolvepoly/found`
	
## Input:
##		pt     	a numerical value
##		pol		polynomial
##		x		its variable
## Output:
##		boolean
## Description:
##		Checks whether pt is a root of pol by computing the distance
##		to the next Newton iterate.
`infsolvepoly/isroot` := proc(pt, pol, x)
	evalb(abs(eval(pol/diff(pol,x),x=evalf(pt)))<Float(1,2-Digits))
end proc: # `infsolvepoly/isroot` 

   
