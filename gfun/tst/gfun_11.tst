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

#######################################################
##### diffeqtohomdiffeq and rectohomrec

#test 5

with(TestTools):
with(gfun):

### Tests if  L1=L2
testini:=proc(L1,L2)
local s1, s2, i, j, n, N, g1, g2, inds, var, S, vars, c0;
    if L1=L2 then RETURN(true) fi;
    # c0 because of a bug in Groebner[Basis] (maple6)
    s1:=subs(_C[0]=c0,indets(L1,_C[anything]));
    s2:=subs(_C[0]=c0,indets(L2,_C[anything]));
    if s1={} and s2={} then RETURN(false) fi;
    n:=nops(s1);
    if n<>nops(s2) then RETURN(false) fi;
    S:={seq(op(1,i),i=L1)};
    if S<>{seq(op(1,i),i=L2)} then RETURN(false) fi;
    N:=nops(L1);
    vars:=[seq(op(i,S)=var[i],i=1..nops(S))];
    g1:=remove(has,Groebner[Basis](subs(vars,_C[0]=c0,
	[seq(op(1,i)-op(2,i),i=L1)]),
	lexdeg([op(s1)],[seq(var[i],i=1..N)])),s1);
    g2:=remove(has,Groebner[Basis](subs(vars,_C[0]=c0,
	[seq(op(1,i)-op(2,i),i=L2)]),
	lexdeg([op(s2)],[seq(var[i],i=1..N)])),s1);
    evalb(g1=g2)
end:


#### Test if two differential equations are the same
testdiffeq:=proc(deq1,deq2,y,z)
local eq1,eq2,Y;
    if type(deq1,set) and type(deq2,set) then
	eq1:=op(select(has,deq1,z));
        eq2:=op(select(has,deq2,z));
	if not testini(deq1 minus {eq1},deq2 minus {eq2}) then return false fi
    elif type(deq1,set) or type(deq2,set) then RETURN(false)
    else eq1:=deq1; eq2:=deq2 fi;
    eq1:=convert(eq1,D); eq2:=convert(eq2,D);
    RETURN(evalb(type(normal(subs((D@@3)(y)(z)=Y^4,(D@@2)(y)(z)=Y^3,
        (D)(y)(z)=Y^2,y(z)=Y,eq1/eq2)),rational)))
end:

### Test if two recurrences are the same
testrec:=proc(rec1,rec2,u,n)
local eq1,eq2,Y;
    if type(rec1,set) and type(rec2,set) then
        eq1:=op(select(has,rec1,n));
        eq2:=op(select(has,rec2,n));
	if not testini(rec1 minus {eq1},rec2 minus {eq2}) then return false fi
    elif type(rec1,set) or type(rec2,set) then RETURN(false)
    else eq1:=rec1; eq2:=rec2 fi;
    RETURN(evalb(type(normal(subs(u(n+3)=Y^4,u(n+2)=Y^3,
        u(n+1)=Y^2,u(n)=Y,eq1/eq2)),rational)))
end:

#################################################################
deq:={diff(y(x),x,x)=y(x),y(0)=1,D(y)(0)=0}:
Try(1,diffeqtohomdiffeq(deq,y(x)), deq);

##############################################
# Test from the help page
deq:=diff(y(x),x)*(x-1)+2*y(x)-2*x-3:

Try[testdiffeq, y, x](2.1,diffeqtohomdiffeq(deq,y(x)),4*y(x)+(-4*x-11)*diff(y(x),x)+(-2*x^2-x+3)*diff(diff(y(x),x),x) ):

Try[testdiffeq, y, x](2.2, diffeqtohomdiffeq({deq,y(0)=2},y(x)), 
{4*y(x)+(-4*x-11)*diff(y(x),x)+(-2*x^2-x+3)*diff(diff(y(x),x),x),
y(0) = 2, D(y)(0) = 1}):
##############################################
# Test from the help page
rec:=u(n+1)=u(n)+n^2+1:

Try[testrec, u, n](3, rectohomrec(rec,u(n)),(-n^2-2*n-2)*u(n)+(2*n^2+2*n+3)*u(n+1)+(-n^2-1)*u(2+n)):

Try[testrec, u, n](3.2,rectohomrec({rec,u(0)=1},u(n)),{u(0)=1,(-n^2-2*n-2)*u(n)+(2*n^2+2*n+3)*u(n+1)+(-n^2-1)*u(2+n),u(1)=2}):

#############################################
# Bug with initial conditions in versions < 2.97
rec:=u(n)=n:

Try[testrec, u, n](4, rectohomrec({rec},u(n)),
{u(0) = 0, u(1) = 1, (n+1)*u(n)-n*u(n+1)});

Try[testrec, u, n](4.1, rectohomrec({rec,u(0)=6},u(n)),
{u(0) = 6, u(1) = 1, (n+1)*u(n)-n*u(n+1)});

Try(4.2,rectoproc({rec,u(0)=6},u(n))(5),5);

#end test
