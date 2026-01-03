# Copyright (C) 1991--2013 by INRIA.
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
##### algebraicsubs

#test 10

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

#Try[testdiffeq, y, x]( , ,):

############################################################
deq:={y(0)=1,D(y)(0)=1/2,(1-z)*y(z)+(1-z)*z*(D(y))(z)+(1-z)*z^2*((D@@2)(y))(z)-1}:

Try[testdiffeq, y, z](1 ,algebraicsubs(deq,(1-z)*y+z,y(z)) ,-y(z)+(-2*z^3+3*z^2-z)*D(y)(z)+(-z^4+2*z^3-z^2)*(D@@2)(y)(z)+1-z):

############################################################
# No initial condition needed
deq:=-y(z)+(-2*z^3+3*z^2-z)*D(y)(z)+(-z^4+2*z^3-z^2)*(D@@2)(y)(z)+1-z:

Try[testdiffeq, y, z](2 ,`diffeq*diffeq`(deq,y(z)*(1-z)-1,y(z)) ,(2*z^2-z+1)*y(z)+(4*z^3-5*z^2+z)*D(y)(z)+(z^4-2*z^3+z^2)*(D@@2)(y)(z)-1):

############################################################
deq:=(2*z^2-z+1)*y(z)+(4*z^3-5*z^2+z)*D(y)(z)+(z^4-2*z^3+z^2)*(D@@2)(y)(z)-1:

Try[testrec, g, n](3 ,diffeqtorec(deq,y(z),g(n)) ,{g(0) = 1, g(1) = 1/2, (n^2+3*n+2)*g(n)+(-6-7*n-2*n^2)*g(n+1)+(4*n+5+n^2)
*g(n+2)}):

############################################################
rec:={g(0) = 1, g(1) = 1/2, (n^2+3*n+2)*g(n)+(-6-7*n-2*n^2)*g(n+1)+(4*n+5+n^2)
*g(n+2)}:

Try(4 ,rectoproc(rec,g(n))(20) ,-3845848802828440117248/7315582032468906326065):

############################################################

# Test from the help page
Try[testdiffeq, y, x](5 , 
algebraicsubs((D@@2)(f)(t)+f(t),f^2-1+4*t,f(t)),
-f(t)+D(f)(t)/2+(t-1/4)*(D@@2)(f)(t)):

############################################################

Try[testdiffeq, y, z](6 ,algebraicsubs({y(0) = 1, -1+(z+1)*y(z)+(z^2+z)*diff(y(z),z)
+(z^2+z^3)*diff(diff(y(z),z),z), (D(y))(0) = -1/2},z*y^2+(2*z-1)*y+z,y(z)) ,{y(0)=1, 2*y(z)+(36*z^2-18*z+2)*D(y)(z)+(3*z-28*z^2+64*
z^3)*(D@@2)(y)(z)+(16*z^4-8*z^3+z^2)*(D@@3)(y)(z)-1}):

############################################################

# bug in versions < 2.29 ### cos(x+2^(1/2)*x^2):

Try[testdiffeq, y, x](7 , algebraicsubs((D@@2)(y)(x)+y(x),2*x^4-y^2+2*y*x-x^2,y(x)),
{(768*x^6-2048*x^8+864*x^4+87-236*x^2)*y(x)+(320*x^3+280*x)*D(y)(x)
+(18-136*x^2-512*x^6)*(D@@2)(y)(x)+(128*x^3-8*x)*(D@@3)(y)(x)+(-32*x^4+4*x^2+
3)*(D@@4)(y)(x), y(0) = -_C[0], D(y)(0) = -_C[1],
(D@@3)(y)(0) = 1/2*(12*_C[0]+_C[1]*RootOf(_Z^2-2))*RootOf(_Z^2-2),
(D@@2)(y)(0) = -2*_C[1]*RootOf(_Z^2-2)+_C[0]}):

############################################################


Try[testdiffeq, y, x](8 , algebraicsubs({(D@@2)(y)(x)+y(x),y(0)=1,D(y)(0)=0},
2*x^4-y^2+2*y*x-x^2,y(x)),{(768*x^6-2048*x^8+864*x^4+87-236*x^2)*y(x)+
(320*x^3+280*x)*(D(y))(x)+(18-136*x^2-512*x^6)*(D@@2)(y)(x)+
(128*x^3-8*x)*(D@@3)(y)(x)+(-32*x^4+4*x^2+3)*(D@@4)(y)(x),
D(y)(0) = 0, (D@@3)(y)(0) = -6*RootOf(_Z^2-2), (D@@2)(y)(0) = -1, y(0) = 1}):

############################################################

Try[testdiffeq, y, x](9 ,algebraicsubs({(D@@2)(y)(x)+y(x),y(0)=1,D(y)(0)=0},
2*x^4-y^2+2*y*x-x^2,y(x),{y(0)=0,D(y)(0)=1,(D@@2)(y)(0)=2*sqrt(2)}) ,{(768*x^6-2048*x^8+864*x^4+87-236*x^2)*y(x)+
(320*x^3+280*x)*(D(y))(x)+(18-136*x^2-512*x^6)*(D@@2)(y)(x)+
(128*x^3-8*x)*(D@@3)(y)(x)+(-32*x^4+4*x^2+3)*(D@@4)(y)(x),
(D@@3)(y)(0) = -6*2^(1/2), D(y)(0) = 0, (D@@2)(y)(0) = -1, y(0) = 1}):

############################################################

Try[testdiffeq, y, x](10 ,algebraicsubs({y(0)=0,D(y)(0)=1, diff(diff(y(x),x),x)+y(x)},y-x^2,y(x)) ,
#{y(0)=0,D(y)(0)=0,(D@@2)(y)(0)=2,4*x^3*y(x)-D(y)(x)+x*(D@@2)(y)(x)}
{y(0)=0,(D@@2)(y)(0)=2,4*x^3*y(x)-D(y)(x)+x*(D@@2)(y)(x)}
):

############################################################

Try[testdiffeq, y, x](11 ,algebraicsubs({y(x)+((`@@`(D,2))(y))(x), y(0) = 1, 
(D(y))(0) = 0}, y^3-x^2, y(x), {y(0) = 0}) ,64*x^3*y(x)+315*D(y)(x)-315*x*(D@@2)(y)(x)+2025*x^3*(D@@4)(y)(x)+
3645*x^4*(D@@5)(y)(x)+729*x^5*(D@@6)(y)(x)):

############################################################

Try[testdiffeq, y, x](12 ,algebraicsubs({y(x)+((`@@`(D,2))(y))(x), y(0) = 1, 
       (D(y))(0) = 0}, y^2-x, y(x), {y(0) = 0}) ,{y(0) = 1, y(x)+2*D(y)(x)+4*x*(D@@2)(y)(x)}):

############################################################

Try[testdiffeq, y, x](13 ,algebraicsubs(diff(y(x),x)*x+(1+x^2)*diff(diff(y(x),x),x),y-1-x,y(x)) ,(1+x)*D(y)(x)+(2+2*x+x^2)*(D@@2)(y)(x)):

############################################################

# arcsec(1/z)
Try[testdiffeq, y, x](14 ,algebraicsubs((2*z^2-1)*diff(y(z),z)+(z^3-z)*diff(diff(y(z),z),z),y*z-1,y(z)) ,-z*D(y)(z)+(-z^2+1)*(D@@2)(y)(z)):

############################################################

# there used to remain a D(x) in the result

Try[testdiffeq, h, y](15,
algebraicsubs({y^2*`@@`(D,2)(h)(y)+y*D(h)(y)+(y^2-1)*h(y),
   `@@`(D,2)(h)(0) = 0, D(h)(0) = 1/2},h-x*y^2,h(y)),
#{(-4+4*x^2*y^4)*h(y)+y*D(h)(y)+y^2*(D@@2)(h)(y),D(h)(0)=0,h(0)=0,
#(D@@2)(h)(0) = x}
{(-4+4*x^2*y^4)*h(y)+y*D(h)(y)+y^2*(D@@2)(h)(y),
(D@@2)(h)(0) = x}
);

#end test

