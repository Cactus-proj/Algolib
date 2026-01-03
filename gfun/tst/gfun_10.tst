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
##### holexprtodiffeq

#test 15

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



#################################################################
# Test from the help page

Try[testdiffeq, y, x](1  ,holexprtodiffeq(BesselJ(2,x),y(x)) , 
{x^2*(D@@2)(y)(x)+x*D(y)(x)+(x^2-4)*y(x),
#(D@@2)(y)(0) = 1/4, (D@@3)(y)(0) = 0, y(0) = 0, D(y)(0) = 0}
(D@@2)(y)(0) = 1/4}
):

#################################################################
# Test from the help page

Try[testdiffeq, y, x](2  ,holexprtodiffeq(arcsec(1/x)+sin(x)^2,y(x)) ,{y(0)=1/2*Pi,(D@@3)(y)(0)=-1,D(y)(0)=-1,(D@@2)(y)(0) = 2,
(16*x^5-8*x^3+52*x)*D(y)(x)+(16*x^6-40*x^4+44*x^2-20)*(D@@2)(y)(x)
+(4*x^5-2*x^3+13*x)*(D@@3)(y)(x)+(4*x^6-10*x^4+11*x^2-5)*(D@@4)(y)(x)} ):

#################################################################

Try[testdiffeq, y, x](3  ,holexprtodiffeq(arcsinh(x^3+x+1), y(x)) ,{D(y)(0) = 1/2*2^(1/2), y(0) = arcsinh(1),
(3*x^7+3*x^5+x^3-11*x-3*x^4-6*x^2+1)*D(y)(x)
+(6*x^5+3*x^8+7*x^6+8*x^3+5*x^4+7*x^2+2*x+2)*(D@@2)(y)(x)} ):

#################################################################

Try[testdiffeq, y, x]( 4 ,holexprtodiffeq(exp((x^2+1)^(1/2)), y(x)) ,{y(0) = exp(1), -x^3*y(x)-D(y)(x)+(x^3+x)*(D@@2)(y)(x),
#(D@@2)(y)(0) = exp(1), D(y)(0) = 0} 
(D@@2)(y)(0) = exp(1)} 
):

#################################################################

Try[testdiffeq, y, x](5  ,holexprtodiffeq(BesselI(3,x^2+x), y(x)) ,
{(-80*x^3-109*x^2-54*x-8*x^7-28*x^6-38*x^5-25*x^4-9)*y(x)+
(x+4*x^3+3*x^2+2*x^4)*D(y)(x)+(x^2+2*x^5+5*x^4+4*x^3)*(D@@2)(y)(x),
#(D@@3)(y)(0) = 1/8, (D@@2)(y)(0) = 0, y(0) = 0, (D(y))(0) = 0} 
(D@@3)(y)(0) = 1/8} 
):

#################################################################

Try[testdiffeq, y, x](6  ,holexprtodiffeq(sin((1-x^2)^(1/2))+exp(1+x), y(x)) ,{y(0) = sin(1)+exp(1), (D@@2)(y)(0) = exp(1)-cos(1), D(y)(0) = exp(1),
(x^3-3*x^2+3*x)*y(x)+(-x^3+x^2-1)*D(y)(x)
+(-x^3+3*x^2-2*x)*(D@@2)(y)(x)+(1-x-x^2+x^3)*(D@@3)(y)(x)} ):

#################################################################

Try[testdiffeq, y, z](7  ,holexprtodiffeq(sin(z)+RootOf(_Z^3+2), y(z)) ,{y(0) = RootOf(_Z^3+2), (D@@2)(y)(z)+y(z)-RootOf(_Z^3+2),D(y)(0) = 1} ):

#################################################################

Try[testdiffeq, y, z]( 8 ,holexprtodiffeq((1+z*(1+z)^(1/2))^(1/2), y(z)) ,{y(0) = 1, D(y)(0) = 1/2,
(-27*z^3-54*z^2-36*z-8)*y(z)+(48*z^4+112*z^3+96*z^2+56*z+32)*D(y)(z)
+(48*z^5+128*z^4+112*z^3-16*z^2-80*z-32)*(D@@2)(y)(z)} ):

#################################################################

# This was the previous result. 
#res:={(D@@2)(y)(0) = 0, (D@@7)(y)(0) = 0, (D@@6)(y)(0) = 0, (D@@5)(y)(0) = 0,
#(D@@4)(y)(0) = 0, (D@@3)(y)(0) = 0, (D@@8)(y)(0) = 0, D(y)(0) = 0, y(0) = 0,
#(18432*x^2-18432)*y(x)+18432*x^2*(D@@2)(y)(x)-x^9+18432*x*D(y)(x)}:

# The new result is better: all other initial conditions can be deduced from
# those ones.
#res:={y(0) = 0, D(y)(0) = 0, (18432*x^2-18432)*y(x)+18432*x^2*diff(diff(y(x),x),x)-x^9+18432*x*diff(y(x),x), `@@`(D,2)(y)(0) = 0}:
## New (Oct. 05) even better
res:={D(y)(0) = 0, (18432*x^2-18432)*y(x)+18432*x^2*diff(diff(y(x),x),x)-x^9+18432*x*diff(y(x),x)}:

Try[testdiffeq, y, x](9  ,holexprtodiffeq(BesselJ(1,x)-1/2*x+1/16*x^3-1/384*x^5+1/18432*x^7, y(x)) , res):

#################################################################

Try[testdiffeq, y, x]( 10 ,holexprtodiffeq(RootOf(_Z^5+_Z-x), y(x)) ,{(D@@2)(y)(0) = 0, (D@@3)(y)(0) = 0, D(y)(0) = 1, y(0) = 0,
-1155*y(x)+31875*x*D(y)(x)+73125*x^2*(D@@2)(y)(x)+31250*x^3*(D@@3)(y)(x)
+(3125*x^4+256)*(D@@4)(y)(x)} ):

#################################################################

Try[testdiffeq, y, x](11  , holexprtodiffeq(ln((1-x^2)^(1/2)), y(x)),{D(y)(x)*(-1+x^2)-x, y(0) = 0} ):

#################################################################

Try[testdiffeq, y, x]( 12 ,holexprtodiffeq(arcsec(1/x), y(x)) ,{x*(D(y))(x)+(-1+x^2)*(D@@2)(y)(x), (D(y))(0) = -1, y(0) = 1/2*Pi} ):

#################################################################

Try[testdiffeq, y, x]( 13 , holexprtodiffeq(t^(2/3)/x^3+(1-x)^2/x^2, y(x)) ,-t^(2/3)+x^3*y(x)-x+2*x^2-x^3 ):

#################################################################

Try(14  ,holexprtodiffeq(GAMMA(a), y(z)) ,y(z)-GAMMA(a) ):

#################################################################

Try[testdiffeq, y, x]( 15 ,holexprtodiffeq(z^2 + 2*GAMMA(a), y(z)) , y(z)-z^2-2*GAMMA(a)):

#################################################################
s:='s':

Try[testdiffeq, y, x]( 16 ,holexprtodiffeq((2*x+3)^(3*s+2),y(x)) ,{(-6*s-4)*y(x)+(2*x+3)*(D(y))(x), y(0) = 3^(3*s+2)} ):

#################################################################
# used to return initial conditions (result is actually 2/Pi/z)

Try[testdiffeq, y, z]( 17 ,holexprtodiffeq(BesselJ(v+1,z)*BesselY(v,z)-BesselJ(v,z)*BesselY(v+1,z),
y(z)) ,(8*z^2+4*v^2+1+4*v)*y(z)+(-4*z^2*v^2+4*z^4+5*z^2-4*v*z^2)*diff(diff(y(z),z),z)
+(-4*z*v+16*z^3-z-4*z*v^2)*diff(y(z),z)+diff(diff(diff(diff(y(z),z),z),z),z)*z
^4+6*diff(diff(diff(y(z),z),z),z)*z^3 ):

#################################################################
# used to be a problem with the order in algfuntoalgeq
f:=-8*(1-sqrt(1-z))^3/z^2:
g:=subs(z=f,f)-z:

Try[testdiffeq, y, z](18  , holexprtodiffeq(g,y(z)),
#{`@@`(D,4)(y)(0) = 0, `@@`(D,5)(y)(0) = 0, `@@`(D,2)(y)(0) = 0, y(0) = 0,
#`@@`(D,3)(y)(0) = 0, D(y)(0) = 0,
#(64-z^3+24*z^2-48*z)*y(z)+(z^4-24*z^3+96*z^2-64*z)*diff(y(z),z)+
#(2*z^5-18*z^4+48*z^3-32*z^2)*diff(diff(y(z),z),z)} 
y(z) # zero is recognized!
):

#################################################################
# used not to be recognized as holonomic
f:=1/exp(x):

Try[testdiffeq, y, x]( 19 ,holexprtodiffeq(f,y(x)) , {y(x)+diff(y(x),x), y(0) = 1}):

#################################################################
# used to return a _C[0]
# did not return initial condition in versions < 3.20
Try[testdiffeq,y,z](20,holexprtodiffeq(hypergeom([1,1],[],z),y(z)),{y(0)=1,y(z)+(3*z-1)*diff(y(z),z)+z^2*diff(diff(y(z),z),z)}):

#################################################################
# used not to be recognized as holonomic
f:=1/x^n:

Try[testdiffeq,y,x](21,holexprtodiffeq(f,y(x)),diff(y(x),x)*x+n*y(x)):

f:=1/(x^2+x+1)^n:

Try[testdiffeq,y,x](21,holexprtodiffeq(f,y(x)),{(2*n*x+n)*y(x)+(x^2+x+1)*diff(y(x),x), y(0) = 1}):

#end test
