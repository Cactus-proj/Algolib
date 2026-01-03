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

# tests rectodiffeq
#test 6

with(TestTools):
with(gfun):

### Tests if [WHAT DOES THIS DO?]
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

#######################################################
###### rectodiffeq
rec:={a(n) = l*(a(n-1)+l), a(0) = l}:
res:=(1-z)*(-l*z+1)*y(z)-l*(1-z)-z*l^2:
Try[testrec, u, n](1, rectodiffeq(rec, a(n), y(z)), res);

##########################
rec:=op(1,listtorec([0,2,10,42,168,660,2574,10010,38896,151164,
587860,2288132,8914800,34767720,135727830,530365050,2074316640,8119857900,
31810737420,124718287980],a(n))):
res:={(-18*z^2+13*z-2)*y(z)+(-12*z^3+7*z^2-z)*(D(y))(z)+6*z,y(0)=0,D(y)(0)=2}:
#res2:= {y(0)=0,D(y)(0)=2,(6*z+2)*y(z)+(14*z^2-2*z)*D(y)(z)
#   +(4*z^3-z^2)*(D@@2)(y)(z)}:
res2:= {D(y)(0)=2,(6*z+2)*y(z)+(14*z^2-2*z)*D(y)(z)
   +(4*z^3-z^2)*(D@@2)(y)(z)}:

eitheror:= (a,b,c)-> evalb(testdiffeq(a,b,y,z) or testdiffeq(a,c,y,z)):

Try[eitheror, res](2, rectodiffeq(rec, a(n), y(z)), res2):

#
## res1:=rectodiffeq(rec,t(n),y(z)):
#res:={(-18*z^2+13*z-2)*y(z)+(-12*z^3+7*z^2-z)*(D(y))(z)+6*z,y(0)=0,D(y)(0)=2}:
#Try[testdiffeq, y, z](2.1, rectodiffeq(rec, a(n), y(z)), res);
##
#
#res:={y(0)=0,D(y)(0)=2,(6*z+2)*y(z)+(14*z^2-2*z)*D(y)(z)
#   +(4*z^3-z^2)*(D@@2)(y)(z)}:
#Try[testdiffeq, y, z](2.2, rectodiffeq(rec, a(n), y(z)), res);#
#
#if testdiffeq(res1,res2,y,z) or testdiffeq(res1,res3,y,z)
#    then okay else res1,res2,res3 fi;


##########################
# It would be a mistake to return initial conditions there:

rec:=(1+k^2)*f(k)=1:
res :=(1-z)*y(z)+(1-z)*z*(D(y))(z)+(1-z)*z^2*((D@@2)(y))(z)-1:
Try[testdiffeq, y, z](3, rectodiffeq(rec, f(k), y(z)), res);


###########################
rec:=(n-10)*a(n+1)-a(n):
#res:=
#{(z+11)*y(z)-z*D(y)(z), y(0) = 0, D(y)(0) = 0, (D@@2)(y)(0) = 0,
#(D@@3)(y)(0) = 0, (D@@4)(y)(0) = 0, (D@@5)(y)(0) = 0, (D@@6)(y)(0) = 0,
#(D@@7)(y)(0) = 0, (D@@8)(y)(0) = 0, (D@@9)(y)(0) = 0, (D@@10)(y)(0) = 0,
#(D@@11)(y)(0) = _C[0]}:
res:=(z+11)*y(z)-z*D(y)(z):
Try[testdiffeq, y, z](4, rectodiffeq(rec, a(n), y(z)), res);


##
# Test from the help page
rec:=(5*n+10)*a(n)+k*a(n+1)-a(n+2):
res:=(-10*t^2-k*t+1)*f(t)-5*t^3*(D(f))(t):
Try[testdiffeq, f, t](5, rectodiffeq({rec,a(0)=0,a(1)=0}, a(n), f(t)), res);

# This used to have extra initial conditions, but they are wrong:
# the only solution has an essential singularity at 0.
#res2:={(5*n+10)*u(n)+k*u(n+1)-u(n+2),u(0)=0,u(1)=0}:
res2:=(5*n+10)*u(n)+k*u(n+1)-u(n+2):
Try[testrec, u, n](7, diffeqtorec(res,f(t),u(n)), res2):


#########
# bug from <Christian.Mallinger@risc.uni-linz.ac.at> (fixed pz 111194) #
rec:=a(n+3)=a(n):
Try[testdiffeq, f, x](8,rectodiffeq({rec,a(0)=1,a(1)=1,a(2)=1},a(n),f(x)),
    1+(x-1)*f(x));

##########
# This used to return a non-homogeneous equation
# with lots of (D@@k)(y)(0). Changed BS Jan 95.

rec:=f(n)-6*f(n+2)-3*f(n+3)+2*f(n+4):
res:=(12*_C[3]-18*_C[2]-36*_C[1])*z^3+(-36*_C[0]+12*_C[2]-18*_C[1])*z^2+
(12*_C[1]-18*_C[0])*z+12*_C[0]+(-6*z^4+36*z^2+18*z-12)*y(z):

Try[testdiffeq, y, z](9,rectodiffeq(rec,f(n),y(z)), res);

########## This used to lose the condition a(2)=1.
rec:=(1+4/5*n+1/5*n^2)*a(n+1)+(-6/5-n-1/5*n^2)*a(n+3):
res:={y(0)=0,D(y)(0)=0,-2*y(z)-3*z*D(y)(z)+(-z^2+1)*(D@@2)(y)(z)-2}:
Try[testdiffeq, y, z](10,rectodiffeq({a(0) = 0, a(1) = 0, a(2) = 1,rec},a(n),y(z)) ,res);

########### Check subtleties with the initial conditions
rec:=(n-2)*v(n):
# Now returns homogeneous (version 3.20)
#res:=_C[0]*z^2-y(z):
res:=-2*y(z)+z*diff(y(z),z):

Try[testdiffeq, y, z](11,rectodiffeq(rec,v(n),y(z)) ,res);

##########
res1:=traperror(rectodiffeq(a(n)*v(n),v(n),y(z))):
res2:=`invalid recurrence or unknown`,   a(n)*v(n),   v(n):
if [res1]=[res2] then okay else [res1],[res2] fi;

Try[testerror](11.1, rectodiffeq(a(n)*v(n),v(n),y(z)), [ "invalid recurrence or unknown, %0", a(n)*v(n), v(n)]);


########### More.
eq2:=(1+x)*(D@@2)(y)(x)+D(y)(x):
rec:=(n+2)*u(n+2)+(n+1)*u(n+1):

Try[testrec, u, n](13,diffeqtorec(eq2,y(x),u(n)), rec);

##########
res:={(-x-1)*D(y)(x)+_C[1],y(0)=_C[0]}:

Try[testdiffeq, y, x](14, rectodiffeq(rec,u(n),y(x)), res);

########### used to have a problem with u
rec:=-u(n)+(n+2)*u(n+2):
res:={u*f(u)-(D(f))(u)+_C[1], f(0) = _C[0]}:

Try[testdiffeq, f,u](15,rectodiffeq(rec,u(n),f(u)), res);

########### The initial conditions were not properly handled
rec:=a^2*u(n)+(-2+4*n)*u(n+1):
# This old result is not optimal: the initial conditions
# can be deduced form the diff. eq. by evaluating its derivatives at 0.
# res:={D(y)(0) = 1, y(0) = 0, (-x*a^2+6)*y(x)-4*x*D(y)(x)-2*x}:
res:=(-x*a^2+6)*y(x)-4*x*D(y)(x)-2*x:

Try[testdiffeq, y, x](16,rectodiffeq({u(0) = 0, u(1) = 1,rec},u(n),y(x)),res);

########### bug in version 2.52
rec:=n*a(n)+(n+1)*a(n+1):
res:={-1+(t+1)*diff(y(t),t), y(0) = _C[0]}:

Try[testdiffeq, y, t](17,rectodiffeq({rec, a(1)=1}, a(n), y(t)), res);

#end test
