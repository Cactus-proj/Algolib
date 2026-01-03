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

### tests diffeqtorec

#test 4

with(TestTools):
with(gfun):

### Tests if  L1=L2
testini:=proc(L1,L2)
local s1, s2, i, j, n, N, g1, g2, inds, var, S, vars, c0;
    if L1=L2 then RETURN(true) fi;
    if verify(L1,L2,set(boolean=float(1))) then return true end if;
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

#######################################################
## question: what is this testing?

Try[testnoerror](1,diffeqtorec(z^2*diff(w(z),z,z)+z*diff(w(z),z)+(z^2-nu^2)*w(z),
w(z),u(n)));


deq:=-z**2*diff(y(z),z)+z*diff(y(z),z)-10*y(z)*z-y(z)+1:
Try[testerror](2,diffeqtorec(deq,y(z),u(n)), "no valid initial conditions");
##################################################
deq:=(67/2000*t^2+129/1000)*diff(diff(f(t),t),t)+
77/500*t*diff(f(t),t)+5859/125*f(t) = 34/625:

Try[testrec, u, n](3, diffeqtorec(deq,f(t),u(n)),
{(93744+241*n+67*n^2)*u(n)+(258*n^2+774*n+516)*u(n+2), u(0)
 = _C[0], u(1) = _C[1], u(2) = -7812/43*_C[0]+136/645}):

##################################################
# try to get a names conflict
Try[testrec, f, n](4,
diffeqtorec((67/2000*t^2+129/1000)*diff(diff(f(t),t),t)+77/500*t*diff(f(t),t)
+5859/125*f(t)= 34/625,f(t),f(n)),
{(93744+241*n+67*n^2)*f(n)+(258*n^2+774*n+516)*f(n+2), f(0)
 = _C[0], f(1) = _C[1], f(2) = -7812/43*_C[0]+136/645}):

##################################################
deq:=diff(diff(y(x),x),x)+3*x*diff(y(x),x)+y(x)=0:

Try[testrec, c, n](5,
diffeqtorec({deq,y(0)=0,(D(y))(0)=1},y(x),c(n)),
{c(0) = 0, c(1) = 1, (1+3*n)*c(n)+(n^2+3*n+2)*c(n+2)});

Try[testrec, c, n](6,
diffeqtorec(deq,y(x),c(n)),
(1+3*n)*c(n)+(n^2+3*n+2)*c(n+2));

##################################################
# Test from the help page
deq:=y(z)=a*diff(y(z),z):

Try[testrec, v, n](7,
diffeqtorec(deq,y(z),v(n)),
v(n)+(-a*n-a)*v(n+1));

##################################################
# used to give an order 4 recurrence
deq:=-y(z)+1/(1-z)+D(y)(z)-1/(1-z)^2+z*((D@@2)(y)(z)-2/(1-z)^3):

Try[testrec, u, n](8,
diffeqtorec(deq,y(z),u(n)),
{u(n)+(-2*n-1-n^2)*u(n+1)+n^2+2*n,u(3)=_C[0],u(0)=36*_C[0]-35,
u(1)=36*_C[0]-35,u(2)=9*_C[0]-8});

##################################################
rec:=diffeqtorec((2*z^2-1)*D(y)(z)+(z^3-z)*(D@@2)(y)(z)-z,y(z),u(n)):
p:=rectoproc(rec,u(n),remember):

Try[testrec, c, n](9,
[seq(p(i),i=0..10)],
[_C[0], 0, -1/4, 0, -3/32, 0, -5/96, 0, -35/1024, 0, -63/2560]
);
##################################################
# used to crash gfun/isolve
deq:=(a[4]*x^4+a[3]*x^3+a[2]*x^2)*diff(f(x),x,x)
+ (b[3]*x^3+b[2]*x^2+b[1]*x)*diff(f(x),x)
+ (c[2]*x^2+c[1]*x+c[0])*f(x):

#Try[testrec, u, n](10,
#diffeqtorec(deq,f(x),u(n)),
#{(a[4]*n^2+(b[3]-a[4])*n+c[2])*u(n)+(a[3]*n^2+(b[2]+a[3])*n+c[1]+b[2])*u(n+1)
#+(a[2]*n^2+(b[1]+3*a[2])*n+c[0]+2*b[1]+2*a[2])*u(n+2), u(0) = 0, u(1) = 0});
Try[testrec, u, n](10,
diffeqtorec(deq,f(x),u(n)),
(a[4]*n^2+(b[3]-a[4])*n+c[2])*u(n)+(a[3]*n^2+(b[2]+a[3])*n+c[1]+b[2])*u(n+1)
+(a[2]*n^2+(b[1]+3*a[2])*n+c[0]+2*b[1]+2*a[2])*u(n+2));

#################################################
# used to have a problem with u
deq:=D(f)(u)-u*f(u):

#Try[testrec, u, n](11, diffeqtorec(deq,f(u),u(n)),
#{u(1) = 0, -u(n)+(n+2)*u(n+2)});
Try[testrec, u, n](11, diffeqtorec(deq,f(u),u(n)),
-u(n)+(n+2)*u(n+2));

##################################################
# bug in 2.50 !

deq:=y(z)-z^4/(1-z)^2/(z^2+4):

Try[testrec, u, n](12,diffeqtorec(deq,y(z),u(n)),
{u(0) = 0, u(1) = 0, u(2) = 0, u(3) = 0, u(4) = 1/4, u(n)+1+4*u(n+2)-n});

##################################################
# bug in solve/linear (workaround in 2.55)
deq:=y(z)-(2.3+z)/(2.1*z-1.2):

Try[testrec, u, n](13,diffeqtorec(deq,y(z),u(n)),
{u(0) = -1.91666666666666630, u(1) = -4.18749999999999999, 2.100000000*u(n)-1.200000000*u(n+1)});

##################################################
# bug in version<2.98
deq:=-z*(z^4-2*z^3+2*z-1)*diff(y(z),z)+2*diff(diff(y(z),z),z)*z^3+y(z)*(-2*z^3-6*z-4):

#Try[testrec, u, n](14,diffeqtorec(deq,y(z),u(n)),
#{-u(n)+2*u(n+1)+(2*n+8)*u(n+3)+u(n+4),u(0) = 0,u(1) = 0,u(2) = 0,u(3) = 0});
Try[testrec, u, n](14,diffeqtorec(deq,y(z),u(n)),
-u(n)+2*u(n+1)+(2*n+8)*u(n+3)+u(n+4));

#end test
