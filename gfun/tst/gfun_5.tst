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
##### rec+rec, rec*rec, cauchyproduct, poltorec

#test 6

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

##########################################################################
# Test from the help page
rec1:=u(n+1)=(n+1)*u(n):
rec2:=u(n+1)=2*u(n):

Try[testrec, u, n](1,`rec*rec`(rec1,rec2,u(n)), (-2*n-2)*u(n)+u(n+1)):
Try[testrec, u, n](2, cauchyproduct(rec1,rec2,u(n))  ,
{(2*n+4)*u(n)+(-n-4)*u(n+1)+u(n+2),u(0)=_C[0],u(1)=3*_C[0]}):

#################
# This used to lose the initial conditions. Fixed BS Jan 95

Try[testrec, u, n](3  ,`rec*rec`({f(n+2)=f(n+1)+f(n),f(0)=1,f(1)=1},f(n)=-1,f(n)),
{f(n)+f(n+1)-f(n+2),f(0)=-1,f(1)=-1}):
#################
# This didn't work for many reasons. Fixed BS Jan 95.
# MM: still a bug!!
#fib:={f(n+2)=f(n+1)+f(n),f(0)=0,f(1)=1}:
#rec1:=`rec*rec`(fib,fib,f(n)):
#rec2:=`rec*rec`({f(n+2)=f(n+1)+f(n),f(0)=1,f(1)=1},{f(n+2)=f(n+1)+f(n),f(1)=0,f(2)=1},f(n)):
#rec2:=`rec*rec`(rec2,f(n)=-1,f(n)):

#Try[testrec, u, n](4,`rec+rec`(rec1,rec2,f(n))  ,{f(n+1)+f(n),f(0)=1}):
#################
# This used to find a 3rd order recurrence. Fixed BS March 95.
rec1:={u(n+1)=(n+1)*u(n),u(1)=1}:
rec2:={u(n+1)=2*u(n),u(1)=1}:

Try[testrec, u, n]( 5 ,`rec+rec`(rec1,rec2,u(n))  ,{(2*n+2*n^2)*u(n)+(-1+n)*u(n+2)+(2-n^2-3*n)*u(n+1),
u(1) = 2, u(0) = 3/2, u(2) = 4, u(3) = 10}):

#################
# an example with parameters, used to give a 4th order rec. Fixed BS Nov 95.
rec:=y(n+1)=a*y(n)-y(n-1): ini:=y(0)=c[0],y(1)=c[1]:
inires:=-a*c[1]*c[0]+c[1]^2+c[0]^2:
Try[testrec, u, n](6  , `rec+rec`(`rec*rec`(y(n)=-1,
      `rec*rec`({rec,y(0)=c[1],y(1)=a*c[1]-c[0]},
                {rec,y(1)=c[0],y(2)=c[1]},y(n)),y(n)),
      `rec*rec`({rec,ini},{rec,ini},y(n)),y(n)),
      {-y(n)+(-1+a^2)*y(n+1)+(1-a^2)*y(n+2)+y(n+3), seq(y(i) = inires,i=0..3)});

#################
# Bug in the inhomogeneous case. Fixed BS Aug 95
rec1:=u(n+1)-u(n) = -2/(n+1)^3:
rec2:=(n+1)*u(n+1)+u(n):

Try[testrec, u, n](7,`rec*rec`(rec1,rec2,u(n))  ,(n^2+2*n+1)*u(n)+(2*n^3+9*n^2+15*n+9)*u(n+1)+
(n^4+8*n^3+24*n^2+32*n+16)*u(n+2)):

#################
# Test from the help page
rec1:={u1(n+1)=(n+1)*u1(n),u1(0)=1}:
rec2:={u2(n+2)=2*u2(n+1)-3*n*u2(n),u2(1)=1,u2(0)=1}:

Try[testrec, u, n]( 8 , poltorec(u1(n)^2+2*u1(n)*u2(n),[rec1,rec2],[u1(n),u2(n)],u(n)) ,{u(2) = 12, u(3) = 48, u(0) = 3, u(1) = 3, (-462*n^4-363*n^2-90*n-579*n^3-3*n
^7-39*n^6-192*n^5)*u(n)+(354*n^2+254*n+60+209*n^3+5*n^5+54*n^4)*u(n+1)+(-15-n^4
-46*n^2-62*n-12*n^3)*u(n+2)+(4*n+n^2)*u(n+3)}):

#################
# Test from the help page
# Cassini's identity
fib:={F(n+2)=F(n+1)+F(n),F(0)=1,F(1)=1}:

Try[testrec, u, n](9 ,  poltorec(F(n+2)*F(n)-F(n+1)^2,[fib],[F(n)],f(n)), {f(0) = 1, f(n+1)+f(n)}):

#end test
