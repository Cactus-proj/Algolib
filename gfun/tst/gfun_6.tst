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
##### borel, invborel

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

## test if two recurrences are the same
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
# Test from the help page
rec:=a(n)-a(n-1)-a(n-2):

Try[testrec,a,n](1 ,borel(rec,a(n))  ,-a(n)+(-n-1)*a(n+1)+(n^2+3*n+2)*a(n+2) ):
Try[testrec,a,n](2 ,invborel(borel(rec, a(n)),a(n)), a(n+2)-a(n+1)-a(n)):



################## Test from the help page
Try[testrec,a,n](3 ,borel({a(n)=a(n-1)+a(n-2)+n,a(0)=0,a(1)=1},a(n))  ,{(n+3)*a(n)+a(n+1)*(n+1)+(-19*n-10-2*n^3-11*n^2)*a(n+2)+
(n^4+8*n^3+23*n^2+28*n+12)*a(n+3), a(2) = 3/2, a(0) = 0, a(1) = 1} ):

#################
rec:={a(n+2)-a(n+1)-a(n),a(0)=0,a(1)=1}:
rec2:=borel(rec,a(n)):

Try(4, invborel(borel(rec,a(n)), a(n)), rec);

#################
rec3:=rectodiffeq(rec2,a(n),f(t)):
rec2:=invborel(rec3,f(t),'diffeq'):
res:=diffeqtorec(rec2,f(t),a(n)):
if testrec(res,rec,a,n) then okay else rec,res fi;

Try[testrec,a,n](5 ,diffeqtorec(rec2,f(t),a(n)) ,rec ):

#################
eq:={z^2*diff(y(z),z,z)+y(z)=z,D(y)(0)=1}:

eitherorminus := proc(a, b)
 evalb ((a=b) or (-a=b))
end:

Try[eitherorminus](6, borel(eq,y(z),'diffeq'),z-y(z));

Try[testrec,a,n](7, borel(rectodiffeq(invborel({a(n)=a(n-1)+a(n-2),a(0)=0,a(1)=1},a(n)),a(n),
f(t)),f(t),'diffeq'),(-t^2-t+1)*f(t)-t ):

#################
Try[testrec,a,n](8 ,borel({y(0)=1, (D(y))(0) = 1, (1-z)*y(z)-z^2*(D(y))(z)-1},y(z),'diffeq')  ,(-z+1)*y(z)-1 ):

#end test
