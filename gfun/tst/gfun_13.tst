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

########guessing functions

#test

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




#######################################################
##### guesseqn
# Test from the help page

Try[testdiffeq, y, z](1, guesseqn([1, 4, 36, 400, 4900, 63504, 853776,
11778624, 165636900, 2363904400,34134779536, 497634306624, 7312459672336],y(z))[1],
{1/4*y(z)+(2*z-1/16)*diff(y(z),z)+(z^2-1/16*z)*diff(diff(y(z),z),z),
D(y)(0) = 4, y(0) = 1}):


#######################################################
#### guessgf
# Test from the help page

expand_try:=(a,b)->evalb(expand(a-b)=0);
Try[expand_try](2,guessgf([1,1,3,10,41,196,1057],x,['lgdegf'])[1],exp(x)+exp(x)*x):

###########################################################
s:=seriestolist(map(collect,series((1/(1-x))^t,x,7),t)):
#res1:=guessgf(s,x):
#res2:=[(x-1)^(-t)/exp(-I*Pi*t), ogf]:
#res3:=[(1-x)^(-t),ogf]:
#if res1=res2 or res1=res3 then okay else res1,res3 fi;##

#eitheror:= (a,b,c)-> evalb(normal(a/b)=1 or normal(a/c)=1);
#Try[eitheror,(1-x)^(-t)](2,guessgf(s,x)[1],(x-1)^(-t)/exp(-I*Pi*t) ):


normal_try:=(a,b)-> evalb(normal(a[1]/b[1])=1 and (a[2]=b[2]));

Try[normal_try](3, guessgf(s,x,['lgdogf']),[-t/(x-1),'lgdogf']);
Try[normal_try](4, guessgf([ 9,8,7,6,5,4,3,2,1,2,3,4,5,6,7,8,9,10,11],x),
         [-10/(x^2-2*x+1)*x+9/(x^2-2*x+1)+2/(x^2-2*x+1)*x^9, ogf]):
Try[normal_try](5,guessgf([1,2,4,7,11,16,22],x), [-(1-x+x**2)/(x-1)**3, ogf]):

###########################################################
Try(6 ,guessgf([1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786],x,[revogf])[1],[x-x^2]):

# Used to FAIL. Fixed BS Aug 04.
# Parameters('maxdegcoeff'=0):
Try(7, seriestorec(series(1/(1-x-x^2),x,10),u(n)), [{-u(n+2)+u(n+1)+u(n), u(0) = 1, u(1) = 1}, ogf]):

# Used to FAIL. Fixed BS Aug 04.
s:=
series(1/479001600*x^12+1/59875200*x^13+13/239500800*x^14+169/1596672000*x^15+2801/19160064000*x
^16+35983/229920768000*x^17+189377/1379524608000*x^18+76403/746197401600*x^19+1621267/
24320507904000*x^20+228273761/5909883420672000*x^21+1194637399/59098834206720000*x^22+
119393131013/12351656349204480000*x^23+8213992511383/1926858390475898880000*x^24+16831693910881/
9634291952379494400000*x^25+90313697397743/134880087333312921600000*x^26+1877467091231/
7781543499998822400000*x^27+1202667386971/14647611294115430400000*x^28+14293085533561/
539520349333251686400000*x^29+1184599565749631/145670494319977955328000000*x^30+1450957532123357
/609167521701725995008000000*x^31+147849385829023/221515462436991270912000000*x^32+
134603783171694853/750494386736526425849856000000*x^33+254894554970217961/
5503625502734527122898944000000*x^34+55697070871520417/4837170852012767979110400000000*x^35+O(x^\
36),x,36):

# Parameters('maxdegcoeff'=5):
Parameters('maxordereqn'=5):

Try(8,seriestodiffeq(s,y(x)),
[{`@@`(D,2)(y)(0) = 0, (-48*x^4+218*x^3+1830*x^2+24640*x)*diff(y(x),x)+(-8*x^5-174*x^4-176*x^3-\
3310*x^2)*diff(diff(y(x),x),x)+(-15*x^5-105*x^4-226*x^3)*diff(diff(diff(y(x),x),x),x)+(-6*x^5+21
*x^4)*diff(diff(diff(diff(y(x),x),x),x),x)+x^5*diff(diff(diff(diff(diff(y(x),x),x),x),x),x)+(
95040+1120*x^2-48*x^3+3280*x)*y(x), `@@`(D,3)(y)(0) = 0, y(0) = 0, `@@`(D,4)(y)(0) = 0, D(y)(0)
= 0}, ogf]):

## Used to FAIL after a long time. Fixed BS Aug 04.
Try(9,seriestodiffeq(series(hypergeom([a,b],[c],x),x,7),y(x)),
[{(-x^2+x)*diff(diff(y(x),x),x)+(c-x*a-x*b-x)*diff(y(x),x)-y(x)*a*b, D(y)(0) = a*b/c, y(0) = 1}, ogf]):

## Used to work as if the valuation was 1. Fixed BS Aug 04.
Try[testerror](10,seriestoseries(series(u^2/(1-u),u),'revogf'),"expected a series with valuation 1"):

#end test
