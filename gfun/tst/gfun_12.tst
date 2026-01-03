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

## mixed bag of functions

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
#######################################################
##### ratpolytocoeff
# Test from the help page
res1:=ratpolytocoeff(1/(1-x-x^2),x,n):
res2:=Sum(-(-1/5-2/5*_alpha)/_alpha*_alpha^(-n),_alpha = RootOf(-1+_Z+_Z^2)):
if res1=res2 then okay else res1,res2 fi;

Try(1, ratpolytocoeff(1/(1-x-x^2),x,n),Sum(-(-1/5-2/5*_alpha)/_alpha*_alpha^(-n),_alpha = RootOf(-1+_Z+_Z^2))):

#######################################################
##### listtoseries

Try(2 , convert(listtoseries([1,1,2,5,14,42,132,429,1430,4862,16796,58786],
x,'egf'),polynom),1+1*x+1*x^2+5/6*x^3+7/12*x^4+7/20*x^5+11/60*x^6+143/1680*x^7+143/4032*x^
8+2431/181440*x^9+4199/907200*x^10+4199/2851200*x^11):
#######################################################
##### seriestolist

Try(3 ,seriestolist(series(z-2*t*z^2+5*t^2*z^3-14*t^3*z^4+42*t^4*z^5+O(z^6),z),
revogf) ,[0,1,2*t,3*t^2,4*t^3,5*t^4] ):


#######################################################
#### listtohypergeom
# Test from the help page
normal_try:=(a,b)->evalb(normal(a-b)=[0,0]);
Try[normal_try](4 ,listtohypergeom([2,5,14,42,132,429,1430],x) ,[-16/(1-4*x)^(1/2)/(1+(1-4*x)^(1/2))^3*x-3/(1-4*x)^(1/2)/x+
3/(1-4*x)^(1/2)/x^2-1/2/(1-4*x)^(1/2)/x^3+1/2/x^3*(1-4*x), ogf] ):

#######################################################
##### listtoratpoly
# Test from the help page

Try(5 ,listtoratpoly([1,1,2,3,5,8,13],x) , [-1/(-1+x+x**2), ogf]):


#######################################################
##### listtorec

# Test from the help page


Try(6 ,listtorec([1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796, 58786],u(n)) ,[{(-2-4*n)*u(n)+(n+2)*u(n+1), u(0) = 1}, ogf] ):

# from <Christian.Mallinger@risc.uni-linz.ac.at>
ll:=[0, 0, 1, 0, 5/6, 0, 13/18, 0, 325/504, 0, 2665/4536, 0, 162565/299376, 0, 
1062925/2095632, 0, 24022105/50295168, 0, 204894425/452656512, 0, 7417178185/
17200947456, 0, 1639196378885/3973418862336, 0, 434387040404525/
1096663606004736, 0, 418348134297281/1096663606004736, 0, 152697069018507565/
414538843069790208, 0]:

# used to give [{a(0)=0, a(1)=0, (-2*n-2*n^2-n^3)*a(n)+(2*n+3*n^2+n^3)*a(n+2)}, ogf]
# where the recurrence contains the factor n so that a(2) is not defined
Try[testrec, a,n](7 ,listtorec(ll,a(n))[1] ,{a(0)=0,a(1)=0,a(2)=1,(-2-2*n-n^2)*a(n)+(2+3*n+n^2)*a(n+2)} ):


# used to give {a(0)=1,a(1)=0,a(n)}
ll:=[1,0,1/2!,0,1/4!,0,1/6!,0,1/8!]:

Try[testrec, a,n](8 ,listtorec(ll,a(n))[1] ,{a(n)+(-3*n-2-n^2)*a(n+2), a(1) = 0, a(0) = 1} ):

# listtodiffeq
# Test from the help page

Try(9.1 ,
op(2, listtodiffeq([1,2,6,22,91,408,1938,9614,49335,260130,1402440,7702632,
42975796,243035536,1390594458,8038677054,46892282815,275750636070,1633292229030
,9737153323590],y(x))), ogf):

Try[testdiffeq, y, x](9.2,
op(1,listtodiffeq([1,2,6,22,91,408,1938,9614,49335,260130,1402440,7702632,
42975796,243035536,1390594458,8038677054,46892282815,275750636070,1633292229030,9737153323590],y(x)))
,{y(0)=1,(D(y))(0)=2,4/9+(-4/9+20/9*x)*y(x)+(-2/3*x+4*x**2)*diff(y(x),x)+
(-4/27*x**2+x**3)*diff(diff(y(x),x),x)}):

# seriestodiffeq
# Test from the help page

Try[testdiffeq, y, x](10.1 ,op(1, seriestodiffeq(series(exp(x)/sqrt(1-x),x,7),y(x))),
{(3/2-x)*y(x)+(x-1)*diff(y(x),x), y(0) = 1}):
Try(10.2 ,op(2, seriestodiffeq(series(exp(x)/sqrt(1-x),x,7),y(x))),
ogf):

# listtoalgeq
# Test from the help page

my_normaltest:=proc(a,b)
type(normal(subs(y(x)=Y,a/b)), rational)
end;

Try[my_normaltest](11.1, op(1,listtoalgeq([1,1,2,5,14,42,132,429,1430,4862,16796,58786],y(x)) ),1-y(x)+x*y(x)**2);
Try(11.2, op(2,listtoalgeq([1,1,2,5,14,42,132,429,1430,4862,16796,58786],y(x)) ),ogf);


# seriestoalgeq
# Test from the help page

Try[my_normaltest](12.1,
op(1,seriestoalgeq(series((1-sqrt(1-4*x)),x,9),y(x))), 
4*x-2*y(x)+y(x)**2):

Try(12.2, op(2, seriestoalgeq(series((1-sqrt(1-4*x)),x,9),y(x))), ogf):

#end test
