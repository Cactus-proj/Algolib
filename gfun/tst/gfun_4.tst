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

################################################################
##### diffeq+diffeq, diffeq*diffeq, hadamardproduct, poltodiffeq

#test 18

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

###################################################
eq := diff(y(z),z,z)+y(z):
#res1:=`diffeq*diffeq`(eq,eq,y(z)):
#res2:=(D@@3)(y)(z)+4*D(y)(z):
#if testdiffeq(res1,res2,y,z) then okay else res1,res2 fi;

Try[testdiffeq, y, z](1,
`diffeq*diffeq`(eq,eq,y(z)),
(D@@3)(y)(z)+4*D(y)(z));

###################################################
rec1:={u(n+1)=(n+1)*u(n),u(0)=1}:
rec2:={u(n+1)=2*u(n),u(0)=1}:
deq1:=rectodiffeq(rec1,u(n),y(z)):
deq2:=rectodiffeq(rec2,u(n),y(z)):
deq3:=`diffeq+diffeq`(deq1,deq2,y(z)):
rec3:=diffeqtorec(deq3,y(z),u(n)):
res:=rectoproc(rec3,u(n))(100)-(2^100+100!):
#if res=0 then okay else res,0 fi;

Try(2, res, 0);

###################################################
# Test from the help page
eq1 := D(y)(x)-y(x):
eq2 := (1+x)*(D@@2)(y)(x)+D(y)(x):
#res1:=`diffeq+diffeq`(eq1,eq2,y(x)):
#res2:=(-x-3)*(D(y))(x)+(-x^2+1-2*x)*((D@@2)(y))(x)+(2+3*x+x^2)*((D@@3)(y))(x):
#if testdiffeq(res1,res2,y,x) then okay else res1,res2 fi;

Try[testdiffeq, y, x](3,`diffeq+diffeq`(eq1,eq2,y(x)),
(-x-3)*(D(y))(x)+(-x^2+1-2*x)*((D@@2)(y))(x)+(2+3*x+x^2)*((D@@3)(y))(x));


#res1:=`diffeq*diffeq`(eq1,eq2,y(x)):
#res2:=y(x)*x+(-2*x-1)*(D(y))(x)+(1+x)*((D@@2)(y))(x):
#if testdiffeq(res1,res2,y,x) then okay else res1,res2 fi;

Try[testdiffeq, y, x](4 ,
`diffeq*diffeq`(eq1,eq2,y(x)),
y(x)*x+(-2*x-1)*(D(y))(x)+(1+x)*((D@@2)(y))(x)
);

#res1:=hadamardproduct(eq1,eq2,y(x)):
#res2:={(-x-1)*(D(y))(x)-(D@@2)(y)(x)*x+_C[1],y(0)=_C[0],D(y)(0)=_C[1]}:
#if testdiffeq(res1,res2,y,x) then okay else res1,res2 fi;


Try[testdiffeq, y, x](5 ,hadamardproduct(eq1,eq2,y(x)),
#{(-x-1)*(D(y))(x)-(D@@2)(y)(x)*x+_C[1],y(0)=_C[0],D(y)(0)=_C[1]}
{(-x-1)*(D(y))(x)-(D@@2)(y)(x)*x+_C[1],y(0)=_C[0]}
);
###################################################
# Initial conditions were lost. Fixed BS Jan 95.
Sin:={diff(y(z),z,z)=-y(z),y(0)=0,D(y)(0)=1}:
Cos:={diff(y(z),z,z)=-y(z),y(0)=1,D(y)(0)=0}:
Sin2:=`diffeq*diffeq`(Sin,Sin,y(z)):
Cos2:=`diffeq*diffeq`(Cos,Cos,y(z)):
#one:=`diffeq+diffeq`(Sin2,Cos2,y(z)):
#res:={4*D(y)(z)+(D@@3)(y)(z),y(0)=1,D(y)(0)=0,(D@@2)(y)(0)=0}:
#if testdiffeq(one,res,y,z) then okay else one,res fi;

Try[testdiffeq, y, z](6 ,`diffeq+diffeq`(Sin2,Cos2,y(z)),
{4*D(y)(z)+(D@@3)(y)(z),y(0)=1,D(y)(0)=0,(D@@2)(y)(0)=0}
);
###################################################
# Invalid format for the result. Fixed BS Jan 95.
deq:=(1-z-z^2)*y(z)-1:
#res1:=`diffeq+diffeq`(deq,deq,y(z)):
#res2:=(1-z-z^2)*y(z)-2:

Try[testdiffeq, y, z](7 ,`diffeq+diffeq`(deq,deq,y(z)),
(1-z-z^2)*y(z)-2
);
###################################################
if testdiffeq(res1,res2,y,z) then okay else res1,res2 fi;
# Invalid initial conditions in the result. Fixed BS Feb 95.
#res1:=`diffeq+diffeq`(
# {y(0)=a, D(y)(0)=a, y(z)+(3*z-1)*D(y)(z)+z^2*(D@@2)(y)(z)},
# {y(0)=a/2, D(y)(0)=a, 2*y(z)+(4*z-1)*D(y)(z)+z^2*(D@@2)(y)(z)},y(z)):
#res2:=
#{-4*y(z)+(-32*z+8)*D(y)(z)+(-38*z^2+10*z-1)*(D@@2)(y)(z)
#+(-12*z^3+2*z^2)*(D@@3)(y)(z)-z^4*(D@@4)(y)(z),
#D(y)(0) = 2*a, (D@@2)(y)(0) = 10*a, y(0) = 3/2*a,
#(D@@3)(y)(0) = 108*a}:
Try[testdiffeq, y, z](8,`diffeq+diffeq`(
 {y(0)=a, D(y)(0)=a, y(z)+(3*z-1)*D(y)(z)+z^2*(D@@2)(y)(z)},
 {y(0)=a/2, D(y)(0)=a, 2*y(z)+(4*z-1)*D(y)(z)+z^2*(D@@2)(y)(z)},y(z)) ,
#{-4*y(z)+(-32*z+8)*D(y)(z)+(-38*z^2+10*z-1)*(D@@2)(y)(z)
#+(-12*z^3+2*z^2)*(D@@3)(y)(z)-z^4*(D@@4)(y)(z),
#D(y)(0) = 2*a, (D@@2)(y)(0) = 10*a, y(0) = 3/2*a,
#(D@@3)(y)(0) = 108*a}
{-4*y(z)+(-32*z+8)*D(y)(z)+(-38*z^2+10*z-1)*(D@@2)(y)(z)
+(-12*z^3+2*z^2)*(D@@3)(y)(z)-z^4*(D@@4)(y)(z),
D(y)(0) = 2*a, y(0) = 3/2*a}
);
#if testdiffeq(res1,res2,y,z) then okay else res1,res2 fi;
###################################################
res1:=`diffeq+diffeq`(
 {y(0)=a, D(y)(0)=a, y(z)+(3*z-1)*D(y)(z)+z^2*(D@@2)(y)(z)},
 {y(0)=b/2, D(y)(0)=b, 2*y(z)+(4*z-1)*D(y)(z)+z^2*(D@@2)(y)(z)},y(z)):
res2:=
{-4*y(z)+(-32*z+8)*D(y)(z)+(-38*z^2+10*z-1)*(D@@2)(y)(z)
+(-12*z^3+2*z^2)*(D@@3)(y)(z)-z^4*(D@@4)(y)(z),
D(y)(0) = a+b, (D@@2)(y)(0) = 4*a+6*b, y(0) = a+b/2,
(D@@3)(y)(0) = 36*a+72*b}:
if testdiffeq(res1,res2,y,z) then okay else res1,res2 fi;

Try[testdiffeq, y, z](9,`diffeq+diffeq`(
 {y(0)=a, D(y)(0)=a, y(z)+(3*z-1)*D(y)(z)+z^2*(D@@2)(y)(z)},
 {y(0)=b/2, D(y)(0)=b, 2*y(z)+(4*z-1)*D(y)(z)+z^2*(D@@2)(y)(z)},y(z)),
{-4*y(z)+(-32*z+8)*D(y)(z)+(-38*z^2+10*z-1)*(D@@2)(y)(z)
+(-12*z^3+2*z^2)*(D@@3)(y)(z)-z^4*(D@@4)(y)(z),
#D(y)(0) = a+b, (D@@2)(y)(0) = 4*a+6*b, y(0) = a+b/2,
#(D@@3)(y)(0) = 36*a+72*b}
D(y)(0) = a+b, y(0) = a+b/2}
);
###################################################
# Wrong initial conditions. Fixed BS Jan 96.
res1:=poltodiffeq(y1(x)^3, [(D@@2)(y1)(x)+y1(x)], [y1(x)], y(x)):
res2:={`@@`(D,4)(y)(x)+10*`@@`(D,2)(y)(x)+9*y(x), D(y)(0) = -3*_C[1]*_C[0]^2,
y(0) = _C[0]^3, `@@`(D,2)(y)(0) = -3*_C[0]^3+6*_C[1]^2*_C[0],
`@@`(D,3)(y)(0) = 21*_C[1]*_C[0]^2-6*_C[1]^3}:
if testdiffeq(res1,res2,y,x) then okay else res1,res2 fi;

Try[testdiffeq, y, x](10 ,poltodiffeq(y1(x)^3, [(D@@2)(y1)(x)+y1(x)], [y1(x)], y(x)),
{`@@`(D,4)(y)(x)+10*`@@`(D,2)(y)(x)+9*y(x), D(y)(0) = -3*_C[1]*_C[0]^2,
y(0) = _C[0]^3, `@@`(D,2)(y)(0) = -3*_C[0]^3+6*_C[1]^2*_C[0],
`@@`(D,3)(y)(0) = 21*_C[1]*_C[0]^2-6*_C[1]^3}
);
###################################################
# Test from the help page
Sin:={diff(y1(z),z,z)=-y1(z),y1(0)=0,D(y1)(0)=1}:
Cos:={diff(y2(z),z,z)=-y2(z),y2(0)=1,D(y2)(0)=0}:
#res:=poltodiffeq(y1(z)^2+y2(z)^2,[Sin,Cos],[y1(z),y2(z)],y(z)):
one:=`diffeq+diffeq`(Sin2,Cos2,y(z)):
#if testdiffeq(one,res,y,z) then okay else one,res fi;

Try[testdiffeq, y, z](11 ,poltodiffeq(y1(z)^2+y2(z)^2,[Sin,Cos],[y1(z),y2(z)],y(z)),
one
);

res1:=poltodiffeq(y1(z)^2+diff(y1(z),z)^2,[Sin],[y1(z)],y(z)):
res2:={y(0) = 1, D(y)(z)}:
if testdiffeq(res1,res2,y,z) then okay else res1,res2 fi;

Try[testdiffeq, y, z](12 ,poltodiffeq(y1(z)^2+diff(y1(z),z)^2,[Sin],[y1(z)],y(z)),
{y(0) = 1, D(y)(z)}
);

###################################################
bessel:=z^2*diff(w(z),z,z)+z*diff(w(z),z)+(z^2-nu^2)*w(z):
j0:={subs(nu=0,w=w0,bessel),w0(0)=1,D(w0)(0)=0}:
j1:={subs(nu=1,w=w1,bessel),w1(0)=0,D(w1)(0)=1/2}:
#res1:=poltodiffeq(diff(w0(z),z)+w1(z),[j0,j1],[w0(z),w1(z)],y(z)):
#res2:={(z^2-1)*y(z)+(D@@2)(y)(z)*z^2+z*(D(y))(z), y(0) = 0, (D(y))(0) = 0}:
#if testdiffeq(res1,res2,y,z) then okay else res1,res2 fi;

Try[testdiffeq, y, z](13,poltodiffeq(diff(w0(z),z)+w1(z),[j0,j1],[w0(z),w1(z)],y(z)),
#{(z^2-1)*y(z)+(D@@2)(y)(z)*z^2+z*(D(y))(z), y(0) = 0, (D(y))(0) = 0}
y(z)
);

###################################################
#res1:=poltodiffeq(y2(z)^2+y2(z),[Cos],[y2(z)],y(z)):
#res2:={(D@@5)(y)(z)+5*(D@@3)(y)(z)+4*D(y)(z), y(0) = 2, D(y)(0) = 0, 
#(D@@2)(y)(0) = -3, (D@@3)(y)(0) = 0, (D@@4)(y)(0) = 9}:
#if testdiffeq(res1,res2,y,z) then okay else res1,res2 fi;

Try[testdiffeq, y, z](14,poltodiffeq(y2(z)^2+y2(z),[Cos],[y2(z)],y(z))  ,
{(D@@5)(y)(z)+5*(D@@3)(y)(z)+4*D(y)(z), y(0) = 2, D(y)(0) = 0, 
(D@@2)(y)(0) = -3, (D@@3)(y)(0) = 0, (D@@4)(y)(0) = 9}
);


###################################################
#res1:=poltodiffeq(y(x),[x*y(x)-1],[y(x)],y(x)):
#res2:=x*y(x)-1:
#if testdiffeq(res1,res2,y,z) then okay else res1,res2 fi;

Try[testdiffeq, y, z](15, poltodiffeq(y(x),[x*y(x)-1],[y(x)],y(x)) ,
x*y(x)-1
);

###################################################
#res1:=poltodiffeq(y[1](x),[{diff(y[1](x),x)-y[1](x), y[1](0) = 1}],[y[1](x)],y(x)):
#res2:={D(y)(x)-y(x), y(0) = 1}:
#if testdiffeq(res1,res2,y,x) then okay else res1,res2 fi;

Try[testdiffeq, y, x](16,poltodiffeq(y[1](x),[{diff(y[1](x),x)-y[1](x), y[1](0) = 1}],[y[1](x)],y(x)),{D(y)(x)-y(x), y(0) = 1}
);

###################################################
#res1:=poltodiffeq((y[2])(x)+(y[1])(x), [{(y[1])(0) = 0 , (D(y[1]))(0) = 1,
# diff(diff((y[1])(x),x),x)+(y[1])(x)}, 1-x*(y[2])(x)], [(y[1 ])(x),(y[2])(x)], y(x)):
#res2:=(D@@2)(y)(x)*x^3-2+y(x)*x^3-x^2:
#if testdiffeq(res1,res2,y,x) then okay else res1,res2 fi;

Try[testdiffeq, y, z](17 , poltodiffeq((y[2])(x)+(y[1])(x), [{(y[1])(0) = 0 , (D(y[1]))(0) = 1,
 diff(diff((y[1])(x),x),x)+(y[1])(x)}, 1-x*(y[2])(x)], [(y[1 ])(x),(y[2])(x)], y(x)) ,
(D@@2)(y)(x)*x^3-2+y(x)*x^3-x^2
);

#################
# This used to lose an initial condition
res1:=poltodiffeq(z^4*y[1](z),
[{D(y[1])(0) = 1, diff(diff(y[1](z),z),z)+y[1](z), y[1](0) = 0}],[y[1](z)],y(z)):
res2:={(20+z^2)*y(z)+(D@@2)(y)(z)*z^2-8*D(y)(z)*z, y(0) = 0, (D@@4)(y)(0) = 
0, (D@@5)(y)(0) = 120, (D@@2)(y)(0) = 0, (D@@3)(y)(0) = 0, D(y)(0) = 0}:
if testdiffeq(res1,res2,y,z) then okay else res1,res2 fi;

Try[testdiffeq, y, z](18, poltodiffeq(z^4*y[1](z),
[{D(y[1])(0) = 1, diff(diff(y[1](z),z),z)+y[1](z), y[1](0) = 0}],[y[1](z)],y(z)),
#{(20+z^2)*y(z)+(D@@2)(y)(z)*z^2-8*D(y)(z)*z, y(0) = 0, (D@@4)(y)(0) = 
#0, (D@@5)(y)(0) = 120, (D@@2)(y)(0) = 0, (D@@3)(y)(0) = 0, D(y)(0) = 0}
{(20+z^2)*y(z)+(D@@2)(y)(z)*z^2-8*D(y)(z)*z, (D@@4)(y)(0) = 0, (D@@5)(y)(0) = 120}
);

#end test
