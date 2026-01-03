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
#### algeqtodiffeq

#test 5

with(gfun):
with(TestTools):

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
# No initial conditions needed.
# Test from the help page

#res1:=algeqtodiffeq(y=1+z*y^2,y(z)):
#res2:=(-z/4+z**2)*D(y)(z)+(-1/4+z/2)*y(z)+1/4:
#if testdiffeq(res1,res2,y,z) then okay else res1,res2 fi;

Try[testdiffeq, y, z](1,algeqtodiffeq(y=1+z*y^2,y(z)), 
 (-z/4+z**2)*D(y)(z)+(-1/4+z/2)*y(z)+1/4);

##############
#res1:=algeqtodiffeq(y^2-x^2,y(x)):
#res2:={x*D(y)(x)-y(x),y(0)=0}:
#if testdiffeq(res1,res2,y,x) then okay else res1,res2 fi;

Try[testdiffeq, y, x](2,algeqtodiffeq(y^2-x^2,y(x)),
#			{x*D(y)(x)-y(x),y(0)=0} 
			x*D(y)(x)-y(x) );


##############
eq:=-28*x**2+3*y*x-31*y+y^2:
#res1:=algeqtodiffeq(eq,y(x)):
#res2:={1736/121*x+(-x+93/121)*y(x)+(x^2-186/121*x+961/121)*(D(y))(x),
#y(0)=RootOf(_Z^2-31*_Z),D(y)(0)=-3/31*RootOf(_Z^2-31*_Z)}:
#if testdiffeq(res1,res2,y,x) then okay else res1,res2 fi;

Try[testdiffeq, y, x](3,algeqtodiffeq(eq,y(x)), 
   {1736/121*x+(-x+93/121)*y(x)+(x^2-186/121*x+961/121)*(D(y))(x),y(0)=RootOf(_Z^2-31*_Z)}); 

##############
#res1:=algeqtodiffeq(eq,y(x),{y(0)=0}):
#res2:={1736/121*x+(-x+93/121)*y(x)+(x^2-186/121*x+961/121)*(D(y))(x),
#y(0) = 0,D(y)(0)=0}:
#if testdiffeq(res1,res2,y,x) then okay else res1,res2 fi;

Try[testdiffeq, y, x](4,algeqtodiffeq(eq,y(x),{y(0)=0}), 
{1736/121*x+(-x+93/121)*y(x)+(x^2-186/121*x+961/121)*(D(y))(x),y(0) = 0}); 

##############
#res1:=traperror(algeqtodiffeq(eq,y(x),{y(0)=1})):
#res2:=`invalid initial conditions`:
#if res1=res2 then okay else res1,res2 fi;
Try[testerror](5, algeqtodiffeq(eq,y(x),{y(0)=1}),"invalid initial conditions");
##TODO: I think there is some test error thing. 

############### Test from the help page
eq:=56*a^3+7*a^3*y^3-14*y*z:
tt:=rectoproc(diffeqtorec(algeqtodiffeq(eq,y(z),{y(0)=-2}),y(z),u(n)),u(n),
remember):
#res1:=map(normal,series(subs(y=listtoseries([seq(tt(i),i=0..10)],z,ogf),eq),z,10)):
#res2:=series(O(z^10),z):
#if res1=res2 then okay else res1,res2 fi;
Try(6, map(normal,series(subs(y=listtoseries([seq(tt(i),i=0..10)],z,ogf),eq),z,10)),series(O(z^10),z)):


######### Test from the help page (of diffeqtorec)
eq:=y=1+z*(y^2+y^3):
tt:=rectoproc(diffeqtorec(algeqtodiffeq(eq,y(z),{}),y(z),u(n)),u(n),remember):
#res1:=series(subs(y=listtoseries([seq(tt(i),i=0..10)],z,ogf),op(1,eq)-op(2,eq))
#,z,10):
#res2:=series(O(z^10),z):
#if res1=res2 then okay else res1,res2 fi;
Try(7,series(subs(y=listtoseries([seq(tt(i),i=0..10)],z,ogf),op(1,eq)-op(2,eq))
,z,10),  series(O(z^10),z));

######### Test from the help page
##res1:=algeqtodiffeq(y^5*(1-x)=1,y(x),{y(0)=1}):
res:={y(x)+(-5+5*x)*D(y)(x),y(0)=1}:
##if testdiffeq(res1,res2,y,x) then okay else res1,res2 fi;
Try[testdiffeq, y, x](8,algeqtodiffeq(y^5*(1-x)=1,y(x),{y(0)=1}), res);

########
#res1:=algeqtodiffeq(x*y^4-1,y(x)):
res:=y(x)+4*x*(D(y))(x):
#if testdiffeq(res1,res2,y,x) then okay else res1,res2 fi;
Try[testdiffeq, y, z](9,algeqtodiffeq(x*y^4-1,y(x)), res);

######## old bug
#res1:=algeqtodiffeq(-(4*x^2*z+4*z^2*x-4*x*z-x-z+1)
#*(4*f^2*x^2*z+4*f^2*z^2*x-4*f^2*x*z-f^2*x-f^2*z+f^2+4*f*x*z-f+x),f(x),{f(0) = 1/(-z+1)}):
res:={-2*x*z+1-z+(4*x^2*z-4*z^2*x-2*x*z-2*z^2+4*z-1)*f(x)+(-12*x^2*z-4*z^2*x+2*x*z+3*x+z-1+8*x^3*z+8*x^2*z^2-2*x^2)*(D(f))(x), f(0) = -1/(-1+z)}:
#if testdiffeq(res1,res2,f,x) then okay else res1,res2 fi;
Try[testdiffeq, f, x](10,algeqtodiffeq(-(4*x^2*z+4*z^2*x-4*x*z-x-z+1)
*(4*f^2*x^2*z+4*f^2*z^2*x-4*f^2*x*z-f^2*x-f^2*z+f^2+4*f*x*z-f+x),f(x),{f(0) = 1/(-z+1)}), res);

#end test
