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

#test
# New Int(f,x) in holexprtodiffeq (3.36)

TestTools:-Try(1,gfun:-holexprtodiffeq(sin(x)+Int(cos(x),x),y(x)),diff(y(x),x$3)+diff(y(x),x));
TestTools:-Try(2,gfun:-holexprtodiffeq(Int(sin(x)^3,x),y(x)),diff(y(x),x$5)+9*diff(y(x),x)+10*diff(y(x),x$3));
TestTools:-Try(3,gfun:-holexprtodiffeq(Int(Int(sin(x)^3,x),x),y(x)),diff(y(x),x$6)+9*diff(y(x),x$2)+10*diff(y(x),x$4));

# Bug fix in rectoproc (3.37) 

rec := {u(n)*n+u(n+2), u(0) = A, u(1) = B}:
p := gfun[rectoproc](rec,u(n),params=[A,B]):
TestTools:-Try(4,p(0,AA,BB),AA); # used to return BB

# Another bug fix in rectoproc (3.38)
p:=gfun:-rectoproc({u(n+2)=u(n+1)+u(n),u(0)=2,u(1)=3},u(n),evalfun=evalf):
# p(1) used to return evalf(3) (not evaluated)
TestTools:-Try(4,p(1),evalf(3));

# Fix in findequationgivenorderratpoly (3.38)
# used to create an error in modp1 because
# one of the coefficients returned by CurveFitting:-RationalInterpolation
# had a z in the denominator.
TestTools:-Try(5,gfun:-seriestodiffeq(series(1/(1-q*p^2*z^2-2*p*z),z,40),y(z)),
[{(2*p+2*z*q*p^2)*y(z)+(-1+2*p*z+q*p^2*z^2)*diff(y(z),z), y(0) = 1}, ogf]);

# Fix a bug introduced in rectoproc by the change in 3.38 (evalfun + option remember)
F:=1/(1-x^2*y-x*y^3):
P:=gfun:-rectoproc(gfun:-diffeqtorec(G(y)-F,G(y),U(k)),U(k),evalfun=expand,remember):
TestTools:-Try(6,P(10),4*x^5+15*x^10+8*x^15+x^20);

# Fix a bug in rationalinterpolation that could not recognize [0$k] (3.41)
# reported by Cyril Banderier 26/06/09
F:=1/sqrt(1+u^2*z^2-2*u*z^2-2*u*z-2*u^2*z^3+u^2*z^4):
TestTools:-Try(7,gfun:-seriestoalgeq(map(expand,series(F,z,20)),y(z),[ogf]),
[-1+(1+u^2*z^2-2*u*z^2-2*u*z-2*u^2*z^3+u^2*z^4)*y(z)^2, ogf]
);

# Simple borderline cases that used to fail
TestTools:-Try(8,
gfun:-listtorec([1,0$10],u(n),[ogf]),
[{u(n+1), u(0) = 1}, ogf]);

TestTools:-Try(9,
gfun :- seriestorec(series(1,x,100), u(n), [ogf]),
[{u(n+1), u(0) = 1}, ogf]);

TestTools:-Try(10,
gfun :- seriestorec(series(x,x,infinity), u(n), [ogf]),
[{(n-1)*u(n), u(0) = 0}, ogf]);

# The same ones, with diffeq instead of rec
TestTools:-Try(11,
        gfun:-listtodiffeq([1],y(x)),FAIL);

TestTools:-Try(12,
gfun:-listtodiffeq([1,0$10],y(x),[ogf]),
[{diff(y(x),x),y(0)=1}, ogf]);

TestTools:-Try(9,
gfun :- seriestodiffeq(series(1,x,100), y(x), [ogf]),
[{diff(y(x),x),y(0)=1}, ogf]);

TestTools:-Try(10,
gfun :- seriestodiffeq(series(x,x,infinity), y(x), [ogf]),
[{-y(x)+x*diff(y(x),x), y(0) = 0}, ogf]);

# And then with algeq
TestTools:-Try(11,
        gfun:-listtoalgeq([1],y(x)),FAIL);

TestTools:-Try(12,
gfun:-listtoalgeq([1,0$10],y(x),[ogf]),
[y(x)-1, ogf]);

TestTools:-Try(9,
gfun :- seriestoalgeq(series(1,x,100), y(x), [ogf]),
[y(x)-1, ogf]);

TestTools:-Try(10,
gfun :- seriestoalgeq(series(x,x,infinity), y(x), [ogf]),
[y(x)-x, ogf]);

# Better error checking
#L:=[seq(exp(i),i=1..10)];
#TestTools:-Try[testerror](11,
#gfun:-listtorec(L,u(n)),"not a list of ratpoly over the rationals: ");
#S:=gfun:-listtoseries(L,z);
#TestTools:-Try[testerror](12,
#gfun:-seriestorec(S,u(n)),"not a list of ratpoly over the rationals: ");
# Not an error any longer (3.57)
L:=[seq(exp(i),i=1..10)];
TestTools:-Try(11,
gfun:-listtorec(L,u(n)),FAIL);
S:=gfun:-listtoseries(L,z);
TestTools:-Try(12,
gfun:-seriestorec(S,u(n)),FAIL);

# Change in 3.51, added an option ini=false to diffeqtorec
deq:={-19*y(x)+(x+37)*diff(y(x),x)+(-2*x-18)*diff(diff(y(x),x),x)+x*diff(diff(diff(y(x),x),x),x), y(0
) = 0, D(y)(0) = 1/19, `@@`(D,2)(y)(0) = 37/342}:
TestTools:-Try[testerror](13,
gfun:-diffeqtorec(deq,y(x),u(n)),
"no valid initial conditions");
TestTools:-Try(14,
gfun:-diffeqtorec(deq,y(x),u(n),ini=false),
(-19+n)*u(n)+(35*n+37-2*n^2)*u(n+1)+(-15*n^2-52*n-36+n^3)*u(n+2)
);

# There was a bug in the new guessing part, which made it miss the following
# Fixed in 3.58
TestTools:-Try(15,
gfun:-listtodiffeq([1, 4, 36, 400, 4900, 63504, 853776, 11778624, 165636900,
2363904400, 34134779536, 497634306624, 7312459672336], y(x), [ogf]),
[{4*y(x)+(32*x-1)*diff(y(x),x)+(16*x^2-x)*diff(diff(y(x),x),x), y(0) = 1, D(y)(0) = 4}, ogf]
);

# This was not found before 3.60
L:=
TestTools:-Try(16,
gfun:-listtoratpoly([1,1,1,1,0,1,0,1,1,1,0,0,1,1,1,0,1,0,1,1,1,0,0],x,['ogf']),
[-(-x^11+x^9+x^8+x^7+x^5+x^3+x^2+x+1)/(x^11-1), ogf]
);

#end test
                       