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

#test
                                           
# New functionality in 3.22: option homogeneous=true in rectodiffeq.
rec:=u(n+1)=(n+1)*u(n):
TestTools:-Try(1,gfun:-rectodiffeq(rec,u(n),y(z)),(-z+1)*y(z)-z^2*diff(y(z),z)-_C[0]);
TestTools:-Try(2,gfun:-rectodiffeq(rec,u(n),y(z),homogeneous),{y(0) = _C[0], y(z)+(3*z-1)*diff(y(z),z)+z^2*diff(diff(y(z),z),z)});
TestTools:-Try(3,gfun:-rectodiffeq(rec,u(n),y(z),homogeneous,ini=false),y(z)+(3*z-1)*diff(y(z),z)+z^2*diff(diff(y(z),z),z));
TestTools:-Try(4,gfun:-rectodiffeq(rec,u(n),y(z),ini=false),y(z)+(3*z-1)*diff(y(z),z)+z^2*diff(diff(y(z),z),z));

rec:=(n+2)^2*u(n+2)-(11*n^2+33*n+25)*u(n+1)-(n+1)^2*u(n):    
TestTools:-Try(5,gfun:-rectodiffeq(rec,u(n),y(z)),{y(0) = _C[0], (-z-3)*y(z)+(-3*z^2-22*z+1)*diff(y(z),z)+(-z^3-11*z^2+z)*diff(diff(y(z),z),z)+3*_C[0]-_C[1]});
TestTools:-Try(6,gfun:-rectodiffeq(rec,u(n),y(z),homogeneous),{y(0) = _C[0], D(y)(0) = _C[1], -y(z)+(-7*z-25)*diff(y(z),z)+(-6*z^2-44*z+2)*diff(diff(y(z),z),z)+(-z^3-11*z^2+z)*diff(diff(diff(y(z),z),z),z)});
TestTools:-Try(7,gfun:-rectodiffeq(rec,u(n),y(z),homogeneous,ini=false),-y(z)+(-7*z-25)*diff(y(z),z)+(-6*z^2-44*z+2)*diff(diff(y(z),z),z)+(-z^3-11*z^2+z)*diff(diff(diff(y(z),z),z),z));
TestTools:-Try(8,gfun:-rectodiffeq(rec,u(n),y(z),ini=false),-y(z)+(-7*z-25)*diff(y(z),z)+(-6*z^2-44*z+2)*diff(diff(y(z),z),z)+(-z^3-11*z^2+z)*diff(diff(diff(y(z),z),z),z));

# Also, non-homogeneous recurrences in input should be ok
rec:=u(n+1)=u(n)+n:
TestTools:-Try(9,gfun:-rectodiffeq(rec,u(n),y(z)),(-z^3+3*z^2-3*z+1)*y(z)-_C[0]*z^2+2*_C[0]*z-_C[0]-z^2);
# The initial conditions do not bring information in this result. Changed test. BS jul 09.
#TestTools:-Try(10,gfun:-rectodiffeq(rec,u(n),y(z),homogeneous),{(2*z+1)*y(z)+(-1+4*z^2-3*z)*diff(y(z),z)+(z^3-2*z^2+z)*diff(diff(y(z),z),z), y(0) = _t[1], `@@`(D,2)(y)(0) = 2*_t[3]});
TestTools:-Try(10,gfun:-rectodiffeq(rec,u(n),y(z),homogeneous),(2*z+1)*y(z)+(-1+4*z^2-3*z)*diff(y(z),z)+(z^3-2*z^2+z)*diff(diff(y(z),z),z));
TestTools:-Try(11,gfun:-rectodiffeq(rec,u(n),y(z),homogeneous,ini=false),-2*y(z)+(-10*z+2)*diff(y(z),z)+(-7*z^2+7*z)*diff(diff(y(z),z),z)+(-z^3+2*z^2-z)*diff(diff(diff(y(z),z),z),z));
TestTools:-Try(12,gfun:-rectodiffeq(rec,u(n),y(z),ini=false),(-1+z^3-3*z^2+3*z)*y(z)+(1+z^4-4*z^3+6*z^2-4*z)*diff(y(z),z)-2*z);
              
# Used to be a bug in <=3.21
# > gfun:-holexprtodiffeq(sqrt(x^2-1),y(x));
# Error, (in gfun:-algeqtodiffeq) invalid initial conditions
# Changed test jul 09. BS.
# Getting I rather than RootOf(_Z^2+1) is actually an improvement: it corresponds to the meaning of sqrt(-1) in Maple.
#TestTools:-Try(13,gfun:-holexprtodiffeq(sqrt(x^2-1),y(x)),{-x*y(x)+(x^2-1)*diff(y(x),x), y(0) = RootOf(_Z^2+1)});
TestTools:-Try(13,gfun:-holexprtodiffeq(sqrt(x^2-1),y(x)),{-x*y(x)+(x^2-1)*diff(y(x),x), y(0) = I});

# Used to be a bug:    
# > gfun[listtorec]([0,1,4,5,8,9], a(n)); 
#          [{7 a(n + 3) + 4 a(n + 2) - 19 a(n + 1), a(0) = 0, a(1) = 1, a(2) = 4}, ogf]
# This used to succeed, but probably with too few terms (the guessed recurrence does not produce integers). Failing is ok.
# Test changed jul 09. BS.
#TestTools:-Try(14,gfun:-listtorec([0,1,4,5,8,9],a(n)),[{7*a(n+4)+4*a(n+3)-19*a(n+2), a(0) = 0, a(1) = 1, a(2) = 4, a(3) = 5}, ogf]);
TestTools:-Try(14,gfun:-listtorec([0,1,4,5,8,9],a(n)),FAIL);
TestTools:-Try(14.5,gfun:-listtorec([0,1,4,5,8,9,116/7,733/49],a(n)),[{a(n+4)+4/7*a(n+3)-19/7*a(n+2), a(0) = 0, a(1) = 1, a(2) = 4, a(3) = 5}, ogf]);
         
# Used to return Error, (in gfun:-goodinitvalues/diffeq) no valid initial conditions
TestTools:-Try(14,gfun:-rectodiffeq({(q*n-n-1+q)*u(n+1)+q*u(n), u(0) = 1}, u(n), f(t)),{f(0) = 1, q*f(t)+(q-1)*diff(f(t),t)});
#end test

