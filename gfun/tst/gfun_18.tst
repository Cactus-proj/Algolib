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

# the initial conditions were not returned for versions < 3.13
TestTools:-Try(1,gfun:-holexprtodiffeq(sin(z)/z,y(z)),
	{diff(diff(y(z),z),z)*z+2*diff(y(z),z)+y(z)*z, y(0) = 1});
# the initial conditions were not returned for versions < 3.14
TestTools:-Try(2,gfun:-holexprtodiffeq(hypergeom([a,b],[c],z),y(z)),
	{a*b*y(z)+(z+z*a+z*b-c)*diff(y(z),z)+(z^2-z)*diff(diff(y(z),z),z), y(0) = 1});
# did not work for versions < 3.15
TestTools:-Try(3,gfun:-holexprtodiffeq(exp(arcsin(z)),y(z)),
	{y(z)+z*diff(y(z),z)+(-1+z^2)*diff(diff(y(z),z),z), y(0) = 1, D(y)(0) = 1});
# returned undesired initial conditions in versions < 3.16
TestTools:-Try(4,
   gfun:-diffeqtorec(gfun:-holexprtodiffeq(BesselJ(nu,x),y(x)),y(x),c(n)),c(n)+(-nu^2+n^2+4*n+4)*c(n+2));
# used to return y(z) in versions < 3.17
TestTools:-Try(5,
   gfun:-holexprtodiffeq(sin(sqrt(z)),y(z)), y(z)+2*diff(y(z),z)+4*z*diff(diff(y(z),z),z));
TestTools:-Try(6,
   gfun:-holexprtodiffeq(cos(sqrt(z)),y(z)), {y(z)+2*diff(y(z),z)+4*z*diff(diff(y(z),z),z),y(0)=1});
# improper op or subscript selector in versions < 3.18
TestTools:-Try(7,
   gfun:-holexprtodiffeq(dilog(1-x),y(x)),(-1+x)*diff(y(x),x)+1+(-x+x^2)*diff(diff(y(x),x),x));
# this syntax was invalid before 3.19
TestTools:-Try(8,
   gfun:-algeqtodiffeq(-1+(1-2*x)*y(x)-x^2*y(x)^2,y(x)),2+(-2+6*x)*y(x)+(-x+4*x^2)*diff(y(x),x));

#end test
