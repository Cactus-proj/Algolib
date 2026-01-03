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

# Bug fix for initial conditions in poltorec
#test
# This did not work for versions <=3.10
f:=3*x*(1-x)/(3-8*x+4*x^2):
eq:=numer(y(x)-f):

TestTools:-Try(1,coeff(series(solve(gfun:-hadamardproduct(eq,eq,y(x)),y(x)),x,6),x,5),coeff(series(f,x,6),x,5)^2);

# This returned unnecessary initial conditions in versions <=3.11
a:=(1-4*x-2*x^2+20*x^3+x^4-40*x^5-8*x^6+32*x^7+16*x^8)*y^2+(8*x-4*x^2+16*x
^4+4*x^5+8*x^8-2+8*x^7-16*x^3-14*x^6)*y+1+6*x^2-2*x^4-4*x+2*x^6-4*x^3+x^8+
4*x^5:

TestTools:-Try(2,gfun[algeqtodiffeq](a,y(x)),
4*x^6+2*x^4-16*x^3+12*x^2+2*x-2+(16*x^6+4*x^4+12*x^3-12*x^2-2*x+2)*y(x)+(
16*x^7+16*x^6-16*x^5-12*x^4+7*x^3+2*x^2-x)*diff(y(x),x));

#end test
