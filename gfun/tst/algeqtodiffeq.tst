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

infolevel[gfun]:=4:
algeqtodiffeq(y=1+z*y^2,y(z));
algeqtodiffeq(y^2-x^2,y(x));
algeqtodiffeq(-28*x**2+3*y*x-31*y+y^2,y(x));
algeqtodiffeq(y=1+z*(y^2+y^3),y(z));
algeqtodiffeq(y^5*(1-x)=1,y(x));
algeqtodiffeq(x*y^4-1,y(x));
