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

with(gfun); with(NumGfun);
b := 1: c := 0: q := 1:
time(evaldiffeq({(1-z^2)*diff(y(z),z,z) - 2*(b+1)*z*diff(y(z),z) + (c-4*q*z^2)*y(z), y(0)=1, D(y)(0)=0},
y(z), [0,1/3],1000));
time(evalf[1000](subs(z=1/3,HeunC(0,-1/2,b,q,1/4-1/4*b-1/4*c,z^2))));
