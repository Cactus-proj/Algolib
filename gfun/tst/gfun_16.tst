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

# test for SCR 41660
#
#test

# This did not work for versions <= 3.08
answer:={y(0) = sin(cos(tt+1/4*Pi)), x^2*y(w[1])+diff(diff(y(w[1]),w[1]),w[1]), D(y)(0) = cos(cos(tt+1/4*Pi))*x};

TestTools:-Try(1,gfun[holexprtodiffeq](sin(w[1]*x+cos(tt+Pi/4)),y(w[1])),answer);
TestTools:-Try(2,gfun[holexprtodiffeq](sin(w[1]*x+cos(tt+Pi/4)),z(w[1])),eval(answer,y=z));

#end test
