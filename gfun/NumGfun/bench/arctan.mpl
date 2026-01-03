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

with(gfun); with(NumGfun);
a := 1/3;
tests := 10;
interface(quiet=true);
deq := diffeqtohomdiffeq(holexprtodiffeq(arctan(z),y(z)),y(z));
seq(time(evaldiffeq(deq,y(z),[0,a],100*2^j)),j=0..tests);
gc();
seq(time(evalf(arctan(a),100*2^j)),j=0..tests);
