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

with(gfun);
rec := { (n+3) * M(n+2) = 3*n*M(n) + (2*n+3)*M(n+1), M(0)=0, M(1)=1, M(2)=1 };
m := rectoproc(rec, M(n));
[seq(m(n),n=0..30)];
r := m(1000000):
save r, "m1000000.txt";
length(r);
