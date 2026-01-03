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

with(TestTools):

with(gfun):
with(NumGfun):


## Order 1

rec := {u(n+1) = (n+1)*u(n), u(0) = 1};

Try(1.1,
    nth_term(rec,u(n),5),
    120);

Try(1.22,
    nth_term(rec,u(n),5,'series'),
    34);

## Trivial cases

rec := {u(n+1) = u(n), u(0) = 1};

Try(2.1,
    nth_term(rec,u(n),100),
    1);

Try(2.2,
    nth_term(rec,u(n),100,'series'),
    100);

## Misc

rec := {u(n+3) = u(n) + u(n+1) + u(n+2), u(0) = 1, u(1) = 1, u(2) = 1};

Try(3.1,
    nth_term(rec,u(n),100),
    127071617887002752149434981);

Try(3.2,
    nth_term(rec,u(n),100,'series'),
    151404293106684183601223222);
