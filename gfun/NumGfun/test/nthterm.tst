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

## ...

rec := {u(n+3) = u(n) + u(n+1) + u(n+2), u(0) = 1, u(1) = 1, u(2) = 1};

Try(3.1,
    nth_term(rec,u(n),100),
    127071617887002752149434981);

Try(3.2,
    nth_term(rec,u(n),100,'series'),
    151404293106684183601223222);

#g := proc(s, prec)
#  local k, rec, a;
#  k := ceil(evalf((prec)*ln(10) + ln((prec)*ln(10)))):
#  rec := {u(n+1) = k/(n+s+1)*u(n), u(0) = 1/s}:
#  a := fnth_term(rec,u(n),6*k,prec+1,'series'):
#  evalf[prec+1](k^s*exp(-k)*a):
#end proc:
#p := 300:
#r1 := g(5/3,p);
#r2 := evalf[p](GAMMA(5/3));
#ASSERT(abs(r1-r2) <= 10^(-p));

