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

# One million decimal digits of Pi using the Chudnovsky formula and
# NumGfun:-fnth_term.

with(gfun:-NumGfun);
acc := 1000002;

A := 13591409;
B := 545140134;
C := 640320^3;
rec := { (A+B*n) * (3*n+3)* (3*n+2)*(3*n+1) * (n+1)^3 * C * u(n+1)
        + (6*n+6)*(6*n+5)*(6*n+4)*(6*n+3)*(6*n+2)*(6*n+1) * (A+B*(n+1)) * u(n), 
        u(0) = A };
t := time();
x := fnth_term(rec, u(n), ceil(acc/14), acc+1, 'series'):
p := evalf[acc](C^(1/2)/(12*x)):
time()-t;
save p, "pi.txt";
