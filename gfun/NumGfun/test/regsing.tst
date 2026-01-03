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

# Regular singular points

with(TestTools):
$include <testutils.mm>
with(gfun):
with(NumGfun):


# Test various forms of singular analytic continuation paths

deq := diffeqtohomdiffeq(holexprtodiffeq(arctan(z), y(z)), y(z));

# local basis = [1, arctan]
# expand(op(1,series(arctan(z), z=I, 1))) assuming z-I>0;
# 1/2*I*ln(2)-1/2*I*ln(z-I)+1/4*Pi
dirI := Matrix([[0, -I/2], [1, Pi/4 + I/2*ln(2)]]);
invI := dirI^(-1);
id := Matrix([[1,0],[0,1]]);

for data in [
    [ [0, I],       dirI ],
    [ [I, 0],       invI ],
    [ [I],          id   ],
    [ [I, I],       id   ],
    [ [I, I, I],    id   ],
    [ [0, 1, I],    dirI ],
    [ [0, I, 0],    id   ],
    [ [I, 1, I],    id   ],
    [ [I, -I, 0],   invI ]
] do
    path, ref := op(data);
    Try[verify, 'Matrix(neighborhood(10^(-10)))'](convert(path,'string'),
        transition_matrix(deq, y(z), path, 10),
        evalf[20](ref));
end do;

# Incorrect paths

for path in [
    I,
    2*I,
    [0, 2*I],
    [I, 0, 2*I]
] do
    Try[testerror](convert(path, 'string'),
        transition_matrix(deq, y(z), path));
end do;

# Irregular singular point

Try[testerror]("irreg",
    transition_matrix(holexprtodiffeq(exp(1/(1-z)), y(z)), y(z), [0,1], 10));

# Bug reported by Christoph Koutschan: we used not to detect additional
# singular points on a straight-line path whose endpoints were themselves
# regular singular points

deq := (-8*z^3+4*z^4+5*z^2-z)*diff(y(z),z)+10*z^2-4*z-8*z^3+1;
Try[testerror]("additional sing 1",
    transition_matrix(deq, y(z), [0,1], 10));

# catch harcoded variable names

deq := holexprtodiffeq(arctan(w), g(w));

Try[testnoerror]("varnames",
    transition_matrix(deq, g(w), [0,I]));

Try[testnoerror]("varnames",
    analytic_continuation(deq, g(w), [0,I], ord=3));
