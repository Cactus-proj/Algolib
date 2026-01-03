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
$include <testutils.mm>

for deq in [
    holexprtodiffeq(arctan(z), y(z)),
    holexprtodiffeq(exp(z), y(z)),
    holexprtodiffeq(erf(z), y(z)),
    holexprtodiffeq(AiryAi(z), y(z)), # lent
    ## deq(arctan) with missing/symbolic/mixed initial values
    (1+z^2)*diff(y(z),z)-1,
    { (1+z^2)*diff(y(z),z)-1 },
    { -2*z*diff(y(z),z)+(-1-z^2)*diff(y(z),z,z),
        y(0) = sqrt(Pi)+t, D(y)(0) = a },
    # random equations (thanks to A. Benoit)
    {(4-z)*(diff(y(z), z, z))+(1/10*(-55-7*z^2+22*z))
        *(diff(y(z), z))+(-94*z+87)*y(z), y(0) = 2, (D(y))(0) = 1},
    {diff(y(z), z, z)+(1/10*(97-62*z))*(diff(y(z), z))
        +(1/7*(-83-73*z^2-4*z))*y(z), y(0) = 2, (D(y))(0) = 1},
    NULL
] do
    myproc := diffeqtoproc(deq, y(z), prec = 20,
        disks = [ [[-1], 1/2], [0,3/4], [[1/2,1], 1/2] ]);
    for pt in [ 0, 1/2, 1.15, -0.8234567887654, 1/3*(1+I) ] do
        Try("diffeqtoproc",
            myproc(pt, 16),
            evaldiffeq(deq, y(z), [pt], 16));
    end do;
end do;


