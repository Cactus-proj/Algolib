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
$include <testutils.mm>

with(gfun):
with(NumGfun):

# examples from the introduction page NumGfun.mws

Try[testabsprec, 50]("help intro 1",
    fnth_term({(3*n+3)*u(n+1)=(3*n+5)*u(n), u(0)=1}, u(n), 2000, 50),
    175.89036294166519099188900014849043485353660447070009);

deq := holexprtodiffeq(arctan(z), y(z)):
Try[testabsprec, 50]("help intro 2",
    evaldiffeq(deq, y(z), 1/2, 50),
    0.46364760900080611621425623146121440202853705428612);

Try[verify, 'polynom(neighborhood(10^(-50)))']("help intro 3",
    analytic_continuation((z^2+1)*diff(y(z),z,z) + (3*z+1)*diff(y(z),z) 
        + z^2*y(z), y(z), [0, 1+I, 2], 50),
    .72678326528197350565935299733280205125629707790244*_C[0]
        + .43578845882065137070719121052451044561729148136090*_C[1]);

rat := (z^7+3*z^2+z+1)/((z-2)^3*(z^3-3)*(z-I)^2):
Try[testmajseries, z]("help intro 4",
    bound_ratpoly(rat, z),
    rat);

Try[testmajseries, z]("help intro 5",
    bound_diffeq(deq, y(z)),
    arctan(z));

