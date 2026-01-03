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


Try("dominant_root 1",
    NumGfun:-dominant_root(z+1, z),
    [-1, 1]);

Try("dominant_root 2",
    NumGfun:-dominant_root(z^2+2*z-2*I*z-2*I, z),
    [I-1, 2]);

Try("dominant_root 3",
    NumGfun:-dominant_root((-2+z)*(2+z)^2, z),
    [-2, 2]);

# roots of close absolute value belonging to different irreducible factors
Try("dominant_root 4",
    NumGfun:-dominant_root(expand((z+1)^4*(z-1+1/10^20)), z),
    [1-1/10^20, 1]);

# several dominant roots belonging to different irreducible factors
Try[member]("dominant_root 5",
    NumGfun:-dominant_root(z^2-1, z),
    [ [1, 1], [-1,1] ]);

# Mignotte polynomial (irreducible polynomial with close roots)
Try[verify, 'neighborhood(1e-20)']("dominant_root 6 (Mignotte)",
    NumGfun:-dominant_root(X^7-2*(1000*X-1)^2, X),
    [RootOf(_Z^7-2000000*_Z^2+4000*_Z-2,.99999999997763932023e-3), 1]);

# Mignotte-like irreducible polynomial with near-opposite roots
Try[verify, 'neighborhood(1e-20)']("dominant_root 7 (opp-Mignotte)",
    NumGfun:-dominant_root(X^7-2*(1000*X^2-1), X),
    [RootOf(_Z^7-2000*_Z^2+2,-.31622776601433793320e-1), 1]);

# similar example with non-real roots
Try[verify, 'neighborhood(1e-20)']("dominant_root 8 (opp-Mignotte)",
    NumGfun:-dominant_root(X^7-2*(1000*X^2-I+1), X),
    [RootOf(_Z^7-2000*_Z^2-2+2*I,-.14391204993750742880e-1
            -.34743442275511562818e-1*I), 1]);

Try("dominant_root 9",
    NumGfun:-dominant_root((z^8-1)*(z-I), z),
    [I, 2]);

Try[testnoerror]("dominant_root 10",
    NumGfun:-dominant_root(z^2+z+1, z));

Try[testnoerror]("dominant_root 11",
    NumGfun:-dominant_root((-6311369*z^2-9420000+131250*I)*(2*z^2+1)^2
            *(-6311369*z^2-3140000+43750*I)^2,z));

# this used to loop forever beacause of an abs() missing in irreducible/check
Try[testnoerror]("dominant_root 12",
    NumGfun:-dominant_root(z^3-2*z+5, z));


