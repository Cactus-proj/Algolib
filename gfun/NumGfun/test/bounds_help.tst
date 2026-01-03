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


with(gfun:-NumGfun):

# from bound_ratpoly.mws

Try[testmajseries, z]("bound_ratpoly help 1",
    bound_ratpoly(1/(z+5),z),
    1/(z+5));

Try[testmajseries, z]("bound_ratpoly help 2",
    bound_ratpoly((5*z^6+z+2)/((z^3+1)^2*(z+I)),z),
    (5*z^6+z+2)/((z^3+1)^2*(z+I)));

# from bound_diffeq.mws

Try[testmajseries, z]("bound_diffeq help 1",
    bound_diffeq({diff(y(z),z)=y(z), y(0)=1}, y(z)),
    exp(z));

Try[testmajseries, z]("bound_diffeq help 2",
    bound_diffeq({(1+z)*diff(y(z),z)=y(z), y(0)=1}, y(z)),
    1+z);

# series(sum(a_k*z^k)) does not work
#Try[testmajseries, z, prec=15]("bound_diffeq help 3",
#    bound_diffeq({diff(y(z),z,z)=z*y(z), y(0)=1, D(y)(0)=1}, y(z)),
#    .999999999999995+.999999999999997*z+.166666666666666*z^3
#    +.833333333333329e-1*z^4+.555555555555553e-2*z^6+.198412698412697e-2*z^7
#    +.771604938271598e-4*z^9+.220458553791886e-4*z^10+.584549195660305e-6*z^12
#    +.141319585764030e-6*z^13+.278356759838240e-8*z^15);
Try[testnoerror]("bound_diffeq help 3",
    bound_diffeq({diff(y(z),z,z)=z*y(z), y(0)=1, D(y)(0)=1}, y(z)));

# from bound_diffeq_tail.mws
#
# FIXME: find a way to actually test the output!

Try[testnoerror]("bound_diffeq_tail help 1",
    bound_diffeq_tail({diff(y(z),z)=y(z), y(0)=1}, y(z), n));

Try[testnoerror]("bound_diffeq_tail help 2",
    bound_diffeq_tail({(1+z)*diff(y(z),z)=y(z), y(0)=1}, y(z), n));

# from bound_rec.mws

rec := {I*u(n+1) = (n+1)*u(n), u(0)=3}:
Try[testrecbound, u, n]("bound_rec help 1",
    bound_rec(rec, u(n)),
    rec);

rec := {(2*n+2)^2*u(n+1) = -(n+1)*u(n), u(0)=1}:
Try[testrecbound, u, n]("bound_rec help 2",
    bound_rec(rec, u(n)),
    rec);

rec := {15*u(n)-4*u(n+1)-13*u(n+2)+5*u(n+3), u(0) = 17/5, u(1) = 3, u(2) = 3/12,
    u(3) = 0, u(4) = 5, u(5) = 0};
Try[testrecbound, u, n]("bound_rec help 3",
    bound_rec(rec, u(n)),
    rec);

Try[testnoerror]("bound_rec help 4",
    bound_rec_tail({I*(n+1)*u(n+1) = u(n), u(0)=1}, u(n)));
