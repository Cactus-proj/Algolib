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

$include <testutils.mm>

# TODO: put this back once we handle regular singular points a bit better

#functions := [
#    arccsc, arccos, arccot, arccsch, AiryAi, arcsec, arcsin, arctan, AiryBi, 
#    #Chi,  # no series solution
#    #Ci,  # no series solution
#    cos,
#    #Ei,  # no series solution
#    erf, erfc, erfi, arccosh, arccoth, arcsech, arcsinh, arctanh, cosh, sinh,
#    #dilog,  # no series solution
#    Shi, Si, sin
#    ];
#
#for f in functions do
#    deq := holexprtodiffeq(f(z), y(z));
#    maj_series := bound_diffeq(deq, y(z));
#    tail_bound := bound_diffeq_tail(deq, y(z), N);
#    print(f(z), 'maj_series' = maj_series, 'tail_bound' = tail_bound);
#    try
#        taylor(f(z), z=0);
#        delta := evalf(taylorcoeffs(maj_series, z) - map(abs, taylorcoeffs(f(z), z))):
#        ASSERT(type(delta,'list'('nonnegative'))):
#    catch:
#        print (f(z), `Not analytic`(f(z) = MultiSeries:-series(f(z), z=0)));
#    end try;
#end do:
