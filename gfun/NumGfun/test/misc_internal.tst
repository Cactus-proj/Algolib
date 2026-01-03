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


kernelopts('opaquemodules'=false);

Try("diffeq_for_derivative arctan",
    gfun:-NumGfun:-diffeq_for_derivative({(1+z^2)*diff(y(z),z)-1, y(0) = 0}, y(z)),
    {2*z*y(z)+(1+z^2)*diff(y(z),z), y(0) = 1});

Try("diffeq_for_derivative erf",
    gfun:-NumGfun:-diffeq_for_derivative(
        gfun:-NumGfun:-diffeq_for_derivative(
            {2*z*diff(y(z),z)+diff(y(z),`$`(z,2)), y(0) = 0,
                D(y)(0) = 2/Pi^(1/2)},
            y(z)),
        y(z)),
    {2*diff(y(z),z)*z+4*y(z)+diff(y(z),`$`(z,2)),
        y(0) = 0, D(y)(0) = -4/Pi^(1/2)});
