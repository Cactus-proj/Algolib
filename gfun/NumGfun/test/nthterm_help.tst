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
$include <testutils.mm>

# from nth_term.mws

with(gfun): with(NumGfun):

Try("nth_term help 1",
    nth_term({(n^2+1)*u(n+2)+u(n+1)=(2*n+2*I+5)*u(n), u(0)=1, u(1)=0},u(n),20),
    569008109867936731210287799/8922539920387740732999062500000
    +860326427452337891464669037/4461269960193870366499531250000*I);

Try("nth_term help 2",
    nth_term(u(n+2)=u(n)+u(n+1), u(n), 50),
    7778742049 * u(0) + 12586269025 * u(1));

Try("nth_term help 3",
    nth_term(u(n+1)=u(n)/(n+1), u(n), 40, 'series'),
    (2772359610018469067133291773316444867218087189
        /1019894104059872167932014086995144867840000000)*u(0));

Try[testabsprec, 30]("nth_term help 4",
    fnth_term(u(n+1)=u(n)/(n+1), u(n), 30, 30, 'series'),
    2.718281828459045235360287471353 * u(0));

