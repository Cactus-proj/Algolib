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

# from evaldiffeq.mws

with(gfun:-NumGfun):

Try[testabsprec, 50]("evaldiffeq help 1",
    evaldiffeq({diff(y(z),z)-y(z), y(0)=1}, y(z), 1, 50),
    2.71828182845904523536028747135266249775724709369996);

Try[testabsprec, 10]("evaldiffeq help 2",
    analytic_continuation({(1+z^2)*diff(y(z),z,z)-(2*z+3)*y(z)+I*y(z),
        y(0)=Pi, D(y)(0)=-I}, y(z), [2]),
    37.6769250446-22.6773530382*I);

deq := gfun:-holexprtodiffeq(arctan(z),y(z)):

p := diffeqtoproc(deq, y(z)):
Try[verify, 'list(neighborhood(10^(-30)))']("evaldiffeq help 3",
    [ p([0, 1+I, 2*I], 30), p([0, -1+I, 2*I], 30) ],
    [1.570796326794896619231321691640+.549306144334054845697622618461*I,
    -1.570796326794896619231321691640+.549306144334054845697622618461*I]);

Try[verify, 'Matrix(neighborhood(10^(-20)))']("evaldiffeq help 4",
    transition_matrix(deq, y(z), [2*I, -1+I, 0, 1+I, 2*I], 20),
    Matrix(2, 2, {
        ( 1, 2 ) = -9.42477796076937971539,
        ( 2, 1 ) = 0.,
        ( 1, 1 ) = 1.00000000000000000000,
        ( 2, 2 ) = 1.00000000000000000000}));
