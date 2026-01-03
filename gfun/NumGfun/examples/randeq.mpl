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

with(gfun:-NumGfun);
kernelopts(opaquemodules=false);


kernelopts(printbytes=false);
deq := { (z+1)*(3*z^2-z+2)*diff(y(z),z$3) + (5*z^3+4*z^2+2*z+4)*diff(y(z),z$2)
    + (z+1)*diff(y(z),z) + (4*z^3+2*z^2+5)*y(z), y(0)=0, D(y)(0)=I, D(D(y))(0)=0 };
sing := [fsolve((z+1)*(3*z^2-z+2), z, 'complex')];
f := diffeqtoproc(deq, y(z));
f(1/2,100);
f([0,-2/5+3/5*I, -2/5+I, -1/5+7/5*I],10);
f([0,-2/5+3/5*I],200);
#f([0,-2/5+3/5*I, -2/5+I, -1/5+7/5*I],400);
transition_matrix(deq, y(z), [0,-2/5+3/5*I, -2/5+4/5*I, -2/5+I, -1/5+6/5*I], 20);

