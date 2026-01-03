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

# Bug fixed by MM. May 2012.

p := gfun:-rectoproc((n-1)*u(n), u(n)):
TestTools:-Try(1,[seq(p(i),i=0..5)],[0, _C[0], 0, 0, 0, 0]);

p := gfun:-rectoproc(u(n), u(n)):
TestTools:-Try(1,[seq(p(i),i=0..5)],[0, 0, 0, 0, 0, 0]);

# Bug in NumGfun affecting rectoproc. Fixed by MM. Jul 2012.

unassign(uu);
p := gfun:-rectoproc({n*uu[0](n)+uu[0](n+2), uu[0](0)=1, uu[0](1)=0},uu[0](n));
TestTools:-Try(1,assigned(uu),false);

#end test

