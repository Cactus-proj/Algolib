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

#### 
#### makeitfloat
####

Try("makeitfloat 1",
    NumGfun:-nthterm:-makeitfloat(1/3, 2),
    0.33);

Try("makeitfloat 2",
    NumGfun:-nthterm:-makeitfloat(2/3, 2),
    0.67);

Try("makeitfloat 3",
    NumGfun:-nthterm:-makeitfloat(-2/3, 2),
    -0.67);

Try("makeitfloat 4",
    NumGfun:-nthterm:-makeitfloat(1/3-2/3*I, 2),
    0.33-0.67*I);

Try("makeitfloat 5",
    NumGfun:-nthterm:-makeitfloat(-995/1000+1005/1000*I, 2),
    -1.00+1.01*I);

