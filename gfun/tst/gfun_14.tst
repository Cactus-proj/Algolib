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

#Fix a problem with Laplace being both a local and a global, which made
#listtoseries/listtolist/... fail depending on whether gfun had been
#loaded using "with" or not.

#test

TestTools:-Try(1,gfun[listtolist]([1,2,3],Laplace),[1,2,6]);
TestTools:-Try(2,gfun[listtolist]([1,2,3],gfun:-Laplace),[1,2,6]);
with(gfun):
TestTools:-Try(3,gfun[listtolist]([1,2,3],Laplace),[1,2,6]);
TestTools:-Try(4,listtolist([1,2,3],Laplace),[1,2,6]);
TestTools:-Try(5,listtolist([1,2,3],gfun:-Laplace),[1,2,6]);

#end test
