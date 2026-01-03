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

# libext.tst: tests for library extension mechanism in MultiSeries

#test
   with(MultiSeries):

   TRY(1,[op(series(mysin(x),x))],
         [mysin(0), 0, D(mysin)(0), 1, 1/2*`@@`(D,2)(mysin)(0), 2, 1
	 /6*`@@`(D,3)(mysin)(0), 3, 1/24*`@@`(D,4)(mysin)(0), 4, 1/
	 120*`@@`(D,5)(mysin)(0), 5, O(1), 6]);
   forget(series);
   AddFunction(mysin,eval(GetFunction(sin)));
   TRY(2, [op(series(mysin(x),x))],
          [1, 1, -1/6, 3, 1/120, 5, O(1), 7]);
   forget(series);
   TRY(3,FunctionSupported(sin),true);
   RemoveFunction(sin);
   TRY(4,FunctionSupported(sin),false);

   AddFunction(sin,eval(GetFunction(mysin)));
   RemoveFunction(mysin);
   TRY(5,FunctionSupported(sin),true);
   TRY(6,FunctionSupported(mysin),false);
   TRY(7, [op(series(sin(x),x))],
          [1, 1, -1/6, 3, 1/120, 5, O(1), 7]);
   TRY(8,[op(series(mysin(x),x))],
         [mysin(0), 0, D(mysin)(0), 1, 1/2*`@@`(D,2)(mysin)(0), 2, 1
	 /6*`@@`(D,3)(mysin)(0), 3, 1/24*`@@`(D,4)(mysin)(0), 4, 1/
	 120*`@@`(D,5)(mysin)(0), 5, O(1), 6]);

#end test
