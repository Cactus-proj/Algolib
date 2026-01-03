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

# test125.tst: test for MultiSeries[multiseries] (no. 125)

   #=============== test nb 125 =====================
   s1:=MultiSeries:-asympt(exp(a*x),x,3) assuming a>0;
   s2:=MultiSeries:-asympt(exp(a*x),x,3) assuming a<0;
   # this checks a fix for a bug that existed before sep. 2010
   TRY("125",s2,1/exp(-a*x));

#end test
