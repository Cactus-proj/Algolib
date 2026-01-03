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

#test gfun:-pade2 
#

# These are the tests from ?numapprox,hermite_pade
ARGS:=[sin(x),cos(x),exp(x)],x=0,[3,2,5]:
TestTools:-Try(1,gfun:-pade2(ARGS),numapprox[hermite_pade](ARGS));

ARGS:=[sin(x),cos(x)],x=Pi,7:
TestTools:-Try(2,gfun:-pade2(ARGS),numapprox[hermite_pade](ARGS));

ARGS:=[cos(2*x)*(x+1)+3,cos(x)^2+x*cos(x)+1,cos(2*x)+1,cos(x)],x=0,20:
TestTools:-Try(3,gfun:-pade2(ARGS),-numapprox[hermite_pade](ARGS));

#end test
                       