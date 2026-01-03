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

# rewritten BS. 14/11/03
FUNCTIONTABLE[euler]:=proc(expr,scale,var,ord)
local n,z;
    n:=expr[1]; z:=expr[2];
    RUN(2^(n+1)* ('Zeta'(0,-n, z/2) - 'Zeta'(0,-n, (z + 1)/2)),args[2..-1])
end:
