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

my_normal:=proc(expr)
    local raw,acc,i;
    option `Copyright (c) 2000-2002 Frederic Chyzak, INRIA, France`;
    raw:=`if`(type(expr,`+`),[op(expr)],[expr]);
    acc:=0;
    for i to nops(raw) do
        acc:=normal(acc+raw[i])
    end do;
    acc
end:
