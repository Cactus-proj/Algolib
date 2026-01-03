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

add_prod_of_sys:=proc()
    local R,T,GBs,GBp,i;
    option `Copyright (c) 1996-2008 Frederic Chyzak, INRIA, France`;
    global tdeg,set,diff,
        distributed;
    R:=recognize_operator_algebra([args[1..-2]]);
    T:=Groebner:-MonomialOrder(R["algebra"],
        'tdeg'(op(R["algebra"]["right_indets"])));
    GBs:=map(Groebner:-Basis,R["system"],T);
    GBp:=`if`(args[-1]="+",
        Holonomy:-HO_Internals:-dfinite_add,Holonomy:-HO_Internals:-dfinite_mul)(
            [seq([i,T],i=GBs)],T);
    collect(map(Ore_algebra:-OA_Internals:-apply_operator,
        convert(GBp,'set'),R["function"],R["algebra"]),
        {'diff',op(0,R["function"])},'distributed')
end proc:
