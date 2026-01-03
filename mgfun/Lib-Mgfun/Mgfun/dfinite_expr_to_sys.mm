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

# Tests the holonomy of an expression, rewrites it if it is necessary,
# returns the system it satisfies according to its variables.
dfinite_expr_to_sys:=proc(expr,
    f_of_typed_x::function(name::{identical(shift),
    identical(diff),identical(qshift),identical(qdilat),
    identical(non_root_of_one)}))
local operator_sys,TOrd,subs_set; 
option `Copyright (c) 1999-2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
global set;

# Reste a verifier que les "names" sont bien deux a deux distincts.
    operator_sys,TOrd,subs_set:=expression_to_system(
        expr,convert(f_of_typed_x,'set'));
    operator_sys:=convert(operator_sys,'set');
    subs(subs_set,map(Ore_algebra:-OA_Internals:-apply_operator,
        operator_sys,map2(op,1,f_of_typed_x),TOrd["algebra"]))

end proc: # dfinite_expr_to_sys
