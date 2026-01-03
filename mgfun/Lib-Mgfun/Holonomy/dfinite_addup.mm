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

# FUNCTIONALITY:
#        compute the sum of holonomic functions
#
# INPUT:
#        descr_list        a list of lists of the form [G,T], one for
#                        each function to be summed, where G is the
#                        Groebner basis with respect to T of the
#                        annihilating ideal of the corresponding
#                        function
#        TOrd                term order
#
# OUTPUT:
#        a Groebner basis with respect to TOrd for an ideal that
#        defines the sum
#
# WEAKNESS:
#        all ideals have to be zero dimensional---this is not tested
#
# ALGORITHM:
#        + reduce sufficiently many derivatives of the sum to be computed
#          and find dependencies
#        + the programme handles with lists of operators of the form
#          [u[1],...,u[n],v] with the meaning of the equation
#          u[1](f[1])+...+u[n](f[n])=v(s), where s=f[1]+...+f[n]
#
dfinite_addup:=proc(descr_list,TOrd)
    local term_number,args_table,i;
    option `Copyright (c) 1995-2009 Frederic Chyzak, INRIA, France`;
    term_number:=nops(descr_list);
    # Convert a list into a table.
    for i to term_number do
        args_table[i]:=op(op(i,descr_list))
    end do;
    `Holonomy/fglm`("+", [
        ARGS_TABLE = args_table, TERM_NUMBER = term_number,
        TERM_ORDER = TOrd, POLY_INDET = TOrd["algebra"]["right_indets"]
      ],
      TOrd)
end proc:
