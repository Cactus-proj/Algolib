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

# FUNCTIONALITY:
#        compute the product of a list of holonomic functions
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
#        defines the product
#
# WEAKNESS:
#        all ideals have to be zero dimensional---this is not tested
#
# TYPES:
#        all term orders must be on the same algebra
#
# ALGORITHM:
#        reduce sufficiently many derivatives of the product to be
#        computed and find dependencies
#
dfinite_mul:=proc(
        descr_list::list(DFiniteDescr),TOrd::MonomialOrder)
    option `Copyright (c) 1995-2005 Frederic Chyzak, INRIA, France`;
    if nops(convert(map(proc(d)
            option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
            d[2]["algebra"]
        end,descr_list),'set'))<>1 then
        error "all term orders must be on the same algebra"
    end if;
    dfinite_multiply(map(expand,descr_list),TOrd)
end proc:
