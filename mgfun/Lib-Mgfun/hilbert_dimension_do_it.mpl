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
#        compute the Hilbert dimension of a monoideal
#
# INPUT:
#        S        set of generators of a monoideal
#        X        list of generators of the ambient monoid
#
# OUTPUT:
#        an integer, the Hilbert dimension of the monoideal
#
# ASSUMPTION:
#        when computing the Hilbert dimension of an ideal, the set S
#        must be the set of leading terms of a Groebner basis for the
#        ideal
#
# ALGORITHM:
#        algorithm DIMENSION, p. 449, from the following reference
#
# REFERENCE:
#        ``Groebner bases'', by Thomas Becker and Volker Weispfenning,
#        no. 141 in Graduate texts in mathematics, Springer Verlag
#        (1993), A computational approach to commutative algebra (in
#        cooperation with Heinz Kredel)
#
`Holonomy/hilbert_dimension_rec`:=proc(S,k,U,M,X)
    local i,N,V;
    option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
    N:=M;
    for i from k to nops(X) do
        V:=U union {X[i]};
        if select(type,S,polynom(integer,V))={} then
            N:=`Holonomy/hilbert_dimension_rec`(S,i+1,V,N,X)
        end if
    end do;
    if not member({},{seq(U minus i,i=N)}) then
        N:=N union {U}
    end if;
    N
end proc:

`Holonomy/hilbert_dimension_do_it`:=proc(S,X)
    option `Copyright (c) 1996-2008 Frederic Chyzak, INRIA, France`;
    max(op(map(nops,`Holonomy/hilbert_dimension_rec`(S,1,{},{{}},X))))
end proc:
