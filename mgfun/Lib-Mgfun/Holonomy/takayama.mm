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
#        user entry to Takayama's algorithm for the elimination
#        indeterminates in the sum of a left ideal and a right ideal of
#        an Ore algebra
#
# INPUT:
#        poly_list        list of generators of the left ideal
#        indet_list        list of indeterminates to be eliminated
#        TOrd                term order with respect to which the procedure
#                        returns a Groebner basis
#        init                a list of the form [elim_GB,GB,N,stop_proc],
#                        where:
#                                + elim_GB is a list of polynomials
#                                  in A;
#                                + GB is a list of polynomials
#                                  in A[Dx,x] that includes elim_GB;
#                                + N is the index of beginning of the
#                                  loop;
#                                + stop_proc(elim_GB,GB,N) is either a
#                                  procedure that returns true as long
#                                  a next iteration of the loop is
#                                  needed.
#
# OUTPUT:
#        a list [elim_GB,GB,N] where elim_GB is a Groebner basis of the
#        ideal sum(Dx[i].Big_Alg + sum(Big_Alg.g[i]) inter Small_Alg,
#        where Big_Alg is the algebra over which TOrd is built, and
#        Small_Alg is the corresponding algebra whithout the
#        indeterminates in indet_list and their pseudo-derivation.
#
# WEAKNESS:
#        there is no way to ensure the termination of the algorithm
#
# ALGORITHM:
#        compute a sequence of Groebner bases for modules, using
#        Takayama's algorithm
#
# NOTE:
#        a standard value for init
#          is [[],[],0,proc(elim_GB,GB,N) elim_GB=[] end].
#
# REFERENCE:
#        ``Groebner basis, integration and transcendental functions'',
#        by N. Takayama, Symbolic and algebraic computation, ACM and
#        Addison-Wesley (1990), 152--156 (Proceedings of ISSAC'90,
#        Kyoto)
#
#        ``An algorithm of constructing the integral of a module --- an
#        infinite dimensional analog of Groebner basis'', by
#        N. Takayama, Symbolic and algebraic computation, ACM and
#        Addison-Wesley (1990), 152--156 (Proceedings of ISSAC'90,
#        Kyoto)
#
`Holonomy/takayama`:=proc(poly_list::{list,set}(polynom),
        Alg::OreAlgebra,x_list::{list,set}(name),TOrd::MonomialOrder,
        init::[list,list,integer,{name,procedure}])
    option `Copyright (c) 1996-2005 Frederic Chyzak, INRIA, France`;
    takayama_algorithm(
        # Conversion for the case of a set of input polynomials.
        convert(expand(poly_list),'list'),
        Alg,
        # Conversion for the case of a set of input variables.
        convert(x_list,'list'),
        TOrd,init)
end proc:
