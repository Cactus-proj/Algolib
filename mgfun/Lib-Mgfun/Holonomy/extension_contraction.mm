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
#        compute the (holonomic) extension/contraction of a D-finite
#        ideal in a Weyl algebra when localizing the base ring by a
#        polynomial
#
# INPUT:
#        G        a list of polynomials that generate a D-finite ideal
#        p        the polynomial to localize with
#        Alg        a Weyl algebra
#
# OUTPUT:
#        a list of polynomials that generate the extension/contraction
#        ideal and that is a Groebner basis
#
# WEAKNESS:
#        the algorithm runs until it finds a holonomic ideal, so it may
#        not terminate either when G is not D-finite or p is not the
#        lcm of leading terms of the elements of G
#
# ASSUMPTION:
#        Alg has to be a (polynomial) Weyl algebra
#
# ALGORITHM:
#        localization by Groebner basis calculation in a term order
#        compatible with the (local) commutations followed by a change
#        of ordering by an extension of the FGLM algorithm to the
#        non-zero dimensional case
#
extension_contraction:=proc(G,TOrd,TOrd_loc)
    local Alg,Alg_loc,u,p,GB_loc,GB;
    option `Copyright (c) 1996-2008 Frederic Chyzak, INRIA, France`;
    # Alg is supposed to be a polynomial algebra.
    Alg:=TOrd["algebra"];
    Alg_loc:=TOrd_loc["algebra"];
    # u is the inverse of p.
    u:=Alg_loc["localization"][1];
    p:=Alg_loc["localization"][2];
    GB_loc:=Groebner:-Basis({op(G),p*u-1},TOrd_loc);
    GB:=`Holonomy/fglm`("extension_contraction",
        [GROEBNER_BASIS=GB_loc,TERM_ORDER=TOrd_loc,
        POLY_INDET=[op(Alg["right_indets"]),
            op(Alg["left_indets"]),u]],TOrd);
    ASSERT(GB=Groebner:-Basis(GB,TOrd),
        "bug in extension/contraction algorithm");
    GB
end proc:
