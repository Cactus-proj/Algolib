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
#        user entry point to compute the holonomic
#        extension/contraction of a D-finite ideal in a differential
#        algebra when localizing the base ring by a specified
#        polynomial
#
# INPUT:
#        G        a list of polynomials that generate a D-finite ideal
#        p        the polynomial to localize with
#        Alg        a differential algebra
#
# OUTPUT:
#        a list of polynomials that generate the extension/contraction
#        ideal
#
# WEAKNESS:
#        the algorithm runs until it finds a holonomic ideal, so it may
#        not terminate either when G is not D-finite or p is not the
#        lcm of leading terms of the elements of G
#
#        besides, it implements a heuristical method only
#
# TYPE:
#        Alg has to be a DiffAlgebra
#
# ALGORITHM:
#        localization by Groebner basis calculation in a term order
#        compatible with the (local) commutations followed by a change
#        of ordering by an extension of the FGLM algorithm to the
#        non-zero dimensional case
#
holon_closure:=proc(G::{list,set}(polynom),TOrd::MonomialOrder,
        TOrd_loc::MonomialOrder)
    local Alg,Alg_loc;
    option `Copyright (c) 1996-2005 Frederic Chyzak, INRIA, France`;
    Alg:=TOrd["algebra"];
    Alg_loc:=TOrd_loc["algebra"];
##### Tester que Alg est bien une localisation de Alg_loc.
    # extension_contraction requires a polynomial algebra.
    # Here, I should check that G is a D-finite system.
    extension_contraction(args)
end proc:
