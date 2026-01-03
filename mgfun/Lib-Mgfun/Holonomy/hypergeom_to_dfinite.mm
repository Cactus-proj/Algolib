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
#        user entry point to compute equations that define an
#        hypergeometric function as holonomic
#
# INPUT:
#        H        expression (value of the function)
#        Alg        Ore algebra
#
# OUTPUT:
#        list of skew polynomials of first order
#
# WEAKNESS:
#        only for rational fractions involving binomial coefficients
#
# TYPES:
#        H has to be a hypergeometric function
#
hypergeom_to_dfinite:=proc(H::algebraic,
        Alg::OreAlgebra)
    option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
    hypergeometric_to_dfinite(H,Alg)
end proc:
