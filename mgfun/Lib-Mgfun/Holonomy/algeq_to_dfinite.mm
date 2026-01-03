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
#        user entry point to compute equations that define an algebraic
#        function as holonomic
#
# INPUT:
#        P        polynomial in x_1,...,x_n,f, that defines an algebraic
#                function f(x_1,...,x_n) by f(x_1,...,x_n,f)=0
#        f        name of the function as it appears in P
#        TOrd        term order on a differential algebra built on the
#                differential operators with respect to which equations
#                have to be returned
#
# OUTPUT:
#        a set of operators that vanish on any solution f of P and form
#        a rectangular system with respect to TOrd["algebra"]
#
# WEAKNESS:
#        so far, only return a rectangular system
#
# TYPES:
#        + the algebra on which the term order is built must be a
#          differential algebra
#        + degree of P in f must be positive
#        + P and its derivative with respect to f must be coprime
#
algeq_to_dfinite:=proc(P::polynom,f::name,TOrd::MonomialOrder)
    local degP,DP,A,B;
    option `Copyright (c) 1994-2005 Frederic Chyzak, INRIA, France`;
    # Check that the algebra is a DiffAlgebra.
    if not type(TOrd["algebra"],'DiffAlgebra') then
        ERROR(cat(TOrd," must be built on a differential algebra"))
    end if;
    # Check that P has non-zero degree.
    degP:=degree(P,f);
    if degP=0 then
        ERROR(cat(P," does not define any algebraic function"))
    end if;
    # Check that the derivative of P w.r.t. f can be inverted.
    DP:=diff(P,f);
    if gcdex(P,DP,f,'A','B')<>1 then
        ERROR(cat("gcd(",P,"diff(",P,",",f,")) is not 1"))
    end if;
    algebraic_to_dfinite(P,f,TOrd)
end proc:
