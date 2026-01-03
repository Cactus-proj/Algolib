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
#        return skew polynomials (of first order) that define a
#        hypergeometric function
#
# INPUT:
#        H        expression (value of the function)
#        Alg        Ore algebra
#
# OUTPUT:
#        set of skew polynomials of first order
#
# WEAKNESS:
#        only for rational fractions involving binomial coefficients
#
# ASSUMPTION:
#        H represents a hypergeometric function
#
hypergeometric_to_dfinite:=proc(H,Alg)
    local Dx,eq,rat,holon_eq,post_subs;
    option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
    global anything,symbolic,expanded,
        # List of combinatorial numbers whose shifts require to be
        # expanded.
        binomial,qbinomial,qfactorial,qpochhammer;
    post_subs:={};
    for Dx in Alg["right_indets"] do
        # This normal@simplify@expand@expand is to have q-calculus
        # working.
        holon_eq[Dx]:=primpart(Dx-
            normal(simplify(expand(expand(
                Ore_algebra:-OA_Internals:-apply_operator(Dx,H,Alg)/H)),
                # This option symbolic appears to be crucial in 5.4,
                # but maybe unnecessary in 5.5.
                'symbolic'),'expanded'),Dx);
        if assigned(Alg["qdilat_auxiliary_info",Dx]) then
            post_subs:=post_subs union
                {Alg["q_of_right",Dx]^Alg["qdilat_auxiliary_info",Dx]
                    =Alg["left_of_right",Dx]};
        end if
    end do;
    subs(post_subs,{seq(collect(holon_eq[Dx],Dx),Dx=Alg["right_indets"])})
end proc:
