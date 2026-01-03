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
#        create frequently used term orders corresponding to a given
#        algebra
#
# INPUT:
#        Alg        an Ore algebra
#        TO_type        an expression select a term order on the algebra
#
# OUTPUT:
#        a term order structure
#
# ASSUMPTION:
#        TO_type must be either strings "Dfinite" or "holonomic", or of
#        the form 'takayama'=<list of names> to denote a term order
#        to eliminate the indeterminates in <list of names> via
#        Takayama's algorithm; in the "holonomic" case, Alg has to be a
#        polynomial algebra
#
usual_term_orders:=proc(Alg,TO_type)
    local new_Alg;
    option `Copyright (c) 1996-2008 Frederic Chyzak, INRIA, France`;
    global takayama,tdeg,lexdeg,list,set,name;
    if TO_type="Dfinite" then
        new_Alg:=Ore_algebra:-OA_Internals:-rationalize_algebra(Alg,"fully");
        if not assigned(new_Alg["usual_term_orders","Dfinite"]) then
            new_Alg["usual_term_orders","Dfinite"]:=
                Groebner:-MonomialOrder(new_Alg,
                    'tdeg'(op(new_Alg["right_indets"])))
        end if
    elif TO_type="holonomic" then
        # The algebra Alg is assumed to be fully polynomial.
        new_Alg:=Alg;
        if not assigned(new_Alg["usual_term_orders","holonomic"]) then
            new_Alg["usual_term_orders","holonomic"]:=
                Groebner:-MonomialOrder(new_Alg,
                    'tdeg'(op(new_Alg["right_indets"]),
                        op(new_Alg["left_indets"])))
        end if
    elif nops(TO_type)=2 and op(1,TO_type)='takayama'
            and type(op(2,TO_type),'list'('name')) then
        new_Alg:=Ore_algebra:-OA_Internals:-rationalize_algebra(Alg,"fully");
        new_Alg:=Ore_algebra:-OA_Internals:-polynomialize_algebra(new_Alg,
            convert(op(2,TO_type),'set'));
        if not assigned(new_Alg["usual_term_orders",TO_type]) then
            new_Alg["usual_term_orders",TO_type]:=
                Groebner:-MonomialOrder(new_Alg,
                    'lexdeg'(op(2,TO_type),
                    convert(new_Alg["right_indets"],'list')),
                    # The third argument denotes indeterminates which
                    # will not be used in multiplications.
                    op(2,TO_type))
        end if
    end if;
    new_Alg["usual_term_orders",TO_type]
end proc:
