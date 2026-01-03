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

diag_of_sys:=proc(sys,x,y)
    local user_system,GB,loc_p,n,Alg_rat,Alg_poly,Alg_elim,C,K,
        TOrd_rat,TOrd_poly,TOrd_elim,new_sys;
    option `Copyright (c) 1997-2008 Frederic Chyzak, INRIA, France`;
    global system,tdeg,wdeg,lexdeg,right_indets;
    user_system:=recognize_operator_algebra(sys);
    Alg_rat:=user_system["algebra"];
    TOrd_rat:=Groebner:-MonomialOrder(Alg_rat,
        'tdeg'(op(Alg_rat["right_indets"])));
    GB:=Groebner:-Basis(user_system["system"],TOrd_rat);
    loc_p:=lcm(op(map(
        Groebner:-LeadingCoefficient,GB,TOrd_rat)));
    # This is a heuristic.
    loc_p:=expand(loc_p/gcd(loc_p,y^degree(loc_p,y)));
    Alg_poly:=Ore_algebra:-OA_Internals:-polynomialize_algebra(
        Ore_algebra:-OA_Internals:-rationalize_algebra(
            Alg_rat,"fully"),Alg_rat["left_indets"]);
    n:=nops(Alg_rat["left_indets"]);
    TOrd_poly:=Groebner:-MonomialOrder(Alg_poly,
        'wdeg'([degree(loc_p,Alg_rat["left_indets"])$n,1$n],
        map(op,[Alg_rat["right_indets"],Alg_rat["left_indets"]])));
    C:=Alg_rat["commutation"];
    K:=C["ground_ring"];
    K:=Ore_algebra:-OA_Internals:-ground_ring(K["characteristic"],
        K["all_indets"] union {y},K["type_struct"]);
    C:=Ore_algebra:-OA_Internals:-commutation(K,
        C["left_indets"] minus {y},
        C["right_indets"] minus {Alg_rat["right_of_left",y]},
        remove(has,C["type_struct"],y),"undefined");
    Alg_elim:=Ore_algebra:-OA_Internals:-algebra(C,
        Alg_rat["rational_indets"] minus {y},{},{});
    TOrd_elim:=Groebner:-MonomialOrder(Alg_elim,
        'lexdeg'([y],[op(Alg_elim["right_indets"])]),[y]);
    new_sys:=Holonomy:-HO_Internals:-holon_diagonal(user_system["system"],
        [x,y],x,TOrd_rat,TOrd_poly,TOrd_elim);
    map(Ore_algebra:-OA_Internals:-apply_operator,
        new_sys,subs(y=NULL,user_system["function"]),Alg_rat)
end proc:
