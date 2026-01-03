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
#        compute the product of holonomic functions
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
# ALGORITHM:
#        + reduce sufficiently many derivatives of the product to be
#          computed and find dependencies
#        + the programme handles with lists of operators of the form
#          [u[1],...,u[n],v] with the meaning of the equation
#          u[1](f[1])+...+u[n](f[n])=v(s), where s=f[1]+...+f[n] (faux !)
#
dfinite_multiply:=proc(descr_list,TOrd)
    local term_number,args_table,i,j,k,subs_list,
        new_comm_params,old_comm_params,
        Alg,new_Alg,new_TOrd,new_sys,C,
        all_right_indets;
    option `Copyright (c) 1995-2009 Frederic Chyzak, INRIA, France`;
    global set,comm,polynom,`Ore_algebra/types_with_q`;
    term_number:=nops(descr_list);
    Alg:=TOrd["algebra"];
    old_comm_params:=op(Alg["comm_indets"]
        # Until I clarify why all left indeterminates are part of
        # Alg["comm_indets"].
        minus Alg["left_indets"]);
    C:=Alg["commutation"];
    all_right_indets:={seq(seq(j[i],j=C["right_indets"]),i=1..term_number)};
    # Derive a list of algebras from the algebra of the input.
    # (All term order structures are on the same algebra.)
    for i to term_number do
        subs_list:=[seq(j=j[i],j=Alg["right_indets"])];
        new_comm_params:=seq(`if`(i<>j,seq(Alg["right_indets"][k][j],
            k=1..nops(Alg["right_indets"])),NULL),j=1..term_number);
        # Build a new algebra.
        new_Alg[i]:=Ore_algebra:-OA_Internals:-skew_algebra(op(subs(subs_list,{
                seq(C["type_of_left",C["left_of_right",j]]=[
                        j,
                        C["left_of_right",j],
                        `if`(type(C["type_of_left",C["left_of_right",j]],
                            `Ore_algebra/types_with_q`),
                            C["q_of_right",j],
                            NULL)],
                    j=C["right_indets"])})),
            'comm'={old_comm_params,new_comm_params},
            'polynom'=all_right_indets);
        # Build a new term order on it.
        new_TOrd[i]:=Groebner:-MonomialOrder(new_Alg[i],
            'prod'(subs(subs_list,descr_list[i][2]["CreationArguments"][2]), 'tdeg'(new_comm_params)));
        # It would be tempting to add a third argument, [new_comm_params], to the previous call
        # to term_order.  However, this bugs down the code.  It could be a bug in Groebner:-Reduce.
        # Next, cast the input into the corresponding algebra.
        new_sys[i]:=subs(subs_list,descr_list[i][1]);
        args_table[i]:=new_sys[i],new_TOrd[i]
    end do;
    `Holonomy/fglm`("*",[ARGS_TABLE=args_table,
        TERM_NUMBER=term_number,
        TERM_ORDER = TOrd,
        POLY_INDET=TOrd["algebra"]["right_indets"],
        TENSOR_POLY_INDET=map(op,[all_right_indets,Alg["right_indets"]])],
        TOrd)
end proc:
