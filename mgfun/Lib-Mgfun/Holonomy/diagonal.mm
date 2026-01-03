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

# Alg has to be a rational algebra.
diagonal:=proc(G,x_list,TOrd_rat,TOrd_poly,TOrd_elim)
    local Alg_rat,Alg_poly,Alg_loc,TOrd_loc,GB_rat,GB_poly,
        df,phi,loc_u,loc_p,i,x,remaining_x,K,C,minimal_dim;
    option `Copyright (c) 1994-2008 Frederic Chyzak, INRIA, France`;
    global diff,comm,takayama,ALGSUBS,list,
        loc_diff,lexdeg;
    remaining_x:=x_list[-1];
    Alg_rat:=TOrd_rat["algebra"];
    Alg_poly:=TOrd_poly["algebra"];
    userinfo(1,'diagonal',"perform algebraic substitution");
    GB_rat:=Groebner:-Basis(G,TOrd_rat);
    df:=dfinite_create(phi,GB_rat,TOrd_rat);
    df:=ALGSUBS(df,convert(Alg_rat["left_indets"],'list'),1,Alg_rat,{
            seq(x_list[i]=x_list[i]/x_list[i-1],i=2..nops(x_list)),
            # The following trivial equations have to be in the set.
            x_list[1]=x_list[1],
            seq(x=x,x=remove(member,Alg_rat["left_indets"],x_list))
        })/`*`(op(1..-2,x_list));
    GB_rat:=`Holonomy/fglm`("holonomic_polynomial_expression",
        [THE_FUNCTION=df,TERM_ORDER=TOrd_rat],TOrd_rat);
    GB_rat:=map(primpart,GB_rat,Alg_rat["non_comm_indets"]);
    userinfo(3,'diagonal',"operators found");
    userinfo(3,'diagonal',GB_rat);
    # Test holonomy.
    userinfo(1,'diagonal',"test holonomy");
    GB_poly:=Groebner:-Basis(GB_rat,TOrd_poly);
    minimal_dim:=nops(Alg_rat["left_indets"]);
    if Groebner:-HilbertDimension(GB_poly,TOrd_poly)
        <>minimal_dim
    then
        # This could probably be part of a general integration
        # routine.  However, keeping it here makes it possible to use
        # the heuristic below.
        userinfo(1,'diagonal',"perform extension/contraction");
        # Compute a polynomial to localize by.  This is the lcm of
        # leading terms of the basis of generators that has been
        # computed.
        loc_p:=lcm(op(map(lcoeff,GB_rat,Alg_rat["right_indets"])));
        # Heuristic:
        loc_p:=primpart(loc_p,remaining_x);
        userinfo(3,'diagonal',"localize by polynomial ",loc_p);
        # Create the corresponding local algebra.
        C:=Alg_poly["commutation"];
        K:=C["ground_ring"];
        K:=Ore_algebra:-OA_Internals:-ground_ring(K["characteristic"],
            K["all_indets"] union {loc_u},K["type_struct"]);
        C:=Ore_algebra:-OA_Internals:-commutation(K,
            C["left_indets"],C["right_indets"],
            {seq('loc_diff'=[C["right_of_left",x],x],x=C["left_indets"])},
            [loc_u,loc_p]);
        Alg_loc:=Ore_algebra:-OA_Internals:-algebra(C,
            K["all_indets"] minus {loc_u},{},{});
        TOrd_loc:=Groebner:-MonomialOrder(Alg_loc,'lexdeg'(
            sort([op(C["right_indets"])],TOrd_poly["order"]),
            [op(sort([op(C["left_indets"])],TOrd_poly["order"])),loc_u]));
        # Perform an extension/contraction.  Termination is ensured by
        # holonomy.
        GB_poly:=extension_contraction(
            map(op,{GB_rat,GB_poly}),TOrd_poly,TOrd_loc);
    end if;
    userinfo(3,'diagonal',"perform elimination and creative telescoping");
    takayama_algorithm(GB_poly,Alg_poly,
        subs(remaining_x=NULL,x_list),TOrd_elim,
        [[],[],0,subs({TERM_ORDER=TOrd_elim,DIM=nops(TOrd_elim["polynomial_indets"])},
            proc(elim_GB,GB,N)
                local dim;
                option `Copyright (c) 1997-2008 Frederic Chyzak, INRIA, France`;
                # The loop stops when the eliminated polynomials
                # ensure a sufficiently small dimension.
                dim:=Groebner:-HilbertDimension(elim_GB,TERM_ORDER);
                userinfo(4,'diagonal',cat("current dimension is ",dim));
                dim>DIM
            end)])[1]
end proc:
