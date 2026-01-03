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

anti_partial_of_sys:=proc(sys,r,opts,sum_or_int)
    local n,m,a,b,t,user_system,Alg_rat,Alg_elim,Alg_poly,Alg_loc,
        T_rat,T_elim,T_poly,T_loc,G,GB,to_be_elim,loc_u,loc_p,
        result,Takayama_algo,Kashiwara_algo,C,K,x;
    option `Copyright (c) 1996-2009 Frederic Chyzak, INRIA, France`;
    global list,tdeg,lexdeg,set,name,algebraic,identical,residues,
        _takayama_algo,_incautious,_natural_boundaries,DiffAlgebra,
        shift,qdilat,diff,Mgfun,LFSol,_f, _F ;
    typematch(r,n::'name'={a::'algebraic'..b::'algebraic',
        t::'identical'('residues')});
    userinfo(1,'Mgfun',"options ",opts," recognized");
    userinfo(1,'Mgfun',"converts the system into linear operators");
    user_system:=recognize_operator_algebra(sys);
    Alg_rat:=user_system["algebra"];
    # n is the index of summation or the variable of integration.  We
    # have to take into account any qx in the internal notation which
    # could denote a q^n.
# Ca devient faux dans le cas shift+qshift.
    to_be_elim:=`if`(has(Alg_rat["left_indets"],n),{n},{}) union
        map2(op,2,select(has,user_system["expr_to_qx"],n));
    if to_be_elim={} then
        error cat(`if`(sum_or_int="sum",
            "cannot sum with respect to variable ",
            "cannot integrate with respect to variable "),n)
    end if;
# Decide if creative_telescoping can be used.
m:=op(Alg_rat["left_indets"] minus {n});
if not member('_takayama_algo',opts)
    and to_be_elim={n}
    and nops(Alg_rat["left_indets"])=2
    and member(Alg_rat["type_of_left",n],{'shift','diff'})
    and member(Alg_rat["type_of_left",m],{'shift','diff'})
then
    userinfo(1,'Mgfun',
        "use Chyzak's extension of Zeilberger's fast algorithm");
# FC, September 2009: Lucien has changed the output format of
# creative_telescoping.
    result:=creative_telescoping('LFSol'(sys),
        m::Alg_rat["type_of_left",m],
        n::Alg_rat["type_of_left",n]);
# So I have to adjust accordingly:
    result := result[1] ; # Under the assumption of a single sum/integral.
    result := [eval(result[1], _F = unapply(_f(m,n), m)), result[2]] ;
# End of patch.
    # If the user assumes analytical conditions on the function to
    # be summed or integrated, we have finished.
    # (Here, _takayama_algo implies _natural_boundaries.)
    if member('_natural_boundaries',opts) or member('_takayama_algo',opts)
        or assigned(t)
    then
        {subs([n=NULL,_f=op(0,user_system["function"])],result[1])}
    # Otherwise, we have to compute the annihilator of the inhomogeneous
    # part.
    else
        error "non homogeneous case not implemented yet"
    end if
else
    # Our goal is to use our Takayama-like algorithm as often as
    # possible, but we have to justify it.
    Takayama_algo:=false;
    # We will need the system.
    G:=user_system["system"];
    # If the Takayama option is set, short-cut the verifications.
    if member('_takayama_algo',opts) then
        Takayama_algo:=true;
        GB:=G
    else
        # Ensure termination in the framework of Weyl algebras only.
        if type(Alg_rat,'DiffAlgebra') then
            # A priori, we will have to ensure holonomy.  When the
            # _incautious option is set however, we do not.
            Kashiwara_algo:=not member('_incautious',opts);
            # We accept to pay for the high cost of a test of holonomy
            # only in case we would run Kashiwara's algorithm.
            if Kashiwara_algo then
                # Perform the test.
                Alg_poly:=Ore_algebra:-OA_Internals:-polynomialize_algebra(
                    Ore_algebra:-OA_Internals:-rationalize_algebra(
                        Alg_rat,"fully"),Alg_rat["left_indets"]);
                T_poly:=Groebner:-MonomialOrder(Alg_poly,
                    'tdeg'(op(Alg_rat["non_comm_indets"])));
                # This should be refined: we need to create a
                # is_holonomic function which tries and uses already
                # known information (the dimension may be read on a
                # Groebner bases for another term order.
                userinfo(1,'Mgfun',"compute the dimension of the system");
                GB:=Groebner:-Basis(G,T_poly);
                if Groebner:-HilbertDimension(GB,T_poly)
                        =nops(Alg_rat["right_indets"]) then
                    # The system is already holonomic.
                    userinfo(2,'Mgfun',"the system is already holonomic");
                    Kashiwara_algo:=false
                else
                    # We have made as much as possible to avoid the
                    # following algorithm.
                    T_rat:=Groebner:-MonomialOrder(Alg_rat,
                        'tdeg'(op(Alg_rat["right_indets"])));
                    GB:=Groebner:-Basis(G,T_rat);
                    # This loc_p should be refined (can it be refined?)
                    loc_p:=lcm(op(map(
                        Groebner:-LeadingCoefficient,GB,T_rat)));
# This heuristic is not proved to terminate.
loc_p:=expand(loc_p/gcd(loc_p,op(to_be_elim)^degree(loc_p,op(to_be_elim))));
                    userinfo(1,'Mgfun',"perform extension/contraction");
                    userinfo(4,'Mgfun',"localize by polynomial ",loc_p);
                    # Create the corresponding local algebra.
                    C:=Alg_rat["commutation"];
                    K:=C["ground_ring"];
                    K:=Ore_algebra:-OA_Internals:-ground_ring(
                        K["characteristic"],
                        K["all_indets"] union {loc_u},K["type_struct"]);
                    C:=Ore_algebra:-OA_Internals:-commutation(K,
                        C["left_indets"],C["right_indets"],
                        {seq('loc_diff'=[C["right_of_left",x],x],
                            x=C["left_indets"])},
                        [loc_u,loc_p]);
                    Alg_loc:=Ore_algebra:-OA_Internals:-algebra(C,
                        K["all_indets"] minus {loc_u},{},{});
                    T_loc:=Groebner:-MonomialOrder(Alg_loc,
                        'lexdeg'(
                            sort([op(C["right_indets"])],T_poly["order"]),
                            [op(sort([op(C["left_indets"])],T_poly["order"])),
                                loc_u]));
                    # Perform the extension/contraction.  Can last a
                    # long time.
                    GB:=Holonomy:-HO_Internals:-extension_contraction(
                        expand(map(op,{GB,G})),T_poly,T_loc)
                end if
            else
                # In case of the _incautious option, we perform further
                # calculations starting from the input itself.
                userinfo(1,'Mgfun',
                    "incautiously perform no extension/contraction");
                userinfo(1,'Mgfun',"algorithm may fail to terminate");
                GB:=G
            end if;
            # In all previous cases, we may use our Takayama-like
            # algorithm if the _natural_boundaries option is set.
            Takayama_algo:=has(opts,'_natural_boundaries') or assigned(t)
        # In the case of general Ore algebras, we pray.
        else
            userinfo(1,'Mgfun',"cannot ensure holonomy");
            userinfo(1,'Mgfun',"algorithm may fail to terminate");
            GB:=user_system["system"]
        end if
    end if;
    # At this stage, either we have proved that GB is holonomic, or
    # the user wants us to take this for granted, or Alg_rat is not a
    # differential algebra and there is no notion of holonomy, but we
    # go on without ensuring termination.
    if Takayama_algo then
        # Takayama's algorithm is the fast algorithm.
        if nops(to_be_elim)>1 then
            # Temporary.
            error "multivariate Takayama's algorithm not implemented yet"
        end if;
        C:=Alg_rat["commutation"];
        K:=C["ground_ring"];
        K:=Ore_algebra:-OA_Internals:-ground_ring(K["characteristic"],
            K["all_indets"] union to_be_elim,K["type_struct"]);
        C:=Ore_algebra:-OA_Internals:-commutation(K,
            C["left_indets"] minus to_be_elim,
            C["right_indets"] minus {C["right_of_left",op(to_be_elim)]},
            {seq(C["type_of_left",x]=[C["right_of_left",x],x],
                x=C["left_indets"] minus to_be_elim)},"undefined");
        Alg_elim:=Ore_algebra:-OA_Internals:-algebra(C,
            (C["all_indets"] minus C["right_indets"]) minus to_be_elim,{},{});
        T_elim:=Groebner:-MonomialOrder(Alg_elim,
            'lexdeg'(op(map(convert,[to_be_elim,Alg_elim["right_indets"]],
                'list'))),convert(to_be_elim,'list'));
        if Alg_rat["type_of_left",op(to_be_elim)]='shift' then
            userinfo(1,'Mgfun',"use Takayama's algorithm for recurrences");
            result:=Holonomy:-HO_Internals:-holon_defsum(
                expand(map(op,{GB,G})),Alg_rat,[op(to_be_elim)],T_elim)
        elif Alg_rat["type_of_left",op(to_be_elim)]='qdilat' then
            userinfo(1,'Mgfun',"use Takayama's algorithm for q-recurrences");
            result:=Holonomy:-HO_Internals:-holon_defqsum(
                expand(map(op,{GB,G})),Alg_rat,[op(to_be_elim)],T_elim)
        elif Alg_rat["type_of_left",op(to_be_elim)]='diff' then
            userinfo(1,'Mgfun',
                "use Takayama's algorithm for differential equations");
            result:=Holonomy:-HO_Internals:-holon_defint(
                expand(map(op,{GB,G})),Alg_rat,[op(to_be_elim)],T_elim)
        else
            error `if`(sum_or_int="sum",
                "bad commutation type for a sum",
                "bad commutation type for an integration")
        end if
    else
        # Naive elimination by a Groebner basis calculation.
        T_elim:=Groebner:-MonomialOrder(
            Ore_algebra:-OA_Internals:-polynomialize_algebra(Alg_rat,to_be_elim),
            'lexdeg'(op(map(convert,[to_be_elim,Alg_rat["right_indets"]],
            'list'))));
        userinfo(1,'Mgfun',
            "perform naive elimination by Groebner basis calculation");
        GB:=Groebner:-Basis(GB,T_elim);
        result:=remove(has,GB,to_be_elim);
        # If the user assumes analytical conditions on the function to
        # be summed or integrated, we have finished.
        # (Here, _takayama_algo implies _natural_boundaries.)
        if member('_natural_boundaries',opts) or member('_takayama_algo',opts)
            or assigned(t)
        then
# Ceci est un probleme : est-ce =0 ou =1 en general ?
# reponse : c'est bien 1, mais il faut type-checker que n correspond a
# un operateur de decalage.
#
# Probleme si to_be_elim a plusieurs elements (shift+qshift).
            result:=subs(Alg_rat["right_of_left",op(to_be_elim)]=
                `if`(sum_or_int="sum",1,0),result);
            # Normalize the result.
            result:=Groebner:-Basis(result,T_elim)
        # Otherwise, we have to compute the annihilator of the
        # inhomogeneous part.
        else
            error "non homogeneous case not implemented yet"
        end if
    end if;
    # Finally return the result in the same form as the input.
    subs(user_system["qx_to_expr"],
        map(Ore_algebra:-OA_Internals:-apply_operator,convert(result,'set'),
        subs(n=NULL,user_system["function"]),Alg_rat))
end if
end proc: # anti_partial_of_sys
