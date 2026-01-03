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

pol_to_sys:=proc(eq,hol_sys,single_eq::specfunc(name,univariate))
    local f_of_z,hol_expr,hol_expr_D,hol_sys_D,indF,namF,
        der_list,lin_sys,df,i,j,f,z,lin_subs,Alg,TOrd,non_lin_expr,
        answer,Dz;
    option `Copyright (c) 1997-2008 Frederic Chyzak, INRIA, France`;
    global function,D,name,list,tdeg,lexdeg,APPLYOPR,set;
    f_of_z:=op(1,eq);
    hol_expr:=op(2,eq);
    # Analyse f_of_z.
    #
    # WEAKNESS: more type-checking is needed!
    f:=op(0,f_of_z);
    z:=op(f_of_z);
    # Normalize input expressions.
    hol_expr_D:=convert(hol_expr,'D');
    hol_sys_D:=convert(hol_sys,'D');
    # Detect all functions.
    indF:=indets({hol_expr_D,hol_sys_D},'function') minus {'D'};
    # List all function names.
    namF:=map2(op,0,indF);
    namF:=map(op,select(has,namF,'D')) union remove(has,namF,'D');
    # For each function, list all its pseudo-derivatives.
    for i to nops(namF) do
        der_list[i]:=convert(select(has,indF,namF[i]),'list');
    end do;
    # Work with a single function name, f, to recognize the operator
    # algebra.
    #
    # WEAKNESS: it follows that all f's need to have the same
    # signature.
    lin_sys:=recognize_operator_algebra([
        seq(subs(namF[i]=f,der_list[i]),i=1..nops(namF)),
        # This second line is here to make
        # recognize_operator_algebra take all names which
        # appear in the equations into account.
        indets(hol_sys_D,'name')]);
    # Now we are in business: we work at the level of operators.
    Alg:=lin_sys["algebra"];
    if nargs=2 then
        TOrd:=Groebner:-MonomialOrder(Alg,
            'tdeg'(op(Alg["right_indets"])))
    else
        Dz:=Alg["right_of_left",op(single_eq)];
        TOrd:=Groebner:-MonomialOrder(Alg,
            'lexdeg'(convert(Alg["right_indets"] minus {Dz},'list'),
            [Dz]))
    end if;
    # We compute the linear system satisfied by each function.
    lin_subs:=[seq(seq(der_list[i][j]=lin_sys["system"][i][j],
        j=1..nops(der_list[i])),i=1..nops(namF))];
    # We declare a low-level d-finite representation for each function.
    for i to nops(hol_sys_D) do
        df[i]:=Holonomy:-HO_Internals:-dfinite_create(hol_sys_D[i][1],
            Groebner:-Basis(subs(lin_subs,hol_sys_D[i][2]),TOrd),
                TOrd)
    end do;
    # We rewrite the non-linear expression hol_expr_D in terms of the
    # previous d-finite representations.
    non_lin_expr:=subs({seq(seq(der_list[i][j]=APPLYOPR(df[i],
        convert(Alg["left_indets"],'list'),lin_sys["system"][i][j],Alg),
        j=1..nops(der_list[i])),i=1..nops(namF))},hol_expr_D);
    # Finally, a system of equation is computed by the extended FGLM
    # algorithm.
    answer:=Holonomy:-HO_Internals:-fglm(
        `if`(nargs=2,"holonomic_polynomial_expression",
            "hpe_single_equation"),
        [THE_FUNCTION=non_lin_expr,TERM_ORDER=TOrd],TOrd);
    if answer=NULL then
            error "bug in Holonomy";
    end if;
    answer:=convert(map(Ore_algebra:-OA_Internals:-apply_operator,
        Groebner:-InterReduce(answer,TOrd),f(z),Alg),'set');
    `if`(nargs=2,answer,op(answer))
end proc:
