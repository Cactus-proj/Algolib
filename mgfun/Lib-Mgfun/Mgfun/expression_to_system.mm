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

# Create a dfinite structure with substitution in an atomic function.
# Assumption: all constants that appear in expr are of type "name".
`Mgfun/substituted_atom_to_system`:=proc(expression,var_type_set,alg_rel_set)
local
# Variables.
f,i,constants,name_constants,rat_or_alg,var_number,descr,type_list,Alg,TOrd,df,ann_poly,type_table,signature,param_seq,j,T,extracted_arg,G,GB,q_calculus,qshift_number,p,q,expr,elem1,elem2,operator_number,inverse_numbers,the_q,
# Symbols.
alg_expr,l,z,qz,dz,inv_dz;
option `Copyright (c) 1999-2008 Frederic Chyzak and Cyril Germa, INRIA, France`;
global comm,function,shift,diff,qshift,qdilat,`shift+dual_shift`,`qdilat+dual_qdilat`,parameter,ratpoly,list,tdeg,lexdeg,RootOf,_Z;

    # Algebra definition.
    f:=op(0,expression);
    if f='hypergeom' then
        p:=nops(op(1,expression));
        q:=nops(op(2,expression));
        operator_number:=p+q+1;
        signature:=['shift'$p+q,'diff'];
        expr:=`Mgfun/hypergeom`(op(op(1,expression)),
            op(op(2,expression)),op(3,expression))
    else
        signature:=dfinite_spec_table[f][1];
        expr:=expression
    end if;
    inverse_numbers:=NULL;
    q_calculus:=false;

    # Determination of the list of OPERATORS and VARIABLES of skew_algebra.
    # Hyp.: type non_root_of_one is reserved as last argument in a
    # q_calculus function, unless there is a parameter afterwards.
    for i to nops(signature) do
        if signature[i]='diff' then
            type_table[i]:=signature[i]=[dz[i],z[i]]
        elif signature[i]='shift' then
            type_table[i]:='`shift+dual_shift`'=[dz[i],inv_dz[i],z[i]];
            inverse_numbers:=inverse_numbers,i
        elif signature[i]='parameter' then
            type_table[i]:=NULL
        elif signature[i]='non_root_of_one' then
            type_table[i]:=NULL;
            # This is very important for compatibility with other
            # parts of the code.
            the_q:=z[i];
            q_calculus:=true
        elif signature[i]='qshift' then
            type_table[i]:='`qdilat+dual_qdilat`'
                =[dz[i],inv_dz[i],qz[i]=the_q^z[i]]
        elif signature[i]='qdilat' then
            type_table[i]:='`qdilat+dual_qdilat`'
                =[dz[i],inv_dz[i],z[i],the_q]
               else
            error "wrong argument in table"
        end if
    end do;
    type_list:=seq(type_table[i],i=1..nops(signature));

    var_number:=nops(subs({'parameter'=NULL,'non_root_of_one'=NULL},
        signature));
    qshift_number:=nops(select(has,signature,'qshift'));
    param_seq:=seq(`if`(signature[i]='parameter',op(i,expr),NULL),
        i=1..nops(expr));

    # Description of the function in the table.
    if f='hypergeom' then
        descr:=["foo",'tdeg'(seq(dz[i],i=1..p+q+1),seq(inv_dz[i],i=1..p+q))]
    else
        descr:=dfinite_spec_table[f][2](
            seq(z[i],i=1..var_number),
            seq(`if`(signature[i]='diff',dz[i],op([dz[i],inv_dz[i]])),
                i=1..var_number),
            `if`(q_calculus,op([the_q,seq(qz[i],i=1..qshift_number)]),NULL),
            param_seq)
    end if;

    # Determination of the SET OF CONSTANTS to extend the algebra.
    # (This has to take place after the set of equations has been
    # extracted from the table.)
    # Caution: no non-name constant is allowed to appear.
    constants:=remove(has,remove(has,
        indets([expr,descr[1]]),map2(op,1,var_type_set)),{z,qz,dz,inv_dz});
    name_constants:=select(type,constants,'name');
    ASSERT(constants=name_constants,"non-name constants not allowed");
    # Extension for q-calculus
    name_constants:=name_constants union `if`(q_calculus,{the_q},{});

    Alg:=Ore_algebra:-OA_Internals:-skew_algebra(type_list,
        'comm'=name_constants union indets(alg_rel_set),
        'alg_relations'=alg_rel_set);
    TOrd:=Groebner:-MonomialOrder(Alg,descr[2]);

    # EQUATIONS FOR THE DIFFERENT SUBSTITUTIONS FOR ALGSUBS.
    for i to nops(expr) do
        # Two cases for the substitution in atomic function: either
        # rational (this case includes integer substitution) or
        # algebraic -> two differents calls in ALGSUBS.
        if type(op(i,expr),'ratpoly') then
            rat_or_alg[i]:=op(i,expr)
        # the multivariate algebraic case is now implemented
        elif nops(indets(op(i,expr)) intersect
        map2(op,1,select(has,var_type_set,'diff'))) >= 1 then
            # Calculate characteristic polynomial of a multivariate
            # algebraic expression.

            # Extraction of term of type `^` in the argument.
            extracted_arg:=convert(select(type,indets(op(i,expr)),
                {`^`,'RootOf'}),'list');
            # Sort by decreasing length.
            extracted_arg:=sort(extracted_arg,
                (a,b)->evalb(length(a)>length(b)));
            for j to nops(extracted_arg) do
                T[j]:=[extracted_arg[j]=l[j],`if`(type(extracted_arg[j],`^`),
                    l[j]^denom(op([j,2],extracted_arg))
                        -op([j,1],extracted_arg)
                            ^numer(op([j,2],extracted_arg)),
                    subs(_Z=l[j],op([j,1],extracted_arg)))]
            end do;

            G:=subs(seq(op(1,T[j]),j=1..nops(extracted_arg)),
                {alg_expr-op(i,expr),seq(op(2,T[j]),
                    j=1..nops(extracted_arg))});
            GB := Groebner:-Basis(map(numer, G),
                lexdeg([seq(l[j], j=1..nops(extracted_arg))], [alg_expr]));
            ann_poly:=op(remove(has,GB,[seq(l[j],j=1..nops(extracted_arg))]));
            rat_or_alg[i]:=RootOf(ann_poly,alg_expr)
        end if
    end do;

    # Compute the Groebner basis in the hypergeom case.
    # Compute the differential equation satisfied by the diff variable.
    if f='hypergeom' then
        elem1:=z[operator_number]*dz[operator_number];
        for i from p+1 to p+q do
            elem1:=Ore_algebra:-OA_Internals:-skew_product
                (elem1,z[operator_number]*dz[operator_number]+z[i]-1,Alg)
        end do;
        elem2:=z[operator_number];
        for i to p do
            elem2:=Ore_algebra:-OA_Internals:-skew_product
                (elem2,z[operator_number]*dz[operator_number]+z[i],Alg)
        end do;

        descr[1]:=[seq(z[i]*dz[i]-z[i]-z[operator_number]*dz[operator_number],
            i=1..p),seq(z[operator_number]*dz[i]*dz[operator_number]+
            z[i]*(dz[i]-1),i=p+1..p+q),
            collect(elem1-elem2,dz[operator_number]),
            seq(dz[i]*inv_dz[i]-1,i=1..p+q)];
        descr[1]:=Groebner:-Basis(descr[1],TOrd)
    end if;

    # DF structure to take the algebraic substitution into account.
    df:=ALGSUBS(Holonomy:-HO_Internals:-dfinite_create(f,
            `if`(nops([inverse_numbers])=0,descr[1],
                # TEMPORARY HACK: since the simplifications of
                # products of shifts and their inverses do not
                # automatically take place in algebras built on the
                # commutation type `shift+dual_shift`, we have to
                # adjoin the corresponding relations here.
                #
                # This work-around is highly sensitive to possible
                # changes in the ordering of Groebner bases.
                #
                # Should disappear when proper normalization has been
                # implemented in the routine computing products.
                [op(descr[1]),seq(dz[i]*inv_dz[i]-1,i=inverse_numbers)]),
            TOrd),
        # So far, only a single basis q may appear in q-calculus.
        [seq(z[i],i=1..var_number+`if`(q_calculus,1,0))],1,Alg,
        {seq(z[i]=rat_or_alg[i],i=1..var_number+`if`(q_calculus,1,0)),seq(
            # 'qshift' cases have been translated into `qdilat+dual_qdilat`,
            # but still need two entries z[i]=... and qz[i]=... here.
            `if`(signature[i]='qshift',
                # Break powers of the_q into pieces.
                #
                # Would combine(...,power,symbolic) be better/faster?
                qz[i]=expand(rat_or_alg[op(the_q)]^rat_or_alg[i]),NULL),
                    i=1..var_number)});

    expression=df

end proc: # `Mgfun/substituted_atom_to_system`

# Return the system satisfied by the expression. Functions in the expression
# are supposed to be in the table (except for hypergeom).
expression_to_system:=proc(expr,var_type_set)
local
# Variables.
f,i,x,type_list,var_set,name_constants,renamed_expr,term,df_tab,Alg,TOrd,fun_set,subs_set,oper,comm_set,alg_rel_set,q_fun_set,q_var_type_set,non_q_var_type_set,j,q_fun,qtype_table,q_operator_number,q_comm_set,alg_constants_set,non_alg_other_constants,alg_constants_list,alg_subs_list,T,aux_U,aux_Alg,type_struct,power_to_qn,df_set,
expanded_expr,
# Symbols.
dv,qn,a,k;
option `Copyright (c) 1999-2009 Frederic Chyzak and Cyril Germa, INRIA, France`;
global tdeg,comm,set,name,function,RootOf,ratpoly,algfun,radfun,rational,`Mgfun/qPOCHHAMMER`,qdilat,qshift;
# type_list: list with variables, their associated operator and their
#              type for the definition of algebra
# var_set: set of variables
# comm_set: set to extend the algebra
# alg_rel_set: set of algebraic relations to extend the algebra
# term: design an atomic function of the expression
# df_tab: table which contains the dfinite structure of
#                       substituted functions

    renamed_expr := type_checking(expr, var_type_set) ;

    # If expression is not recognized as d-finite, try to normalize it
    # and test again.
    if renamed_expr = `Mgfun/NOT_DFINITE` then
      expanded_expr := expand(expr) ;
      if expanded_expr <> expr then
        renamed_expr := type_checking(expanded_expr, var_type_set)
      end if
    end if ;

    if renamed_expr = `Mgfun/NOT_DFINITE` then
      error "expression is not d-finite, or not recognized as such"
    end if ;

    # Rename some elements of the expression in order to extend the
    # algebra.

    # Rename non-name constants to names.
    #
    # In fact, one should compute representation of the complete
    # algebra of constants that appear in the expression.  With the
    # current implementation, phi(a) and phi(a)^2 will not be viewed
    # as algebraically related constants.
    var_set:=map2(op,1,var_type_set);
    # Select non-algebraic constants.
    name_constants:=indets(renamed_expr,'name') minus var_set;
    non_alg_other_constants:=remove(type,
        remove(has,indets(renamed_expr,{'function',`^`}),var_set),
            {'algfun','radfun'}('rational'));
    renamed_expr:=subs(
        {seq(op(i,non_alg_other_constants)=k[i],
            i=1..nops(non_alg_other_constants)),
        Pi=`Mgfun/Pi`},renamed_expr);

    # Get rid of any complex constant, for I has to be viewed
    # as the algebraic extension by RootOf(_Z^2+1).
    renamed_expr:=subs(
        map(z->z=Re(z)+RootOf(_Z^2+1)*Im(z),
            remove(type,indets(renamed_expr,'complexcons'),'realcons')),
        renamed_expr);

    # Detect algebraic constants (including RootOf) and rename to
    # name, keeping track of algebraic relations.
    alg_constants_set:=remove(has,indets(renamed_expr,{`^`,'RootOf'}),
        var_set);
    alg_constants_set:=remove(type,alg_constants_set,
        'ratpoly'('rational',indets(alg_constants_set,'name')));
    alg_constants_list:=sort(convert(alg_constants_set,list),
        (a,b)->evalb(length(a)>length(b)));
    alg_subs_list:=[seq(op(i,alg_constants_list)=a[i],
        i=1..nops(alg_constants_list))];
    renamed_expr:=subs(op(alg_subs_list),renamed_expr);
    for i to nops(alg_subs_list) do
        T[i]:=[op(i,alg_subs_list),
            `if`(type(op([i,1],alg_subs_list),`RootOf`),
            subs(_Z=op([i,2],alg_subs_list),op(op([i,1],alg_subs_list))),
                # ``numer'' is mandatory here for the case when the
                # exponent op([i,1,2],alg_subs_list) is negative.
                numer(
                    op([i,2],alg_subs_list)^denom(op([i,1,2],alg_subs_list))
                    -op([i,1,1],alg_subs_list)^numer(op([i,1,2],alg_subs_list))
                )
            )]
    end do;
    alg_rel_set:=subs(seq(op(1,T[j]),j=1..nops(alg_subs_list)),
            {seq(op(2,T[j]),j=1..nops(alg_subs_list))});

    # For each atomic function, create its DF structure.
    oper:="holonomic_polynomial_expression";
    fun_set:=select(has,indets(renamed_expr,'function'),var_set);
    # RootOf may now appear only as a substitution, but
    # indets(...,'function') has returned them.
    fun_set:=remove(type,fun_set,'RootOf');
    for term in fun_set do
        f:=op(0,term);
        if assigned(dfinite_spec_table[f]) or f='hypergeom' then
            df_tab[term]:=
                `Mgfun/substituted_atom_to_system`(term,var_type_set,
                alg_rel_set);
            if oper="holonomic_polynomial_expression" and
                has(df_tab[term],'RootOf')
            then
                oper:="holonomic_polynomial_expression_RootOf"
            end if
        else
            error "bug: recognized function not in table!"
        end if
    end do;

    # q-Case: define a subset q_fun_set of fun_set dedicated to
    # q-calculus and build the corresponding elements for the
    # definition of the algebra.
    q_fun_set:=select(p->evalb(op(0,p)=`Mgfun/qPOCHHAMMER`),fun_set);
    q_comm_set:=map2(op,1,select(has,var_type_set,'non_root_of_one'));
    q_var_type_set:=remove(has,`union`(seq(map((t,q)->[t,q],
        select(has,var_type_set,indets(q_fun,'name')),op(3,q_fun)),
            q_fun=q_fun_set)),'non_root_of_one');
#     # Actually, powers of constants may also be considered part of
#     # q-calculus.
#     q_var_type_set:=q_var_type_set union
#         indets(select((f,v)->type(op(1,f),{'linear','quadratic'}(v)) and
#                     type(op(2,f),'constant'),
#             select(p->evalb(op(0,p)=`Mgfun/POWER`),fun_set),
#             map2(op,1,select(has,var_type_set,'qshift'))),'name')
#         intersect map2(op,1,var_type_set);
    for i to nops(q_var_type_set) do
        if op([i,1,2],q_var_type_set)='qshift' then
            qtype_table[i]:='qdilat'=
                [dv[i],qn[i]=op([i,2],q_var_type_set)^op([i,1,1],
                    q_var_type_set)]
        elif op([i,1,2],q_var_type_set)='qdilat' then
            qtype_table[i]:='qdilat'=
                [dv[i],op([i,1,1],q_var_type_set),op([i,2],q_var_type_set)]
        end if
    end do;
    q_operator_number:=nops(q_var_type_set);
    non_q_var_type_set:=remove(has,var_type_set,
        {'qshift','qdilat','non_root_of_one'});

    # Algebra definition.
    type_list:=
        seq(qtype_table[i],i=1..q_operator_number),
        seq(op([i,2],non_q_var_type_set)=
            [dv[i+q_operator_number],op([i,1],non_q_var_type_set)],
                i=1..nops(non_q_var_type_set));
    # Extension for the algebra
    comm_set:=`union`(name_constants,q_comm_set,
        convert(map2(op,2,alg_subs_list),'set'),
        {seq(k[i],i=1..nops(non_alg_other_constants)),
        # Hard-coding the list *HERE* is ugly style.
        #
        # What about Fresnelf and Fresnelg?
        `if`(has(renamed_expr,{`Mgfun/Pi`,'FresnelC','FresnelS'}),
            `Mgfun/Pi`,NULL)});
    Alg:=Ore_algebra:-OA_Internals:-skew_algebra(type_list,'comm'=comm_set,
        'alg_relations'=alg_rel_set);
    TOrd:=Groebner:-MonomialOrder(Alg,
        'tdeg'(seq(dv[i],i=1..nops(non_q_var_type_set)+q_operator_number)));
    power_to_qn:=map(x->x[4]^x[3]=x[2],map2(op,2,
        select(l->nops(l)=4,select(has,Alg["type_struct"],'qdilat'))));
    # This ``for'' loop is not restricted to q_fun_set to allow for
    # the introduction of new q-calculus functions in the future.
    for term in fun_set do
# The following fails for ``term'' whose df_tab is term=n*ALGSUBS(...).
# (e.g., term=`Mgfun/GAMMA`(n+1,1), which represents n!.)
#        aux_Alg:=op([2,4],df_tab[term]);
        df_set:=select(has,indets(op(2,df_tab[term]),'function'),'ALGSUBS');
        # Functions like the Whittaker functions, which depends on
        # several ALGSUBS structures, are no q-calculus functions and
        # can be rejected with no harm.
        if nops(df_set)<>1 then next end if;
        aux_Alg:=op([1,4],df_set);
        if has(aux_Alg["type_struct"],'`qdilat+dual_qdilat`') then
            type_struct:=select(p->evalb(nops(op(2,p))=5),select(has,
                aux_Alg["type_struct"],'`qdilat+dual_qdilat`'));
# Avec une hypothese sur les formes entieres qui interviennent.
            aux_U:={seq(x[3]=subs(power_to_qn,
                    # This use of expand@subs makes a strong
                    # assumption on the structure of x[4]: its
                    # constant term is required to be an integer.
                    expand(subs(op([2,5],df_tab[term]),x[5]^x[4]))),
                x=map2(op,2,type_struct))};
            df_tab[term]:=
                subsop([2,-1]=aux_U union op([2,-1],df_tab[term]),df_tab[term])
        end if
    end do;
    subs_set:={seq(df_tab[term],term=fun_set)};
    # Use of 'fglm' (by substituting, in THE_FUNCTION, atomic function
    # with their dfinite structure) to obtain the system satisfied by
    # the expression.
    Holonomy:-HO_Internals:-fglm(oper,
            [THE_FUNCTION=subs(subs_set,renamed_expr),TERM_ORDER=TOrd],TOrd),
        TOrd,
        {seq(k[i]=op(i,non_alg_other_constants),
            i=1..nops(non_alg_other_constants)),
        seq(a[i]=op([i,1],alg_subs_list),i=1..nops(alg_subs_list)),
        `Mgfun/Pi`=Pi}

end proc: # expression_to_system
