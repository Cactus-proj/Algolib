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
#        work around a bug in coeffs (Maple V release 4)
#
# INPUT:
#        p        a polynomial
#        x        a list of indeterminates with respect to which the
#                coefficients have to be computed
#
# OUTPUT:
#        the sequence of all coefficients
#
# ALGORITHM:
#        recursive
#
# NOTE:
#        should disappear in rmaple/next release, since the bug has
#        been fixed since the last release
#
`Holonomy/my_coeffs_rec`:=proc(p,x)
    local i;
    option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
    seq(coeff(p,x,i),i=0..degree(p,x))
end proc:

`Holonomy/my_coeffs`:=proc(p,indet_list)
    option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
    if indet_list=[] then p else
        op(map(`Holonomy/my_coeffs_rec`,
            [`Holonomy/my_coeffs`(p,indet_list[2..-1])],
            indet_list[1]))
    end if
end proc:

# FUNCTIONALITY:
#        procedure for normal form in the case of sums
#
# INPUT:
#        term                a term
#        normal_form        a table
#        TOrd                term order
#        (See fglm_algorithm for further
#        explanations.)
#
# OUTPUT:
#        the normal form of term
#
# ALGORITHM:
#        + try to use already computed normal forms when possible
#        + the normal form is of the form
#
#                [t(f[1]),...,t(f[n]),r*t]
#
#          where each t(f[i]) is the normal form of t viewed as acting
#          on f[i] and r is a coefficient used to stay fraction-free
#
# NOTE:
#        normal_form[term] is also assigned the output
#
`Holonomy/normal_form`["+"]:=proc(term,normal_form,TOrd)
    local remainder_table,args_table,i,den_list,position,increment,
        reduced_form,old_normal_form,scale;
    option `Copyright (c) 1995-2009 Frederic Chyzak, INRIA, France`;
    if assigned(normal_form[term]) then
        normal_form[term]
    else
        args_table:=ARGS_TABLE;
        # Compute the normal form recursively.
        #
        # Operands number 1 to TERM_NUMBER are derivatives of
        # functions number 1 to TERM_NUMBER.  Operand number
        # TERM_NUMBER+1 is a multiple of the derivative of the sum.
        if member(true,map2(has,term,TOrd["order_indets"]),
                position) then
            increment:=op(position,TOrd["order_indets"]);
            old_normal_form:=procname(term/increment,normal_form,TOrd);
            reduced_form:=[seq(args_table[i][2]["algebra"]["product"](
                increment,old_normal_form[i]),i=1..TERM_NUMBER),
                TOrd["algebra"]["product"](increment,
                    old_normal_form[TERM_NUMBER+1])]
        else
            reduced_form:=[1$TERM_NUMBER+1]
        end if;
        for i to TERM_NUMBER do
            remainder_table[i] := `Mgfun/old_behaviour_of_Groebner_Reduce`(
              reduced_form[i], args_table[i], 'scale') ;
            remainder_table[i]:=remainder_table[i],scale
        end do;
        den_list:=[seq(remainder_table[i][2],i=1..TERM_NUMBER)];
        normal_form[term]:=
            Ore_algebra:-OA_Internals:-vectprimpart(expand([seq(convert(subsop(
                i=remainder_table[i][1],den_list),`*`),
                i=1..TERM_NUMBER),
                convert(den_list,`*`)*reduced_form[TERM_NUMBER+1]]),
                POLY_INDET);
    end if
end proc:

# FUNCTIONALITY:
#        procedure for normal form in the case of products
#
# INPUT:
#        term                a term
#        normal_form        a table
#        TOrd                term order
#        (See fglm_algorithm for further
#        explanations.)
#
# OUTPUT:
#        the normal form of term
#
# ALGORITHM:
#        + try to use already computed normal forms when possible
#        + the normal form is of the form
#
#                [sum of monomials over D[i][f],r*t]
#
#          where D[i][f]^k denotes the k-th derivative with respect to
#          D[i] of the f-th function and r is a coefficient used to
#          stay fraction-free
#
# NOTE:
#        normal_form[term] is also assigned the output
#
`Holonomy/normal_form`["*"]:=proc(term,normal_form,TOrd)
    local remainder,args_table,i,position,increment,reduced_form,
        old_normal_form,scale,tensor_increment,operator_type;
    option `Copyright (c) 1995-2009 Frederic Chyzak, INRIA, France`;
    global diff,shift,qdilat;
    if assigned(normal_form[term]) then
        normal_form[term]
    else
        args_table:=ARGS_TABLE;
        # Compute the normal form recursively.
        if member(true,map2(has,term,TOrd["order_indets"]),
                position) then
            increment:=op(position,TOrd["order_indets"]);
# Vraisemblablement, ca devrait bugguer (a voir plus tard).
# (pour le cas ou increment est un x, pas un Dx)
            # Find which kind of operator increment is.
            operator_type:=TOrd["algebra"]["type_of_left",
                op([1,1,2],select(has,select(has,
                op(eval(TOrd["algebra"])),"right_of_left"),increment))];
            tensor_increment:=
                `if`(operator_type='diff',
                    convert([seq(increment[i],i=1..TERM_NUMBER)],`+`),
                `if`(member(operator_type,{'shift','qdilat'}),
                    convert([seq(increment[i],i=1..TERM_NUMBER)],`*`),
                    ERROR(cat("type ",operator_type," not dealt with yet"))));
            old_normal_form:=procname(term/increment,normal_form,TOrd);
            reduced_form:=[args_table[1][2]["algebra"]["product"](
                tensor_increment,old_normal_form[1]),
                TOrd["algebra"]["product"](increment,old_normal_form[2])]
        else
            reduced_form:=[1,1]
        end if;
        for i to TERM_NUMBER do
            # No expand is needed here because reduce normalizes its input by default.
            remainder := `Mgfun/old_behaviour_of_Groebner_Reduce`(
              reduced_form[1], args_table[i], 'scale') ;
            reduced_form:=[remainder,scale*reduced_form[2]]
        end do;
        normal_form[term]:=Ore_algebra:-OA_Internals:-vectprimpart(expand(reduced_form),
            TENSOR_POLY_INDET);
    end if
end proc:

# FUNCTIONALITY:
#        procedure for normal form in the case of extension/contraction
#
# INPUT:
#        term                a term
#        normal_form        a table
#        TOrd                term order
#        (See fglm_algorithm for further
#        explanations.)
#
# OUTPUT:
#        the normal form of term
#
# WEAKNESS:
#        does not try to work fraction-free
#
# NOTE:
#        normal_form[term] is also assigned the output
#
`Holonomy/normal_form`["extension_contraction"]:=
proc(term,normal_form,TOrd)
    local red_form,scale;
    option `Copyright (c) 1996-2009 Frederic Chyzak, INRIA, France`;
    if assigned(normal_form[term]) then
        normal_form[term]
    else
        red_form := `Mgfun/old_behaviour_of_Groebner_Reduce`(
          term, GROEBNER_BASIS, TERM_ORDER, scale) ;
        normal_form[term]:=red_form/scale
    end if
end proc:

# FUNCTIONALITY:
#        procedure for normal form in the case of an holonomic
#        polynomial expression without RootOf's
#
# INPUT:
#        term                a term
#        normal_form        a table
#        TOrd                term order
#        (See fglm_algorithm for further
#        explanations.)
#
# OUTPUT:
#        the normal form of term
#
# WEAKNESS:
#        + does not try to use already computed normal forms when
#          possible
#        + does not try to work fraction-free
#
# NOTE:
#        normal_form[term] is also assigned the output
#
`Holonomy/normal_form`["holonomic_polynomial_expression"]:=
proc(term,normal_form,TOrd)
    local red_form;
    option `Copyright (c) 1997-2002 Frederic Chyzak, INRIA, France`;
    global APPLYOPR,ALGSUBS,distributed;
    if assigned(normal_form[term]) then
        normal_form[term]
    else
        red_form:=normal(Ore_algebra:-OA_Internals:-apply_operator(term,
            THE_FUNCTION,TOrd["algebra"]));
        normal_form[term]:=collect([numer(red_form),denom(red_form)*term],
            {'APPLYOPR','ALGSUBS'},'distributed',normal)
    end if
end proc:
`Holonomy/normal_form`["hpe_single_equation"]:=
`Holonomy/normal_form`["holonomic_polynomial_expression"]:

# FUNCTIONALITY:
#        procedure for normal form in the case of an holonomic
#        polynomial expression with RootOf's
#
# INPUT:
#        term                a term
#        normal_form        a table
#        TOrd                term order
#        (See fglm_algorithm for further
#        explanations.)
#
# OUTPUT:
#        the normal form of term
#
# WEAKNESS:
#        + does not try to use already computed normal forms when
#          possible
#        + does not try to work fraction-free
#
# NOTE:
#        normal_form[term] is also assigned the output
#
`Holonomy/normal_form`["holonomic_polynomial_expression_RootOf"]:=
proc(term,normal_form,TOrd)
    local red_form;
    option `Copyright (c) 1997-2002 Frederic Chyzak, INRIA, France`;
    global APPLYOPR,ALGSUBS,RootOf,distributed;
    if assigned(normal_form[term]) then
        normal_form[term]
    else
        red_form:=collect(Ore_algebra:-OA_Internals:-apply_operator(term,
            THE_FUNCTION,TOrd["algebra"]),{'APPLYOPR','ALGSUBS'},
            'distributed',evala@Normal);
        normal_form[term]:=collect([numer(red_form),denom(red_form)*term],
            {'APPLYOPR','ALGSUBS','RootOf'},'distributed',normal)
    end if
end proc:
`Holonomy/normal_form`["hpe_single_equation_RootOf"]:=
`Holonomy/normal_form`["holonomic_polynomial_expression_RootOf"]:

# FUNCTIONALITY:
#        procedure to find dependencies in the case of sums
#
# INPUT:
#        monoideal                a list of terms
#        normal_form_table        a table of normal forms
#        (See fglm_algorithm for further
#        explanations.)
#
# OUTPUT:
#        either FAIL if no dependency can be found, or a polynomial
#        if one has been found
#
# ALGORITHM:
#        Gaussian elimination between the normal form already computed
#
`Holonomy/find_dependency`["+"]:=proc(monoideal,
        normal_form_table)
    local u;
    option `Copyright (c) 1995-2005 Frederic Chyzak, INRIA, France`;
    yet_another_gauss(map(proc(l,u)
            local i;
            option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
            [expand(add(u^i*l[i],i=1..TERM_NUMBER)),l[TERM_NUMBER+1]]
        end,
        # First isolate terms to try and find a linear dependency
        # between.
        map(proc(i,t)
                option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
                t[i]
            end,
            # Take only those terms for which no linear dependency has
            # been found.
            map(op,{indices(normal_form_table)}) minus
                convert(monoideal,set),
            normal_form_table),u),
      [op(POLY_INDET),u],
      convert(TERM_ORDER["order_indets"], 'set'))
end proc:

# FUNCTIONALITY:
#        procedure to find dependencies in the case of products
#
# INPUT:
#        monoideal                a list of terms
#        normal_form_table        a table of normal forms
#        (See fglm_algorithm for further
#        explanations.)
#
# OUTPUT:
#        either FAIL if no dependency can be found, or a polynomial
#        if one has been found
#
# ALGORITHM:
#        Gaussian elimination between the normal form already computed
#
`Holonomy/find_dependency`["*"]:=proc(monoideal,
        normal_form_table)
    local u;
    option `Copyright (c) 1995-2005 Frederic Chyzak, INRIA, France`;
    yet_another_gauss(
        # First isolate terms to try and find a linear dependency
        # between.
        map(proc(i,t)
                option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
                t[i]
            end,
            # Take only those terms for which no linear dependency has
            # been found.
            map(op,{indices(normal_form_table)}) minus
                convert(monoideal,set),
            normal_form_table),
      [op(TENSOR_POLY_INDET),u],
      convert(TERM_ORDER["order_indets"], 'set'))
end proc:

# FUNCTIONALITY:
#        procedure to find dependencies in the case of extension/contraction
#
# INPUT:
#        monoideal                a list of terms
#        normal_form_table        a table of normal forms
#        (See fglm_algorithm for further
#        explanations.)
#
# OUTPUT:
#        either FAIL if no dependency can be found, or a polynomial
#        if one has been found
#
# ALGORITHM:
#        Gaussian elimination between the normal form already computed
#
`Holonomy/find_dependency`["extension_contraction"]:=
proc(monoideal,normal_form_table)
    local indet_list,term,eta,solution,zero,sys,dependency;
    option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
    global FAIL,distributed;
    indet_list:=map(op,[indices(normal_form_table)]);
    for term in monoideal do
        indet_list:=remove(divide,indet_list,term)
    end do;
    # Compute a linear combination of normal forms already computed
    # with undetermined coefficients.  We want this combination to be
    # zero.
    zero:=expand(numer(normal(convert([seq(eta[term]*normal_form_table[term],
        term=indet_list)],`+`))));
    # Compute the set of all coefficients with respect to all non
    # commutative indeterminates of the localized algebra.
    sys:={`Holonomy/my_coeffs`(zero,POLY_INDET)};
    # Solve this system in all undetermined coefficients.
    solution:=SolveTools:-Linear(sys,{seq(eta[term],term=indet_list)});
    dependency:=subs(solution,
        convert([seq(eta[term]*term,term=indet_list)],`+`));
    `if`(dependency=0,'FAIL',collect(primpart(numer(subs(map(n->n=1,
        # Actually, not all indeterminates in POLY_INDET are remaining
        # at this time.
        map2(op,1,select(evalb,solution))),dependency)),POLY_INDET),
        POLY_INDET,'distributed',factor))
end proc:

# FUNCTIONALITY:
#        procedure to find dependencies in the case of an holonomic
#        polynomial expression without RootOf
#
# INPUT:
#        monoideal                a list of terms
#        normal_form_table        a table of normal forms
#        (See fglm_algorithm for further
#        explanations.)
#
# OUTPUT:
#        either FAIL if no dependency can be found, or a polynomial
#        if one has been found
#
# ALGORITHM:
#        Gaussian elimination between the normal form already computed
#
`Holonomy/find_dependency`["holonomic_polynomial_expression"]:=
proc(monoideal,normal_form_table)
    local sys,var;
    option `Copyright (c) 1997-2009 Frederic Chyzak, INRIA, France`;
    global set,specfunc,anything,APPLYOPR,ALGSUBS,distributed;
    sys:=map(proc(i,t)
            option `Copyright (c) 1997-2002 Frederic Chyzak, INRIA, France`;
            t[i]
        end,map(op,{indices(normal_form_table)}) minus
            convert(monoideal,'set'),normal_form_table);
    sys := collect(sys,
      {'APPLYOPR','ALGSUBS','Ore_algebra:-APPLYOPR','Ore_algebra:-ALGSUBS'},
      'distributed', normal);
    var := indets(sys,
      'specfunc'('anything',
        {'APPLYOPR','ALGSUBS','Ore_algebra:-APPLYOPR','Ore_algebra:-ALGSUBS'}
      )) ;
    yet_another_gauss(sys, var,
      convert(TERM_ORDER["order_indets"], 'set'))
end proc:
`Holonomy/find_dependency`["hpe_single_equation"]:=
`Holonomy/find_dependency`["holonomic_polynomial_expression"]:

# FUNCTIONALITY:
#        procedure to find dependencies in the case of an holonomic
#        polynomial expression with RootOf
#
# INPUT:
#        monoideal                a list of terms
#        normal_form_table        a table of normal forms
#        (See fglm_algorithm for further
#        explanations.)
#
# OUTPUT:
#        either FAIL if no dependency can be found, or a polynomial
#        if one has been found
#
# ALGORITHM:
#        Gaussian elimination between the normal form already computed
#
`Holonomy/find_dependency`["holonomic_polynomial_expression_RootOf"]:=
proc(monoideal,normal_form_table)
    local sys,var;
    option `Copyright (c) 1997-2009 Frederic Chyzak, INRIA, France`;
    global set,specfunc,anything,APPLYOPR,ALGSUBS,RootOf,distributed;
    sys:=map(proc(i,t)
            option `Copyright (c) 1997-2002 Frederic Chyzak, INRIA, France`;
            t[i]
        end,map(op,{indices(normal_form_table)}) minus
            convert(monoideal,'set'),normal_form_table);
    sys := collect(sys,
      {'APPLYOPR','ALGSUBS','Ore_algebra:-APPLYOPR','Ore_algebra:-ALGSUBS',
      'RootOf'},
      'distributed',normal);
    var := indets(sys,
      'specfunc'('anything',
        {'APPLYOPR','ALGSUBS','Ore_algebra:-APPLYOPR','Ore_algebra:-ALGSUBS',
        'RootOf'}
      )) ;
    yet_another_gauss(sys, var,
      convert(TERM_ORDER["order_indets"], 'set'))
end proc:
`Holonomy/find_dependency`["hpe_single_equation_RootOf"]:=
`Holonomy/find_dependency`["holonomic_polynomial_expression_RootOf"]:

# FUNCTIONALITY:
#        provide a test to stop the main loop in algorithms based on
#        the FGLM algorithm
#
# INPUT:
#        border                a list of terms still to be dealt with
#        monoideal        a list of terms to be avoided
#        TOrd                the term order with respect to with the terms
#                        are considered
#
# OUTPUT:
#        a boolean value, true to go on, false to stop
#
# ALGORITHM:
#        o in the zero dimensional cases, testing if border is [] is
#          enough
#        o in the holonomic cases, compute the Hilbert dimension of the
#          monoideal and compare with the number of differential
#          indeterminates in TOrd[algebra]
#
# WEAKNESS:
#        when a zero dimensional monoideal may appear as an input,
#        the procedure should explicitly test ``and monoideal<>[]'',
#        fglm_algorithm may bomb
#
`Holonomy/termination`["+"]:=
    '`Holonomy/fglm_termination_proc`("zero_dimensional_case")':
`Holonomy/termination`["*"]:=
    '`Holonomy/fglm_termination_proc`("zero_dimensional_case")':
`Holonomy/termination`["extension_contraction"]:=
    '`Holonomy/fglm_termination_proc`("holonomic_case")':
`Holonomy/termination`["holonomic_polynomial_expression"]:=
    '`Holonomy/fglm_termination_proc`("zero_dimensional_case")':
`Holonomy/termination`["holonomic_polynomial_expression_RootOf"]:=
    '`Holonomy/fglm_termination_proc`("zero_dimensional_case")':
`Holonomy/termination`["hpe_single_equation"]:=
    '`Holonomy/fglm_termination_proc`("single_equation_case")':
`Holonomy/termination`["hpe_single_equation_RootOf"]:=
    '`Holonomy/fglm_termination_proc`("single_equation_case")':

# FUNCTIONALITY:
#        use FGLM algorithm to compute the sum of holonomic functions,
#        their product, or more generally a polynomial that involves
#        holonomic functions
#
# INPUT:
#        operation        the name of the operation to perform;
#                        currently recognized names are:
#
#                                "+"        to compute the sum of the list
#                                        of holonomic described in
#                                        subs_list
#
#                                "*"        to compute the product of the
#                                        list of holonomic described in
#                                        subs_list
#
#                                "extension_contraction"
#                                        to compute the
#                                        extension/contraction of an
#                                        ideal when localizing a Weyl
#                                        algebra
#
#                                "holonomic_polynomial_expression"
#                                        to compute an expression
#                                        involving several holonomic
#                                        functions but no substitutions
#                                        by RootOf's
#
#                                "hpe_single_equation"
#                                        same as previously, but
#                                        computing a single equation
#
#                                "holonomic_polynomial_expression_RootOf"
#                                        to compute an expression
#                                        involving several holonomic
#                                        functions and with
#                                        substitutions by RootOf's
#
#                                "hpe_single_equation_RootOf"
#                                        same as previously, but
#                                        computing a single equation
#
#        subs_list        a list of equations that parametrize the
#                        pre-existing functions for normal form and
#                        finding dependencies; this list depends on
#                        operation
#
#        TOrd                term order to perform FGLM algorithm with
#                        respect to
#
# OUTPUT:
#        a Groebner basis with respect to the term order TOrd which
#        describes the operation operation
#
# ALGORITHM:
#        call a FGLM algorithm procedure with the functions for normal
#        form and finding dependencies that correspond to the operation
#        being computed
#
`Holonomy/fglm`:=proc(operation,subs_list,TOrd)
    local normal_form,find_dependency,res,GB;
    option `Copyright (c) 1997-2008 Frederic Chyzak, INRIA, France`;
    ASSERT(member(operation,["+","*","extension_contraction",
        "holonomic_polynomial_expression",
        "holonomic_polynomial_expression_RootOf",
        "hpe_single_equation","hpe_single_equation_RootOf"]),
        "operation not available");
    normal_form:=subs(subs_list,eval(`Holonomy/normal_form`[operation]));
    find_dependency:=subs(subs_list,eval(
        `Holonomy/find_dependency`[operation]));
    res:=`Holonomy/fglm_algorithm`(normal_form,
        find_dependency,`Holonomy/termination`[operation],TOrd);
    res:=map(op,[entries(res[2])]);
    GB:=Groebner:-InterReduce(res,TOrd);
    # This outputs the Groebner basis itself.
    Groebner:-RememberBasis(res,GB,TOrd);
    GB
end proc:
