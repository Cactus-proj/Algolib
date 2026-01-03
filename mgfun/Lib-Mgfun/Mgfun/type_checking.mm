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

# Check for integer linear substitution in a function argument
`Mgfun/is_integer_linear_term`:=proc(term,shift_set)
# term: expression of an operand of a function argument
# shift_set: set of shift variable
local v_set,v,correct;
option `Copyright (c) 1999-2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
global integer;

    correct:=true;
    v_set:=shift_set intersect indets(term);
    if v_set<>{} then                         # Term is not constant
        if nops(v_set)<>1 then                # Several shift variables
            correct:=false
        else                        # number of shift variable is 1
            v:=op(v_set);
            if not type(term/v,'integer')
            then
                correct:=false
            end if
        end if
    end if;
    correct

end proc: # `Mgfun/is_integer_linear_term`

# Check for integer quadratic substitution in a function argument
`Mgfun/is_integer_quadratic_term`:=proc(term,qshift_set)
# term: expression of an operand of a function argument
# qshift_set: set of qshift variable
local v_set,v,correct;
option `Copyright (c) 2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
global integer;

    correct:=true;
    v_set:=qshift_set intersect indets(term);
    if v_set<>{} then                         # Term is not constant
        if nops(v_set)<>1 then                # Several qshift variables
            correct:=false
        else                        # number of qshift variable is 1
            v:=op(v_set);
            if not type(coeff(term,v,2),'integer')
            or not type(coeff(term,v,1),'integer')
            then
                correct:=false
            end if
        end if
    end if;
    correct

end proc: # `Mgfun/is_quadratic_linear_term`

# Check that the expression is an integer linear substitution or not
`Mgfun/args_type_check_table`['shift']:=proc(expr,var_type_set)
# expr: expression of a function argument which has to be an integer
# substitution
# var_type_set: set of the differents variables with their type i.e.,
# shift, diff, qshift, qdilat
local correct,term,shift_set,other_set;
# shift_set: set of shift variable
# other_set: set of other variable
option `Copyright (c) 1999-2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
global shift;

    correct:=true;
    shift_set:=map2(op,1,select(has,var_type_set,'shift'));
    other_set:=map2(op,1,remove(has,var_type_set,'shift'));

    if not has(expr,other_set) then

        for term in `if`(type(expr,`+`),expr,[expr]) while correct do
            correct:=`Mgfun/is_integer_linear_term`(term,shift_set)
        end do;

    else
        correct:=false
    end if;

    correct

end proc: # `Mgfun/args_type_check_table`['shift']


# Check that the expression is algebraic
`Mgfun/args_type_check_table`['diff']:=proc(expr,var_type_set)
local diff_set,other_set,correct;
# diff_set: set of differential variables
option `Copyright (c) 1999-2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
global diff,radfun,algfun,anything;

    correct:=true;
    diff_set:=map2(op,1,select(has,var_type_set,'diff'));
    other_set:=map2(op,1,remove(has,var_type_set,'diff'));

    if has(expr,other_set) then
        correct:=false
    else
        correct:=type(expr,{'radfun'('anything',diff_set),'algfun'('anything',diff_set)})
    end if;

    correct

end proc: # `Mgfun/args_type_check_table`['diff']

# Check that the type qdilat is an algebraic substitution
`Mgfun/args_type_check_table`[qdilat]:=proc(expr,var_type_set)
local qdilat_set,var_set,term,correct;
# diff_set: set of differential variables
option `Copyright (c) 1999-2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
global integer,qdilat;

    correct:=true;
    qdilat_set:=map2(op,1,select(has,var_type_set,'qdilat'));
    var_set:=map2(op,1,var_type_set);
    for term in `if`(type(expr,`*`),expr,[expr]) while correct do
        if (type(term,`^`) and has(op(1,term),qdilat_set) and
            type(op(2,term),'integer'))
        or member(term,qdilat_set) or not has(term,var_set) then
            next
        else
            correct:=false
        end if
    end do;
    correct

end proc: # `Mgfun/args_type_check_table`[qdilat]


# Check that the type qshift is an integer linear substitution
`Mgfun/args_type_check_table`['qshift']:=proc(expr,var_type_set)
# expr: expression of a function argument which has to be an integer linear substitution
# var_type_set: set of the differents variables with their type i.e. shift,
# diff, qshift, qdilat ....
local correct,term,qshift_set,other_set;
# qshift_set: set of qshift variable
# other_set: set of other variable
option `Copyright (c) 1999-2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
global qshift;
    correct:=true;
    qshift_set:=map2(op,1,select(has,var_type_set,'qshift'));
    other_set:=map2(op,1,remove(has,var_type_set,'qshift'));
    if not has(expr,other_set) then
        for term in `if`(type(expr,`+`),expr,[expr]) while correct do
            correct:=`Mgfun/is_integer_quadratic_term`(term,qshift_set)
        end do
    else
        correct:=false
    end if;
    correct
end proc: # `Mgfun/args_type_check_table`['qshift']


# Check that the type parameter corresponds to an integer
`Mgfun/args_type_check_table`[parameter]:=proc(expr,var_type_set)
local correct;
option `Copyright (c) 1999-2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
global integer;

    if type(expr,'integer') then
        correct:=true
    else
        correct:=false
    end if;
    correct

end proc: # `Mgfun/args_type_check_table`[parameter]


##### Probleme si q a une valeur ; il devrait etre possible de traiter
##### plus de cas de q numeriques (complexes) autres que les racines de
##### l'unite.
`Mgfun/args_type_check_table`[non_root_of_one]:=proc(expr,var_type_set)
option `Copyright (c) 1999-2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
global rational;
    (type(expr,'rational') and expr<>0 and expr^2<>1) or
        (type(expr,'name') and not assigned(expr))
end proc: # `Mgfun/args_type_check_table`[non_root_of_one]


# Test if the expression belongs to the table and is dfinite
`Mgfun/is_dfinite_expr`:=proc(expr,var_type_set)
local correct,f,i,signature;
option `Copyright (c) 1999-2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
    f:=op(0,expr);
    signature:=dfinite_spec_table[f][1];
    # Wrong number of arguments?
    correct:=evalb(nops(expr)=nops(signature));
    # Check the substitution of each argument.
    for i from 1 to nops(expr) while correct do
        correct:=assigned(`Mgfun/args_type_check_table`[signature[i]]) and
           `Mgfun/args_type_check_table`[signature[i]](op(i,expr),var_type_set)
    end do;
    correct
end proc: # `Mgfun/is_dfinite_expr`

# Test if expr is hypergeometric or not
`Mgfun/is_hypergeom_expr`:=proc(expr,var_type_set)
local i,correct;
option `Copyright (c) 1999-2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
global diff,shift;
    correct:=`Mgfun/args_type_check_table`['diff'](op(3,expr),
        var_type_set);
    for i to nops(op(1,expr)) while correct do
        correct:=`Mgfun/args_type_check_table`['shift']
                (op([1,i],expr),var_type_set);
    end do;
    for i to nops(op(2,expr)) while correct do
        correct:=`Mgfun/args_type_check_table`['shift']
                (op([2,i],expr),var_type_set);
    end do;
    correct
end proc: # `Mgfun/is_hypergeom_expr`

# Rewrite exp(.)^. and .^.^. in exp(.) and .^.
`Mgfun/type_checking/normalization`:=proc(expression)
local expr,f;
option `Copyright (c) 1999-2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
global exp;
    expr:=expression;
    f:=op(0,expr);
    if f=`^` and op([1,0],expr)=`^` then
        expr:=`Mgfun/type_checking/normalization`(op([1,1],expr)^(op([1,2],expr)*op(2,expr)))
    elif f=`^` and op([1,0],expr)='exp' then
        expr:=exp(op([1,1],expr)*op(2,expr))
    end if;
    expr
end proc: # `Mgfun/type_checking/normalization`


# When possible, decompose
#    a^(diff,shift,other)  into a^diff*a^shift*a^other
#    exp(diff,shift,other) into exp(diff)*exp^shift*exp(1)^other
# But doesn't check if diff is algebraic or shift is an integer
# substitution.
`Mgfun/type_checking/rewrite_powers`:=proc(expression,var_type_set)
local expr,base,exponent,sub_set,diff_set,qshift_set,shift_set,other_set,diff_expr,qshift_expr,shift_expr,other_expr,i;
option `Copyright (c) 1999-2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
global diff,shift,qshift,exp;
    expr:=`Mgfun/type_checking/normalization`(expression);
    if op(0,expr)='exp' then
        expr:=exp(1)^op(expr);
    end if;

    if op(0,expr)=`^` then
        expr:=applyop(expand,2,expr);
        base:=op(1,expr);
        exponent:=op(2,expr);
        sub_set:=`if`(type(exponent,`+`),{op(exponent)},{exponent});
        other_set:=remove(has,sub_set,map2(op,1,var_type_set));
        diff_set:=remove(has,sub_set,
            map2(op,1,select(has,var_type_set,{'shift','qshift'})))
                minus other_set;
        shift_set:=remove(has,sub_set,
            map2(op,1,select(has,var_type_set,{'diff','qshift'})))
                minus other_set;
        qshift_set:=remove(has,sub_set,
            map2(op,1,select(has,var_type_set,{'diff','shift'})))
                minus other_set;
        if
            nops(sub_set)=nops(other_set)+nops(diff_set)
                +nops(qshift_set)+nops(shift_set)
        then
            diff_expr:=`if`(base='exp',exp(add(i,i=diff_set)),
                base^(add(i,i=diff_set)));
            qshift_expr:=base^(add(i,i=qshift_set));
            shift_expr:=base^(add(i,i=shift_set));
            other_expr:=base^(add(i,i=other_set));
            expr:=diff_expr*qshift_expr*shift_expr*other_expr
        else
            error "expression is not d-finite"
        end if
    end if;
    expr
end proc: # `Mgfun/type_checking/rewrite_powers`

# Rename existing special functions according to names in
# dfinite_spec_table.  Should not do anything unless called on a
# function.
`Mgfun/type_checking/rewrite_spec_functions`:=proc(expression)
local f,expr,i,x,q;
option `Copyright (c) 1999-2009 Frederic Chyzak and Cyril Germa, INRIA, France`;
global binomial,factorial,pochhammer,qbinomial,qfactorial,qpochhammer,GAMMA;

    f:=op(0,expression);
    # Rewrite orthogonal polynomials
    if op(0,f)='orthopoly' then
        # Special case of L(n,x)->L(n,0,x)
        if op(f)='L' then
            expr:=`if`(nops(expression)=2,LaguerreL(op(1,expression),
            0,op(2,expression)),
            LaguerreL(op(expression)))
        # Special case of P(n,x)->P(n,0,0,x)
        elif op(f)='P' then
            expr:=`if`(nops(expression)=2,JacobiP(op(1,expression),
            0,0,op(2,expression)),
            JacobiP(op(expression)))
        elif op(f)='G' then
            expr:=GegenbauerC(op(expression))
        elif op(f)='H' then
            expr:=HermiteH(op(expression))
        elif op(f)='T' then
            expr:=ChebyshevT(op(expression))
              elif op(f)='U' then
            expr:=ChebyshevU(op(expression))
        end if
    # Rewrite functions of q-calculus.
    elif f='qfactorial' then
        # qfactorial(q,n) -> qPOCHHAMMER(n,q,q,1)
        expr:=`Mgfun/qPOCHHAMMER`(op(2,expression),op(1,expression)$2,1)
    elif f='qbinomial' then
        # qbinomial(q,n,m) ->
        #   qPOCHHAMMER(n,q,q,1)*qPOCHHAMMER(m,q,q,-1)*qPOCHHAMMER(n-m,q,q,-1)
        q:=op(1,expression);
        expr:=`Mgfun/qPOCHHAMMER`(op(2,expression),q$2,1)
            *`Mgfun/qPOCHHAMMER`(op(3,expression),q$2,-1)
            *`Mgfun/qPOCHHAMMER`(op(2,expression)-op(3,expression),q$2,-1)
    elif f='qpochhammer' then
        # qpochhammer(q,n,x) -> qPOCHHAMMER(n,x,q,1)
        expr:=`Mgfun/qPOCHHAMMER`(op(2..3,expression),op(1,expression),1)
    # Hypergeometric sequences rewritten in terms of GAMMA.
    elif member(f,{'binomial','factorial','pochhammer','GAMMA'}) then
        expr:=mul(i,i=map(
            z->`if`(op(0,z)=`^`,`Mgfun/GAMMA`(op([1,1],z),op(2,z)),
                `Mgfun/GAMMA`(op(1,z),1)),
            `if`(member(f,{'GAMMA','factorial'}),{convert(expression,'GAMMA')},
                {op(convert(expression,'GAMMA'))})))
    # Whittaker and Kelvin functions rewritten in terms of
    # hypergeometric, Kummer, and Bessel functions.
    elif f='WhittakerM' then
        expr:=((mu,nu,z)->
            exp(-z/2)*z^(nu+1/2)*hypergeom([nu-mu+1/2],[2*nu+1],z))
                (op(expression))
    elif f='WhittakerW' then
        expr:=((mu,nu,z)->
            exp(-z/2)*z^(nu+1/2)*KummerU(nu-mu+1/2,2*nu+1,z))
                (op(expression))
    elif f='KelvinBer' then
        expr:=1/2*(BesselJ(op(1,expression),exp(3*I*Pi/4)*op(2,expression))
            +BesselJ(op(1,expression),exp(-3*I*Pi/4)*op(2,expression)))
    elif f='KelvinBei' then
        expr:=1/2/I*(BesselJ(op(1,expression),exp(3*I*Pi/4)*op(2,expression))
            -BesselJ(op(1,expression),exp(-3*I*Pi/4)*op(2,expression)))
    elif f='KelvinKer' then
        expr:=exp(-op(1,expression)*Pi*I/2)/2*(
            BesselJ(op(1,expression),exp(I*Pi/4)*op(2,expression))
            +BesselJ(op(1,expression),exp(-I*Pi/4)*op(2,expression)))
    elif f='KelvinKei' then
        expr:=exp(-op(1,expression)*Pi*I/2)/2/I*(
            BesselJ(op(1,expression),exp(I*Pi/4)*op(2,expression))
            -BesselJ(op(1,expression),exp(-I*Pi/4)*op(2,expression)))
    elif f='KelvinHer' then
        expr:=1/2*(HankelH1(op(1,expression),exp(3*I*Pi/4)*op(2,expression))
            +HankelH2(op(1,expression),exp(-3*I*Pi/4)*op(2,expression)))
    elif f='KelvinHei' then
        expr:=1/2/I*(HankelH1(op(1,expression),exp(3*I*Pi/4)*op(2,expression))
            -HankelH2(op(1,expression),exp(-3*I*Pi/4)*op(2,expression)))
    # Fresnel auxilliary functions rewritten in terms of Fresnel sine
    # and cosine integrals.
    #
    # A problem is that cosine is not recognized as the derivative of
    # sine, and that both are not recognized as elements of the module
    # generated by FresnelC/FresnelS.  Thus, the order will be too
    # large.
    elif f='Fresnelf' then
        x:=op(expression);
        expr:=(1/2-FresnelS(x))*cos(Pi/2*x^2)-(1/2-FresnelC(x))*sin(Pi/2*x^2)
    elif f='Fresnelg' then
        x:=op(expression);
        expr:=(1/2-FresnelC(x))*cos(Pi/2*x^2)-(1/2-FresnelS(x))*sin(Pi/2*x^2)
    # Legendre polynomials rewritten in terms of Jacobi polynomials.
    elif f='LegendreP' then
        expr:=JacobiP(op(1,expression),0,0,op(2,expression))
    else
        expr:=expression
    end if;
    expr
end proc: # `Mgfun/type_checking/rewrite_spec_functions`

# If expression is dfinite, rewrite the expression in terms of
# IDENTITY, POWER, EXP ...
type_checking:=proc(expression,var_type_set)
local diff_set,shift_set,qshift_set,var_set,correct,expr,f,base,exponent,term,new_form,i,q;
option `Copyright (c) 1999-2009 Frederic Chyzak and Cyril Germa, INRIA, France`;
global radfun,anything,exp,diff,shift,qshift,nonnegint,add,mul,binomial,factorial,pochhammer,GAMMA,integer,`Mgfun/qPOCHHAMMER`;

    diff_set:=map2(op,1,select(has,var_type_set,'diff'));
    shift_set:=map2(op,1,select(has,var_type_set,'shift'));
    qshift_set:=map2(op,1,select(has,var_type_set,'qshift'));
    var_set:=map2(op,1,var_type_set);

    # Rewrite of exp(.)^. and .^.^. , separate diff and shift
    # variables.
    expr:=`Mgfun/type_checking/rewrite_powers`(expression,var_type_set);
    # Rewrite special functions (and power of (q-)factorial, etc) into
    # dfinite_spec_table syntax.
    expr:=`Mgfun/type_checking/rewrite_spec_functions`(expr);
    f:=op(0,expr);
    if not has(expr,var_set) then # Do nothing.
    # Algebraic substitution.
    elif `Mgfun/args_type_check_table`['diff'](expr,var_type_set) then
        expr:=`Mgfun/IDENTITY`(expr)
    # Expression rational in diff and in shift is not rewritten,
    # unless it also depends on other typed variables.
    elif type(expr,'radfun'('anything',diff_set union shift_set)) and
        indets(expr) intersect qshift_set={}
    then # Do nothing.
    # Atomic function (which has to belong to the table).
    elif assigned(dfinite_spec_table[f])
        and `Mgfun/is_dfinite_expr`(expr,var_type_set)
    then # Do nothing.
    # Special case of hypergeometric function, which is not in table.
    # --> hypergeom(shift_list,shift_list,diff)
    elif f=`hypergeom`
        and `Mgfun/is_hypergeom_expr`(expr,var_type_set)
    then # Do nothing.
    # Hyperexponential and hypergeometric cases.  At this stage, we
    # assume that all exp(n+x) have been split into exp(n)*exp(x).
    elif f='exp'
        and `Mgfun/args_type_check_table`['diff'](op(expr),var_type_set)
    then
        # exp(algebraic in diff_set)
        expr:=`Mgfun/EXP`(op(expr))
    elif f=`^` then
        base:=op(1,expr);
        exponent:=op(2,expr);
        # a^(algebraic in diff_set)
        if `Mgfun/args_type_check_table`['diff'](exponent,var_type_set) and
        not has(base,var_set) then
            expr:=`Mgfun/EXP`(ln(base)*exponent)
        # or a^(linear in shift_set) including exp(linear in shift_set)
        elif (type(exponent,linear(shift_set)) and
        not has(exponent,diff_set union qshift_set) and
        not has(base,var_set)) or
        # or (rational in diff)^(integer subs)
        (type(base,'radfun'('anything',diff_set)) and
        not has(base,shift_set union qshift_set) and
        `Mgfun/args_type_check_table`['shift'](exponent,var_type_set)) then
            expr:=`Mgfun/POWER`(exponent,base)
        # a^(at most quadratic in qshift_set)
        elif type(exponent,{'linear','quadratic'}(qshift_set)) and
        not has(exponent,diff_set union diff_set) and
        not has(base,var_set) and
        `Mgfun/args_type_check_table`['qshift'](exponent,var_type_set) then
            expr:=`Mgfun/qPOWER`(exponent,base)
        # (....)^(integer)
        elif type(exponent,'integer') then
            # Positive power of a special function.
            if exponent>0 and assigned(dfinite_spec_table[op(0,
                `Mgfun/type_checking/rewrite_spec_functions`(base))])
            then
                expr:=type_checking(base,var_type_set)^exponent
            # Integer power for (q-)hypergeometric terms.
            elif member(op(0,base),
                {'binomial','factorial','pochhammer','GAMMA'}) then
                expr:=mul(i,i=map(z->if op(0,z)=`^` then
                            `Mgfun/GAMMA`(op(1,op(1,z)),op(2,z)*exponent)
                            else
                            `Mgfun/GAMMA`(op(1,z),exponent)
                            end if,
                    `if`(has(op([1,0],expr),{'GAMMA','factorial'}),
                    {convert(base,'GAMMA')},{op(convert(base,'GAMMA'))})))
            elif op(0,base)='qfactorial' then
                # qfactorial(q,n)^a -> qPOCHHAMMER(n,q,q,a)
                expr:=`Mgfun/qPOCHHAMMER`(op(2,base),op(1,base)$2,exponent)
            elif op(0,base)='qbinomial' then
                # qbinomial(q,n,m) ->
                #   qPOCHHAMMER(n,q,q,1)*qPOCHHAMMER(m,q,q,-1)
                #   *qPOCHHAMMER(n-m,q,q,-1)
                q:=op(1,base);
                expr:=`Mgfun/qPOCHHAMMER`(op(2,base),q$2,exponent)
                    *`Mgfun/qPOCHHAMMER`(op(3,base),q$2,-exponent)
                    *`Mgfun/qPOCHHAMMER`(op(2,base)-op(3,base),q$2,-exponent)
            elif op(0,base)='qpochhammer' then
                # qpochhammer(q,n,x)^a -> qPOCHHAMMER(n,x,q,a)
                expr:=`Mgfun/qPOCHHAMMER`(op(2..3,base),op(1,base),exponent)
            else
                expr:=`Mgfun/NOT_DFINITE`
            end if
        else
            expr:=`Mgfun/NOT_DFINITE`
        end if
    elif f=`+` or f=`*` then
        correct:=true;
        for term in expr while correct do
            new_form[term]:=type_checking(term,var_type_set);
            correct:=evalb(new_form[term]<>`Mgfun/NOT_DFINITE`)
        end do;
        expr:=`if`(correct,
            `if`(f=`+`,'add','mul')(new_form[term],term=expr),
            `Mgfun/NOT_DFINITE`)
    else
        expr:=`Mgfun/NOT_DFINITE`
    end if;

    expr

end proc: # type_checking
