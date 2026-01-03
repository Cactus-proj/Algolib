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

## Bounds for numerical evaluation
## Marc Mezzarobba, projet Algorithms, INRIA Paris-Rocquencourt

numeric_bounds := module()

uses LinearAlgebra;

export bound_frobenius_norm, bound_fundamental_solutions, one_sol_of_ineq,
    needed_terms, bound_step_transition_matrix, bound_path_transition_matrix,
    bound_transition_matrix, bound_small_transition,
    bound_fundamental_solutions_exact_point;

## Floating-point Frobenius norm with directed rounding

bound_frobenius_norm := overload([

    proc(mat::{'Vector'('anything'), 'Matrix'('anything')})
        option overload;
        Rounding := infinity;  # environment variable
        Norm(evalf(mat), 'Frobenius');
        # · return a rational above instead of a Float?
    end proc,

    proc(mat::ndmatrix)
        option overload;
        local num, den;
        UseHardwareFloats := false;  # insufficient exponent range
        num, den := op(mat);
        num := rndu(num);
        den := rndz(den);
        procname(rndu(`/`, num, den));
    end proc

]);


# Le rec*rec fait par normalize_diffeq est coûteux (mais moins, quand même,
# depuis qu'il ne calcule plus les conditions initiales). Comme kappa (<=0)
# ne change pas quand on déplace l'équation, ce serait bien de faire la
# normalisation une fois pour toutes. Problèmes :
# · en appelant normalize_diffeq avec un a formel, on ne calcule pas
#   le bon kappa ;
# · en faisant tout le normalize_diffeq avec le bon kappa formellement,
#   le rectodiffeq fait des expressions mostrueuses ;
# · si on essaie de faire normalize_*rec* en un point générique, puis subs,
#   et diffeqtorec après substitution, ça marche en temps de calcul, mais
#   ça donne des gros coefficients, d'où une borne (un K) très pessimiste.
#
#    rec := diffeqtorec(deq, yofz, u(n));
#    kappa := rec_factorial_growth(rec, u(n));
#    genericdeq := algebraicsubs(deq, y=a+z, yofz);
#    genericrec := diffeqtorec(genericdeq, yofz, u(n));
#    genericnormalrec := normalize_rec_doit(genericrec, u(n), kappa);
#    localnormalrec := subs(a=z0, genericnormalrec);
#    localnormaldeq := rectodiffeq(localnormalrec, u(n), y(z), 'ini'=false, 
#                                                    'homogeneous'=true);


# Input: 
#   deq - homogeneous diffeq
#   z0 - an *ordinary* (for the time being) point of deq
# Output:
#   parameters of a majorant series that bounds all fundamental solutions at z0
#   of deq
# Note: There is some (not that much) code duplication between
#   bound_diffeq_doit, bound_rec_doit, and bound_fundamental_solutions,
#   because I don't know yet how these procedures will evolve or how to
#   factorize it.
bound_fundamental_solutions_exact_point := proc(deq, yofz, z0)
    option cache;
    local y, z, ordeq, kappa, localdeq, localnormaldeq, saved_mode,
        bound_params, thr, headrec, generic_head, k, head, cst, ini;
    userinfo(4, 'gfun', "enter bound_fundamental_solutions", "point"=z0);
    bound_params;
    y, z := getname(yofz);
    ordeq := orddiffeq(deq, yofz);
    localdeq := algebraicsubs(deq, y=z0+z, yofz);
    # this is expensive, but I do not know how to avoid it
    kappa, localnormaldeq := bounds:-normalize_diffeq(localdeq, yofz);
    if kappa = -infinity then
        return [0, 0, 1, 1, [], 1];
    end if:
    saved_mode := set_mode(numeric_mode);
    bound_params, thr := bound_normal_diffeq:-doit(localnormaldeq, yofz);
    reset_mode(saved_mode);
    # To compute the first terms of the fundamental solutions at z0, which are
    # needed to determine the constant, we translate the diffeq *without
    # normalisation*.
    localdeq := algebraicsubs(bare_diffeq(deq, z), y=z0+z, yofz);
    headrec := diffeqtorec(
        { localdeq, seq( (D@@k)(y)(0)=ini[k]/k!, k=0..ordeq-1 ) },
        yofz, u(n));
    generic_head := rectoproc( headrec,
        u(n), 'remember', 'params'=[seq(ini[k], k=0..ordeq-1)]);
    head := proc(n)
        local j;
        max(seq(
            abs( generic_head(n, 0$j, 1, 0$(ordeq-1-j)) ),
            j=0..ordeq-1));
    end proc;
    # 
    bound_params := [kappa, op(bound_params)];
    cst := bounds:-find_constant(bound_params, thr, head);
    bound_params := [op(bound_params), cst];
    bound_params := bounds:-get_rid_of_P(bound_params, op(yofz));
    userinfo(3, 'gfun', "bound on fundamental solutions:", "point"=z0,
        "parameters"=bound_params);
    bound_params;
end proc:

# bound the Frobenius norm (for now; this may not be what I want in the long
# term) of M-Id where M = transition matrix from center to any point at distance
# < rad [experimental version, to be improved]
bound_small_transition := proc(deq, yofz, center, rad)
    local params, ordeq, dz, B, k, maj;
    params := bound_fundamental_solutions_exact_point(deq, yofz, center);
    ordeq := orddiffeq(deq,yofz);
    maj := table([seq(
        k = bounds:-tail_bound(op(params), dz, 0, 'derivative' = k),
        k = 0 .. ordeq-1 )]);
    maj := eval(`simplify/piecewise`(maj));
    B := sqrt(ordeq * add(
        (1/k! * (subs(dz=rad,maj[k]) - subs(dz=0, maj[k])))^2,
        k = 0 .. ordeq-1 ));
    B := eval(`simplify/piecewise`(B));
    Digits := max(0, -ilog10(rad)) + 10;
    B := evalrC(B);
    B := bound_abs_interval(B);
    userinfo(5, 'gfun', sprintf("|z0-%a| < %a, bound ~= %a",
        center, evalf[2](rad), evalf[2](B)));
    B;
end proc:

# See bound_fundamental_solutions_exact_point.  This version is allowed to
# replace z0 to a nearby point of small bit-size before computing the majorant.
# To take this into account, it adjusts params, and returns in addition to it a
# change of variable to be done in the majorant series.
bound_fundamental_solutions := proc(deq, yofz, z0, approx_size := Digits, $)
    local center, params, delta, invtransbound, cst, res, K;
    if length(denom(z0)) < approx_size then
        res := [bound_fundamental_solutions_exact_point(deq, yofz, z0), z->z];
    else
        # this is intended to yield the same result for consecutive z0 close
        # enough to each other (bit burst), so that the majorants can be
        # remembered
        center := convert(rndz(z0, prec = approx_size), 'rational', 'exact');
        userinfo(5, 'gfun', "z0"=z0, "center"=center);
        params := bound_fundamental_solutions_exact_point(deq, yofz, center);
        delta := z0 - center;
        delta := convert(rndu(abs(delta)),'rational', 'exact'); # XXX pas rigoureux
        invtransbound := bound_small_transition(deq, yofz, center, delta);
        if invtransbound > 1/2 then
            # If maj = exp(K/(1-z)), we expect that approx_size should be of the
            # same order of magnitude as K for the condition invtransbound > 1/2
            # to hold; so we try again recursively with this value.
            K := params[4];
            userinfo(5, 'gfun', "bad approx, recursing", "size" = approx_size,
                "K" = K, "bound on M^(-1)" = evalf[2](invtransbound));
            return bound_fundamental_solutions(deq, yofz, z0,
                                            approx_size + max(K, Digits));
        end if;
        cst := rndu(orddiffeq(deq, y(z))/rndz(1-invtransbound));
        cst := convert(cst, 'rational', 'exact');
        params := subsop(-1 = params[-1]*cst, params);
        res := [params, z->(delta+z)];
    end if:
    userinfo(6, 'gfun', "done");
    op(res);
end proc:

one_sol_of_ineq := proc(ineq, x, n0 := 1, $)::posint;
    description "Finds n >= n0 s.t. expr(n) < epsilon (for nonnegative expr tending to 0)";
    local expression,evalexpr,epsilon,dicho,n,s;
    ASSERT(type(ineq, `<`)); ASSERT(type(x, 'name'));
    expression := lhs(ineq);
    # makes numerical evaluation much faster (otherwise too much is done at simplification time)
    if hastype(expression, `^`) then expression := convert(expression, 'exp') end if;
    epsilon := below(rhs(ineq));
    userinfo(5, 'gfun', 'epsilon' = evalf(epsilon));
#    evalexpr := proc(t)
#        local u;
#        # Gives "Error, (in evalr/shake) Not implemented, evalr/hypergeom"
#        #u := upper(shake(subs(x=t,expression)));
#        u := above(subs(x=evalf(t), expression));
#        userinfo(6, 'gfun', 'f'(t) = u);
#        u;
#    end proc:
    dicho := proc(i, j)  # expr(j) always < epsilon (=> OK even if expr non-nonincreasing!)
        local m;
        if j-i <= 2 then j
        else
            m := iquo(i+j,2);
            if evalf(subs(x=m, expression))>epsilon then
		dicho(m,j) 
            else dicho(i,m) 
            end if;
	end if;
    end proc:
    n := n0;
    #while evalexpr(2*n) > epsilon do n := 2*n end do;
    # expression may be a piecewise => use eval, not subs
    # it would be better to use interval arithmetic here, but evalrC does not
    # support hypergeom
    #while bound_abs_interval(evalrC(eval(expression, x=2*n))) > epsilon do
    while evalf(eval(expression, x=2*n)) > epsilon do
        n := 2*n;
        if n > 2^20 then
            error "unable to compute a suitable truncation order for the Taylor "
            "expansion (precision too high?)"
        end if;
    end do;
    n := dicho(n,2*n);
    userinfo(5, 'gfun', "done");
    n;
end proc:

# assumes homogeneous diffeq, ordinary point
needed_terms := proc(deq, yofz, derivation_order, z0, Rad, epsilon,
                {canonical := true})::nonnegint; 
    local bound, params, changevar, nt, rad;
    userinfo(6, 'gfun', "called");
    if canonical then
        params, changevar := bound_fundamental_solutions(deq, yofz, z0);
    else
        # useful for testing
        error("no more implemented!");
    end if;
    rad := rndu(Rad);  # XXX rigoureux ? 
    bound := bounds:-tail_bound(op(params), rad, n,
        'derivative'=derivation_order, 'simplify_hypergeom'=false,
        'transform'=changevar);
    if type(bound, 'SymbolicInfinity') then 
        error "no finite bound for series tail"
    end if;
    userinfo(6, 'gfun', "tail bound computation done, now solving ineq");
    nt := one_sol_of_ineq(bound < epsilon, n, 1);
    userinfo(6, 'gfun', "done");
    nt;
end proc:

## Bound transition matrices (in Frobenius norm)

# La norme de Frobenius est une norme d'algèbre, et majore la norme subordonnée
# à la norme euclidienne (d'après l'inégalité de Cauchy-Schwarz).

bound_step_transition_matrix := proc(deq, yofz, z0, z1)
    local params, changevar, ordeq, dz, B, k;
    userinfo(6, 'gfun', "called");
    params, changevar := bound_fundamental_solutions(deq, yofz, z0);
    ordeq := orddiffeq(deq,yofz);
    dz := abs(evalf(z1-z0));  # FIXME
    B := evalrC(sqrt(ordeq * add(
        (1/k! * bounds:-tail_bound(op(params), dz, 0, 'derivative' = k,
                                                    'transform'=changevar))^2,
        k = 0..ordeq-1)));
    B := bound_abs_interval(B);
    userinfo(3, 'gfun', sprintf("%a -> %a, bound ~= %a", z0, z1, evalf[2](B)));
    above(B); # inutile ?
end proc:

# Version d'avant le tail_bound actuel :
#
# |Sum(a_n*z^n/n!^tau)| <= Sum(|a_n·t^n|·|z/t|^n/n!^tau)
#                       <= max(|a_n·t^n|) · Sum(|z/t|^n/n!^tau)
#                       <= Sum(|a_n·t^n|) · Sum(|z/t|^n/n!^tau)
# (La dernière inégalité n'est pas fine, mais pas monstrueusement large.)
#
#t := 1/2 * abs(evalf(1/alpha));  # arbitrary point where maj converges
#B2 := evalf(
#    sqrt(r * add(
#        1/k! * eval(diff(maj, [z$k]), z=t)^2,
#        k=0..r-1 ))
#    *tail_constant(dz/t, tau));

bound_path_transition_matrix := proc(deq, yofz, path, $)
    option cache;
    local steps, m, B1, B2;
    steps := nops(path) - 1;
    if steps <= 0 or steps = 1 and path[1] = path[2] then
        1
    elif steps = 1 then
        bound_step_transition_matrix(deq, yofz, op(path));
    else
        m := iquo(steps+1, 2);
        B1 := bound_path_transition_matrix(deq, yofz, path[1..m]);
        B2 := bound_path_transition_matrix(deq, yofz, path[m+1..-1]);
        B1 * B2;
    end if;
end proc:

bound_transition_matrix := bound_path_transition_matrix;

end module:
