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

## Bounds for numerical evaluation
## Marc Mezzarobba, projet Algorithms, INRIA Paris-Rocquencourt

numeric_bounds := module()

uses LinearAlgebra;

export bound_frobenius_norm, bound_fundamental_solutions, dicho_solve_ineq,
    needed_terms, bound_step_transition_matrix, bound_path_transition_matrix,
    bound_transition_matrix, bound_small_transition,
    bound_fundamental_solutions_exact_point;

bound_frobenius_norm := proc(mat)
    local mymat;
    mymat := map(above_abs, mat);
    Rounding := infinity;
    Norm(mymat, 'Frobenius');
    # · return a rational above instead of a Float?
end proc;

# Input:
#   deq - homogeneous diffeq
#   z0 - an *ordinary* (for the time being) point of deq
# Output:
#   parameters of a majorant series that bounds all fundamental solutions at z0
#   of deq
# Note: see also bounds:-params_given_normaldeq
bound_fundamental_solutions_exact_point := proc(deq, yofz, z0, {wini:=true}, $)
    option cache;
    local y, z, ordeq, kappa, localdeq, localnormaldeq, saved_mode,
        bound_params, validity, headrec, generic_head, k, head, cst, ini;
    userinfo(4, 'gfun', "enter", "point"=z0);
    y, z := getname(yofz);
    ordeq := orddiffeq(deq, yofz);
    localdeq := algebraicsubs(bare_diffeq(deq, z), y=z0+z, yofz);
    kappa, localnormaldeq := bounds:-normalize_diffeq(localdeq, yofz);
    saved_mode := set_mode(numeric_mode);
    bound_params, validity := bound_normal_diffeq(localnormaldeq, yofz);
    ASSERT(kappa > -infinity or bound_params[2] = 0); # κ=-∞ => α=0
    reset_mode(saved_mode);
    # Unlike the pseudocode version in [Mezzarobba and Salvy, 2010],
    # bound_normal_diffeq computes its bound up to a constant factor ('A'). Here
    # we compare the first few terms of the bound (taking into account the
    # normalization by psi(n)) with those of the fundamental solutions at z0 of
    # the original diffeq (not the normalized one!) to find the correct constant
    # part.
    if ordeq = 0 then
        head := 0;
    elif wini then
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
    else
        head := 1;
    end if;
    bound_params := [kappa, op(bound_params)];
    cst := bounds:-find_constant(bound_params, validity, head);
    bound_params := [op(bound_params), cst];
    # Counterproductive in the numeric case?
    #bound_params := bounds:-get_rid_of_P(bound_params, op(yofz));
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
        k = bounds:-tail_bound(op(params), dz, 0, 'deriv'=k),
        k = 0..ordeq-1 )]);
    maj := eval(`simplify/piecewise`(maj));
    B := sqrt(ordeq * add(
        (1/k! * (subs(dz=rad,maj[k]) - subs(dz=0, maj[k])))^2,
        k = 0 .. ordeq-1 ));
    B := eval(`simplify/piecewise`(B));
    Digits := max(0, -ilog10(rad)) + 10;
    _EnvNumGfunExtendEvalrC := ["GAMMA", "Hypergeom"];
    B := above_abs(B, 'rational');
    userinfo(5, 'gfun', sprintf("|z0-%a| < %a, bound ~= %a",
        center, evalf[2](rad), evalf[2](B)));
    B;
end proc:

# See bound_fundamental_solutions_exact_point.  This version is allowed to
# replace z0 by a nearby point of small bit-size before computing the majorant.
# To take this into account, it adjusts params, and returns (in addition to
# params) a change of variable to be done in the majorant series.
bound_fundamental_solutions := proc(deq, yofz, z0, approx_size := Digits, $)
    local center, params, delta, invtransbound, cst, res, K;
    if length(denom(z0)) < approx_size then
        res := [bound_fundamental_solutions_exact_point(deq, yofz, z0), 0];
    else
        # This is intended to yield the same result for consecutive z0 close
        # enough to each other (bit burst), so that the majorants can be
        # remembered.
        center := convert(rndz(z0, prec = approx_size), 'rational', 'exact');
        userinfo(5, 'gfun', "z0"=sprint_small_approx(z0), "center"=center);
        params := bound_fundamental_solutions_exact_point(deq, yofz, center);
        delta := above_abs(z0 - center, 'rational');
        # FIXME: Fails if D(center, delta) contains a singular point of deq,
        # which could happen, e.g., if |z0-sing| << 10^(-approx_size).  However,
        # assumptions of this kind are made in several other places in the code.
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
        cst := rndu(orddiffeq(deq, yofz)/rndz(1-invtransbound));
        cst := convert(cst, 'rational', 'exact');
        params := subsop(-1 = params[-1]*cst, params);
        res := [params, delta];
    end if:
    userinfo(6, 'gfun', "done");
    op(res);
end proc:

# Finds n in [low, high] s.t. fun(n) < epsilon (assuming fun(n) ≥ 0, fun(n) → 0)
dicho_solve_ineq := proc(fun, eps, low := 1, high := infinity, $)
    local mid;
    if low > Settings:-max_series_terms then
        error "about %1 terms or more of some series expansion seem necessary "
            "to reach the required precision: giving up.  (Increase "
            "NumGfun:-Settings:-max_series_terms to proceed.  Increasing "
            "Digits may also give tighter bounds in some cases.)",
            Settings:-max_series_terms;
    end if;
    mid := `if`(high=infinity, 2*low, iquo(low+high,2));
    if high - low <= 2 then
        high;
    elif fun(mid) < eps then
        dicho_solve_ineq(fun, eps, low, mid);
    else
        dicho_solve_ineq(fun, eps, mid, high);
    end if;
end proc:

# Assumes homogeneous diffeq, ordinary point.
#
# FIXME: As usual, this could fail if rad is close enough to the distance to
# the nearest singular point that rounding it makes the bounds infinite.
# This should not happen in practice, but...
needed_terms := proc(deq::hrdeq, yofz::function(name), difforder::nonnegint,
            z0::complexcons, rad::complexcons, epsilon::positive,
            {canonical := true})::nonnegint;
    local bound, num_bound, evalbound, params, zshift, myrad, nt;
    userinfo(6, 'gfun', "called");
    if canonical then
        params, zshift := bound_fundamental_solutions(deq, yofz, z0);
    else
        # useful for testing
        error("not implemented anymore!");  # TODO: put it back!
    end if;
    myrad := evalrCf(rad);
    bound := bounds:-tail_bound(op(params), myrad, n,
        'deriv'=difforder, 'simplify_hgeom'=false, 'zshift'=zshift);
    if type(bound, 'SymbolicInfinity') then
        error "no finite bound for series tail"
    end if;
    # faster numerical evaluation when expr contains large integers
    if hastype(bound, `^`) then bound := convert(bound, 'exp') end if;
    # try a fast but non-rigorous search...
    num_bound := evalf(subs(myrad=evalf(above_abs(rad)), bound));
    evalbound := proc(k) evalf(subs(n=k, num_bound)); end proc:
    userinfo(8, 'gfun', "tail bound computation done, now solving ineq");
    nt := dicho_solve_ineq(evalbound, epsilon/10);
    # ...then check the result rigorously and fallback to slow method if needed
    _EnvNumGfunExtendEvalrC := ["GAMMA", "Hypergeom"];
    if above(eval(bound, n=nt)) > epsilon then
        userinfo(3, 'gfun', "fast tail bound computation failed, falling back "
            "to slow method");
        evalbound := proc(k) above(eval(bound, n=k)); end proc:
        nt := dicho_solve_ineq(evalbound, epsilon);
    end if;
    userinfo(8, 'gfun', "done");
    nt;
end proc:

## Bound transition matrices (in Frobenius norm)

bound_step_transition_matrix := proc(deq, yofz, z0, z1)
    local params, zshift, ordeq, rad, B, k;
    userinfo(6, 'gfun', "called");
    if ancont:-is_regsing_step(z0) or ancont:-is_regsing_step(z1) then
        userinfo(3, 'gfun', sprintf("singular step %a -> %a ignored",
            sprint_small_approx(op(z0)), sprint_small_approx(op(z1))));
        return 1;
    end if;
    _EnvNumGfunExtendEvalrC := ["GAMMA", "Hypergeom"];
    params, zshift := bound_fundamental_solutions(deq, yofz, z0);
    ordeq := orddiffeq(deq,yofz);
    rad := above_abs(z1-z0, 'rational') + zshift; # [Mez2011, Remark 6.7]
    B := evalrC(sqrt(ordeq * add(
        (1/k! * bounds:-tail_bound(op(params), rad, 0, 'deriv'=k))^2,
        k = 0..ordeq-1)));
    B := above_abs(B);
    userinfo(3, 'gfun', sprintf("%a -> %a, bound ~= %a",
        sprint_small_approx(z0), sprint_small_approx(z1), evalf[2](B)));
    B;
end proc:

# Used to be (prior to changes in tail_bound):
#
# |Sum(a_n*z^n/n!^tau)| <= Sum(|a_n·t^n|·|z/t|^n/n!^tau)
#                       <= max(|a_n·t^n|) · Sum(|z/t|^n/n!^tau)
#                       <= Sum(|a_n·t^n|) · Sum(|z/t|^n/n!^tau)
# (The last inequality is not tight, but it could be worse.)
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
