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

## 'Tight' bounds for holonomic sequences: core functions
## Marc Mezzarobba, projet Algorithms, INRIA Paris-Rocquencourt

numeric_mode :: boolean := false;  # see utilities.mm for how to set/reset

bounds := module()

export common_root_multiplicity, infroot_resultant, longgcd,
    normal_majorant, normal_coeff_bound, tail_bound, tail_bound_finregdom,
    sum_bound_entire_regsing, rec_factorial_growth, normalize_rec_doit,
    normalize_rec, normalize_diffeq, find_constant, find_constant_from_rec,
    get_rid_of_P, bound_diffeq_doit, bound_rec_doit, piecewise_bound, psi,
    params_given_normaldeq;

################################################################################
## Utilities
################################################################################

# These functions are used by the bound computation code (not only in this file)
# but are too specific for utilities.mm.

## Roots & poles

#root_multiplicity := proc(poly, z, alpha, numroots := 
#                [fsolve(poly, z, 'complex')], 
#                remaining_roots := NULL, $)
#    local r, eqr, neqr;
#    r := evalf(alpha);
#    eqr, neqr := selectremove(
#        x -> evalb(evalf(abs(x-r)) < Float(1, 2-Digits)),
#        eval(numroots));
#    if remaining_roots <> NULL then remaining_roots := neqr end if;
#    nops(eqr);
#end proc:

# Maximum multiplicity in 'poly' of a root of 'ref'
common_root_multiplicity := proc(poly, ref, z, $)
    local p, g, mult, sqf;
    p := poly;
    sqf := sqrfreepart(ref, z);
    for mult from 0 do
        g := gcd(p, sqf);
        if degree(g, z) = 0 then break end if;
        p := quo(p, g, z); # cannot use divide (non-rational coefficients)
    end do;
    mult;
end proc:

################################################################################
## Explicit bounds (including human-readable formulae) associated to parameters
################################################################################

# Internally, the bounds are represented by a vector of parameters
# [kappa, T, alpha, K, [P0, P1, ...], A].  Usually κ \in ℝ, α ≠ 0, and the
# parameters correspond to a majorant roughly of the form
#   A·Sum((1/psi(n))·alpha^n·exp(O(n^(T/(T+1)))+int(P0+P1·z+...))·n^(K-1)*z^n).
# See [Mezzarobba and Salvy, 2010] or better [Mezzarobba, 2011, chap. 5] for
# details.  When alpha=0, we have T=P0=P1=...=0 and the other parameters are
# overriden to mean A·(1+z+···+z^K-1).  Needless to say, this representation is
# used for historical reasons only and should change if a significant part of
# the code is to be rewritten.
#
# See also bound_normal_diffeq.mm for more on degenerate cases.

# For absalpha<>0: solution to y' = (absalpha*K*(1-absalpha*z)^(-T-1)+P)*y with
# y(0)=1. For absalpha=0 (degenerate case): 1+z+···+z^(K-1).
normal_majorant := proc(T, absalpha, K, P, z, $)
    local exppol, k;
    ASSERT(signum(0, absalpha, 1) = 1);
    exppol := exp(int(PolynomialTools:-FromCoefficientList(P, z),z));
    if absalpha = 0 then
        ASSERT(type(K, 'integer'), "degenerate case: K must be an integer");
        return add(z^k, k=0..K-1);
    elif T = 0 then
        1/(1-absalpha*z)^K * exppol;
    else
        exp((K/T)/(1-absalpha*z)^T) * exppol;
    end if;
end proc:

# Bound on the coefficient of z^n in the result of the above
normal_coeff_bound := proc(T, absalpha, K, A, P, n, {noexpand:=false}, $)
    local maj, z, bin, k, saddle;
    ASSERT(signum(0, absalpha, 1) = 1);
    if absalpha = 0 then  # degenerate case
        ASSERT(type(K, 'integer'), "degenerate case: K must be an integer");
        simplify(piecewise(n<=K, A, 0), 'piecewise');
    elif T = 0 and P = [] then  # exact expression
        if K < Settings:-binomial_expand_threshold or noexpand then
            # TBI: we should probably keep the binomial form at this point and
            # postopone this kind of stuff to a final simplification phase in
            # symbolic_bounds
            bin := mul(n+k, k=1..K-1)/(K-1)!;
        else
            bin := binomial(n+K-1,K-1);
        end if;
        A * bin * absalpha^n
    else  # saddle-point bound
        maj := A * normal_majorant(T, absalpha, K, P, z);
        saddle := 1/absalpha * (1-(K/(n+K+1))^(1/(T+1)));
        simplify(subs(z = saddle, maj)/saddle^n, 'constant', 'power', 'radical',
                                              'sqrt') assuming n::nonnegint;
    end if;
end proc:

# piecewise(n <= validity, bound_n_leq_val, bound_n_gt_val) with some
# simplifications
piecewise_bound := proc(n, validity, bound_n_leq_val, bound_n_gt_val, $)
    local val, cmp;
    val := eval(validity);
    if indets(val) = {} then val := ceil(above(val)) end if;
    _Envsignum0 = -1;
    cmp := signum(n-val) assuming n::nonnegint;
    if n = 0 or cmp = -1 or Testzero(bound_n_leq_val - bound_n_gt_val) then
        bound_n_leq_val;
    elif cmp = 1 then
        bound_n_gt_val;
    else
        piecewise(n <= val, bound_n_leq_val, bound_n_gt_val);
    end if;
end proc:

# The solution of (n+q)^p*psi(n+q)=psi(n) that we use to reduce to the Gevrey
# class of analytic series with finite radius of convergence.
psi := proc(kappa, n, $)
    local p, q;
    if kappa = -infinity then
        1
    else
        p, q := numer(kappa), denom(kappa);
        q^(-p/q*n) * GAMMA(n/q+1)^(-p);
    end if;
end proc;

# Simpler tail bound when κ = T = 0 (and P = 0).  Assumes deriv=0, but it is
# easy to reduce the general case to deriv=0 when this bound applies.
#
# Two versions of the bound are available depending on the value of
# simplify_hgeom: an expression in terms for 2F1 or an explicit polynomial.
# The hypergeometric form avoids the numerical cancellation that can occur
# when evaluating a polynomial. But the polynomial one is more readable and
# has the benefit that we can use evalrC().
tail_bound_finregdom := proc(absalpha, K, A, r, n, simplify_hgeom, $)
    local z, dummy_alpha_z_n, k, bound;
    ASSERT(signum(0, absalpha, 1) = 1);
    # [Mez2011, Prop. 6.14]. This expression simplifies faster than
    # sum(product(n+k+j,j=1..K-1)*(absalpha*z)^(n+k),k=0..infinity)
    bound := A * binomial(n+K-1,K-1) * dummy_alpha_z_n
                   * Hypergeom([1,n+K],[n+1], absalpha*z);
    if simplify_hgeom then
        # Polynomial form: P(n)·(absalpha·z)^n where P \in Q(absalpha·z)[n]. The
        # hypergeometric function is expanded only for small K (K<=12?).
        bound := subs(
            binomial(n+K-1,K-1)=expand(mul(n+k, k=1..K-1))/(K-1)!,
            bound);
        bound := normal(expand(simplify(bound, 'hypergeom'), 'binomial'));
        bound := subs('hypergeom'='Hypergeom', bound);
    end if;
    subs(dummy_alpha_z_n=(absalpha*r)^n, collect(
        expand(eval(bound, z=r)),
        dummy_alpha_z_n,
        proc(pol)
            if type(n, 'symbol') and type(pol, 'polynom'('rational', n)) then
                collect(pol, n, ratabove)
            else
                factor(pol)
            end if
        end proc));
end proc:

# Special case to get bounds of the form poly(z)*exp(c·z^(-q/p)) (which is both
# tighter and more readable than the form used otherwise) when possible for
# entire functions (κ<0) with T=0. Thanks to Sylvain Chevillard for the idea.
# Does not handle derivatives. [Mez2011, Prop. 6.13]
sum_bound_entire_regsing := proc(p, q, absalpha, K, r, $)
    local z, s, B, cst, u, bound_for_small_n;
    ASSERT(signum(0, absalpha, 1) = 1);
    s := ceil(-(K-1)/p);
    B := ((q-p)*s)^(-p*s)/(s!)^(-p);  # improvable (bound_ratpoly)
    _EnvNumGfunExtendEvalrC := ["GAMMA"];
    cst := max(0, seq(
        ratabove(binomial(u+K-1, K-1) / psi(p/q, u)),
        u = 0..(q*s-1)));
    bound_for_small_n :=
#       add(binomial(u+K-1, K-1) * z^u / ratbelow(psi(p/q, u)), u = 0..(q*s-1))
        cst * `if`(absalpha*r=1, add(z^u, u=0..q*s-1), (z^(q*s)-1)/(z-1))
        + z^(q*s) * B/((K-1)!*q^(-p*s))
            * `if`(absalpha*r=1, add(z^u, u=0..q-1), (z^q-1)/(z-1))
            * exp(-p/q * z^(-q/p));
    bound_for_small_n := subs(z = absalpha*r, bound_for_small_n);
end proc:

# Input: kappa::rational, T::nonnegint, alpha::algebraic, K::nonnegint,
#   A::nonnegative, P polynomial (list of coeffs), r cst or iv or symb, n::name
#   or int, deriv::nonnegint, simplify_hgeom::boolean
# Output: A bound for the tail O(z^n) of the majorant series described by these
#   parameters. See [MezSal2010, Sec. 4.2] or better [Mez2011, Sec. 6.2].
tail_bound := proc(kappa, T, alpha, K, P, A, r, n, { deriv:=0,
            simplify_hgeom:=true, zshift:=0 }, $)
    local p, q, saddle, x, maj, z, dflt_bound, dflt_val, bound, k, rbis,
        u, t, bound_for_small_n, deg, absalpha, cst;
    ASSERT(zshift >= 0);
    absalpha := abs(alpha);
    # Default bound for large n: improved version of [Mez2011, Prop. 6.5] (it
    # turns out that the factor h() in the bound stated there is not necessary).
    # Handles derivatives.
    # Valid for -∞ < κ <= 0 as soon as saddle < 1/α, a condition translated into
    # n >= dflt_val on a case-by-case basis below. It is convenient to
    # exclude α=0 as well.
    # Defined here to allow reusing intermediate results. Our choice of
    # 'saddle' differs slighlty from the one in [Mez2001]: here, we
    # additionally try to ensure that α·saddle > 1/2. (This may or may not be a
    # good idea.)
    if -infinity < kappa and kappa <= 0 and alpha <> 0 then
        p := numer(kappa); q := denom(kappa);
        saddle := 1/absalpha*(1-(K/(n+2^(T+1)*K+1))^(1/(T+1)));
        x := r/saddle;
        maj := A*diff(normal_majorant(T, absalpha, K, P, z), [z$deriv]);
        dflt_bound := 1/psi(kappa, n) * eval(maj, z=saddle+zshift) * x^n;
    end if;
    # Specific bounds. It is up to the code below to use dflt_bound or not
    # depending on whether a simpler/tighter bound is available.
    if T = 0 and P <> [] then
        userinfo(6, 'gfun', "missed simple bound", "P"=P);
    end if;
    if kappa > 0 then
        userinfo(4, 'gfun', "divergent series case");
        infinity;  # We could actually return a finite bound in a few cases.
    elif alpha = 0 then
        # This case is not handled in [MezSal2010, Mez2011].
        userinfo(4, 'gfun', "polynomial case");
        deg := K-1-deriv;
        piecewise_bound(n, deg, (deg+1-n)*A*K^deriv*(r+zshift)^deg, 0);
    elif kappa = 0 then
        userinfo(4, 'gfun', "analytic (non-entire) case");
        if type(r, 'realcons') and below_abs((r+zshift)*alpha) >= 1 then
            WARNING("unable to compute a finite tail bound on this disk, "
                "although the series has a nonzero radius of convergence");
            infinity;
        elif T = 0 and P = [] and zshift = 0 then
            # Reduce to the case deriv=0.
            # v(z)=(1-αz)^(-K) => v^(j)(z)=j!·α^j·(1-αz)^(-K-j)
            cst := A*above_abs(deriv!*alpha^deriv, 'rational');
            tail_bound_finregdom(absalpha, K+deriv, cst, r, n, simplify_hgeom);
        else
            # Validity: [Mez2011, Eq. (6.4)] modified as per 20130611A
            dflt_val := K/(1-absalpha*(r+zshift))^(T+1);
            piecewise_bound(n, dflt_val, eval(maj, z=r+zshift), dflt_bound);
        end if;
    else
        userinfo(4, 'gfun', "entire function case");
        ASSERT(kappa < 0 and kappa > -infinity);
        # Essentially [Mez2011, Eq. (6.5)], up to the slightly different choice
        # of 'saddle', see 20130611A for details. Note that r+zshift may lie
        # outside the disk of cvgce of maj (and thus be > saddle for all n).
        rbis := r + zshift;
        dflt_val := rbis^(q/(-p)) * subs(n=(alpha*rbis)^(q/(-p)), saddle)^(q/p);
        if T = 0 and P = [] and deriv = 0 and zshift = 0 then
            bound_for_small_n := sum_bound_entire_regsing(p, q, absalpha, K, r);
        else # [Mez2011, Prop. 6.11]
            t := 1/(2*above_abs(alpha, 'rational')); # somewhat arbitrary
            bound_for_small_n := eval(maj, z=t)
                             * exp(-p/q * (rbis/t)^(-q/p))
                             * add((rbis/t)^u, u=0..q-1);
        end if;
        piecewise_bound(n, dflt_val, bound_for_small_n, dflt_bound);
    end if;
end proc:

################################################################################
## Parameter computation: reduction of general case to normal case
################################################################################

# Opposite of the slope of the rightmost edge of the Newton polygon;
# -∞ if the polygon has a single vertex (rec b(n)*u(n+k)=0).
rec_factorial_growth := proc(rec, uofn, $)
    local coef, s, ini, kappa;
    coef, s, ini := read_rec(rec, uofn);
    kappa := max(seq(
        ( degree(coef[j+2]) - degree(coef[s+2]) )/(s-j),
        j=0..s-1));
end proc:

# Compute a recurrence relation that cancels the sequences u(n)*psi(n) (see the
# paper), and whose fastest-growing (germs of) solutions are only exponential.
# It would be nice to somehow bypass this step since it takes time and yields
# recurrences with large coefficients...
normalize_rec_doit := proc(rec, uofn, kappa, $)
    local u, n, p, q, auxrec, decrec, cont, ord, normalrec;
    userinfo(5, 'gfun', "starting normalization");
    u, n := getname(uofn);
    p, q := numer(kappa), denom(kappa);
    # Formerly we just did
    #   normalrec := `rec*rec`(rec, (n+q)^p*u(n+q)=u(n), u(n), 'ini'=false).
    # But rec*rec removes common divisors from the polynomial coefficients [so
    # that all solutions of rec may not be sol of `rec*rec`(rec, auxrec)] and
    # possibly does other clever tricks to find a "better" recurrence in
    # special cases, while we need to know exactly what we will get.  The
    # present ad-hoc version may also be faster.
    auxrec := u(n+q)=u(n);
    cont := content(rec_to_recop(rec, u(n), u), u);
    decrec := `rec*rec`(rec, auxrec, u(n), 'ini'=false);
    ASSERT(not type(decrec, 'set'));
    decrec := cont * decrec;
    ord := ordrec(decrec, u(n));
    normalrec := numer(
        eval(decrec,
        u = proc(arg)
            local i;
            mul((arg+q*i), i=1..((n+ord-arg)/q))^(-p)*u(arg)
        end proc));
    userinfo(5, 'gfun', "done");
    collect(normalrec, u);
end proc:

# Peut-être pourrait-on en fait *garder* les conditions initiales dans le cas
# q=1 au moins, histoire que gfun (qui essaie via les conditions initiales de
# faire en sorte que rectodiffeq o diffeqtorec = id) calcule une équa diff
# normalisée plus petite.

# Initial conditions get lost, unless the recurrence is already normalized.
#
# Recs whose Newton polygon is reduced to a single vertex do not satisfy α≠0 (as
# we usually expect from normalized recs), but can still be considered
# normalized for most purposes and are handled by the code for the normalized
# case.  The value of κ is irrelevant.  For now I'm keeping κ=-∞.  This allows
# to distinguish this degenerate case from that of recurrences with constant
# coefficients (for which κ=0 and α=0).  Switching to κ=0 in this case could
# make the code a bit simpler (cf. psi(-∞,n)), though.
normalize_rec := proc(rec, uofn, $)
    local kappa;
    kappa := rec_factorial_growth(rec, uofn);
    if kappa = 0 or kappa = -infinity then
        kappa, rec
    else
        kappa, normalize_rec_doit(rec, uofn, kappa);
    end if;
end proc:

# Compute a `normalized' (see paper + comment above) differential equation deq'
# such that for any generalized series solution sum(u(n)*z^n) of deq and for
# any normalizing sequence psi_n with the right `support' (e.g. prod(lambda+k,
# k=0..n) where lambda \in C is a root of the inditial polynomial of deq), deq'
# cancels the generalized series sum(u(n)*psi(n)*z^n). The `generalized series'
# here may be series of the form sum(u(n)*z^n, n in lambda+N), and
# perhaps/hopefully logarithmic power series too.  Initial conditions get lost.
normalize_diffeq := proc(deq, yofz, $)
    option cache;
    local rec, u, n, kappa, normalrec, normaldeq;
    rec := purediffeqtorec(deq, yofz, u(n));
    kappa, normalrec := normalize_rec(rec, u(n));
    normaldeq := purerectodiffeq(normalrec, u(n), yofz);
    kappa, normaldeq;
end proc:

################################################################################
## Parameter computation: constant part
################################################################################

# Fit initial values of maj and head to deduce a bound for the solution of
# the differential equation
#
# Input:
#   params - bound parameters (kappa, T, alpha, K, P) that define  a majorant
#       series v
#   validity - N such that |u_j| <= v_j for j <= N implies u <| v
#   head - a procedure that computes (upper bounds on) the u_j
# Output:
#   a constant A such that u <| A·v
#
# Note: It might be possible to extend this procedure to handle variables in
# the initial values.
find_constant := proc(params, validity, head, $)
    local kappa, T, alpha, K, P, normalhead, below_alpha, below_maj,
        explicit_maj_head, below_maj_coeff, below_maj_head, mydivabs, cst, z, n;
    kappa, T, alpha, K, P := op(params);
    # 'normalhead' computes rational upper bounds on psi_n·|u_n|
    # (warning: head may return non-rational results; I do not know how to avoid
    # that in general)
    normalhead := proc(n)
        #_EnvNumGfunExtendEvalrC := ["GAMMA"];
        _EnvNumGfunUsePoorMansEvalrC := true;
        above_abs(psi(kappa, n) * head(n))
    end proc:
    # Now compute rational lower bounds on the first terms of psi*v.
    below_alpha := below_abs(alpha, 'rational', 'test_zero');
    below_maj := normal_majorant(T, below_alpha, K, P, z);
    below_maj_coeff := normal_coeff_bound(T,below_alpha,K,1,P, n, 'noexpand');
    # exact taylor() to large orders can be very expensive
    Order := min(Settings:-find_constant_max_expand, validity) + 1;
    explicit_maj_head := taylor(below_maj, z=0);
    below_maj_head := [seq(coeff(explicit_maj_head, z, n), n=0..Order-1),
                       seq(eval(below_maj_coeff), n=Order..validity)];
    # Finally, compare normalhead and below_maj_head to find the constant.
    mydivabs := proc(a, b)
        if a = 0 then 0 else rndu(`/`, a, below_abs(b)) end if;
    end proc;
    cst := max(seq(
        mydivabs(normalhead(n), below_maj_head[n+1]),
        n = 0..validity));
    cst := ratabove(cst);
end proc:

# Note: In the degenerate case b(n)*u(n+k)=0, the multiplicative cst should be
# larger than both the (nonnegative, integer) roots of b(n) and the initial
# values u(0), ..., u(k-1).  The setting of 'validity' takes care of the former
# (both here and in bound_fundamental_solutions), and goodinitvalues of the
# later (here only, since this does not apply to fund. sol.).
find_constant_from_rec := proc(params, validity, rec, uofn, incompl_ini_msg, $)
    local u, n, fmt, ini, head, k, maxini;
    global _C;
    u, n := getname(uofn);
    # Check that the initial values are consistent and introduce as many
    # symbolic initial values as needed to make a complete set. Expensive when
    # rec has large singularities.
    fmt := formatrec([rec, u(n)], u, n, ini);
    ini := `goodinitvalues/rec`(fmt, u, n, ini, false);
    ini := remove(has, ini, n);
    if indets(ini) = {} then
        head := rectoproc(rec, u(n), 'remember');
        find_constant(params, validity, head);
    else
        userinfo(1, 'gfun', incompl_ini_msg);
        _C;
    end if;
end proc:

# Get rid of factors exp(P(z)) (introduced by the `tighter' bounds for rational
# functions) in the majorant series.  I am not sure this is exactly what I want.
#
# This makes use of the following, where
# v = normal_majorant(T, alpha, K, [], z):
# (1) the sequence v_n/alpha^n is nondecreasing;
# (2) if g is a series with nonnegative nondecreasing coefficients and f is an
#     entire function, then g(1)·f(z) is a majorant series of g(z)·f(z).
# Taking g = v(t·z) in (2) with t = 1/alpha or t = K/alpha depending on T yields
# f(z)·v(t·z) <| f(1)·v(t·z), hence f(z/t)·v(z) <| f(1)·v(z), which is a useful
# bound for f = exp(P(t*z)).
get_rid_of_P := proc(params, z, $)
    local kappa, T, alpha, K, P, A, pol, t, logcst, cst;
    kappa, T, alpha, K, P, A := op(params);
    if alpha = 0 then
        return params;
    elif type(A, 'symbol') then
        return subsop(-2=[], params);
    end if:
    pol := int(PolynomialTools:-FromCoefficientList(P, z),z);
    # kludge
    alpha := make_RootOfs_indexed(alpha);
    if op(0, 1/alpha) = abs then alpha := 1/op(1/alpha) end if;
    # Note that pol has nonnegative coefficients.
    t := 1/below_abs(alpha);
    logcst := above_abs(subs(z=t, pol));
    cst := rndu(exp, logcst);  # this assumes that exp honors rounding modes
                               # (perhaps we should use intervals instead)
    userinfo(4, 'gfun',
        "bound parameters before removal of polynomial part" = params,
        "logcst" = evalf(logcst));
    if logcst > Settings:-get_rid_of_poly_thr then
        params
    else
        # For some reason this gives a very pessimistic bound in some cases. But
        # since pol has nonnegative coefficients, we can do the whole
        # computation in floating-point (see above).
        #cst := ratabove(exp(logcst));
        subsop(-2=[], -1=ratabove(cst*A), params);
    end if:
end proc:

################################################################################
## Parameter computation: general case
################################################################################

# Helper used by the next two procs.
params_given_normaldeq := proc(normaldeq, yofz, rec, uofn, kappa,
        incompl_ini_msg, $)
    local params, validity, cst;
    # Majorant without constant factor
    params, validity := bound_normal_diffeq(normaldeq, yofz);
    params := [kappa, op(params)];
    ASSERT(kappa > -infinity or params[2] = 0); # κ=-∞ => α=0
    # Multiplicative constant
    cst := find_constant_from_rec(params, validity, rec, uofn, incompl_ini_msg);
    params := [op(params), cst];
    params := get_rid_of_P(params, op(yofz));
    userinfo(3, 'gfun', "bound parameters: ", params);
    params;
end proc:

# Input:  deq
# Output: Bound parameters kappa, T, alpha, K, P, A
bound_diffeq_doit := proc(deq, yofz, $)
    local rec, u, n, kappa, normaldeq;
    # Check that series solutions exist and prepare constant computation
    try rec := diffeqtorec(deq, yofz, u(n), 'ini'=true)
    catch "no valid initial conditions":
        error "no power series solution"
    end try;
    kappa, normaldeq := normalize_diffeq(deq, yofz);
    params_given_normaldeq(normaldeq, yofz, rec, u(n), kappa,
        "Incomplete initial conditions: result valid up to some constant, "
        "for all *power series* solutions.");
end proc:

# Input:  rec
# Output: Bound parameters kappa, T, alpha, K, P, A
bound_rec_doit := proc(rec, uofn, $)
    local deq, y, z, kappa, normalrec;
    kappa, normalrec := normalize_rec(rec, uofn);
    deq := rectodiffeq(normalrec, uofn, y(z), 'ini'=false, 'homogeneous'=true);
    params_given_normaldeq(deq, y(z), rec, uofn, kappa,
        "Incomplete initial conditions: result valid up to some constant.");
end proc:

end module:


