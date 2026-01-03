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

## 'Tight' bounds for holonomic sequences: core functions
## Marc Mezzarobba, projet Algorithms, INRIA Paris-Rocquencourt

numeric_mode :: boolean := false;  # see utilities.mm for how to set/reset

bounds := module()

global Coeftayl, `value/Coeftayl`;

export common_root_multiplicity, infroot_resultant, longgcd, 
    normal_majorant_series_formula, normal_coeff_formula, tail_bound,
    normal_type, rec_factorial_growth, normalize_rec_doit, normalize_rec,
    normalize_diffeq, make_above_psi, find_constant, find_constant_from_rec,
    get_rid_of_P, bound_diffeq_doit, bound_rec_doit;

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

common_root_multiplicity := proc(poly, ref, z, $)
    local p, g, mult;
    p := poly;
    for mult from 0 do
        g := gcd(p, ref);
        if degree(g, z) = 0 then break end if;
        p := quo(p, g, z); # can't use divide (non-rational coefficients)
    end do;
    mult;
end proc:

## Inert Taylor coefficients

# serait-il possible de faire value/NumGfun:-Coeftayl ?
`value/Coeftayl` := proc(expr, eqn, k)
    if type(k, 'nonnegint') then
        return(coeftayl(args))
    elif type(k, 'name') and type(expr, 'ratpoly') and rhs(eqn)=0 then
        return(ratpolytocoeff(expr, lhs(eqn), k));
    end if;
    return(Coeftayl(args))
end proc:

################################################################################
## Explicit bounds (including human-readable formulae) associated to parameters
################################################################################

# Solution to y' = (alpha*K*(1-alpha*z)^(-T-1)+P)*y with y(0)=1
normal_majorant_series_formula := proc(T, alpha, K, P, z, $)
    local exppol;
    exppol := exp(int(PolynomialTools:-FromCoefficientList(P, z),z));
    if alpha = 0 then
        exppol;
    elif T = 0 then
        1/(1-alpha*z)^K * exppol;
    else
        exp((K/T)/(1-alpha*z)^T) * exppol;
    end if;
end proc:

# Coeff of z^n in the solution above
normal_coeff_formula := proc(T, alpha, K, A, P, n, $)
    local maj, z, bin, k;
    if T = 0 and P = [] then
        if K < Settings:-binomial_expand_threshold then
            bin := expand(mul(n+k, k=1..K-1));
        else
            bin := binomial(n+K-1,K-1);
        end if;
        A * bin * alpha^n
    else
        maj := A * normal_majorant_series_formula(T,alpha,K,P,z);
        value(Coeftayl(maj, z=0, n));
    end if;
end proc:

tail_bound := module()

export simplify_validity, psi, tail_bound, ModuleApply;

simplify_validity := proc(expr)
    if indets(expr) = {} then
        ceil(evalf(expr)+.5)  # FIXME
    else
        expr
    end if;
end proc:

psi := proc(kappa, n)
    local p, q;
    p, q := numer(kappa), denom(kappa);
    q^(-p/q) * GAMMA(n/q+1)^(-p);
end proc;
 
# Input: kappa::extended_rational, T::nonnegint, alpha>0, K::nonnegative,
#   A::nonnegative, P polynomial, r cst or formal, n::name,
#   derivative::nonnegint, simplify_hypergeom::boolean
# Output: A bound for the tail O(z^n) of the majorant series described by these
#   parameters. See [preprint v2], §4.2.
tail_bound := proc(kappa, T, alpha, K, P, A, r, n,
            {derivative:=0, simplify_hypergeom:=true, transform:=(z->z)}, $)
    local p, q, saddle, x, h, maj, z, default_bound, default_val, bound, k,
        u, t, s, B, bound_for_small_n;
    # some parameters used by various bounds, starting with default_bound
    p := numer(kappa); q := denom(kappa);
    # saddle ~ (1-(K/n)^.../alpha; compromise between maj(saddle) and
    # 1/(1-saddle) or r/saddle; here we try to ensure that alpha·saddle > 1/2
    # even for small n, which may or may not be a good idea.
    saddle := 1/alpha * (1 - (K/(n+2^(T+1)*K+1))^(1/(T+1)));  
    x := r/saddle;
    h := 1/(1-x^q/(n+q)^(-p)) * add(x^u, u=0..q-1);
    maj := diff(
        A * normal_majorant_series_formula(T, alpha, K, P, z),
        [z $ derivative]);
    maj := subs(z=transform(z), maj);
    # This bound is correct for -infinity < kappa <= 0 as soon as
    # n >= default_val (computed below). It is up to the code below to use it
    # or not depending on whether a simpler/tighter bound is available.
    default_bound := 1/psi(kappa, n) * eval(maj, z=saddle) * x^n *  h;
    if kappa > 0 then # divergent series
        if r = 0 or A = 0 then bound := A else bound := infinity end if;
    elif kappa = -infinity then # polynomial (FIXME: to be improved)
        piecewise(
            n >= simplify_validity(K+1), 0,
            infinity);
    elif kappa = 0 then # finite radius of convergence
        if type(r, 'realcons') and evalf(1/alpha-r) < -Float(1, 2-Digits) then
            error "divergent sum (1/alpha=%1, r=%2)", 1/alpha, r
        end if;
        if T = 0 and P = [] then 
            # this form simplifies faster than
            # sum(product(n+k+j,j=1..K-1)*(alpha*z)^(n+k),k=0..infinity)
            bound := A * binomial(n+K-1,K-1) * (alpha*z)^n
                     * hypergeom([1,n+K],[n+1],alpha*z);
            if simplify_hypergeom then
                # this returns P(n)·(alpha·z)^n where P \in Q(alpha·z)[n], which
                # is usually more readable than the above version but leads to
                # cancellations when evaluated
                bound := normal(expand(simplify(bound))); # expand/binomial
            end if;
            eval(bound, z=r);
        else
            default_val := simplify_validity( K/(1-alpha*r)^(T+1) );
            piecewise(
                n >= default_val, default_bound,
                eval(maj, z=r));
        end if;
    else # entire function
        # r may be outside the disk of convergence of maj (and thus > saddle for
        # all n)
        default_val := ( alpha*r / subs(n=(alpha*r)^(-q/p), saddle) )^(-q/p);
        if T = 0 and transform = (z -> z) then
            # special case to get bounds of the form poly(z)*exp(c·z^(-q/p))
            # (which is both tighter and more readable than the form used
            # otherwise) when possible
            s := ceil(-(K-1)/p);
            B := ((q-p)*s)^(-p*s)/(s!)^(-p);  # improvable (bound_ratpoly)
            bound_for_small_n :=
                add( binomial(u+K-1, K-1) * z^u / ratbelow(psi(kappa, u)),
                    u = 0..(q*s-1) ) +
                z^(q*s) * B/((K-1)!*q^(-p*s)) * add(z^u, u=0..q-1) *
                    exp(-p/q * z^(-q/p));
            bound_for_small_n := subs(z = alpha*r, bound_for_small_n);
        else
            t := convert(1/(2*evalf(alpha)),'rational', 'exact'); # TBI!!!
            bound_for_small_n := eval(maj, z=t) ### problème quand transform 
                                                ### non trivial ?
                             * exp(-p/q * (r/t)^(-q/p))
                             * add((r/t)^u, u=0..q-1);
        end if;
        if n = 0 or signum(n - default_val) =-1 then
            bound_for_small_n  # avoid unnecessary piecewise
        else
            piecewise(
                n > default_val, default_bound,
                bound_for_small_n);
        end if:
    end if;
end proc:

ModuleApply := tail_bound;

end module:

################################################################################
## Parameter computation for majorant series: reduction of general case to
## normal case
################################################################################

normal_type := proc(rec, uofn, $)
    description "Tests whether a recurrence equation is normalized.";
    local coef, s, ini;
    coef, s, ini := read_rec(rec, uofn);
    evalb( degree(coef[s+2]) = max(seq(degree(coef[j+2]),j=0..s-1)) );
end proc:

rec_factorial_growth := proc(rec, uofn, $)
    local coef, s, ini;
    coef, s, ini := read_rec(rec, uofn);
    # opposite of the slope of the rightmost edge of the Newton polygon;
    # -infinity if the polygon is reduced to a point (rec u(n+k)=0)
    max(seq(
        ( degree(coef[j+2]) - degree(coef[s+2]) )/(s-j),
        j=0..s-1));
end proc:

normalize_rec_doit := proc(rec, uofn, kappa, $)
    local u, n, p, q, psirec, normalrec;
    userinfo(4, 'gfun', "starting normalization");
    u, n := getname(uofn);
    p, q := numer(kappa), denom(kappa);
    psirec := (n+q)^p*u(n+q)=u(n);
    # this takes time and increases both the order and the height of the
    # recurrence, but I don't know how to avoid it
    normalrec := `rec*rec`(rec, psirec, u(n), 'ini'=false);
    if type(normalrec, set) then 
        normalrec := op(select(has, normalrec, n))
    end if;
    userinfo(4, 'gfun', "done");
    normalrec;
end proc:

# Initial conditions get lost, unless the recurrence is already normalized.
normalize_rec := proc(rec, uofn, $)
    local kappa;
    kappa := rec_factorial_growth(rec, uofn);
    if kappa = 0 or kappa = -infinity then
        # it might be possible to avoid treating the case kappa=-infinity
        # separately in all bound_* by doing something clever here
        kappa, rec
    else
        kappa, normalize_rec_doit(rec, uofn, kappa);
    end if;
end proc:

normalize_diffeq := proc(deq, yofz, $)
    option cache;
    local rec, u, n, kappa, normalrec, normaldeq;
    rec := diffeqtorec(deq, yofz, u(n));
    if normal_type(rec, u(n)) then
        return 0, bare_diffeq(deq, yofz)
    end if;
    # initial conditions get lost at this point...
    kappa, normalrec := normalize_rec(rec, u(n));
    # ...ini=false here prevents gfun to reintroduce symbolic initial conditions
    # when the recurrence normalrec is singular
    normaldeq := rectodiffeq(normalrec, u(n), yofz, 'ini'=false,
                                                    'homogeneous'=true);
    kappa, normaldeq;
end proc:

# Returns a procedure that computes terms of a *rational* sequence 'above_psi'
# such that above_psi(n) >= psi(n) for all n, where psi is our usual
# normalisation factor
make_above_psi := proc(kappa, $)
    local p, q, psirec, above_psi_ini, psi, i;
    p,q := numer(kappa), denom(kappa);
    psirec := (n+q)^p*psi(n+q)=psi(n);
    above_psi_ini := { seq(
        psi(i) = ratabove(GAMMA(i/q+1)^(-p) * q^(-p/q*i)),
        i=0..q-1 )};
    rectoproc({psirec} union above_psi_ini, psi(n), 'remember');
end proc:

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
find_constant := proc(params, validity, head)
    local
        kappa, T, alpha, K, P,
        above_psi, normalhead,
        below_alpha, below_maj, below_maj_head,
        mydiv, cst;
    kappa, T, alpha, K, P := op(params);
    # 'normalhead' computes rational upper bounds on psi_n·|u_n|
    # (warning: head may return non-rational results; I do not know how to avoid
    # that in general)
    above_psi := make_above_psi(kappa);
    normalhead := proc(n) above_psi(n) * ratabove(abs(head(n))) end proc:
    # Now compute rational lower bounds on the first terms of psi*v.
    below_alpha := 1/ratabove(1/abs(alpha));  # est-ce le bon sens ?
    below_maj := normal_majorant_series_formula(T, below_alpha, K, P, z);
    Order := validity+1;
    below_maj_head := taylor(below_maj,z=0);
    # Finally, compare normalhead and below_maj_head to find the constant.
    # (With a symbolic max below we should be able to bound equations with
    # variables in initial values.)
    mydiv := proc(a, b)
        if a = 0 then 0 else a/b end if;
    end proc;
    seq(
        [head(n), above_psi(n), normalhead(n), coeff(below_maj_head,z,n)],
        n = 0..validity);
    cst := max(seq(
        mydiv(normalhead(n), ratbelow(coeff(below_maj_head,z,n))),
        n = 0..validity));
    cst := ratabove(cst);
end proc:

# j'aimerais ici remplacer ma récurrence par une rec à coefficients
# rationnels qui la majore, mais je ne vois pas bien comment faire.
find_constant_from_rec := proc(params, validity, rec, uofn, 
                            incomplete_inicond_msg)
    local u, n, fmt, ini, head;
    u, n := getname(uofn);
    fmt := formatrec([rec, u(n)], u, n, ini);
    ini := `goodinitvalues/rec`(fmt, u, n, ini, false);  # ???
    if indets(remove(has, ini, n)) = {} then 
        head := rectoproc(rec, u(n), 'remember');
        find_constant(params, validity, head);
    else
        userinfo(1, 'gfun', incomplete_inicond_msg);
        _C;
    end if;
end proc:


# Get rid of factors exp(P(z)) (introduced by the `tighter' bounds for rational
# functions) in the majorant series.  I am not sure this is exactly what we
# want.
#
# This makes use of the following, where
# v = normal_majorant_series_formula(T, alpha, K, [], z):
# (1) for T = 0, the sequence (K/alpha)^n*v_n is nondecreasing;
# (2) for T > 0, the sequence v_n/alpha^n is nondecreasing;
# (3) if g is a series with nonnegative nondecreasing coefficients and f is an 
#     entire function, then g(1)·f(z) is a majorant series of g(z)·f(z).
# Taking g = v(t·z) in (3) with t = 1/alpha or t = K/alpha depending on T yields
# f(z)·v(t·z) <| f(1)·v(t·z), hence f(z/t)·v(z) <| f(1)·v(z), which is a useful
# bound for f = exp(P(t*z)).
#
# The coefficients of P are typically rather small, but f(1) can be very large
# if K is large, hence the condition on logcst (FIXME: ...which should be
# improved, possibly by introducing a new parameter in NumGfun:-settings).
get_rid_of_P := proc(params, z)
    local kappa, T, alpha, K, P, A, pol, t, logcst, cst;
    kappa, T, alpha, K, P, A := op(params);
    if type(A, 'symbol') then
        return subsop(-2=[], params);
    end if:
    pol := int(PolynomialTools:-FromCoefficientList(P, z),z);
    # The following floating-point computations induce no cancellation and
    # yield upper bounds on the exact result.
    Rounding := infinity;
    t := evalf(`if`(T=0, K/alpha, 1/alpha));
    logcst := eval(pol, z=t);
    cst := exp(logcst);
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


# Input:  deq
# Output: Bound parameters kappa, T, alpha, K, P, A
# Note: There is some (not that much) code duplication between
#   bound_diffeq_doit, bound_rec_doit and bound_fundamental_solutions. This is
#   because I don't know yet how these procedures will evolve or how to factor
#   it.
bound_diffeq_doit := proc(deq, yofz)
    local rec, u, n, kappa, normaldeq, params, validity, cst;
    # Check that series solutions exist and prepare constant computation
    try rec := diffeqtorec(deq, yofz, u(n))
    catch "no valid initial conditions":
        error "no power series solution"
    end try;
    # Majorant (without constant factor)
    kappa, normaldeq := normalize_diffeq(deq, yofz);
    params, validity := bound_normal_diffeq(normaldeq, yofz);
    # Constant factor
    params := [kappa, op(params)];
    cst := find_constant_from_rec(params, validity, rec, u(n),
        "Incomplete initial conditions: result valid up to some constant, "
        "for all *power series* solutions.");
    params := [op(params), cst];
    params := get_rid_of_P(params, op(yofz));
    userinfo(3, 'gfun', "bound parameters: ", params);
    params;
end proc:


# Input:  rec
# Output: Bound parameters kappa, T, alpha, K, P, A
# Note: There is some (not that much) code duplication between
#   bound_diffeq_doit, bound_rec_doit and bound_fundamental_solutions. This is
#   because I don't know yet how these procedures will evolve or how to factor
#   it.
bound_rec_doit := proc(rec, uofn)
    local kappa, normalrec, deq, params, validity, cst, y, z;
    kappa, normalrec := normalize_rec(rec, uofn);
    if kappa = -infinity then  # trivial rec ("u(n+k)=0" for some k)
        params := [0, 0, 1, 1, []];
        validity := ordrec(rec, uofn);
    else
        deq := rectodiffeq(normalrec, uofn, y(z), 'ini'=false,
                                                'homogeneous'=true);
        params, validity := bound_normal_diffeq(deq, y(z));
        params := [kappa, op(params)];
    end if;
    cst := find_constant_from_rec(params, validity, rec, uofn, 
        "Incomplete initial conditions: result valid up to some constant.");
    params := [op(params), cst];
    params := get_rid_of_P(params, op(yofz));
    userinfo(3, 'gfun', "bound parameters: ", params);
    params;
end proc:

end module:


