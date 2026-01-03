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

## Parameter computation for majorant series (normal case)
## Marc Mezzarobba, projet Algorithms, INRIA Paris-Rocquencourt

# Compute the parameters of a majorant for the solution of deq specified by
# deq. The differential equation is assumed to be normalized.  The
# code actually works in the following degenerate cases where α=0:
# · Deq independent on z when written on (z,θ): Q(θ)·y(z)=0 (→ Q(n)·u(n)=0,
#   ← Q(n)·u(n+k)=0 after simplification); κ=-∞ with my current conventions;
# · Order 0: p(z)·y(z)=0 (rec with cst coeffs, valid on the relative integers);
#   could be considered normalized but simplifies to just y(z)=0, which is of
#   the previous form except that κ=0 when p≠0 (unless we choose to simplify
#   the rec before computing κ). Note that normalization can result in
#   equations of this form, i.e. they can also occur with a finite nonzero κ.

# TODO: Extend to allow computing a bound on [N,∞) only?

bound_normal_diffeq := module()

local n, y, z, ordeq;  # names shared by all functions

export
    # to ease debug
    rewrite_diffeq, bound_coefficients, reduce_order, fit_initial_values, doit,
    # really public
    ModuleApply;

# Rewrite deq as a polynomial in (z,theta) and isolate the coefficient of z^0
rewrite_diffeq := proc(deq, $)
    local p, k, rat, c, a, theta;
    p := thetadeq(deq, y(z));
    for k from 0 to ordeq do
        rat := normal(p[k]/p[ordeq]);
        if eval(denom(rat), z=0) = 0 then
            error "irregular singular points are not supported"
        end if;
        c[k] := eval(rat, z=0);
        a[k] := normal((rat - c[k])/z);
    end do;
    userinfo(5, 'gfun',
        sprintf("differential equation rewritten as (%a)y = z*(%a)y",
        sort(add(c[k]*theta^k, k=0..ordeq),theta),
        add(a[k].theta^k,  k=0..ordeq-1) ));
    a, c;
end proc:

# Compute bounds for the rational functions a[k]
bound_coefficients := proc(a, $)
    local lcden, mu, mult, inffactors, refpoly, alpha, T, k, M, P, MM, PP, j,
        t, degPP, PPjk;
    lcden := lcm(seq(denom(a[k]),k=0..ordeq-1));
    # mult is not used
    mu, mult, inffactors := op(dominant_root(lcden, z, 'inffactors'));
    if ordeq = 0 then return mu, 0, 0, [] end if;
    if numeric_mode then
        # Make sure that mu is smaller than all other *approximate* roots of
        # the denominators (as needed by bound_ratpoly)
        mu := (1-10^(-Digits_plus(-2)))*mu;
        mu := convert(rndz(abs(mu)), rational, exact);
    end if;
    refpoly := `*`(op(inffactors));
    T := max(0, seq(
        bounds:-common_root_multiplicity(denom(a[k]), refpoly, z) - ordeq + k,
        k = 0..ordeq-1) );
    alpha := 1/mu;  # WARNING: alpha need not be real!
    userinfo(3, 'gfun', "abs(alpha)" = evalf[5]('abs'(alpha)), "irreg" = T);
    for k from 0 to ordeq-1 do
        M[k], P[k] := bound_ratpoly:-doit(a[k], z, alpha, T+ordeq-k);
        P[k] := PolynomialTools:-FromCoefficientList(P[k], z)
    end do;
    MM := max(seq(M[k]/binomial(ordeq-1,k), k=0..ordeq-1));
    ASSERT(type(MM, 'rational'));
    if type(alpha, 'rational') then  # TBI
        t := abs(alpha)
    else
        t := below_abs(make_RootOfs_indexed(alpha));
    end if;
    # 'New' formula for majorants with polynomial parts.  We need
    # a <| binom(r-1,k) * (MM*(1-α)^(r-k+T) + sum((j+1)^(r-k)*P_j*z^j, n>=0).
    # There is an opportunity to absorb part of the P^[k] in
    # (MM-M[k]/binom(...))*(1-α)^(r-k+T).
    degPP := max(seq(degree(P[k]), j=0..ordeq-1)); # max w/o seq is too clever
    PPjk := proc(j, k)
        ( binomial(j+ordeq-k+T-1, j) * t^j  # <= [z^j] (1-α)^(r-k+T)
                    * (M[k] - binomial(ordeq-1, k)*MM)
          + coeff(P[k], z, j) )
        / binomial(ordeq-1, k) * j^(r-k);
    end proc:
    PP := [seq( max(0, seq(PPjk(j, k), k=0..ordeq-1)), j=0..degPP)];
    mu, T, MM, PP;
end proc:

# Compute K,N s.t. n>=N => K·q(n) >= M·mu·n^ordeq, in order to bound the
# diffeq by one of order 1.
#
# With qrest(n) >= |Q(n)-n^r| and K0 >= M·mu, it is enough to have
# qrest(n) <= (1-K0/K)·n^r.
#
# NOTE: this seems quite pessimistic for differential equations with large
# rational coefficients (such as, typically, those obtained by algebraicsubs +
# normalize_diffeq).
#
# TODO: handle generalized series at regular singular points
reduce_order := proc(c, mu, M, $)
    local qrest, K0, K, lfac, N;
    qrest := add(above_abs(c[k], 'rational')*n^k, k=0..ordeq-1);
    if M = 0 then # degenerate case, see comment above
        # The condition on N reduces to Q(N) > 0.
        ASSERT(mu = infinity);
        K := 0; lfac := 1;
    else
        K0 := M * above_abs(mu, 'rational');
        # Heuristic to try to keep N reasonable.  All we really need is K>K0
        # (K=K0 would be okay for qrest=0).
        K := ceil(K0 * (2 + abs(evalf(coeff(qrest, n, ordeq-1))/50)));
        lfac := 1-K0/K;
    end if;
    N := ordeq;
    _Envsignum0 := 0;  # not 1 => equality below is ok
    while signum(abs(subs(n=N, qrest)) - lfac * N^ordeq) = 1 do
        N := 2*N;
        if N > Settings:-max_indicial_eq_tail_index then
            error "unable to compute a reasonable bound "
                "(increase Settings:-max_indicial_eq_tail_index to proceed)";
        end if;
    end do;
    userinfo(5, 'gfun', "M" = M, "K" = K, "N" = N);
    if K > Settings:-max_bound_exponent then
        error "unable to compute a reasonable bound "
            "(increase Settings:-max_bound_exponent to proceed)";
    end if;
    K, N;
end proc:

# Input: deq (Note: 'numeric' is not used explicitly but forces cache table
#   entries to take into account 'numeric_mode'; see ModuleApply below)
# Output: parameters of a majorant series 'maj' and threshold 'validity' such
#   that, IF abs([z^n](sol of deq)) <= A*[z^n](maj) for n <= validity, THEN
#   sol <| A*maj
doit := proc(deq, yofz, numeric, $)
    option cache;
    local a, c, mu, T, M, P, K, validity, below_maj, A, alpha, my_alpha, maj,
        trailing_zeroes;
    userinfo(5, 'gfun', "deq" = deq);
    # module-global, used by some fns we call
    y,z := getname(yofz);
    ordeq := orddiffeq(deq, yofz);
    a, c           := rewrite_diffeq(deq);
    mu, T, M, P    := bound_coefficients(a);
    K, validity    := reduce_order(c, mu, M);
    alpha := 1/mu;
    if alpha = 0 then  # degenerate case
        ASSERT(T=0); ASSERT(K=0); ASSERT(type(P, 'list(0)'));
        K := validity;
        # Note that we may not set validity=0, since we will use validity to
        # choose the multiplicative constant A.
    end if;
    for trailing_zeroes to nops(P) while P[-trailing_zeroes] = 0 do end do;
    P := P[1..-trailing_zeroes];
    maj := bounds:-normal_majorant(T, abs(evalf(alpha)), K, P, z);
    userinfo(4, 'gfun', 'majorant' = evalf[5](maj));
    [T, alpha, K, P], validity;
end proc:

# Wrapper around doit to make cache table entries depend on numeric_mode.
ModuleApply := proc(deq, yofz, $)
    # NO 'option cache' here
    doit(deq, yofz, numeric_mode);
end proc:

end module:

