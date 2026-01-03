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

## Parameter computation for majorant series (normal case)
## Marc Mezzarobba, projet Algorithms, INRIA Paris-Rocquencourt

# Compute the parameters of a majorant for the solution of deq specified by deq.
# See normal_majorant_series_formula for the meaning of these parameters.

# · Pourquoi cette fonction ne renverrait pas aussi une borne valable à partir
# d'un certain rang ?... (c'est moins utile que pour les fractions rationnelles,
# certes)
# · Souci : la taille des équations à borner grossit méchamment pour bit burst.

bound_normal_diffeq := module()

local n, y, z, ordeq;  # names shared by all functions

export
    # to ease debug
    rewrite_diffeq, bound_coefficients, reduce_order, fit_initial_values,
    # really public
    doit;

# Rewrite deq as a polynomial in (z,theta) and isolate the coefficient of z^0
rewrite_diffeq := proc(deq, $)
    local p, k, rat, c, a, theta;
    p := thetadeq(deq, y(z));
    for k from 0 to ordeq do
        rat := normal(p[k]/p[ordeq]);
        if eval(denom(rat), z=0) = 0 then
            error "0 should be a regular point of the differential equation"
        end if;
        c[k] := eval(rat, z=0);
        a[k] := normal((rat - c[k])/z);
    end do;
    userinfo(5, 'gfun',  sprintf("differential equation rewritten as %a",
        add(c[k]*theta^k, k=0..ordeq).y = add(a[k].theta^k,  k=0..ordeq-1).y ));
    a, c;
end proc:

# Compute bounds for the rational functions a[k]
bound_coefficients := proc(a, $)
    local lcden, mu, mult, inffactors, refpoly, alpha, T, k, M, P, jj;
    lcden := lcm(seq(denom(a[k]),k=0..ordeq-1));
    # mult is not used
    mu, mult, inffactors := op(dominant_root(lcden, z, 'inffactors'));
    if numeric_mode then
        # this is to make sure that mu is smaller than all other *approximate*
        # roots of the denominators (as needed by bound_ratpoly)
        mu := (1-10^(-Digits_plus(-2)))*mu;
        mu := convert(rndz(abs(mu)), rational, exact);
    end if;
    refpoly := `*`(op(inffactors));
    T := max(0, seq(
        bounds:-common_root_multiplicity(denom(a[k]), refpoly, z) - ordeq + k,
        k = 0..ordeq-1) );
    alpha := 1/mu;  # WARNING: alpha need not be real!
    userinfo(3, 'gfun', 'abs'('alpha') = evalf[5]('abs'(alpha)), 
        'irregularity' = T);
    for k from 0 to ordeq-1 do
        M[k], P[k] :=  bound_ratpoly:-doit(a[k], z, alpha, T+ordeq-k);
        P[k] := PolynomialTools:-FromCoefficientList(P[k], z)
    end do;
    M := max(seq(M[k]/binomial(ordeq-1,k), k=0..ordeq-1));
    ASSERT(type(M, 'rational'));
    P := [seq(
        max(0, seq(
            coeff(P[k], z, jj) / (binomial(ordeq-1,k)*(jj+1)^(ordeq-1-k)),
            k = 0 .. ordeq-1)),
        # max is too clever, using it directly causes an error
        jj = 0 .. max(seq(degree(P[k]), k=0..ordeq-1)))];
    mu, T, M, P;
end proc:

# Compute K,N s.t. n>=N => K·q(n) >= M·mu·n^ordeq, in order to bound the
# diffeq by one of order 1.
#
# FIXME: this seems very pessimistic for differential equations with large
# rational coefficients (such as, typically, those obtained by algebraicsubs +
# normalize_diffeq).
reduce_order := proc(c, mu, M, $)
    local qrest, K0, K, N;
    # recall that the c[k] may be complex
    qrest := add(abs(c[k])*n^k, k=0..ordeq-1);
    K0 := M * abs(evalf(mu)); # no cancellation (mu is usually a RootOf)
    K := ceil(K0);
    if K - K0 < 1/10 and absqrest <> 0 then
        K := K+1
    end if;
    N := ordeq; 
    while signum(abs(subs(n=N, absqrest)) - N^ordeq * (1-(M/K)*abs(mu))) = 1 do
        N := 2*N;
        if N > 3000 then
            error "unable to compute a reasonable bound (indicial pol)";
        end if;
    end do;
    userinfo(5, 'gfun', 'M' = M, 'K' = K, 'N' = N);
    K, N;
end proc:


# Input: deq
# Output: parameters of a majorant series 'maj' and threshold 'validity' such
#   that, IF abs([z^n](sol of deq)) <= A*[z^n](maj) for n <= validity, THEN
#   sol <| A*maj
doit := proc(deq, yofz, $)
    option cache;
    local a, c, mu, T, M, P, K, validity, below_maj, A, alpha, my_alpha, maj;
    userinfo(5, 'gfun', "deq" = deq);
    y,z := getname(yofz);
    ordeq := orddiffeq(deq, yofz);
    #if ordeq = 0 then return [0, 0, 1, 0] end if;
    ###
    a, c           := rewrite_diffeq(deq);
    mu, T, M, P    := bound_coefficients(a);
    if mu = infinity then
        # this would never happen if rec <-> deq translations were performed
        # in the naive way, but sometimes gfun is too clever, so that normalized
        # diffeqs may be degenerate
        return [0, 1, 1, []], ordeq;
    end if:
    K, validity    := reduce_order(c, mu, M);
    #my_alpha       := below(1/abs(evalf(mu)));
    #below_maj      := normal_majorant_series_formula(T, my_alpha, K, P, 1, z);
    #A              := fit_initial_values(below_maj, head, validity);
    #A, P           := get_rid_of_P(T, my_alpha, K, P, 1, z);
    ###
    alpha := abs(1/mu);  # what is the right place to do this?
    maj := bounds:-normal_majorant_series_formula(T, alpha, K, P, 1, z);
    userinfo(4, 'gfun', 'majorant' = evalf[5](maj));
    [T, alpha, K, P], validity; # sorted by `importance'
end proc:

end module:

