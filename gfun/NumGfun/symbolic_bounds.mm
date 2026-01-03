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

## Symbolic and human-readable bounds
## written by Marc Mezzarobba, projet Algorithms, INRIA Paris-Rocquencourt

symbolic_bounds := module()

global Coeftayl;

export bound_rec_tail, bound_rec, bound_diffeq_tail, bound_diffeq,
    simplify_algebraic; 

simplify_algebraic := proc(x)
    radnormal(convert(
        evala(
            eval(subs(abs=abs_with_RootOf,x))),
        'radical'));
end proc:

bound_diffeq := proc(deq::hrdeq, yofz::function(name), z0 := 0)
    description "Computes a majorant series for the *power series* "
        "solutions of deq";
    local y, z, kappa, T, alpha, K, A, n, maj, k, p, q, psibound, P, coef, cst;
    y, z := getname(yofz);
    # special case for rational functions (see note in bound_rec below)
    if orddiffeq(deq, y(z)) = 0 then
        return bound_ratpoly(solve(deq, yofz), z)
    end if;
    kappa, T, alpha, K, P, A := op(bounds:-bound_diffeq_doit(_params[1..2]));
    alpha := simplify_algebraic(alpha);
    p := numer(kappa); q := denom(kappa);
    psibound := GAMMA(n/q+1)^p * q^(kappa*n);
    if kappa = 0 then
        A * bounds:-normal_majorant_series_formula(T, alpha, K, P, z)
    elif kappa > 0 then
        WARNING("divergent bound");
        maj := bounds:-normal_majorant_series_formula(T,alpha,K,P,z);
        #A * value(Sum(psibound * Coeftayl(maj, z=0, n) * z^n, n=0..infinity));
        A * Sum(psibound * Coeftayl(maj, z=0, n) * z^n, n=0..infinity);
    elif kappa = -infinity then
        A * add(z^k, k=0..K);
    else # kappa < 0
        maj := bounds:-normal_majorant_series_formula(T,alpha,K,P,z);
        #value(Sum(psibound * Coeftayl(maj, z=0, n) * z^n, n=0..infinity));
        coef :=  simplify(psibound * value(Coeftayl(maj, z=0, n)));
        cst := 1;
        if type(coef, `*`) then
            cst, coef := selectremove(type, coef, 'constant');
        end if;
        A * cst * Sum(coef * z^n, n=0..infinity);
    end if;
end proc:

bound_diffeq_tail := proc(deq::hrdeq, yofz::function(name), n)
    local z, kappa, T, alpha, K, P, A;
    z := getname(yofz)[2];
    # j'ai l'impression que la ligne suivante met déjà son abs à alpha, est-ce
    # ce que je veux ?
    kappa, T, alpha, K, P, A := op(bounds:-bound_diffeq_doit(deq, yofz)); 
    alpha := simplify_algebraic(alpha);
    eval(bounds:-tail_bound(kappa, T, alpha, K, P, A, z, n), z=abs(z));
end proc:

bound_rec := proc(rec::hrrec, uofn::function(name))
    local kappa, T, alpha, K, A, u, n, p, q, psibound, P, y, z, deq;
    u, n := getname(uofn);
    # special case for constant coefficients (NB: it would be nice to detect
    # this at the level of bound_normal_diffeq, but currently the output
    # of bound_diffeq does not have the
    # right form)
    #
    # FIXME: this fails if there are no initial conditions
    if rec_has_constant_coefficients(rec, u, n) then
        deq := rectodiffeq(rec, u(n), y(z));
        return ratpolytocoeff(bound_ratpoly(solve(deq, y(z)), z),z,n);
    end if;
    kappa, T, alpha, K, P, A := op(bounds:-bound_rec_doit(args));
    alpha := simplify_algebraic(alpha);
    if kappa = -infinity then
        piecewise(A, n <= K, 0);
    else
        p := numer(kappa); q := denom(kappa);
        if p > 0 then
            psibound := ratabove((2*Pi)^(p/q)) * ((n+ceil(3*q/2))/q)^p*n!^(p/q);
        else
            psibound := (n+ceil(3*q/2))^(-kappa) * n!^(kappa);
        end if;
        psibound * bounds:-normal_coeff_formula(T, alpha, K, A, P, n);
    end if;
end proc:

bound_rec_tail := proc(rec::hrrec, uofn::function(name),
            n0::name:=getname(uofn)[2])
    local z, maj, validity, kappa, T, alpha, K, P, A;
    kappa, T, alpha, K, P, A := op(bounds:-bound_rec_doit(rec, uofn)); 
    alpha := simplify_algebraic(alpha);
    eval(
        bounds:-tail_bound(kappa, T, alpha, K, P, A, 1, n0),
        z = abs(z));
end proc:

end module:
