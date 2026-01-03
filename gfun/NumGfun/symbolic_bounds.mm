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

## Symbolic and human-readable bounds
## written by Marc Mezzarobba, projet Algorithms, INRIA Paris-Rocquencourt

symbolic_bounds := module()

export bound_rec_tail, bound_rec, bound_diffeq_tail, bound_diffeq;

local bound_on_psi, simplify_algebraic;

simplify_algebraic := proc(x, $)
    radnormal(convert(
        evala(
            eval(subs(abs=abs_with_RootOf, x))),
        'radical'));
end proc:

# See ?NumGfun,bound_diffeq
bound_diffeq := proc(Deq::hrdeq, yofz::function(name), $)
    description "Computes a majorant series for the *power series* "
        "solutions of deq";
    local y, z, deq, kappa, T, alpha, K, A, n, k, P, coef, cst, rat;
    y, z := getname(yofz);
    # Special case for rational functions. See note in bound_rec below.
    # Equations with missing initial values are excluded (bound_ratpoly is not
    # clever enogh to handle them), as well as rational functions with a pole
    # at the origin (=> no power series solution).
    if orddiffeq(Deq, y(z)) = 0 then
        rat := solve(Deq, yofz);
        if indets(rat) minus {z} = {} and eval(denom(rat), z=0)<>0 then
            return bound_ratpoly(rat, z)
        end if;
    end if;
    deq := diffeqtohomdiffeq_warn(Deq, y(z));
    kappa, T, alpha, K, P, A := op(bounds:-bound_diffeq_doit(_params[1..2]));
    alpha := abs(simplify_algebraic(alpha));
    if kappa = 0 or kappa = -infinity then
        A * bounds:-normal_majorant(T, alpha, K, P, z)
    else
        if kappa > 0 then WARNING("divergent bound") end if;
        coef := bounds:-normal_coeff_bound(T, alpha, K, 1, P, n)
                / bounds:-psi(kappa, n);
        coef := simplify(coef, 'constant', 'power', 'radical', 'sqrt')
                                                            assuming n::integer;
        cst := 1;
        if type(coef, `*`) then
            cst, coef := selectremove(type, coef, 'constant');
        end if;
        A * cst * Sum(coef * z^n, n=0..infinity);
    end if;
end proc:

# See ?NumGfun,bound_diffeq
#
# Note: it might be nice to have a special case for rational functions here too.
bound_diffeq_tail := proc(Deq::hrdeq, yofz::function(name),
                          n::{name, integer}, $)
    local z, deq, kappa, T, alpha, K, P, A;
    z := op(yofz);
    deq := diffeqtohomdiffeq_warn(Deq, yofz);
    kappa, T, alpha, K, P, A := op(bounds:-bound_diffeq_doit(deq, yofz));
    alpha := abs(simplify_algebraic(alpha));
    eval(bounds:-tail_bound(kappa, T, alpha, K, P, A, z, n), z=abs(z));
end proc:

# See bounds:-psi and [Mezzarobba2011, lemme 5.35].
bound_on_psi := proc(kappa, n, $)
    local p, q;
    p := numer(kappa); q := denom(kappa);
    if kappa = -infinity then
        1
    elif p > 0 then
        ratabove((2*Pi)^(p/q)) * ((n+ceil(3*q/2))/q)^p*n!^(p/q);
    else
        (n+ceil(3*q/2))^(-kappa) * n!^(kappa);
    end if;
end proc:

# See ?NumGfun,bound_rec
bound_rec := proc(rec::hrrec, uofn::function(name), $)
    local kappa, T, alpha, K, A, u, n, p, q, psibound, P, y, z, rat;
    u, n := getname(uofn);
    # Special case for constant coefficients (NB: it would be nice to detect
    # this at the level of bound_normal_diffeq, but currently the output
    # of bound_diffeq does not have the right form)
    if rec_has_constant_coefficients(rec, u, n) then
        rat := solve(rectodiffeq(rec, u(n), y(z)), y(z));
        if indets(rat) minus {z} = {} then
            return ratpolytocoeff(bound_ratpoly(rat, z), z, n);
        end if;
    end if;
    kappa, T, alpha, K, P, A := op(bounds:-bound_rec_doit(args));
    alpha := abs(simplify_algebraic(alpha));
    bound_on_psi(kappa, n) * bounds:-normal_coeff_bound(T, alpha, K, A, P, n);
end proc:

# See ?NumGfun,bound_rec_tail
bound_rec_tail := proc(rec::hrrec, uofn::function(name),
            n0::{name, integer}:=getname(uofn)[2], $)
    local z, maj, validity, kappa, T, alpha, K, P, A;
    kappa, T, alpha, K, P, A := op(bounds:-bound_rec_doit(rec, uofn)); 
    alpha := abs(simplify_algebraic(alpha));
    eval(
        bounds:-tail_bound(kappa, T, alpha, K, P, A, 1, n0),
        z = abs(z));
end proc:

end module:
