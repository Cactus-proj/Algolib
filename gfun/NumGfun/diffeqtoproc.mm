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

## diffeqtoproc.mm -- code generation
## written by Marc Mezzarobba

diffeqtoproc := module()

export default_disks, ModuleApply,
    trial_run, basic_series_sol, precompute_local_sol, fundamental_solution,
    proc_template;

local PRECOMPUTED_DATA, PRECOMPUTATION_PREC, DEQ, Y, Z;

# Input:
#   deq, yofz: as usual
#   prec: target precision for precomputations
#   disks: disks on which low-precision evaluations should be precomputed
# Output:
#   A procedure that evaluates y anywhere on its Riemann surface, using either 
#   precomputed local expansions (for points in one of the given disks and
#   precisions up to prec) or multiple-precision numerical analytic
#   continuation.
#   - The default evaluation precision is 'Digits', even though the procedure
#     takes an *absolute* precision.
#   - Precomputed expansions are used whenever possible if 'path' a point,
#     regardless whether it lies inside or outside the disk of convergence of
#     the solutions, and are not used when 'path' is a path.  For multivalued
#     functions, the determination is thus chosen at precomputation time.
#   - Precomputed expansions are considered in the order they where given to
#     diffeqtoproc.
#   (See the description of precompute_local_sol and proc_template below for
#   more details.)
ModuleApply := proc(Deq, yofz,
        { prec :: nonnegint := 0,
          disks :: list([path,And(numeric,positive)]) 
                := default_disks(Deq, yofz, prec) },
        $)
    local deq, eps, precomputed_data, y, z;
    ## Check arguments
    deq := diffeqtohomdiffeq(Deq, yofz);
    trial_run(deq, yofz);
    if prec = 0 and disks <> [] then
        WARNING("'disks' will be ignored since prec = 0");
    end if;
    ## Precompute local expansions for faster low-precision evaluation
    # The expansion we compute is intended to be evaluated at a
    # 'complex(rational)' value of z (using exact arithmetic), and then
    # converted to floating-point using makeitfloat, thus the -1.
    eps := Float(1,-prec-1);
    precomputed_data := map[3](precompute_local_sol, deq, yofz, disks, eps);
    ## Return proc
    y, z := getname(yofz);
    subs(
        { PRECOMPUTATION_PREC = prec, PRECOMPUTED_DATA = precomputed_data,
            DEQ = deq, Y = y, Z = z },
        eval(proc_template));
end proc:

# Procedure template used by diffeqtoproc.  Identifiers in uppercase get
# substituted (and are module-local for this purpose).
proc_template := proc(path, prec := Digits)
    local disk, center, arg, rad, local_exp, res;
    if prec <= PRECOMPUTATION_PREC and type(path, 'complex'('numeric')) then
        arg := convert(path, 'rational', 'exact');
        for disk in PRECOMPUTED_DATA do
            center, rad, local_exp := op(disk);
            if signum(abs(arg - center) - rad) < 0 then
                userinfo(4, 'gfun', sprintf("using precomputed series at %a",
                                            center));
                # local_exp gives a 10^(-precomp_prec-1)-infinite norm
                # approximation of the function (see ModuleApply),
                # so that makeitfloat(res) will yield a result with
                # absolute error <= 10^(-precomp_prec) <= 10^(-prec).
                res := eval(local_exp, Z=arg);
                return nthterm:-makeitfloat(res, prec);
            end if;
        end do;
    end if;
    userinfo(2, 'gfun', "using multiprecision analytic continuation");
    ancont:-analytic_continuation(DEQ, Y(Z), path, prec);
end proc:

# Default value for 'disk' when only 'prec' was specified
default_disks := proc(deq, yofz, prec, $)
    local rad;
    rad := 3/4 * abs(
        convert(diffeq_infsing(deq, yofz, 'numeric'), 'rational', 3));
    if rad = infinity then rad := 10 end if;
    if prec = 0 then []
    else [[[0], rad]]
    end if;
end proc:

# Ensure that we will actually be able to evaluate the solution of deq. This
# also triggers some bounds computations whose results will be remembered.
trial_run := proc(deq, yofz, $)
    local a;
    a := convert(diffeq_infsing(deq, yofz, 'numeric')/10,'rational',2);
    if a = infinity then a := 1 end if;
    ancont:-analytic_continuation(deq, yofz, [0,a], 10);
end proc:

# Compute the 'prec' first terms of the Taylor expansion at 'pt' of a solution
# of 'deq' of the form y(pt+dz) = dz^sol_idx + O(dz^ordeq).
basic_series_sol := proc(deq, yofz, pt, sol_idx, prec, $)
    local y, z, ordeq, ini, k, sol, dz, mydeq, rec, u, n, coef;
    y, z := getname(yofz);
    ordeq := orddiffeq(deq, y(z));
##  simple but too slow for large precisions
#   ini := { (D@@sol_idx)(y)(pt) = sol_idx!, 
#             seq( (D@@k)(y)(pt) = 0, k in {$0..ordeq-1} minus {sol_idx} ) };
#   Order := prec;
#   sol := rhs(dsolve({bare_diffeq(deq, z), op(ini)}, yofz, 'series'));
#   sol := subs(z-pt = dz, sol);
    mydeq := bare_diffeq(algebraicsubs(deq, y = z+pt, yofz), z);
    ini := { (D@@sol_idx)(y)(0) = sol_idx!, 
              seq( (D@@k)(y)(0) = 0, k in {$0..ordeq-1} minus {sol_idx} ) };
    rec := diffeqtorec({mydeq, op(ini)}, yofz, u(n));
    coef := rectoproc(rec, u(n), 'remember');
    sol := add(coef(k)*dz^k, k = 0..prec-1);
    # faster to eval(..., z=...) than series
    sol := convert(convert(sol, 'polynom'), 'horner');
    subs(dz = z-pt, sol);
end proc;
    
# FIXME: fusionner avec analytic_continuation ?
# en l'état actuel des choses, renvoie une matrice et une approximation par
# excès de sa norme de Frobenius (pour le cas où il y a des CI symboliques)
# ini may be a matrix
# would it make sense to have fundamental_solution return an ndmatrix when
# possible??? (probably not)
fundamental_solution := proc(deq, yofz, path, Ini, eps)
    local norm_ini, eps_transmat, transmat, iniconds_are_symbolic;
    norm_ini, iniconds_are_symbolic := op(ancont:-ext_norm_ini(Ini));
    eps_transmat := rndz(`/`, eps, rndu(`*`, 2, norm_ini));
    transmat := ancont:-path_transition_matrix(deq, yofz, path, eps_transmat);
    ancont:-apply_ini(transmat, Ini, iniconds_are_symbolic, eps, infinity);
end proc:

# Input:
#   deq, yofz: as usual
#   disk: [center, radius], where center is a point of the Riemann surface of
#     deq given by an analytic continuation path (in the format accepted by
#     rewrite_path)
#   eps: target precision
# Output: 
#   A polynomial p('z') with 'complex(rational)' coefficients such that
#   abs(p(z)-y(z)) <= eps for z in disk. If some initial values of deq are
#   non-numeric (missing, symbolic...), this holds only for the coefficients of
#   y(z) as a linear function of the initial values.
precompute_local_sol := proc(deq, yofz, disk, eps)
    local ordeq, my_eps, path, rad, pt, bound_on_disk, eps_local_ini,
        local_ini, norm_local_ini, eps_series, nt, complete_formula,
        diff_order, sol_idx, params, changevar;
    ordeq := orddiffeq(deq, yofz);
    my_eps := rndz(`/`, eps, 2*ordeq);
    path, rad := op(disk);
    path := ancont:-rewrite_path(deq, yofz, path, true);
    rad := convert(rad, rational, 'exact');
    pt := path[-1];
    ## Compute `initial values' at path end [y(pt), y'(pt), 1/2·y''(pt), ...]
    # Error analysis (y denotes the `true' solution):
    # (E1)  |y^(k)(pt)/k! - local_ini[k+1]| <= eps_local_ini  for all k
    params, changevar :=
        numeric_bounds:-bound_fundamental_solutions(deq, yofz, pt);
    bound_on_disk := max(seq(
        bounds:-tail_bound(op(params), rad, 0, 'derivative'=diff_order,
                                                    'transform'=changevar),
        diff_order = 0..ordeq-1));
    eps_local_ini := rndz(`/`, my_eps, bound_on_disk);
    # norm_local_ini is a bound on the Frobenius (= Euclidean, for a vector)
    # norm of local_ini, hence also on all |local_ini[k]|
    local_ini, norm_local_ini := op(fundamental_solution(deq, yofz, path,
        ancont:-diffeq_inicond_matrix(deq, yofz),
        eps_local_ini));
    ## Compute Taylor expansions at pt of all basic solutions
    # (E2)  sup(|y[k](z) - basic_series_sol(sol_idx=k)(z)|, z in disk)
    #           <= eps_series   for all k
    # where y[k](pt+dz) = dz^k + O(z^ordeq) is the basic sol of index k.
    eps_series := rndz(`/`, my_eps, norm_local_ini);
    nt := numeric_bounds:-needed_terms(deq, yofz, 0, pt, rad, eps_series);
    if nt > 500 then
        error "precomputation failed (would use %1 terms)", nt;
    end if;
    userinfo(2, 'gfun', "point" = pt, "radius" = rad, "#terms" = nt);
    # Let z in disk. The analytic continuation rule is
    #   y(z) = y(pt)·y[0](z) + y'(pt)·y[1](z) + ···
    # Using (E1), (E2), and AB-A'B' = (A-A')B + A'(B-B'), we have
    #   |y^(k)(pt)/k! · y[k](z) - (corresp. term below)|
    #   <= eps_local_ini · |y[k](z)| + |local_ini[1+k]| · eps_series
    #   <= eps_local_ini · bound_on_disk + norm_local_ini · eps_series
    #   <= 2·my_eps
    # thus
    #   |complete_formula(z) - y(z)| <= 2·ordeq·my_eps <= eps.
    complete_formula := add(
        local_ini[1+sol_idx,1] * basic_series_sol(deq, yofz, pt, sol_idx, nt),
        sol_idx = 0..ordeq-1);
    userinfo(5, 'gfun', "approx" = complete_formula); 
    [pt, rad, complete_formula];
end proc:

end module:
