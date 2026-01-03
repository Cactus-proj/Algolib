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

## ancont.mm: Numerical evaluation and analytic continuation of holonomic
## functions.


ancont := module()

uses LinearAlgebra;

export rectodiffrec, parametered_rec, step_transition_matrix,
    path_transition_matrix, plot_path, fail_if_singular_path, bit_burst_path,
    subdivide_path, rewrite_path, absolute_precision_warning,
    diffeq_inicond_matrix, ext_norm_ini, apply_ini, analytic_continuation,
    transition_matrix, local_monodromy, monodromy,
    # new binary splitting
    rec_matrix_num_den, binsplit_matrix, binsplit, diffeq_to_local_rec,
    ordinary_step_transition_matrix, rec_mat_mul,
    # regular singular points
    dist_to_sing, direction, subdivide_ordinary_path, connect_sing,
    sol_value_from_matrix,
    REGSING, is_regsing_step;


################################################################################
# Ad hoc binary splitting routines in the ordinary case
################################################################################

# TODO: This function should eventually be replaced/merged with the more general
# version currently in regsing.mm.
# Piqué de la réécriture des matrices, mais un peu modifié.
rec_matrix_num_den := proc(rec, uofn, $)
    local u, n, barerec, recop, charpoly, den, mat, Shift;
    u, n := getname(uofn);
    barerec := bare_rec(rec, uofn);
    recop := eval(barerec, u = (x -> Shift^eval(x,n=0)));
    charpoly := eval(radnormal(recop / lcoeff(recop, Shift)));
    den := denom(charpoly);
    mat := den * Transpose([CompanionMatrix(charpoly, Shift)][1]);
    mat := map(normal, mat);
    mat := map(convert, mat, 'horner');
    den := convert(den, 'horner');
    numdenmatrix:-make(mat, den)
end proc:

# TODO: This function should eventually be replaced/merged with the more general
# version currently in regsing.mm.
binsplit_matrix := proc(rec, uofn, pt, lambda, diff_order, $)
    local coeffs_mat, den, pow_num, pow_den, sums_row;
    coeffs_mat, den := op(rec_matrix_num_den(rec, uofn));
    pow_num := series(numer(pt + lambda), lambda, diff_order);
    pow_den := denom(pt);  # merge with den?
    sums_row := Matrix([[den*pow_den, 0 $ (ordrec(rec, uofn) - 1)]]);
    [coeffs_mat, den, pow_num, pow_den, sums_row];
end proc:

# Order must be set by the caller
rec_mat_mul := proc(high, low, $)
    # We divert a protected global variable to avoid passing around a name and
    # perhaps gain a bit of time.
    global _Z;
    local tmp1, tmp2, j,
        coeffs_mat_l, den_l, pow_num_l, pow_den_l, sums_row_l,
        coeffs_mat_h, den_h, pow_num_h, pow_den_h, sums_row_h,
        coeffs_mat,   den,   pow_num,   pow_den,   sums_row;
    coeffs_mat_l, den_l, pow_num_l, pow_den_l, sums_row_l := op(low);
    coeffs_mat_h, den_h, pow_num_h, pow_den_h, sums_row_h := op(high);
    coeffs_mat := mvMultiply(coeffs_mat_h, coeffs_mat_l);
    ###
    #sums_row := MatrixAdd(
    #    MatrixScalarMultiply(
    #        mvMultiply(sums_row_h, coeffs_mat_l),
    #        pow_num_l),
    #    MatrixScalarMultiply(sums_row_l, den_h * pow_den_h));
    #    sums_row := map(series, sums_row, _Z);
    ### Less robust but much more efficient version of the above:
    tmp1 := mvMultiply(sums_row_h, coeffs_mat_l);
    tmp2 := den_h * pow_den_h;
    sums_row := rtable(1..1, 1..op([1,2], tmp1),
        [[seq(
            series(pow_num_l * tmp1[1,j] + tmp2 * sums_row_l[1,j], _Z),
            j=1..op([1,2], tmp1))]],
        'subtype' = 'Matrix');
    ###
    pow_num := series(pow_num_h * pow_num_l, _Z);
    pow_den := pow_den_h * pow_den_l;
    den := den_h * den_l;
    [coeffs_mat, den, pow_num, pow_den, sums_row];
end proc:

# Order must be set by the caller
binsplit := proc(gen_factor, n, low, high, $)
    local tmp, i, mid;
    if high - low <= Settings:-binary_splitting_threshold then
        tmp := eval(gen_factor, n=low);
        for i from low + 1 to high - 1 do
            tmp := rec_mat_mul(eval(gen_factor, n=i), tmp);
        end do;
        tmp;
    else
        mid := iquo(low + high, 2);
        rec_mat_mul(
            procname(gen_factor, n, mid, high),
            procname(gen_factor, n, low, mid));
    end if;
end proc:

# z0 and u(n) are chosen here and returned in order for 'option cache' to work
diffeq_to_local_rec := proc(deq, yofz, $)
    option cache(1);
    local y, z, local_deq, rec, i, z0, u, n, delta;
    y, z := getname(yofz);
    local_deq := algebraicsubs(deq, y = z0 + z, yofz);
    local_deq := { local_deq,
                   seq((D@@i)(y)(0) = delta(i)*i!,
                       i=0..orddiffeq(deq, yofz) -1) };
    rec := diffeqtorec(local_deq, yofz, u(n));
    rectohomrec(rec, u(n)), z0, u, n, delta;
end proc:

ordinary_step_transition_matrix := proc(deq, yofz, z0, z1, epsilon,
                                                              first_row_only, $)
    local ordeq, diff_order, rec, dummy, u, n, delta, rec_matrix, lambda, rad,
        nterms, prod, den, pow_den, sums_row, ordcoefrec, ini, ini_matrix, i, j,
        canonical_sols_row;
    ordeq := orddiffeq(deq,yofz);
    diff_order := `if`(first_row_only, 1, ordeq);
    # The 'matrices' we are going to multiply
    rec, dummy, u, n, delta := diffeq_to_local_rec(deq, yofz);
    rec := subs(dummy = z0, rec);
    rec_matrix := binsplit_matrix(rec, u(n), z1 - z0, lambda, diff_order);
    # How many terms to sum
    rad := abs(z1 - z0);
    nterms := max(seq(   # this could probably be improved
        numeric_bounds:-needed_terms(deq, yofz, i, z0, rad, epsilon/ordeq),
        i = 0 .. diff_order-1));
    nterms := floor(Settings:-terms_factor * nterms + Settings:-terms_delta);
    # Now compute the product
    userinfo(2, 'gfun', sprintf(
        "%s --> %s ord=%a, prec~=%a, terms=%a",
        sprint_small_approx(z0), sprint_small_approx(z1), diff_order,
        evalf[5](epsilon), nterms));
    Order := diff_order;
    prod := subs(_Z=lambda,
        binsplit(subs(lambda=_Z, rec_matrix), n, 0, nterms + 1));
    den, pow_den, sums_row := op(prod[[2,4,5]]);
    # Extract from sums_row the coeffs of the transition matrix. This is not
    # entirely straightforward since the fundamental solutions of rec (that is,
    # the columns of sums_row) do *not* correspond to the canonical solutions of
    # deq.
    # TODO: try to remove a common factor?
    ini := remove(has, rec, n);
    ordcoefrec := ordrec(rec, u(n));
    ini_matrix := eval(Matrix([seq( [seq(
        subs(
            eval(ini, delta = proc(x) `if`(x = j, 1, 0) end proc),
            u(i)),
        j = 0..ordeq - 1)], i = 0..ordcoefrec - 1 )]));
    canonical_sols_row := map(series,
        MatrixMatrixMultiply(sums_row, ini_matrix), lambda);
    numdenmatrix:-make(
        Matrix([seq(
            [map(coeff, canonical_sols_row, lambda, i)],
            i = 0..diff_order-1)]),
        den * pow_den);
end proc:

################################################################################
## Analytic continuation
################################################################################

step_transition_matrix := proc(deq, yofz, z0, z1, epsilon,
                                          {first_row_only::boolean := false}, $)
    local z, lc, ordeq, diff_order, step_str, mat;
    z := op(yofz);
    lc := diffeq_lcoeff(deq, yofz);
    ordeq := orddiffeq(deq,yofz);
    diff_order := `if`(first_row_only, 1, ordeq);
    step_str := sprintf("%s --> %s", sprint_small_approx(z0),
                                                       sprint_small_approx(z1));
    if ordeq = 0 then
        mat := numdenmatrix:-make(Matrix(0, 0), 1);
    elif z0 = z1 then
        userinfo(3, 'gfun', step_sr, "trivial case");
        mat := numdenmatrix:-make(IdentityMatrix(ordeq)[1..diff_order],1)
    elif is_regsing_step(z0) then  # only vertices in Q(i) are supported for now
        userinfo(3, 'gfun', step_str, "direct regular singular case");
        ASSERT(not(is_regsing_step(z1)));  # only one endpoint may be singular
        mat := regsing:-singular_step_transition_matrix(deq, yofz,
                op(z0), z1, epsilon, first_row_only);
    elif is_regsing_step(z1) then  # only vertices in Q(i) are supported for now
        userinfo(3, 'gfun', step_str, "inverse regular singular case");
        mat := regsing:-inverse_singular_step_transition_matrix(deq, yofz,
                op(z1), z0, epsilon, first_row_only);
    else
        userinfo(3, 'gfun', step_str, "ordinary case");
        mat := ordinary_step_transition_matrix(deq, yofz, z0, z1, epsilon,
                                                                first_row_only);
    end if;
    userinfo(10, 'gfun', sprintf("computed matrix ~= %a",
        evalf[5](op(1, mat))/evalf[5](op(2, mat))));
    mat;
end proc:

# Compute an epsilon-approximation (with rational coefficients) of the
# transition matrix associated to deq along any nonsingular broken-line path.
# The product of the transition matrices for each step is done by binary
# splitting.
path_transition_matrix := proc(deq, yofz, path, epsilon, $)
    local step_count, P, eps, m, stage, i, mb;
    step_count := nops(path) - 1;
    userinfo(7, 'gfun', "eps" = evalf[3](epsilon), "path"=evalf[3](path));
    if step_count <= 0 or (step_count=1 and path[1]=path[2]) then
        numdenmatrix:-identity(orddiffeq(deq,yofz));
    elif step_count = 1 then
        step_transition_matrix(deq,yofz,op(path),epsilon);
    else
        m := iquo(step_count,2);
        stage := [ path[1..m+1], path[m+1..-1] ];
        for i to 2 do
            mb := numeric_bounds:-bound_transition_matrix(deq,yofz,stage[-i]);
            eps := epsilon/(2*mb);
            P[i] := path_transition_matrix(deq,yofz,stage[i],eps);
        end do;
        numdenmatrix:-multiply(P[2],P[1]);
    end if;
end proc:

###############################################################################
## Utilities for analytic continuation paths
###############################################################################

# NOTE: Internally, it would be better to represent paths as sequences of
# steps (increments) instead of sequences of vertices.

plot_path := proc(deq, yofz, path, $) # usage: display(plot_path(...))
    local sing, singplot, pathplot, circplots, allpoints, xmin, xmax, ymin, ymax, gridplot;
    sing := diffeq_singularities(deq, yofz);
    singplot := plots:-complexplot(sing, style=point);
    pathplot := plots:-complexplot(path);
    circplots := [ seq(
        plottools:-circle( [Re(path[i]),Im(path[i])], abs(path[i+1] - path[i]) ),
        i=1..(nops(path)-1) ) ];
    allpoints := evalf([op(sing), op(path)]);
    xmin := min(op(map(Re, allpoints))) - 1;
    xmax := max(op(map(Re, allpoints))) + 1;
    ymin := min(op(map(Im, allpoints))) - 1;
    ymax := max(op(map(Im, allpoints))) + 1;
    gridplot := plots:-coordplot(cartesian,view=[xmin..xmax,ymin..ymax],
        linestyle='DOT',scaling='constrained'):
    [ gridplot, singplot, pathplot, op(circplots) ];
end proc:

fail_if_singular_path := proc(deq, yofz, Path, {check_convergence := false}, $)
    local path, sing, t, j, s;
    path := evalf(Path);
    sing := diffeq_singularities(deq, yofz);
    for j from 1 to nops(path)-1 do
        for s in sing do
            t := (s-path[j])/(path[j+1]-path[j]);
            if Im(t) < Float(1, 2-Digits) and t >= 0 and t <= 1 then
                error "Unable to perform analytic continuation: the path "
                    "passes through or too close to a singular point of "
                    "the differential equation. To go through a regular "
                    "singular point, please make it one of the vertices. "
                    "If the singular point is only close to the path, try "
                    "increasing Digits. "
                    "The problematic segment is [%1,  %2].",
                    Path[j], Path[j+1];
            end if;
        end do;
        # this loop is separate from the previous one to avoid confusing error
        # messages
        for s in sing do
            if check_convergence and abs(path[j+1]-path[j])>=abs(s-path[j]) then
               error "Step %1->%2 may escape from the disk of (guaranteed) "
                "convergence of the series expansions of the solutions of %3",
                Path[j], Path[j+1], deq;
            end if;
        end do;
    end do;
end proc:

###############################################################################
## Path rewriting
###############################################################################

# Replace a path [a,b] where a is assumed to have small bit-size but not b by a
# path along which efficient analytic continuation is possible.
# (Compare evaldiffeq(deq[arctan],y(z),[0,evalf[1000](Pi/5)],1000) w/ and w/o
# usebitburst.)
# Note that the returned path starts just after z0 (i.e., does not contain z0).
bit_burst_path := proc(step::[complex(numeric), complex(numeric)], $)
        :: list(complex(numeric));
    local thr, z0, z1, dz, dir, z0bis, path, p, q, q0, Q;
    thr := Settings:-bit_burst_threshold;
    z0, z1 := op(convert(step, 'rational', 'exact'));
    dz := z1 - z0;
    if length(denom(z1)) <= thr+1 then
        return step
    end if:
    # Reduce to the case of step length < 10^-thr by introducing an intermediate
    # point z0 (= 10^-thr-approx of step[2] of small bit-size) if necessary
    if signum(abs(dz)-10^(-thr))=1 then
        # FIXME: cvgce issues?
        z0bis := convert(evalf_complex_abs_error(z1, thr+1), 'rational');
        return [ z0, op(bit_burst_path([z0bis, z1])) ];
    end if;
    path := z1;
    p, q := numer(dz), denom(dz);
    q0 := max(denom(z0),2);
    while p <> 0 and q > q0^2 do
        Q := q;
        q := isqrt(q);
        p := trunc(p * q/Q); 
        path := z0 + p/q, path;
    end do;
    userinfo(4, 'gfun', "bit-burst path" = [path]);
    [path];
end proc:

dist_to_sing := proc(deq, yofz, pt, $)
    local s;
    min(seq(abs(pt - s), s in diffeq_singularities(deq, yofz, 'exclude'=[pt])));
end proc:

# Almost unit vector pointing from a to b.  The direction has to be exact to
# ensure that the local basis uses the expected determination of multivalued
# functions in the singular case.
direction := proc(a, b, $)
    local dir;
    dir := b-a;
    dir/above_abs(dir, 'rational');
end proc:

# For now, this is done naively using floating-point arithmetic.  In some very
# special cases, the rewritten path might not be homotopic to the original one
# or contain illegal steps.
subdivide_ordinary_path := proc(deq, yofz, path, start:=1)
    local rad, point, digits, split_thr;
    split_thr := Settings:-split_path_threshold;
    if nops(path) <= 1 then
        path
    elif start > nops(path) then
        error "invalid path"
    elif nops(path) > Settings:-max_steps then
        error "emergency stop: too many (%1) analytic continuation steps "
              "(increase NumGfun:-Settings:-max_steps to proceed)",
              Settings:-max_steps;
    elif start = nops(path) then # base case
        path;
    else
        rad := dist_to_sing(deq, yofz, path[start]);
        if abs(evalf(path[start+1] - path[start])) <  split_thr * rad then
            # we have finished subdividing one step, now handle the remaining
            # ones
            subdivide_ordinary_path(deq, yofz, path, start+1)
        else
            # insert one point on the current segment, then recursively
            # subdivide the tail of the path thus created
            point := path[start]
                            + 0.5 * rad * direction(path[start], path[start+1]);
            # keep the bit size of the intermediate points small unless the path
            # runs very close to a singular point (note that rad(path[start+1])
            # >= (1-0.5)*rad)
            digits :=  max(1, -ilog10(split_thr*rad)) + 1;
            #point := convert(point, 'rational', digits);
            point := convert(evalf[digits](point), 'rational', 'exact');
            subdivide_ordinary_path(deq, yofz,
                   [op(path[1..start]), point, op(path[start+1..-1])], start+1);
        end if;
    end if;
end proc:

# Returns an point sequence (actually, either a single ordinary point or NULL)
# appropriate for ``going out'' of sing in the direction towards neib.
connect_sing := proc(deq, yofz, sing, neib, $)
    local rad;
    rad := dist_to_sing(deq, yofz, sing);
    if abs(evalf(sing-neib)) > Settings:-split_path_threshold * rad then
        Digits := max(2, iquo(Digits, 3));
        rad := convert(rad, 'rational', 'exact');
        sing + rad/2 * direction(sing, neib);
    else
        NULL
    end if;
end proc:

# Set up a legal and efficient ordinary path between each pair of consecutive
# singular steps, with suitable variations at the endpoints.
subdivide_path := proc(deq, yofz, path, usebitburst, $)
    local new_path, last, len, cur, pt_type, subpath;
    new_path := [];
    last := 0;
    len := nops(path);
    for cur to len+1 do
        if cur <= len then
            pt_type := singularity_type(deq, yofz, path[cur]);
        end if;
        if pt_type = "regular singular" or cur = len+1 then
            subpath := path[last+1..cur-1];
            # Ensure that subpath starts and ends with ordinary points close
            # enough to the singular points they will connect to.
            if 0 < last and last < len then
                # This works even if subpath = [].
                subpath := [connect_sing(deq, yofz, path[last], path[last+1]),
                            op(subpath)];
            end if;
            # Here subpath may be empty in the case of a path of the form
            # [..., sing, sing, ...]
            if 1 < cur and cur < len+1 and subpath <> [] then
                subpath := [op(subpath),
                            connect_sing(deq, yofz, path[cur], subpath[-1])];
            end if;
            fail_if_singular_path(deq, yofz, subpath);
            subpath := subdivide_ordinary_path(deq, yofz, subpath);
            fail_if_singular_path(deq, yofz, subpath, 'check_convergence');
            if cur <= len then # we are still in the middle of the path
                WARNING("analytic continuation through regular singular point "
                    "%1: using experimental, INCOMPLETE numerical connection "
                    "code", op(yofz)=path[cur]);
                new_path := [op(new_path), op(subpath), REGSING(path[cur])];
                last := cur;
            else
                if nops(subpath) >= 2 and usebitburst then
                    subpath := [op(subpath[1..-2]),
                                           op(bit_burst_path(subpath[-2..-1]))];
                    fail_if_singular_path(deq, yofz, subpath,
                                                                'check_convergence');
                end if;
                new_path := [op(new_path), op(subpath)];
            end if;
        elif pt_type = "irregular singular" then
            error "The path goes through %1, which is an irregular singular "
                "point of the equation. Only regular singular points are "
                "supported.", op(yofz)=path[cur];
        end if;
    end do;
end proc:

is_regsing_step := proc(z0, $)
    type(z0, 'function') and op(0, z0)=REGSING;
end proc:

rewrite_path := proc(deq, yofz, Path, usebitburst,
        {subdivide := true, fromzero := true}, $)::internal_path;
    local path;
    path := Path;
    # Handle a few special cases of the input path syntax
    if type(path, 'complex(numeric)') then
        path := [0, path];
        try fail_if_singular_path(deq, yofz, path, 'check_convergence')
        catch "Step":
            error "evaluation point outside the disk of convergence of the "
                "differential equation (try specifying an analytic "
                "continuation path such as [0, %1])", Path;
        end try;
    elif path = [] then
        error("empty analytic continuation path!");
    elif path[1] <> 0 and fromzero then
        userinfo(1, 'gfun', "adding 0 in front of analytic continuation path");
        path := [0, op(path)];
    end if;
    if nops(path) = 1 then
        path := [op(path), op(path)];
    end if;
    path := map(convert, path, 'rational', 'exact');
    if subdivide then
        path := subdivide_path(deq, yofz, path, usebitburst);
    end if;
    userinfo(4, 'gfun', "final analytic continuation path is", path);
    path;
end proc:

###############################################################################
## Evaluation
###############################################################################

absolute_precision_warning := proc($)
    if Settings:-enable_absolute_precision_warning then
        userinfo(1, 'gfun', "Recall that gfun:-NumGfun uses *absolute* error.");
    end if:
end proc:

# ERROR ANALYSIS: The basic fact is that
#   N(A'·B' - A·B) <= N(A'-A)·N(B') + N(A)·N(B'-B)
# where N(·) is any sub-multiplicative matrix norm.  We use the Frobenius norm
# N(A) = sqrt(sum(a[i,j]², [i,j])).
#
# This gives the following algorithm to compute C'≈A'B' such that N(C'-A·B)<=ε:
#   1. Find M[A] >= N(A) (a priori bound).
#   2. Compute B' s.t. N(B'-B) <= ε/(3·M[A]).
#   3. Compute M[B'] >= N(B').
#   4. Compute A' s.t. N(A'-A) <= ε/(3·M[B']).
#   5. Compute C' s.t. N(C'-A'·B') <= ε/3.
# (Indeed, we have N(C'-AB) <= N(C'-A'·B') + N(A'-A)·N(B') + N(A)·N(B'-B).)

diffeq_inicond_matrix := proc(deq, yofz, $)
    local r, i, u, n, proc_ini, st;
    global _C;
    st := singularity_type(deq, yofz, 0);
    r := orddiffeq(deq, yofz);
    if st = "ordinary" then
        proc_ini := rectoproc(diffeqtorec(deq, yofz, u(n)), u(n), 'remember');
        # column matrix rather than vector because this extends more
        # easily to full fundamental matrices
        Matrix(r, 1, [seq([proc_ini(i)], i = 0..r-1)]); # r may be zero!
    elif st = "regular singular" then
        # In the regular singular case, introduce symbolic initial values for
        # all fundamental solutions, regardless of what the user specified.
        # Using rectoproc as above instead would compute a basis of the *power
        # series* solutions.  [This may or may not be a good idea.  I will
        # eventually need to support explicit initial values at regular singular
        # points anyway.]
        Matrix([seq([_C[i]], i=0..r-1)]);
    else
        ASSERT(false, "initial conditions at irregular singular point?!");
    end if:
end proc:

# Input:
#   ini::Matrix, initial values of a differential equation
#   force_symbolic_ini, whether to see them as symbolic even if they are cst
# Ouptut:
#   [ upper approx of "effective" Frobenius norm (1 for symbolic),
#     whether symbolic ]
ext_norm_ini := proc(ini, force_symb := false, $)
    local msg;
    if (not force_symb) and type(ini, 'Matrix'('complexcons')) then
        _EnvNumGfunUsePoorMansEvalrC := true;
        [ numeric_bounds:-bound_frobenius_norm(ini), false ];
    else
        msg := "the result will be a linear combination of the given initial "
            "values with coefficients computed to the prescribed accuracy.  "
            "The order of magnitude of the initial value will not be taken "
            "into account.";
        if type(ini, 'Matrix'({'name', 'poszero', 'negzero', 'cx_zero'})) then
            userinfo(1, 'gfun', cat("symbolic initial values: ", msg));
        else
            WARNING(cat("non-constant initial values: ", msg));
        end if;
        [1., true]
    end if;
end proc:

# Compute an approximation of transmat·ini and an upper bound on the Frobenius
# norm of the resulting approximate matrix.  (Though usually redundant, the
# bound can be useful when dealing with symbolic initial values.)  The initial
# values 'ini' are usually a vector but may be any matrix.
# 1. If symbolic_ini=true, then the elements of ini must be constants *that can
#    be faithfully evaluated with evalf()*.  Note that this excludes composite
#    expressions where rounding could occur.  The output is an approximation of
#    transmat·ini with *entrywise* error <= (eps+9·10^(-prec))/10.
# 2. Otherwise, the elements of ini are assumed to be symbolic values.  The
#    entries of the output vector are linear combinations of the quoted initial
#    values.  The coefficients of the linear combinations are quoted using
#    'quote' (e.g. proc(x::uneval) ''x'' end). Again, the error on the
#    coefficients is <= (eps+9·10^(-prec))/10.
# 3. The computed coefficients are represented either as "Gaussian rationals"
#    (for prec=infinity) or as "Gaussian floats" of the form
#    (int+I*int)·10^(-prec).
apply_ini := proc(transmat, ini, eps, prec, symbolic_ini, quote:=``, $)
    local norm_transmat, mat, eps_ini, approx_ini, val, bound;
    norm_transmat := numdenmatrix:-bound_norm(transmat);
    if symbolic_ini then
        mat := nthterm:-ratorfloat(transmat, Settings:-precision_ini(prec));
        mat := map(quote, mat);
        [ MatrixMatrixMultiply(mat, ini), norm_transmat ];
    else
        eps_ini := rndz(`/`, eps,
            rndu(`*`, 10*numdenmatrix:-row_dimension(transmat), norm_transmat));
        # Allow falling back on evalf (with a warning) to evaluate the initial
        # values.
        _EnvNumGfunUsePoorMansEvalrC := true;
        # Entrywise error <= eps_ini (unless funny custom precision_ini);
        # hence, error in norm subordinate to uniform <= eps/(10*norm_transmat).
        approx_ini := ndmatrix_approximation(ini,
                                 Settings:-precision_ini(-ilog10(eps_ini)) );
        # Entrywise error <= eps/10
        val := numdenmatrix:-multiply(transmat, approx_ini);
        # |makeitfloat(x)-x| <=  0.85·10^(-prec) (assuming precision_ini=id),
        # hence entrywise error <= (1/10)·eps + (9/10)·10^(-prec).
        bound := numdenmatrix:-bound_norm(val);
        val := nthterm:-ratorfloat(val, Settings:-precision_ini(prec));
        [ val, bound ];
    end if;
end proc:

# From one or more columns of the fundamental matrix of 'deq' at 'pt', compute
# various kinds of values or asymptotic expansions of y(z) at 'pt'.
sol_value_from_matrix := proc(deq, yofz, Pt, resmat, monomials, ord, $)
    local basis, pt;
    if is_regsing_step(Pt) then pt := op(Pt) else pt := Pt end if;
    if monomials then
        basis := Vector(regsing:-local_basis_monomials(deq, yofz, pt));
    elif ord > 0 then
        basis := Vector(regsing:-local_basis_expansions(deq, yofz, pt, ord));
    end if;
    if assigned(basis) then
        DotProduct(map(``, Column(resmat, 1)), basis, 'conjugate'=false);
    elif RowDimension(resmat) = 0 then
        0
    else
        resmat[1,1];
    end if;
end proc:

# TODO: Move most of the code from analytic_continuation into a new function
# fundamental_matrix taking as input a matrix of initial values with any number
# of columns, and rewrite both analytic_continuation and transition_matrix in
# terms of fundamental_matrix.

# See ?NumGfun,analytic_continuation.
analytic_continuation := proc(Deq::hrdeq, yofz::function(name),
        Path::path, precision::posint:=Settings:-default_eval_precision,
        { usebitburst::boolean:=true, forcepath::boolean:=false,
          symbini::boolean:=false, monomials:=false, ord:=0 }, $ )
    local deq, path, nbsteps, ini, eps, bound_ini, symbolic_ini, bound_last,
        transmat_butlast, bound_approx_butlast, transmat_last, transmat, resmat;
    deq := diffeqtohomdiffeq_warn(Deq, yofz);
    path := `if`(forcepath, Path, rewrite_path(deq, yofz, Path, usebitburst));
    nbsteps := nops(path) - 1;
    # If the user gave symbolic initial values (or none at all), we will return
    # a linear combination of the initial values instead of a single number.
    ini := diffeq_inicond_matrix(deq, yofz);
    # The final complex result should be within 10^(-precision) of the exact
    # value of y, and use 'precision' decimal digits only.  We first compute a
    # 10^(-precision-1)-approximation (in Frobenius norm) which will get rounded
    # to the output format at the very end.  NOTE: All we should have to do to
    # be able to compute the products approximately is to adjust the denom.
    eps := rndz(`/`, Float(1, -precision-1), (nbsteps + 1));
    # Find bound_ini >= norm(ini) (bound_ini = 1 if ini not constant),
    # bound_exact_butlast >= norm(exact trans mat but last step).
    bound_ini, symbolic_ini := op(ext_norm_ini(ini, 'force_symb'=symbini));
    bound_last := numeric_bounds:-bound_transition_matrix(deq, yofz,
                                                                  path[-2..-1]);
    if bound_ini = 0 then bound_ini := 1 end if; # suboptimal (but who cares)
    if bound_last = 0 then bound_last := 1 end if;
    # Compute a matrix 'transmat' such that transmat·ini is "the value at the
    # end of the path".  Depending on whether we are interested only in the
    # value of y or in a complete set of "initial" conditions, 'transmat' may be
    # either a row matrix or a square matrix.
    transmat_butlast := path_transition_matrix(deq, yofz, path[1..-2],
        below((nbsteps-1)*evalrC(eps)/(evalrC(bound_ini)*evalrC(bound_last))));
    bound_approx_butlast := numdenmatrix:-bound_norm(transmat_butlast);
    if bound_approx_butlast = 0 then bound_approx_butlast := 1 end if;
    transmat_last := step_transition_matrix(deq, yofz, path[-2], path[-1],
        below(evalrC(eps)/(evalrC(bound_ini)*evalrC(bound_approx_butlast))),
        'first_row_only'=(not monomials and ord <= 0));
    # Let Δ'=transmat, Δbl'=transmat_butlast, Δl'=transmat_last, and Δ, Δbl, Δl
    # be the corresponding exact transition matrices.  After the next line:
    # N(Δ'-Δ) = N(Δl'·Δbl' - Δl·Δbl) <= N(Δl'-Δl)·N(Δbl') + N(Δl)·N(Δbl'-Δbl)
    #         <= nbsteps·eps/bound_ini.
    transmat := numdenmatrix:-multiply(transmat_last, transmat_butlast);
    # Now compute transmat·ini.  We have |resmat-Δ'·ini|<=eps+(9/10)·10^(-p)
    # (entrywise), hence |resmat-Δ·ini|<=(nbsteps+1)·eps+(9/10)·10^-p<=10^(-p).
    # Note that the bounds on ini and Δ where computed in Frobenius norm, but
    # both the vector uniform norm and the matrix norm it induces are bounded by
    # the Frobenius norm.
    resmat := op(1, apply_ini(transmat, ini, eps, precision, symbolic_ini));
    absolute_precision_warning();
    sol_value_from_matrix(deq, yofz, path[-1], resmat, monomials, ord);
end proc:

# See ?NumGfun,transition_matrix
transition_matrix := proc(userdeq::hrdeq, yofz::function(name),
                Path::path, precision::posint:=Settings:-default_eval_precision,
                { usebitburst::boolean:=true, forcepath::boolean:=false }, $ )
                ::Matrix(complex(float));
    local deq, path;
    deq := userdeq;
    if type(deq, 'set') and nops(deq) > 0 then
        WARNING("initial conditions %1 will be ignored",
            remove(has, deq, op(yofz)));
        deq := bare_diffeq(deq, yofz);
    end if;
    deq := diffeqtohomdiffeq_warn(deq, yofz);
    path := `if`(forcepath,
        Path,
        rewrite_path(deq, yofz, Path, usebitburst, 'fromzero'=false));
    absolute_precision_warning();
    nthterm:-makeitfloat(path_transition_matrix(deq, yofz, path,
                                                   10^(-precision)), precision);
end proc:

###############################################################################
## Monodromy
###############################################################################

local_monodromy := proc(deq, yofz, z0, start, precision, $)
    local rad, path, k;
    rad := start - z0;
    path := [ seq(convert(evalf[2](z0 + rad * exp(k*2*I*Pi/17)), rational, exact), k=0..17) ];
    transition_matrix(deq, yofz, path, precision);
end proc:

monodromy := proc()
    error("Not implemented yet.");
end proc:

end module:
