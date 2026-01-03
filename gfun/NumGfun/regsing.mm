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

## regsing.mm: Connection between ordinary and regular singular points

# This is EXPERIMENTAL, unfinished code. Current status: seems to work well
# enough on some examples, but not thoroughly tested.  Not profiled/optimized
# yet (hence very slow).  Error control is still heuristic. Algebraic singular
# points with non-rational coordinates are not handled yet.

regsing := module()

export
    `type/generalized_rec_matrix`, `type/regsing_params`,
    local_basis_monomials, local_basis_expansions,
    singular_step_transition_matrix, inverse_singular_step_transition_matrix;

local

    grm_to_list, list_to_grm, identity_grm, grm_order,

    grm_mul_doit, grm_mul, reduce_index, map_reduce_index, reduce_den,
    reduce_point, binsplit, read_abstract_RootOf, unroll,

    make_RootOf_monic, diffeq_to_local_rec, rec_matrix_num_den,
    binsplit_matrix,

    re_im_roots_resultant, is_isolated_root, cmp_algeb_re_im,
    sort_canonical_solutions, transition_matrix_from_unsorted_columns,

    map_record, apply_grm, irred_factor_sequences, irred_factor_columns,
    shift_logs, new_iniconds, make_steplist, expand_logs_and_shift_exponents,

    needed_terms_heu
    ;

# 'regsing_params' records are used to bundle various formal parameters (along
# with their properties) used in the computation.
#
# Among other things, this allows us to use 'option inline' in many procedures
# operating on GRMs (which would be impossible with lexically scoped
# module-local parameters) without having to pass along tons of parameters at
# each call.
#
# Note that the names of the parameters are consistent with the notations of
# [Mezzarobba, ISSAC 2010], but not with the binary splitting code in ancont.mm.
`type/regsing_params` := 'record'(
    "lambda",         # Formal root of monic polynomial (numerator of
                      #     valuation, up to some integer shift)
    "minpoly_lambda", # Minimal polynomial of lambda
    "delta",          # Formal perturbation, used to compute derivatives
    "order_delta",    # Order of expansions wrt delta (usually orddiffeq)
    "Shift_k",        # Symbolic shift operator
    "order_Shift_k",  # Order of truncation of series in Shift_k (usually = 
                      #     sum of multiplicities of integer-spaced roots)
    "Log"             # Log^k = log(some point of interest)^k/k!, used to
                      #     represent the sequences of coefficients of powers of
                      #     z in logarithmic power series expansions.
    # More parameters to be added to handle irrational singular points.
);

################################################################################
# ``Generalized Recurrence Matrices''
################################################################################

# ``Generalized recurrence matrices'' (GRMs) are matrices of the special shape
# used to compute sums of series, with numerators and denominators kept
# separate.  Depending on the context, a GRM may be thought of either as
#
#               M = [ (coeff_mat/den)·(pow_num/pow_den)   0 ]
#                   [ sums_row/(den·pow_den)              1 ]
#
# or as
#
#   den·pow_den·M = [coeff_mat·pow_num       0     ]
#                   [    sums_row       den·pow_den].
#
# GRMs may be multiplied using 'grm_mul'.  The heart of the computation in this
# file consists in building a product tree of GRMs.  See this function for
# details.

`type/generalized_rec_matrix` := 'record'(
    "coeff_mat",    # Matrix
    "den",          # Denominator of coeff_mat
    "pow_num",      # Pure power (eval. pt, indep. on the leaf of the tree)
    "pow_den",      # Denominator of the power part
    "sums_row"      # Row matrix
);


# TODO: Faudra un jour penser à gérer efficacement les cas particuliers de Q, de
# Q[i] (aussi bien pour la valeur de la singularité que pour la valuation !), du
# calcul d'une solution sans ses dérivées, et des racines de multiplicité 1.

# TODO: Make the various reduction functions as efficient as possible.
# (En particulier, clarifier en quelles variables je veux des séries et en
# quelles variables je veux des polynômes.)

# Reduce wrt Shift_k and lambda (valuation).
reduce_index := proc(params, pol, $)
    option inline;
    map(rem,
        series(pol, params:-Shift_k, params:-order_Shift_k),
        params:-minpoly_lambda,
        params:-lambda);
end proc:

map_reduce_index := proc(params, mat, $)
    local rows, cols, i, j;
    rows, cols := op(1, mat);
    # try both versions...
    for i to rows do
        for j to cols do
            mat[i,j] := reduce_index(params, mat[i,j]);
        end do;
    end do;
    mat;
    #rtable(1..rows, 1..cols,[seq(
    #    [seq(
    #        reduce_index(params, mat[i,j]),
    #        j = 1..cols)],
    #    i = 1..rows)]);
end proc:

# Reduce wrt to lambda only; used for denominators.
reduce_den := proc(params, pol, $)
    option inline;
    rem(pol, params:-minpoly_lambda, params:-lambda);
end proc:

# Reduce wrt zeta (algebraic singular point) and delta (formal perturbation to
# compute derivatives).
reduce_point := proc(params, pol, $)
    option inline;
    #map(rem, series(pol, delta, diff_order), minpoly_zeta, zeta);
    # convert pour pouvoir ensuite réduire par rapport à lambda ; mais c'est
    # sans doute à améliorer...
    convert(series(pol, params:-delta, params:-order_delta), 'polynom');
end proc:

grm_to_list := proc(mat, $)
    option inline;
    [mat:-coeff_mat, mat:-den, mat:-pow_num, mat:-pow_den, mat:-sums_row];
end proc:

list_to_grm := proc(list, $)
    local r;
    r := types:-new_record('generalized_rec_matrix');
    r:-coeff_mat, r:-den, r:-pow_num, r:-pow_den, r:-sums_row := op(list);
    r;
end proc:

# The product of two GRMs takes the following form:
#
# [coeff_mat_h·pow_num_h       0       ] [coeff_mat_h·pow_num_l       0       ]
# [    sums_row_h       den_h·pow_den_h] [    sums_row_l       den_h·pow_den_l]
#
#          [ (coeff_mat_h·coeff_mat_l)          |                              ]
#          [             ·(pow_num_h·pow_num_l) |               0              ]
#       =  [------------------------------------+------------------------------]
#          [ sums_row_h·coeff_mat_l·pow_num_l   | (den_h·den_l)                ]
#          [       + sums_row_l·den_h·pow_den_h |      · (pow_den_h·pow_den_l) ]
#
# Dependencies:
#
#   coeff_mat   depends on  Shift_k, Lambda
#   sums_row    depends on  Shift_k, Lambda, Zeta0, delta
#   den         depends on           Lambda, Zeta0
#   pow_num     depends on                   Zeta0, delta
#   pow_den     depends on                   Zeta0
#
# I'm ignoring (some of) the dependencies on Zeta0 for now, since they seem
# a bit tricky to handle.

# This function multiplies matrices of the above shape represented by *lists* of
# fields, because this is what binsplit needs.  It takes pow_cache as input for
# the same reason.  We also provide a wrapper that works with the (nicer)
# representation by records.
#
# TODO: Use Kronecker substitution to multiply the series?
grm_mul_doit := proc(params, mat_h, mat_l, pow_cache, factor_count, $)
    local tmp1, tmp2, j,
        coeffs_mat_l, den_l, pow_num_l, pow_den_l, sums_row_l,
        coeffs_mat_h, den_h, pow_num_h, pow_den_h, sums_row_h,
        coeff_mat,   den,   pow_num,   pow_den,   sums_row;
    coeffs_mat_l, den_l, pow_num_l, pow_den_l, sums_row_l := op(mat_l);
    coeffs_mat_h, den_h, pow_num_h, pow_den_h, sums_row_h := op(mat_h);
    # coeff_mat = coeff_mat_h·coeff_mat_l
    coeff_mat := map_reduce_index(params,
                                        mvMultiply(coeffs_mat_h, coeffs_mat_l));
    # sums_row = sums_row_h·coeff_mat_l·pow_num_l + sums_row_l·den_h·pow_den_h
    tmp1 := mvMultiply(sums_row_h, coeffs_mat_l);  # reduce?
    tmp2 := den_h * pow_den_h;  # integer, no reduction
    sums_row := rtable(1..1, 1..op([1,2], tmp1),          # seems to be the most
        [[seq(                               # efficient way to create an rtable
            reduce_index(params, reduce_point(params, # TBI: optim? order?
                pow_num_l * tmp1[1,j] + tmp2 * sums_row_l[1,j])),
            j=1..op([1,2], tmp1))]],
        'subtype' = 'Matrix');
    # pow_num = pow_num_h·pow_num_l; pow_den = pow_den_h·pow_den_l
    if assigned(pow_cache[factor_count]) then
        userinfo(10, 'gfun', "using pow_cache");
        pow_num, pow_den := pow_cache[factor_count];
    else
        pow_num := reduce_point(params, pow_num_h * pow_num_l);
        pow_den := pow_den_h * pow_den_l;  # integer, no reduction
        pow_cache[factor_count] := pow_num, pow_den;
    end if;
    # den = den_h·den_l
    den := reduce_den(params, den_h * den_l);
    # return
    [coeff_mat, den, pow_num, pow_den, sums_row];
end proc:

# Multiply GRMs represented by records.
grm_mul := proc(params, mat_h, mat_l, $)
    list_to_grm(grm_mul_doit(params, grm_to_list(mat_h), grm_to_list(mat_l),
                                                                   table(), 0));
end proc:

# For use by 'unroll'.  Builds the product tree.
# Requires high > low (strict inequality).
binsplit := proc(params, gen_factor, n, low, high, pow_cache, $)
    local factor_count, mid, mat_l, mat_h;
    factor_count := high - low;
    if factor_count = 1 then  # TODO: binary splitting threshold
        eval(gen_factor, n = low);
    else
        mid := iquo(low + high, 2);
        mat_l := binsplit(params, gen_factor, n, low,  mid, pow_cache);
        mat_h := binsplit(params, gen_factor, n, mid, high, pow_cache);
        grm_mul_doit(params, mat_h, mat_l, pow_cache, factor_count);
    end if;
end proc:

identity_grm := proc(ord, $)
    Record(
        ':-coeff_mat' = LinearAlgebra:-IdentityMatrix(ord),
        ':-den' = 1,
        ':-pow_num' = 1,
        ':-pow_den' = 1,
        ':-sums_row' = Matrix(1, ord));
end proc:

grm_order := proc(grm, $)
    option inline;
    op([1,1], grm:-coeff_mat);
end proc:

# "Unroll" a (generalized) sequence by multiplying the GRMs 'fac'('i') for
# consecutive values 'low_idx' <= i < 'high_idx' of the parameter 'n'.
#
# Note to self: Je voudrai peut-être (re)déplacer plus de choses ici. (Essayer
# de gérer plus intelligemment la différence entre algeb_shift + Z et Z ?)
unroll := proc(params, fac, n, low_idx, high_idx, $)
    if high_idx <= low_idx then
        identity_grm(grm_order(fac));
    else
        list_to_grm(
            binsplit(params, grm_to_list(fac), n, low_idx, high_idx, table()));
    end if;
end proc:

################################################################################
# Symbolic computation of the GRM used to "unroll" the local expansions of the
# solutions of a differential equation
################################################################################

# XXX: Où est-ce que je gère les valeurs de z0 et z1 ??? typiquement, c'est z0
# qui n'est pas rationnel !

read_abstract_RootOf := proc(alg, $)
    if type(alg, 'And'('algnum', 'abstract_rootof')) and nops(alg) = 1
                                and type(op(alg), 'polynom'('integer', _Z)) then
        op(alg)
    else
        error "expected: RootOf(pol(_Z)) where pol has integer coefficients"
    end if;
end proc:

# Given a = RootOf(pol in Z[_Z]), computes a monic polynomial m and an integer
# lc such that pol(z) = 0 iff m(lc·z) = 0.  Returns RootOf(m)/lc, i.e. the
# abstract algebraic number a represented as the quotient of an abstract
# algebraic integer and an integer denominator.
make_RootOf_monic := proc(alg, $)
    local pol, lc, new_modulus;
    pol := read_abstract_RootOf(alg);
    lc := lcoeff(pol, _Z);
    new_modulus := sort(numer(subs(_Z = _Z/lc, pol)));
    RootOf(new_modulus)/lc;
end proc:

# Express the general coefficient u(n) of the series expansion of the
# solutions of deq at z0 as a function of u(n-1), ..., u(n-s).  This should
# work even for symbolic z0.
#
# TODO: add option remember, and ensure it is actually used?
diffeq_to_local_rec := proc(deq, yofz, z0, uofn, $)
    local y, z, local_deq, rec, ord, n;
    y, z := getname(yofz);
    local_deq := algebraicsubs(deq, y = z0 + z, yofz);
    rec := purediffeqtorec(local_deq, yofz, uofn);
    ord := ordrec(rec, uofn);
    n := op(uofn);
    subs(n = n-ord, rec);
end proc:

# Compute the "generalized recurrence matrix" associated to rec, that is, a
# matrix M of truncated series in Shift_k, depending on n, such that
#   M(n)·(u_(n,k))_k = (u_(n+1,k))_k
# for the natural action of such matrices on sequences (u_k)_k.
#
# (Note that, although the general form of the recurrence relation does not
# depend of algeb_shift, the matrix does, once we introduce lambda.)
#
# INPUT:
# params - as usual; only Shift_k and order_Shift_k are used (XXX: is that
#     a good choice? should we drop params here? or should we use lambda too,
#     instead of algeb_shift?)
# rec - linear equation in u(n), u(n-1), u(n-2), ... (NOT u(n+1) etc.!)
# uofn - variables: u(n)
# algeb_shift - rational or abstract algebraic number; root (or conjugate roots)
#     of the indicial equation to consider; the "algebraic part" of the indices
# int_idx - EITHER a symbol (generic case: algeb_shift + int_idx not root of the
#     indicial polynomial) OR an integer (singular shift of algeb_shift)
# mult - multiplicity of algeb_shift + int_idx as a root of the indicial
#     polynomial, in the symbolic case
#
# OUTPUT:
# num - A matrix whose entries are polynomials in the index, Shift_k, and a
#     RootOf representing an (abstract) algebraic integer (if needed to express
#     the indices), with integer (as opposed to rational) coefficients.
# den - A single polynomial of the same type (except that it does not depend
#     on Shift_k)
# ...such that M = num/den.
rec_matrix_num_den := proc(params, rec, uofn, algeb_shift, int_idx, mult, $)
    uses LinearAlgebra;
    local Sk, ordSk, recop, ord, lc, invlc, charpoly, den, num, mat, k,
        horner_vars, Shift_n;
    Sk, ordSk := params:-Shift_k, params:-order_Shift_k;
    recop := rec_to_recop(rec, uofn, Shift_n);  # polynomial in 1/Shift_n
    ord := -ldegree(recop, Shift_n);
    # int_idx = integer index, while n \in algeb_shift + Z.  (Note: while Sk
    # must be introduced here, it might be better to introduce n only after
    # computing invlc.)
    recop := subs(op(uofn) = algeb_shift + int_idx + Sk, recop);
    lc := lcoeff(recop, Shift_n);  # aka Q0 aka indicial polynomial(n+Sk)
    # The plain series command is too dumb to find Laurent series at algebraic
    # poles.  As far as I understand, using a custom Normalizer() is supposed to
    # do the job, but I have not managed to make it work.  Besides, there are
    # nasty 'option remember' issues.
    invlc := convert(MultiSeries:-series(Sk^mult/lc, Sk, ordSk), 'polynom');
    # - Alternative: radnormal(..., 'rationalized').
    # - Perhaps collect [Shift_n, Sk].
    # - Would it be better to first collect Shift_n and then normalize? Hum,
    #   non, ça n'a pas l'air d'une bonne idée : je veux au final un seul
    #   dénominateur.
    # - L'appel explicite à series() pourrait sans doute être remplacé par un
    #   appel à reduce_index. Mais est-ce mieux ? reduce_index est fait pour
    #   travailler avec Lambda.
    charpoly := convert(series(invlc*recop, Sk, ordSk), 'polynom');
    charpoly := collect(evala('Normal'(charpoly)), Shift_n, normal);
    den := denom(charpoly) * Shift_n^(ldegree(charpoly, Shift_n));
    # The idea here is (i) to collect the coefficients corresponding to each
    # entry of the matrix [collect(Shift_n)]; (ii) to make it as easy as
    # possible to compute the remainder of a product of such entries modulo the
    # polynomial appearing in the RootOf; (iii) to make the remaining
    # polynomials in int_idx easy to evaluate [Horner]. Is collecting RootOf's
    # really of any use for (ii)? Should we collect Sk's too? (Probably
    # not.)
    horner_vars := select(type, [int_idx], 'symbol');
    num := collect(
        den*charpoly,
        [Shift_n, RootOf],
        proc(rat) convert(normal(rat), 'horner', horner_vars) end proc);
    # Build an 'empty' companion matrix; then modify last row.
    # (Sauf erreur, la présence du numérateur et du dénominateur dans la matrice
    # fait qu'il ne peut pas y avoir de facteurs communs à sortir et mettre dans
    # pow_num/pow_den.)
    mat := den * Transpose(CompanionMatrix(Shift_n^(ord)));
    mat[-1] := Matrix([seq(-coeff(num, Shift_n^(-ord+k)), k=0..ord-1)]);
    [mat, den];
end proc:

# Compute the GRM associated to a recurrence relation.
#
# Input:
# params, rec, uofn, algeb_shift, int_idx, mult - As in rec_matrix_num_den
# pt - evaluation point (in Q(i))
#
# Note: The entries of sums_row correspond to a basis of solutions of the
# *recurrence relation*; they must not be confused with the fundamental
# solutions of the differential equation.  We need the complete row (whose
# entries will be combined according to the initial conditions) to compute any
# single solution of the differential equation.
#
# Note: It is convenient to have this function take 'params' as an argument
# since it uses 'irred_factor', 'diff_order' & co.  It is less pleasant that it
# also needs 'algeb_shift', but I see no cleaner organization for now.  (An
# option might be to replace all indeterminates by RootOf-like structures that
# encode their properties, e.g., "Sk+O(Sk^ord)".)
binsplit_matrix := proc(params, rec, uofn, pt, algeb_shift, 
                                              int_idx := op(uofn), mult := 0, $)
    local coeff_mat, mat, ord;
    ASSERT(mult = 0 or type(int_idx, 'integer'));
    mat := types:-new_record('generalized_rec_matrix');
    # Note to self: C'est un peu arbitraire de faire cette substitution ici (ça
    # permettra peut-être de garder des RootOf si on en a envie).
    coeff_mat := rec_matrix_num_den(params, rec, uofn, algeb_shift, int_idx,
                                                                          mult);
    if degree(params:-minpoly_lambda, params:-lambda) > 1 then
        coeff_mat := subs(RootOf(params:-minpoly_lambda) = params:-lambda,
                                                                     coeff_mat);
    end if:
    mat:-coeff_mat, mat:-den := op(coeff_mat);
    mat:-pow_num := series(numer(pt + params:-delta), params:-delta, 
                                                           params:-order_delta);
    mat:-pow_den := denom(pt);  # TODO: update for eval at algebraic points
    ord := op([1,1], mat:-coeff_mat);  # ordrec does not work on 'rec'
    mat:-sums_row := Matrix([[0 $ (ord-1), mat:-den * mat:-pow_den]]);
    mat := subs(RootOf(params:-minpoly_lambda) = params:-lambda, mat);
    eval(mat);
end proc:


################################################################################
# Canonical local basis
################################################################################

re_im_roots_resultant := proc(Pol, z, ReIm, $)
    local x, y, pol, res;
    ASSERT(irreduc(Pol));
    pol := evalc(subs(z = x + I*y, Pol));
    res := resultant(evalc(Re(pol)), evalc(Im(pol)), evalc(ReIm(x+I*y)));
    sqrfreepart(res);
end proc:

is_isolated_root := `infsolvepoly/isroot`;

# Three-way comparison of real or imaginary parts of algebraic numbers given
# as RootOf(poly(_Z), approx or index) + shift.  It is assumed that the two
# polynomials are not obtained from one another by integer shifts of the
# variable.
#
# Output:
#   -1 if a < b
#    0 if a = b
#    1 if a > b
cmp_algeb_re_im := proc(a, b, ReIm, $)
    local fa, fb, fdiff, split_alg_int, na, ra, nb, rb, pola, polb, approx, g;
    fa, fb := evalf(a), evalf(b);
    fdiff := ReIm(fa) - ReIm(fb);
    if abs(fdiff) > Float(1, 2 - Digits) then  # obviously different
        sign(fdiff);
    elif Re(a) = Re(b) then  # obviously equal
        0
    else  # likely equal
        split_alg_int := proc(u)
            if type(u, `+`) then
                selectremove(type, u, 'integer')
            else
                0, u
            end if;
        end proc:
        na, ra := split_alg_int(ra);
        nb, rb := split_alg_int(rb);
        if na = nb and is_simple_RootOf(ra) and is_simple_RootOf(rb) then
            # check that pola and polb have a common root close to approx, and
            # that none of them has two such roots
            pola, polb := op(1, ra), op(1, rb);
            approx := ReIm(fa - na);
            g := gcd(
                re_im_roots_resultant(pola, _Z, ReIm),
                re_im_roots_resultant(polb, _Z, ReIm));
            if is_isolated_root(approx, g)
                        and not is_isolated_root(approx, pola/g, _Z)
                        and not is_isolated_root(approx, polb/g, _Z) then 0;
            elif Digits < 200 then Digits := 2*Digits; cmp_algeb_re_im(_params);
            else error "unable to compare %3(%1) and %3(%2)", a, b, ReIm
            end if;
        end if;
    end if;
end proc:

# Whether a < b in the order given by their fields 'sort_key'.
sort_canonical_solutions := proc(a, b, $)
    local ka, kb, s, t;
    ka := a:-sort_key; kb := b:-sort_key;
    s := cmp_algeb_re_im(ka:-valuation, kb:-valuation, Re);
    if s <> 0 or ka:-log_power <> kb:-log_power then
        evalb((s < 0) or (s = 0 and ka:-log_power > kb:-log_power))
    else
        t := cmp_algeb_re_im(ka:-valuation, kb:-valuation, Im);
        if t <> 0 then
            evalb(t < 0)
        else
            error "bug: two fundamental solutions with the same parameters!"
        end if;
    end if;
end proc:

# Sorts the entries of the list 'cols' according to the keys in their fields
# 'sort_key', and build a matrix whose columns are given by the fields 'col'.
transition_matrix_from_unsorted_columns := proc(pt, cols, $)
    local sorted_cols, Z0, Z;
    sorted_cols := sort(cols, sort_canonical_solutions);
    userinfo(2, 'gfun', Z0=pt, "generalized valuations of the local basis" = 
        map(
            c -> Z^c:-sort_key:-valuation*log(z)^c:-sort_key:-log_power,
            sorted_cols));
    Matrix(map(c -> c:-col, sorted_cols));
end proc:

# Compute the the coefficients up to z^ord of the canonical basis of sols of deq
# in z0, by diverting the binary splitting code.
local_basis_expansions := proc(Deq::hrdeq, yofz::function(name),
            z0::complex(numeric), ord::posint:=Order, $)
    local deq, z, res;
    deq := diffeqtohomdiffeq_warn(Deq, yofz);
    z := op(yofz);
    res := convert(
            singular_step_transition_matrix(deq, yofz, z0, z0+z, z^ord, true),
            'list');
    res := convert(res, 'radical');
    res := map(sort, collect(res, z, radnormal), [z, ln(z)], 'ascending');
    res := combine(res, 'power');
    res := subs(z=z-z0, res);
end proc:

# Compute the structure of the canonical basis of sols of deq (which should be
# homogeneous) at z0 in human-readable form.  The output is a a list of
# expressions of the form series(distinguished monomial + `...`).  We cannot
# directly use local_basis_expansions since it will compute more than the first
# term of some of the expansions in some cases, and it does not seem easy to
# retrieve just the monomial we want, hence some copy-paste...
local_basis_monomials := proc(deq, yofz, z0, $)
    local rec, u, n, Shift_n, indicial_pol, facs, fac, i, k, basis, Z, sol,
        `...`;
    rec := diffeq_to_local_rec(deq, yofz, z0, u(n));
    indicial_pol := factor(lcoeff(rec_to_recop(rec, u(n), Shift_n), Shift_n));
    facs := factors(indicial_pol);
    basis := [seq(seq(seq(
        Record(':-sort_key' = Record(
            ':-valuation' = RootOf(fac[1], n, 'index'=i),
            ':-log_power' = k)),
        k=0..fac[2]-1), i=1..degree(fac[1])), fac in facs[2])];
    basis := sort(basis, sort_canonical_solutions);
    Z := op(yofz) - z0;
    [seq(
        series(Z^sol:-sort_key:-valuation * log(Z)^sol:-sort_key:-log_power /
                            sol:-sort_key:-log_power! + `...`, `...`, infinity),
        sol in basis)];
end proc:



################################################################################
# Transition matrix (regular singular point to ordinary point)
################################################################################

# Separate out those roots of the indicial equation that are larger than the
# last term of the sequence we are interested in.  Build a list of rec unrolling
# steps from the remaining shifts and the index of this last term.
# Input:
#   sl_group: [sl_factor, [ [shift,mul], ... ]] (from the output of
#       PolynomialTools[ShiftlessDecomposition])
#   goal: the index of the term we wish to compute
# Output:
# - shiftless factor taken from sl_groups
# - list of steps of the form Record(shift=integer amount by which to shift the
#   reference root of sl_factor, mult=multiplicity of the corresponding root),
#   ending with the index 'goal' (which may or may not be a root of the indicial
#   polynomial)
# - list of "steps" corresponding to (logarithmic) series of valuation greater
#   than 'goal' (which contribute to the dimension of the solution space, but
#   are indistinguishable from zero at the expansion order 'goal').
make_steplist := proc(sl_group, goal, $)
    local sl_factor, shifts, shifts_below_goal, shifts_above_goal;
    sl_factor, shifts := op(sl_group);
    # Convert shift to Record form to ease later use.
    # Note: the shifts in sl_group are shift of the polynomial indeterminate; we
    # want shifts that apply to the value of the root.
    shifts := map(
        proc(u) Record(':-shift'=-u[1], ':-mult'=u[2]) end,
        ListTools:-Reverse(shifts));  # voire sort ?
    shifts_below_goal, shifts_above_goal := selectremove(
        proc(s) s:-shift <= goal end proc,
        shifts);
    # If (as will usually be the case) goal does not correspond to a root of the
    # indicial polynomial, add it to the step list.
    # (XXX: I'm not sure I like this organisation.)
    if shifts_below_goal[-1]:-shift <> goal then
        shifts_below_goal := [op(shifts_below_goal), Record(':-shift'=goal,
                                                                   ':-mult'=0)];
    end if;
    sl_factor, shifts_below_goal, shifts_above_goal;
end proc:

# Create the column of "generalized initial values" (written in Shift_k rather
# than with logs) corresponding to the canonical solution (or group of conjugate
# solutions) of "generalized valuation" (algeb_shift + shift, multiplicity)
# given by 'valuation':
#
# y(n-s+1) = ... = y(n-1) = 0; y(n) = Log^v; sum(y(j), j=0..n-1) = 0
#
# With option 'zero', create null "generalized initial values" instead.
new_iniconds := proc(params, valuation, ord, {zero := false}, $)
    local v, ini;
    if zero then ini := 0 else ini := params:-Log^v end if;
    [seq(
        Record(
            ':-shift' = valuation:-shift,
            ':-log_power' = v,
            ':-column' = Record(
                ':-coeff_mat' = Matrix(ord, 1, {(ord,1) = eval(ini)}),
                ':-den' = 1,
                ':-pow_num' = 1,
                ':-pow_den' = 1,
                ':-sums_row' = [[0]])),
        v = 0..(valuation:-mult)-1)];
end proc:

# Map over records.  May not work in all cases.  Type annotations get lost.
map_record := proc(fun, rec, $)
    local names, vals, field, i;
    names := exports(rec);
    # Note that fun may very well return a sequence (hence the brackets).
    vals := seq([fun(eval(field))], field in exports(rec, 'instance'));
    Record(seq(names[i] = op(vals[i]), i = 1..nops([names])));
end proc:

# Apply a GRM of shift operators (i.e., polynomials/series in Shift_k) to a GRM
# of polynomials in Log, according to the rule:
#     Shift_k·Log^k = Log^(k-1), k > 0
#     Shift_k·Log^0 = 0.
# (In other words, a_0+a_1·Log+··· represents the sequence [with finite support]
# (a_k)_(k in N), and Shift_k acts naturally.)  Column GRMs involving Log are
# used in 'singular_step_transition_matrix'  to represent "partially unrolled"
# expansions of solutions of the differential equation.
apply_grm := proc(params, grm, col, $)
    local rewrite, rewrite2, ini_s, res_s;
    rewrite := proc(f) subs(params:-Log = params:-Shift_k^(-1), f) end proc;
    ini_s := map_record(rewrite, col);
    res_s := grm_mul(params, grm, ini_s);
    rewrite2 := proc(expr)
        subsindets['flat'](
            subs(params:-Shift_k = params:-Log^(-1), expr),
            {'specfunc'('anything',O), 'identical'(params:-Log)^'negint'},
            proc() 0 end proc);
    end proc:
    map_record(rewrite2, res_s);
end proc:

# Shift the entries of a grm by Shift_k^amount.  Used on vectors representing
# fundamental solutions when we need to "cross" "exceptional" indices.  Works in
# place.
shift_logs := proc(params, col, amount, $)
    local doit;
    doit := proc(u)
        local k;
        applyrule(
            params:-Log^(k::negint) = 0,
            expand(params:-Log^amount * u))
    end proc:
    # Only the last coefficient row must be shifted!  Other coefficients rows,
    # as well as sums_row, represent coefficients that come *before* the
    # exceptional index and should be left alone.
    col:-coeff_mat[-1] := LinearAlgebra:-Map(doit, col:-coeff_mat[-1]);
    # It is convenient (though not necessary) to return the updated col
    col;
end proc:

# Computes the list of columns of the transition matrix corresponding to a given
# irreducible factor of the indicial equation. Each column is associated to a
# choice of shift and j <= mult in steplist or shifts_above_goal. The columns
# are returned in the internal format used in this file, i.e., they involve the
# parameters specified in 'params'.  The abstract algebraic number 'algeb_shift'
# is not yet instanciated for each root of irred_factor (this is done in
# irred_factor_column) nor sorted (this is the job of sort_canonical_solutions,
# once all solutions have been put together).
#
# Each column (<-> canonical solution of the *differential equation*) is
# represented as a Record with the following fields:
#   shift: valuation shift
#   log_power
#   column: the column Matrix of (abstract, i.e. w/o root choice) corresponding
#       to the value at the current index of the sequence of vectors of
#       coefficients and partial sums of the series given by the first-order
#       matrix recurrence relation
#
# ALGORITHM (sketch):
# for each admissible shift (= possible valuation)
#     // actually the shifts > goal are handled separately
#     add "new" solutions (those with nonzero intial value at this shift)
#     compute the recurrence matrix from that shift to the next one
#     extend all known solutions (including the new ones) using this matrix
irred_factor_sequences := proc(params, rec, uofn, pt, algeb_shift, steplist,
                                                              shifts_above_goal)
    local grm, ordrec, sequences, k, curstep, nextstep, unrollmat, singmat,
        seq_data, s;
    # The recurrence matrix (generic case).  Computed here for consistency with
    # the exceptional case. 
    grm := binsplit_matrix(params, rec, uofn, pt, algeb_shift);
    ordrec := op([1,1], grm:-coeff_mat);
    # The list of partially unrolled fundamental sequences.
    sequences := new_iniconds(params, steplist[1], ordrec);
    # Unroll the generalized recurrence relation
    for k to nops(steplist) - 1 do  # for each shift <= goal...
        curstep := steplist[k]; nextstep := steplist[k+1];
        # UNROLL: compute mat curstep:-shift -> nextstep:-shift
        # XXX: possibilité d'augmenter order_Shift_k progressivement, pour tenir
        # compte du fait que les « premières » solutions fondamentales ont
        # « peu » de Log ?
        unrollmat := unroll(params, grm, op(uofn), curstep:-shift + 1,
                                                               nextstep:-shift);
        # HANDLE EXCEPTIONAL STEP (Including the exceptional matrix among those
        # multiplied by binsplit would likely be more efficient, but it also
        # looks more complicated.)
        singmat := binsplit_matrix(params, rec, uofn, pt, algeb_shift,
                                               nextstep:-shift, nextstep:-mult);
        unrollmat := grm_mul(params, singmat, unrollmat);
        # EXTEND KNOWN SOLUTIONS: get the value of the solutions at
        # n=nextstep:-shift: apply matrix, then shift logs to fix the result.
        # Note: This could easily be adapted to work with several columns at
        # once, should we decide to represent several solutions with "similar"
        # parameters in a single matrix.
        for seq_data in sequences do
            seq_data:-column := apply_grm(params, unrollmat, seq_data:-column);
            shift_logs(params, seq_data:-column, nextstep:-mult);
        end do;
        # ADD NEW SOLUTIONS with exponent n='curstep:-shift':
        sequences := [op(sequences), op(new_iniconds(params, nextstep, ordrec))];
    end do;  # shift
    # Add zero columns for shifts greater than goal
    sequences := [
        op(sequences),
        seq(
            op(new_iniconds(params, s, ordrec, 'zero')),
            s in shifts_above_goal)];
end proc:

# Keeping this function separate may prove convenient later.
expand_logs_and_shift_exponents := proc(params, abstract_sum, pt, expnt_shift, $)
    local subs_log, k;
    subs_log := {seq(params:-Log^k = log(pt+params:-delta)^k/k!,
                                               k = 0..params:-order_Shift_k-1)};
    series(
        (pt + params:-delta)^expnt_shift * subs(subs_log, abstract_sum),
        params:-delta, params:-order_delta);
end proc:

# Get rid of the parameters defined by 'params' in the encoding of the
# solutions.
#
# TODO: Horner?  Reorder the rewriting steps for efficiency?
irred_factor_columns := proc(params, sequences, algeb_shift, pt)
    local POINT, numden, asl, cols, abstract_seq, num, abstract_col, j, idx,
        concrete_lambda, newcol;
    ### PROVISOIRE
    numden := `/`;
    ###
    asl := subs(RootOf(params:-minpoly_lambda) = params:-lambda, algeb_shift);
    ASSERT(not has(asl, RootOf));
    cols := [];
    for abstract_seq in sequences do
        # Shifting exponents requires computations in Q[lambda], hence it should
        # be done before introducing 'concrete_lambda'.
        num := expand_logs_and_shift_exponents(params,
                                            abstract_seq:-column:-sums_row[1,1],
                                            POINT, abstract_seq:-shift + asl);
        abstract_col := Matrix([seq(
            [numden(
                coeff(num, params:-delta, j),
                abstract_seq:-column:-den * abstract_seq:-column:-pow_den)], 
            j = 0..params:-order_delta - 1)]);
        for idx to degree(params:-minpoly_lambda) do
            # XXX: indeq_root = evalf_absolute_prec(lambda, roots_prec)...
            concrete_lambda := RootOf(params:-minpoly_lambda, 'index'=idx);
            newcol := Record(
                ':-col' = subs(params:-lambda = concrete_lambda, abstract_col),
                ':-sort_key' = Record(
                    ':-valuation' = subs(params:-lambda = concrete_lambda, asl)
                                                  + abstract_seq:-shift,
                    ':-log_power' = abstract_seq:-log_power));
            LinearAlgebra:-Map2(subs, POINT = pt, newcol:-col);
            cols := [op(cols), newcol];
        end do;  # idx (index of root of irred_factor)
    end do;  # abstract_seq
end proc:

needed_terms_heu := proc(deq, yofz, z0, rad, epsilon, $)
    local params, bound, n;
    params := numeric_bounds:-bound_fundamental_solutions_exact_point(deq, yofz,
        z0, 'wini'=false);
    bound := bounds:-tail_bound(op(params), rad, n, 'deriv'=0,
                                                    'simplify_hgeom'=false);
    if type(bound, 'SymbolicInfinity') then 
        error "no finite bound for series tail"
    end if;
    _EnvNumGfunExtendEvalrC := ["GAMMA", "Hypergeom"];
    2*numeric_bounds:-dicho_solve_ineq(
        proc(k) above(eval(bound, n=k)) end proc,
        epsilon);
end proc:

# Computes the transition matrix from a regular singular point to an ordinary
# point nearby.
#
# Input:
#   deq, yofz - as usual
#   z0, z1 - ends of the step, z0 may be a (regular) singular point of deq, and
#       z1 may to some extent contain symbolic parameters
#   epsilon - target precision -- either epsilon=z^g, in which case goal=g
#       unconditionally, or epsilon>0 (allowed only if z1 is purely numeric)
#
# NOTE: In particular, this function allows to compute (though not very
# efficienlty) the local basis itself, using z1=z0+z.
#
# NOTE: The local bases at singular points involve multivalued functions (logs
# and non-integer powers).  The rule to ``choose which determination we connect
# to'' is that (i) all logs in the local basis are interpreted as ``standard''
# log(sing+z) [analytic on a slit plane of the form sing+(C\R_-)]; and (ii) the
# linear combination of these functions we compute coincides with the D-finite
# function defined by analytic continuation near the end of the final step of the
# path.  In other words, whether the analytic continuation of log(z-1) [given by
# initial values in 0] into its singularity in 1 along a path that does *not*
# wind around z=1 yields 1·log(z-1) or 1·log(z-1)-2πi depends on the direction
# of the last segment of the path.
#
# ALGORITHM (sketch):
# for each shiftless factor group of the indicial equation
#     build a list of admissible shifts
#     for each irreducible factor of the shiftless factor
#         compute the values of the solutions associated to that factor
#         (this involves calling irred_factor_sequences to compute the
#         "abstract" solutions, and then irred_factor_columns to deduce the
#         "concrete" entries of the transition matrix)
#
# (Décidément, on dirait qu'on perd un facteur #racines deux à deux non
# décalages, i.e. somme des degrés des shiftless factors, par rapport au cas
# ordinaire -- bon, ce n'est peut-être pas si terrible que ça, après tout.)
singular_step_transition_matrix := proc(deq, yofz, z0, z1, epsilon,
                                                       first_row_only:=false, $)
    local Shift_n, rec, u, n, indicial_pol, columns, params, sl_decomp,
        sl_group, sl_factor, steplist, shifts_above_goal, step, irred_factor,
        pt, algeb_shift, sequences, m, mat, goal;
    rec := diffeq_to_local_rec(deq, yofz, z0, u(n));
    indicial_pol := lcoeff(rec_to_recop(rec, u(n), Shift_n), Shift_n);
    # goal = number of terms to sum; more precisely, if a is the "largest" among
    # a group of roots of the indicial polynomial that differ by integers, the
    # generalized series whose valuation is in this group will be computed to
    # the order O(z^(a+goal)). TODO: compute goal from epsilon rigorously.
    if type(epsilon, 'numeric') then
        goal := needed_terms_heu(deq, yofz, z0, above_abs(z1-z0), epsilon);
    elif type(epsilon, 'monomial'(1, op(yofz))) then
        goal := degree(epsilon, op(yofz));
    end if;
    userinfo(4, 'gfun', "truncation order" = goal);
    columns := [];
    # Not sure if initializing params here (or using it at all!) is a
    # good choice, but right now this seems the easiest way to go.
    params := types:-new_record('regsing_params');
    params:-order_delta :=`if`(first_row_only, 1, orddiffeq(deq, yofz));
    # sl_decomp = list of [sl_factor, list of [shift, mul]] ('shift' is the
    # amount by which to shift the variable of sl_factor, not its roots).
    sl_decomp := op(2, PolynomialTools:-ShiftlessDecomposition(indicial_pol,n));
    for sl_group in sl_decomp do
        sl_factor, steplist, shifts_above_goal := make_steplist(sl_group, goal);
        # The maximum number of logs that can appear in the result of this
        # particular compuration is the sum of the multiplicities of the roots
        # "below the goal".  (NOTE: It may in fact be possible to change it
        # after each binary splitting step, but it will make matters a bit more
        # complex--think binsplit_matrix.)
        params:-order_Shift_k := add(m, m=[seq(step:-mult, step=steplist)]);
        for irred_factor in op(2, factors(sl_factor)) do
            ASSERT(op(2,irred_factor)=1, "non-squarefree shiftless factor?!");
            irred_factor := op(1, irred_factor);
            if degree(irred_factor) = 1 then
                algeb_shift := solve(irred_factor, n);
                params:-minpoly_lambda := params:-lambda;
            else
                algeb_shift := make_RootOf_monic(RootOf(irred_factor));
                params:-minpoly_lambda := subs(_Z=params:-lambda,
                                                        op(numer(algeb_shift)));
            end if;
            pt := z1 - z0;
            sequences := irred_factor_sequences(params, rec, u(n), pt,
                                      algeb_shift, steplist, shifts_above_goal);
            # At this point, 'sequences' contains the value of all partial sums
            # corresponding to a given irreducible shiftless factor; but still
            # in abstract form, with unevalated Logs and algebraic numbers.  It
            # remains to replace elements of number fields by their complex
            # images and Logs by their values.)
            columns := [
                op(columns),
                op(irred_factor_columns(params, sequences, algeb_shift, pt))];
        end do;  # irred_factor
    end do;  # sl_group
    mat := transition_matrix_from_unsorted_columns(z0, columns);
    if type(z1, 'complex'('numeric')) then
        mat := ndmatrix_approximation(mat, -ilog10(epsilon)+1);
    end if;
    mat;
end proc:

# Compute the regular singular transition matrix from ***z1*** to ***z0***,
# where z0 is the possibly-singular point.
inverse_singular_step_transition_matrix := proc(deq, yofz, z0, z1, epsilon,
                                                       first_row_only:=false, $)
    local approx, det_mat, norm_mat, eps_mat, mat, inv, saved_infolevel;
    uses LinearAlgebra;
    # XXX: remplacer ce truc complètement heuristique par une version rigoureuse
    # Kludge to prevent some intrusive output
    saved_infolevel := :-infolevel[gfun]; :-infolevel[gfun] := 0;
    approx := singular_step_transition_matrix(deq, yofz, z0, z1, Float(1, -5));
    :-infolevel[gfun] := saved_infolevel;
    norm_mat := numdenmatrix:-bound_norm(approx);
    approx := `/`(op(approx));
    det_mat := evalf(Determinant(approx));
    eps_mat := 1/100 * (abs(det_mat)/norm_mat)^2 * epsilon;
    mat := singular_step_transition_matrix(deq, yofz, z0, z1, eps_mat, false);
    mat := `/`(op(mat));
    if first_row_only then
        inv := Transpose(LinearSolve(
            Transpose(mat),
            Matrix([[1], [0]$(orddiffeq(deq, yofz)-1)])));
    else
        inv := MatrixInverse(mat);
    end if;
    inv := ndmatrix_approximation(inv, -ilog10(epsilon)+1);
end proc:

end module:
