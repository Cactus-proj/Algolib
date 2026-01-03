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

## utilities.mm
## Marc Mezzarobba, Algorithms project, INRIA Rocquencourt

# Utilities can not be just a module (at least, a submodule of NumGfun), beacuse
# we want its exports to be available to other submodules under their
# unqualified name, but 'use foo' works only when foo is a fully constructed
# module, which does not seem to be the case until the end of the definition of
# foo's englobing module(s). (Did I already say how much I like the Maple
# "programming" language?)

# Those utility functions that we want to make available without the utilities:-
# prefix.  Right now, these are just the exports of the module utilities below.
# Assignments are after the definition.

$define UTILITIES                                                             \
    evalf_rnd, rndu, rndz, rndd, rndn, rndi, upper, lower, below, above,      \
    ratabove, evalf_abs_error, evalf_complex_abs_error, my_shake,   \
    ndmatrix_approximation, thetadeq, diffeq_for_derivative, getname,         \
    ordfrec, orddiffeq, ordrec, rec_to_recop,      \
    rec_has_constant_coefficients, read_diffeq, read_rec, diffeq_lcoeff,      \
    diffeq_singularities, diffeq_infsing, bare_diffeq, bare_rec,              \
    simplify_RootOf, set_mode, reset_mode, argmax, falling_factorial,   \
    colinear, ratbelow, Digits_plus, sprint_small_approx,                     \
    sqrfreepart, purerectodiffeq, evalrCf, above_abs, below_abs,              \
    complex_iv_diameter, iter_approx, abs_with_RootOf, mapReIm,         \
    purediffeqtorec, fail_if_Digits_too_large, singularity_type,              \
    iv_contains_zero, algebraic_roots, is_simple_RootOf, complex_iv_contains, \
    approx_to_indexed_RootOf, make_RootOfs_indexed, myevalrC, poormansevalrC, \
    diffeqtohomdiffeq_warn, approx_abs_error, complex_iv_center, \
    poormansevalrC_warn


local tmp;
local UTILITIES;

utilities := module()

option package;

export UTILITIES;
local rnd_mode_aliases, above_below, read_diffeq_or_rec, `my_abs/RootOf`,
    myevalrC_ext;

################################################################################
## Directed rounding, interval arithmetic, etc.
################################################################################

Digits_plus := proc(n, $)
    if Digits + n < 1 then
        error("Digits should be at least %1", 1 - n)
    end if;
    Digits + n
end proc:

## evalf with directed rounding

# These functions allow to write expressions that mix different rounding mode,
# like rndz(`/`, rndz(a), rndu(b)).

# Examples:
# > rndu(.3333333333333333333333333);
#                                  0.3333333334
# > rndz(.3333333333333333333333333);
#                                  0.3333333333
# > rndz(`/`, 2.0, 3.0);
#                                  0.6666666666
# > rndu(`/`, 2.0, 3.0);
#                                  0.6666666667
# > rndz(evalf(1/3-2/3*I), additional_prec = -2);
#                            0.33333333 - 0.66666666 I

rnd_mode_aliases := table([
    infinity    =   infinity,
    "rndu"      =   infinity,
    "u"         =   infinity,
    -infinity   =   -infinity,
    "rndd"      =   -infinity,
    "d"         =   -infinity,
    0           =   0,
    "rndz"      =   0,
    "z"         =   0,
    "nearest"   =   'nearest',
    "rndn"      =   'nearest',
    "n"         =   'nearest',
    NULL
]);

evalf_rnd := proc(rnd_mode::uneval, a,
        {additional_prec := 0, prec := Digits_plus(additional_prec)}) # no '$'
    Rounding := rnd_mode_aliases[rnd_mode];     # environment variables
    Digits := prec;                             #
    if _nrest = 0 then
        evalf(a)
    else
        evalf(a(_rest))
    end if;
end proc:

rndu := proc() # no '$'
    evalf_rnd("rndu", _passed)
end proc:

rndz := proc() # no '$'
    evalf_rnd("rndz", _passed)
end proc:

rndd := proc() # no '$'
    evalf_rnd("rndd", _passed)
end proc:

rndn := proc() # no '$'
    evalf_rnd("rndn", _passed)
end proc:

# rounding to ±infinity is not natively supported
#rndi := proc() # no '$'
#end proc:

## Complex intervals

# works for both real and complex intervals
iv_contains_zero := proc(iv, $)
    option inline;
    evalb(
        nops(iv)=2 and op(1,iv)*op(2,iv) <= 0
        or
        nops(iv)=4 and op(1,iv)*op(3,iv) <= 0
                   and op(2,iv)*op(4,iv) <= 0)
end proc:

complex_iv_diameter := proc(iv, $)
    local lx, ly, ux, uy;
    lx, ly, ux, uy := op(iv);
    Rounding := infinity;
    sqrt((ux-lx)^2 + (uy-ly)^2);
end proc;

complex_iv_contains := proc(z, iv, $)
    option inline;
    evalb(
        op(1, iv) <= Re(z) and op(2, iv) <= Im(z) and
        op(3, iv) >= Re(z) and op(4, iv) >= Im(z))
end proc:

complex_iv_center := proc(iv, $)
    option inline;
    Complex(1/2*(op(1, iv) + op(3, iv), 1/2*(op(2, iv) + op(4, iv))));
end proc:

## Complex enclosures--variants and extensions of evalrC

# evalrC will return intervals with exact bounds when possible.  This is a
# variant that returns only intervals with floating-point bounds.
evalrCf := proc(x, $)
    local lx, ly, ux, uy;
    lx, ly, ux, uy := op(evalrC(x));
    INTERVAL(rndd(lx), rndd(ly), rndu(ux), rndu(uy));
end proc:

myevalrC_ext["GAMMA"] := proc(x)
    local l, u;
    if _npassed = 1 and type(x, 'rational') then
        if Digits < 10 then Digits := 10 end if;
        l, u := op(op(shake(GAMMA(x))));
        INTERVAL(l, 0, u, 0);
    else
        error "not implemented yet: 4"
    end if;
end proc:

myevalrC_ext["Hypergeom"] := proc(upper, lower, x)
    local l, u, iv;
    iv := evalrC(x);
    # The hypergeometric functions used in our bounds have rational parameters
    # and a nonnegative argument lying inside their disks of convergence. They
    # are increasing.
    if _nrest = 0 and type(upper, 'list(And(positive,rational))')
        and type(lower, 'list(And(positive,rational))')
        and op(1, iv) >= 0. and op(2, iv)=0. and op(4,iv)=0.
    then
        if Digits < 10 then Digits := 10 end if;
        l, u := op(op(shake(evalf(Hypergeom(upper, lower, op(3, iv))))));
        INTERVAL(l, 0, u, 0);
    else
        error "not implemented yet: 4"
    end if;
end proc:

poormansevalrC_warn := proc(expr, warn, $)
    option remember; # warn only once
    if warn then
        WARNING("unable to evaluate %1 using interval arithmetic, "
            "falling back on evalf (may be inaccurate)", expr);
    else
        ASSERT(type(expr, {'function(complex(numeric))'}));
        userinfo(1, 'gfun', "emulating interval arithmetic using evalf",
            "expr"=expr);
    end if;
end proc:

# Poor man's evalrC, emulated using evalf.  Should be accurate when
# expr::function(complex(numeric)).
poormansevalrC := proc(expr, {warn:=false}, $)
    local approx, shift;
    try
        evalrCf(expr);
    catch "not implemented yet: 4":
        poormansevalrC_warn(expr, warn);
        approx := evalf[Digits+3](expr);
        shift := proc(fl, dir, $)  # compare `evalrC/INTERVAL`
            if fl = 0 then
                0.
            else
                Float(SFloatMantissa(fl)+dir*501, SFloatExponent(fl))
            end if:
        end proc:
        INTERVAL(shift(Re(approx), -1), shift(Im(approx), -1),
            shift(Re(approx), +1), shift(Im(approx), +1));
    end;
end proc:

myevalrC := proc(expr, $)
    local fun, evalrCfun, saved, tmp, res;
    if type(_EnvNumGfunExtendEvalrC, 'list') then
        for fun in _EnvNumGfunExtendEvalrC do
            evalrCfun := convert(`evalrC/` || fun, '`global`');
            saved[fun] := evalrCfun;
            assign(evalrCfun, myevalrC_ext[fun]);
        end do;
    end if;
    if _EnvNumGfunUsePoorMansEvalrC = true then
        tmp := subsindets(expr, 'function(complex(numeric))', poormansevalrC);
        try res := evalrCf(tmp) catch:
            tmp := subsindets(expr, 'specfunc'('anything', INTERVAL),
                                                             complex_iv_center);
            res := poormansevalrC(tmp, warn=true);
        end
    else
        res := evalrCf(expr);
    end if;
    if type(_EnvNumGfunExtendEvalrC, 'list') then
        for fun in _EnvNumGfunExtendEvalrC do
            evalrCfun := convert(`evalrC/` || fun, '`global`');
            assign(evalrCfun, saved[fun]);
        end do;
    end if;
    res;
end proc:

## Upper and lower bounds on absolute values

# The following two functions work for real and complex numbers as well as
# complex intervals.  When an expression contains floating-point numbers at its
# leaves, those should be enclosed using evalrC before calling above/below_abs
# to ensure that no subexpression is evaluated in pure floating-point before
# evalrC gets called.

above_abs := proc(x, {rational:=false}, $)
    local iv, lx, ly, ux, uy, res;
    iv := myevalrC(x);
    lx, ly, ux, uy := op(iv);
    Rounding := infinity;
    res := sqrt(max(lx^2+ly^2, lx^2+uy^2, ux^2+ly^2, ux^2+uy^2));
    if rational then
        res := convert(res, convert("rational", 'name'), 'exact')
    end if;
    res;
end proc:

below_abs := proc(x, {rational:=false, test_zero:=false}, $)
    local iv, lx, ly, ux, uy, nonzero_x, nonzero_y, res;
    if test_zero and Testzero(x) then return 0 end if;
    iv := myevalrC(x);
    lx, ly, ux, uy := op(iv);
    nonzero_x := evalb(sign(lx) = sign(ux) and lx <> 0.);
    nonzero_y := evalb(sign(ly) = sign(uy) and ly <> 0.);
    Rounding := 0;
    if nonzero_x and nonzero_y then
        res := sqrt(min(lx^2+ly^2, lx^2+uy^2, ux^2+ly^2, ux^2+uy^2));
    elif nonzero_x then
        res := sqrt(min(lx^2, ux^2));
    elif nonzero_y then
        res := sqrt(min(ly^2, ly^2));
    else
        error "interval %1 enclosing %2 contains zero ",
            "(if %2 is nonzero, try increasing Digits)", iv, x;
    end if;
    if rational then
        res := convert(res, convert("rational", 'name'), 'exact')
    end if;
    res;
end proc:

## Real enclosures.  We use evalrC here too.

above_below := proc(expr, idx, $)::float;
    local iv;
    iv := myevalrC(expr);
    if op(2, iv) <> 0. or op(4, iv) <> 0. then
        error "result may not be real"
    end if;
    op(idx, iv);
end proc:

below := proc() above_below(args, 1) end proc:
above := proc() above_below(args, 3) end proc:

ratabove := proc(x, $)::rational;
    convert(above(x), 'rational', 'exact')
end proc:

ratbelow := proc(x, $)::rational;
    convert(below(x), 'rational', 'exact')
end proc:

################################################################################
## Approximation with prescribed absolute error
################################################################################

# Compute one of the floating-point numbers (depending on the current rounding
# mode) res = (integer)·10^(-prec) closest to expr (hence |res-expr|<=10^(-p).
# WARNING: this works only for REAL expr and assuming that evalf(expr) actually
# returns a faithful rounding of expr.  In practice, this means that expr
# should get evaluated in a single floating-point operation, as opposed to
# several steps with intermediate roundings.
evalf_abs_error := proc(expr, pre, $)
    local relprec, res, rnd;
    ASSERT(type(expr, 'numeric'));  # might actually work in some other cases
    # The rndz is there to handle cases like 99/100
    relprec := SFloatExponent(rndz(expr, 'prec'=1)) + pre + 1;
    if relprec <= 0 then
        res := 0.;
    else
        res := evalf[relprec](expr);
        ASSERT(SFloatExponent(res) = -pre or type(expr, 'integer')
                or type(expr, 'sfloat'));
    end if;
    res;
end proc:

# WARNING: this is rigorous when expr is an expression of the form a+I*b where a
# and b satisfy the requirements of evalf_abs_error.  Otherwise this may or
# may not work.
evalf_complex_abs_error := proc(expr, prec, $)
    # |(x+i·y) - (x~~ + i·y~)| <= |x-x~| + |y-y~|
    ASSERT(type(expr, 'complex(numeric)'));
    evalf_abs_error(Re(expr), prec+1)
        + I * evalf_abs_error(Im(expr), prec+1);
end proc:

# Compute an approximation 'approx' of 'expr' such that |approx-expr|<=eps,
# using complex interval arithmetic.
# WARNING: this can be very inefficient in some cases
iter_approx := proc(expr, eps, max_digits:=Settings:-max_digits_huge, $)
    local estimate, iniprec, iv, err;
    # Initial guess of required precision based on the order of magnitude of
    # expr and the observed cancellation
    Digits := 5;
    estimate := undefined;
    while type(estimate, 'undefined') or type(estimate, 'infinity') do
        fail_if_Digits_too_large("iter_approx, initial guess");
        estimate := evalf(expr);
        Digits := 2*Digits;
    end do;
    userinfo(5, 'gfun', "expr"=expr, "initial estimate"=estimate);
    # ilog10 works for complex input
    iniprec := max(1, -ilog10(eps) + ilog10(estimate));
    Digits := Digits + iniprec + 3*ilog10(iniprec);
    while Digits < max_digits do
        iv := myevalrC(expr);
        # this needs to be done at prec Digits too :-(
        err := complex_iv_diameter(iv);
        userinfo(5, 'gfun', "Digits"=Digits, "err"=evalf[2](err));
        if err < eps then
            return op(1,iv) + I*op(2, iv);
        end if;
        Digits := 2*Digits;
    end do:
    error "max_digits reached (increase NumGfun:-Settings:-max_digits_huge "
        "to proceed)";
end proc:

approx_abs_error := proc(expr, eps, $)
    if type(expr, 'complex(numeric)') then
        evalf_complex_abs_error(expr, -ilog10(eps)+1);
    else
        iter_approx(expr, eps);
    end if;
end proc:

# Compute an approximation of mat with *entrywise* absolute error <= 10^(-prec)
# as an ndmatrix. It is convenient to allow prec=-infinity for degenerate cases
# of the analytic continuation algorithm (trying to compute the identically zero
# solution of a differential equation).
ndmatrix_approximation := proc(mat, prec::{integer, neg_infinity}, $)
    local fmat;
    fmat := map(approx_abs_error, mat, 10^(-prec));
    numdenmatrix:-from_matrix_float(fmat);
end proc:

################################################################################
## Differential / recurrence operators
################################################################################

thetadeq := proc(Deq, yofz, {polynomialize:=false}, $)
    local y, z, deq, theta, j, r, poly, u;
    y, z := getname(yofz);
    if type(Deq, 'set') then
        deq := op(select(has, Deq, z))
    else
        deq := Deq
    end if;
    if type(deq, `=`) then deq := lhs(deq)-rhs(deq) end if;
    r := orddiffeq(deq, yofz);
    poly := expand(
        subs(
            {seq( diff(y(z),[z$j]) = z^(r-j)*mul(theta-u, u=0..j-1), j=0..r) },
            deq),
        theta);
    if polynomialize then poly := primpart(numer(normal(poly)), theta) end if;
    table([seq(j=coeff(poly, theta, j), j=0..r)]);
end proc;

# Given a differential equation satisfied by y(z), this procedure computes
# one for y'(z). It propagates initial values as needed and avoids adding new
# singular points.
#
# FIXME: this may not work in all cases yet
# FIXME: should we do linear combinations instead of differentiating in some
#        cases?

diffeq_for_derivative := proc(deq, yofz, $)
    local y, z, diffdeq, difforder, ini, newini, k, u, n;
    y, z := getname(yofz);
    diffdeq := op(select(has, deq, z));
    # differentiate deq until the term of order 0 disappears
    if not type(diffdeq, `+`) then error "not implemented" end if;
    for difforder from 0 while remove(has, diffdeq, diff) <> 0 do
        diffdeq := diff(diffdeq, z);
    end do;
    # get new initial values using the recurrence relation associated to the
    # original differential equation
    ini := rectoproc(diffeqtorec(deq, y(z), u(n)), u(n));
    newini := { seq(
        (D@@k)(y)(0) = (1+k)! * ini(1+k),
        k = 0 .. orddiffeq(deq,y(z))+difforder-2 ) };
    { subs(diff(y(z), z) = y(z), diffdeq) } union newini;
end proc:

# A variant of rectodiffeq that does not mess with initial values or contents.
purerectodiffeq := proc(Rec, uofn, yofz, $)
    local u, n, y, z, rec, rewrite_term, tmp, deq;
    u, n, y, z := getname(uofn), getname(yofz);
    rec := bare_rec(Rec, uofn);
    rewrite_term := proc(term)
        local d, theta;
        d := degree(term, n);
        theta := proc(f) z*diff(f, z) end proc;
        (theta@@d)(term/n^d * y(z));
    end proc:
    tmp := expand(eval(rec, u = proc(arg) z^(n-arg) end proc));
    if type(tmp, `+`) then tmp := [op(tmp)] else tmp := [tmp] end if;
    deq := `+`(op(map(rewrite_term, tmp)));
    deq := numer(expand(deq));
    deq := collect(deq, diff);
end proc:

# Same for diffeqtorec
purediffeqtorec := proc(deq, yofz, uofn, $)
    local u, n, z, rec, rewrite_term, coef, ordrec, i;
    u, n, z := getname(uofn), op(yofz);
    coef := thetadeq(deq, yofz, 'polynomialize');  # table(0..r)
    ordrec := 0;
    rewrite_term := proc(term, i)
        local j;
        j := degree(term, z);
        ordrec := max(j, ordrec);
        (n-j)^i * subs(z=1, term) * u(n-j)
    end proc:
    rec := add(
        maptype(`+`, rewrite_term, coef[i], i),
        i in indices(coef, 'nolist'));
    rec := subs(n=n+ordrec, rec);
    rec := collect(rec, u);
end proc:

diffeqtohomdiffeq_warn := proc(deq, yofz, $)
    local homdeq, y, z, k;
    y, z := getname(yofz);
    homdeq := diffeqtohomdiffeq(deq, yofz);
    if homdeq <> deq and (not type (homdeq, 'set') or
        # Better condition?
        indets(
            subs(remove(has, homdeq, z),
                {seq((D@@k)(y)(0), k=0..orddiffeq(homdeq, yofz)-1)}))
        <> {})
    then
        WARNING("replaced %1 by homogenized equation %2; the result is "
            "expressed with respect to the canonical basis of solutions of "
            "the homogenized equation (i.e., what NumGfun considers as initial "
            "values may not be what you expect)", deq, homdeq);
    end if;
    homdeq;
end proc:

################################################################################
## Parsing etc.
################################################################################

## Names

getname := proc(fofx, $)
    option inline;
    op(0, fofx), op(1, fofx)
end proc:

## Recs/diffeqs

ordfrec := proc(frec, $)::nonnegint;
    option inline;
    nops(frec) - 2
end proc:

orddiffeq := proc(deq, yofz, $)::nonnegint;
    option cache;
    nops(formatdiffeq([deq, yofz])) - 2;
end proc:

ordrec := proc(eq, fofx, $)::nonnegint;
    local shifted;
    max(seq(op(shifted)-op(fofx),
        shifted = indets(eq, 'specfunc'('linear',op(0,fofx)))));
end proc:

rec_has_constant_coefficients := proc(rec, u, n, $)
    local tmp, dummy;
    not member(n, indets(
        subs(
            {seq(
                tmp=subs(n=dummy,tmp),
                tmp in indets(rec, 'specfunc'('linear'(n),u)))},
            rec)));
end proc:

# NOTE: In the future, we should probably use records/``objects'' to represent
# diffeqs/recs in internal code, and this should be replaced by functions that
# translate from the representation by Maple expressions to the internal one.
# The only drawback I see is that it would make it a bit harder to call them
# from the shell when needed.

read_diffeq_or_rec := proc(format_function, eq, fofx, $)
    local coefs, r, f, x, inicond_eqns;
    coefs := format_function([eq, fofx], f, x, inicond_eqns);
    r := ordfrec(coefs);
    coefs, r, inicond_eqns
end proc:

read_diffeq := proc(deq, yofz, $)
    option cache;
    read_diffeq_or_rec(gfun:-formatdiffeq, args)
end proc:

read_rec := proc(rec, uofn, $)
    option cache;
    read_diffeq_or_rec(gfun:-formatrec, args)
end proc:

diffeq_lcoeff := proc(deq, yofz, $)
    read_diffeq(deq,yofz)[1][-1];
end proc;

diffeq_singularities := proc(deq, yofz, {exclude:=[]}, $)
    option cache;
    local z, pol, pt;
    z := op(yofz);
    pol := diffeq_lcoeff(deq, yofz);
    for pt in exclude do
        while divide(pol, z-pt, 'pol') do end do;
    end do;
    # Should I replace this by a rigourous symbolic version (e.g., using indexed
    # RootOf's to be evalrC'd by the caller)?
    [fsolve(pol, z, 'complex')];
end proc;

singularity_type := proc(deq, yofz, pt, $)
    local z, iv, coef, lc, ordeq, i;
    z := op(yofz);
    coef := read_diffeq(deq, yofz)[1];
    lc := coef[-1];
    iv := evalrCf(pt); # filter to answer fast when pt is large
    if not (iv_contains_zero(evalrC(subs(z=iv,lc))) and eval(lc,z=pt)=0) then
        "ordinary"
    else
        ordeq := nops(coef) - 2;
        # Fuchs criterion
        for i from 0 to ordeq-1 do
            if divide(denom(normal(coef[i+2]/coef[-1])), (z-pt)^(ordeq-i+1))
            then
                return "irregular singular"
            end if;
        end do;
        "regular singular";
    end if;
end proc:

# XXX: idem : regarder ce que fait dominant_root, si le mode numérique sert
# vraiment à quelque chose, comment le rendre plus rigoureux...
# XXX: option cache pose pb avec numeric_mode
diffeq_infsing := proc(deq, yofz, {numeric:=numeric_mode, nonzero:=false}, $)
    option cache;
    local sing, n, pol, z;
    if numeric then
        sing := diffeq_singularities(deq, yofz);
        if nonzero then
            sing := remove(verify, sing, Float(1, -Digits), 'less_than');
        end if;
        if sing = [] then
            infinity
        else
            sing[argmax(abs(sing[n]), n=1..nops(sing))]
        end if;
    else
        pol := diffeq_lcoeff(deq, yofz);
        if nonzero then
            z := op(yofz);
            pol := expand(pol);
            pol := pol/z^(ldegree(pol));
        end if;
        dominant_root(pol, op(yofz))[1];
    end if;
end proc;

bare_diffeq := proc(deq, z_or_yofz, $)
    option inline;
    `if`(type(deq, 'set'),
        op(select(has, deq, op(z_or_yofz))),
        deq);
end proc:

bare_rec := proc(rec, n_or_uofn, $)
    option inline;
    `if`(type(rec, 'set'),
        # rec needs not contain "u(n)", but op(n) = n
        op(select(has, rec, op(n_or_uofn))),
        rec);
end proc:

rec_to_recop := proc(rec, uofn, Shift, $)
    local u, n, barerec;
    u, n := getname(uofn);
    barerec := bare_rec(rec, uofn);
    if type(barerec, `=`) then
        barerec := lhs(barerec) - rhs(barerec);
    end if;
    eval(barerec, u = proc(x) Shift^eval(x,n=0) end proc);
end proc:

################################################################################
## RootOfs
################################################################################

# This could be improved. But a less naive version must exist somewhere...
simplify_RootOf := proc(pol::polynom(anything,_Z), approx::complex(float), $)
    local candidates;
    candidates := select(
        fac -> `infsolvepoly/isroot`(approx,fac[1], _Z),
        factors(pol)[2]);  # factors(pol, I) ???
    if nops(candidates) = 0 then 
        WARNING("ill-conditioned polynomial in abs(RootOf)");
        pol;
    else
        candidates[1][1];
    fi;
end proc:

`my_abs/RootOf` := proc(Poly)  # no '$'
    local z, poly, conjpoly, poly2, Y, approx, j;
    z := RootOf(args);
    if signum(z) = 1 then
        z
    elif type(Poly, 'polynom(complex(rational),_Z)') then
        approx := evalf(z); 
        poly := simplify_RootOf(Poly, approx);
        poly := expand(poly);
        conjpoly := add(conjugate(coeff(poly,_Z,j))*_Z^j, j=0..degree(poly));
        poly2 := resultant(
            subs(_Z=Y, poly),
            expand(Y^(degree(poly))*eval(conjpoly,_Z=_Z/Y)),
            Y);
        approx := abs(approx);
        poly2 := simplify_RootOf(subs(_Z=_Z^2, poly2), abs(approx));
        RootOf(poly2, approx);
    else
        'abs'(z)
    end if;
end proc:

abs_with_RootOf := proc(t, $)
    global `abs/RootOf`;
    local saved, res;
    forget(`simpl/abs`, t);
    forget(`simpl/simpl/abs`, t, _Envsignum0);
    saved := `abs/RootOf`;
    `abs/RootOf` := `my_abs/RootOf`;
    res := eval(subs(abs=procname, _passed));
    res := abs(res);
    `abs/RootOf` := saved;
    forget(`simpl/abs`, t);
    forget(`simpl/simpl/abs`, t, _Envsignum0);
    res;
end proc:

algebraic_roots := proc(pol, z)
    local i;
    map(
        fac -> seq([RootOf(fac[1], z, 'index'=i), fac[2]], i=1..degree(fac[1])),
        op(2, factors(pol)));
end proc:

is_simple_RootOf := proc(a, $)
    option inline;
    type(a, 'specified_rootof')
            and type(op(1,a), 'polynom(complex(rational),_Z)');
end proc:

# Convert specified RootOf's using approximations to indexed RootOf's.  This is
# useful because evalrC only understands the later.  I'm not sure how rigorous
# this is.  When possible, it is better to avoid relying on these functions and
# use indexed RootOf's directly.

approx_to_indexed_RootOf := proc(a, $)
    local pol, approx, candidates, i;
    if type(op(2,a), `=`) then return a end if;
    pol := op(1, a);
    ASSERT(type(pol, 'polynom(complex(rational),_Z)'));
    approx := evalf(a);
    pol := sqrfreepart(pol, _Z);
    candidates := [ seq(
        `if`(complex_iv_contains(approx, evalrC(RootOf(pol, 'index'=i))),
                                                                       i, NULL),
        i=1..degree(pol)) ];
    if nops(candidates) = 1 then
        RootOf(pol, 'index'=op(candidates))
    else
        fail_if_Digits_too_large(convert(procname, 'string'));
        Digits := 2*Digits;
        approx_to_indexed_RootOf(a);
    end if;
end proc:

make_RootOfs_indexed := proc(expr, $)
    subsindets(expr, 'specified_rootof', approx_to_indexed_RootOf);
end proc:

################################################################################
## Misc
################################################################################

## Set/reset (module-)global variable

set_mode := proc(var::uneval, val := true, $)
    local oldval;
    oldval := eval(var);
    var := val;
    return([ToInert(var), oldval]);
end proc:

reset_mode := proc(u, $)
    assign(FromInert(op(1,u)), op(2,u));
end proc;

##

# Smallest integer n between first and last for which f(n) is maximal
argmax := proc(f, r::name=integer..integer, $)::integer;
    local x, first, last, n, val, nmax, valmax;
    x, first, last := lhs(r), op(rhs(r));
    valmax := -infinity;
    for n from first to last do
        val := eval(f, x=n);
        if val > valmax then
            valmax := val;
            nmax := n;
        end if;
    end do;
    nmax;
end proc:

falling_factorial := proc(z, n, $)
    local k;
    mul(z-k, k=0..n-1)
end proc:

colinear := proc(u, v, support, $)::boolean;
    local i, uoverv, voveru;
    for i in support do
        if assigned(voveru) then
            if normal(v[i] - u[i] * voveru) <> 0 then return false end if;
        elif assigned(uoverv) then
            if normal(u[i] - v[i] * uoverv) <> 0 then return false end if;
        elif u[i] <> 0 then
            voveru := v[i]/u[i]
        elif v[i] <> 0 then
            uoverv := u[i]/v[i]
        end if;
    end do;
    true;
end proc:

##

sqrfreepart := proc(pol, x, $)
    local fac;
    mul(op(1, fac), fac in op(2, sqrfree(pol,x)));
end proc:

##

# for use in userinfo
sprint_small_approx := proc(x, $)
    if length(x) < 10 then
        sprintf("%a",x)
    else
        sprintf("~%a...(length=%a)", evalf[5](x), length(x))
    end if;
end proc:

mapReIm := proc(f, $)
    proc(z) f(Re(z)) + I*f(Im(z)) end proc;
end proc:

fail_if_Digits_too_large := proc(context, $)
    if Digits > Settings:-max_digits then
        error "%1: emergency stop -- Digits (=%2) too large (increase"
            "NumGfun:-Settings:-max_digits to proceed", context, Digits;
    end if;
end proc:

end module:

for tmp in [UTILITIES] do
    assign(tmp, utilities[tmp]);
end do;
