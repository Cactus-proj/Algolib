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
local evalf_rnd, rndu, rndz, rndd, rndn, rndi, upper, lower, below, above,
    ratabove, evalf_absolute_error, evalf_complex_absolute_error,
    ndmatrix_approximation, thetadeq, diffeq_for_derivative, getname,
    make_fresh_name_generator, ordfrec, orddiffeq, ordrec,
    rec_has_constant_coefficients, read_diffeq, read_rec, diffeq_lcoeff,
    diffeq_singularities, diffeq_infsing, bare_diffeq, bare_rec,
    simplify_RootOf, set_mode, reset_mode, call, argmax, falling_factorial,
    colinear, ratbelow, Digits_plus, bound_abs_interval, sprint_small_approx;

utilities := module()

option package;

# these would be exported if utilities was a submodule
export evalf_rnd, rndu, rndz, rndd, rndn, rndi, upper, lower, below, above,
    ratabove, evalf_absolute_error, evalf_complex_absolute_error,
    ndmatrix_approximation, thetadeq, diffeq_for_derivative, getname,
    make_fresh_name_generator, ordfrec, orddiffeq, ordrec,
    rec_has_constant_coefficients, read_diffeq, read_rec, diffeq_lcoeff,
    diffeq_singularities, diffeq_infsing, bare_diffeq, bare_rec,
    simplify_RootOf, set_mode, reset_mode, call, argmax, falling_factorial,
    colinear, ratbelow, Digits_plus, bound_abs_interval, sprint_small_approx;

# ...while these should remain local (or even become local to new, smaller
# modules)
local rnd_mode_aliases, above_below, read_diffeq_or_rec, `my_abs/RootOf`;

################################################################################
## Directed rounding, interval arithmetic, etc.
################################################################################

Digits_plus := proc(n)
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
        {additional_prec := 0, prec := Digits_plus(additional_prec)})
    Rounding := rnd_mode_aliases[rnd_mode];     # environment variables
    Digits := prec;                             #
    if _nrest = 0 then
        evalf(a)
    else
        evalf(a(_rest))
    end if;
end proc:

rndu := proc()
    evalf_rnd("rndu", _passed)
end proc:

rndz := proc()
    evalf_rnd("rndz", _passed)
end proc:

rndd := proc()
    evalf_rnd("rndd", _passed)
end proc:

rndn := proc()
    evalf_rnd("rndn", _passed)
end proc:

# rounding to ±inifinity is not nativevly supported
rndi := proc()
    # TODO
end proc:

## Real intervals

upper := proc(i)
    option inline;
    op([1,1], i);
end proc;

lower := proc(i)
    option inline;
    op([2,1], i);
end proc;

## Complex intervals (for use with evalrC)

bound_abs_interval := proc(iv)
    local a, b, c, d;
    a, b, c, d := op(iv);
    Rounding := infinity;
    sqrt(max(a^2+b^2, c^2+d^2, a^2+d^2, c^2+b^2));
end proc:


## Poor man's directed rounding

above_below := proc(x,i)::realcons;
    local y, ampl;
    if type(x,rational) and length(x) < 3*Digits then
        x;
    else 
        try
            op(i,op(shake(x)))
        catch: 
            WARNING("unable to compute an enclosure for %1 "
                "-- using evalf() instead (may be unsafe)", x);
            op(i,op(shake(evalf(x))))
        end try
    end if;
end proc:

below := proc() above_below(args,1) end proc:
above := proc() above_below(args,2) end proc:

ratabove := proc(x)
    convert(above(x), 'rational', 'exact')
end proc:

ratbelow := proc(x)
    convert(below(x), 'rational', 'exact')
end proc:

## Approximation with absolute error

evalf_absolute_error := proc(expr, prec)
    local relprec;
    relprec := SFloatExponent(evalf[1](expr)) + prec + 1;
    if relprec <= 0 then 0. else evalf[relprec](expr) end if;
end proc:

evalf_complex_absolute_error := proc(expr, prec)
    # |(x+i·y) - (x~ + i·y~)| <= |x-x~| + |y-y~|
    evalf_absolute_error(Re(expr), prec+1)
        + I * evalf_absolute_error(Im(expr), prec+1);
end proc:

# Compute an approximation of mat with *entrywise* absolute error <= 10^(-prec)
# as an ndmatrix. It is convenient to allow prec=-infinity for degenerate cases
# of the analytic continuation algorithm (trying to compute the identically zero
# solution of a differential equation).
ndmatrix_approximation := proc(mat, prec::{integer, neg_infinity}, $)
    local fmat, rmat;
    fmat := map(evalf_complex_absolute_error, mat, prec);
    rmat := convert(fmat, 'rational', 'exact');
    convert(rmat, 'ndmatrix');
end proc:

################################################################################
## Differential / recurrence operators
################################################################################

thetadeq := proc(Deq, yofz, $)
    local y, z, deq, theta, j, r, poly, u;
    y, z := getname(yofz);
    if type(Deq, 'set') then deq := op(select(has, Deq, z)) else deq := Deq end if;
    if type(deq, `=`) then deq := lhs(deq) -rhs(deq) end if;
    r := orddiffeq(deq, yofz);
    poly := expand(
        subs(
            {seq( diff(y(z),[z$j]) = z^(r-j)*mul(theta-u, u=0..j-1), j=0..r) },
            deq),
        theta);
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
    local y, z, diffdeq, difforder, ini, newini, k;
    y, z := getname(yofz);
    diffdeq := op(select(has, deq, z));
    # differentiate deq until the term of order 0 disappears
    if not type(diffdeq, `+`) then error "ploum" end if;
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

################################################################################
## Parsing etc.
################################################################################

## Names

getname := proc(fofx)
    option inline;
    op(0, fofx), op(1, fofx)
end proc:

make_fresh_name_generator := proc(base)::procedure;
    local curname;
    curname := 0;
    proc()::name;
        curname := curname + 1;
        return(cat(base,curname))
    end proc:
end proc:

## Recs/diffeqs

ordfrec := proc(frec)::nonnegint;
    option inline;
    nops(frec) - 2
end proc:

orddiffeq := proc(deq, yofz)::nonnegint;
    option cache;
    nops(formatdiffeq([deq, yofz])) - 2;
end proc:

ordrec := proc(eq, fofx)::nonnegint;
    local shifted;
    max(seq(op(shifted)-op(fofx),
        shifted = indets(eq, 'specfunc'('linear',op(0,fofx)))));
end proc:

rec_has_constant_coefficients := proc(rec, u, n)
    local tmp, dummy;
    not member(n, indets(
        subs(
            {seq(
                tmp=subs(n=dummy,tmp),
                tmp in indets(rec, 'specfunc'('linear'(n),u)))},
            rec)));
end proc:

# renvoyer une table pour avoir les indices corrects ?
read_diffeq_or_rec := proc(format_function, eq, fofx)
    local coefs, r, f, x, inicond_eqns;
    f,x := getname(fofx);
    coefs := format_function([eq, fofx], f, x, inicond_eqns);
    r := ordfrec(coefs);
    coefs, r, inicond_eqns
end proc:

read_diffeq := proc(deq, yofz)
    option cache;
    read_diffeq_or_rec(gfun:-formatdiffeq, args)
end proc:

read_rec := proc(rec, uofn)
    option cache;
    read_diffeq_or_rec(gfun:-formatrec, args)
end proc:

diffeq_lcoeff := proc(deq, yofz)
    read_diffeq(deq,yofz)[1][-1];
end proc;

diffeq_singularities := proc(deq, yofz)
    option cache;
    # Ignore the singularity at the origin to allow evaluating Si and Shi.
    remove(x -> abs(x) < Float(1, 2-Digits),
        [fsolve(diffeq_lcoeff(deq,yofz), op(yofz), 'complex')]);
end proc;

diffeq_infsing := proc(deq, yofz, {numeric := numeric_mode})
    option cache;
    local sing, n;
    if numeric then
        sing := diffeq_singularities(deq, yofz);
        if sing = [] then infinity else sing[argmax(abs(sing[n]), n=1..nops(sing))] end if;
    else
        dominant_root(diffeq_lcoeff(deq, yofz), op(yofz))[1];
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

################################################################################
## RootOfs
################################################################################

# This could be improved. But a less naive version must exist somewhere...
simplify_RootOf := proc(pol::polynom(anything,_Z), approx::complex(float))
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

`my_abs/RootOf` := proc(Poly)
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

abs_with_RootOf := proc(t) # exported
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

################################################################################
## Misc
################################################################################

## Set/reset (module-)global variable

set_mode := proc(var::uneval, val := true)
    local oldval;
    oldval := eval(var);
    var := val;
    return([ToInert(var), oldval]);
end proc:

reset_mode := proc(u)
    assign(FromInert(op(1,u)), op(2,u));
end proc;

##

# FIXME: this should disappear (use overload instead?)
call := proc(base,case)
    local casename,fun;
    for casename in [case,`generic`] do
        fun := base[casename];
        if type(fun,procedure) then
            return(fun(args[3..-1]));
        end if
    end do;
    error("Case %1 not implemented for %2",case,base)
end proc:

##

argmax := proc(f, r::name=integer..integer)::integer;
    description "Returns the smallest integer n between first and last for which f(n) is maximal";
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

falling_factorial := proc(z, n)
    local k;
    mul(z-k, k=0..n-1)
end proc:

colinear := proc(u, v, support)::boolean;
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

# for use in userinfo
sprint_small_approx := proc(x)
    if length(x) < 30 then
        sprintf("%a",x)
    else
        sprintf("~%a", evalf[10](x))
    end if;
end proc:

end module:


evalf_rnd                     := utilities:-evalf_rnd                    ;
rndu                          := utilities:-rndu                         ;
rndz                          := utilities:-rndz                         ;
rndd                          := utilities:-rndd                         ;
rndn                          := utilities:-rndn                         ;
rndi                          := utilities:-rndi                         ;
upper                         := utilities:-upper                        ;
lower                         := utilities:-lower                        ;
below                         := utilities:-below                        ;
above                         := utilities:-above                        ;
ratabove                      := utilities:-ratabove                     ;
evalf_absolute_error          := utilities:-evalf_absolute_error         ;
evalf_complex_absolute_error  := utilities:-evalf_complex_absolute_error ;
ndmatrix_approximation        := utilities:-ndmatrix_approximation       ;
thetadeq                      := utilities:-thetadeq                     ;
diffeq_for_derivative         := utilities:-diffeq_for_derivative        ;
getname                       := utilities:-getname                      ;
make_fresh_name_generator     := utilities:-make_fresh_name_generator    ;
ordfrec                       := utilities:-ordfrec                      ;
orddiffeq                     := utilities:-orddiffeq                    ;
ordrec                        := utilities:-ordrec                       ;
rec_has_constant_coefficients := utilities:-rec_has_constant_coefficients;
read_diffeq                   := utilities:-read_diffeq                  ;
read_rec                      := utilities:-read_rec                     ;
diffeq_lcoeff                 := utilities:-diffeq_lcoeff                ;
diffeq_singularities          := utilities:-diffeq_singularities         ;
diffeq_infsing                := utilities:-diffeq_infsing               ;
bare_diffeq                   := utilities:-bare_diffeq                  ;
bare_rec                      := utilities:-bare_rec                     ;
simplify_RootOf               := utilities:-simplify_RootOf              ;
set_mode                      := utilities:-set_mode                     ;
reset_mode                    := utilities:-reset_mode                   ;
call                          := utilities:-call                         ;
argmax                        := utilities:-argmax                       ;
falling_factorial             := utilities:-falling_factorial            ;
colinear                      := utilities:-colinear                     ;
ratbelow                      := utilities:-ratbelow                     ;
Digits_plus                   := utilities:-Digits_plus                  ;
bound_abs_interval            := utilities:-bound_abs_interval           ;
sprint_small_approx           := utilities:-sprint_small_approx          ;
