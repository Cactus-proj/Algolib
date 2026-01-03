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

## nthterm.mpl: Recurrence unrolling by binary splitting.
## Marc Mezzarobba, Algorithms project, INRIA Rocquencourt

nthterm := module()

uses LinearAlgebra;

export frectopoly, recmatrix, recmatrix_rational, recmatrix_numdenmatrix,
    recmatrix_numdenseries, rec_inicond_vector, binsplit,
    `makeitfloat/mantissa`, makeitfloat, extract_row, extract_rownumdenmatrix,
    extract_row_numdenseries, extract_row_generic, binsplit_params,
    ratorfloat, nth_term_doit, choose_ring, fnth_term, nth_term_of_numdenseries,
    mydiv, dispatch;

################################################################################
## Utilities
################################################################################

# For compatibility with some old code.  Should eventually disappear, perhaps in
# favor of matrix objects if I really want to support abstract matrix types.
dispatch := proc(eqs, $)
    local dispatch_table;
    dispatch_table := table(eqs);
    proc(dispatch_name)
        dispatch_table[convert(dispatch_name, 'string')](_rest);
    end proc:
end proc:

## Recurrence to operator/matrix

frectopoly := proc(coef, Sn, $)
    local i;
    add( coef[i+2]*Sn^i, i=0..ordfrec(coef) )
end proc:

recmatrix_numdenmatrix := proc(L, Sn, $)
    local charpoly, den, mat;
    charpoly := evalc(normal(L / lcoeff(L, Sn)));
    den := denom(charpoly);
    mat := map(normal, den * Transpose([CompanionMatrix(charpoly, Sn)][1]));
    mat := map(convert, mat, 'horner');
    den := convert(den, 'horner');
    numdenmatrix:-make(mat, den)
end proc:

recmatrix_rational := proc(L, Sn, $)
    local den, mat;
    mat, den := op(recmatrix_numdenmatrix(args));
    map(normal, 1/den * mat, expanded)
end proc:

recmatrix_numdenseries := proc(L, Sn, $)::Matrix(polynom(complex(integer)));
    local den, coeff_mat, mat, r;
    coeff_mat, den := op(recmatrix_numdenmatrix(args));
    r := degree(L, Sn);
    mat := Matrix(r+1, r+1, coeff_mat);
    mat[-1,-1] := den;
    mat[-1, 1] := den;
    return(mat);
end proc:

recmatrix := dispatch([
    "numdenmatrix" = recmatrix_numdenmatrix,
    "numdenseries" = recmatrix_numdenseries,
    "generic"  = recmatrix_rational]);

## Initial values

rec_inicond_vector := proc(rec, uofn, $)::Vector[column];
    local u,n,i;
    u,n := getname(uofn);
    Vector(subs(select(type,rec,`=`),[seq(u(i),i=0..ordrec(rec,u(n))-1)]));
end proc:

################################################################################
## Binary splitting
################################################################################

# Given A(n) [usually a matrix], computes A(j-1)...A(i) by binary splitting.
binsplit := proc(A, n, i, j, grp, expected_size, $)
    local P, P1, P2, k, m, Ak;
    # The binary splitting algorithm brings nothing until the multiplication
    # becomes subquadratic (regardless of the number of matrices to multiply.)
    if j-i <= 3 or expected_size(i,j) <= Settings:-binary_splitting_threshold
    then
        userinfo(6, 'gfun', `iterative product, bounds =`, i, j);
        P := grp:-identity;
        for k from i to j-1 do
            # The degrees (in n) of the entries of A are typically low, but
            # their coefficients may be huge (e.g., in the "bit-burst"
            # algorithm). The index k may be in the billions but little more.
            Ak := subs(n=k, A);
            # If A does not depend on n, Maple doesn't copy it when eval() is
            # called, so we need to do it by hand (since the matrix product may
            # be destructive -- well, not anymore)
            #if Ak=A then Ak := map(copy,A) end if;
            P := grp:-multiply(Ak, P);
        end do;
    else
        userinfo(6, 'gfun', `split, bounds =`, i, j);
        m := iquo(i+j, 2); # gmp-chudnovsky does slightly unbalanced rec calls
        P1 := binsplit(A, n, m, j, grp, expected_size);
        P2 := binsplit(A, n, i, m, grp, expected_size);
        P := grp:-multiply(P1, P2);
    end if;
    P;
end proc:

# Note: Some older versions of NumGfun had specialized binary splitting routines
# that reused rtable structures to save a bit of memory management. See git
# history if needed.

################################################################################
## Rational to Float conversion without gcd
################################################################################

# Compute (without gcd) an integer m such that fp = m·10^(-prec) satisfies
# abs(fp-p/q) <= 6/10 · 10^(-prec) (however fp may not be the best approximation
# of p/q by an integer multiple of 10^(-prec)).  Originally based on code by
# Bruno Salvy.
`makeitfloat/mantissa` := proc(num, den, prec, $)
    local m, r, sgn;
    ASSERT(den > 0);
    sgn := sign(num);
    m := iquo(sgn*num*10^(prec+1), den);
    m := iquo(m, 10, 'r');
    if r < 5 then sgn*m else sgn*(m + 1) end if;
end proc:

# Compute a complex(integer)-multiple fp of 10^(-prec) (as a complex(float))
# such that abs(fp-x) < 6/10·sqrt(2)·10^(-prec) < .85·10^(-prec). In particular,
# if x is itself an approximation of y with abs(x-y) < 10^(-prec-1), then
# abs(fp-y) < 10^(-prec). (For matrices, this holds *entrywise*, as it is
# obviously not possible in general in Frobenius norm.)
#
# Perhaps some of the code should move to ratorfloat.
makeitfloat := proc(x, prec, $)
    local p, q, val;
    if type(x, 'complex'('rational')) then
        procname([numer(x),denom(x)], prec)
    elif type(x, 'numdenmatrix') then  # FIXME: this does not really belong here
        map(p -> makeitfloat([p,op(2,x)], prec), op(1, x));
    elif type(x, ['complex'('integer'),'integer']) then
        p, q := op(x);
        # Since the error made by `makeitfloat/mantissa` is < 6/10 10^(-prec),
        # here abs(result-exact value) < 6/10·sqrt(2)·10^(-prec) < 10^(-prec)
        Float(
            `makeitfloat/mantissa`(Re(p),q, prec)
                + I * `makeitfloat/mantissa`(Im(p),q, prec),
            -prec);
    else
        # For linear combinations of initial values as used by diffeqtoproc. May
        # not be too robust.
        val := frontend(subsindets[flat],
            [x],
            [{`+`,`*`}, {}],
            {'complex'('rational')}, procname, prec);
        # To return, e.g., 1.00...00*Pi rather than Pi. This should be
        # improved--but how?
        if type(val, `+`) then val := [op(val)] else val := [val] end if;
        val := map(
            u -> `if`(hastype(u,float), u,  Float(10^prec, -prec)*u),
            val);
        `+`(op(val));
    end if
end proc:


################################################################################
## Nth term
################################################################################

extract_rownumdenmatrix := proc(mat, $)
    numdenmatrix:-make(Row(op(1,mat),1), op(2,mat));
end proc:

extract_row_numdenseries := proc(mat, $)
    numdenmatrix:-make(mat[-1,1..-2], mat[-1,-1]);
end proc:

extract_row_generic := proc(mat, $)
    Row(mat, 1)
end proc:

extract_row := dispatch([
    "numdenmatrix" = extract_rownumdenmatrix,
    "numdenseries" = extract_row_numdenseries,
    "generic"  = extract_row_generic]):

# Warning: initial values given in rec are ignored.
binsplit_params := proc(rec, uofn, startidx, stopidx, ringname, $)
    local r, grp, A, d, h, expected_size, i, k, frec, ordrec, ini, u, n, Sn;
    ASSERT(type(stopidx, 'Or'('nonnegint', 'name')));
    u, n := getname(uofn);
    frec, r, ini := read_rec(rec, uofn);
    grp := matrices:-matrix_ops[ringname](r);
    A := recmatrix(ringname, frectopoly(frec, Sn), Sn);
    userinfo(6,'gfun', printf("recurrence matrix = %a\n", A));
    d := max(degree(frec[i],n)$i=1..r);
    h := add(add(length(k), k=coeffs(expand(frec[i]),n)), i=2..r+1);
    expected_size := eval((i,j) -> (j-i)*(d*ilog2(j)+h));
    [A, n, startidx, stopidx, grp, expected_size];
end proc:

mydiv := proc(num, den)
    local q;
    if irem(num, den, 'q') = 0 then q
    else num/den
    end if;
end proc:

ratorfloat := proc(ndm, prec, $)
    if prec = infinity then
        map(mydiv, op(ndm))
    else
        makeitfloat(ndm, prec)
    end if;
end proc:

nth_term_doit := proc(binsplitparams, Ini, prec, ringname, $)
    local ndrow, row, matrix_1toN, ini, res;
    # Actually [u(k),...,u(N)] = A(k-1)···A(0)·[u(0),...,u(r-1)] where k=N-r+1,
    # but to use that we would have to treat the case N<r separately.
    matrix_1toN := binsplit(op(binsplitparams));
    ndrow := extract_row(ringname, matrix_1toN);
    ini := Ini;
    if type(ini, 'Vector'('complex'('numeric'))) then
        if type(ini, 'Vector'('complex'('float'))) then
            ini := convert(ini, 'rational', 'exact');
        end if;
        res := numdenmatrix:-multiply( ndrow, numdenmatrix:-from_matrix(ini));
        ratorfloat(res, prec)[1];
    else
        row := ratorfloat(ndrow, prec);
        DotProduct(row, ini, 'conjugate'=false);
    end if;
end proc:

choose_ring := proc(series, rational, $)
    if rational then    error "not implemented yet"
    elif series then    "numdenseries"
    else                "numdenmatrix"
    end if;
end proc:

# gfun:-nth_term
nth_term := proc(rec, uofn::function(name), N::nonnegative, 
    { series::boolean := false, [rational,gcd]::boolean:= false }, $ )
    description "Computes u(N) \in Q[i] where u satisfies rec";
    local ringname;
    ringname := choose_ring(series, rational);
    nth_term_doit(
        binsplit_params(rec, uofn, 0, N, ringname),
        rec_inicond_vector(rec, uofn),
        infinity,
        ringname);
end proc:

fnth_term := proc(rec::hrrec, uofn::function(name), N::nonnegint,
    prec::posint:=Settings:-default_eval_precision,
    { series::boolean := false, [rational,gcd]::boolean:= false }, $ )
    description "Computes a floating-point approximation of u(N) where u satisfies rec";
    local ringname;
    ringname := choose_ring(series, rational);
    nth_term_doit(
        binsplit_params(rec, uofn, 0, N, ringname),
        rec_inicond_vector(rec, uofn),
        prec,
        ringname);
end proc:

# Computes Sum(u(n)*dz^n,n=0..N-1) where u(n) satisfies rec. (To compute
# Sum(u(n),n=0..N-1), call nth_term(series) or binsplit(ndseries) directly.)
#
# Called by step_transition_matrix.
nth_term_of_numdenseries := proc(rec, uofn, dz::complex(rational), N, $)
        ::numdenmatrix;
    local u, n, s, k, termrec, P;
    u, n := getname(uofn);
    s := ordrec(rec, uofn);
    termrec := subs(  # the summation is done by binsplit itself (numdenseries)
        { seq( u(n+k) = dz^(s-k)*u(n+k), k=0..s ) },
        rec );
    P := binsplit(op(binsplit_params(termrec, uofn, 0, N, "numdenseries")));
    extract_row_numdenseries(P);
end proc:

################################################################################
## gfun:-rectoproc helpers
################################################################################

# These procedures are local to *gfun* (not NumGfun), but part of NumGfun as
# they need to access internal functions.

`rectoproc/binsplitparameters` := proc(rec, uofn, startidx, stopidx, ini, $)
    local inline_ndmatrix, bsp;
    # FromInert does not seem to support rtables containing local names
    inline_ndmatrix := proc(m)
        numdenmatrix:-make(
            'Matrix'(convert(op(1,m), 'listlist')),
            op(2,m));
    end proc;
    bsp := binsplit_params(_params[1..4], "numdenmatrix");
    bsp := subsop( 1 = inline_ndmatrix(op(1,bsp)), bsp );
    map(ToInert, _Inert_EXPSEQ(bsp, ini));
end proc:

# This allows procedures generated by rectoproc to call internal NumGfun
# functions without having these functions escape to gfun.
`gfun/rectoproc/binsplit` := proc(binsplitparams, ini, $)
    description "NumGfun helper used by some procedures generated by rectoproc";
    nth_term_doit(binsplitparams, ini, infinity, "numdenmatrix");
end proc:

end module:

