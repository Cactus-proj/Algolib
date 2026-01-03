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

## nthterm.mpl: Recurrence unrolling by binary splitting.
## Marc Mezzarobba, Algorithms project, INRIA Rocquencourt

nthterm := module()

uses LinearAlgebra;

export frectopoly, recmatrix, frectomatrix, rec_inicond_vector, binsplit,
    `makeitfloat/mantissa`, makeitfloat, extractline, binsplit_params,
    ratorfloat, nth_term_doit, choose_ring, fnth_term, nth_term_of_ndseries,
    mydiv;

################################################################################
## Utilities
################################################################################

## Recurrence to operator/matrix

frectopoly := proc(coef, Sn, $)
    local i;
    add( coef[i+2]*Sn^i, i=0..ordfrec(coef) )
end proc:

recmatrix['generic'] := proc(L, Sn, $) 
    local den, M;
    M, den := op(recmatrix[ndmatrix](args));
    map(normal, 1/den * M, expanded)
end proc:

# FIXME: Horner?
recmatrix['ndmatrix'] := proc(L, Sn, $)
    local charpoly, den, M;
    charpoly := evalc(normal(L / lcoeff(L, Sn)));
    den := denom(charpoly);
    M := map(normal,den * Transpose([CompanionMatrix(charpoly, Sn)][1]));
    M := map(convert, M, 'horner');
    den := convert(den, 'horner');
    ndmatrix(M,den)
end proc:

recmatrix['ndseries'] := proc(L, Sn, $)::Matrix(polynom(complex(integer)));
    local den, Mcoef, M, r;
    Mcoef, den := op(recmatrix[ndmatrix](args));
    r := degree(L, Sn);
    M := Matrix(r+1,r+1,Mcoef);
    M[-1,-1] := den;
    M[-1, 1] := den; #???
    return(M);
end proc:

#`recmatrix/imatrix` := proc(L, Sn, $)
#    local den, M, M1, X, Y;
#    M, den := `recmatrix/numden`(args);
#    X := map(evalc@Re, M);
#    Y := map(evalc@Im, M);
#    imatrix(map(convert,X,`horner`), map(convert,Y,`horner`), convert(den,`horner`));
#end proc:

# Voir si je veux mettre des rec partout... (Cf. surtout Nth_term vs /common, et gestion CI.)
frectomatrix := proc(frec, convname, $)
    description "Recurrence (formatrec style) --> matrix";
    local Sn, M;
    M := call(recmatrix, convname, frectopoly(frec, Sn), Sn);
    userinfo(6,'gfun', printf("recurrence matrix = %a\n", M));
    M;
end proc:

## Initial values

rec_inicond_vector := proc(rec, uofn, $)::Vector[column];
    local u,n,i;
    u,n := getname(uofn);
    Vector(subs(select(type,rec,`=`),[seq(u(i),i=0..ordrec(rec,u(n))-1)]));
end proc:

################################################################################
## Binary splitting
################################################################################

# Prend une matrice dépendant d'un paramètre et deux indices. Calcule le produit matriciel, soit par
# multiplication directe soit par scindage binaire (en se rappelant récursivement), suivant le
# nombre de matrices à multiplier et la taille des coefficients. 
# - Le scindage binaire présente un certain overhead, et *n'apporte rien* tant que la multiplication
# est en régime quadratique. On s'attend donc à ce qu'il devienne intéressant quand le produit des
# coefficients passe à Karatsuba. Quel que soit le nombre de matrices à multiplier, tant que la
# taille des coefs du résultat n'atteint pas ce seuil, on a intérêt à les multiplier naïvement.
# - Les produits à la Waksman, eux, sont intéressants même (voire surtout) pour une multiplication
# d'entiers quadratique. En revanche, ils perdent du temps sur les matrices peu denses.
binsplit := proc(A, n, i, j, ring, expected_entry_size, $)
    description "Given A(n) [usually a matrix], computes A(j-1)...A(i) by binary splitting.";
    local P, P1, P2, k, m, matmult, id, Ak;
    matmult, id := op(2,ring), op(4, ring); ## test
    if j-i <= 3 or expected_entry_size(i,j) <= Settings:-binary_splitting_threshold then
        userinfo(6, 'gfun', `iterative product, bounds =`, i, j);
        P := id;
        for k from i to j-1 do
            # The degrees (in n) of the entries of A are typically low, but their coefficients may
            # be huge (e.g., in the "bit-burst" algorithm). The index k may be in the billions but
            # little more.
            Ak := subs(n=k, A);
            # if A does not depend on n, Maple doesn't copy it when eval() is called, so we need to
            # do it by hand (since the matrix product may be destructive -- well, not anymore)
            #if Ak=A then Ak := map(copy,A) end if;
            P := matmult(Ak, P);
        end do;
    else
        userinfo(6, 'gfun', `split, bounds =`, i, j);
        m := iquo(i+j, 2);  #  gmp-chudnovsky.c déséquilibre très légèrement (a+(b-a)*0.5224)
        P1 := binsplit(A,n,m,j,ring,expected_entry_size);
        P2 := binsplit(A,n,i,m,ring,expected_entry_size);
        P := matmult(P1,P2); # try using waksman here?
    end if;
    P;
end proc:

# Reuse matrices to save on memory management. Maybe a few % faster than /generic.
#`binsplit/inlined` := proc(A, n, i, j, ring::matrix_ring,
#                        expected_entry_size, $)
#    description "Given A(n) [usually a matrix], computes A(j-1)...A(i) by binary splitting.";
#    local P, P1, P2, k, m, numAk, numA, denA, numP, r, tmp;
#    if j-i <= 3 or expected_entry_size(i,j) <= binary_splitting_threshold then
#        numA, denA := op(A);
#        r := op(1,numA); # size
#        numP := LA_Main:-IdentityMatrix(r,'compact'=false,'outputoptions'=[]);
#        tmp := Matrix(r);
#        for k from i to j-1 do
#            numAk := eval(numA, n=k);
#            if numAk=A then numAk := copy(numA) end if;
#            mvMultiply(numAk,numP, tmp);
#            numP := tmp;
#            tmp := numAk;
#        end do;
#        P := ndmatrix(
#            numP,
#            mul( eval(denA, n=k), k=i..j-1 ));
#    else
#        m := iquo(i+j, 2);  #  gmp-chudnovsky.c déséquilibre très légèrement (a+(b-a)*0.5224)
#        P1 := binsplit(A,n,m,j,ring,expected_entry_size);
#        P2 := binsplit(A,n,i,m,ring,expected_entry_size);
#        P := ndmatrix( ## test
#            mvMultiply(op(1,P1),op(1,P2)),
#            op(2,P1) * op(2,P2));
#    end if;
#    P;
#end proc:


################################################################################
## Rational to Float conversion without gcd
################################################################################

# originally based on code by Bruno Salvy

# Compute (without gcd) an integer m such that fp = m·10^(-prec) satisfies
# abs(fp-p/q) <= 6/10 · 10^(-prec) (however fp may not be the best approximation
# of p/q by an integer multiple of 10^(-prec)).
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
# abs(fp-y) < 10^(-prec). (For matrices, this holds *entrywise*.)
#
# Maybe some of this should move to ratorfloat.
makeitfloat := proc(x, prec, $)
    local p, q, val;
    if type(x, 'complex'('rational')) then
        procname([numer(x),denom(x)], prec)
    elif type(x, 'ndmatrix') then  # this does not really belong here
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

extractline[generic] := proc(M, $)
    Row(M, 1)
end proc:

extractline[ndmatrix] := proc(M, $)
    ndmatrix(Row(op(1,M),1),op(2,M));
end proc:

extractline[ndseries] := proc(M, $)
    ndmatrix(M[-1,1..-2],M[-1,-1]);
end proc:

# Warning: initial values given in rec are ignored.
binsplit_params := proc(rec, uofn, startidx, stopidx, ringname, $)
    local r, ring, A, d, h, expected_entry_size, i, k, frec, ordrec, ini, u, n;
    ASSERT(type(stopidx, 'Or'('nonnegint', 'name')));
    u, n := getname(uofn);  # encore une bonne raison de se débarrasser de frec
    frec, r, ini := read_rec(rec, uofn);
    ring := call(matrices:-genmatring, ringname, r);
    A := frectomatrix(frec, ringname);
    d := max(degree(frec[i],n)$i=1..r);
    h := add(add(length(k), k=coeffs(expand(frec[i]),n)), i=2..r+1);
    expected_entry_size := eval((i,j) -> (j-i)*(d*ilog2(j)+h));
    [A, n, startidx, stopidx, ring, expected_entry_size];
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
    local ndlin, lin, matrix_1toN, ini, res;
    # Actually [u(k),...,u(N)] = A(k-1)···A(0)·[u(0),...,u(r-1)] where k=N-r+1,
    # but to use that we would have to treat the case N<r separately.
    matrix_1toN := binsplit(op(binsplitparams));
    ndlin := call(extractline, ringname, matrix_1toN); # to be improved
    ini := Ini;
    if type(ini, 'Vector'('complex'('numeric'))) then
        if type(ini, 'Vector'('complex'('float'))) then
            ini := convert(ini, 'rational', 'exact');
        end if;
        res := matrices:-ndmatrix_multiply(ndlin, convert(ini,ndmatrix));
        ratorfloat(res, prec)[1];
    else
        lin := ratorfloat(ndlin, prec);
        DotProduct(lin, ini, 'conjugate'=false);
    end if;
end proc:

choose_ring := proc(series, rational, $)
    if rational then    error "not implemented yet"
    elif series then    'ndseries'
    else                'ndmatrix'
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

fnth_term := proc(rec, uofn::function(name), N::nonnegative, prec::posint:=Digits,
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
nth_term_of_ndseries := proc(rec, uofn, dz::complex(rational), N, $)::ndmatrix;
    local u, n, s, k, termrec, P;
    u, n := getname(uofn);
    s := ordrec(rec, uofn);
    termrec := subs(  # the summation is done by binsplit itself (ndseries)
        { seq( u(n+k) = dz^(s-k)*u(n+k), k=0..s ) },
        rec );
    P := binsplit(op(binsplit_params(termrec, uofn, 0, N, 'ndseries')));
    call(extractline,'ndseries',P);
end proc:

################################################################################
## gfun:-rectoproc helpers
################################################################################

# These procedures are local to *gfun* (not NumGfun), but part of NumGfun as they need to access
# internal functions.

`rectoproc/binsplitparameters` := proc(rec, uofn, startidx, stopidx, ini, $)
    local inline_ndmatrix, bsp;
    # FromInert seems not to support rtables containing local names
    inline_ndmatrix := proc(m)
        ''ndmatrix''(
            'Matrix'(convert(op(1,m),'listlist')),
            op(2,m));
    end proc;
    bsp := binsplit_params(_params[1..4], 'ndmatrix');
    bsp := subsop( 1 = inline_ndmatrix(op(1,bsp)), bsp );
    map(ToInert, _Inert_EXPSEQ(bsp, ini));
end proc:

# This allows procedures generated by rectoproc to call internal NumGfun functions without having
# these functions escape to gfun.
`gfun/rectoproc/binsplit` := proc(binsplitparams, ini, $)
    description "NumGfun helper used by some procedures generated by rectoproc";
    nth_term_doit(binsplitparams, ini, infinity, 'ndmatrix');
end proc:

end module:

