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

# Dominant roots of polynomials.
# Marc Mezzarobba, Algorithms project, INRIA. 2009.

# We call dominant roots of a polynomial those of maximal multiplicity among
# the nonzero roots of smallest modulus.

# What follows is basically a less clever and more specialized version of
# infsolvepoly (heavily inspired by it, though) that seems to work better for
# our purposes.

# TODO: option pour renvoyer directement les valeurs absolues ?

dominant_root := module()

description "compute a root of maximal multiplicity among those of minimal"
    "modulus of a polynomial in Q(i)[z]";
export longgcd, infroot_resultant, sqrabs, irreducible_solve, irreducible_check,
    inffactors, doit, ModuleApply, dominant_first, ratbelow_complex;


# gcd of more than two polynomials
longgcd := proc(polys, $)
    option inline;
    foldl(gcd, op(polys));
end proc;

infroot_resultant := proc(Poly, z, $)
    local poly, conjpoly, y, j;
    poly := expand(Poly);
    conjpoly := add(conjugate(coeff(poly, z, j))*z^j, j=0..degree(poly, z));
    resultant(
        subs(z=y, poly),
        numer(subs(z=z/y, conjpoly)),
        y);
end proc:

sqrabs := proc(z, $)
    option inline;
    z * conjugate(z);
end proc:

# Input:  [irreducible factor, multiplicity], variable
# Output: [factor, mult, numerical roots sorted by modulus]
irreducible_solve := proc(fac, z, $)
    local sol, sorted;
    sol := [fsolve(op(1,fac), z, 'complex')];
    sorted := sort( sol, (x, y) -> evalb(abs(x) < abs(y)) );
    # fac may have trailing garbage
    [ fac[1], fac[2], op(sorted) ]; # polynomial, multiplicity, numerical roots
end proc:

# Input:  [factor, mult, numerical roots sorted by modulus], variable
# Output: boolean, whether the numerical roots of modulus close to the minimum
#   really correspond to roots of the same modulus
irreducible_check := proc(facwithroots, z, $)
    local poly, sorted, nbmini, poly2, minsqr, testfun, candidates;
    poly, sorted := facwithroots[1], facwithroots[3..-1];
    for nbmini to nops(sorted)-1
        while sqrabs(sorted[1+nbmini])-sqrabs(sorted[1])<Float(1,2-Digits) do
    end do;
    userinfo(6, 'gfun', "nbmini" = nbmini);
    if nbmini = 1 then
        true
    else  # several roots of absolute value very close to the minimum
        # Compute a polynomial whose roots include the squared absolute values
        # of those of poly. (We could remember poly2 between successive
        # refinements, but those should be very rare.) The smallest positive
        # root of poly2 should be minsqr and have multiplicity nbmini. (Note to
        # self: recall that if u and v are roots of poly both close to 1, u², v²
        # and u·v will all be roots of poly2, all close to 1.)
        poly2 := infroot_resultant(poly, z); 
        minsqr := sqrabs(sorted[1]);
        # Here we do *not* want to discard sqfactors with several roots close to
        # minsqr (hence the use of eval instead of `infsolvepoly/isroot`).
        testfun := u -> sqrabs(eval(op(1,u), z=minsqr)) < Float(1,2-Digits);
        candidates := select(testfun, op(2,sqrfree(poly2,z)));
        (nops(candidates) = 1 and op(2,candidates[1]) = nbmini);
    end if;
end proc;

# whether x < y in the "dominance" order
dominant_first := proc(x,y)
    evalb(
            abs(op(3,x)) + Float(1, 2-Digits) < abs(op(3,y))
        or  abs(abs(op(3,x)) - abs(op(3,y))) < Float(1, 2-Digits)
                and op(2,x) > op(2,y));
end proc:

ratbelow_complex := proc(z)
    Digits := Digits_plus(-2);
    # rndz with additional_prec is almost what we need here; this tries to avoid
    # double rounding issues that may exist with it
    convert(
        NextAfter(Re(z), 0) + I * NextAfter(Im(z), 0),
        'rational', 'exact');
end proc:

# Input:  list of irreducible polynomials with multiplicities
# Output: a root of maximal multiplicity among those of minimal modulus,
#   its multiplicity, and the list of those of the input polynomials+mult that
#   have a root of minimal modulus
#   (in numeric mode, an approximation of such a root, with coefficients in
#   Q[i], and closer to zero than the exact value [provided that fsolve finds a
#   good approximation of the roots])
doit := proc(facpoly, z, {banzai := false}, $)
    local sorted, nbmini, chk, i, res;
    if Digits > 200 then error "emergency stop (Digits too large)" end if;
    sorted := sort(map(irreducible_solve, facpoly, z), dominant_first);
    # Count irreducible factors with roots of modulus close to the minimum
    for nbmini to nops(sorted)-1 while 
            sqrabs(sorted[1+nbmini][3])-sqrabs(sorted[1][3])
            < Float(1,2-Digits) do
    end do;
    userinfo(6, 'gfun', "nbmini" = nbmini);
    if numeric_mode or banzai then
        userinfo(6, 'gfun', "skipping checks");
    else
        # Check that none of these factors has several roots with different
        # absolute values all close to the minimum. Otherwise, refine. (We need
        # to refine all factors with roots close to the min to be able to sort
        # the roots afterwards.)
        for i to nbmini do
            if not irreducible_check(sorted[i], z) then
                userinfo(5, 'gfun', sorted[i], 
                    "factor root check failed, refining");
                Digits := 2*Digits;
                return procname(sorted[1..nbmini], z);
            end if;
        end do;
        if nbmini > 1 then
            # Check that the dominant roots of the remaining factors sorted[1],
            # ..., sorted[nbmini] all have exactly the same absolute value.
            chk := longgcd([seq(
                infroot_resultant(sorted[i][1], z),
                i=1..nbmini)]);
            if chk = 1 or not `infsolvepoly/isroot`(
                        sqrabs(sorted[1][3]), chk, z) then
                userinfo(5,'gfun',"cross-factor root check failed, refining");
                Digits := 2*Digits;
                return procname(sorted[1..nbmini], z);
            end if;
        end if;
    end if;
    if numeric_mode then
        res := ratbelow_complex(sorted[1][3]);
    else
        res := RootOf(sorted[1][1], z, sorted[1][3])
    end if;
    [ 
        res,                                # a dominant root
        sorted[1][2],                       # its multiplicity
        [ seq(sorted[i][1], i=1..nbmini) ]  # all candidate factors
    ];
end proc:

ModuleApply := proc(poly, z, { inffactors := false }, $)
    local res;
    if degree(collect(poly, z), z) = 0 then 
        res := [infinity, 1, poly];
    else
        # FIXME: 'factors' below may factor over Q or over Q(i) depending on the
        # coefficients of poly--is this really what we want? This is related to
        # what kinds of polynomials we allow in RootOf()s, and to how we compute
        # root multiplicities.
        res := doit(op(2, factors(poly)), z);
    end if;
    if not inffactors then res[1..2] else res end if;
end proc:

end module:
