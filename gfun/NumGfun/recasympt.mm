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

# Asymptotics of P-recursive sequences based on singularity analysis and
# numerical analytic continuation.

# WORK IN PROGRESS, not yet usable.

# The basic idea behind this algorithm appears in: Philippe Flajolet & Claude
# Puech, Partial match retrieval of multidimensional data, Journal of the ACM,
# 33, 371-407, 1986.

recasympt := module()

export cmp_algeb_abs;

local
    absroot_resultant, iv_intersect, remaining_roots_dist;

################################################################################
# Comparison of algebraic numbers
################################################################################

# Compute a polynomial whose roots include abs(a)^2.  See also abs_with_RootOf
# (and dominant_root as well) and try to factor some code between all these
# functions.
absroot_resultant := proc(a, $)
    local poly, conjpoly, Y, j;
    # RootOf's of linear polynomials evaluate to the unique root
    if type(a, 'complex'('rational')) then
        return _Z-a*conjugate(a);
    end if;
    if not is_simple_RootOf(a) then
        error "expected simple RootOf, got %1", a;
    end if;
    poly := op(1, a);
    conjpoly := add(conjugate(coeff(poly,_Z,j))*_Z^j, j=0..degree(poly));
    resultant(
        subs(_Z=Y, poly),
        expand(Y^(degree(poly))*eval(conjpoly,_Z=_Z/Y)),
        Y);
end proc:

# Whether two real intervals ([a,b], INTERVAL(a,b), or a..b) intersect.
iv_intersect := proc(a, b, $)
    op(1, a) <= op(2, b) and op(1, b) <= op(2, a);
end proc:

# Ensure that pol has only one root in the (real) interval iv_ini, and return a
# lower bound on its distance to the remaining roots.
remaining_roots_dist := proc(pol, iv_ini, $)
    local candidates, other_roots, ivpol, iv;
    candidates, other_roots := selectremove(iv_intersect,
                                    realroot(pol, iv_ini[2]-iv_ini[1]), iv_ini);
    ASSERT(candidates <> []);
    if nops(candidates) = 1 then
        ivpol := op(candidates);
        # One and only one of the arguments of max is positive.
        min(1, seq(max(ivpol[1]-iv[2], iv[1]-ivpol[2]), iv in other_roots));
    else
        FAIL
    end if;
end proc:

# Three-way comparison of absolute values of algebraic numbers given by
# (indexed) RootOf's.
cmp_algeb_abs := proc(a, b, $)
    option remember;
    local alg, iv, lsq, usq, sqp, dist, g, ga;
    for alg in [a, b] do
        iv[alg] := evalrC(alg);
        lsq[alg] := below_abs(iv[alg], 'rational')^2;
        usq[alg] := above_abs(iv[alg], 'rational')^2;
    end do;
    if usq[a] < lsq[b] then -1
    elif usq[b] < lsq[a] then 1
    elif abs(a) = abs(b) then 0
    else
        # Try to prove that |a|=|b|.  We compute a polynomial sqp[a] whose roots
        # include |a|², and we check that |a|² is actually the only (possibly
        # multiple, though) root of sqp[a] of value compatible with our initial
        # enclosure of |a|.  Same thing for b.  Then, it is enough to look for a
        # common root of sqp[a] and sqp[b] close to |a|² in order to decide
        # whether |a|=|b|.
        for alg in [a, b] do
            sqp[alg] := absroot_resultant(alg);
            dist[alg] := remaining_roots_dist(sqp[alg], [lsq[alg], usq[alg]]);
        end do;
        if dist[a] <> FAIL and dist[b] <> FAIL then
            g := gcd(sqp[a], sqp[b]);
            ga := select(iv_intersect, realroot(g,dist[a]/2), [lsq[a],usq[a]]);
            ASSERT(nops(ga) <= 1);
            if nops(ga) = 1 then return 0 end if;
        end if;
        # We end up here if we were unable to isolate either |a|² or |b|² among
        # the roots of psqa and psqb, if or they turned out to be close but
        # distinct after all--but we still have to tell which is larger.  In
        # either case, we refine the initial enclosure and try again.
        Digits := 2*Digits;
        fail_if_Digits_too_large("cmp_algeb_abs");
        userinfo(10, 'gfun', "cmp_algeb_abs: refining, now", "Digits"=Digits);
        cmp_algeb_abs(a, b);
    end if;
end proc:

end module:
