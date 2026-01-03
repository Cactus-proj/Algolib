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

## old_bounds.mm: Experiments with bounds on tails of holonomic power series. Currently unused.
## Marc Mezzarobba, Algorithms project, INRIA Rocquencourt

####################################################################################################
## Rational polynomials
####################################################################################################

bound_ratpoly_int := proc(rat::ratpoly, x)
    local critical, candidates, i;
    description "Compute a bound on the abs(rat(i)), i nonnegative integer.";
    critical := {fsolve(denom(rat)=0),fsolve(diff(rat,x)=0)};
    candidates := select(
        type,
        map(ceil, critical) union map(floor, critical) union {0},
        'nonnegative');
    max( seq(abs(eval(rat, x=i)), i in candidates) );
end proc:

bound_ratpoly_on_disk := proc(rat::ratpoly, z::name, radius::realcons, $)::realcons;
    local theta, m, eq, critical;
    description "Bound a rational function on a zero-centered disk (containing no pole).";
    if rat = 0 then
        0
    else
        m := eval(abs(rat)^2, z=radius*exp(I*theta)) assuming theta::real;
        eq := numer(convert(diff(m, theta),trig));
        critical := select(type,{evalf(solve(eq,theta))},float) union {0.};
        max(op(map(t -> eval(m, theta=t), critical)));
    end if;
end proc:

###################################################################################################
## Other (older) majorant series bounds
###################################################################################################

# Computes a majorant for the rest starting at n of the expansion in z0 of some derivative of
# any base solution; working on a disk of radius 1/nu (which must not contain any singularity).
# Adapted from [vdH01].
`rest_majorant/vdHlike` := proc(inideq, yofz, n, derivation_order, z0, z1, $)
    option remember;
    local y,z,coef,r,P,rho,nu,M,N,A,none,absdz,i,k,bounds,j;
    y, z := getname(yofz);
    coef, r, none := read_diffeq(algebraicsubs(inideq,y=z+z0,y(z)), y(z));
    ASSERT(coef[1] = 0);
    P := table([seq(i = coef[i+2], i=0..r)]); # table pour forcer l'indexation 0..r
    rho := first_root(P[r], z); # rho may =infty
    absdz := abs(z1 - z0);
    if evalf(1/absdz < 1/rho) then # evalf --> sinon pbs pour tester...
        error "z1 should be inside the disk extending to the first singularity"
    end if;
    nu := (1/absdz+4/rho)/5; # compromis à faire...
    ASSERT( evalf(nu*absdz) < 1 );
    M := table([seq(
        i = bound_ratpoly_on_disk(P[i]/P[r], z, 1/nu),   # essayer avec bound_ratpoly_coefs
        i = 0..r-1)]);
    N := ceil(
        1/nu
        * max( seq(
            evalf((r*M[i])^(1/(r-i))),
            i = 0..r-1)));
    A := max(seq(
        1 / ( nu^j * binomial(N+j, j) ),
        j = 0..r-1)); 
    userinfo(5, 'bounds', "N=", N, "A=", A);
    return(A * Product(N+n+k,k=0..derivation_order-1) * binomial(N+n-1,n) * nu^derivation_order
        * (nu * absdz)^n * Hypergeom([1, N+n+derivation_order], [n+1], nu*absdz));
end proc:

####################################################################################################
## Naive implementation of the bound from [vdH01]
####################################################################################################

`needed_terms/vdH01/zero` := proc(deq, yofz, derivation_order, z0, a, epsilon)
    local coef, r, inicond_eqns, B, rho, tau, N, C1, C2, p, y, z;
    y, z := getname(yofz);
    if derivation_order <> 0 or z0 <> 0 then
        error("Only derivation_order=0, z0=0 are allowed with `needed_terms/vdH01/zero`");
    end if;
    p := max(0, -log10(epsilon));
    coef, r, inicond_eqns := read_diffeq(deq, yofz);
    rho := simplify(convert( (abs(a)+first_root(coef[r+2],z))/2, rational)); 
    if rho = +infinity then rho := convert( 2*abs(a), rational) end if;
    tau := rho/abs(a); 
    ASSERT(tau > 1);
    B := evalf(vdH01_bound(deq, yofz, 1/rho));
        # Attention, B n'est pas une borne sur les valeurs de f (mais tq |f_k| <= B mu^k tout de
        # même).
    C1 := evalf(1/log10(tau));
    C2 := evalf(log10(B/(1-1/tau)) / log10(tau));
    N := ceil( C1 * (p+1) + C2 );
end proc:

needed_terms := `needed_terms/sharp`;
#needed_terms := `needed_terms/vdH01/zero`;

# See [vdH01, §2.4]. Traduction la plus directe possible des formules de l'article
vdH01_bound := proc(deq, yofz, mu)
    local coef, r, inicond_eqns, P, lambda, nu, M, N, A, f, y, z;
    coef, r, inicond_eqns := read_diffeq(deq, yofz);
    y, z := getname(yofz);
    if coef[1] <> 0 then
        error "inhomogenous equation"
    end if;
    P := table([seq(i = (coef[i+2]), i=0..r)]);
    f := table([seq(
        subs( inicond_eqns, i = (D@@i)(y)(0) / i! ),
        i = 0..r-1 )]);
    lambda := 1/first_root(P[r],z);
    if evalf(mu) < evalf(lambda) then
        error "1/mu should be smaller than the distance to the first singularity"
    end if;
    nu := convert((lambda + mu)/2, rational);
    #nu := convert((lambda + 4*mu)/5, rational);
    M := table([seq(
        i = bound_ratpoly_on_disk(P[i]/P[r], z, nu),
        #i = bound_ratpoly_coeffs(P[i]/P[r], z, nu, 1), 
        i = 0..r-1)]);
    N := ceil(
        1/nu
        * max( seq(
            evalf((r*M[i])^(1/(r-i))),
            i = 0..r-1)));
    A := max(seq(
        abs(f[i]) / ( nu^i * binomial(N+i, i) ),
        i = 0..r-1));
    evalf(A * binomial( ceil(mu/(mu-nu) * N), N ) * (nu/mu)^floor( nu/(mu-nu) * N ))
end proc:


