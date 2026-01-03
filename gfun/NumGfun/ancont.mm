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

## ancont.mm: Numerical evaluation and analytic continuation of holonomic functions.


ancont := module()

uses LinearAlgebra;

export rectodiffrec, parametered_rec, step_transition_matrix,
    path_transition_matrix, plot_path, fail_if_singular_path, bit_burst_path,
    subdivide_path, rewrite_path, absolute_precision_warning,
    diffeq_inicond_matrix, ext_norm_ini, apply_ini, analytic_continuation,
    transition_matrix, local_monodromy, monodromy;

####################################################################################################
## Differential / recurrence equations (formal) manipulation
####################################################################################################

## Recurrences for derivatives

# Based on code by Bruno Salvy.
rectodiffrec := proc(rec, uofk, $)::set;
    description "Given a recurrence satisfied by the coefficients of a formal power series,"
        "compute one satisfied by those of its derivative, propagating initial values as needed.";
    local newini, s, i, rrec, ini, u, k, nextvalue;
    u,k := getname(uofk);
    s := ordrec(rec, uofk);
    ini,rrec := selectremove(type,rec,`=`);
    rrec := op(rrec);
    nextvalue := solve(
        subs(ini,eval(rrec,k=0)),
        u(s));
    newini := subs(
        ini union {u(s)=nextvalue},
        [seq(i*u(i),i=1..s)]);
    newini := {seq(u(i)=newini[i+1],i=0..s-1)};
    rrec := numer(
        subs(
            k=k+1,
            eval(rrec,u=proc(k) u(k-1)/k end proc)));
    newini union {rrec}
end proc:

####################################################################################################
## Analytic continuation
####################################################################################################

parametered_rec := proc(deq, yofz, f, n, z0, delta, $)
    description "Compute a recurrence satisfied by the Taylor coefficients of the formal",
        "fundamental solutions of deq (as a function of the expansion point and the 'diagonal'",
        "initial values).";
    option cache;
    local y,z,orddeq,deq0,i,rec;
    y,z := getname(yofz);
    orddeq := orddiffeq(deq, yofz);
    deq0 := {
        algebraicsubs(deq, y = z+z0, y(z)), # Initial conditions get lost here.
        seq((D@@i)(y)(0) = delta(i)*i!, i=0..orddeq-1) };
            # *Multiply* by i! to compute Taylor coefficients from derivatives
    rectohomrec(diffeqtorec(deq0, y(z), f(n)), f(n));
end proc:

# FIXME: would use some cleaning up
step_transition_matrix := proc(deq, yofz, z0, z1, epsilon, {firstlineonly::boolean := false}, $)

    description "Compute the transition matrix for one step of analytic continuation (z0 to z1).";
    local
        generic_rec,f,n,a,delta,ordeq,nb_derivatives,i,l,coefrec,ordcoefrec,ini,
        first_partial_sums,nterms,Nth_term_form,row,mu,den,P,c;
    global exact;

    ordeq := orddiffeq(deq,yofz);
    nb_derivatives := `if`(firstlineonly, 1, ordeq);
    userinfo(2, 'gfun', sprintf("%s --> %s (%a derivative[s]), prec~=%a",
        sprint_small_approx(z0), sprint_small_approx(z1), nb_derivatives,
        evalf[5](epsilon)));

    if z0 = z1 then return(ndmatrix(IdentityMatrix(ordeq),1)) end if;

    generic_rec := parametered_rec(deq, yofz, f, n, a, delta);

    # coefrec is the recurrence satisfied by the Taylor coefficients *in z0* of the *i-th
    # derivative*
    coefrec := subs(a=z0, generic_rec);
    ordcoefrec := ordrec(coefrec, f(n));
    
    for i from 0 to nb_derivatives-1 do  # Compute the (i+1)th line of the transition matrix
        
        # Initial coefficients of each fundamental solution. (Each column of the matrix contains
        # the first coefficients of the series giving (as a function of z1-z0) the i-th Taylor
        # coefficient in z1 of one fundamental solution. Without the factor 1/i!, we would get the
        # value of the i-th derivative.)
        ini := remove(has, coefrec, n);
        mu := [ seq( [ seq(
            subs(
                eval(ini, delta = proc(x) if x=c then 1 else 0 end if end proc),
                1/i! * f(l) * (z1-z0)^l ), 
            c = 0..ordeq-1) ], l=0..ordcoefrec-1) ]; 
        first_partial_sums := convert(mu, ndmatrix);
        userinfo(6, 'gfun', "first partial sums" = print(Matrix(mu)));

        # BOUND!
        nterms := floor(Settings:-terms_factor
                        * numeric_bounds:-needed_terms(deq,yofz,i,z0,abs(z1-z0),epsilon/ordeq))
                + Settings:-terms_delta;
        userinfo(3, 'gfun', sprintf("%s --> %s, difforder=%a, #terms=%a",
            sprint_small_approx(z0), sprint_small_approx(z1), i, nterms));
        Nth_term_form := nthterm:-nth_term_of_ndseries(coefrec, f(n), z1-z0, nterms);
        row[i] := matrices:-ndmatrix_multiply(Nth_term_form,first_partial_sums);
        
        coefrec := rectodiffrec(coefrec, f(n));
        
    end do;
    
    # Gather the rows we computed in one matrix
    den := mul(op(2,row[i]), i=0..nb_derivatives-1);
    P := ndmatrix(
        Matrix([seq(
            convert(den/op(2,row[i]) * op(1,row[i]), list),
            i=0..nb_derivatives-1)]),
        den);
    userinfo(10, 'gfun', sprintf("computed matrix ~= %a", evalf[5](P)));
    return(P);

end proc:

# This computes an epsilon-approximation (with rational coefficients) of the
# transition matrix associated to deq along any nonsingular broken-line path.
# The product of the transition matrices for each step is done by binary
# splitting.
path_transition_matrix := proc(deq, yofz, path, epsilon, $)
    local step_count,P,eps,m,stage,i;
    step_count := nops(path) - 1;
    if step_count <= 0 or (step_count=1 and path[1]=path[2]) then
        ndmatrix(IdentityMatrix(orddiffeq(deq,yofz)),1);
    elif step_count = 1 then
        step_transition_matrix(deq,yofz,op(path),epsilon);
    else
        m := iquo(step_count,2);
        stage := [ path[1..m+1], path[m+1..-1] ];
        for i from 1 to 2 do
            # BOUND!
            # subtilité ici dans le cas bit-burst ?
            eps := epsilon / (2*numeric_bounds:-bound_transition_matrix(deq,yofz,stage[-i]) );
            P[i] := path_transition_matrix(deq,yofz,stage[i],eps);
        end do;
        matrices:-ndmatrix_multiply(P[2],P[1]); # ou ring/pr ?
    end if;
end proc:

####################################################################################################
## Utilities for analytic continuation paths
####################################################################################################

# Internally, it might be a better idea to represent paths as sequences of steps (increments)
# instead of one of vertices.

plot_path := proc(deq, yofz, path, $) # usage: diplay(plot_path(...))
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
    # ignore singularities at origin for now
    #sing := remove(x -> abs(x) < Float(1, 2-Digits), diffeq_singularities(deq, yofz));
    sing := diffeq_singularities(deq, yofz);
    for j from 1 to nops(path)-1 do
        for s in sing do
            t := (s-path[j])/(path[j+1]-path[j]);
            if Im(t) < Float(1, 2-Digits) and t >= 0 and t <= 1 then
                error "unable to perform analytic continuation: the path %1 passes through "
                    "(or very close to) a singularity of %2 (in the later case, try increasing "
                    "Digits).", Path, deq;
            end if;
            if check_convergence and abs(path[j+1]-path[j]) >= abs(s-path[j]) then
               error "step %1->%2 may escape from the disk of (guaranteed) convergence of "
                "the series expansions of the solutions of %3", path[j], path[j+1], deq; 
            end if;
        end do;
    end do;
end proc:

####################################################################################################
## Path rewriting
####################################################################################################

# Replace a path [a,b] where a is assumed to have small bit-size but not b by a
# path along which efficient analytic continuation is possible.
# (Compare evaldiffeq(deq[arctan],y(z),[0,evalf[1000](Pi/5)],1000) w/ and w/o
# usebitburst.)
bit_burst_path := proc(step::[complex(numeric), complex(numeric)], $)
        :: list(complex(numeric));
    local thr, z0, z1, dz, dir, z0bis, path, p, q, q0, Q;
    thr := 1;  # ``moderate'' size below which (+1) bb-evaluation is not used
    z0, z1 := op(convert(step, 'rational', 'exact'));
    dz := z1 - z0;
    if length(denom(z1)) <= thr+1 then
        return step
    end if:
    # Reduce to the case of step length < 10^-thr by introducing an intermediate
    # point z0 (= 10^-thr-approx of step[2] of small bit-size) if necessary
    if signum(abs(dz)-10^(-thr))=1 then
        dir := dz/abs(dz);
        z0bis := convert(
            evalf_complex_absolute_error(z1, thr+1), # FIXME: cvgce issues?
            'rational');
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

subdivide_path := proc(deq, yofz, path, start:=1)
    local rad, point, s, direction, digits;
    if start > nops(path) then
        error "invalid path"
    elif nops(path) > 30 then
        error "emergency stop (too many analytic continuation steps)"
    elif start = nops(path) then # base case
        path;
    else
        rad := min(seq(abs(path[start] - s), s in diffeq_singularities(deq, yofz)));
        if abs(evalf(path[start+1] - path[start])) <  0.7 * rad then
            # we have finished subdividing one step, now handle the remaining ones
            subdivide_path(deq, yofz, path, start+1)
        else
            # insert one point on the current segment, then recursively subdivide the tail of the
            # path thus created
            direction := evalf((path[start+1] - path[start])/abs(path[start+1] - path[start]));
            point := path[start] + 0.5 * rad * direction;
            # keep the bit size of the intermediate points small unless the path runs very close to
            # a singular point (note that rad(path[start+1]) >= (1-0.5)*rad)
            digits :=  max(1, -ilog10(rad)) + 1;
            #point := convert(point, 'rational', digits);
            point := convert(evalf[digits](point), 'rational', 'exact');
            subdivide_path(deq, yofz, [op(path[1..start]), point, op(path[start+1..-1])], start+1);
        end if;
    end if;
end proc:

rewrite_path := proc(deq, yofz, Path, usebitburst,
        {subdivide := true, fromzero := true}, $) :: list(complex(numeric));
    local path;
    path := Path;
    if type(path, 'complex(numeric)') then
        path := [0, path];
        try fail_if_singular_path(deq, yofz, path, 'check_convergence') catch:
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
    fail_if_singular_path(deq, yofz, path);
    if subdivide then
        path := subdivide_path(deq, yofz, path);
    end if;
    if usebitburst then
        path := [ op(path[1..-3]), op(bit_burst_path(path[-2..-1])) ];
    end if;
    fail_if_singular_path(deq, yofz, path, 'check_convergence');
    path := map(convert, path, 'rational', 'exact');
    userinfo(4, 'gfun', "analytic continuation along path", path);
    path;
end proc:

####################################################################################################
## Evaluation
####################################################################################################

absolute_precision_warning := proc($)
    userinfo(1, 'gfun', "Recall that gfun:-NumGfun works with *absolute* error.");
end proc:

#
# Error analysis: basically all error bounds in analytic_continuation should follow from
#    |A'B'-AB| <= |A'-A|·|B| + |A|·|B'-B|
# where |A| = sum(a[i,j],i,j) is the Frobenius norm, and
#
# FIXME: Vérifier que c'est bien le cas. En particulier, il y a sans doute des confusions (sans
# conséquence...) entre |A| et |A'|. Clarifier.
#

diffeq_inicond_matrix := proc(deq, yofz, $)
    local r, i, u, n, proc_ini, ini;
    r := orddiffeq(deq, yofz);
    proc_ini := rectoproc(diffeqtorec(deq, yofz, u(n)), u(n), 'remember');
    # ini = column matrix rather than vector because this extends more easily
    # to full fundamental matrices
    Matrix([seq([proc_ini(i)], i = 0..r-1)]);
end proc:

# Input:
#   ini::Matrix, initial values of a differential equation
#   force_symbolic_ini, whether to see them as symbolic even if they are cst
# Ouptut:
#   [ upper approx of "effective" Frobenius norm (1 for symbolic),
#     whether symbolic ]
ext_norm_ini := proc(ini, force_symbolic_ini := false, $)
    if (not force_symbolic_ini) and type(ini, 'Matrix'('complexcons')) then
        [ numeric_bounds:-bound_frobenius_norm(ini), false ];
    else
        if not type(ini, 'Matrix'({'name', 'poszero', 'negzero', 'cx_zero'}))
        then
            WARNING("mixed symbolic-numeric initial values: "
                "all initial values will be treated as symbolic values, and "
                "their order of magnitude will not be taken into account in "
                "error bound computations");
        end if;
        [1., true]
    end if;
end proc:


## Apply the transition matrix to the initial values. If those are constants, we
## evalf()...
## Otherwise (or if the user asked for it
## explicitly using symbini), we return a linear combination of those initial values whose
## numeric *coefficients* satisfy this accuracy requirement.

# eps = précision mathématique du calcul
# prec = où tronquer le résultat, peut être infinity
# renvoie une matrice, plus une approximation par excès de sa norme de Frobenius
# (c'est un peu redondant quand c'est une matrice de constantes, mais ça sert
# dans le cas où ini contenait des valeurs symboliques)
# renvoie toujours le "résultat final", sans ndmatrix (destiné à être appelé en
# dernier, donc, pas pour repasser derrière en multipliant à gauche par un
# vecteur ligne -- c'est la seule façon saine de faire avec la variété de
# conditions initiales qu'on a)
apply_ini := proc(transmat, ini, symbolic_ini, eps, prec, $)
    local mat, eps_ini, approx_ini, val, norm_transmat;
    norm_transmat := numeric_bounds:-bound_frobenius_norm(transmat);
    if symbolic_ini then
        mat := nthterm:-ratorfloat(transmat, prec);
        [ MatrixMatrixMultiply(mat, ini), norm_transmat ];
    else
        # remplacer Settings:-precision_{factor,delta} par une procédure 
        # precision_ini
        eps_ini := rndz(`/`, eps, rndu(`*`, 2, norm_transmat));
        approx_ini := ndmatrix_approximation( ini,
                                 Settings:-precision_ini(-ilog10(eps_ini)) );
        val := matrices:-ndmatrix_multiply(transmat, approx_ini);
        # BOUND! rounding error: |makeitfloat(x)-x| <=  6/10 sqrt(2) 10^(-result_precision)
        val := nthterm:-ratorfloat(val, Settings:-precision_ini(prec));
        [ val, numeric_bounds:-bound_frobenius_norm(val) ];
    end if;
end proc:

# Digits is used as the default precision, even though analytic_continuation takes an *absolute*
# precision.
# On veut un résultat flottant proche de la vraie valeur (en module) à 10^-d
# près. La conversion complex(rational) -> complex(float) par makeitfloat
# garantit que le flottant obtenu diffère au plus de 8.5·10^-prec du rationnel.
# On prend prec = d+1 et on calcule la valeur rationnelle à 10^-prec.
# · On a vraisemblablement un problème avec certaines bornes qui confondent
#   la précision de calcul des coefficients d'une matrice et la borne sur
#   la *norme* de celle-ci. (Il doit manquer des facteurs r par-ci par-là,
#   quoi.)
analytic_continuation := proc(userdeq, yofz::function(name),
        inipath::path, precision::posint:=Digits,
        { usebitburst::boolean:=true, forcepath::boolean:=false, 
          symbini::boolean:=false }, $ )
        ::complex_linear;
    local deq,ini,P,Q,L,epsilon,nbsteps,norm_ini,boundP,epsP,epsQ,path, symbolic_ini,
        precisionX,result_precision, resmat;
    deq := diffeqtohomdiffeq(userdeq, yofz);
    # FIXME: maybe forcepath should disable more of the rewriting?
    path := rewrite_path(deq, yofz, inipath, usebitburst,
                         'subdivide'=(not forcepath)); 
    # If the user gave symbolic initial values (or none), we will return a linear combination of
    # them (see below).
    ini := diffeq_inicond_matrix(deq, yofz);
    norm_ini, symbolic_ini := op(
        ext_norm_ini(ini, 'force_symbolic_ini' = symbini));
    nbsteps := nops(path)-1;
    # We want to compute Q·P·ini with error <= 10^(-p) (Q=row, P=square,
    # ini=col; P is itself a product of nbsteps-1 matrices). To bound the error,
    # we write Q·P·I - Q~·P~.I~ = (Q-Q~)PI + 
    # BOUND! ('-1' because of makeitfloat, see below)
    epsilon := evalf(10^(-precision-1) / (norm_ini * nbsteps)); # below?
    
    ## Compute the linear form (= row matrix) L: "initial conditions --> value in z_m" with
    ## absolute error <= nbsteps*epsilon <= ½·10^(-precision)·norm(ini~). No rounding error occurs
    ## at this point.
    boundP := numeric_bounds:-bound_transition_matrix(deq,yofz, path[1..-2]); # BOUND!
    # For the last step, we compute only the first line of the transition matrix
    epsQ := evalf( epsilon / boundP ); # BOUND!  below?
    Q := step_transition_matrix(deq, yofz, path[-2], path[-1], epsQ, 'firstlineonly');
    # Previous steps: analytic continuation, compute the whole transition matrix
    epsP := evalf((nbsteps-1)*epsilon/matrices:-ndmatrix_norm(Q));  # below?
    P := path_transition_matrix(deq, yofz, path[1..-2], epsP);
    L := matrices:-ndmatrix_multiply(Q,P);

    ## Apply the linear form L to the initial values. If those are constants, we evalf() so as to
    ## return a single number that approximates the corresponding solution of userdeq at the
    ## endpoint of path with absolute error <= 10^-precision. Otherwise (or if the user asked for it
    ## explicitly using symbini), we return a linear combination of those initial values whose
    ## numeric *coefficients* satisfy this accuracy requirement.
    resmat := apply_ini(L, ini, symbolic_ini, epsilon, precision)[1];
    absolute_precision_warning();
    resmat[1,1];

end proc:

# ajouter une fonction fundamental_solution qui fasse à peu près comme analytic_continuation mais
# avec une matrice de conditions initiales ? transition_matrix deviendrait en gros un appel à
# funcamental_solution avec des CI identité

transition_matrix := proc(userdeq, yofz::function(name),
                inipath::Or(complex(numeric),list(complex(numeric))),
                precision::posint:=Digits,
                { usebitburst::boolean:=true }, $ )
                ::Matrix(complex(float));
    local deq, path;
    deq := diffeqtohomdiffeq(userdeq, yofz);
    if type(deq, 'set') and nops(deq) > 0 then
        infolevel(1, 'gfun', "initial conditions will be ignored");
    end if;
    # FIXME: give a way not to subdivide?
    path := rewrite_path(deq, yofz, inipath, usebitburst, subdivide, fromzero=false);
    absolute_precision_warning();
    nthterm:-makeitfloat(path_transition_matrix(deq, yofz, path, 10^(-precision)), precision);
end proc:

####################################################################################################
## Monodromy
####################################################################################################

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
