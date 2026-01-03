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

####################################################################################################
## Majorant series for rational polynomials
####################################################################################################

bound_ratpoly := module()

export
    parse_exppoly_term, ratabove_algebraic, ratabove_algebraic_doit, bound_term,
    bound_tail, doit, ModuleApply;

# Dans tout ce truc, ça serait quand même bien d'arriver à faire le calcul entier en flottants, sans
# above(), et de vérifier à la fin. Avec en option le fait de faire exactement en rationnels ce qui
# peut l'être.

# Input:  expression of the form c·(n+1)···(n+d)·a^(-n)
# Output: [c, d, a] 
parse_exppoly_term := proc(Term, n, $)::[complexcons,nonnegint,anything];
    local t, c, p, e, d, a;
    if type(Term, `*`) then t := [op(Term)] else t := [Term] end if;
    c, t := selectremove(type, t, 'constant');
    p, t := selectremove(type, t, 'linear'(n));
    e, t := selectremove(type, t, 'specop'('anything', `^`));
    if t <> [] or nops(e) > 1 or nops(e) = 1 and op(2,op(e)) <> -n then
        error "unable to parse %1", Term;
    end if;
    d := nops(p);
    a := `*`(op(map[2](op,1,e)));
    ASSERT( `*`(op(subs(n=0,p))) = d! );
    [`*`(op(c)), d, a];
end proc:

# Input:  zeta algebraic, abs(zeta) <= 1
# Output: r rational s.t. abs(zeta) <= r <= 1 with r < 1 if abs(zeta) < 1
ratabove_algebraic := proc(zeta)
    option cache;
    Digits := 10;
    ratabove_algebraic_doit(zeta);
end proc:

ratabove_algebraic_doit := proc(zeta)
    local num, r, a;
    if Digits > 200 then error "emergency stop: Digits too large (%1)", Digits end if;
    #if type(zeta, 'complex'('rational')) or hastype(zeta, 'complex'('float')) then
    if type(zeta, 'complex'('rational')) then
        a := abs(zeta);
        if a^2 > 1 then error "something is going wrong here" end if;
        return a;
    end if;
    num := abs(evalf(zeta)); # no cancellation here
    r := num + Float(1, 2-Digits);
    if r < 1 then
        convert(r, 'rational', 'exact');
    elif num > 1 +  Float(1, 2-Digits) then
        error "expected abs(zeta) <= 1, received zeta = %1", zeta;
    else  # it seems that abs(zeta) = 1
        a := abs_with_RootOf(zeta);
        if a = 1 then
            1
        else
            Digits := 2 * Digits;
            procname(a);
        end if;
    end if;
end proc;

# Helper function for bound_ratpoly:-doit.
#
# Input:  'Term' is one term of a sum returned by ratpolytocoeff, that is, an
#   expression of the form Sum(rat(zeta,n)·zeta^(-n), zeta=RootOf(P_zeta)) with
#   possible degenerate cases ranging from a single rat(n)·zeta^(-n) to a
#   constant
# Output: 'bound' and 'n0' such that for n >= n0, abs(Term(n)) <= [z^n]
#   bound/(1-alpha·z)^m.
bound_term := proc(Term, n, alpha, m, $)
    local expr, eq, zeta, P_zeta, npart, rat, ldeg, poly, sum_value, infroot, cste, deg, t, lambda,
       bound, k, n0, sol;
    # relâcher la contrainte de type et déplacer en fallback si parse_exppoly_term échoue ??
    if type(Term, 'Or'('constant','linear'(n))) then # there may remain a linear part <> n+1...
        if Term = 0 then return [0, 1] end if; # even if alpha > 1
        ASSERT(m >= degree(Term, n), "m too small");
        lambda := ratabove_algebraic(1/alpha); 
        npart := abs(Term) assuming n::nonnegint;
        bound := (m-1)! * normal(npart/mul(n+k,k=1..m-1)) * lambda^n;
        return [bound, 1];
    elif type(Term, 'specfunc'('anything', 'Sum')) then
        expr, eq := op(Term);
        ASSERT(type(rhs(eq), 'RootOf'), "malformed expression");
        zeta := lhs(eq); P_zeta := subs(_Z=zeta, op(rhs(eq)));
        if type(expr, `*`) then expr := [op(expr)] else expr := [expr] end if;
        npart, rat := selectremove(has, expr, n);
        npart := `*`(op(npart)); rat := `*`(op(rat));
        t := parse_exppoly_term(npart, n); ASSERT(t[1] = 1); ASSERT(t[3] = zeta);
        cste := add(
            bound_abs_interval(evalrC(subs(_alpha=evalrC(sol),rat))),
            sol in [fsolve(P_zeta, zeta, 'complex')]);
        cste := ratabove(cste);
        userinfo(5, 'gfun', 'cste'=cste);
        infroot := dominant_root(P_zeta,zeta)[1]; # one of the zeta's closest to 0
        deg := t[2];
    else 
        t := parse_exppoly_term(Term, n);
        cste := abs(t[1]); deg := t[2]; infroot := t[3];
    end if;
    lambda := ratabove_algebraic(1/(alpha*infroot));
    bound := cste * (m-1)! * normal(mul(n+k,k=1..deg)/mul(n+k,k=1..m-1)) * lambda^n;
    if m > deg then
        n0 := 1
    else
        ASSERT(lambda < 1);
        n0 := max(1, ceil(above((deg-m)/log(1/lambda))));
    end if;
    [bound, n0];
end proc:

# FIXME: Le truc avec bound_ratpoly_max_expand ne marche pas bien, parce qu'au
# moins sur certains exemples (Airy), on gagne sur certains coeffs de l'équa
# diff mais pas sur le coeff dominant, de sorte que la borne a une partie
# exp(poly) inutile !

bound_tail := proc(rat, z, alpha, m, n, $)
    local den, polypart, num, rofn, term_bounds, tofn, n0, prev_tail_bound,
        tail_bound;
    den := denom(rat); polypart := quo(numer(rat), den, z, 'num');
    # Compute the general coefficient of the series expansion of rat, as a sum (of Sums over the
    # roots of some squarefree factor of den) of terms of the form cste·(n+1)···(n+k)·zeta^(-n)
    # (where cste usually involves the root over which the Sum if taken).  Trivial piecewise's may
    # appear, convert/piecewise gets rid of them.
    rofn := convert(ratpolytocoeff(num/den, z, n),'piecewise');
    if type(rofn, `+`) then rofn:=[op(rofn)] else rofn:=[rofn] end if;
    # Compute a formal expression that bounds abs(rofn)/([z^n]1/(1-alpha·z)^m) and a positive
    # integer n0 such that our bound is nonincreasing for n >= n0.
    term_bounds := map(bound_term, rofn, n, alpha, m);
    tofn := `+`(op(map[2](op, 1, term_bounds)));
    n0 := max(op(map[2](op, 2, term_bounds)));
    # Increase n0 so that |[z^n]rat| <= tofn(n) for n >= n0. 
    n0 := max(1, degree(polypart) + 1, n0);
    # (We could already return [tofn, n0], with a "small" n0.)

    # Increase n0 again until tofn(n0) seems to approach its limit.
    while prev_tail_bound/tail_bound > Settings:-bound_ratpoly_tail_prec
            and n0 <= Settings:-bound_ratpoly_max_expand
            or type(prev_tail_bound, 'symbol') do
        prev_tail_bound := tail_bound;
        tail_bound := evalf(subs(n=n0, tofn));
        n0 := 2*n0;
    end do;
    n0 := iquo(n0, 2);

    [tofn, n0]
end proc:


# Input:  a rational function rat(z), an algebraic number alpha, a positive integer m; with
#   abs(alpha) <= abs(dominant singularity of rat), and, in the case
#   abs(alpha) = abs(dominant sing), m >= multiplicity(dominant sing)
# Output: A, [b0, b1, ..., b(n0-1)]] s.t. |[z^n]rat| <= A·([z^n]1/(1-alpha·z)) for n >= n0,
#   and |[z^n]rat| <= A·([z^n]1/(1-alpha·z)) + b_n for n < n0
doit := proc(rat, z, alpha, m, $)
    local tofn, n0, tail_bound, l_alpha, headrec, head, head_bound, j, y, u, n, delta;
    if type(rat, 'polynom') then
        head_bound := [seq(abs(coeff(rat,z,i)), i=0..degree(rat))];
        tail_bound := 0;
    else
        ASSERT(evalf(abs(alpha)) > 0);
        ## Compute a tight bound on the tail of the series
        tofn, n0 := op(bound_tail(rat, z, alpha, m, n));
        tail_bound := ratabove(subs(n=n0, tofn));  # this is A
        ## Bound the head
        headrec := diffeqtorec(y(z)=rat, y(z), u(n));
        head := rectoproc(headrec, u(n), 'remember');
        # no cancellation here, we get l_alpha < abs(alpha)
        l_alpha := convert(abs(evalf(alpha)) - Float(1, 2-Digits), 'rational', 'exact');
        delta := seq(
            abs(head(j)) - tail_bound * l_alpha^j * binomial(j+m-1, m-1),
            j = 0..n0-1);
        head_bound := [seq(max(0, delta[1+j]), j = 0..n0-1)];
        head_bound := convert(rndu(head_bound), rational, exact);
        userinfo(4, 'gfun', sprintf("bounded %a by %a/(1-%a*%a)^%a + %a", rat,
            evalf[5](tail_bound), abs(evalf[5](alpha)), z, m,
            PolynomialTools[FromCoefficientList](evalf[5](head_bound), z)));
    end if;
    tail_bound, head_bound;
end proc:
    
# éventuellement, réécrire un bound_ratpoly:-doit à l'ancienne (sans partie polynomiale) qui
# appelle le précédent

ModuleApply := proc(Rat::ratpoly, z::name, $) # exported
    description "Compute a majorant of the form z^(-t)*M/(1-alpha·z)^m+P(z) for rat.";
    local rat, mult0, dompole, mult, M, P, i, maj;
    rat := normal(Rat);
    mult0 := bounds:-common_root_multiplicity(denom(rat), z, z);
    rat := normal(z^mult0 * rat);
    dompole, mult := op(dominant_root(denom(rat), z));
    M, P := bound_ratpoly:-doit(rat, z, 1/dompole, mult);
    maj := M * 1/(1-z/abs_with_RootOf(dompole))^mult 
            + PolynomialTools:-FromCoefficientList(P, z);
    z^(-mult0) * maj;
end proc:

end module:


