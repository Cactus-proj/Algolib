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

### Was:
### //wmi/projects/mapleV/inria/lib/Groebner/src/find_dependency.mm#1 - add change 116973 (text)
### until this was removed from Groebner.  In fact, this is never used
### by either Ore_algebra or Groebner by themselves.

# FUNCTIONALITY:
#        compute a non-zero, constant linear combinaison of the
#        equations of the list equa_list of pairs of polynomials in the
#        indeterminates of var_list
#
# INPUT:
#        equa_list       a list of pairs [NF[i], alg[i]], where
#                          + NF[i] is a polynomial in the u[i]'s
#                          + alg[i] is a polynomial in the v[i]'s
#        NF_var_list     a list of indeterminates, the u[i]'s
#        pp_var_list     a list of indeterminates, the v[i]'s
#
# OUTPUT:
#        a linear combinaison of the alg[i] of the form
#        sum(c[i]*alg[i]) with the property that sum(c[i]*NF[i]) is
#        zero, or FAIL if no such dependency can be found; the output
#        is made primitive with respect to the v[i]'s
#
# ASSUMPTION:
#        polynomials in equa_list must be expanded with respect to the
#        u[i]'s
#
# ALGORITHM:
#        Fraction-free Gaussian elimination with partial pivot
#
yet_another_gauss:=proc(equa_list, NF_var_list, pp_var_list, $)
    local equa_nb, i, j, L, c, t, input_term_list, all_input_term_list,
        term_nb, all_var_list, sb, pvc, pvm, pvd, pvl, ct, cl, dt, term_lcm,
        u, m, den, expanded_form ;
    option `Copyright (c) 1994-2009 Frederic Chyzak, INRIA, France` ;
    equa_nb := nops(equa_list) ;
    # Input need not be fraction-free: only the calculation and the output are.
    for i to equa_nb do
      den := lcm(op(denom(equa_list[i]))) ;
      L[i] := normal(map(`*`, equa_list[i], den))
    end do ;
    userinfo(1, 'gausselim', "elimination of ", nops(NF_var_list),
        " variables from ", equa_nb, " equations") ;
    userinfo(3, 'gausselim',
        "first convert the problem to linear algebra") ;
    all_var_list := [op(NF_var_list), u] ;
    for i to equa_nb do
        L[i] := [collect(u*L[i][1], all_var_list, 'distributed'), L[i][2]] ;
        # Store monomial into t[i].
        c[i] := coeffs(L[i][1], all_var_list, t[i])
    end do ;
    # Select monomials other than 1.
    input_term_list := [op({seq(t[i], i=1..equa_nb)} minus {1})] ;
    # Adds those of NF_var_list that could have been forgotten.
    all_input_term_list := {op(input_term_list),
        seq(op(indets(L[i][2])), i=1..equa_nb),
        # This type took me so much time to figure out!
        # It is to allow things like qfactorial(q,n-m) to work.
        op(indets(indets([args], function), `^`(name,name)))} ;
    term_nb := nops(input_term_list) ;
    # Monomials t[i] are renamed by variables m[j] (up to order and 1).
    sb := [seq(input_term_list[i]=m[i], i=1..term_nb)] ;
    t := subs(sb, op(t)) ;
    for i to equa_nb do
        L[i] := [convert(zip((a,b)->a*b, [c[i]],[t[i]]),`+`), L[i][2]]
    end do ;
    userinfo(3, 'gausselim', "now perform Gaussian elimination") ;
    userinfo(5, 'gausselim',
        "the matrix before any elimination looks like: ", eval(L)) ;
    for i to equa_nb do
        userinfo(4, 'gausselim', "finds a pivot on a new equation") ;
        # If the left-hand side is 0, elimination has been achieved.
        if L[i][1] = 0 then
            userinfo(5, 'gausselim', "no pivot found on line ", i) ;
            break
        end if ;
        pvm:=1 ;  # Monomial chosen.
        pvd:=-1 ; # Degree of the corresponding coefficient.
        pvl:=0 ;  # Number of terms of the corresponding coefficient.
        for j to term_nb do
            # Choose pivot element of least degree and least length.
            ct := coeff(L[i][1],m[j]) ;
            dt := degree(ct) ;
            cl := nops(ct) ; # Or length?
            if ct<>0 and (dt<pvd or pvd=-1) and (pvl=0 or cl<pvl) then
                pvc := ct ;
                pvm := m[j] ;
                pvd := dt ;
                pvl := cl
            end if
        end do ;
        ASSERT(pvm <> 1, "BUG, found pivot should not be 1") ;
        userinfo(5, 'gausselim', "the pivot is: ", pvc*pvm) ;
        for j from i+1 to equa_nb do
            ct := coeff(L[j][1], pvm) ;
            if ct<>0 then
                term_lcm := lcm(pvc, ct) ;
                expanded_form :=
                  expand(normal(term_lcm/ct)*L[j]-normal(term_lcm/pvc)*L[i]) ;
                expanded_form := eval(expanded_form,
                  map(x -> x = freeze(x),
                    indets(expanded_form, 'specfunc(anything,
                      {:-ApplyOpr,:-AlgSubs,
                      Ore_algebra:-ApplyOpr,Ore_algebra:-AlgSubs})'))) ;
                expanded_form := Ore_algebra:-OA_Internals:-vectprimpart(
                  expanded_form,
                  # When removing contents, we must take care of pairs
                  # of the form [1,f(x)]: contents must also be taken
                  # with respect to the indeterminate f(x).
                  all_input_term_list) ;
                L[j] := thaw(expanded_form)
            end if
        end do ;
        # Now, the i-th line is useless, so discard it, and free the
        # corresponding memory.
        L[i] := evaln(L[i]) ;
        c[i] := evaln(c[i]) ;
        userinfo(5, 'gausselim',
            "the matrix after elimination looks like: ", eval(L))
    end do ;
    userinfo(3, 'gausselim',
        "finally return to the original problem") ;
    if L[i][1] = 0 then
        primpart(subs([seq(m[j] = input_term_list[j], j=1..term_nb)],
            L[i][2]), pp_var_list)
    else
        FAIL
    end if
end proc :
