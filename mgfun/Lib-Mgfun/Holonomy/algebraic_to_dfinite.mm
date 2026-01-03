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

# FUNCTIONALITY:
#        compute equations that define an algebraic function as
#        holonomic
#
# INPUT:
#        P        polynomial in x_1,...,x_n,f, that defines an algebraic
#                function f(x_1,...,x_n) by P(x_1,...,x_n,f)=0
#        f        name of the function as it appears in P
#        TOrd        term order with respect to which the output has to be
#                a Groebner basis
#
# OUTPUT:
#        a set of operators that vanish on any solution f of P and form
#        a Groebner basis with respect to T
#
# WEAKNESS:
#        so far, only return a rectangular system; the value TOrd is
#        not really taken into account
#
# ASSUMPTION:
#        the algebra on which the term order is built must be a
#        differential algebra
#        degree of P in f is positive
#        P and its derivative with respect to f are coprime
#
# ALGORITHM:
#        + compute one differential equation for each x=x_i
#        + computing an inverse modulo P for diff(P,f) yields an
#          expression for diff(f,x) in the basis of the f^k's
#        + differentiate successive equations of the form
#          den^k*S[k]*diff(f,x$k)=R[k] without introducing any fraction
#        + find linear combination without right-hand side
#
algebraic_to_dfinite:=proc(P,f,TOrd)
    local degP,x,k,x_lst,DP,A,B,R,S,den,num,eq,alg,one;
    option `Copyright (c) 1994-2002 Frederic Chyzak, INRIA, France`;
    global DiffAlgebra,right_indets;
    ASSERT(type(TOrd["algebra"],'DiffAlgebra'),
        "term order over non-differential algebra");
    degP:=degree(P,f);
    ASSERT(degP>0,"P=0");
    x_lst:=TOrd["algebra"]["left_indets"];
    DP:=diff(P,f);
    # Compute the inverse of DP modulo P.
    one:=gcdex(P,DP,f,'A','B');
    ASSERT(one=1,"non-trivial gcd");
    # DP*num/den=1 (P), where den is a polynomial in f and num is a
    # coefficient.
    num:=numer(B);
    den:=denom(B);
    # R[0]=f and S[0]=1 except when f is a rational fraction.
    R[0]:=normal(rem(f,P,f));
    S[0]:=denom(R[0]);
    R[0]:=numer(R[0]);
    # Compute one equation for each variable.
    for x in x_lst do
        # The k-th derivative of f is R[k]/(S[k]*den^k), where R[k] is
        # a polynomial in f and S[k] is a coefficient.
        for k to degP do
            # This rem may introduce rational fractions in x if the
            # leading coefficient of P contains x.  This was a bug
            # reported by Glenn P. Tesler, Wed, 25 Oct 1995 11:59:11
            # -0700 (PDT), and which led to the following
            # implementation.  S[k] is here to keep track of these
            # denominators in x appearing.
            R[k]:=normal(rem(
                den*S[k-1]*diff(R[k-1],x)
                -S[k-1]*num*diff(P,x)*diff(R[k-1],f)
                -den*diff(S[k-1],x)*R[k-1]
                -(k-1)*diff(den,x)*S[k-1]*R[k-1],P,f));
            S[k]:=denom(R[k])*S[k-1]^2;
            R[k]:=numer(R[k])
        end do;
        eq[x]:=subs(
            [seq(alg[k]=TOrd["algebra"]["right_of_left",x]^k,k=0..degP)],
            yet_another_gauss(
                [seq([R[k],den^k*S[k]*alg[k]],k=0..degP)],
                [seq(alg[k],k=0..degP),f]))
    end do;
    eval(map(primpart,[seq(eq[x],x=x_lst)],TOrd["algebra"]["right_indets"]))
end proc:
