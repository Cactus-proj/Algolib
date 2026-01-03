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
#        compute all terms of degree exactly d over variables in v
#
# INPUT:
#        v        a list of names
#        d        an integer
#
# OUTPUT:
#        a list
#
# NOTE:
#        `randpoly/monomials/dense` computes all terms of degree less
#        than of equal to an integer
#
`Holonomy/homogeneous`:=proc(v,d)
    option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
    global randpoly;
    if nops(v)=1 then [op(v)^d] else
        randpoly;
        expand(`randpoly/monomials/dense`(expand(subsop(1=NULL,v)/v[1]),d)
            *v[1]^d)
    end if
end proc:

# FUNCTIONALITY:
#        make all Dx disappear --- differential case
#
# INPUT:
#        a skew polynomial p in x and Dx, such that Dx x = x Dx + 1
#
# OUTPUT:
#        a polynomial q free from Dx, such that q = p + Dx r, for a
#        skew polynomial r(x,Dx)
#
# ALGORITHM:
#        use the equation P(x) Dx = Dx P(x) - P'(x)
#
`Holonomy/pretreat`['diff']:=proc(p,Dx,x)
    local coeff_list,term_list,degree_list,i;
    option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
    coeff_list:=[coeffs(p,Dx,term_list)];
    term_list:=[term_list];
    degree_list:=map(degree,term_list,Dx);
    expand(convert([seq((-1)^degree_list[i]*diff(coeff_list[i],
        [x$degree_list[i]]),i=1..nops(coeff_list))],`+`))
end proc:

# FUNCTIONALITY:
#        make all Sn disappear --- difference case for the shift
#
# INPUT:
#        a skew polynomial p in n and Sn, such that Sn n = (n+1) Sn
#
# OUTPUT:
#        a polynomial q free from Sn, such that q = p + (Sn-1) r, for a
#        skew polynomial r(n,Sn)
#
# ALGORITHM:
#        use the equation P(n) (Sn-1) = (Sn-1) P(n-1) - P(n-1)
#
`Holonomy/pretreat`['shift']:=proc(p,Sn,n)
    local coeff_list,term_list,i;
    option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
    coeff_list:=[coeffs(p,Sn,term_list)];
    term_list:=[term_list];
    expand(convert([seq(subs(n=n-degree(term_list[i],Sn),coeff_list[i]),
        i=1..nops(coeff_list))],`+`))
end proc:

# FUNCTIONALITY:
#        make all Sn disappear --- q-difference case for the q-dilation
#        operator
#
# INPUT:
#        a skew polynomial p in n and Sn, such that Sn qn = q qn Sn
#
# OUTPUT:
#        a polynomial pp free from Sn, such that pp = p + (Sn-1) r, for a
#        skew polynomial r(q,qn,Sn)
#
# ALGORITHM:
#        use the equation P(qn) (Sn-1) = (Sn-1) P(qn/q) - P(qn/q)
#
# WEAKNESS:
#        Actually, one would expect this to work on a q-shift operator
#        also.  It does not due to weaknesses in the way powers q^n are
#        dealt with in the qshift predefined commutation type.
#
`Holonomy/pretreat`['qdilat']:=proc(p,Sn,qn,q)
    local coeff_list,term_list,i;
    option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
    # It seems this expand is needed.
    coeff_list:=[coeffs(expand(p),Sn,term_list)];
    term_list:=[term_list];
    # Here, we take a numer; we are thus off by a factor of a power of
    # q, which is meaningless.
    numer(convert([seq(subs(qn=qn/q^degree(term_list[i],Sn),coeff_list[i]),
        i=1..nops(coeff_list))],`+`))
end proc:

# FUNCTIONALITY:
#        elimination of an indeterminate in the sum of a left ideal and
#        a right ideal of an Ore algebra
#
# INPUT:
#        poly_list        list of generators of the left ideal
#        indet_list        list of indeterminates to be eliminated
#        TOrd                term order with respect to which the procedure
#                        returns a Groebner basis
#        init                a list of the form [elim_GB,GB,N,stop_proc],
#                        where:
#                                + elim_GB is a list of polynomials
#                                  in A;
#                                + GB is a list of polynomials
#                                  in A[Dx,x] that includes elim_GB;
#                                + N is the index of beginning of the
#                                  loop;
#                                + stop_proc(elim_GB,GB,N) is either a
#                                  procedure that returns true as long
#                                  a next iteration of the loop is
#                                  needed.
#
# OUTPUT:
#        a list [elim_GB,GB,N] where elim_GB is a Groebner basis of the
#        ideal sum(Dx[i].Big_Alg + sum(Big_Alg.g[i]) inter Small_Alg,
#        where Big_Alg is the algebra over which TOrd is built, and
#        Small_Alg is the corresponding algebra whithout the
#        indeterminates in indet_list and their pseudo-derivation.
#
# WEAKNESS:
#        there is no way to ensure the termination of the algorithm
#
# ALGORITHM:
#        compute a sequence of Groebner bases for modules, using
#        Takayama's algorithm
#
# NOTE:
#        A standard value for init
#          is [[],[],0,proc(elim_GB,GB,N) elim_GB=[] end].
#        poly_list really has to be a list.
#
# REFERENCE:
#        ``Groebner basis, integration and transcendental functions'',
#        by N. Takayama, Symbolic and algebraic computation, ACM and
#        Addison-Wesley (1990), 152--156 (Proceedings of ISSAC'90,
#        Kyoto)
#
#        ``An algorithm of constructing the integral of a module --- an
#        infinite dimensional analog of Groebner basis'', by
#        N. Takayama, Symbolic and algebraic computation, ACM and
#        Addison-Wesley (1990), 152--156 (Proceedings of ISSAC'90,
#        Kyoto)
#
takayama_algorithm:=proc(poly_list,Alg,indet_list,TOrd,init)
    local comm_type,degree_list,N0,N,i,GB,x,increment_list,elim_GB,
        stop_proc;
    option `Copyright (c) 1996-2008 Frederic Chyzak, INRIA, France`;
##### PORT TO MAPLE6!
#    global name,procedure,qdilat,takayama,
#        `Holonomy/homogeneous`,`Holonomy/pretreat`;
    global name,procedure,qdilat,takayama;
    typematch(init,[elim_GB::list,GB::list,N0::integer,
        stop_proc::{'name','procedure'}]);
    degree_list:=map(degree,poly_list,indet_list);
    for x in indet_list do
        comm_type[x]:=Alg["type_of_left",x]
    end do;
    # N0 is the degree up to which calculations have already been
    # performed.  So we begin with N=N0+1.  (Of course, N0=infinity
    # denotes that no calculations have already been performed.)
    for N from max(N0+1,min(op(degree_list)))
    while stop_proc(elim_GB,GB,N) do
        userinfo(1,'takayama',cat("degree = ",N));
        increment_list:=[seq(`if`(degree_list[i]<=N,op(expand(
            map(`*`,`Holonomy/homogeneous`(indet_list,N-degree_list[i]),
                poly_list[i]))),NULL),i=1..nops(poly_list))];
        for x in indet_list do
            # This `Holonomy/pretreat`[...] makes the
            # pseudo-differential indeterminate associated to x
            # disappear.
            increment_list:=map(`Holonomy/pretreat`[comm_type[x]],
                increment_list,Alg["right_of_left",x],x,
                # In the q-calculus case, we have to specify q.
                `if`(comm_type[x]='qdilat',Alg["q_of_right",
                        Alg["right_of_left",x]],NULL))
        end do;
        # This computes the Groebner basis of a module by Takayama's
        # algorithm.
        GB:=Groebner:-Basis(map(primpart,
            map(op,[increment_list,GB]),TOrd["order_indets"]),TOrd);
        elim_GB:=remove(has,GB,indet_list)
    end do;
    # The for has incremented N.
    [elim_GB,GB,N-1]
end proc:
