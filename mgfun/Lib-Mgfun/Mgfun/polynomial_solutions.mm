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

#####
##### For the (usual) shift case only!
#####
##### (After the summary in the proceedings of the Algo seminar.)
`Mgfun/degree_bound`:=proc(eq,psi,k,order)
    local CF,cf,tmp,i,j,X,a,b,d,alpha;
    option `Copyright (c) 2000-2002 Frederic Chyzak, INRIA, France`;
    global nonnegint;
    # Rewrite the coefficients of eq viewed as an operator in Dk=Sk+1
    # on the basis of the binomial(k-a,i), which is denoted X^i here.
    for i from 0 to order do
        cf[i]:=expand(coeff(eq,psi(k+i)))
    end do;
    for i from 0 to order do
        CF[i]:=add(binomial(j,i)*cf[j],j=i..order)
    end do;
    a:=max(-1,op(`Mgfun/integer_solns`(CF[order],k)))+1;
    for i from 0 to order do
        tmp:=CF[i];
        cf[i]:=0;
        while tmp<>0 do
            d:=degree(tmp,k);
            cf[i]:=cf[i]+d!*coeff(tmp,k,d)*X^d;
            tmp:=tmp-expand(d!*coeff(tmp,k,d)*binomial(k-a,d))
        end do
    end do;
    b:=max(seq(degree(cf[i],X)-i,i=0..order));
    d:=max(seq(degree(cf[i],X),i=0..order));
    alpha:=add(add(expand(binomial(X+b,j)*binomial(j,b+i)*coeff(cf[i],X,j)),
        j=0..d),i=0..order);
    max(-b-1,degree(remove(has,eq,psi),k)-b,
        op(select(type,`Mgfun/integer_solns`(alpha,X),'nonnegint')))
end:

`Mgfun/polynomial_solve`['shift']:=proc(eqn,psi,k,order,param_set)
    local D,P,T,eq,c,i,sys,var,sol,remaining_ci;
    option `Copyright (c) 2000-2002 Frederic Chyzak, INRIA, France`;
    D:=`Mgfun/degree_bound`(eqn,psi,k,order);
    eq:=collect(eqn,psi,expand);
    P:=add(c[i]*k^i,i=0..D);
    for i from 0 to order do
        T[i]:=expand(subs(k=k+i,P));
        T[i]:=expand(coeff(eq,psi(k+i))*T[i])
    end do;
    eq:=expand(remove(has,eqn,psi))+add(T[i],i=0..order);
    sys:={coeffs(eq,k)};
    var:=param_set union {seq(c[i],i=0..D)};
    sol:=SolveTools:-Linear(sys,var);
    # Remove possible unconstrained c[i]'s.
    remaining_ci:=select(has,map2(op,1,select(evalb,sol)),c);
    if remaining_ci<>{} then
        P,sol:=op(subs(c[min(op(map(op,remaining_ci)))]=1,
            map(v->v=0,remaining_ci),[P,sol]));
        sol:=sol minus {1=1}
    end if;
    subs(sol,P),{seq(i=normal(subs(sol,i)),i=param_set)}
end:

# Empty shell.
# Should be changed into a user-level command that calls the
# appropriate `Mgfun/polynomial_solve`, possibly made available through
# Mgfun[polynomial_solve].
polynomial_solutions:=proc()
    option `Copyright (c) 2000-2002 Frederic Chyzak, INRIA, France`;
end:
