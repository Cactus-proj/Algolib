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

`Mgfun/diff_to_order`:=proc(expr)
    option `Copyright (c) 2000-2002 Frederic Chyzak, INRIA, France`;
    global function,diff;
    if type(expr,'function') and op(0,expr)='diff' then
        `Mgfun/diff_to_order`(op(1,expr))+1
    else
        0
    end if
end:

`Mgfun/integer_solns`:=proc(P,x)
    option `Copyright (c) 2000-2002 Frederic Chyzak, INRIA, France`;
    global integer;
    `if`(indets(P)={x},
        map2(op,[1,2],[isolve(P)]),
        # This ``solve'' still costs a lot.
        select(type,[solve(P,x)],'integer'))
end:

`Mgfun/denominator_bound`['shift']:=proc(AA,BB,x)
    local su,A,B,u,R,m,N,i,j,d,TIME;
    option `Copyright (c) 2000-2002 Frederic Chyzak, INRIA, France`;
    global nonnegint;
TIME:=time();
    N:=infinity;
    to `if`(indets([AA,BB])={x},1,5) while N>10 do
userinfo(2,'Mgfun',sprintf("Intermediate bound on dispersion: %a",N));
        A:=0;
        while A*B=0 do
            su:=map(v->v=rand()/rand(),indets([AA,BB]) minus {x});
            A:=subs(su,AA);
            B:=subs(su,BB)
        end do;
        u:=1;
        # Factoring usually helps the calculation of a resultant a lot.
        A:=factor(A);
        B:=factor(B);
        R:=resultant(A,subs(x=x+m,B),x);
        N:=min(N,
            max(0,op(select(type,`Mgfun/integer_solns`(R,mb),'nonnegint'))))
    end do;
userinfo(2,'Mgfun',sprintf("Bound on dispersion: %a",N));
    A:=AA;
    B:=BB;
    for i from N to 0 by -1 do
        d:=gcd(A,subs(x=x+i,B));
        A:=normal(A/d);
        B:=normal(B/subs(x=x-i,d));
        u:=u*mul(subs(x=x-j,d),j=0..i)
    end do;
TIME:=time()-TIME;
userinfo(3,'Mgfun',sprintf("Computing a resultant: %a seconds.",TIME));
    u
end:

`Mgfun/rational_solve`['shift']:=proc(lofe,phi_of_k,param_set)
    local phi,psi,k,order,cf,phi_denom,shifted_phi_denom,
        D,eq,i,sol,res,X,TIME;
    option `Copyright (c) 2000-2002 Frederic Chyzak, INRIA, France`;
    global function,infinity,name;
    phi:=op(0,phi_of_k);
    k:=op(phi_of_k);
    order:=max(op(map(op,select(has,indets(lofe,'function'),phi))));
    if order<>-infinity then
        order:=order-k
    end if;
if order<>0 then
TIME:=time();
    for i from 0 to order do
        cf[i]:=coeff(lofe,phi(k+i))
    end do;
    # Il faut une borne pour l'exposant de phi_denom.
    phi_denom:=`Mgfun/denominator_bound`['shift'](
        expand(subs(k=k-order,cf[order])),expand(cf[0]),k);
TIME:=time()-TIME;
userinfo(3,'Mgfun',sprintf("Computing denominator bound: %a seconds.",TIME));
    for i from 0 to order do
        shifted_phi_denom[i]:=subs(k=k+i,phi_denom);
    end do;
TIME:=time();
    # Substitute phi(k)=psi(k)/phi_denom into lofe.
    eq:=eval(subs(phi=0,lofe))
        +add(normal(cf[i]/shifted_phi_denom[i])*psi(k+i),i=0..order);
    eq:=normal(eq);
    eq:=numer(eq);
    sol:=`Mgfun/polynomial_solve`['shift'](eq,psi,k,order,param_set);
    res:={phi=unapply(normal(sol[1]/phi_denom),k)} union sol[2]
else
    res:={phi=unapply(op([1,2],SolveTools:-Linear(
            [subs(phi_of_k=X,lofe)],[X])),k),seq(i=i,i=param_set)}
end if;
    remove(evalb,res)
end:

`Mgfun/denominator_bound`['diff']:=proc(C,x)
    option `Copyright (c) 2000-2002 Frederic Chyzak, INRIA, France`;
    # Temporary: wrong heuristic.
    C
end:

`Mgfun/rational_solve`['diff']:=proc(lofe,phi_of_k,param_set)
    local phi,k,order,cf,phi_denom,lofo,U,
        D,eq,c,i,j,sys,var,sol,res,remaining_ci,X,TIME;
    option `Copyright (c) 2000-2002 Frederic Chyzak, INRIA, France`;
    global function,infinity,name;
    phi:=op(0,phi_of_k);
    k:=op(phi_of_k);
    order:=max(op(map(`Mgfun/diff_to_order`,
        select(has,indets(lofe,'function'),phi))));
if order<>0 then
TIME:=time();
    lofo:=subs([seq(diff(phi(k),[k$i])=U^(i+1),i=0..order)],lofe);
    for i from 0 to order do
        cf[i]:=coeff(lofo,U,i+1)
    end do;
    # Il faut une borne pour l'exposant de phi_denom.
    phi_denom:=`Mgfun/denominator_bound`['diff'](cf[order],k);
TIME:=time()-TIME;
userinfo(3,'Mgfun',sprintf("Computing denominator bound: %a seconds.",TIME));
TIME:=time();
    # Il faut une vraie borne pour ce D.
    D:=7+2*degree(phi_denom,k);
    # Substitute phi(k)=add(c[j]*k^j,j=0..D)/phi_denom into lofe.
    eq:=eval(subs(phi=0,lofe))
        +add(normal(cf[i]*my_normal(
            diff(add(c[j]*k^j,j=0..D)/phi_denom,[k$i]))),i=0..order);
    eq:=normal(eq);
    eq:=numer(eq);
    sys:={coeffs(eq,k)};
    var:=param_set union {seq(c[i],i=0..D)};
    sol:=SolveTools:-Linear(sys,var);
    res:={phi=unapply(normal(subs(sol,add(c[i]*k^i,i=0..D)/phi_denom)),k),
        seq(i=normal(subs(sol,i)),i=param_set)};
    # Remove possibly remaining c[i]'s
    remaining_ci:=select(has,indets(res,'name'),c);
    if remaining_ci<>{} then
        res:=subs(c[min(op(map(op,remaining_ci)))]=1,
            map(v->v=0,remaining_ci),res)
    end if
else
    res:={phi=unapply(op([1,2],SolveTools:-Linear(
        [subs(phi_of_k=X,lofe)],[X])),k),seq(i=i,i=param_set)}
end if;
    remove(evalb,res)
end:

# Empty shell.
# Should be changed into a user-level command that calls the
# appropriate `Mgfun/rational_solve`, possibly made available through
# Mgfun[rational_solve].
rational_solutions:=proc()
    option `Copyright (c) 2000-2002 Frederic Chyzak, INRIA, France`;
end:
