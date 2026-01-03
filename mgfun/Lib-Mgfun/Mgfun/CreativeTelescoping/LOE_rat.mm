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

## LOE_rat.mm
## Copyright (c) 2009, INRIA. All rights reserved.
## Author: Lucien Pech <lucien.pech@ens.fr>.




## LOE_rat


LOE_rat := proc(Alg_::OreAlgebra,
                L_::list, var::name, const_rhs_ := 0, var_rhs_::list := [],
                prev_constraints::set := {}, formal_indet::name := 'p',
                eta::name := 'Eta',
                indices_eta::list(list) := [seq([i], i = 0..nops(var_rhs_)-1)],
                $)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local d, dvar, Alg, L, const_rhs, var_rhs, type_var, V, diff_equ,
    diff_equ_poly, equ, comm_denom, new_var_rhs, poly_sol, U, i,
    k, sqfr_V, max_denom, T;

  ASSERT(L_[-1] <> 0, "L[-1] <> 0");
  ASSERT(Alg["type_of_left", var] = 'diff' or L_[1] <> 0, "L[1] <> 0");

  d := nops(L_)-1;
  dvar := Alg_["right_of_left", var];

  if Alg_["type_of_left", var] in {'dual_shift', 'dual_qdilat'} then
    Alg := Ore_algebra:-dual_algebra(Alg_, {dvar});
    L := map2(Ore_algebra:-applyopr, dvar^d, ListTools:-Reverse(L_), Alg);
    const_rhs := Ore_algebra:-applyopr(dvar^d, const_rhs_, Alg);
    var_rhs := map2(Ore_algebra:-applyopr, dvar^d, var_rhs_, Alg)
  else
    Alg, L, const_rhs, var_rhs := Alg_, L_, const_rhs_, var_rhs_
  end if;

  type_var := Alg["type_of_left", var];
  V := denom_solution(Alg, L, var);

  if type_var = 'diff' then

    divide(V, gcd(V, diff(V, var)), 'sqfr_V');
    max_denom := lcm(V*sqfr_V^(nops(L)-1), denom(const_rhs));

    for i from 0 to nops(L)-1 do
      T[i] := diff(U(var) / V, [var$i]);
      divide(max_denom, denom(T[i]), 'den');
      T[i] := expand(numer(T[i])*den);
    end do ;

    diff_equ := add(L[i+1] * T[i], i = 0..nops(L)-1);
    diff_equ_poly := collect(eval(diff_equ,
                                  [seq(diff(U(var), [var$i]) = dx^(i+1),
                                       i = 0..nops(L)-1)]), dx);

    equ := [seq(expand(coeff(diff_equ_poly, dx, k)), k = 1..nops(L))];
    const_rhs := normal(max_denom*const_rhs - eval(diff_equ_poly, dx = 0));
    var_rhs := map(normal@`*`, var_rhs, max_denom);

  else

    equ := map(normal, [seq(L[i+1] / Ore_algebra:-applyopr(dvar^i, V, Alg),
                            i = 0..nops(L)-1)]);

    comm_denom := lcm(op(denom(equ)), denom(const_rhs));
    equ := map(normal@`*`, equ, comm_denom);
    const_rhs := normal(const_rhs * comm_denom);
    var_rhs := map(normal@`*`, var_rhs, comm_denom);

  end if;

  poly_sol := LOE_poly(Alg, equ, var, const_rhs, var_rhs,
                       prev_constraints, formal_indet, eta, indices_eta);

  if poly_sol = [] then [] else [normal(poly_sol[1] / V), poly_sol[2]] end if;

end proc;




## denom_solution


denom_solution := proc(Alg::OreAlgebra, L::list, var::name, $)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local type_var, dvar;

  type_var := Alg["type_of_left", var];
  dvar := Alg["right_of_left", var];

  if type_var = 'shift' then
    denom_solution_shift(L, var);
  elif type_var = 'diff' then
    denom_solution_diff(L, var);
  elif type_var = 'qdilat' then
    denom_solution_qdilat(L, var, Alg["q_of_right", dvar]);
  else
    error "Operator type has to be either shift, diff or qdilat.";
  end if

end proc;


denom_solution_shift := proc(L, var::name, $)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

   local d, vars, R, N, i, LL;

   d := nops(L)-1;
   vars := indets(L) minus {var};

   LL := `if`(vars={},L,subs([seq(v=rand(142..2530)(142..2530)/127,v=vars)],L));
   R := resultant(normal(eval(LL[1], var = var + N)),
                  normal(eval(LL[-1], var = var - d)), var);

   N := max(-1, op(select(type, map2(op, 1, roots(R, N)), nonnegint)));

   gcd(mul(eval(L[1], var = var + i), i = 0..N),
       mul(eval(L[-1], var = var - d - i), i = 0..N));

end proc;




## denom_solution_diff


denom_solution_diff := proc(L::list, var::name, $)

  option
    `Copyright (c) 2010, INRIA. All rights reserved.`;

  local nu, Lambda_rat, get_integer_roots, r_min, V, m, r, f;

  nu := proc(l_k, var::name, pi)
    local nu_k, ll_k;
    if l_k = 0 then return infinity end if;
    ll_k := l_k;
    for nu_k from 0 while ll_k <> 0 and rem(ll_k, pi, var, 'll_k') = 0 do
    end do;
    nu_k
  end proc;

  Lambda_rat := proc(pi, m, new_var::name)
    local s, k, l_k, nu_k, j;
    s := 0;
    for k from 0 to nops(L)-1 do
      l_k := L[k+1];
      nu_k := nu(l_k, var, pi);
      if nu_k-k = m then
        s := s + diff(l_k, [var$nu_k]) / nu_k! * mul(new_var-j+1, j = 1..k)
      end if
    end do;
    s
  end proc;

  get_integer_roots := proc(Lambda, var::name)
    local indeterminates, i;
    indeterminates := indets(Lambda) minus {var};
    [isolve(eval(Lambda, [seq(i = rand()/127, i in indeterminates)]))]
  end proc;

  r_min := proc(pi, m)
    local R, r, Q, rr_min, RR, lambda_i, integer_roots, ind;
    R := [pi, collect(expand(Lambda_rat(pi, m, r)), var)];
    ind := [op(indets(R) minus {r, var})];
    R := eval(R, [seq(ind[i] = 1/ithprime(4+i), i = 1..nops(ind))]);
    rr_min := infinity;
    while R[-1] <> 0 do
      RR := collect(expand(rem(R[-2], R[-1], var, 'Q')), var);
      lambda_i := lcm(op(map(denom, [coeffs(collect(Q, var), var)])));
      integer_roots := get_integer_roots(lambda_i, r);
      if integer_roots <> [{}] then
         rr_min := min(rr_min, op(map(rhs@op, integer_roots)));
      end if;

      R := [op(R), collect(expand(lambda_i * RR), var)]
    end do;
    if rr_min < 0 then
      -rr_min
    else
      0
    end if
  end proc;

  V := 1;
  for f in factors(L[-1])[2] do
    if degree(f[1], var) > 0 then
      m := min(map(nu, L, var, f[1]) - [$0..nops(L)-1]);
      r := r_min(f[1], m);
      V := V * f[1]^max(r, m)
    end if
  end do;

  V

end proc;




## denom_solution_qdilat


denom_solution_qdilat := proc(L::list, var::name, q::name, $)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local r, low_degree, i, n, deg_0, LL, comm_denom, H;

  r := nops(L)-1;
  low_degree := min(op(map(ldegree, L, var)));
  deg_0 := max(0, op(select(type, [solve(add(coeff(L[i+1], var, low_degree)*
                                             q^(n*(r-i)), i = 0..r), n)],
                            nonnegint)));

  LL := [seq(normal(L[i+1]/(var*q^i)^deg_0), i = 0..r)];
  comm_denom := lcm(op(map(denom, LL)));
  LL := map(normal@`*`, LL, comm_denom);

  H := max(-1, op(select(type,
                         [solve(resultant(normal(eval(L[1], var = var*q^n*q^r)),
                                          L[-1], var), n)], nonnegint)));
  normal(var^deg_0 * eval(gcd(normal(mul(eval(L[1], var = q^i*var*q^r*q^H),
                                         i = 0..H)),
                              normal(mul(eval(L[-1], var = var*q^(H-i)),
                                         i = 0..H))),
                          var = var*q^(-H-r)));

end proc;
