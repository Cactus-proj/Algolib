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

## LOE_poly.mm
## Copyright (c) 2009, INRIA. All rights reserved.
## Author: Lucien Pech <lucien.pech@ens.fr>.




## LOE_poly


LOE_poly := proc(Alg::OreAlgebra,
                 L::list, var::name, const_rhs := 0, var_rhs::list := [],
                 prev_constraints::set := {}, formal_indet::name := 'p',
                 eta::name := 'Eta',
                 indices_eta::list(list) := [seq([i], i = 0..nops(var_rhs)-1)],
                 $)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local d, dvar, order_rhs, f, k, N, i, P, equ, sol, my_indet, indets_rhs;

  ASSERT(L[-1] <> 0, "L[-1] <> 0");
  ASSERT(Alg["type_of_left", var] = 'diff' or L[1] <> 0, "L[1] <> 0");

  d := nops(L)-1;
  dvar := Alg["right_of_left", var];
  order_rhs := nops(var_rhs)-1;
  f := normal(const_rhs + add(eta[op(indices_eta[k+1])] * var_rhs[k+1],
                              k = 0..order_rhs));

  N := degree_upper_bound(Alg, L, var, degree(f, var));
  ASSERT(N <= 10000, "Degree bigger than 10000 (it's probably a bug)");

  indets_rhs := map(rhs, select(u -> op(0, rhs(u)) = formal_indet,
                                prev_constraints))
    union select(u -> op(0, u) = formal_indet, indets(const_rhs));

  P := add(my_indet[k] * var^k, k = 0..N);

  equ := collect(expand(add(L[k+1]*Ore_algebra:-applyopr(dvar^k, P, Alg),
                            k = 0..d) - f), var);

  sol := SolveTools:-Linear({coeffs(equ, var)} union prev_constraints,
                            [seq(my_indet[i], i = 0..N),
                             seq(eta[op(indices_eta[k+1])], k = 0..order_rhs),
                             op(indets_rhs)]);

  if sol = NULL then # can it happen?
    return []
  end if;

  rename_indets([eval(P, sol), select(u -> op(0, lhs(u)) in {eta, formal_indet},
                                      sol)],
                my_indet, formal_indet);

end proc;




## degree_upper_bound
## Compute a upper bound for the degree of all possible polynomial solutions of
## the equation Ly=f

degree_upper_bound := proc(Alg::OreAlgebra, L::list, var::name, degree_f, $)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local type_var, dvar;

  type_var := Alg["type_of_left", var];
  dvar := Alg["right_of_left", var];

  if type_var = 'shift' then
    degree_upper_bound_shift(L, var, degree_f);
  elif type_var = 'diff' then
    degree_upper_bound_diff(L, var, degree_f);
  elif type_var = 'qdilat' then
    degree_upper_bound_qdilat(L, var, degree_f, Alg["q_of_right", dvar]);
  else
    error "Operator type has to be either shift, diff or qdilat.";
  end if

end proc;




## degree_upper_bound_shift


degree_upper_bound_shift := proc(L::list, var::name, degree_f, $)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local d, order_rhs, b, beta, alpha, i, j, k;

  d := nops(L) - 1;
  b := [seq(normal(add(binomial(i, k) * L[i+1], i=k..d)), k = 0..d)];
  beta := max(op(map(degree, b, var)-[i$i=0..d]));
  alpha := max(op(select(type,map2(op,1,roots(add(coeff(b[k+1], var, k+beta)
                                                  * mul(var-j, j = 0..k-1),
                                                  k = 0..d), var)), integer)));

  max(alpha, degree_f - beta);

end proc;




## degree_upper_bound_diff


degree_upper_bound_diff := proc(L::list, var::name, degree_f, $)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local d, m, alpha, Lambda;

  d := nops(L)-1;
  m := max(op(map(degree, L, var) - [$0..d]));
  Lambda := add(coeff(L[k+1], var, m+k) * mul(var-j, j = 0..k-1), k = 0..d);
  alpha := max(op(select(type, map2(op, 1, roots(Lambda, var)), integer)));

  max(alpha, degree_f - m)

end proc;




## degree_upper_bound_qdilat


degree_upper_bound_qdilat := proc(L::list, var::name, degree_f,
                                  _q::name := 'q', $)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local d, P, X, i;

  d := max(op(map(degree, L, var)));
  P := add(coeff(normal(expand(L[i+1])), var, d) * X^i, i = 0..nops(L)-1);

  max(select(type,[solve(eval(P, X = _q^n), n)], integer), degree_f);

end proc;
