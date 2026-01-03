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

## LOE_sys_rat_lexdeg.mm
## Copyright (c) 2009, INRIA. All rights reserved.
## Author: Lucien Pech <lucien.pech@ens.fr>.




## uncouple_system_lexdeg

## INPUT

## OUTPUT

uncouple_system_lexdeg := proc(Alg::OreAlgebra, var::name, A, R)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local dim, dvar, r, i, j, G, L_array, var_rhs_array, const_rhs_array, val_r,
    formal_param, g, k, comm_var, Alg_, monomial_order, GB, n, nf, sys, c, sol,
    params_sol, new_eq, rhs_, const_rhs, var_rhs, L, comm_denom, new_GB, gb,
    list_var;

  dim := op(1, A)[1];
  dvar := Alg["right_of_left", var];

  G := [seq(dvar*g[i]-add(A[i,j]*g[j], j = 1..dim)-`if`(R[i]<>0,r[dim+1, i],0),
            i = 1..dim)];

  L_array := Array(1..dim);
  var_rhs_array := Array(1..dim);
  const_rhs_array := Array(1..dim);

  val_r[dim+1] := [seq(R[i], i = 1..dim)];

  for j from dim to 1 by -1 do

    userinfo(2, :-CreativeTelescoping, "j = ", j);

    list_var := [dvar, var, `if`(Alg["type_of_left", var] = 'dual_qdilat',
                                 Alg["q_of_right", dvar], NULL)];
    formal_param := [seq(g[i], i = 1..dim),
                     seq(r[j+1,i], i = 1..dim),
                     op(indets(R))];
    comm_var := Alg["comm_indets"] minus {var} union {op(formal_param)};

    Alg_ := Ore_algebra:-skew_algebra(Alg["type_of_left", var] = list_var,
                                      comm = comm_var, polynom = formal_param);

    monomial_order := Groebner:-MonomialOrder(Alg_,
              tdeg(dvar, op(formal_param)), [op(formal_param)]);

    GB := Groebner:-Basis(G, monomial_order);


    for n from 0 to j do
      nf[n] := collect(Groebner:-NormalForm(dvar^n * g[j], GB, monomial_order),
                       [seq(g[i], i = 0..j)], normal)
    end do;

    sys := [seq(coeff(add(c[k]*nf[k], k = 0..j), g[i]), i = 1..j)];
    sol := SolveTools:-Linear(sys, [seq(c[i], i = 0..j)]);


    # Parameters of the solution. We set the first parameter to 1, and
    # the following to 0.

    params_sol := select(has, sol, [seq(c[i]=c[i], i = 0..j)]);
    new_eq := rhs(params_sol[1]) = 1,
              seq(rhs(i) = 0, i in params_sol minus {params_sol[1]});
    sol := eval(sol minus params_sol, [new_eq]) union {new_eq};

    rhs_ := eval(add(c[i] * eval(nf[i], [seq(g[k] = 0, k = 1..j)]),
                     i = 0..j), sol);
    rhs_ := add(
      Ore_algebra:-skew_product(coeff(rhs_, r[j+1,k]), val_r[j+1][k], Alg_),
      k = 1..nops(val_r[j+1])) +
      eval(rhs_, [seq(r[j+1,k] = 0, k = 1..nops(val_r[j+1]))]);

    const_rhs := normal(eval(rhs_, [seq(R[i] = 0, i = 1..dim)]));
    var_rhs := normal(eval(rhs_, [seq(g[i] = 0, i = 1..dim)]));
    L := [seq(normal(eval(c[i], sol)), i = 0..j)]:

    ASSERT({op(L)} <> {0});
    while L[-1] = 0 do
      L := L[1..nops(L)-1]
    end do;

    comm_denom := lcm(op(denom(L)), denom(const_rhs), denom(var_rhs));
    const_rhs := normal(comm_denom * const_rhs);
    L := map(normal@`*`, L, comm_denom);
    var_rhs := normal(var_rhs * comm_denom);

    L_array[dim-j+1] := L;
    var_rhs_array[dim-j+1] := var_rhs;
    const_rhs_array[dim-j+1] := const_rhs;

    i := 1;
    new_GB := NULL;
    val_r[j] := [0$nops(GB)];

    for gb in GB do

      val_r[j][i] := add(Ore_algebra:-skew_product(
        coeff(eval(gb, [seq(g[k] = 0, k = 1..j-1)]), r[j+1,k]),
        val_r[j+1][k], Alg_), k = 1..nops(val_r[j+1])) +
        eval(gb, [seq(g[k] = 0, k = 1..j-1),
                  seq(r[j+1,k]=0, k = 1..nops(val_r[j+1]))]);

      new_GB := new_GB,
       eval(gb, [g[j] = 0, seq(r[j+1,k] = 0, k = 0..nops(val_r[j+1]))])+
        `if`(val_r[j][i] <> 0, r[j,i], 0);

      i := i+1
    end do;

    G := [new_GB]

  end do;

  [[seq(L_array[i], i=1..dim)], [seq(var_rhs_array[i], i=1..dim)],
   [seq(const_rhs_array[i], i = 1..dim)], g]

end proc;




## LOE_sys_rat_lexdeg


LOE_sys_rat_lexdeg := proc(Alg::OreAlgebra, var, uncoupled_system, Pf, eta,
                           indices_eta, formal_indet, formal_rhs,
                           {constrained_eta := {}}, $)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local dim, dvar, constraints, r, u, i, j, g, L, var_rhs, id, const_rhs, k,
    phi, comm_denom, sol_rat_unknowns, local_formal_indet, old_form_param,
    sol_rat, res, nb_indets, ind;

  dim := nops(uncoupled_system[1]);
  dvar := Alg["right_of_left", var];
  constraints := constrained_eta;

  r := map(u -> normal(add(Ore_algebra:-applyopr(coeff(u, formal_rhs[j]),
                                                 Pf[j], Alg),
                           j = 1..dim)), uncoupled_system[2]);

  g := uncoupled_system[4];

  for j to dim do

    userinfo(2, :-CreativeTelescoping, "j = ", j);

    L := uncoupled_system[1][j];
    var_rhs := [seq(normal(coeff(r[j], eta[op(id)])), id in indices_eta)];
    const_rhs := normal(add(Ore_algebra:-applyopr(coeff(uncoupled_system[3][j],
                                                        g[k]), phi[k], Alg),
                            k = dim-j+2..dim));


    # Clear denominators in var_rhs, and const_rhs.

    comm_denom := lcm(denom(const_rhs), op(denom(var_rhs)));

    const_rhs := normal(comm_denom * const_rhs);
    L := map(normal@`*`, L, comm_denom);
    var_rhs := map(normal@`*`, var_rhs, comm_denom);


    # Rename indeterminates

    sol_rat_unknowns := LOE_rat(Alg, L, var, const_rhs, var_rhs, constraints,
                                local_formal_indet, eta, indices_eta);
    old_form_param := [op(select(x -> op(0, x) = local_formal_indet
                                 and nops(x) = 1, indets(sol_rat_unknowns)))];
    sol_rat := subs([seq(old_form_param[i] = local_formal_indet[j,i],
                         i = 1..nops(old_form_param))], sol_rat_unknowns);

    phi[dim+1-j] := normal(sol_rat[1]);
    constraints := sol_rat[2];

  end do;     ## for j to dim


  res := [eval([seq(phi[i], i = 1..dim)], constraints), constraints];
  rename_indets(res, local_formal_indet, formal_indet)

end proc;
