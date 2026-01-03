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

## LOE_sys_rat_AZ.mm


uncouple_system_AZ := proc(Alg, var, A_, r_)

  local dvar, A, r, n, T, perm, I_, sigma, delta, i, j, perm_i, tmp, b, k, c,
     new_ri, s, d, m, t, L;

  dvar := Alg[("right_of_left", var)];
  A := Matrix(A_);
  r := Vector(r_);
  n := op(1, A)[1];
  T := Matrix(n, n);
  T[1,1] := 1;
  perm := Vector([$1..n]);
  I_ := 1;

 if Alg[("type_of_left", var)] = diff then
      sigma := proc(a)
         a
      end proc;
      delta := proc(a)
         diff(a, var)
      end proc;
   elif Alg[("type_of_left", var)] = shift then
      sigma := proc(a)
         eval(a, var = var + 1)
      end proc;
      delta := proc(a)
         0
      end proc
   elif Alg[("type_of_left", var)] = dual_shift then
      sigma := proc(a)
         eval(a, var = var - 1)
      end proc;
      delta := proc(a)
         0
      end proc
   else
      error "Neither shift nor diff algebra.";
   end if;

   for i to n-1 do

      userinfo(3, Zeilberger, "i = ", i);

      for j from i+1 to n while A[i, j] = 0 do end do;

      if j <= n then
         if j <> i+1 then
            perm_i := perm[i+1];
            perm[i+1] := perm[j];
            perm[j] := perm_i;
            LinearAlgebra:-RowOperation(A, [i+1, j], inplace = true);
            LinearAlgebra:-ColumnOperation(A, [i+1, j], inplace = true);
            tmp := r[i+1]; r[i+1]:=r[j]; r[j] := tmp;
         end if;

         for j from i+1 to n do
            T[i+1,j] := A[i,j];
         end do;

         b := Vector(n);
         for j from i+1 to n do
            b[j] := normal(expand(A[i,j] / A[i,i+1]));
         end do;

         for k from i+1 to n do
            for j from i+2 to n do
               A[k,j] := normal(expand(A[k,j] - A[k,i+1] * b[j]))
            end do;
            A[k,i+1] := normal(expand(A[k,i+1] / A[i,i+1]))
         end do;

         c := Vector(n);
         c[i+1] := delta(A[i,i+1]) / A[i,i+1];
         for j from i+2 to n do
            c[j] := normal(expand(-delta(A[i,i+1]) * b[j]));
         end do;


         new_ri := 0;
         for k from i+1 to n do
            for j to n do
               c[j] := normal(expand(c[j] + sigma(A[i,k]) * A[k, j]))
            end do;
            new_ri := new_ri + sigma(A[i,k]) * r[k];
         end do;
         r[i+1] := new_ri;

         for j to n do
            A[i+1,j] := c[j]
         end do;

         A[i,i+1] := 1;
         for j from i+2 to n do
            A[i,j] := 0;
         end do;

      else           ## j = n+1
         I_ := I_, i+1;
         T[i+1,i+1] := 1;
      end if;

   end do;           ## for i to n-1 do

  s := nops(I_);
  I_ := [I_, n+1];

  d := Array(1..n, 1..n, 0..n);
  for i to n do
    for j to n do
      d[i,j,0] := A[i,j]
    end do
  end do;

  for k to s do
    for m to I_[k+1]-I_[k]-1 do
      for t to I_[k] do
        d[I_[k]+m,t,0] := normal(expand(delta(d[I_[k]+m-1,t,0]) + d[I_[k]+m,t,0]
                          - d[I_[k]+m,I_[k]+m,0] * d[I_[k]+m-1,t,0]));
        for j to m-1 do
          d[I_[k]+m,t,j] := normal(expand(sigma(d[I_[k]+m-1,t,j-1]) + delta(d[I_[k]+m-1,t,j])
                            + d[I_[k]+m,t,j]
                            - d[I_[k]+m,I_[k]+m,0] * d[I_[k]+m-1,t,j]))
        end do;
        d[I_[k]+m,t,m] := normal(expand(sigma(d[I_[k]+m-1,t,m-1])))
      end do;

      d[I_[k]+m,I_[k],m] := normal(expand(d[I_[k]+m,I_[k],m] + d[I_[k]+m,I_[k]+m,0]));
      r[I_[k]+m] := normal(r[I_[k]+m] + dvar*r[I_[k]+m-1]
                    - d[I_[k]+m,I_[k]+m,0] * r[I_[k]+m-1]);
      d[I_[k]+m,I_[k]+m,0] := 0;

      for i from m+1 to I_[k+1]-I_[k]-1 do
        for t from 1 to I_[k] do
          for j from 0 to m-1 do
            d[I_[k]+i,t,j] := normal(expand(d[I_[k]+i,t,j]
                              - d[I_[k]+i,I_[k]+m,0] * d[I_[k]+m-1,t,j]))
          end do
        end do;
        d[I_[k]+1,I_[k],m] := d[I_[k]+i,I_[k]+m,0];
        r[I_[k]+i] := normal(expand(r[I_[k]+i] - d[I_[k]+i,I_[k]+m,0] * r[I_[k]+m-1]));
        d[I_[k]+i,I_[k]+m,0] := 0
      end do
    end do
  end do;

   ASSERT(s = 1, "WTF ?");

  k := 1;
  L := [seq(-d[I_[k+1]-1,1,j], j=0..I_[k+1]-I_[k]-1), 1];

#lprint("L = ", L);

#  comm_denom := lcm(op(denom(L)), seq(denom(r[i]), i = 1..n));
#
#  L := map(u -> collect(normal(u * comm_denom), var), L);
#  r := map(u -> normal(u * comm_denom), r);

  # Remove the leading 0s in L.

  ASSERT({op(L)} <> {0});
  while L[-1] = 0 do
    L := L[1..nops(L)-1]
  end do;

  [L, r, A, T, perm];

end proc;




LOE_sys_rat_AZ := proc(Alg::OreAlgebra, var::name, uncoupled_system, Pf, eta,
                       indices_eta, formal_indet, formal_rhs, $)

  local L, r, A, T, perm, n, I_, dvar, constraints, i, j, k, var_rhs,
    comm_denom, rr, sol_rat_unknowns, max_unknowns, sol_rat, z, phi, inv_perm,
    res, nb_indets, ind;

  L, r, A, T, perm := op(uncoupled_system);

  n := op(1, A)[1];
  I_ := [1, n+1];
  dvar := Alg[("right_of_left", var)];

  constraints := {};

  r := add(map(Ore_algebra:-applyopr,
               eval(r, [formal_rhs[j] = 1,
                        seq(formal_rhs[i] = 0, i in {$1..n} minus {j})]),
               Pf[j], Alg), j = 1..n);
  r := eval(r, [seq(formal_rhs[i] = Pf[i], i = 1..n)]);


  k := 1;

  var_rhs := [seq(coeff(r[I_[k+1]-1], eta[op(rr)]), rr in indices_eta)];


  # Clear denominators in L, e, and const_rhs.

  comm_denom := lcm(op(denom(normal(L))), op(denom(var_rhs)));

  L := map(u -> collect(normal(u * comm_denom), var), L);
  var_rhs := map(u -> normal(u * comm_denom), var_rhs);

  sol_rat_unknowns := LOE_rat(Alg, L, var, 0, var_rhs, constraints, local_formal_indet, eta, indices_eta);

  max_unknowns := degree(numer(normal(sol_rat_unknowns[1])), var);

  sol_rat := subs([seq(local_formal_indet[i] = local_formal_indet[j,i], i = 0..max_unknowns),
                          seq(eta[i] = eta[op(indices_eta[i+1])], i = 0..nops(var_rhs)-1)],
                         sol_rat_unknowns);

  z[I_[k]] := sol_rat[1];
  constraints := sol_rat[2];

  for i from I_[k]+1 to I_[k+1]-1 do
    z[i] := Ore_algebra:-applyopr(dvar, z[i-1], Alg) - add(A[i-1,j]*z[j], j = 1..i-1) - eval(r[i-1], constraints);
  end do;


  # Solve the linear equation T*phi = z.

  phi := Vector(n);
  for i from n to 1 by -1 do
    phi[i] := normal((1 / T[i,i]) * (z[i] - add(T[i,j]*phi[j], j = i+1..n)))
  end do;

  inv_perm := Vector(n);
  for i to n do
    inv_perm[perm[i]] := i
  end do;

  res := [[seq(phi[inv_perm[i]], i = 1..n)], constraints];

  nb_indets := 1;
  for ind in indets(res) do
    if op(0, ind) = local_formal_indet then
      res := eval(res, ind = formal_indet[nb_indets]);
      nb_indets := nb_indets+1
    end if
  end do;

  res

end proc;
