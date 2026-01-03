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

## creative_telescoping.mm
## Copyright (c) 2010, INRIA. All rights reserved.
## Author: Lucien Pech <lucien.pech@ens.fr>.



## creative_telescoping


creative_telescoping := proc(expr,
                             typed_n::{'list'(`::`), 'set'(`::`), `::`},
                             typed_k::{'list'(`::`), `::`},
                             opt::list := [], $)

  option
    `Copyright (c) 2010, INRIA. All rights reserved.`;

  local create_comm, typed_var_int, var_int, dvar_int, ore_int, typed_v, v, dv,
    ore_v, vars, indets_, Alg, expr_arg, ct, A, var, res, R, M, P, Q,
    Mord, d, r, i, j, k, co, rem_;

  create_comm := proc(typed_v, dv)
    local v, type_, t;
    v,type_ := op(typed_v);
    if type_ = 'qshift' then
      t := q^v
    elif type_ = 'qdilat' then
      t := v,q
    else
      t := v;
    end if;
    type_ = [dv, t];
  end proc;

  typed_var_int := `if`(type(typed_k,`::`), [typed_k], typed_k);
  var_int := map2(op, 1, typed_var_int);
  dvar_int := [seq(cat(`_d`, var_int[j], `__`, j), j = 1..nops(var_int))];
  ore_int := [seq(create_comm(typed_var_int[i], dvar_int[i]),
                 i = 1..nops(var_int))];

  typed_v := `if`(type(typed_n,{'list','set'}), [op(typed_n)], [typed_n]);
  v := map2(op, 1, typed_v);
  dv := [seq(cat(`_d`, v[i], `__`, i+nops(var_int)), i = 1..nops(v))];
  ore_v := [seq(create_comm(typed_v[i], dv[i]), i = 1..nops(v))];

  vars := [op(var_int), op(v)];
  indets_ := select(type, indets(expr), name) minus {op(vars)};

  Alg := Ore_algebra:-skew_algebra(op(ore_int), op(ore_v), comm = indets_);

  if type(expr,'function') and op(0, expr)='LFSol' then
    expr_arg := LFOS(eval(map(my_conv, op(1, expr), d),
                     [seq(d[i] = Alg["right_of_left", i], i in vars)]));
  else
    expr_arg := expr
  end if;

  ct := table();
  for i from nops(var_int) to 1 by -1 do
    A := Ore_algebra:-skew_algebra(op(ore_int[1..i]), op(ore_v),
      comm = indets_);

    var := var_int[i];
    if op(2, typed_var_int[i]) = 'qshift' then
      var := q^var
    end if;

    ct[i] := skew_poly_creative_telescoping(
      `if`(i = nops(var_int), expr_arg, LFOS(map2(op, 1, ct[i+1]))), A, var,
       ListTools:-Reverse([op(dv), op(dvar_int[1..i-1])]),
       `if`(i = 1, op(opt), NULL));

  end do;

  userinfo(1, :-CreativeTelescoping, "Start to reconstruct rhs operators.");

  M := nops(ct[1]);
  res := Array(1..M, 1..nops(var_int)+1);
  R := table();

  for i to M do
    P,Q := op(ct[1][i]);
    P := primpart(P, [op(dvar_int), op(dv), op(var_int)], 'co') ;
    if co <> 0 then Q := Q/co end if;
    res[i,1] := P;
    res[i,2] := Q;
    R[i] := P-Ore_algebra:-skew_product(
        `if`(op(2, typed_var_int[1]) = 'diff', dvar_int[1], dvar_int[1]-1),
        Q, Alg)
  end do;

  if nops(var_int) > 1 then
    # TODO: doesn't work for multi-sums with q-calculus
    Mord :=
      Groebner:-MonomialOrder(Alg, tdeg(op(ListTools:-Reverse([op(dv),
                                                               op(dvar_int)]))))
  end if;

  for i to nops(var_int)-1 do
    for j to M do
      rem_:=`Mgfun/NOT_BUGGED_Reduce`(R[j],
                                      [seq(ct[i+1][k][1], k=1..nops(ct[i+1]))],
                                      Mord, 'r', 'a');
      ASSERT(rem_ = 0, "r = 0");
      Q := normal(add(Ore_algebra:-skew_product(a[k], ct[i+1][k][2], Alg),
                      k = 1..nops(ct[i+1]))/r);
      res[j,i+2] := Q;
      if i < nops(var_int)-1 then
        R[j] := R[j]-Ore_algebra:-skew_product(
          `if`(op(2,typed_var_int[i+1])='diff', dvar_int[i+1], dvar_int[i+1]-1),
          Q, Alg)
      end if
    end do
  end do;

  [seq([Ore_algebra:-applyopr(res[i,1], _F(op(v)), Alg),
        seq(Ore_algebra:-applyopr(res[i,1+j], _f(op(v), op(var_int)), Alg),
            j = 1..nops(var_int))],
       i = 1..M)]

end proc;
