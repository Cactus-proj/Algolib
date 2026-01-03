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

## skew_creative_telescoping.mm
## Copyright (c) 2010, INRIA. All rights reserved.
## Author: Lucien Pech <lucien.pech@ens.fr>.




$include <Mgfun/CreativeTelescoping/LOE_poly.mm>
$include <Mgfun/CreativeTelescoping/LOE_rat.mm>
$include <Mgfun/CreativeTelescoping/LOE_sys_rat_AZ.mm>
$include <Mgfun/CreativeTelescoping/LOE_sys_rat_lexdeg.mm>
#$include <Mgfun/CreativeTelescoping/LOE_sys_rat_custom.mm>
$include <Mgfun/CreativeTelescoping/LOE_sys_rat.mm>
$include <Mgfun/CreativeTelescoping/zeilberger.mm>




## skew_poly_creative_telescoping

## INPUT
## - f: D-finite expression we want to sum, or LFOS(sys), with sys a list or a
##      a set of polynomial in Alg
## - Alg: Ore Algebra.

## OUTPUT: list of lists of two operators [P, Q].


skew_poly_creative_telescoping := proc(f,
                                _Alg::OreAlgebra,
                                _var,
#                                _var::{name, `^`(name, name)},
                                _order_dx,
                                {formal_indet := 'c',
                                 min_order := 1, max_order := infinity,
                                 uncoupling_method::{identical(lexdeg),
                                                     identical(AZ)}
                                   := lexdeg,
                                 double_order := false,
                                 target_dimension::nonnegint := 0,
                                 constrained_P := 0
                                }, $)

  option
    `Copyright (c) 2010, INRIA. All rights reserved.`;

  local conv_var, type_var, vars, dvar, typed_var, v, dv, typed_v, sys_mgfun,
    F, sys, d, i, short_mono_order, Mord, GB, J, B, rev, dim, Dv, Dvar, z,
    dvar_only_eq, deg_var, non_zero_rhs, type_v, qshift_vars, qshift_dvar, qs,
    new_names, new_dx, Alg, var, sys_output, Alg_output, Mord_output, GB_output,
    g, j, e, u, eis, gb, order_dx;

  conv_var := proc(var)
    if _Alg["type_of_left", var] = 'qshift' then
      op(2, var)::_Alg["type_of_left", var];
    else
      var::_Alg["type_of_left", var];
    end if;
  end proc;

  dvar := _Alg["right_of_left", _var];
  type_var := _Alg["type_of_left", _var];
  v := [op(_Alg["left_indets"] minus {_var})];
  vars := [op(v), _var];
  dv := map(u -> _Alg["right_of_left", u], v);
  type_v := map(u -> _Alg["type_of_left", u], v);
  qs := [];

  qshift_vars := select(u -> _Alg["type_of_left", u] = 'qshift', vars);

  if type(f, 'function') and op(0, f)='LFOS' then
    sys := op(1, f);
  else
    typed_var := conv_var(_var);
    typed_v := map(conv_var, v);
    sys_mgfun := dfinite_expr_to_sys(f, F(typed_var, op(typed_v)));
    sys := eval(map(my_conv, sys_mgfun, d),
                [seq(d[i] = _Alg["right_of_left", i], i in vars),
                seq(d[op(2, i)] = _Alg["right_of_left", i], i in qshift_vars)]);
  end if;

  if qshift_vars <> [] then
    qshift_dvar := map(u -> _Alg["right_of_left", u], qshift_vars);
    qs := map(u -> _Alg["q_of_right", u], qshift_dvar);
    new_names := [seq(cat(`x`,
                 op(2, qshift_vars[i]), `__`, i), i = 1..nops(qshift_vars))];
    new_dx := [seq(cat(`d`, new_names[i]), i = 1..nops(qshift_vars))];
    order_dx := eval(_order_dx, [seq(qshift_dvar[i] = new_dx[i],
                                     i = 1..nops(qshift_vars))]);

    sys := eval(sys, [seq(qshift_vars[i] = new_names[i],
                         i = 1..nops(qshift_vars)),
                      seq(qshift_dvar[i] = new_dx[i],
                         i = 1..nops(qshift_vars))]);
    Alg := Ore_algebra:-skew_algebra(
      seq(qdilat = [new_dx[i], new_names[i], qs[i]], i = 1..nops(qshift_vars)),
      op(select(u -> op(1, u) <> 'qshift', _Alg["type_struct"])),
      comm = _Alg["comm_indets"] minus {op(qs), op(v), _var});

    if _var in qshift_vars then
      for i while qshift_vars[i] <> _var do end do;
      dvar := new_dx[i];
      var := new_names[i];
    end if;

  else
    Alg := _Alg;
    var := _var;
    order_dx := _order_dx;
  end if;

  v := [op(Alg["left_indets"] minus {var})];
  type_v := map(u -> Alg["type_of_left", u], v);
  dv := map(u -> Alg["right_of_left", u], v);

  short_mono_order := tdeg(op(Alg["non_alg_poly_indets"] minus {op(order_dx)}),
                           op(order_dx));
  Mord := Groebner:-MonomialOrder(Alg, short_mono_order);
  GB := Groebner:-Basis(sys, Mord);

  J := Groebner:-LeadingMonomial(GB, Mord);
  B, rev := Groebner:-NormalSet(J, short_mono_order);
  dim := nops(B);


  # Heuristic to make most cases faster: we only consider places in the rhs
  # where there will possibly be some eta.

  non_zero_rhs := Vector([true$dim]);

  if nops(v) = 1 then
    dvar_only_eq := remove(has, GB, dvar);
    if dvar_only_eq <> [] then
      non_zero_rhs := Vector([false$dim]);
      deg_var := degree(Groebner:-LeadingMonomial(op(dvar_only_eq), Mord));
      for i from 0 to deg_var-1 do
        non_zero_rhs[rev[dv[1]^i]] := true;
      end do;
    end if;
  end if;


  userinfo(4, :-CreativeTelescoping, "PROFILE - DIMENSION", dim);

  Dv := map2(NonCommutativeMultiplicationMatrix, Alg, dv, B, rev, GB, Mord);
  Dvar := NonCommutativeMultiplicationMatrix(Alg, dvar, B, rev, GB, Mord);

  z := Zeilberger(Alg, var, dim, Dvar, v, Dv,
                  Vector([1, 0$(dim-1)]),
                  formal_indet,
                  ':-min_order' = min_order,
                  ':-max_order' = max_order,
                  ':-uncoupling_method' = uncoupling_method,
                  ':-double_order' = double_order,
                  ':-non_zero_rhs' = non_zero_rhs,
                  ':-target_dimension' = target_dimension,
                  ':-constrained_P' = constrained_P);

  eis := seq(e[i], i = 0..nops(z));
  sys_output := {seq(z[i][1]*e[0]-e[i], i = 1..nops(z))};

  Alg_output := Ore_algebra:-skew_algebra(
    seq(type_v[i] =
        [dv[i], v[i], `if`(type_v[i]='qdilat', Alg["q_of_right", dv[i]], NULL)],
        i = 1..nops(v)),
      comm = {eis} union Alg["comm_indets"] minus {op(qs), op(v), var},
      polynom = [eis]);

  Mord_output :=
    Groebner:-MonomialOrder(Alg_output,
                            tdeg(eis, op(order_dx),
                                 op({op(dv)} minus {op(order_dx)})),
                            [eis]);

  GB_output := Groebner:-Basis(sys_output, Mord_output);

  g := [seq(add(z[j][2][i]*B[i], i = 1..dim), j = 1..nops(z))];
  GB_output := select(u -> coeff(u, e[0]) <> 0, GB_output);

  z := [seq([coeff(gb, e[0]),
             -add(Ore_algebra:-skew_product(coeff(gb, e[j]), g[j], Alg),
                  j = 1..nops(z))],
            gb in GB_output)];

  if qshift_vars <> [] then
    z := eval(z, [seq(new_names[i] = qshift_vars[i], i = 1..nops(qshift_vars)),
      seq(new_dx[i] = qshift_dvar[i], i = 1..nops(qshift_vars))]);
  end if;

  z

end proc;




## my_conv
## Convert a maple differential/shift expression in F to an Ore polynomial

## INPUT
## - expr: expression. differential/shift expression in F.
## - d: name.

## OUTPUT: a formal Ore polynomial in d[var].


my_conv := proc(expr, d::name)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local i;

  if type(expr, function) and op(0, expr) <> RootOf then

    if op(0, expr) = diff then
      d[op(2, expr)] * procname(op(1, expr), d);
    elif op([0$3], expr) = D then
      d[op(op([0$2, 1], expr), expr)] * procname(op([0, 1], expr)(op(expr)), d)
    else
      mul(d[op(indets(i))]^(eval(i-op(indets(i)))), i in op(expr));
    end if

  else

    if type(expr, atomic) then
      expr
    else
      map(procname, expr, d)
    end if

  end if

end proc;




## NonCommutativeMultiplicationMatrix
## A non-commutative equivalent to Groebner[MultiplicationMatrix]


NonCommutativeMultiplicationMatrix := proc(Alg::OreAlgebra, x, B, rev::table,
                                           GB, Mord, $)

  local c, i, j, n, t, xw, X, s;

  n := nops(B);
  X := Matrix(1..n, 1..n);
  xw := map2(Ore_algebra:-skew_product, x, B, Alg);
  xw := expand(Groebner:-NormalForm(xw, GB, Mord));

  for i to n do
    c := map(normal@expand, [coeffs(xw[i], B, 't')]);
    t := [t];
    for j to nops(t) do
      X[rev[t[j]], i] := c[j]
    end do
  end do;

  X

end proc;




## rename_indets


rename_indets := proc(expr, old_indet_name::{name, set},
                      new_indet_name::name)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local old_indet_name_set, old_indets, i;

  old_indet_name_set := {op(old_indet_name)};
  old_indets := [op(select(x -> op(0, x) in old_indet_name_set, indets(expr)))];
  eval(expr, [seq(old_indets[i] = new_indet_name[i], i = 1..nops(old_indets))]);

end proc;
