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

## zeilberger.mm
## Copyright (c) 2009, INRIA. All rights reserved.
## Author: Lucien Pech <lucien.pech@ens.fr>.



## Zeilberger

## INPUT

## - Alg: skew algebra.
## - var: name. The integrand variable.
## - dim: positive integer. Dimension of the vector space closed under
## derivation.
## - Dvar: dim*dim matrix. Matrix of the derivation with respect to var in the
##         basis.
## - v: list.
## - Dv: list of matrices.
## - f_coords: vector of dimension dim, with coefficients in K(var1, var2).
##             Coordinates of f in the basis.
## - formal_indet: name.
## - min_order: positive integer.
## - max_order: positive integer or infinity.
## - uncoupling method: lexdeg or AZ.
## - double_order: boolean.


## OUTPUT: [P, [g_1, ... , g_dim]]

## An operator P and a function g such that Pf = dv2 g.
## P = add(p[i]*dv1^i, i = 0..d), with p[i] a polynomial in v1.
## [g_1, ... , g_dim] the coordinates of g in the basis.


Zeilberger := proc(Alg::OreAlgebra,
                   var::name, dim::posint,
                   Dvar, v, Dv, f_coords,
                   formal_indet,
                   {min_order::nonnegint := 1,
                    max_order := infinity,
                    uncoupling_method::{identical(lexdeg), identical(AZ)}
                      := lexdeg,
                    double_order::boolean := false,
                    non_zero_rhs::Vector(boolean) := Vector([true$dim]),
                    target_dimension::nonnegint := 0,
                    constrained_P := 0
                   } ,$)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local dv, nb_v, dvar, var_type, formal_rhs_vect, formal_rhs, uncoupled_system,
    dual_alg, d, indices_eta, res, i, converted_res, Mord, constrained_P_op,
    d_, coeffs_P, monoms_P, eta;

  dv := map(u -> Alg["right_of_left", u], v);
  nb_v := nops(v);
  dvar := Alg["right_of_left", var];
  var_type := Alg["type_of_left", var];
  Mord := Groebner:-MonomialOrder(Alg, tdeg(op(dv),dvar));
  formal_rhs_vect := Vector(dim, i -> `if`(non_zero_rhs[i], formal_rhs[i], 0));

  userinfo(1, :-CreativeTelescoping, "Start uncoupling system.");

  if var_type = 'diff' then
    uncoupled_system :=
      uncouple_system(Alg, var, -Dvar, formal_rhs_vect, uncoupling_method);
  elif var_type in {'shift', 'qdilat'} then
    dual_alg := Ore_algebra:-dual_algebra(Alg, {dvar});
    uncoupled_system :=
      uncouple_system(dual_alg, var,
                      map2(normal@Ore_algebra:-applyopr, dvar, Dvar, dual_alg),
                      formal_rhs_vect, uncoupling_method);
  else
    error "Operator type has to be either shift, qdilat or diff.";
  end if;

  if constrained_P <> 0 then

    constrained_P_op :=
      eval(my_conv(constrained_P, d_), [seq(d_[v[i]] = dv[i], i = 1..nb_v)]);
#### Wrong code that relied on coeffs returning coefficients in the same order as in compute_indices_eta
#### Fixed BS. June 2013.
    coeffs_P := [coeffs(constrained_P_op, dv, 'monoms_P')];
    d := degree(constrained_P_op, dv);

    indices_eta := compute_indices_eta(d, nb_v);
    #res := test_order(Alg, d, dim, var, dv, Dv, f_coords,
    #                  uncoupled_system, eta, indices_eta, local_formal_indet,
    #                  formal_rhs, uncoupling_method,
    #                  constrained_eta={seq(eta[op(indices_eta[i])]=coeffs_P[i],
    #                                       i = 1..nops(constrained_P_op))});
	
    res := test_order(Alg, d, dim, var, dv, Dv, f_coords,
                      uncoupled_system, eta, indices_eta, local_formal_indet,
                      formal_rhs, uncoupling_method,
                      constrained_eta={seq(eta[op(map2(degree,monoms_P[i],dv))]
						=coeffs_P[i],i=1..nops(coeffs_P))});
#### End fix.
    return convert_res(res, v, dv, dim, local_formal_indet, formal_indet, eta,
                       indices_eta);

  end if;

  if not double_order or nb_v > 1 then

    for d from min_order to max_order do

      indices_eta := compute_indices_eta(d, nb_v);

      res := test_order(Alg, d, dim, var, dv, Dv, f_coords, uncoupled_system,
                        eta, indices_eta, local_formal_indet, formal_rhs,
                        uncoupling_method);

      if not trivial_res(res, eta, indices_eta) then
        converted_res := convert_res(res, v, dv, dim, local_formal_indet,
                                     formal_indet, eta, indices_eta);

       	#if Groebner:-HilbertDimension([op(map2(op, 1, converted_res)),
       	#		                               dvar], Mord) <= target_dimension then
		# Modif BS. June 2013.
        if Groebner:-HilbertDimension(map(numer,[op(map2(op, 1, converted_res)),
                                       dvar]), Mord) <= target_dimension then
          userinfo(4, :-CreativeTelescoping, "PROFILE - LAST_D", d);
          return converted_res
        end if
      end if

    end do

  else

    d := min_order;
    indices_eta := compute_indices_eta(d, nb_v);
    res := test_order(Alg, d, dim, var, dv, Dv, f_coords, uncoupled_system,
                      eta, indices_eta, local_formal_indet, formal_rhs,
                      uncoupling_method);

    while trivial_res(res, eta, indices_eta) do
      if d = max_order then
        return []
      end if;
      d := min(2*d, max_order);
      indices_eta := compute_indices_eta(d, nb_v);
      res := test_order(Alg, d, dim, var, dv, Dv, f_coords, uncoupled_system,
                        eta, indices_eta, local_formal_indet, formal_rhs,
                        uncoupling_method)
    end do;

    userinfo(4, :-CreativeTelescoping, "PROFILE - LAST_D", d);
    convert_res(res, v, dv, dim, local_formal_indet, formal_indet, eta,
                indices_eta)

  end if

end proc;




## test_order


test_order := proc(Alg::OreAlgebra, d::nonnegint,
                   dim, var, dv, Dv, f_coords,
                   uncoupled_system, eta, indices_eta,
                   local_formal_indet, formal_rhs, uncoupling_method,
                  {constrained_eta := {}}, $)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local nb_v, dv_f, Pf, r, pp, diff_prev, dual_alg;

  userinfo(1, :-CreativeTelescoping, "Test operator P of order", d);

  nb_v := nops(dv);

  # dv_f[[k_1, ... k_i]] is dv[1]^k_1 * dv[i]^k_i f, expressed in the basis.

  dv_f := table();
  Pf := Vector([0$dim]);

  for r in indices_eta do
    for pp to nb_v while r[pp] = 0 do end do;
      if pp = nb_v + 1 then
        dv_f[r] := f_coords;
      else
        diff_prev := map2(Ore_algebra:-skew_product, dv[pp],
                          dv_f[r - [0$(pp-1), 1, 0$(nb_v - pp)]], Alg);
        dv_f[r] := LinearAlgebra:-MatrixVectorMultiply(Dv[pp],
                                                       map(coeff, diff_prev,
                                                           dv[pp]))
                     + eval(diff_prev, dv[pp] = 0)
      end if;
    Pf := Pf + eta[op(r)] * dv_f[r]
  end do;

  Pf := map(normal@expand, Pf);

  if Alg["type_of_left", var] = 'diff' then
    LOE_sys_rat(Alg, var, uncoupled_system, Pf, eta, indices_eta,
                local_formal_indet, formal_rhs, uncoupling_method,
                ':-constrained_eta' = constrained_eta)
  elif Alg["type_of_left", var] = 'shift' then
    dual_alg := Ore_algebra:-dual_algebra(Alg, {Alg["right_of_left", var]});
    LOE_sys_rat(dual_alg, var, uncoupled_system,
                map(normal, -eval(Pf, var = var-1)),
                eta,
                indices_eta, local_formal_indet, formal_rhs,
                uncoupling_method, ':-constrained_eta' = constrained_eta)
  elif Alg["type_of_left", var] = 'qdilat' then
    dual_alg := Ore_algebra:-dual_algebra(Alg, {Alg["right_of_left", var]});
    LOE_sys_rat(dual_alg, var, uncoupled_system,
                map(normal,
                    -eval(Pf, var = var/Alg["q_of_right",
                                            Alg["right_of_left", var]])),
                eta,
                indices_eta, local_formal_indet, formal_rhs,
                uncoupling_method)
  else
    error "Operator type has to be either shift, qdilat or diff.";
  end if

end proc;




## compute_indices_eta


compute_indices_eta := proc(d::nonnegint, nb_v::posint)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local P, r, t, x, u, i, compare_lists;

  compare_lists := proc(l1, l2)
    for i to nops(l1) do
      if l1[i] > l2[i] then return false end if;
      if l1[i] < l2[i] then return true end if;
    end do;
    false;
  end proc;

  P := convert(series(1/(1-u*add(x[i], i=1..nb_v)), u, d+1), polynom);
  r := expand(eval(P, u = 1));
  coeffs(r, [seq(x[i], i = 1..nb_v)], 't');
  sort(map(u -> [seq(degree(u, x[i]), i = 1..nb_v)], [t]), compare_lists)

end proc;




## trivial_res


trivial_res := proc(res, eta, indices_eta)
  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;
  local id;
  evalb(res[2] = {} or {seq(eval(eta[op(id)], res[2]),
                            id in indices_eta)} = {0});
end proc;




## convert_res


convert_res := proc(res, v, dv, dim, local_formal_indet, formal_indet, eta,
                    indices_eta)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  local nb_v, new_res, id, nb_indets, ind, old_form_param;

  nb_v := nops(v);
  new_res := [eval(add(normal(eta[op(id)]) * mul(dv[i]^id[i], i = 1..nb_v),
                       id in indices_eta), res[2]),
              [seq(eval(res[1][i], res[2]), i = 1..dim)]];

  old_form_param := [op(select(x -> op(0, x) = local_formal_indet or
                               op(0, x) = eta, indets(new_res)))];

  if old_form_param = [] then
    [new_res]
  else
    [seq(eval(new_res, [i = 1, seq(j=0, j in {op(old_form_param)} minus {i})]),
         i in old_form_param)]
  end if

end proc;
