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

unprotect('Mgfun');

Mgfun:=module()
    export MG_Internals, creative_telescoping,
        dfinite_expr_to_sys, dfinite_expr_to_diffeq, dfinite_expr_to_rec,
        diag_of_sys, int_of_sys, pol_to_sys, sum_of_sys, `sys+sys`, `sys*sys`,
	#hermite_telescoping, almkvist_zeilberger
	rational_creative_telescoping;
    option `Copyright (c) 2000-2010 Frederic Chyzak, INRIA, France`,
      package, load = startup ;
    local
      startup,
      # Functions used by CreativeTelescoping
      uncouple_system_AZ, denom_solution, denom_solution_shift,
      denom_solution_diff, denom_solution_qdilat, LOE_sys_rat_AZ,
      uncouple_system, LOE_sys_rat, Zeilberger, test_order, compute_indices_eta,
      trivial_res, convert_res, my_conv, NonCommutativeMultiplicationMatrix,
      uncouple_system_lexdeg, LOE_sys_rat_lexdeg, degree_upper_bound,
      degree_upper_bound_shift, degree_upper_bound_diff,
      degree_upper_bound_qdilat, rename_indets, skew_poly_creative_telescoping,
      LOE_poly, LOE_rat, RationalTelescoping ;

    startup := proc()
        option `Copyright (c) 2009 Frederic Chyzak, INRIA, France`;
      # Ore_algebra:-OA_Internals, `diff/ApplyOpr`, and other unames
      # procedures in Ore_algebra use the global name ApplyOpr, so that
      # :-ApplyOpr and Ore_algebra:-ApplyOpr have to name the same value.
      # `:-ApplyOpr` names are also generated from I don't know where, and
      # so have to map to the same value also.
      :-ApplyOpr := eval(Ore_algebra:-ApplyOpr) ;
      :-`:-ApplyOpr` := :-ApplyOpr ;
      unprotect(Ore_algebra:-ApplyOpr) ;
      Ore_algebra:-ApplyOpr := :-ApplyOpr ;
      protect(Ore_algebra:-ApplyOpr) ;
      :-AlgSubs := eval(Ore_algebra:-AlgSubs) ;
      :-`:-AlgSubs` := :-AlgSubs ;
      unprotect(Ore_algebra:-AlgSubs) ;
      Ore_algebra:-AlgSubs := :-AlgSubs ;
      protect(Ore_algebra:-AlgSubs)
    end ;

    MG_Internals:=module()
        export add_prod_of_sys,int_of_sys,addition_of_sys,diag_of_sys,anti_partial_of_sys,pol_to_sys,product_of_sys,sum_of_sys,recognize_operator_algebra,dfinite_spec_table,dfinite_expr_to_diffeq,dfinite_expr_to_rec,dfinite_expr_to_sys,type_checking,expression_to_system,rational_solutions,my_normal,creative_telescoping,polynomial_solutions, skew_poly_creative_telescoping, LOE_poly, LOE_rat;
        option `Copyright (c) 2000-2007 Frederic Chyzak, INRIA, France`;
        # Why do we have any Holonomy/* here???
        local `Holonomy/new_dx`, `Holonomy/new_qx`, `Mgfun/args_type_check_table`, `Mgfun/choose_wdeg/simplify_poly`, `Mgfun/choose_wdeg`, `Mgfun/chyzak97`, `Mgfun/degree_bound`, `Mgfun/denominator_bound`, `Mgfun/diff_to_order`, `Mgfun/integer_solns`, `Mgfun/is_dfinite_expr`, `Mgfun/is_hypergeom_expr`, `Mgfun/is_integer_linear_term`, `Mgfun/is_integer_quadratic_term`, `Mgfun/polynomial_solve`, `Mgfun/rational_solve`, `Mgfun/rational_sys_solve`, `Mgfun/substituted_atom_to_system`, `Mgfun/type_checking/normalization`, `Mgfun/type_checking/rewrite_powers`, `Mgfun/type_checking/rewrite_spec_functions`, `Mgfun/uncoupling`, `Mgfun/under_the_stairs`, `Mgfun/undiffy`,
        # The following locals are used at compile-time by dfinite_spec_table.mm.
        spec_table_aux, i, j, signature, non_param_number, param_number, inverse_number, the_proc, args_exprseq, q_calculus, x_offset, dx_offset, q_offset, qx_offset, type_table, type_list, descr, Alg, TOrd, GB, hdim;

$include <ApplyOpr_AlgSubs.mi>
$include <compilation.mi>

$include <Mgfun/add_prod_of_sys.mm>
$include <Mgfun/int_of_sys.mm>
$include <Mgfun/addition_of_sys.mm>
$include <Mgfun/diag_of_sys.mm>
$include <Mgfun/anti_partial_of_sys.mm>
$include <Mgfun/pol_to_sys.mm>
$include <Mgfun/product_of_sys.mm>
$include <Mgfun/sum_of_sys.mm>
$include <Mgfun/recognize_operator_algebra.mm>
$include <Mgfun/dfinite_spec_table.mm>
$include <Mgfun/dfinite_expr_to_diffeq.mm>
$include <Mgfun/dfinite_expr_to_rec.mm>
$include <Mgfun/dfinite_expr_to_sys.mm>
$include <Mgfun/type_checking.mm>
$include <Mgfun/expression_to_system.mm>
$include <Mgfun/rational_solutions.mm>
$include <Mgfun/my_normal.mm>
$include <Mgfun/creative_telescoping.mm>
$include <Mgfun/polynomial_solutions.mm>
$include <Mgfun/CreativeTelescoping/skew_poly_creative_telescoping.mm>

    end module;

    creative_telescoping:=MG_Internals:-creative_telescoping;
    dfinite_expr_to_sys:=MG_Internals:-dfinite_expr_to_sys;
    dfinite_expr_to_diffeq:=MG_Internals:-dfinite_expr_to_diffeq;
    dfinite_expr_to_rec:=MG_Internals:-dfinite_expr_to_rec;
    diag_of_sys:=MG_Internals:-diag_of_sys;
    int_of_sys:=MG_Internals:-int_of_sys;
    pol_to_sys:=MG_Internals:-pol_to_sys;
    sum_of_sys:=MG_Internals:-sum_of_sys;
    `sys+sys`:=MG_Internals:-addition_of_sys;
    `sys*sys`:=MG_Internals:-product_of_sys;
    skew_poly_creative_telescoping:=MG_Internals:-skew_poly_creative_telescoping;
    LOE_poly:=MG_Internals:-LOE_poly;
    LOE_rat:=MG_Internals:-LOE_rat;

$include <Mgfun/RationalTelescoping.mm>

    rational_creative_telescoping := proc(R, typed_x :: `::`, typed_y :: `::`,
      { normalized :: boolean := true })

      option `Copyright (c) 2010, INRIA. All rights reserved.` ;

      local x, y ;
      x := op(1, typed_x) ;
      y := op(1, typed_y) ;
      if typed_x <> x :: diff or typed_y <> y ::diff then
        error "only the purely differential case is implemented" ;
      end if ;

      if normalized then
        RationalTelescoping:-AlmkvistZeilberger(R, x, y)
      else
        RationalTelescoping:-HermiteTelescoper(R, x, y)
      end if

    end proc

end module :

protect(Mgfun);

`Mgfun/old_behaviour_of_Groebner_Reduce` := proc(f, G, MOrd, s, $)
  option `Copyright (c) 2009 Frederic Chyzak, INRIA, France` ;
  local res, scale ;
  global set, distributed ;
  res := Groebner:-Reduce(f, G, MOrd, scale) ;
  s := numer(scale) ;
  scale := denom(scale) ;
  collect(scale*res,
    `if`(type(MOrd, ShortMonomialOrder), indets(MOrd),
      convert(MOrd["order_indets"], 'set')),
    'distributed')
end proc:


# Work around bug in Maple13 that should be fixed in Maple[>13].

`Mgfun/NOT_BUGGED_Reduce` := proc(f, GB, Mord, s::name, Q::name, $)

  option
    `Copyright (c) 2010, INRIA. All rights reserved.`;

  local A, p, q, a, lc, lm, i, c, l, d, dd, reduced, j, n, nlc, N, LM_GB;

  A := Mord["algebra"];
  N := nops(GB);
  LM_GB := [seq(Groebner:-LeadingMonomial(i, Mord), i in GB)];
  p := normal(f);
  dd := denom(p);
  p := numer(p);
  a := table([0$N]);
  reduced := true;

  while reduced do
    reduced := false;
    for i to N do
      lc,lm := Groebner:-LeadingTerm(p, Mord);
      if divide(lm, LM_GB[i], 'q') then
        reduced := true;
        c := Ore_algebra:-skew_product(q, GB[i], A);
        l := Groebner:-LeadingCoefficient(c, Mord);

        nlc := normal(lc/l);
        d := denom(nlc);
        n := numer(nlc);
        dd := expand(d*dd);

        for j to N do a[j] := normal(d*a[j]) end do;

        p := normal(d*p-n*c);
        a[i] := a[i] + n*q;

      end if
    end do
  end do;

  Q := [seq(a[i], i = 1..N)];
  s := dd;
  p

end proc:


`Mgfun/version` := 4.0 :

#savelib('`Mgfun/version`', '`Mgfun/old_behaviour_of_Groebner_Reduce`', 'Mgfun', '`Mgfun/NOT_BUGGED_Reduce`');
