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

## creative_telescoping_tools.mpl
## Functions to check creative_telescoping.
## Copyright (c) 2009, INRIA. All rights reserved.
## Author: Lucien Pech <lucien.pech@ens.fr>.



Digits := 200;
EPS := evalf(10^(-100));



## my_rand_float
## INPUT: interval
## OUTPUT: a random float in the interval


my_rand_float := proc(r::`..`, $)
  local maxi, mini;
  maxi := op(2, r);
  mini := op(1, r);
  RandomTools:-Generate(float(range = 0 .. maxi-mini, method = uniform)) + mini;
end proc;



## get_rand
## INPUT: list. [var::type] or [var::type, interval]
## OUTPUT: a random value of type type.


get_rand := proc(cpl::list, $)
  if op(2, cpl[1]) = int then
    if nops(cpl) = 1 then
      rand()
    else
      rand(cpl[2])()
    end if
  elif op(2, cpl[1]) = float then
    if nops(cpl) = 1 then
      my_rand_float(0..1)
    else
      my_rand_float(cpl[2])
    end if
  else
    error("Unknown variable type: %1", op(2, cpl[1]));
  end if
end proc;




## eval_indet
## INPUT:
## OUTPUT:


eval_indet := proc(res, evals := [], $)
  local t, j;
  eval(res, [seq(op(1, j[1]) = get_rand(j), j = evals)])
end proc;




## check_op

## INPUT:
## - f: a d-finite function.
## - var_list: a list whose entries are of type name::type. The variables of
##      the operator P.
## - var_int: a list whose entries are of type name::type. The
##      integrand variables.
## - res: a list [operator, g]. (op1 operates on _F and op2 on _f).
## - eval (optional):

## OUTPUT: a boolean.


check_op := proc(f, vars_list, var_int_, res, evals := [], $)

  local var_int, v_int, v, type_var, P, q, Q, r, i;

  var_int := `if`(type(var_int_, 'list'), var_int_, [var_int_]);
  v_int := map2(op, 1, var_int);
  type_var := map2(op, 2, var_int);
  v := map2(op, 1, vars_list);

  P := eval(res[1], _F = unapply(f, op(v)));
  Q := table();

  for i to nops(var_int) do
     q := eval(res[i+1], _f = unapply(f, op(v), op(v_int)));
     if type_var[i] = 'diff' then
       Q[i] := diff(q,  v_int[i]);
     elif type_var[i] in {'shift', 'qshift'} then
       Q[i] := eval(q, v_int[i] = v_int[i] + 1)-q;
     end if;
  end do;

  r := abs(simplify(eval_indet(simplify(P-add(Q[i], i = 1..nops(var_int))),
                               evals)));
  evalb(r < EPS)

end proc;




## check_ct

## INPUT:
## - f: a d-finite function.
## - var_list: a list whose entries are of type name::type. The variables of
##      the operator P.
## - var_int: list of name::type. The integrand variable(s).
## - res: a list of lists [op1, op2], returned by creative_telescoping (op1
##      operates on _F(op(map2(op, 1, vars_list))) and op2 on _f).
## - eval (optional):

## OUTPUT: okay if all pairs [P, g] satisfy Pf = d[var_int] g in the
##   differential case or Pf = (S[var_int]-1) g in the shift / qdilat cases,
##   FAILURE otherwise.


check_ct := proc(f, vars_list, var_int, res, evals := [], $)
  if {op(map[4](check_op, f, vars_list, var_int, res, evals))} = {true} then
    okay
  else
    FAILURE
  end if
end proc;
