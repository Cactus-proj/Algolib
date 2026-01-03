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

## LOE_sys_rat.mm
## Copyright (c) 2009, INRIA. All rights reserved.
## Author: Lucien Pech <lucien.pech@ens.fr>.



## uncouple_system

## Uncouple the system: dvar Phi = A Phi + R

## INPUT

## - Alg: Ore algebra.
## - var: name.


## OUTPUT

## List: the uncoupled system. Format depends on the uncoupling method.


uncouple_system := proc(Alg::OreAlgebra, var::name, A, R,
                        uncoupling_method::{identical(lexdeg), identical(AZ)}
                          := lexdeg, $)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  if uncoupling_method = 'lexdeg' then
    uncouple_system_lexdeg(Alg, var, A, R)
  elif uncoupling_method = 'AZ' then
    uncouple_system_AZ(Alg, var, A, R)
  end if

end proc;




## LOE_sys_rat

## INPUT

## - Alg: Ore algebra.
## - var: name.

## OUTPUT


LOE_sys_rat := proc(Alg::OreAlgebra, var::name, uncoupled_system, R, eta::name,
                    indices_eta, formal_indet, formal_rhs,
                    uncoupling_method::{identical(lexdeg), identical(AZ)}
                      := lexdeg, {constrained_eta := {}}, $)

  option
    `Copyright (c) 2009, INRIA. All rights reserved.`;

  if uncoupling_method = 'lexdeg' then
    LOE_sys_rat_lexdeg(Alg, var, uncoupled_system, R, eta, indices_eta,
                       formal_indet, formal_rhs,
                       ':-constrained_eta' = constrained_eta)
  elif uncoupling_method = 'AZ' then
    LOE_sys_rat_AZ(Alg, var, uncoupled_system, R, eta, indices_eta,
                   formal_indet, formal_rhs)
  end if

end proc;
