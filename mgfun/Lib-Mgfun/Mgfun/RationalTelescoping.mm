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

## Copyright (c) 2010, INRIA & KLMM. All rights reserved.
## Authors: Shaoshi Chen <schen@amss.ac.cn> and Ziming Li <zmli@mmrc.iss.ac.cn>.

RationalTelescoping := module()
  export HermiteTelescoper, AlmkvistZeilberger ;
  local Division, List, Reduction, Telescoper, Decomposition, RationalZeilberger ;

$include <Mgfun/RationalTelescoping/Division.mm>
$include <Mgfun/RationalTelescoping/List.mm>
$include <Mgfun/RationalTelescoping/Reduction.mm>
$include <Mgfun/RationalTelescoping/Telescoper.mm>
$include <Mgfun/RationalTelescoping/Decomposition.mm>

$include <Mgfun/RationalTelescoping/RationalZeilberger.mm>

  HermiteTelescoper := proc(rat, x::name, y::name, $)
    local CT, dx, L, i ;
    CT := Telescoper:-Hermite(rat, x, y, dx, 'output' = 'unnormalized') ;
    L := op(1, CT) ;
    subsop(
      1 = add(coeff(L, dx, i) * diff(_F(x), [x$i]), i=0..degree(L, dx)), CT
    )
  end proc ;

  AlmkvistZeilberger := proc(rat, x::name, y::name, $)
    local CT, dx, L, i ;
    CT := RationalZeilberger:-RatTelescoping(rat, x, y, dx) ;
    L := op(1, CT) ;
    subsop(
      1 = add(coeff(L, dx, i) * diff(_F(x), [x$i]), i=0..degree(L, dx)), CT
    )
  end proc ;

end module :
