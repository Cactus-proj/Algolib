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

`multiseries/function`["Helper","RegularBasisVector"] := proc (msl, arglist, alg, cst, lim) local res, lnexpr, lnpow, i, tmp; option `Copyright (c) 2003 by Bruno Salvy and the Algorithms Project, INRIA. All rights reserved.`; res := MultiSeries:-`multiseries/compose`(msl[1],op(arglist)); if 1 < nops(msl) then lnexpr := MultiSeries:-`multiseries/function`['ln'](op(arglist)); lnpow := 1; for i from 2 to nops(msl) do lnpow := MultiSeries:-`multiseries/MulDoit`(lnpow,lnexpr); res := MultiSeries:-`multiseries/AddDoit`(res,MultiSeries:-`multiseries/MulDoit`(lnpow,MultiSeries:-`multiseries/compose`(msl[i],op(arglist)))) end do end if; tmp := MultiSeries:-`multiseries/mul`([cst, res, MultiSeries:-`multiseries/pow`(arglist[1],alg,op(2 .. -1,arglist))]); if 5 < nargs then subs(args[6],tmp) else tmp end if end proc:
