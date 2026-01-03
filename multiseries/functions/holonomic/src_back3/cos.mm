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

`multiseries/function`[cos, _AnyFiniteOrdinaryPoint, "CoeffProc"]:=proc (n, arg0, arg1, xarg) local i1, loc; option `Copyright (c) 2004 by Ludovic Meunier and the Algorithms Project, INRIA. All rights reserved. - generated 10/26/2004 @ 23:24:18`; if n = 0 then [arg0] elif n = 1 then [arg0, arg1] else loc[0] := arg0; loc[1] := arg1; for i1 from 2 to n do loc[i1] := -loc[i1-2]/i1/(i1-1) end do; [seq(loc[i1],i1 = 0 .. n)] end if end proc:
`multiseries/function`[cos, infinity, -Pi .. Pi]:=proc (expr, scale, var, order, arglist) local res; option `Copyright (c) 2004 by Ludovic Meunier and the Algorithms Project, INRIA. All rights reserved. - generated 10/26/2004 @ 23:24:19`; res := `multiseries/add`([`multiseries/mul`([`multiseries/function`[exp](`multiseries/MulDoit`(`multiseries/pow`(expr,-1,scale,var,order),-I),scale,var,order), `multiseries/Convert2SERIES`(1/2,scale,var,false)]), `multiseries/mul`([`multiseries/function`[exp](`multiseries/MulDoit`(`multiseries/pow`(expr,-1,scale,var,order),I),scale,var,order), `multiseries/Convert2SERIES`(1/2,scale,var,false)])]) end proc:
`multiseries/function`[cos]:=proc (expr, scale, var, order) local lim, lim2, argseq, res; option `Copyright (c) 2004 by Ludovic Meunier and the Algorithms Project, INRIA. All rights reserved. - generated 10/26/2004 @ 23:24:19`; if expr = 0 then return `multiseries/Convert2SERIES`(cos(0),scale,var,false) end if; lim := `multiseries/limit`(expr,scale,var); lim2 := lim; if lim = undefined then error "unable to compute series" elif has(lim,infinity) then lim := infinity end if; if lim = infinity then argseq := `multiseries/pow`(expr,-1,scale,var,order), scale, var, order else argseq := `multiseries/AddDoit`(expr,`multiseries/Convert2SERIES`(-lim,scale,var,false)), scale, var, order end if; if lim = 0 then res := `multiseries/function`[cos,0](argseq) elif lim = infinity then res := `multiseries/function`[cos,infinity,-Pi .. Pi](argseq,[op(1,[argseq]), scale, var, order]) else res := eval(`multiseries/function`[cos,_AnyFiniteOrdinaryPoint](argseq),_AnyFiniteOrdinaryPoint = lim) end if; subsop(9 = cos(op(9,expr)),res) end proc:
`multiseries/function`[cos, 0]:=proc (expr, scale, var, order) option `Copyright (c) 2004 by Ludovic Meunier and the Algorithms Project, INRIA. All rights reserved. - generated 10/26/2004 @ 23:24:18`; `multiseries/compose`(SERIES(scale,`multiseries/function`[cos,_AnyFiniteOrdinaryPoint,"CoeffProc"](order,1,0,0),1,algebraic,[`$`(0 .. order)],order+1,integer,eval(scale['variable'],1),cos(eval(scale['variable'],1))),expr,scale,var,order) end proc:
`multiseries/function`[cos, _AnyFiniteOrdinaryPoint]:=proc (expr, scale, var, order) option `Copyright (c) 2004 by Ludovic Meunier and the Algorithms Project, INRIA. All rights reserved. - generated 10/26/2004 @ 23:24:18`; `multiseries/compose`(SERIES(scale,map(normal,`multiseries/function`[cos,_AnyFiniteOrdinaryPoint,"CoeffProc"](order,cos(_AnyFiniteOrdinaryPoint),-sin(_AnyFiniteOrdinaryPoint),_AnyFiniteOrdinaryPoint)),1,algebraic,[`$`(0 .. order)],order+1,integer,eval(scale['variable'],1),cos(_AnyFiniteOrdinaryPoint+eval(scale['variable'],1))),expr,scale,var,order) end proc:
