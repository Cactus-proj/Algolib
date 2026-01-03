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

`multiseries/function`[arccoth, 1, "BasisVector", 0, 0, 0, "MultiSeries", 0]:=proc (expr, scale, var, order) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:11`; SERIES(scale,`multiseries/function`[arccoth,1,"CoeffProc",0,0,0](order),1,algebraic,[`$`(0 .. order)],order+1,integer,eval(scale['variable'],1),`arccoth/1/BasisVector/0/0/0/MultiSeries/0`(eval(scale['variable'],1))) end proc:
`multiseries/function`[arccoth, -1, "BasisVector", 0, 0, 1]:=proc (n) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:12`; [0, `multiseries/function`[arccoth,-1,"CoeffProc",0,0,1](n), `multiseries/function`[arccoth,-1,"CoeffProc",0,0,0](n)] end proc:
`multiseries/function`[arccoth, 1, "BasisVector", 0, 0, 1, "MultiSeries", 0]:=proc (expr, scale, var, order) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:11`; SERIES(scale,`multiseries/function`[arccoth,1,"CoeffProc",0,0,1](order),1,algebraic,[`$`(0 .. order)],order+1,integer,eval(scale['variable'],1),`arccoth/1/BasisVector/0/0/1/MultiSeries/0`(eval(scale['variable'],1))) end proc:
`multiseries/function`[arccoth, _AnyFiniteOrdinaryPoint]:=proc (expr, scale, var, order) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:10`; `multiseries/compose`(SERIES(scale,map(normal,`multiseries/function`[arccoth,_AnyFiniteOrdinaryPoint,"CoeffProc"](order,arccoth(_AnyFiniteOrdinaryPoint),-1/(_AnyFiniteOrdinaryPoint^2-1),_AnyFiniteOrdinaryPoint)),1,algebraic,[`$`(0 .. order)],order+1,integer,eval(scale['variable'],1),arccoth(_AnyFiniteOrdinaryPoint+eval(scale['variable'],1))),expr,scale,var,order) end proc:
`multiseries/function`[arccoth, -1, "BasisVector", 0, 0, 1, "MultiSeries"]:=proc (expr, scale, var, order) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:12`; [`multiseries/function`[arccoth,-1,"BasisVector",0,0,1,"MultiSeries",0](args), `multiseries/function`[arccoth,-1,"BasisVector",0,0,1,"MultiSeries",1](args)] end proc:
`multiseries/function`[arccoth, infinity, "CoeffProc"]:=proc (n, arg0, arg1) local i1, loc; option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:10`; if n = 0 then [arg0] elif n = 1 then [arg0, arg1] else loc[0] := arg0; loc[1] := arg1; for i1 from 2 to n do loc[i1] := -(-i1+2)*loc[i1-2]/i1 end do; [seq(loc[i1],i1 = 0 .. n)] end if end proc:
`multiseries/function`[arccoth, 1, "BasisVector", 0, 0, 1]:=proc (n) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:11`; [0, `multiseries/function`[arccoth,1,"CoeffProc",0,0,1](n), `multiseries/function`[arccoth,1,"CoeffProc",0,0,0](n)] end proc:
`multiseries/function`[arccoth, infinity]:=proc (expr, scale, var, order) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:10`; `multiseries/compose`(SERIES(scale,`multiseries/function`[arccoth,infinity,"CoeffProc"](order,0,1),1,algebraic,[`$`(0 .. order)],order+1,integer,eval(scale['variable'],1),arccoth(1/eval(scale['variable'],1))),expr,scale,var,order) end proc:
`multiseries/function`[arccoth]:=proc (expr, scale, var, order) 
local tmp, re, im, lim, lim2, argseq, res;
option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:13`; 
    if expr = 0 then return `multiseries/Convert2SERIES`(arccoth(0),scale,var,false) end if; 
    lim := `multiseries/limit`(expr,scale,var); lim2 := lim; 
    if lim = undefined then error "unable to compute series" 
    elif has(lim,infinity) then lim := infinity 
    end if; 
    if lim = infinity then argseq := `multiseries/pow`(expr,-1,scale,var,order), scale, var, order 
    else argseq := `multiseries/AddDoit`(expr,`multiseries/Convert2SERIES`(-lim,scale,var,false)), scale, var, order 
    end if; 
    if lim = -1 or Testzero(lim+1) then res := `multiseries/function`[arccoth,-1](argseq) 
    elif lim = 0 then res := `multiseries/function`[arccoth,0](argseq) 
    elif lim = 1 or Testzero(lim-1) then res := `multiseries/function`[arccoth,1](argseq) 
    elif lim = infinity then res := `multiseries/function`[arccoth,infinity](argseq) 
    else res := eval(`multiseries/function`[arccoth,_AnyFiniteOrdinaryPoint](argseq),_AnyFiniteOrdinaryPoint = lim) 
    end if; 
    if _Envnobranchcut <> true then 
        if `multiseries/IsOnBranchCut`([-1, 1],expr,scale,var) then 
            userinfo(2,MultiSeries,`branchcut:`,arccoth,[-1, 1]); 
            if lim = infinity then im := `multiseries/sign`(Im(`multiseries/MulSeriesCst`(expr,-1,algebraic))) 
            else im := `multiseries/sign`(Im(`multiseries/MulSeriesCst`(`multiseries/AddDoit`(expr,-lim),-1,algebraic))) 
            end if; 
            if im = 1 then 
                _Envnobranchcut := true; 
                userinfo(2,MultiSeries,`jump:`,arccoth,`multiseries/run`(I*Pi,scale,var,order)); 
                res := `multiseries/run`(I*Pi+res,scale,var,order); 
                _Envnobranchcut := false 
            elif type(im,specfunc(anything,'signum')) or type(-im,specfunc(anything,signum)) then
                _Envnobranchcut := true; 
                userinfo(2,MultiSeries,`jump:`,arccoth,`multiseries/run`(1/2*I*(im+1)*Pi,scale,var,order)); 
                res := `multiseries/run`(1/2*I*(im+1)*Pi+res,scale,var,order); 
                _Envnobranchcut := false 
            end if 
        end if 
    end if; 
    subsop(9 = ('arccoth')(op(9,expr)),res) 
end proc:
`multiseries/function`[arccoth, 1, "CoeffProc", 0, 0, 1]:=proc (n) local i1, loc, xxloc1; option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:11`; xxloc1 := `multiseries/function`[arccoth,1,"CoeffProc",0,0,0](n); loc[0] := 0; loc[1] := -1/2; for i1 from 2 to n do loc[i1] := 1/2*((-(2*i1-1)*op(i1,xxloc1)-4*i1*op(i1+1,xxloc1))/i1-(i1-1)*loc[i1-1])/i1 end do; [seq(loc[i1],i1 = 0 .. n)] end proc:
`multiseries/function`[arccoth, 1, "BasisVector", 0, 0, 1, "MultiSeries", 1]:=proc (expr, scale, var, order) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:11`; SERIES(scale,`multiseries/function`[arccoth,1,"CoeffProc",0,0,0](order),1,algebraic,[`$`(0 .. order)],order+1,integer,eval(scale['variable'],1),`arccoth/1/BasisVector/0/0/1/MultiSeries/1`(eval(scale['variable'],1))) end proc:
`multiseries/function`[arccoth, 1, "BasisVector", 0, 0, 1, "MultiSeries"]:=proc (expr, scale, var, order) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:11`; [`multiseries/function`[arccoth,1,"BasisVector",0,0,1,"MultiSeries",0](args), `multiseries/function`[arccoth,1,"BasisVector",0,0,1,"MultiSeries",1](args)] end proc:
`multiseries/function`[arccoth, -1, "BasisVector", 0, 0, 0]:=proc (n) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:12`; [0, `multiseries/function`[arccoth,-1,"CoeffProc",0,0,0](n)] end proc:
`multiseries/function`[arccoth, -1, "BasisVector", 0, 0, 0, "MultiSeries"]:=proc (expr, scale, var, order) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:12`; [`multiseries/function`[arccoth,-1,"BasisVector",0,0,0,"MultiSeries",0](args)] end proc:
`multiseries/function`[arccoth, -1, "BasisVector", 0, 0, 1, "MultiSeries", 1]:=proc (expr, scale, var, order) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:12`; SERIES(scale,`multiseries/function`[arccoth,-1,"CoeffProc",0,0,0](order),1,algebraic,[`$`(0 .. order)],order+1,integer,eval(scale['variable'],1),`arccoth/-1/BasisVector/0/0/1/MultiSeries/1`(eval(scale['variable'],1))) end proc:
`multiseries/function`[arccoth, _AnyFiniteOrdinaryPoint, "CoeffProc"]:=proc (n, arg0, arg1, xarg) local i1, loc; option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:10`; if n = 0 then [arg0] elif n = 1 then [arg0, arg1] else loc[0] := arg0; loc[1] := arg1; for i1 from 2 to n do loc[i1] := (2*loc[i1-2]+2*xarg*loc[i1-1]-(loc[i1-2]+2*xarg*loc[i1-1])*i1)/(xarg^2-1)/i1 end do; [seq(loc[i1],i1 = 0 .. n)] end if end proc:
`multiseries/function`[arccoth, -1, "CoeffProc", 0, 0, 1]:=proc (n) local i1, loc, xxloc1; option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:12`; xxloc1 := `multiseries/function`[arccoth,-1,"CoeffProc",0,0,0](n); loc[0] := 0; loc[1] := -1/2; for i1 from 2 to n do loc[i1] := -1/2*((-(1-2*i1)*op(i1,xxloc1)+4*i1*op(i1+1,xxloc1))/i1-(1-i1)*loc[i1-1])/i1 end do; [seq(loc[i1],i1 = 0 .. n)] end proc:
`multiseries/function`[arccoth, 1, "CoeffProc", 0, 0, 0]:=proc (n) local i1, loc; option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:11`; loc[0] := 1; loc[1] := 0; for i1 from 2 to n do loc[i1] := -1/2*(i1-1)*loc[i1-1]/i1 end do; [seq(loc[i1],i1 = 0 .. n)] end proc:
`multiseries/function`[arccoth, 1, "BasisVector", 0, 0, 0, "MultiSeries"]:=proc (expr, scale, var, order) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:11`; [`multiseries/function`[arccoth,1,"BasisVector",0,0,0,"MultiSeries",0](args)] end proc:
`multiseries/function`[arccoth, -1, "BasisVector", 0, 0, 1, "MultiSeries", 0]:=proc (expr, scale, var, order) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:12`; SERIES(scale,`multiseries/function`[arccoth,-1,"CoeffProc",0,0,1](order),1,algebraic,[`$`(0 .. order)],order+1,integer,eval(scale['variable'],1),`arccoth/-1/BasisVector/0/0/1/MultiSeries/0`(eval(scale['variable'],1))) end proc:
`multiseries/function`[arccoth, -1, "CoeffProc", 0, 0, 0]:=proc (n) local i1, loc; option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:12`; loc[0] := 1; loc[1] := 0; for i1 from 2 to n do loc[i1] := 1/2*(1-i1)*loc[i1-1]/i1 end do; [seq(loc[i1],i1 = 0 .. n)] end proc:
`multiseries/function`[arccoth, -1, "BasisVector", 0, 0, 0, "MultiSeries", 0]:=proc (expr, scale, var, order) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:12`; SERIES(scale,`multiseries/function`[arccoth,-1,"CoeffProc",0,0,0](order),1,algebraic,[`$`(0 .. order)],order+1,integer,eval(scale['variable'],1),`arccoth/-1/BasisVector/0/0/0/MultiSeries/0`(eval(scale['variable'],1))) end proc:
`multiseries/function`[arccoth, 1, "BasisVector", 0, 0, 0]:=proc (n) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:11`; [0, `multiseries/function`[arccoth,1,"CoeffProc",0,0,0](n)] end proc:
`multiseries/function`[arccoth, -1]:=proc (expr, scale, var, order) local loc1, loc2, loc3, res, res1, res2; option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:12`; ASSERT(`multiseries/limit`(expr,scale,var) = 0 or Testzero(`multiseries/limit`(expr,scale,var))); loc3 := `multiseries/MulSeriesCst`(expr,-1,algebraic); loc1 := `multiseries/function`[arccoth,-1,"BasisVector",0,0,0,"MultiSeries"](loc3,scale,var,order); loc2 := -1/2*ln(2); res1 := `multiseries/function`["Helper","RegularBasisVector"](loc1,[loc3, scale, var, order],0,loc2,-1); loc1 := `multiseries/function`[arccoth,-1,"BasisVector",0,0,1,"MultiSeries"](loc3,scale,var,order); loc2 := 1/2; res2 := `multiseries/function`["Helper","RegularBasisVector"](loc1,[loc3, scale, var, order],0,loc2,-1); res := `multiseries/add`([res1, res2]); subsop(9 = ('arccoth')(op(9,expr)),res) end proc:
`multiseries/function`[arccoth, 0]:=proc (expr, scale, var, order) option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:10`; `multiseries/compose`(SERIES(scale,`multiseries/function`[arccoth,_AnyFiniteOrdinaryPoint,"CoeffProc"](order,-1/2*I*Pi,1,0),1,algebraic,[`$`(0 .. order)],order+1,integer,eval(scale['variable'],1),arccoth(eval(scale['variable'],1))),expr,scale,var,order) end proc:
`multiseries/function`[arccoth, 1]:=proc (expr, scale, var, order) local loc1, loc2, loc3, res, res1, res2; option `Copyright (c) 2006 by the Algorithms Project, INRIA. All rights reserved. - generated 05/16/2006 @ 11:19:11`; ASSERT(`multiseries/limit`(expr,scale,var) = 0 or Testzero(`multiseries/limit`(expr,scale,var))); loc3 := `multiseries/MulSeriesCst`(expr,1,algebraic); loc1 := `multiseries/function`[arccoth,1,"BasisVector",0,0,0,"MultiSeries"](loc3,scale,var,order); loc2 := 1/2*ln(2); res1 := `multiseries/function`["Helper","RegularBasisVector"](loc1,[loc3, scale, var, order],0,loc2,1); loc1 := `multiseries/function`[arccoth,1,"BasisVector",0,0,1,"MultiSeries"](loc3,scale,var,order); loc2 := -1/2; res2 := `multiseries/function`["Helper","RegularBasisVector"](loc1,[loc3, scale, var, order],0,loc2,1); res := `multiseries/add`([res1, res2]); subsop(9 = ('arccoth')(op(9,expr)),res) end proc:
