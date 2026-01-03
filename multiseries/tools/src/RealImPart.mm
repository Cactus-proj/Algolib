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

###    -*-Maple-*-
###
###    Title: 	`multiseries/RealImPart`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 : 
###                         ser       a SERIES data-structure or 0
###                         real      a name
###                         imaginary a name
### Output                : 
###                         the real (resp. imaginary) is assigned to the
###                         real (resp. imaginary) part of ser
###

`multiseries/RealImPart` := proc(ser,real,imaginary)
local realexpr, re, imexpr, im, i, lre, lim, expr, lcoeffs :: list, scale;
option ALGOCOPYRIGHT; 

    ASSERT(ser::{'SERIES',identical(0)} and real::name and imaginary::name);

    # zero series
    if ser = 0 then (real, imaginary) := (0,0); return NULL end if;

    expr := op(EXPR4SERIES,ser);
    realexpr := Re(expr); 
    if realexpr=expr then imexpr:=0 else imexpr := Im(expr) fi;
    scale := op(THESCALE,ser);

    # either the real part or the imaginary part is zero
    if imexpr = 0 or imexpr = 0. or TESTZERO(imexpr,scale) then
       imaginary := 0;
       if realexpr = 0 or realexpr = 0. or TESTZERO(realexpr,scale) then 
	  real := 0 
       else real := ser
       end if;
       return NULL
    elif realexpr = 0 or imexpr = 0. or TESTZERO(realexpr,scale) then
       real := 0;
       imaginary := MULSERIESCST(ser,-I,algebraic);
       return NULL
    end if;

    # neither the real part nor the imaginary part is zero
    lcoeffs := op(LISTCOEFF,ser);
    if op(TYPECOEFF,ser) = 't_SERIES' then
       for i to nops(lcoeffs) do
          procname(lcoeffs[i],'re','im');
	  lre[i],lim[i] := re,im
       end do;
    else
       lre,lim := map(Re,lcoeffs),map(Im,lcoeffs)
    end if;

    real := SERIES(scale,
               [seq(lre[i],i=1..nops(op(LISTCOEFF,ser)))],
	       op(COEFFBIGO..EXPANVAR,ser),realexpr);

    imaginary := SERIES(scale,
               [seq(lim[i],i=1..nops(op(LISTCOEFF,ser)))],
	       op(COEFFBIGO..EXPANVAR,ser),imexpr);

    NULL

end proc:  # `multiseries/RealImPart`
#----------------------------------------------------------------------------

