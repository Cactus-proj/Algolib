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

# test110.tst: test for MultiSeries[multiseries] (no. 110)

#test 40

   # Normalizer:=proc(x) normal(x) end:
   TESTZERO:=proc(x) evalb(normal(x)=0) end:
   Testzero:=`limit/mrv/Testzero`:

   with(MultiSeries,multiseries);
   Order:=3:

   SERIES2Series:=proc(s)
       if s=0 then 0
       else subs(SERIES='Series',op(1,s)=NULL,s)
       fi
   end:

   compare_SERIES:=proc(s1,s2)
   local i;
       if s1=s2 then true
       elif type([s1,s2],list(specfunc(anything,'Series'))) then
   	evalb(	    (op(5,s1)=op(5,s2) or Testzero(op(5,s1)-op(5,s2))) and
   	    Testzero(op(7,s1)-op(7,s2)) and
   	    map(Testzero,{seq(op([4,i],s1)-op([4,i],s2),i=1..nops(op(4,s1)))})
   		={true} and
   	    {seq(procname(op([1,i],s1),op([1,i],s2)),i=1..nops(op(1,s1)))}
   		={true})
       elif type(s1,specfunc(anything,'Series')) then false
       elif type(s2,specfunc(anything,'Series')) then false
       else Testzero(s1-s2)
       fi
   end:

   #=============== test nb 110 =====================
   Order:=3;
   TRY[compare_SERIES]("110-1",
    op([assign(s=multiseries(op([(2/(1/x)^(5/2)*Psi(x)*2^(1/2)*Pi^(1/2)*
    exp(ln(x)*(2*x-1)+lnGAMMA(x)-2*ln(x)*x+3/2*ln(x)+(-ln(x)+1)*x-1/2*
    ln(2*Pi))+2*x^4*(1/2*Psi(1,x)*2^(1/2)*Pi^(1/2)*(1/x)^(1/2)*exp(ln(x)*
    (2*x-1)+lnGAMMA(x)-2*ln(x)*x+3/2*ln(x)+(-ln(x)+1)*x-1/2*ln(2*Pi))+
    1/2*Psi(x)^2*2^(1/2)*Pi^(1/2)*(1/x)^(1/2)*exp(ln(x)*(2*x-1)+
    lnGAMMA(x)-2*ln(x)*x+3/2*ln(x)+(-ln(x)+1)*x-1/2*ln(2*Pi))))*2^(1/2)/
    Pi^(1/2)/(1/x)^(1/2)/exp(ln(x)*(2*x-1)+lnGAMMA(x)-2*ln(x)*x+3/2*ln(x)+
    (-ln(x)+1)*x-1/2*ln(2*Pi))-2*x^4*Psi(x)^2-2*x^3-4*ln(x)*x^3+x^2,
    x = infinity]))),SERIES2Series(s)]),
    Series([-1/6/_var[1/ln(x)]^2+1/Pi^(1/2)*2^(1/2)*(Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]+Pi^(1/2)*2^(1/2)+1/12*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2)-2/_var[1/ln(x)]-2,
    1/144/_var[1/ln(x)]^2-1/12/Pi^(1/2)*2^(1/2)*(Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]+Pi^(1/2)*2^(1/2)+1/12*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]^2)+1/Pi^(1/2)*2^(1/2)*(1/12*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]-5/12*Pi^(1/2)*2^(1/2)+1/288*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]^2+Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*
    (-1/6*_var[1/ln(x)]+1/4*_var[1/ln(x)]^2))-2/
    _var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+1/4*
    _var[1/ln(x)]^2)+1],1,algebraic,[-3, -2],-1,rational,
    _var[1/x],(2*Psi(1/_var[1/x])/_var[
    1/x]^(5/2)*2^(1/2)*Pi^(1/2)*exp(1/_var[1/ln(x)]*(2/
    _var[1/x]-1)+lnGAMMA(1/_var[1/x])-2/_var[
    1/ln(x)]/_var[1/x]+3/2/_var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi))+2*(1/2*Psi(1,
    1/_var[1/x])*2^(1/2)*Pi^(1/2)*_var[1/x]^(1/2)*
    exp(1/_var[1/ln(x)]*(2/_var[1/x]-1)+
    lnGAMMA(1/_var[1/x])-2/_var[1/ln(x)]/_var[1/x]+3/2/_var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/
    _var[1/x]-1/2*ln(2*Pi))+1/2*Psi(1/_var[1/x])^2*
    2^(1/2)*Pi^(1/2)*_var[1/x]^(1/2)*exp(1/_var[1/ln(x)]*
    (2/_var[1/x]-1)+lnGAMMA(1/_var[1/x])-2/_var[1/ln(x)]/_var[1/x]+3/2/_var[1/ln(x)]+
    (-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi)))/
    _var[1/x]^4)*2^(1/2)/Pi^(1/2)/_var[1/x]^(1/2)/
    exp(1/_var[1/ln(x)]*(2/_var[1/x]-1)+lnGAMMA(1/
    _var[1/x])-2/_var[1/ln(x)]/_var[1/x]+
    3/2/_var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[
    1/x]-1/2*ln(2*Pi))-2/_var[1/x]^4*Psi(1/_var[1/x])^2-
    2/_var[1/x]^3-4/_var[1/ln(x)]/_var[1/x]^3+
    1/_var[1/x]^2));
   TRY[compare_SERIES]("110-2",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][1..1])),
    Series([Series([-1/30],0,rational,[0],infinity,rational,_var[
    1/ln(x)],-163879/104509440/_var[1/ln(x)]^2-571/2488320/Pi^(1/2)*
    2^(1/2)*(Pi^(1/2)*2^(1/2)/_var[1/ln(x)]+Pi^(1/2)*2^(1/2)+1/12*
    Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2)+139/51840/Pi^(1/2)*2^(1/2)*
    (1/12*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]-5/12*Pi^(1/2)*2^(1/2)+
    1/288*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2+Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+1/4*
    _var[1/ln(x)]^2))+1/288/Pi^(1/2)*2^(1/2)*(1/288*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]+13/288*Pi^(1/2)*2^(1/2)-139/51840*
    Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2+1/12*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+1/4*
    _var[1/ln(x)]^2))-1/12/Pi^(1/2)*2^(1/2)*(-139/51840*
    Pi^(1/2)*2^(1/2)/_var[1/ln(x)]+131/51840*Pi^(1/2)*2^(1/2)-
    571/2488320*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2+1/288*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+
    1/4*_var[1/ln(x)]^2)+Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*
    (1/60*_var[1/ln(x)]+1/144*_var[1/ln(x)]^2))+1/Pi^(1/2)*
    2^(1/2)*(-571/2488320*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]-58723/2488320*
    Pi^(1/2)*2^(1/2)+163879/209018880*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2-
    139/51840*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(-1/6*_var[
    1/ln(x)]+1/4*_var[1/ln(x)]^2)+1/12*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2*(1/60*_var[1/ln(x)]+1/144*_var[1/ln(x)]^2))+1/60)],
    Series([],1,integer,[],0,integer,_var[1/ln(x)],1),t_SERIES,[1],2,
    rational,_var[1/x],(2*Psi(1/_var[1/x])/_var[
    1/x]^(5/2)*2^(1/2)*Pi^(1/2)*exp(1/_var[1/ln(x)]*(2/_var[
    1/x]-1)+lnGAMMA(1/_var[1/x])-2/_var[1/ln(x)]/_var[
    1/x]+3/2/_var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[
    1/x]-1/2*ln(2*Pi))+(Psi(1,1/_var[1/x])*2^(1/2)*Pi^(1/2)*_var[
    1/x]^(1/2)*exp(1/_var[1/ln(x)]*(2/_var[1/x]-1)+
    lnGAMMA(1/_var[1/x])-2/_var[1/ln(x)]/_var[1/x]+
    3/2/_var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[1/x]-
    1/2*ln(2*Pi))+Psi(1/_var[1/x])^2*2^(1/2)*Pi^(1/2)*_var[
    1/x]^(1/2)*exp(1/_var[1/ln(x)]*(2/_var[1/x]-1)+
    lnGAMMA(1/_var[1/x])-2/_var[1/ln(x)]/_var[
    1/x]+3/2/_var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi)))/_var[1/x]^4)*2^(1/2)/Pi^(1/2)/_var[1/x]^(1/2)/exp(1/_var[1/ln(x)]*(2/_var[1/x]-1)+
    lnGAMMA(1/_var[1/x])-2/_var[1/ln(x)]/_var[
    1/x]+3/2/_var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi))-2/_var[1/x]^4*Psi(1/_var[
    1/x])^2-2/_var[1/x]^3-4/_var[1/ln(x)]/_var[
    1/x]^3+1/_var[1/x]^2));
   TRY[compare_SERIES]("110-3",
    SERIES2Series(multiseries(op(9,s),op(1,s))),
    Series([Series([-1/30],0,rational,[0],infinity,rational,_var[
    1/ln(x)],-163879/104509440/_var[1/ln(x)]^2-571/2488320/Pi^(1/2)*
    2^(1/2)*(Pi^(1/2)*2^(1/2)/_var[1/ln(x)]+Pi^(1/2)*2^(1/2)+
    1/12*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2)+139/51840/Pi^(1/2)*
    2^(1/2)*(1/12*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]-5/12*Pi^(1/2)*
    2^(1/2)+1/288*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2+Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+
    1/4*_var[1/ln(x)]^2))+1/288/Pi^(1/2)*2^(1/2)*(1/288*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]+13/288*Pi^(1/2)*2^(1/2)-139/51840*
    Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2+1/12*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+1/4*
    _var[1/ln(x)]^2))-1/12/Pi^(1/2)*2^(1/2)*(-139/51840*
    Pi^(1/2)*2^(1/2)/_var[1/ln(x)]+131/51840*Pi^(1/2)*2^(1/2)-
    571/2488320*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2+1/288*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+
    1/4*_var[1/ln(x)]^2)+Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*
    (1/60*_var[1/ln(x)]+1/144*_var[1/ln(x)]^2))+1/Pi^(1/2)*
    2^(1/2)*(-571/2488320*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]-
    58723/2488320*Pi^(1/2)*2^(1/2)+163879/209018880*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]^2-139/51840*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2*(-1/6*_var[1/ln(x)]+1/4*_var[1/ln(x)]^2)+
    1/12*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(1/60*_var[
    1/ln(x)]+1/144*_var[1/ln(x)]^2))+1/60)],
    Series([],1,integer,[],0,integer,_var[1/ln(x)],1),t_SERIES,[1],
    2,rational,_var[1/x],(2*Psi(1/_var[1/x])/
    _var[1/x]^(5/2)*2^(1/2)*Pi^(1/2)*exp(1/_var[1/ln(x)]*
    (2/_var[1/x]-1)+lnGAMMA(1/_var[1/x])-2/
    _var[1/ln(x)]/_var[1/x]+3/2/_var[1/
    ln(x)]+(-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi))+
    (Psi(1,1/_var[1/x])*2^(1/2)*Pi^(1/2)*_var[1/x]^(1/2)*
    exp(1/_var[1/ln(x)]*(2/_var[1/x]-1)+lnGAMMA(1/_var[1/x])-2/_var[1/ln(x)]/_var[1/x]+3/2/
    _var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/
    _var[1/x]-1/2*ln(2*Pi))+Psi(1/_var[1/x])^2*2^(1/2)*
    Pi^(1/2)*_var[1/x]^(1/2)*exp(1/_var[1/ln(x)]*
    (2/_var[1/x]-1)+lnGAMMA(1/_var[1/x])-2/
    _var[1/ln(x)]/_var[1/x]+3/2/_var[1/ln(x)]+
    (-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi)))/
    _var[1/x]^4)*2^(1/2)/Pi^(1/2)/_var[1/x]^(1/2)/
    exp(1/_var[1/ln(x)]*(2/_var[1/x]-1)+
    lnGAMMA(1/_var[1/x])-2/_var[1/ln(x)]/
    _var[1/x]+3/2/_var[1/ln(x)]+(-1/_var[
    1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi))-2/_var[
    1/x]^4*Psi(1/_var[1/x])^2-2/_var[1/x]^3-
    4/_var[1/ln(x)]/_var[1/x]^3+1/_var[1/x]^2));
   TRY[compare_SERIES]("110-4",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,exact_order)),
    Series([Series([-1/30],0,rational,[0],infinity,rational,_var[1/ln(x)],-163879/104509440/_var[1/ln(x)]^2-
    571/2488320/Pi^(1/2)*2^(1/2)*(Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]+Pi^(1/2)*2^(1/2)+1/12*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2)+139/51840/Pi^(1/2)*2^(1/2)*(1/12*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]-5/12*Pi^(1/2)*2^(1/2)+1/288*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2+Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+
    1/4*_var[1/ln(x)]^2))+1/288/Pi^(1/2)*2^(1/2)*
    (1/288*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]+13/288*
    Pi^(1/2)*2^(1/2)-139/51840*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2+1/12*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*
    (-1/6*_var[1/ln(x)]+1/4*_var[1/ln(x)]^2))-
    1/12/Pi^(1/2)*2^(1/2)*(-139/51840*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]+131/51840*Pi^(1/2)*2^(1/2)-571/2488320*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]^2+1/288*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2*(-1/6*_var[1/ln(x)]+1/4*_var[
    1/ln(x)]^2)+Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(1/60*
    _var[1/ln(x)]+1/144*_var[1/ln(x)]^2))+
    1/Pi^(1/2)*2^(1/2)*(-571/2488320*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]-58723/2488320*Pi^(1/2)*2^(1/2)+163879/209018880*
    Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2-139/51840*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2*(-1/6*_var[
    1/ln(x)]+1/4*_var[1/ln(x)]^2)+1/12*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2*(1/60*_var[1/ln(x)]+
    1/144*_var[1/ln(x)]^2))+1/60),
    Series([2/63],0,rational,[0],infinity,rational,_var[
    1/ln(x)],534703531/451480780800/_var[1/ln(x)]^2+
    5246819/75246796800/Pi^(1/2)*2^(1/2)*(Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]+Pi^(1/2)*2^(1/2)+1/12*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2)-163879/209018880/Pi^(1/2)*
    2^(1/2)*(1/12*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]-5/12*
    Pi^(1/2)*2^(1/2)+1/288*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2+Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(-1/6*
    _var[1/ln(x)]+1/4*_var[1/ln(x)]^2))-571/
    2488320/Pi^(1/2)*2^(1/2)*(1/288*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]+13/288*Pi^(1/2)*2^(1/2)-139/51840*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2+1/12*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2*(-1/6*_var[1/ln(x)]+1/4*_var[
    1/ln(x)]^2))+139/51840/Pi^(1/2)*2^(1/2)*(-139/51840*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]+131/51840*Pi^(1/2)*2^(1/2)-
    571/2488320*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2+1/288*
    Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(-1/6*_var[
    1/ln(x)]+1/4*_var[1/ln(x)]^2)+Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(1/60*_var[1/ln(x)]+1/144*_var[1/ln(x)]^2))+1/288/Pi^(1/2)*2^(1/2)*(-571/2488320*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]-58723/2488320*Pi^(1/2)*2^(1/2)+
    163879/209018880*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2-
    139/51840*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(-1/6*
    _var[1/ln(x)]+1/4*_var[1/ln(x)]^2)+
    1/12*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(1/60*
    _var[1/ln(x)]+1/144*_var[1/ln(x)]^2))-
    1/12/Pi^(1/2)*2^(1/2)*(163879/209018880*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]-294299/209018880*Pi^(1/2)*2^(1/2)+
    5246819/75246796800*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2-571/2488320*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2*(-1/6*_var[1/ln(x)]+1/4*_var[
    1/ln(x)]^2)+1/288*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*
    (1/60*_var[1/ln(x)]+1/144*_var[1/ln(x)]^2)+
    Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(-1/126*_var[1/ln(x)]-1/720*_var[1/ln(x)]^2))+1/Pi^(1/2)*
    2^(1/2)*(5246819/75246796800*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]+1460769839/75246796800*Pi^(1/2)*2^(1/2)-534703531/
    902961561600*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2+163879/
    209018880*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(-1/6*
    _var[1/ln(x)]+1/4*_var[1/ln(x)]^2)-139/
    51840*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(1/60*_var[1/ln(x)]+1/144*_var[1/ln(x)]^2)+1/12*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2*(-1/126*_var[1/ln(x)]-
    1/720*_var[1/ln(x)]^2))-1/126),
    Series([-1/20],0,rational,[0],infinity,rational,_var[
    1/ln(x)],-432261921612371/257452400443392000/_var[
    1/ln(x)]^2-4483131259/86684309913600/Pi^(1/2)*2^(1/2)*(Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]+Pi^(1/2)*2^(1/2)+1/12*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2)+534703531/902961561600/Pi^(1/2)*
    2^(1/2)*(1/12*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]-5/12*Pi^(1/2)*
    2^(1/2)+1/288*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2+Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+
    1/4*_var[1/ln(x)]^2))+5246819/75246796800/Pi^(1/2)*2^(1/2)*
    (1/288*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]+13/288*Pi^(1/2)*2^(1/2)-
    139/51840*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2+1/12*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+
    1/4*_var[1/ln(x)]^2))-163879/209018880/Pi^(1/2)*2^(1/2)*
    (-139/51840*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]+131/51840*
    Pi^(1/2)*2^(1/2)-571/2488320*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2+
    1/288*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(-1/6*_var[
    1/ln(x)]+1/4*_var[1/ln(x)]^2)+Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2*(1/60*_var[1/ln(x)]+1/144*_var[
    1/ln(x)]^2))-571/2488320/Pi^(1/2)*2^(1/2)*(-571/2488320*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]-58723/2488320*Pi^(1/2)*2^(1/2)+163879/209018880*
    Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2-139/51840*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+1/4*_var[1/ln(x)]^2)+1/12*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*
    (1/60*_var[1/ln(x)]+1/144*_var[1/ln(x)]^2))+139/
    51840/Pi^(1/2)*2^(1/2)*(163879/209018880*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]-294299/209018880*Pi^(1/2)*2^(1/2)+5246819/75246796800*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2-571/2488320*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+1/4*
    _var[1/ln(x)]^2)+1/288*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2*(1/60*_var[1/ln(x)]+1/144*_var[
    1/ln(x)]^2)+Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(-1/126*
    _var[1/ln(x)]-1/720*_var[1/ln(x)]^2))+1/288/Pi^(1/2)*
    2^(1/2)*(5246819/75246796800*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]+
    1460769839/75246796800*Pi^(1/2)*2^(1/2)-534703531/902961561600*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2+163879/209018880*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+1/4*
    _var[1/ln(x)]^2)-139/51840*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2*(1/60*_var[1/ln(x)]+1/144*_var[1/ln(x)]^2)+
    1/12*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(-1/126*_var[
    1/ln(x)]-1/720*_var[1/ln(x)]^2))-1/12/Pi^(1/2)*2^(1/2)*
    (-534703531/902961561600*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]+
    1046332379/902961561600*Pi^(1/2)*2^(1/2)-4483131259/86684309913600*Pi^(1/2)*
    2^(1/2)/_var[1/ln(x)]^2+5246819/75246796800*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+1/4*
    _var[1/ln(x)]^2)-571/2488320*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2*(1/60*_var[1/ln(x)]+1/144*_var[
    1/ln(x)]^2)+1/288*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*
    (-1/126*_var[1/ln(x)]-1/720*_var[1/ln(x)]^2)+
    Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(1/120*_var[
    1/ln(x)]+221/302400*_var[1/ln(x)]^2))+1/Pi^(1/2)*2^(1/2)*
    (-4483131259/86684309913600*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]-
    100005471235/3467372396544*Pi^(1/2)*2^(1/2)+432261921612371/514904800886784000*
    Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2-534703531/902961561600*
    Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(-1/6*_var[1/ln(x)]+
    1/4*_var[1/ln(x)]^2)+163879/209018880*Pi^(1/2)*2^(1/2)/
    _var[1/ln(x)]^2*(1/60*_var[1/ln(x)]+1/144*
    _var[1/ln(x)]^2)-139/51840*Pi^(1/2)*2^(1/2)/_var[
    1/ln(x)]^2*(-1/126*_var[1/ln(x)]-1/720*_var[
    1/ln(x)]^2)+1/12*Pi^(1/2)*2^(1/2)/_var[1/ln(x)]^2*(1/120*
    _var[1/ln(x)]+221/302400*_var[1/ln(x)]^2))+1/120)],
    0,t_SERIES,[1, 3, 5],6,rational,_var[1/x],(2*Psi(1/_var[
    1/x])/_var[1/x]^(5/2)*2^(1/2)*Pi^(1/2)*exp(1/_var[
    1/ln(x)]*(2/_var[1/x]-1)+lnGAMMA(1/_var[1/x])-
    2/_var[1/ln(x)]/_var[1/x]+3/2/_var[
    1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi))+
    (Psi(1,1/_var[1/x])*2^(1/2)*Pi^(1/2)*_var[1/x]^(1/2)*
    exp(1/_var[1/ln(x)]*(2/_var[1/x]-1)+lnGAMMA(1/_var[1/x])-2/_var[1/ln(x)]/_var[1/x]+3/2/_var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*
    ln(2*Pi))+Psi(1/_var[1/x])^2*2^(1/2)*Pi^(1/2)*_var[
    1/x]^(1/2)*exp(1/_var[1/ln(x)]*(2/_var[1/x]-1)+
    lnGAMMA(1/_var[1/x])-2/_var[1/ln(x)]/_var[
    1/x]+3/2/_var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[
    1/x]-1/2*ln(2*Pi)))/_var[1/x]^4)*2^(1/2)/Pi^(1/2)/_var[
    1/x]^(1/2)/exp(1/_var[1/ln(x)]*(2/_var[1/x]-1)+
    lnGAMMA(1/_var[1/x])-2/_var[1/ln(x)]/_var[
    1/x]+3/2/_var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi))-2/_var[1/x]^4*Psi(1/_var[1/x])^2-2/_var[1/x]^3-4/_var[1/ln(x)]/_var[1/x]^3+1/_var[1/x]^2));
   unassign('s');

#end test
