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

# test109.tst: test for MultiSeries[multiseries] (no. 109)

#test 15

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

   #=============== test nb 109 =====================
   Order:=3;
   TRY[compare_SERIES]("109-1",
    op([assign(s=multiseries(op([10^x/GAMMA(x), x = infinity]))),SERIES2Series(s)]),
    Series([Series([Series([1/2*2^(1/2)/Pi^(1/2), -1/24*2^(1/2)/Pi^(1/2),
    1/576*2^(1/2)/Pi^(1/2)],1,algebraic,[-1/2, 1/2, 3/2],5/2,rational,
    _var[1/x],1/2/exp(lnGAMMA(1/_var[1/x])-
    (1/_var[1/ln(x)]-1)/_var[1/x]+1/2/_var[
    1/ln(x)]-1/2*ln(2*Pi))*2^(1/2)/Pi^(1/2)/_var[1/x]^(1/2))],
    0,t_SERIES,[-1],infinity,integer,_var[1/exp(x*ln(10))],
    1/2/exp(lnGAMMA(1/_var[1/x])-(1/_var[1/ln(x)]-1)/
    _var[1/x]+1/2/_var[1/ln(x)]-1/2*ln(2*Pi))*2^(1/2)/
    Pi^(1/2)/_var[1/x]^(1/2)/_var[1/exp(x*ln(10))])],0,
    t_SERIES,[1],infinity,integer,_var[1/exp((ln(x)-1)*x)],1/2/
    _var[1/exp(x*ln(10))]*2^(1/2)/Pi^(1/2)/_var[1/x]^(1/2)*
    _var[1/exp((ln(x)-1)*x)]/exp(lnGAMMA(1/_var[1/x])-
    (1/_var[1/ln(x)]-1)/_var[1/x]+1/2/
    _var[1/ln(x)]-1/2*ln(2*Pi))));
   TRY[compare_SERIES]("109-2",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][1..1])),
    Series([Series([Series([Series([1/2*2^(1/2)/Pi^(1/2)],0,algebraic,[0],
    infinity,rational,_var[1/ln(x)],1/2*2^(1/2)/Pi^(1/2))],
    Series([],1,integer,[],0,integer,_var[1/ln(x)],-1/12),
    t_SERIES,[-1/2],1/2,rational,_var[1/x],1/2*2^(1/2)/Pi^(1/2)/
    _var[1/x]^(1/2)/exp(lnGAMMA(1/_var[1/x])+(-1/
    _var[1/ln(x)]+1)/_var[1/x]+1/2/_var[
    1/ln(x)]-1/2*ln(2*Pi)))],0,t_SERIES,[-1],infinity,integer,_var[
    1/exp(x*ln(10))],1/2/exp(lnGAMMA(1/_var[1/x])+(-1/_var[1/ln(x)]+1)/_var[1/x]+1/2/_var[1/ln(x)]-
    1/2*ln(2*Pi))*2^(1/2)/_var[1/exp(x*ln(10))]/Pi^(1/2)/
    _var[1/x]^(1/2))],0,t_SERIES,[1],infinity,integer,_var[
    1/exp((ln(x)-1)*x)],1/2*2^(1/2)/_var[1/exp(x*ln(10))]/Pi^(1/2)/
    _var[1/x]^(1/2)*_var[1/exp((ln(x)-1)*x)]/
    exp(lnGAMMA(1/_var[1/x])+(-1/_var[1/ln(x)]+1)/
    _var[1/x]+1/2/_var[1/ln(x)]-1/2*ln(2*Pi))));
   TRY[compare_SERIES]("109-3",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][3..3])),
    Series([Series([1/2*2^(1/2)/Pi^(1/2)/_var[1/x]^(1/2)/
    exp((1/_var[1/x]-1/2)/_var[1/ln(x)]+
    lnGAMMA(1/_var[1/x])-1/_var[1/ln(x)]/
    _var[1/x]+1/_var[1/ln(x)]+(-1/_var[
    1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi))],0,algebraic,[-1],
    infinity,rational,_var[1/exp(x*ln(10))],1/2/exp((1/_var[1/x]-1/2)/_var[1/ln(x)]+lnGAMMA(1/_var[1/x])-1/_var[1/ln(x)]/_var[1/x]+1/_var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[
    1/x]-1/2*ln(2*Pi))*2^(1/2)/_var[1/exp(x*ln(10))]/Pi^(1/2)/
    _var[1/x]^(1/2))],0,t_SERIES,[1],infinity,integer,
    _var[1/exp((ln(x)-1)*x)],1/2*2^(1/2)/_var[
    1/exp(x*ln(10))]/Pi^(1/2)/_var[1/x]^(1/2)*_var[
    1/exp((ln(x)-1)*x)]/exp((1/_var[1/x]-1/2)/_var[
    1/ln(x)]+lnGAMMA(1/_var[1/x])-1/_var[1/ln(x)]/
    _var[1/x]+1/_var[1/ln(x)]+(-1/_var[
    1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi))));
   TRY[compare_SERIES]("109-4",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][4..4])),
    Series([1/2/exp(1/_var[1/ln(x)]*(2/_var[1/x]-1)+
    (1/_var[1/x]-1/2)/_var[1/ln(x)]+lnGAMMA(1/
    _var[1/x])-3/_var[1/ln(x)]/_var[1/x]+
    2/_var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi))/_var[1/x]^(1/2)/Pi^(1/2)/
    _var[1/exp(x*ln(10))]*2^(1/2)],0,algebraic,[1],infinity,
    rational,_var[1/exp((ln(x)-1)*x)],1/2/_var[
    1/x]^(1/2)/Pi^(1/2)/_var[1/exp(x*ln(10))]*2^(1/2)/
    exp(1/_var[1/ln(x)]*(2/_var[1/x]-1)+
    (1/_var[1/x]-1/2)/_var[1/ln(x)]+lnGAMMA(1/
    _var[1/x])-3/_var[1/ln(x)]/_var[1/x]+
    2/_var[1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[
    1/x]-1/2*ln(2*Pi))*_var[1/exp((ln(x)-1)*x)]));
   TRY[compare_SERIES]("109-5",
    SERIES2Series(multiseries(op(9,s),op(1,s))),
    Series([Series([Series([Series([1/2*2^(1/2)/Pi^(1/2)],0,algebraic,[0],
    infinity,rational,_var[1/ln(x)],1/2*2^(1/2)/Pi^(1/2)),
    Series([-1/24*2^(1/2)/Pi^(1/2)],0,algebraic,[0],infinity,rational,
    _var[1/ln(x)],-1/24*2^(1/2)/Pi^(1/2)),
    Series([1/576*2^(1/2)/Pi^(1/2)],0,algebraic,[0],infinity,rational,
    _var[1/ln(x)],1/576*2^(1/2)/Pi^(1/2))],
    Series([],1,integer,[],0,integer,_var[1/ln(x)],-10369/10368),
    t_SERIES,[-1/2, 1/2, 3/2],5/2,rational,_var[1/x],1/2*2^(1/2)/
    Pi^(1/2)/_var[1/x]^(1/2)/exp(3/_var[1/ln(x)]*
    (2/_var[1/x]-1)+lnGAMMA(1/_var[1/x])-6/_var[1/ln(x)]/_var[1/x]+7/2/_var[1/ln(x)]+
    (-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi)))],
    0,t_SERIES,[-1],infinity,rational,_var[1/exp(x*ln(10))],
    1/2/Pi^(1/2)/_var[1/x]^(1/2)/exp(3/_var[1/ln(x)]*
    (2/_var[1/x]-1)+lnGAMMA(1/_var[1/x])-6/_var[1/ln(x)]/_var[1/x]+7/2/_var[
    1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi))/
    _var[1/exp(x*ln(10))]*2^(1/2))],0,t_SERIES,[1],infinity,rational,
    _var[1/exp((ln(x)-1)*x)],1/2/exp(2/_var[1/ln(x)]*
    (2/_var[1/x]-1)+(1/_var[1/x]-1/2)/_var[
    1/ln(x)]+lnGAMMA(1/_var[1/x])-5/_var[1/ln(x)]/
    _var[1/x]+3/_var[1/ln(x)]+(-1/_var[
    1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi))/_var[1/x]^(1/2)/
    Pi^(1/2)/_var[1/exp(x*ln(10))]*2^(1/2)*
    _var[1/exp((ln(x)-1)*x)]));
   TRY[compare_SERIES]("109-6",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,exact_order)),
    Series([Series([Series([Series([1/2*2^(1/2)/Pi^(1/2)],0,algebraic,
    [0],infinity,rational,_var[1/ln(x)],1/2*2^(1/2)/Pi^(1/2)),
    Series([-1/24*2^(1/2)/Pi^(1/2)],0,algebraic,[0],infinity,rational,
    _var[1/ln(x)],-1/24*2^(1/2)/Pi^(1/2)),
    Series([1/576*2^(1/2)/Pi^(1/2)],0,algebraic,[0],infinity,rational,
    _var[1/ln(x)],1/576*2^(1/2)/Pi^(1/2))],
    Series([],1,integer,[],0,rational,_var[1/ln(x)],
    -10369/10368*2^(1/2)/Pi^(1/2)),t_SERIES,[-1/2, 1/2, 3/2],5/2,
    rational,_var[1/x],1/2/exp(4/_var[1/ln(x)]*
    (2/_var[1/x]-1)+lnGAMMA(1/_var[1/x])-
    8/_var[1/ln(x)]/_var[1/x]+9/2/_var[
    1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[1/x]-
    1/2*ln(2*Pi))*2^(1/2)/Pi^(1/2)/_var[1/x]^(1/2))],0,
    t_SERIES,[-1],infinity,rational,_var[1/exp(x*ln(10))],
    1/2*2^(1/2)/Pi^(1/2)/_var[1/x]^(1/2)/exp(4/_var[
    1/ln(x)]*(2/_var[1/x]-1)+lnGAMMA(1/_var[1/x])-
    8/_var[1/ln(x)]/_var[1/x]+9/2/_var[
    1/ln(x)]+(-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi))/
    _var[1/exp(x*ln(10))])],0,t_SERIES,[1],infinity,rational,
    _var[1/exp((ln(x)-1)*x)],1/2*2^(1/2)/_var[
    1/exp(x*ln(10))]/Pi^(1/2)/_var[1/x]^(1/2)/exp(3/
    _var[1/ln(x)]*(2/_var[1/x]-1)+(1/_var[1/x]-
    1/2)/_var[1/ln(x)]+lnGAMMA(1/_var[1/x])-
    7/_var[1/ln(x)]/_var[1/x]+4/_var[1/ln(x)]+
    (-1/_var[1/ln(x)]+1)/_var[1/x]-1/2*ln(2*Pi))*
    _var[1/exp((ln(x)-1)*x)]));
   unassign('s');

#end test
