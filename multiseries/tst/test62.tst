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

# test62.tst: test for MultiSeries[multiseries] (no. 62)

#test 50

   # Normalizer:=proc(x) normal(x) end:
#   TESTZERO:=proc(x) evalb(normal(x)=0) end:
#   Testzero:=`limit/mrv/Testzero`:

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

   #=============== test nb 62 =====================
   Order:=4;
   TRY[compare_SERIES]("62-1",
    op([assign(s=multiseries(op([exp(exp(-x/(1+exp(-x))))^2*exp(-x/(1+
    exp(-x/(1+exp(-x)))))/exp(-x/(1+exp(-x)))^2-exp(x)+x, x = infinity, 4]))),
    SERIES2Series(s)]),
    Series([Series([2],0,integer,[0],infinity,integer,_var[1/x],2)],
    Series([3/2, 1],1,rational,[-2, -1],0,integer,_var[1/x],6/
    _var[1/x]+5/2/_var[1/x]^2-2/_var[1/x]*
    (1/_var[1/x]+2)+(1/_var[1/x]-1)/_var[1/x]+2),
    t_SERIES,[0],1,integer,_var[1/exp(x)],exp(exp(-1/_var[1/x]/
    (1+_var[1/exp(x)])+1/_var[1/x])*_var[1/exp(x)])^2*
    exp(-1/_var[1/x]/(1+exp(-1/_var[1/x]/(1+_var[1/
    exp(x)])+1/_var[1/x])*_var[1/exp(x)])+1/_var[1/x])/
    _var[1/exp(x)]/exp(-1/_var[1/x]/(1+_var[1/exp(x)])+
    1/_var[1/x])^2-1/_var[1/exp(x)]+1/_var[1/x]));
   TRY[compare_SERIES]("62-2",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][2..2])),
    Series([2, 6/_var[1/x]+5/2/_var[1/x]^2-2/
    _var[1/x]*(1/_var[1/x]+2)-(-1/_var[1/x]+1)/
    _var[1/x]+2, 4/_var[1/x]^2-25/6/_var[1/x]^3+
    6*(-1/_var[1/x]+1/2/_var[1/x]^2)/_var[1/x]+
    (2/_var[1/x]+2/_var[1/x]^2)*(1/_var[1/x]+2)-
    2/_var[1/x]*(-(-1/_var[1/x]+1)/_var[1/x]+1/
    2/_var[1/x]^2+4/_var[1/x]+2)-(3/_var[1/x]-1/2/
    _var[1/x]^2-1)/_var[1/x]-(-1/_var[1/x]+1)/
    _var[1/x]^2-2*(-1/_var[1/x]+1)/_var[1/x]+1/
    _var[1/x]*(2/_var[1/x]+2)+4/3],1,algebraic,[0, 1, 2],3,
    integer,_var[1/exp(x)],exp(exp(-1/_var[1/x]/(1+
    _var[1/exp(x)])+1/_var[1/x])*_var[1/exp(x)])^2*
    exp(-1/_var[1/x]/(1+exp(-1/_var[1/x]/(1+_var[
    1/exp(x)])+1/_var[1/x])*_var[1/exp(x)])+1/_var[
    1/x])/_var[1/exp(x)]/exp(-1/_var[1/x]/(1+_var[
    1/exp(x)])+1/_var[1/x])^2-1/_var[1/exp(x)]+
    1/_var[1/x]));
   TRY[compare_SERIES]("62-3",
    SERIES2Series(multiseries(op(9,s),op(1,s))),
    Series([Series([2],0,integer,[0],infinity,integer,_var[1/x],2),
    Series([3/2, 1, 2],0,rational,[-2, -1, 0],infinity,integer,_var[1/x],
    6/_var[1/x]+5/2/_var[1/x]^2-2/_var[1/x]*
    (1/_var[1/x]+2)+(1/_var[1/x]-1)/_var[1/x]+2),
    Series([-2/3, -2, 1, 4/3],0,rational,[-3, -2, -1, 0],infinity,integer,
    _var[1/x],4/_var[1/x]^2-25/6/_var[1/x]^3+
    (-6/_var[1/x]+3/_var[1/x]^2)/_var[1/x]+
    (2/_var[1/x]+2/_var[1/x]^2)*(1/_var[1/x]+2)-
    2*((1/_var[1/x]-1)/_var[1/x]+1/2/_var[1/x]^2+
    4/_var[1/x]+2)/_var[1/x]+(-3/_var[1/x]+
    1/2/_var[1/x]^2+1)/_var[1/x]+(1/_var[1/x]-1)/
    _var[1/x]^2+(2/_var[1/x]-2)/_var[1/x]+
    1/_var[1/x]*(2/_var[1/x]+2)+4/3)],
    Series([],1,integer,[],0,integer,_var[1/x],1),t_SERIES,[0, 1, 2],3,
    integer,_var[1/exp(x)],exp(exp(-1/_var[1/x]/
    (1+_var[1/exp(x)])+1/_var[1/x])*
    _var[1/exp(x)])^2*exp(-1/_var[1/x]/(1+exp(-1/
    _var[1/x]/(1+_var[1/exp(x)])+1/_var[1/x])*
    _var[1/exp(x)])+1/_var[1/x])/_var[1/exp(x)]/
    exp(-1/_var[1/x]/(1+_var[1/exp(x)])+1/_var[1/x])^2-
    1/_var[1/exp(x)]+1/_var[1/x]));
   TRY[compare_SERIES]("62-4",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,exact_order)),
    Series([Series([2],0,integer,[0],infinity,integer,_var[1/x],2),
    Series([3/2, 1, 2],0,rational,[-2, -1, 0],infinity,integer,_var[1/x],
    6/_var[1/x]+5/2/_var[1/x]^2-2/_var[1/x]*
    (1/_var[1/x]+2)+(1/_var[1/x]-1)/_var[1/x]+2),
    Series([-2/3, -2, 1, 4/3],0,rational,[-3, -2, -1, 0],infinity,integer,
    _var[1/x],4/_var[1/x]^2-25/6/_var[1/x]^3+
    (-6/_var[1/x]+3/_var[1/x]^2)/_var[1/x]+
    (2/_var[1/x]+2/_var[1/x]^2)*(1/_var[1/x]+2)-
    2*((1/_var[1/x]-1)/_var[1/x]+1/2/_var[1/x]^2+
    4/_var[1/x]+2)/_var[1/x]+(-3/_var[1/x]+
    1/2/_var[1/x]^2+1)/_var[1/x]+(1/_var[1/x]-1)/
    _var[1/x]^2+(2/_var[1/x]-2)/_var[1/x]+
    1/_var[1/x]*(2/_var[1/x]+2)+4/3),
    Series([17/24, 5/2, 9/2, 5/3],1,rational,[-4, -3, -2, -1],0,integer,
    _var[1/x],3/_var[1/x]-2/_var[1/x]^2+
    5/3/_var[1/x]^3+119/24/_var[1/x]^4+(6/_var[1/x]-
    6/_var[1/x]^2+1/_var[1/x]^3)/_var[1/x]+
    3*(-1/_var[1/x]+1/2/_var[1/x]^2)^2+(12/_var[1/x]-
    6/_var[1/x]^2)/_var[1/x]^2+(1/_var[1/x]+2)*
    (-2/_var[1/x]+2/_var[1/x]^2-13/3/_var[1/x]^3+
    (-6/_var[1/x]+3/_var[1/x]^2)/_var[1/x])+
    ((1/_var[1/x]-1)/_var[1/x]+1/2/_var[1/x]^2+
    4/_var[1/x]+2)*(2/_var[1/x]+2/_var[1/x]^2)-
    2*((-3/_var[1/x]+1/2/_var[1/x]^2+1)/_var[1/x]+
    (1/_var[1/x]-1)/_var[1/x]^2+1/6/_var[1/x]^3+
    (2/_var[1/x]-2)/_var[1/x]+2/_var[1/x]^2+
    1/_var[1/x]*(2/_var[1/x]+2)+4/3+2/_var[1/x])/
    _var[1/x]+(6/_var[1/x]-3/_var[1/x]^2+1/6/
    _var[1/x]^3-1)/_var[1/x]+(-3/_var[1/x]+1/2/
    _var[1/x]^2+1)/_var[1/x]^2+1/2*(-1/_var[1/x]+1)^2/
    _var[1/x]^2+(1/2/_var[1/x]-1/2)/_var[1/x]^3+
    (-6/_var[1/x]+1/_var[1/x]^2+2)/_var[1/x]+
    (2/_var[1/x]-2)/_var[1/x]^2+(2/_var[1/x]+2)*
    ((1/_var[1/x]-1)/_var[1/x]+1/2/_var[1/x]^2)+
    1/_var[1/x]*(1/_var[1/x]^2+4/3+2/_var[1/x])+
    5/12+(1/_var[1/x]+1/2)^2)],
    Series([],1,integer,[],-5,integer,_var[1/x],1/10-2*((6/_var[
    1/x]-3/_var[1/x]^2+1/6/_var[1/x]^3-1)/_var[1/x]+
    (-3/_var[1/x]+1/2/_var[1/x]^2+1)/_var[1/x]^2+1/2*
    (-1/_var[1/x]+1)^2/_var[1/x]^2+(1/2/_var[1/x]-1/2)/
    _var[1/x]^3+1/24/_var[1/x]^4+(-6/_var[1/x]+
    1/_var[1/x]^2+2)/_var[1/x]+(2/_var[1/x]-2)/
    _var[1/x]^2+2/3/_var[1/x]^3+(2/_var[1/x]+2)*
    ((1/_var[1/x]-1)/_var[1/x]+1/2/_var[1/x]^2)+
    1/_var[1/x]*(1/_var[1/x]^2+4/3+2/_var[1/x])+
    1/_var[1/x]+5/12+1/_var[1/x]^2+(1/_var[1/x]+1/2)^2)/
    _var[1/x]+(12/_var[1/x]-6/_var[1/x]^2+1/3/
    _var[1/x]^3-2)/_var[1/x]+(1/_var[1/x]+1/2)*
    (1/_var[1/x]^2+1/3)-8*(-1/_var[1/x]+1/2/_var[1/x]^2)^2/
    _var[1/x]+(3/_var[1/x]-1/2/_var[1/x]^2-1)/
    _var[1/x]^2*(-1/_var[1/x]+1)+1/3*(-1/_var[1/x]+1)^2/
    _var[1/x]^3+(1/_var[1/x]-1)/_var[1/x]^3+
    (2/_var[1/x]+2)*((-3/_var[1/x]+1/2/_var[1/x]^2+1)/
    _var[1/x]+(1/_var[1/x]-1)/_var[1/x]^2+1/6/
    _var[1/x]^3)+(1/_var[1/x]^2+4/3+2/_var[1/x])*
    ((1/_var[1/x]-1)/_var[1/x]+1/2/_var[1/x]^2)+
    (2/_var[1/x]-3/_var[1/x]^2+1/_var[1/x]^3+
    59/12/_var[1/x]^4+(6/_var[1/x]-6/_var[1/x]^2+
    1/_var[1/x]^3)/_var[1/x]+3*(-1/_var[1/x]+1/2/
    _var[1/x]^2)^2+(12/_var[1/x]-6/_var[1/x]^2)/
    _var[1/x]^2)*(1/_var[1/x]+2)+(-6/_var[1/x]+9/
    _var[1/x]^2-3/_var[1/x]^3+1/4/_var[1/x]^4)/
    _var[1/x]+(-1/_var[1/x]+1/2/_var[1/x]^2)*
    (6/_var[1/x]-6/_var[1/x]^2+1/_var[1/x]^3)+
    (-4/_var[1/x]+4/_var[1/x]^2-2/3/_var[1/x]^3)/
    _var[1/x]^2+(-6/_var[1/x]+1/_var[1/x]^2+2)/
    _var[1/x]^2+(-20/_var[1/x]+10/_var[1/x]^2)/
    _var[1/x]^3+(-2/_var[1/x]+2/_var[1/x]^2-
    13/3/_var[1/x]^3+(-6/_var[1/x]+3/_var[1/x]^2)/
    _var[1/x])*((1/_var[1/x]-1)/_var[1/x]+1/2/
    _var[1/x]^2+4/_var[1/x]+2)+(-10/_var[1/x]+
    8/_var[1/x]^2-5/6/_var[1/x]^3+1/24/_var[1/x]^4-
    (-2/_var[1/x]+1/_var[1/x]^2)/_var[1/x]+1)/
    _var[1/x]+(6/_var[1/x]-3/_var[1/x]^2+1/6/
    _var[1/x]^3-1)/_var[1/x]^2-721/120/_var[1/x]^5+
    (-1/_var[1/x]+1)^2/_var[1/x]^2+(-1/2/_var[1/x]+
    1/12/_var[1/x]^2+1/6)/_var[1/x]^3+(1/6/_var[1/x]-
    1/6)/_var[1/x]^4-4*((2/_var[1/x]-2/_var[1/x]^2+
    1/3/_var[1/x]^3)/_var[1/x]+(-1/_var[1/x]+
    1/2/_var[1/x]^2)^2)/_var[1/x]-5/3/_var[1/x]+
    13/2/_var[1/x]^2-7/3/_var[1/x]^3+1/2/_var[1/x]^4+
    (-2/_var[1/x]+1/_var[1/x]^2)/_var[1/x]+
    1/_var[1/x]*(1/3/_var[1/x]^3+1/_var[1/x]+
    5/12+1/_var[1/x]^2+(1/_var[1/x]+1/2)^2)+1/6*
    ((-6/_var[1/x]+1/_var[1/x]^2+2)/_var[1/x]^2+
    (-1/_var[1/x]+1)^2/_var[1/x]^2)/_var[1/x]+
    (2/_var[1/x]+2/_var[1/x]^2)*((-3/_var[1/x]+
    1/2/_var[1/x]^2+1)/_var[1/x]+(1/_var[1/x]-1)/
    _var[1/x]^2+1/6/_var[1/x]^3+(2/_var[1/x]-2)/
    _var[1/x]+2/_var[1/x]^2+1/_var[1/x]*
    (2/_var[1/x]+2)+4/3+2/_var[1/x])),t_SERIES,[0, 1, 2, 3],
    4,integer,_var[1/exp(x)],exp(exp(-1/_var[1/x]/
    (1+_var[1/exp(x)])+1/_var[1/x])*
    _var[1/exp(x)])^2*exp(-1/_var[1/x]/(1+exp(-1/
    _var[1/x]/(1+_var[1/exp(x)])+1/_var[1/x])*
    _var[1/exp(x)])+1/_var[1/x])/_var[1/exp(x)]/
    exp(-1/_var[1/x]/(1+_var[1/exp(x)])+1/_var[1/x])^2-
    1/_var[1/exp(x)]+1/_var[1/x]));
   unassign('s');

#end test
