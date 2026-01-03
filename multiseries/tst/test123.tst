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

# test123.tst: test for MultiSeries[multiseries] (no. 123)

#test 20

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

   #=============== test nb 123 =====================
   Order:=3;
   TRY[compare_SERIES]("123-1",
    op([assign(s=multiseries(op([exp(exp(x))*(exp(sin(1/x+
    1/exp(exp(x))))-exp(sin(1/x))), x = infinity]))),
    SERIES2Series(s)]),
    Series([Series([Series([1, 1, -1/2],1,rational,[0, 1, 3],4,
    integer,_var[1/x],cos(_var[1/x])*
    exp(sin(_var[1/x])))],0,t_SERIES,[0],infinity,
    integer,_var[1/exp(x)],cos(_var[1/x])*
    exp(sin(_var[1/x])))],
    Series([],Series([1/2, -3/4],1,rational,[0, 2],3,integer,
    _var[1/x],(-1/2*sin(_var[1/x])+
    1/2*cos(_var[1/x])^2)*exp(sin(_var[
    1/x]))),t_SERIES,[],0,integer,_var[1/exp(x)],
    (-1/2*sin(_var[1/x])+1/2*cos(_var[
    1/x])^2)*exp(sin(_var[1/x]))),t_SERIES,[0],
    1,integer,_var[1/exp(exp(x))],(exp(sin(_var[1/x]))*exp(sin(_var[1/x]+_var[
    1/exp(exp(x))])-sin(_var[1/x]))-exp(sin(_var[1/x])))/_var[1/exp(exp(x))]));
   TRY[compare_SERIES]("123-2",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][2..2])),
    Series([Series([cos(_var[1/x])*exp(sin(_var[1/x]))],0,algebraic,[0],infinity,integer,_var[1/exp(x)],cos(_var[1/x])*exp(sin(_var[1/x])))],
    Series([],1,algebraic,[],0,integer,_var[1/exp(x)],
    (-1/2*sin(_var[1/x])+1/2*cos(_var[1/x])^2)*
    exp(sin(_var[1/x]))),t_SERIES,[0],1,integer,_var[1/exp(exp(x))],(exp(sin(_var[1/x]))*exp(
    sin(_var[1/x]+_var[1/exp(exp(x))])-sin(_var[1/x]))-exp(sin(_var[1/x])))/
    _var[1/exp(exp(x))]));
   TRY[compare_SERIES]("123-3",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][3..3])),
    Series([cos(_var[1/x])*exp(sin(_var[1/x])),
    (-1/2*sin(_var[1/x])+1/2*cos(_var[1/x])^2)*
    exp(sin(_var[1/x]))],1,algebraic,[0, 1],2,integer,
    _var[1/exp(exp(x))],(exp(sin(_var[1/x]))*
    exp(sin(_var[1/x]+_var[1/exp(exp(x))])-
    sin(_var[1/x]))-exp(sin(_var[1/x])))/
    _var[1/exp(exp(x))]));
   TRY[compare_SERIES]("123-4",
    SERIES2Series(multiseries(op(9,s),op(1,s))),
    Series([Series([Series([1, 1, -1/2],1,rational,[0, 1, 3],4,
    integer,_var[1/x],cos(_var[1/x])*
    exp(sin(_var[1/x])))],0,t_SERIES,[0],infinity,
    integer,_var[1/exp(x)],cos(_var[1/x])*
    exp(sin(_var[1/x]))),
    Series([Series([1/2, -3/4],1,rational,[0, 2],3,integer,
    _var[1/x],(-1/2*sin(_var[1/x])+1/2*
    cos(_var[1/x])^2)*exp(sin(_var[1/x])))],
    0,t_SERIES,[0],infinity,integer,_var[1/exp(x)],
    (-1/2*sin(_var[1/x])+1/2*cos(_var[1/x])^2)*
    exp(sin(_var[1/x])))],
    Series([],Series([],1,integer,[],0,integer,_var[1/x],
    1-1/2*sin(_var[1/x])*cos(_var[1/x])+
    1/6*cos(_var[1/x])^3),t_SERIES,[],0,integer,
    _var[1/exp(x)],1-1/2*sin(_var[1/x])*
    cos(_var[1/x])+1/6*cos(_var[1/x])^3),
    t_SERIES,[0, 1],2,integer,_var[1/exp(exp(x))],
    (exp(sin(_var[1/x]))*exp(sin(_var[1/x]+
    _var[1/exp(exp(x))])-sin(_var[1/x]))-
    exp(sin(_var[1/x])))/_var[1/exp(exp(x))]));
   TRY[compare_SERIES]("123-5",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,exact_order)),
    Series([Series([Series([1, 1, -1/2],1,rational,[0, 1, 3],4,
    integer,_var[1/x],cos(_var[1/x])*
    exp(sin(_var[1/x])))],0,t_SERIES,[0],infinity,
    integer,_var[1/exp(x)],cos(_var[1/x])*
    exp(sin(_var[1/x]))),
    Series([Series([1/2, -3/4, -2/3],1,rational,[0, 2, 3],4,
    integer,_var[1/x],(-1/2*sin(_var[
    1/x])+1/2*cos(_var[1/x])^2)*exp(sin(_var[1/x])))],0,t_SERIES,[0],infinity,integer,_var[1/exp(x)],(-1/2*sin(_var[1/x])+1/2*
    cos(_var[1/x])^2)*exp(sin(_var[1/x]))),
    Series([Series([-1/2, -2/3, -1/12],1,rational,[1, 2, 3],4,
    integer,_var[1/x],(-1/6*cos(_var[1/x])-
    1/2*sin(_var[1/x])*cos(_var[1/x])+
    1/6*cos(_var[1/x])^3)*exp(sin(_var[1/x])))],
    0,t_SERIES,[0],infinity,integer,_var[1/exp(x)],
    (-1/6*cos(_var[1/x])-1/2*sin(_var[1/x])*
    cos(_var[1/x])+1/6*cos(_var[1/x])^3)*
    exp(sin(_var[1/x])))],
    Series([],Series([],1,integer,[],0,integer,_var[1/x],
    (1/24*sin(_var[1/x])-1/6*cos(_var[1/x])^2+
    1/8*sin(_var[1/x])^2-1/4*sin(_var[1/x])*
    cos(_var[1/x])^2+1/24*cos(_var[1/x])^4)*
    exp(sin(_var[1/x]))),t_SERIES,[],0,integer,
    _var[1/exp(x)],(1/24*sin(_var[1/x])-
    1/6*cos(_var[1/x])^2+1/8*sin(_var[1/x])^2-
    1/4*sin(_var[1/x])*cos(_var[1/x])^2+
    1/24*cos(_var[1/x])^4)*exp(sin(_var[1/x]))),
    t_SERIES,[0, 1, 2],3,integer,_var[1/exp(exp(x))],
    (exp(sin(_var[1/x]))*exp(sin(_var[1/x]+
    _var[1/exp(exp(x))])-sin(_var[1/x]))-
    exp(sin(_var[1/x])))/_var[1/exp(exp(x))]));
   unassign('s');

#end test
