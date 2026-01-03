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

# test86.tst: test for MultiSeries[multiseries] (no. 86)

#test 9

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

   #=============== test nb 86 =====================
   Order:=3;
   TRY[compare_SERIES]("86-1",
    op([assign(s=multiseries(op([x^x/((1+x)^x), x = infinity]))),SERIES2Series(s)]),
    Series([Series([1/exp(1), 1/2/exp(1)],1,algebraic,[0, 1],2,integer,
    _var[1/x],1/exp((ln(1+_var[1/x])+1/_var[1/ln(x)])/
    _var[1/x]-1/_var[1/ln(x)]/_var[1/x]-1)/exp(1))],
    0,t_SERIES,[0],infinity,integer,_var[1/exp(ln(x)*x)],
    1/exp((ln(1+_var[1/x])+1/_var[1/ln(x)])/_var[1/x]-
    1/_var[1/ln(x)]/_var[1/x]-1)/exp(1)));
   TRY[compare_SERIES]("86-2",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][1..1])),
    Series([Series([1/exp(1)],0,algebraic,[0],infinity,integer,
    _var[1/ln(x)],1/exp(1))],
    Series([],1/exp(1),algebraic,[],0,integer,_var[1/ln(x)],1/2/exp(1)),
    t_SERIES,[0],1,integer,_var[1/x],1/exp((ln(1+_var[1/x])+
    1/_var[1/ln(x)])/_var[1/x]-1/_var[1/ln(x)]/
    _var[1/x]-1)/exp(1)));
   TRY[compare_SERIES]("86-3",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][3..3])),
    Series([1/exp((ln(1+_var[1/x])+1/_var[1/ln(x)])/
    _var[1/x]-1/_var[1/ln(x)]/_var[1/x]-1)/
    exp(1)],0,algebraic,[0],infinity,integer,_var[1/exp(ln(x)*x)],
    1/exp((ln(1+_var[1/x])+1/_var[1/ln(x)])/_var[1/x]-
    1/_var[1/ln(x)]/_var[1/x]-1)/exp(1)));
   TRY[compare_SERIES]("86-4",
    SERIES2Series(multiseries(op(9,s),op(1,s))),
    Series([Series([Series([1/exp(1)],0,algebraic,[0],infinity,integer,
    _var[1/ln(x)],1/exp(1)),
    Series([1/2/exp(1)],0,algebraic,[0],infinity,integer,_var[1/ln(x)],
    1/2/exp(1))],
    Series([],1,integer,[],0,integer,_var[1/ln(x)],-7/8/exp(1)),
    t_SERIES,[0, 1],2,integer,_var[1/x],1/exp((ln(1+_var[1/x])+
    1/_var[1/ln(x)])/_var[1/x]-1/_var[1/ln(x)]/
    _var[1/x]-1)/exp(1))],0,t_SERIES,[0],infinity,integer,
    _var[1/exp(ln(x)*x)],1/exp((ln(1+_var[1/x])+
    1/_var[1/ln(x)])/_var[1/x]-1/_var[
    1/ln(x)]/_var[1/x]-1)/exp(1)));
   TRY[compare_SERIES]("86-5",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,exact_order)),
    Series([Series([Series([1/exp(1)],0,algebraic,[0],infinity,integer,
    _var[1/ln(x)],1/exp(1)),
    Series([1/2/exp(1)],0,algebraic,[0],infinity,integer,
    _var[1/ln(x)],1/2/exp(1)),
    Series([-5/24/exp(1)],0,algebraic,[0],infinity,integer,
    _var[1/ln(x)],-5/24/exp(1))],
    Series([],1,integer,[],0,integer,_var[1/ln(x)],5/48/exp(1)),
    t_SERIES,[0, 1, 2],3,integer,_var[1/x],
    1/exp((ln(1+_var[1/x])+1/_var[1/ln(x)])/
    _var[1/x]-1/_var[1/ln(x)]/_var[1/x]-1)/
    exp(1))],0,t_SERIES,[0],infinity,integer,_var[1/exp(ln(x)*x)],
    1/exp((ln(1+_var[1/x])+1/_var[1/ln(x)])/
    _var[1/x]-1/_var[1/ln(x)]/_var[1/x]-1)/exp(1)));
   unassign('s');

#end test
