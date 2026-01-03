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

# test83.tst: test for MultiSeries[multiseries] (no. 83)

#test 5

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

   #=============== test nb 83 =====================
   Order:=2;
   TRY[compare_SERIES]("83-1",
    op([assign(s=multiseries(op([exp(x+ln(x)), x, 2]))),SERIES2Series(s)]),
    Series([1, 1],1,integer,[1, 2],3,integer,_var[x],exp(_var[x])*
    _var[x]));
   TRY[compare_SERIES]("83-2",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][1..1])),
    Series([Series([1],0,integer,[0],infinity,integer,_var[1/ln(1/x)],1)],
    Series([],1,integer,[],0,integer,_var[1/ln(1/x)],1),t_SERIES,[1],2,
    integer,_var[x],exp(_var[x])*_var[x]));
   TRY[compare_SERIES]("83-3",
    SERIES2Series(multiseries(op(9,s),op(1,s))),
    Series([Series([1],0,integer,[0],infinity,integer,_var[1/ln(1/x)],1),
    Series([1],0,integer,[0],infinity,integer,_var[1/ln(1/x)],1)],
    Series([],1,integer,[],0,integer,_var[1/ln(1/x)],1),t_SERIES,[1, 2],3,
    integer,_var[x],exp(_var[x])*_var[x]));
   TRY[compare_SERIES]("83-4",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,exact_order)),
    Series([Series([1],0,integer,[0],infinity,integer,_var[1/ln(1/x)],1),
    Series([1],0,integer,[0],infinity,integer,_var[1/ln(1/x)],1)],
    Series([],1,integer,[],0,integer,_var[1/ln(1/x)],1),t_SERIES,
    [1, 2],3,integer,_var[x],exp(_var[x])*_var[x]));
   unassign('s');

#end test
