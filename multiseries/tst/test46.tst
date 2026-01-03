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

# test46.tst: test for MultiSeries[multiseries] (no. 46)

#test 5

   # Normalizer:=proc(x) normal(x) end:
   TESTZERO:=proc(x) evalb(normal(x)=0) end:
   Testzero:=`limit/mrv/Testzero`:

   with(MultiSeries,multiseries);
   Order:=3:

   SERIES2Series:=proc(s)
   local news;
       if s=0 then 0
       else
       	  news:='Series'(op(subsop(1=NULL,s)));
	  if op(3,news)<>t_SERIES then news
	  else
	  	subsop(1=map(procname,op(1,news)),news)
	  fi
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

  compare_ms_SERIES:=proc(s1,s2) compare_SERIES(SERIES2Series(s1),s2) end:

   #=============== test nb 46 =====================
   Order:=4;
   TRY[compare_ms_SERIES]("46-1",
    multiseries(exp(x+exp(1/x)), x, 4),
    Series([Series([1, 1, 1/2, 1/6],1,rational,[0, 1, 2, 3],4,integer,
    _var[x],exp(_var[x]))],0,t_SERIES,[-1],infinity,integer,
    _var[1/exp(exp(1/x))],exp(_var[x])/_var[1/exp(exp(1/x))]),
    assign='s');
   scale:=op(1,s);
   closedform:=op(9,s);
   TRY[compare_ms_SERIES]("46-2",
    multiseries(closedform,scale,Order,scale[list][2..2]),
    Series([Series([exp(_var[x])],0,algebraic,[0],infinity,integer,
    _var[1/exp(1/x)],exp(_var[x]))],0,t_SERIES,[-1],infinity,
    integer,_var[1/exp(exp(1/x))],exp(_var[x])/_var[1/exp(exp(1/x))]));
   TRY[compare_ms_SERIES]("46-3",
    multiseries(closedform,scale,Order,scale[list][3..3]),
    Series([exp(_var[x])],0,algebraic,[-1],infinity,integer,
    _var[1/exp(exp(1/x))],exp(_var[x])/_var[1/exp(exp(1/x))]));
   TRY[compare_ms_SERIES]("46-4",
    multiseries(closedform,scale),
    Series([Series([Series([1, 1, 1/2, 1/6],1,rational,[0, 1, 2, 3],4,
    integer,_var[x],exp(_var[x]))],0,t_SERIES,[0],infinity,integer,
    _var[1/exp(1/x)],exp(_var[x]))],0,t_SERIES,[-1],infinity,
    integer,_var[1/exp(exp(1/x))],exp(_var[x])/_var[1/exp(exp(1/x))]));
   TRY[compare_ms_SERIES]("46-5",
    multiseries(closedform,scale,Order,exact_order),
    Series([Series([Series([1, 1, 1/2, 1/6],1,rational,[0, 1, 2, 3],4,integer,
    _var[x],exp(_var[x]))],0,t_SERIES,[0],infinity,integer,
    _var[1/exp(1/x)],exp(_var[x]))],0,t_SERIES,[-1],infinity,integer,
    _var[1/exp(exp(1/x))],exp(_var[x])/_var[1/exp(exp(1/x))]));
#end test
