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

# test124.tst: test for MultiSeries[multiseries] (no. 124)

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

   #=============== test nb 124 =====================
   Order:=3;
   TRY[compare_SERIES]("124-1",
    op([assign(s=multiseries(
    op([1/1606938044258990275541962092341162602522202993782792835301376*
    (1-cos(515377520732011331036461129765621272702107522001*x^100))/x^200, x]))),
    SERIES2Series(s)]),
    Series([2656139888758747693387813220357796268292334526533944959745749\
61739092490901302182994384699044001/
   3213876088517980551083924184682325205044405987565585670602752],1,
   rational,[0],200,integer,_var[x],(1/
   1606938044258990275541962092341162602522202993782792835301376-1/
   1606938044258990275541962092341162602522202993782792835301376*cos(
   515377520732011331036461129765621272702107522001*_var[x]^100))/
   _var[x]^200));
   unassign('s');

#end test
