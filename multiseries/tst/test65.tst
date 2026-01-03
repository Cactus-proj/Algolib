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

# test65.tst: test for MultiSeries[multiseries] (no. 65)

#test 32

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

   #=============== test nb 65 =====================
   Order:=3;
   TRY[compare_SERIES]("65-1",
    op([assign(s=multiseries(op([exp(1/(x^3+x^4)^7), x]))),SERIES2Series(s)]),
    Series([Series([exp(-296010)],1,algebraic,[0],1,integer,_var[x],
    exp(1/(_var[x]^3+_var[x]^4)^7-1/_var[x]^21+7/_var[x]^20-
    28/_var[x]^19+84/_var[x]^18-210/_var[x]^17+462/_var[x]^16-
    924/_var[x]^15+1716/_var[x]^14-3003/_var[x]^13+5005/_var[x]^12-
    8008/_var[x]^11+12376/_var[x]^10-18564/_var[x]^9+27132/_var[x]^8-
    38760/_var[x]^7+54264/_var[x]^6-74613/_var[x]^5+100947/_var[x]^4-
    134596/_var[x]^3+177100/_var[x]^2-230230/_var[x]+296010)*
    exp(-296010))],0,t_SERIES,[-1],infinity,integer,_var[1/exp(1/x^21)/
    exp(-7/x^20)/exp(28/x^19)/exp(-84/x^18)/exp(210/x^17)/exp(-462/x^16)/
    exp(924/x^15)/exp(-1716/x^14)/exp(3003/x^13)/exp(-5005/x^12)/exp(8008/x^11)/
    exp(-12376/x^10)/exp(18564/x^9)/exp(-27132/x^8)/exp(38760/x^7)/exp(-54264/x^6)/
    exp(74613/x^5)/exp(-100947/x^4)/exp(134596/x^3)/exp(-177100/x^2)/exp(230230/x)],
    exp(-296010)/_var[1/exp(1/x^21)/exp(-7/x^20)/exp(28/x^19)/exp(-84/x^18)/
    exp(210/x^17)/exp(-462/x^16)/exp(924/x^15)/exp(-1716/x^14)/exp(3003/x^13)/
    exp(-5005/x^12)/exp(8008/x^11)/exp(-12376/x^10)/exp(18564/x^9)/exp(-27132/x^8)/
    exp(38760/x^7)/exp(-54264/x^6)/exp(74613/x^5)/exp(-100947/x^4)/exp(134596/x^3)/
    exp(-177100/x^2)/exp(230230/x)]*exp(1/(_var[x]^3+_var[x]^4)^7-1/
    _var[x]^21+7/_var[x]^20-28/_var[x]^19+84/_var[x]^18-210/
    _var[x]^17+462/_var[x]^16-924/_var[x]^15+1716/_var[x]^14-3003/
    _var[x]^13+5005/_var[x]^12-8008/_var[x]^11+12376/_var[x]^10-
    18564/_var[x]^9+27132/_var[x]^8-38760/_var[x]^7+54264/_var[x]^6-
    74613/_var[x]^5+100947/_var[x]^4-134596/_var[x]^3+177100/
    _var[x]^2-230230/_var[x]+296010)));
   TRY[compare_SERIES]("65-2",SERIES2Series(multiseries(op(9,s),op(1,s),
    Order,op(1,s)[list][2..2])),
    Series([exp(1/(_var[x]^3+_var[x]^4)^7-1/_var[x]^21+7/_var[x]^20-
    28/_var[x]^19+84/_var[x]^18-210/_var[x]^17+462/_var[x]^16-924/
    _var[x]^15+1716/_var[x]^14-3003/_var[x]^13+5005/_var[x]^12-
    8008/_var[x]^11+12376/_var[x]^10-18564/_var[x]^9+27132/_var[x]^8-
    38760/_var[x]^7+54264/_var[x]^6-74613/_var[x]^5+100947/_var[x]^4-
    134596/_var[x]^3+177100/_var[x]^2-230230/_var[x]+296010)*exp(-296010)],
    0,algebraic,[-1],infinity,integer,_var[1/exp(1/x^21)/exp(-7/x^20)/exp(28/x^19)/
    exp(-84/x^18)/exp(210/x^17)/exp(-462/x^16)/exp(924/x^15)/exp(-1716/x^14)/exp(3003/
    x^13)/exp(-5005/x^12)/exp(8008/x^11)/exp(-12376/x^10)/exp(18564/x^9)/exp(-27132/x^8)/
    exp(38760/x^7)/exp(-54264/x^6)/exp(74613/x^5)/exp(-100947/x^4)/exp(134596/x^3)/
    exp(-177100/x^2)/exp(230230/x)],exp(-296010)/_var[1/exp(1/x^21)/exp(-7/x^20)/
    exp(28/x^19)/exp(-84/x^18)/exp(210/x^17)/exp(-462/x^16)/exp(924/x^15)/
    exp(-1716/x^14)/exp(3003/x^13)/exp(-5005/x^12)/exp(8008/x^11)/exp(-12376/x^10)/
    exp(18564/x^9)/exp(-27132/x^8)/exp(38760/x^7)/exp(-54264/x^6)/exp(74613/x^5)/
    exp(-100947/x^4)/exp(134596/x^3)/exp(-177100/x^2)/exp(230230/x)]*
    exp(1/(_var[x]^3+_var[x]^4)^7-1/_var[x]^21+7/_var[x]^20-
    28/_var[x]^19+84/_var[x]^18-210/_var[x]^17+462/_var[x]^16-
    924/_var[x]^15+1716/_var[x]^14-3003/_var[x]^13+5005/_var[x]^12-
    8008/_var[x]^11+12376/_var[x]^10-18564/_var[x]^9+27132/_var[x]^8-
    38760/_var[x]^7+54264/_var[x]^6-74613/_var[x]^5+100947/_var[x]^4-
    134596/_var[x]^3+177100/_var[x]^2-230230/_var[x]+296010)));
   TRY[compare_SERIES]("65-3",
member(
    SERIES2Series(multiseries(op(9,s),op(1,s))),
[
    Series([Series([exp(-296010)],1,algebraic,[0],1,integer,_var[x],
    exp(1/(_var[x]^3+_var[x]^4)^7-1/_var[x]^21+7/_var[x]^20-
    28/_var[x]^19+84/_var[x]^18-210/_var[x]^17+462/_var[x]^16-
    924/_var[x]^15+1716/_var[x]^14-3003/_var[x]^13+5005/_var[x]^12-
    8008/_var[x]^11+12376/_var[x]^10-18564/_var[x]^9+27132/
    _var[x]^8-38760/_var[x]^7+54264/_var[x]^6-74613/_var[x]^5+
    100947/_var[x]^4-134596/_var[x]^3+177100/_var[x]^2-230230/
    _var[x]+296010)*exp(-296010))],0,t_SERIES,[-1],infinity,integer,
    _var[1/exp(1/x^21)/exp(-7/x^20)/exp(28/x^19)/exp(-84/x^18)/exp(210/x^17)/
    exp(-462/x^16)/exp(924/x^15)/exp(-1716/x^14)/exp(3003/x^13)/exp(-5005/x^12)/
    exp(8008/x^11)/exp(-12376/x^10)/exp(18564/x^9)/exp(-27132/x^8)/exp(38760/x^7)/
    exp(-54264/x^6)/exp(74613/x^5)/exp(-100947/x^4)/exp(134596/x^3)/exp(-177100/x^2)/
    exp(230230/x)],exp(-296010)/_var[1/exp(1/x^21)/exp(-7/x^20)/exp(28/x^19)/
    exp(-84/x^18)/exp(210/x^17)/exp(-462/x^16)/exp(924/x^15)/exp(-1716/x^14)/
    exp(3003/x^13)/exp(-5005/x^12)/exp(8008/x^11)/exp(-12376/x^10)/exp(18564/x^9)/
    exp(-27132/x^8)/exp(38760/x^7)/exp(-54264/x^6)/exp(74613/x^5)/exp(-100947/x^4)/
    exp(134596/x^3)/exp(-177100/x^2)/exp(230230/x)]*exp(1/(_var[x]^3+
    _var[x]^4)^7-1/_var[x]^21+7/_var[x]^20-28/_var[x]^19+84/
    _var[x]^18-210/_var[x]^17+462/_var[x]^16-924/_var[x]^15+1716/
    _var[x]^14-3003/_var[x]^13+5005/_var[x]^12-8008/_var[x]^11+12376/
    _var[x]^10-18564/_var[x]^9+27132/_var[x]^8-38760/_var[x]^7+54264/
    _var[x]^6-74613/_var[x]^5+100947/_var[x]^4-134596/_var[x]^3+
    177100/_var[x]^2-230230/_var[x]+296010)),
# different number of terms
    Series([Series([exp(-296010), 376740*exp(-296010), 70966038780*exp(-296010)],1,algebraic,[0,1,2],3,integer,_var[x],
    exp(1/(_var[x]^3+_var[x]^4)^7-1/_var[x]^21+7/_var[x]^20-
    28/_var[x]^19+84/_var[x]^18-210/_var[x]^17+462/_var[x]^16-
    924/_var[x]^15+1716/_var[x]^14-3003/_var[x]^13+5005/_var[x]^12-
    8008/_var[x]^11+12376/_var[x]^10-18564/_var[x]^9+27132/
    _var[x]^8-38760/_var[x]^7+54264/_var[x]^6-74613/_var[x]^5+
    100947/_var[x]^4-134596/_var[x]^3+177100/_var[x]^2-230230/
    _var[x]+296010)*exp(-296010))],0,t_SERIES,[-1],infinity,integer,
    _var[1/exp(1/x^21)/exp(-7/x^20)/exp(28/x^19)/exp(-84/x^18)/exp(210/x^17)/
    exp(-462/x^16)/exp(924/x^15)/exp(-1716/x^14)/exp(3003/x^13)/exp(-5005/x^12)/
    exp(8008/x^11)/exp(-12376/x^10)/exp(18564/x^9)/exp(-27132/x^8)/exp(38760/x^7)/
    exp(-54264/x^6)/exp(74613/x^5)/exp(-100947/x^4)/exp(134596/x^3)/exp(-177100/x^2)/
    exp(230230/x)],exp(-296010)/_var[1/exp(1/x^21)/exp(-7/x^20)/exp(28/x^19)/
    exp(-84/x^18)/exp(210/x^17)/exp(-462/x^16)/exp(924/x^15)/exp(-1716/x^14)/
    exp(3003/x^13)/exp(-5005/x^12)/exp(8008/x^11)/exp(-12376/x^10)/exp(18564/x^9)/
    exp(-27132/x^8)/exp(38760/x^7)/exp(-54264/x^6)/exp(74613/x^5)/exp(-100947/x^4)/
    exp(134596/x^3)/exp(-177100/x^2)/exp(230230/x)]*exp(1/(_var[x]^3+
    _var[x]^4)^7-1/_var[x]^21+7/_var[x]^20-28/_var[x]^19+84/
    _var[x]^18-210/_var[x]^17+462/_var[x]^16-924/_var[x]^15+1716/
    _var[x]^14-3003/_var[x]^13+5005/_var[x]^12-8008/_var[x]^11+12376/
    _var[x]^10-18564/_var[x]^9+27132/_var[x]^8-38760/_var[x]^7+54264/
    _var[x]^6-74613/_var[x]^5+100947/_var[x]^4-134596/_var[x]^3+
    177100/_var[x]^2-230230/_var[x]+296010))
    
]),true);
   unassign('s');

#end test
