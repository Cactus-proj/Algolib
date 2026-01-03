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

# test66.tst: test for MultiSeries[multiseries] (no. 66)

#test 46

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

   #=============== test nb 66 =====================
   Order:=21;
   TRY[compare_SERIES]("66-1",op([assign(s=multiseries(op([exp(1/(x^3+x^4)^7), x, 21]))),
    SERIES2Series(s)]),
    Series([Series([exp(-296010)],1,algebraic,[0],1,integer,_var[x],
    exp(1/(_var[x]^3+_var[x]^4)^7-1/_var[x]^21+7/_var[x]^20-28/
    _var[x]^19+84/_var[x]^18-210/_var[x]^17+462/_var[x]^16-924/
    _var[x]^15+1716/_var[x]^14-3003/_var[x]^13+5005/_var[x]^12-8008/
    _var[x]^11+12376/_var[x]^10-18564/_var[x]^9+27132/_var[x]^8-
    38760/_var[x]^7+54264/_var[x]^6-74613/_var[x]^5+100947/_var[x]^4-
    134596/_var[x]^3+177100/_var[x]^2-230230/_var[x]+296010)*exp(-296010))],
    0,t_SERIES,[-1],infinity,integer,_var[1/exp(1/x^21)/exp(-7/x^20)/exp(28/x^19)/
    exp(-84/x^18)/exp(210/x^17)/exp(-462/x^16)/exp(924/x^15)/exp(-1716/x^14)/
    exp(3003/x^13)/exp(-5005/x^12)/exp(8008/x^11)/exp(-12376/x^10)/exp(18564/x^9)/
    exp(-27132/x^8)/exp(38760/x^7)/exp(-54264/x^6)/exp(74613/x^5)/exp(-100947/x^4)/
    exp(134596/x^3)/exp(-177100/x^2)/exp(230230/x)],exp(-296010)/_var[1/
    exp(1/x^21)/exp(-7/x^20)/exp(28/x^19)/exp(-84/x^18)/exp(210/x^17)/
    exp(-462/x^16)/exp(924/x^15)/exp(-1716/x^14)/exp(3003/x^13)/exp(-5005/x^12)/
    exp(8008/x^11)/exp(-12376/x^10)/exp(18564/x^9)/exp(-27132/x^8)/
    exp(38760/x^7)/exp(-54264/x^6)/exp(74613/x^5)/exp(-100947/x^4)/
    exp(134596/x^3)/exp(-177100/x^2)/exp(230230/x)]*exp(1/(_var[x]^3+
    _var[x]^4)^7-1/_var[x]^21+7/_var[x]^20-28/_var[x]^19+
    84/_var[x]^18-210/_var[x]^17+462/_var[x]^16-924/_var[x]^15+
    1716/_var[x]^14-3003/_var[x]^13+5005/_var[x]^12-8008/_var[x]^11+
    12376/_var[x]^10-18564/_var[x]^9+27132/_var[x]^8-38760/_var[x]^7+
    54264/_var[x]^6-74613/_var[x]^5+100947/_var[x]^4-134596/_var[x]^3+
    177100/_var[x]^2-230230/_var[x]+296010)));
   TRY[compare_SERIES]("66-2",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][2..2])),
    Series([exp(1/(_var[x]^3+_var[x]^4)^7-1/_var[x]^21+7/
    _var[x]^20-28/_var[x]^19+84/_var[x]^18-210/_var[x]^17+
    462/_var[x]^16-924/_var[x]^15+1716/_var[x]^14-3003/_var[x]^13+
    5005/_var[x]^12-8008/_var[x]^11+12376/_var[x]^10-18564/_var[x]^9+
    27132/_var[x]^8-38760/_var[x]^7+54264/_var[x]^6-74613/_var[x]^5+
    100947/_var[x]^4-134596/_var[x]^3+177100/_var[x]^2-230230/_var[x]+
    296010)*exp(-296010)],0,algebraic,[-1],infinity,integer,_var[1/exp(1/x^21)/
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
   TRY[compare_SERIES]("66-3",
 member(
   SERIES2Series(multiseries(op(9,s),op(1,s))),
[
    Series([Series([exp(-296010)],1,algebraic,[0],1,integer,_var[x],
    exp(1/(_var[x]^3+_var[x]^4)^7-1/_var[x]^21+7/_var[x]^20-
    28/_var[x]^19+84/_var[x]^18-210/_var[x]^17+462/_var[x]^16-
    924/_var[x]^15+1716/_var[x]^14-3003/_var[x]^13+5005/_var[x]^12-
    8008/_var[x]^11+12376/_var[x]^10-18564/_var[x]^9+27132/_var[x]^8-
    38760/_var[x]^7+54264/_var[x]^6-74613/_var[x]^5+100947/_var[x]^4-
    134596/_var[x]^3+177100/_var[x]^2-230230/_var[x]+296010)*exp(-296010))],
    0,t_SERIES,[-1],infinity,integer,_var[1/exp(1/x^21)/exp(-7/x^20)/exp(28/x^19)/
    exp(-84/x^18)/exp(210/x^17)/exp(-462/x^16)/exp(924/x^15)/exp(-1716/x^14)/
    exp(3003/x^13)/exp(-5005/x^12)/exp(8008/x^11)/exp(-12376/x^10)/exp(18564/x^9)/
    exp(-27132/x^8)/exp(38760/x^7)/exp(-54264/x^6)/exp(74613/x^5)/exp(-100947/x^4)/
    exp(134596/x^3)/exp(-177100/x^2)/exp(230230/x)],exp(-296010)/_var[1/exp(1/x^21)/
    exp(-7/x^20)/exp(28/x^19)/exp(-84/x^18)/exp(210/x^17)/exp(-462/x^16)/exp(924/x^15)/
    exp(-1716/x^14)/exp(3003/x^13)/exp(-5005/x^12)/exp(8008/x^11)/exp(-12376/x^10)/
    exp(18564/x^9)/exp(-27132/x^8)/exp(38760/x^7)/exp(-54264/x^6)/exp(74613/x^5)/
    exp(-100947/x^4)/exp(134596/x^3)/exp(-177100/x^2)/exp(230230/x)]*
    exp(1/(_var[x]^3+_var[x]^4)^7-1/_var[x]^21+7/_var[x]^20-28/
    _var[x]^19+84/_var[x]^18-210/_var[x]^17+462/_var[x]^16-924/
    _var[x]^15+1716/_var[x]^14-3003/_var[x]^13+5005/_var[x]^12-8008/
    _var[x]^11+12376/_var[x]^10-18564/_var[x]^9+27132/_var[x]^8-38760/
    _var[x]^7+54264/_var[x]^6-74613/_var[x]^5+100947/_var[x]^4-134596/
    _var[x]^3+177100/_var[x]^2-230230/_var[x]+296010)),
# different number of terms
    Series([Series([exp(-296010), 376740*exp(-296010), 70966038780*exp(-296010), 8911795844562975*exp(-296010)],1,algebraic,[0,1,2,3],4,integer,_var[x],
    exp(1/(_var[x]^3+_var[x]^4)^7-1/_var[x]^21+7/_var[x]^20-
    28/_var[x]^19+84/_var[x]^18-210/_var[x]^17+462/_var[x]^16-
    924/_var[x]^15+1716/_var[x]^14-3003/_var[x]^13+5005/_var[x]^12-
    8008/_var[x]^11+12376/_var[x]^10-18564/_var[x]^9+27132/_var[x]^8-
    38760/_var[x]^7+54264/_var[x]^6-74613/_var[x]^5+100947/_var[x]^4-
    134596/_var[x]^3+177100/_var[x]^2-230230/_var[x]+296010)*exp(-296010))],
    0,t_SERIES,[-1],infinity,integer,_var[1/exp(1/x^21)/exp(-7/x^20)/exp(28/x^19)/
    exp(-84/x^18)/exp(210/x^17)/exp(-462/x^16)/exp(924/x^15)/exp(-1716/x^14)/
    exp(3003/x^13)/exp(-5005/x^12)/exp(8008/x^11)/exp(-12376/x^10)/exp(18564/x^9)/
    exp(-27132/x^8)/exp(38760/x^7)/exp(-54264/x^6)/exp(74613/x^5)/exp(-100947/x^4)/
    exp(134596/x^3)/exp(-177100/x^2)/exp(230230/x)],exp(-296010)/_var[1/exp(1/x^21)/
    exp(-7/x^20)/exp(28/x^19)/exp(-84/x^18)/exp(210/x^17)/exp(-462/x^16)/exp(924/x^15)/
    exp(-1716/x^14)/exp(3003/x^13)/exp(-5005/x^12)/exp(8008/x^11)/exp(-12376/x^10)/
    exp(18564/x^9)/exp(-27132/x^8)/exp(38760/x^7)/exp(-54264/x^6)/exp(74613/x^5)/
    exp(-100947/x^4)/exp(134596/x^3)/exp(-177100/x^2)/exp(230230/x)]*
    exp(1/(_var[x]^3+_var[x]^4)^7-1/_var[x]^21+7/_var[x]^20-28/
    _var[x]^19+84/_var[x]^18-210/_var[x]^17+462/_var[x]^16-924/
    _var[x]^15+1716/_var[x]^14-3003/_var[x]^13+5005/_var[x]^12-8008/
    _var[x]^11+12376/_var[x]^10-18564/_var[x]^9+27132/_var[x]^8-38760/
    _var[x]^7+54264/_var[x]^6-74613/_var[x]^5+100947/_var[x]^4-134596/
    _var[x]^3+177100/_var[x]^2-230230/_var[x]+296010))
    ]),true);
   TRY[compare_SERIES]("66-4",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,exact_order)),
    Series([Series([exp(-296010), 376740*exp(-296010), 70966038780*exp(-296010),
    8911795844562975*exp(-296010), 839340636644066521419*exp(-296010),
    63240945002634784431784752*exp(-296010), 7941532076329595696203903773289/2*exp(-296010),
    213698045326561121670148062008172619*exp(-296010),
    20126207330602985832101935166478417928981/2*exp(-296010),
    842437924499497990972659608001232534227801585/2*exp(-296010),
    31736094373523646001741129157396513195372961961167/2*exp(-296010),
    11955455880405564466308808053204834654001673505862638627/22*exp(-296010),
    1501255602913489536799422355108260366756314583345481817759213/88*exp(-296010),
    5437861593112707000288301173694173426238102266162252099118653457/11*exp(-296010),
    585280487322262691680232826800145019591962041087689789653009888969453/44*exp(-296010),
    29397054318229228569625359895650539814559720875482460055703672878419000711/88*exp(-296010),
    173030222524839024031259499752030325422420353158027442723317923187052581433603/22*exp(-296010),
    130360848921784169352407287916563921513601354817281307414953346173177983584601941895/748*exp(-296010),
    32737705037575685040410627047642753494268537242354123254341988642857381839377074128183217/8976*exp(-296010),
    1027676412112627191072270406328521836319284341225685173728062919906491617485888534650717240029/14212*
    exp(-296010),
    387117570873019063240385018597098775444295260303063944263982901350101146213581822669003119527160253/284240*
    exp(-296010)],1,algebraic,[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20],21,integer,_var[x],exp(1/(_var[x]^3+_var[x]^4)^7-
    1/_var[x]^21+7/_var[x]^20-28/_var[x]^19+84/_var[x]^18-210/
    _var[x]^17+462/_var[x]^16-924/_var[x]^15+1716/_var[x]^14-3003/
    _var[x]^13+5005/_var[x]^12-8008/_var[x]^11+12376/_var[x]^10-18564/
    _var[x]^9+27132/_var[x]^8-38760/_var[x]^7+54264/_var[x]^6-74613/
    _var[x]^5+100947/_var[x]^4-134596/_var[x]^3+177100/_var[x]^2-230230/
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
    _var[x]^18-210/_var[x]^17+462/_var[x]^16-924/_var[x]^15+
    1716/_var[x]^14-3003/_var[x]^13+5005/_var[x]^12-8008/
    _var[x]^11+12376/_var[x]^10-18564/_var[x]^9+27132/_var[x]^8-
    38760/_var[x]^7+54264/_var[x]^6-74613/_var[x]^5+100947/
    _var[x]^4-134596/_var[x]^3+177100/_var[x]^2-230230/_var[x]+296010)));
   unassign('s');

#end test
