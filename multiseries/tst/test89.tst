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

# test89.tst: test for MultiSeries[multiseries] (no. 89)

#test 45

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
   	    Testzero(op(op(7,s1))-op(op(7,s2))) and
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

   #=============== test nb 89 =====================
   Order:=3;
   TRY[compare_ms_SERIES]("89-1",
    multiseries(4/9*exp(exp(5/2/x^(5/7)+21/8*x^(6/11)+
    2/x^8+54/17*x^(49/45))^8)/ln(ln(-ln(4/3/x^(5/14))))^(7/6), x = infinity),
    Series([Series([4/9/(ln(1+ln(1-14/5*ln(4/3)*_var[1/ln(x)])/
    (1/_var[1/ln(ln(x))]+ln(5/14)))+ln(1+ln(5/14)*_var[
    1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(7/6)],0,algebraic,[0],
    infinity,integer,_var[1/x],4/9/(ln(1+ln(1-14/5*ln(4/3)*
    _var[1/ln(x)])/(1/_var[1/ln(ln(x))]+ln(5/14)))+
    ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^(7/6))],0,t_SERIES,[-1],infinity,integer,_var[1/exp(exp(1/136*(340*x^8*(1/x)^(809/495)+357*x^(713/77)*
    (1/x)^(809/495)+272*x^(5/7)*(1/x)^(809/495)+432*x^(3088/315)*
    (1/x)^(809/495)-432*x^(61/7)*(1/x)^(6/11)-357*x^(61/7)*(1/x)^(49/45))/
    x^(61/7)/(1/x)^(809/495))^8*exp(54/17/(1/x)^(49/45))^8*exp(21/8/
    (1/x)^(6/11))^8)],4/9/(ln(1+ln(1-14/5*ln(4/3)*_var[1/ln(x)])/
    (1/_var[1/ln(ln(x))]+ln(5/14)))+ln(1+ln(5/14)*
    _var[1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(7/6)/
    _var[1/exp(exp(1/136*(340*x^8*(1/x)^(809/495)+357*x^(713/77)*
    (1/x)^(809/495)+272*x^(5/7)*(1/x)^(809/495)+432*x^(3088/315)*(1/x)^(809/495)-
    432*x^(61/7)*(1/x)^(6/11)-357*x^(61/7)*(1/x)^(49/45))/x^(61/7)/
    (1/x)^(809/495))^8*exp(54/17/(1/x)^(49/45))^8*exp(21/8/(1/x)^(6/11))^8)]),
    assign='s');
    closedform:=op(9,s);
    scale:=op(1,s);
   TRY[compare_ms_SERIES]("89-2",
    multiseries(closedform,scale,Order,scale[list][1..1]),
    Series([Series([Series([Series([4/9],0,rational,[7/6],infinity,rational,
    _var[1/ln(ln(ln(x)))],4/9/(1/_var[1/ln(ln(ln(x)))])^(7/6))],
    Series([],4/9,integer,[],13/6,integer,_var[1/ln(ln(ln(x)))],
    -14/27*ln(5/14)*_var[1/ln(ln(ln(x)))]/(1/_var[1/
    ln(ln(ln(x)))])^(7/6)),t_SERIES,[0],1,rational,_var[1/ln(ln(x))],
    4/9/(ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^(7/6))],
    Series([Series([],1,rational,[],13/6,rational,_var[1/ln(ln(ln(x)))],
    49/15/(1/_var[1/ln(ln(ln(x)))])^(7/6)*ln(4/3)*_var[
    1/ln(ln(ln(x)))])],
    Series([],1,integer,[],13/6,rational,_var[1/ln(ln(ln(x)))],
    (-ln(5/14)*_var[1/ln(ln(ln(x)))]-ln(5/14)*_var[
    1/ln(ln(ln(x)))]^2)/(1/_var[1/ln(ln(ln(x)))])^(7/6)-7/6*
    ln(5/14)*_var[1/ln(ln(ln(x)))]/(1/_var[
    1/ln(ln(ln(x)))])^(7/6)),t_SERIES,[1],2,rational,_var[1/ln(ln(x))],
    49/15*ln(4/3)/(1/_var[1/ln(ln(x))]+ln(5/14))/(ln(1+ln(5/14)*
    _var[1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(13/6)),
    t_SERIES,[0],1,rational,_var[1/ln(x)],4/9/(ln(1+ln(1-14/5*ln(4/3)*
    _var[1/ln(x)])/(1/_var[1/ln(ln(x))]+ln(5/14)))+
    ln(1+ln(5/14)*_var[1/ln(ln(x))])+
    1/_var[1/ln(ln(ln(x)))])^(7/6))],0,t_SERIES,[-1],infinity,integer,
    _var[1/exp(exp(1/136*(340*x^8*(1/x)^(809/495)+357*x^(713/77)*
    (1/x)^(809/495)+272*x^(5/7)*(1/x)^(809/495)+432*x^(3088/315)*(1/x)^(809/495)-
    432*x^(61/7)*(1/x)^(6/11)-357*x^(61/7)*(1/x)^(49/45))/x^(61/7)/(1/x)^(809/495))^8*
    exp(54/17/(1/x)^(49/45))^8*exp(21/8/(1/x)^(6/11))^8)],4/9/(ln(1+ln(1-14/5*
    ln(4/3)*_var[1/ln(x)])/(1/_var[1/ln(ln(x))]+ln(5/14)))+
    ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(7/6)/
    _var[1/exp(exp(1/136*(340*x^8*(1/x)^(809/495)+357*x^(713/77)*
    (1/x)^(809/495)+272*x^(5/7)*(1/x)^(809/495)+432*x^(3088/315)*(1/x)^(809/495)-
    432*x^(61/7)*(1/x)^(6/11)-357*x^(61/7)*(1/x)^(49/45))/x^(61/7)/
    (1/x)^(809/495))^8*exp(54/17/(1/x)^(49/45))^8*exp(21/8/(1/x)^(6/11))^8)]));
   TRY[compare_ms_SERIES]("89-3",
    multiseries(closedform,scale,Order,scale[list][2..2]),
    Series([Series([Series([4/9/(1/_var[1/ln(ln(ln(x)))])^(7/6),
    -14/27*ln(5/14)*_var[1/ln(ln(ln(x)))]/(1/_var[
    1/ln(ln(ln(x)))])^(7/6), 4/9/(1/_var[1/ln(ln(ln(x)))])^(7/6)*
    (7/12*_var[1/ln(ln(ln(x)))]*ln(5/14)^2+91/72*ln(5/14)^2*
    _var[1/ln(ln(ln(x)))]^2)],(1/ln(ln(ln(x))))^(13/6),algebraic,
    [0, 1, 2],3,rational,_var[1/ln(ln(x))],4/9/(ln(1+ln(5/14)*
    _var[1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(7/6))],
    Series([],_var[1/ln(ln(ln(x)))]/(1/_var[
    1/ln(ln(ln(x)))])^(7/6),algebraic,[],1,rational,_var[1/ln(ln(x))],
    49/15*ln(4/3)/(1/_var[1/ln(ln(x))]+ln(5/14))/(ln(1+ln(5/14)*
    _var[1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(13/6)),
    t_SERIES,[0],1,rational,_var[1/ln(x)],4/9/(ln(1+ln(1-14/5*ln(4/3)*
    _var[1/ln(x)])/(1/_var[1/ln(ln(x))]+ln(5/14)))+
    ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^(7/6))],0,t_SERIES,[-1],infinity,integer,
    _var[1/exp(exp(1/136*(340*x^8*(1/x)^(809/495)+357*x^(713/77)*
    (1/x)^(809/495)+272*x^(5/7)*(1/x)^(809/495)+432*x^(3088/315)*(1/x)^(809/495)-
    432*x^(61/7)*(1/x)^(6/11)-357*x^(61/7)*(1/x)^(49/45))/x^(61/7)/(1/x)^(809/495))^8*
    exp(54/17/(1/x)^(49/45))^8*exp(21/8/(1/x)^(6/11))^8)],4/9/(ln(1+ln(1-14/5*
    ln(4/3)*_var[1/ln(x)])/(1/_var[1/ln(ln(x))]+ln(5/14)))+
    ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^(7/6)/_var[1/exp(exp(1/136*(340*x^8*
    (1/x)^(809/495)+357*x^(713/77)*(1/x)^(809/495)+272*x^(5/7)*(1/x)^(809/495)+
    432*x^(3088/315)*(1/x)^(809/495)-432*x^(61/7)*(1/x)^(6/11)-357*x^(61/7)*
    (1/x)^(49/45))/x^(61/7)/(1/x)^(809/495))^8*exp(54/17/(1/x)^(49/45))^8*
    exp(21/8/(1/x)^(6/11))^8)]));
   TRY[compare_ms_SERIES]("89-4",
    multiseries(closedform,scale,Order,scale[list][3..3]),
    Series([Series([4/9/(ln(1+ln(5/14)*_var[1/ln(ln(x))])+
    1/_var[1/ln(ln(ln(x)))])^(7/6), 196/135*ln(4/3)/(1/
    _var[1/ln(ln(x))]+ln(5/14))/(ln(1+ln(5/14)*_var[
    1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(13/6),
    4/9/(ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^(7/6)*(-7/6/(ln(1+ln(5/14)*_var[1/ln(ln(x))])+
    1/_var[1/ln(ln(ln(x)))])*(-98/25/(1/_var[1/ln(ln(x))]+
    ln(5/14))*ln(4/3)^2-98/25*ln(4/3)^2/(1/_var[1/ln(ln(x))]+
    ln(5/14))^2)+4459/450/(ln(1+ln(5/14)*_var[1/ln(ln(x))])+
    1/_var[1/ln(ln(ln(x)))])^2*ln(4/3)^2/(1/_var[
    1/ln(ln(x))]+ln(5/14))^2)],(1/ln(ln(ln(x))))^(13/6)/ln(ln(x)),algebraic,
    [0, 1, 2],3,rational,_var[1/ln(x)],4/9/(ln(1+ln(1-14/5*ln(4/3)*
    _var[1/ln(x)])/(1/_var[1/ln(ln(x))]+ln(5/14)))+
    ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^(7/6))],0,t_SERIES,[-1],infinity,integer,_var[
    1/exp(exp(1/136*(340*x^8*(1/x)^(809/495)+357*x^(713/77)*(1/x)^(809/495)+
    272*x^(5/7)*(1/x)^(809/495)+432*x^(3088/315)*(1/x)^(809/495)-432*x^(61/7)*
    (1/x)^(6/11)-357*x^(61/7)*(1/x)^(49/45))/x^(61/7)/(1/x)^(809/495))^8*
    exp(54/17/(1/x)^(49/45))^8*exp(21/8/(1/x)^(6/11))^8)],4/9/(ln(1+ln(1-14/5*
    ln(4/3)*_var[1/ln(x)])/(1/_var[1/ln(ln(x))]+ln(5/14)))+
    ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^(7/6)/_var[1/exp(exp(1/136*(340*x^8*(1/x)^(809/495)+
    357*x^(713/77)*(1/x)^(809/495)+272*x^(5/7)*(1/x)^(809/495)+432*x^(3088/315)*
    (1/x)^(809/495)-432*x^(61/7)*(1/x)^(6/11)-357*x^(61/7)*(1/x)^(49/45))/x^(61/7)/
    (1/x)^(809/495))^8*exp(54/17/(1/x)^(49/45))^8*exp(21/8/(1/x)^(6/11))^8)]));
   TRY[compare_ms_SERIES]("89-5",
    multiseries(closedform,scale,Order,scale[list][5..5]),
    Series([Series([4/9/(ln(1+ln(1-14/5*ln(4/3)*_var[1/ln(x)])/
    (1/_var[1/ln(ln(x))]+ln(5/14)))+ln(1+ln(5/14)*_var[
    1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(7/6)],0,algebraic,[0],
    infinity,rational,_var[1/exp(54/17/(1/x)^(49/45))/exp(21/8/
    (1/x)^(6/11))],4/9/(ln(1+ln(1-14/5*ln(4/3)*_var[1/ln(x)])/
    (1/_var[1/ln(ln(x))]+ln(5/14)))+ln(1+ln(5/14)*_var[
    1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(7/6))],0,t_SERIES,
    [-1],infinity,integer,_var[1/exp(exp(1/136*(340*x^8*(1/x)^(809/495)+
    357*x^(713/77)*(1/x)^(809/495)+272*x^(5/7)*(1/x)^(809/495)+432*x^(3088/315)*
    (1/x)^(809/495)-432*x^(61/7)*(1/x)^(6/11)-357*x^(61/7)*(1/x)^(49/45))/x^(61/7)/
    (1/x)^(809/495))^8*exp(54/17/(1/x)^(49/45))^8*exp(21/8/(1/x)^(6/11))^8)],
    4/9/(ln(1+ln(1-14/5*ln(4/3)*_var[1/ln(x)])/(1/_var[
    1/ln(ln(x))]+ln(5/14)))+ln(1+ln(5/14)*_var[1/ln(ln(x))])+
    1/_var[1/ln(ln(ln(x)))])^(7/6)/_var[1/exp(exp(1/136*(340*
    x^8*(1/x)^(809/495)+357*x^(713/77)*(1/x)^(809/495)+272*x^(5/7)*(1/x)^(809/495)+
    432*x^(3088/315)*(1/x)^(809/495)-432*x^(61/7)*(1/x)^(6/11)-357*x^(61/7)*
    (1/x)^(49/45))/x^(61/7)/(1/x)^(809/495))^8*exp(54/17/(1/x)^(49/45))^8*
    exp(21/8/(1/x)^(6/11))^8)]));
   TRY[compare_ms_SERIES]("89-6",
    multiseries(closedform,scale,Order,scale[list][6..6]),
    Series([4/9/(ln(1+ln(1-14/5*ln(4/3)*_var[1/ln(x)])/(1/
    _var[1/ln(ln(x))]+ln(5/14)))+ln(1+ln(5/14)*
    _var[1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(7/6)],
    0,algebraic,[-1],infinity,rational,_var[1/exp(exp(1/136*(340*
    x^8*(1/x)^(809/495)+357*x^(713/77)*(1/x)^(809/495)+272*x^(5/7)*
    (1/x)^(809/495)+432*x^(3088/315)*(1/x)^(809/495)-432*x^(61/7)*(1/x)^(6/11)-
    357*x^(61/7)*(1/x)^(49/45))/x^(61/7)/(1/x)^(809/495))^8*exp(54/17/
    (1/x)^(49/45))^8*exp(21/8/(1/x)^(6/11))^8)],4/9/(ln(1+ln(1-14/5*
    ln(4/3)*_var[1/ln(x)])/(1/_var[1/ln(ln(x))]+
    ln(5/14)))+ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^(7/6)/_var[1/exp(exp(1/136*(340*x^8*
    (1/x)^(809/495)+357*x^(713/77)*(1/x)^(809/495)+272*x^(5/7)*(1/x)^(809/495)+
    432*x^(3088/315)*(1/x)^(809/495)-432*x^(61/7)*(1/x)^(6/11)-357*x^(61/7)*
    (1/x)^(49/45))/x^(61/7)/(1/x)^(809/495))^8*exp(54/17/(1/x)^(49/45))^8*
    exp(21/8/(1/x)^(6/11))^8)]));
   TRY[compare_ms_SERIES]("89-7",
    multiseries(closedform,scale),
    Series([Series([Series([Series([Series([Series([4/9],0,rational,[7/6],
    infinity,rational,_var[1/ln(ln(ln(x)))],4/9/(1/
    _var[1/ln(ln(ln(x)))])^(7/6)),
    Series([-14/27*ln(5/14)],0,algebraic,[13/6],infinity,rational,
    _var[1/ln(ln(ln(x)))],-14/27*ln(5/14)*_var[
    1/ln(ln(ln(x)))]/(1/_var[1/ln(ln(ln(x)))])^(7/6)),
    Series([7/27*ln(5/14)^2, 91/162*ln(5/14)^2],0,algebraic,[13/6, 19/6],
    infinity,rational,_var[1/ln(ln(ln(x)))],4/9/(1/
    _var[1/ln(ln(ln(x)))])^(7/6)*(7/12*_var[
    1/ln(ln(ln(x)))]*ln(5/14)^2+91/72*ln(5/14)^2*_var[
    1/ln(ln(ln(x)))]^2))],
    Series([],1,integer,[],13/6,rational,_var[
    1/ln(ln(ln(x)))],(-7/6*_var[1/ln(ln(ln(x)))]-91/72*
    _var[1/ln(ln(ln(x)))]^2*ln(5/14)^3-1729/1296*
    ln(5/14)^3*_var[1/ln(ln(ln(x)))]^3)/(1/_var[
    1/ln(ln(ln(x)))])^(7/6)),t_SERIES,[0, 1, 2],3,rational,
    _var[1/ln(ln(x))],4/9/(ln(1+ln(5/14)*_var[
    1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(7/6)),
    Series([Series([196/135*ln(4/3)],0,algebraic,[13/6],infinity,
    rational,_var[1/ln(ln(ln(x)))],196/135/(1/_var[
    1/ln(ln(ln(x)))])^(13/6)*ln(4/3)), Series([-196/135*ln(5/14)*
    ln(4/3), -1274/405*ln(5/14)*ln(4/3)],0,algebraic,[13/6, 19/6],
    infinity,rational,_var[1/ln(ln(ln(x)))],-1274/405/(1/
    _var[1/ln(ln(ln(x)))])^(13/6)*ln(5/14)*_var[
    1/ln(ln(ln(x)))]*ln(4/3)-196/135/(1/_var[
    1/ln(ln(ln(x)))])^(13/6)*ln(5/14)*ln(4/3)),
    Series([196/135*ln(5/14)^2*ln(4/3), 637/135*ln(5/14)^2*ln(4/3),
    12103/2430*ln(5/14)^2*ln(4/3)],0,algebraic,[13/6, 19/6, 25/6],
    infinity,rational,_var[1/ln(ln(ln(x)))],196/135/(1/
    _var[1/ln(ln(ln(x)))])^(13/6)*(13/12*_var[
    1/ln(ln(ln(x)))]*ln(5/14)^2+247/72*ln(5/14)^2*_var[
    1/ln(ln(ln(x)))]^2)*ln(4/3)+1274/405/(1/_var[
    1/ln(ln(ln(x)))])^(13/6)*ln(5/14)^2*_var[1/ln(ln(ln(x)))]*
    ln(4/3)+196/135/(1/_var[1/ln(ln(ln(x)))])^(13/6)*ln(5/14)^2*
    ln(4/3))],
    Series([],1,integer,[],0,integer,_var[1/ln(ln(ln(x)))],1),
    t_SERIES,[1, 2, 3],4,rational,_var[1/ln(ln(x))],196/135*
    ln(4/3)/(1/_var[1/ln(ln(x))]+ln(5/14))/(ln(1+ln(5/14)*
    _var[1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(13/6)),
    Series([Series([1372/675*ln(4/3)^2],0,algebraic,[13/6],infinity,rational,
    _var[1/ln(ln(ln(x)))],1372/675*ln(4/3)^2*_var[
    1/ln(ln(ln(x)))]/(1/_var[1/ln(ln(ln(x)))])^(7/6)),
    Series([-1372/675*ln(4/3)^2*ln(5/14)+1372/675*ln(4/3)^2, -8918/2025*ln(4/3)^2*
    ln(5/14)+8918/2025*ln(4/3)^2],0,algebraic,[13/6, 19/6],infinity,rational,
    _var[1/ln(ln(ln(x)))],1/(1/_var[1/ln(ln(ln(x)))])^(7/6)*
    (4/9*(-343/75*ln(4/3)^2*ln(5/14)+343/75*ln(4/3)^2)*_var[
    1/ln(ln(ln(x)))]-1372/675*ln(4/3)^2*ln(5/14)*_var[
    1/ln(ln(ln(x)))]^2+8918/2025*ln(4/3)^2*_var[1/ln(ln(ln(x)))]^2)-
    4802/2025*ln(4/3)^2*_var[1/ln(ln(ln(x)))]^2*ln(5/14)/
    (1/_var[1/ln(ln(ln(x)))])^(7/6)),
    Series([1372/675*ln(4/3)^2*ln(5/14)^2-2744/675*ln(4/3)^2*ln(5/14),
    4/9*(343/75*ln(4/3)^2*ln(5/14)-343/75*ln(4/3)^2)*ln(5/14)+4459/2025*
    ln(4/3)^2*ln(5/14)^2-17836/2025*ln(4/3)^2*ln(5/14)+ln(5/14)*(4802/2025*
    ln(4/3)^2*ln(5/14)-4802/2025*ln(4/3)^2), 55909/12150*ln(4/3)^2*
    ln(5/14)^2-17836/2025*ln(4/3)^2*ln(5/14)+ln(5/14)*(4802/2025*ln(4/3)^2*
    ln(5/14)-31213/6075*ln(4/3)^2)],0,algebraic,[13/6, 19/6, 25/6],infinity,
    rational,_var[1/ln(ln(ln(x)))],(4/9*(343/75*ln(4/3)^2*ln(5/14)^2-
    686/75*ln(4/3)^2*ln(5/14))*_var[1/ln(ln(ln(x)))]+4/9*(343/75*
    ln(4/3)^2*ln(5/14)-343/75*ln(4/3)^2)*ln(5/14)*_var[
    1/ln(ln(ln(x)))]^2+1372/675*ln(4/3)^2*_var[1/ln(ln(ln(x)))]*
    (1/2*_var[1/ln(ln(ln(x)))]*ln(5/14)^2+ln(5/14)^2*
    _var[1/ln(ln(ln(x)))]^2)+4/9*ln(4/3)^2*(-4459/225*ln(5/14)*
    _var[1/ln(ln(ln(x)))]^2-4459/225*_var[1/ln(ln(ln(x)))]^3*
    ln(5/14)))/(1/_var[1/ln(ln(ln(x)))])^(7/6)+(-14/27*(-343/75*
    ln(4/3)^2*ln(5/14)+343/75*ln(4/3)^2)*_var[1/ln(ln(ln(x)))]+
    4802/2025*ln(4/3)^2*ln(5/14)*_var[1/ln(ln(ln(x)))]^2-31213/6075*
    ln(4/3)^2*_var[1/ln(ln(ln(x)))]^2)*ln(5/14)*_var[
    1/ln(ln(ln(x)))]/(1/_var[1/ln(ln(ln(x)))])^(7/6)+1372/675*
    ln(4/3)^2*_var[1/ln(ln(ln(x)))]/(1/_var[1/ln(ln(ln(x)))])^(7/6)*
    (7/12*_var[1/ln(ln(ln(x)))]*ln(5/14)^2+91/72*ln(5/14)^2*_var[
    1/ln(ln(ln(x)))]^2))],
    Series([],1,integer,[],0,integer,_var[1/ln(ln(ln(x)))],1),
    t_SERIES,[1, 2, 3],4,rational,_var[1/ln(ln(x))],4/9/
    (ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^(7/6)*(-7/6/(ln(1+ln(5/14)*_var[1/ln(ln(x))])+
    1/_var[1/ln(ln(ln(x)))])*(-98/25/(1/_var[1/ln(ln(x))]+
    ln(5/14))*ln(4/3)^2-98/25/(1/_var[1/ln(ln(x))]+ln(5/14))^2*
    ln(4/3)^2)+4459/450/(ln(1+ln(5/14)*_var[1/ln(ln(x))])+
    1/_var[1/ln(ln(ln(x)))])^2/(1/_var[1/ln(ln(x))]+
    ln(5/14))^2*ln(4/3)^2))],
    Series([],Series([],1,integer,[],13/6,rational,_var[1/ln(ln(ln(x)))],
    -7/6/(1/_var[1/ln(ln(ln(x)))])^(7/6)*_var[1/ln(ln(ln(x)))]),
    t_SERIES,[],1,rational,_var[1/ln(ln(x))],
    ((-7/6*_var[1/ln(ln(x))]+4802/375/(1/_var[1/ln(ln(x))]+
    ln(5/14))^2*ln(4/3)^3+9604/1125/(1/_var[1/ln(ln(x))]+ln(5/14))^3*
    ln(4/3)^3)/(ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])-637/90/(ln(1+ln(5/14)*_var[1/ln(ln(x))])+
    1/_var[1/ln(ln(ln(x)))])^2*(-98/25/(1/_var[1/ln(ln(x))]+
    ln(5/14))*ln(4/3)^2-98/25/(1/_var[1/ln(ln(x))]+ln(5/14))^2*
    ln(4/3)^2)/(1/_var[1/ln(ln(x))]+ln(5/14))*ln(4/3)+593047/20250/
    (ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^3/(1/_var[1/ln(ln(x))]+ln(5/14))^3*ln(4/3)^3)/
    (ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^(7/6)),t_SERIES,[0, 1, 2],3,rational,_var[
    1/ln(x)],4/9/(ln(1+ln(1-14/5*ln(4/3)*_var[1/ln(x)])/(1/
    _var[1/ln(ln(x))]+ln(5/14)))+ln(1+ln(5/14)*_var[
    1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(7/6))],0,t_SERIES,
    [0],infinity,rational,_var[1/x],4/9/(ln(1+ln(1-14/5*ln(4/3)*
    _var[1/ln(x)])/(1/_var[1/ln(ln(x))]+ln(5/14)))+
    ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^(7/6))],0,t_SERIES,[0],infinity,rational,_var[
    1/exp(54/17/(1/x)^(49/45))/exp(21/8/(1/x)^(6/11))],4/9/(ln(1+ln(1-14/5*ln(4/3)*
    _var[1/ln(x)])/(1/_var[1/ln(ln(x))]+ln(5/14)))+
    ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/
    _var[1/ln(ln(ln(x)))])^(7/6))],0,t_SERIES,[-1],infinity,rational,
    _var[1/exp(exp(1/136*(340*x^8*(1/x)^(809/495)+357*x^(713/77)*
    (1/x)^(809/495)+272*x^(5/7)*(1/x)^(809/495)+432*x^(3088/315)*(1/x)^(809/495)-
    432*x^(61/7)*(1/x)^(6/11)-357*x^(61/7)*(1/x)^(49/45))/x^(61/7)/
    (1/x)^(809/495))^8*exp(54/17/(1/x)^(49/45))^8*exp(21/8/(1/x)^(6/11))^8)],
    4/9/(ln(1+ln(1-14/5*ln(4/3)*_var[1/ln(x)])/(1/_var[
    1/ln(ln(x))]+ln(5/14)))+ln(1+ln(5/14)*_var[1/ln(ln(x))])+
    1/_var[1/ln(ln(ln(x)))])^(7/6)/_var[1/exp(exp(1/136*
    (340*x^8*(1/x)^(809/495)+357*x^(713/77)*(1/x)^(809/495)+272*x^(5/7)*
    (1/x)^(809/495)+432*x^(3088/315)*(1/x)^(809/495)-432*x^(61/7)*
    (1/x)^(6/11)-357*x^(61/7)*(1/x)^(49/45))/x^(61/7)/(1/x)^(809/495))^8*
    exp(54/17/(1/x)^(49/45))^8*exp(21/8/(1/x)^(6/11))^8)]));
   TRY[compare_ms_SERIES]("89-8",
    multiseries(closedform,scale,Order,exact_order),
    Series([Series([Series([Series([Series([Series([4/9],0,rational,
    [7/6],infinity,rational,_var[1/ln(ln(ln(x)))],4/9/
    (1/_var[1/ln(ln(ln(x)))])^(7/6)),
    Series([-14/27*ln(5/14)],0,algebraic,[13/6],infinity,rational,
    _var[1/ln(ln(ln(x)))],-14/27*ln(5/14)*_var[
    1/ln(ln(ln(x)))]/(1/_var[1/ln(ln(ln(x)))])^(7/6)),
    Series([7/27*ln(5/14)^2, 91/162*ln(5/14)^2],0,algebraic,[13/6, 19/6],
    infinity,rational,_var[1/ln(ln(ln(x)))],4/9/(1/
    _var[1/ln(ln(ln(x)))])^(7/6)*(7/12*_var[
    1/ln(ln(ln(x)))]*ln(5/14)^2+91/72*ln(5/14)^2*_var[
    1/ln(ln(ln(x)))]^2))],
    Series([],1,integer,[],13/6,rational,_var[1/ln(ln(ln(x)))],
    (-7/6*_var[1/ln(ln(ln(x)))]-91/72*_var[
    1/ln(ln(ln(x)))]^2*ln(5/14)^3-1729/1296*ln(5/14)^3*
    _var[1/ln(ln(ln(x)))]^3)/(1/_var[
    1/ln(ln(ln(x)))])^(7/6)),t_SERIES,[0, 1, 2],3,rational,_var[
    1/ln(ln(x))],4/9/(ln(1+ln(5/14)*_var[1/ln(ln(x))])+
    1/_var[1/ln(ln(ln(x)))])^(7/6)),
    Series([Series([196/135*ln(4/3)],0,algebraic,[13/6],infinity,
    rational,_var[1/ln(ln(ln(x)))],196/135/(1/
    _var[1/ln(ln(ln(x)))])^(13/6)*ln(4/3)),
    Series([-196/135*ln(5/14)*ln(4/3), -1274/405*ln(5/14)*ln(4/3)],
    0,algebraic,[13/6, 19/6],infinity,rational,_var[
    1/ln(ln(ln(x)))],-1274/405/(1/_var[
    1/ln(ln(ln(x)))])^(13/6)*ln(5/14)*_var[1/ln(ln(ln(x)))]*
    ln(4/3)-196/135/(1/_var[1/ln(ln(ln(x)))])^(13/6)*ln(5/14)*
    ln(4/3)),
    Series([196/135*ln(5/14)^2*ln(4/3), 637/135*ln(5/14)^2*ln(4/3),
    12103/2430*ln(5/14)^2*ln(4/3)],0,algebraic,[13/6, 19/6, 25/6],
    infinity,rational,_var[1/ln(ln(ln(x)))],196/135/
    (1/_var[1/ln(ln(ln(x)))])^(13/6)*(13/12*_var[
    1/ln(ln(ln(x)))]*ln(5/14)^2+247/72*ln(5/14)^2*_var[
    1/ln(ln(ln(x)))]^2)*ln(4/3)+1274/405/(1/_var[
    1/ln(ln(ln(x)))])^(13/6)*ln(5/14)^2*_var[1/ln(ln(ln(x)))]*
    ln(4/3)+196/135/(1/_var[1/ln(ln(ln(x)))])^(13/6)*
    ln(5/14)^2*ln(4/3))],Series([],1,integer,[],0,integer,_var[
    1/ln(ln(ln(x)))],1),t_SERIES,[1, 2, 3],4,rational,_var[
    1/ln(ln(x))],196/135*ln(4/3)/(1/_var[1/ln(ln(x))]+
    ln(5/14))/(ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/
    _var[1/ln(ln(ln(x)))])^(13/6)),
    Series([Series([1372/675*ln(4/3)^2],0,algebraic,[13/6],infinity,
    rational,_var[1/ln(ln(ln(x)))],1372/675*ln(4/3)^2*
    _var[1/ln(ln(ln(x)))]/(1/_var[
    1/ln(ln(ln(x)))])^(7/6)),
    Series([-1372/675*ln(4/3)^2*ln(5/14)+1372/675*ln(4/3)^2, -8918/2025*
    ln(4/3)^2*ln(5/14)+8918/2025*ln(4/3)^2],0,algebraic,[13/6, 19/6],
    infinity,rational,_var[1/ln(ln(ln(x)))],1/(1/_var[
    1/ln(ln(ln(x)))])^(7/6)*(4/9*(-343/75*ln(4/3)^2*ln(5/14)+343/75*
    ln(4/3)^2)*_var[1/ln(ln(ln(x)))]-1372/675*ln(4/3)^2*
    ln(5/14)*_var[1/ln(ln(ln(x)))]^2+8918/2025*ln(4/3)^2*
    _var[1/ln(ln(ln(x)))]^2)-4802/2025*ln(4/3)^2*_var[
    1/ln(ln(ln(x)))]^2*ln(5/14)/(1/_var[1/ln(ln(ln(x)))])^(7/6)),
    Series([1372/675*ln(4/3)^2*ln(5/14)^2-2744/675*ln(4/3)^2*ln(5/14),
    4/9*(343/75*ln(4/3)^2*ln(5/14)-343/75*ln(4/3)^2)*ln(5/14)+4459/2025*
    ln(4/3)^2*ln(5/14)^2-17836/2025*ln(4/3)^2*ln(5/14)+ln(5/14)*(4802/2025*
    ln(4/3)^2*ln(5/14)-4802/2025*ln(4/3)^2), 55909/12150*ln(4/3)^2*ln(5/14)^2-
    17836/2025*ln(4/3)^2*ln(5/14)+ln(5/14)*(4802/2025*ln(4/3)^2*ln(5/14)-
    31213/6075*ln(4/3)^2)],0,algebraic,[13/6, 19/6, 25/6],infinity,rational,
    _var[1/ln(ln(ln(x)))],(4/9*(343/75*ln(4/3)^2*ln(5/14)^2-686/75*
    ln(4/3)^2*ln(5/14))*_var[1/ln(ln(ln(x)))]+4/9*(343/75*ln(4/3)^2*
    ln(5/14)-343/75*ln(4/3)^2)*ln(5/14)*_var[1/ln(ln(ln(x)))]^2+
    1372/675*ln(4/3)^2*_var[1/ln(ln(ln(x)))]*(1/2*_var[
    1/ln(ln(ln(x)))]*ln(5/14)^2+ln(5/14)^2*_var[1/ln(ln(ln(x)))]^2)+
    4/9*ln(4/3)^2*(-4459/225*ln(5/14)*_var[1/ln(ln(ln(x)))]^2-
    4459/225*_var[1/ln(ln(ln(x)))]^3*ln(5/14)))/(1/_var[
    1/ln(ln(ln(x)))])^(7/6)+(-14/27*(-343/75*ln(4/3)^2*ln(5/14)+343/75*ln(4/3)^2)*
    _var[1/ln(ln(ln(x)))]+4802/2025*ln(4/3)^2*ln(5/14)*_var[
    1/ln(ln(ln(x)))]^2-31213/6075*ln(4/3)^2*_var[1/ln(ln(ln(x)))]^2)*
    ln(5/14)*_var[1/ln(ln(ln(x)))]/(1/_var[
    1/ln(ln(ln(x)))])^(7/6)+1372/675*ln(4/3)^2*_var[1/ln(ln(ln(x)))]/
    (1/_var[1/ln(ln(ln(x)))])^(7/6)*(7/12*_var[
    1/ln(ln(ln(x)))]*ln(5/14)^2+91/72*ln(5/14)^2*_var[1/ln(ln(ln(x)))]^2))],
    Series([],1,integer,[],0,integer,_var[1/ln(ln(ln(x)))],1),t_SERIES,
    [1, 2, 3],4,rational,_var[1/ln(ln(x))],4/9/(ln(1+ln(5/14)*
    _var[1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(7/6)*
    (-7/6/(ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])*(-98/25/(1/_var[1/ln(ln(x))]+ln(5/14))*
    ln(4/3)^2-98/25/(1/_var[1/ln(ln(x))]+ln(5/14))^2*ln(4/3)^2)+
    4459/450/(ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^2/(1/_var[1/ln(ln(x))]+ln(5/14))^2*ln(4/3)^2))],
    Series([],Series([],1,integer,[],13/6,rational,_var[1/ln(ln(ln(x)))],
    -7/6/(1/_var[1/ln(ln(ln(x)))])^(7/6)*_var[1/ln(ln(ln(x)))]),
    t_SERIES,[],1,rational,_var[1/ln(ln(x))],((-7/6*_var[
    1/ln(ln(x))]+4802/375/(1/_var[1/ln(ln(x))]+ln(5/14))^2*ln(4/3)^3+
    9604/1125/(1/_var[1/ln(ln(x))]+ln(5/14))^3*ln(4/3)^3)/(ln(1+ln(5/14)*
    _var[1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])-637/90/
    (ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^2*
    (-98/25/(1/_var[1/ln(ln(x))]+ln(5/14))*ln(4/3)^2-98/25/(1/
    _var[1/ln(ln(x))]+ln(5/14))^2*ln(4/3)^2)/(1/_var[
    1/ln(ln(x))]+ln(5/14))*ln(4/3)+593047/20250/(ln(1+ln(5/14)*_var[
    1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^3/(1/_var[
    1/ln(ln(x))]+ln(5/14))^3*ln(4/3)^3)/(ln(1+ln(5/14)*_var[
    1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(7/6)),t_SERIES,
    [0, 1, 2],3,rational,_var[1/ln(x)],4/9/(ln(1+ln(1-14/5*
    ln(4/3)*_var[1/ln(x)])/(1/_var[1/ln(ln(x))]+
    ln(5/14)))+ln(1+ln(5/14)*_var[1/ln(ln(x))])+1/_var[
    1/ln(ln(ln(x)))])^(7/6))],0,t_SERIES,[0],infinity,rational,_var[
    1/x],4/9/(ln(1+ln(1-14/5*ln(4/3)*_var[1/ln(x)])/(1/_var[
    1/ln(ln(x))]+ln(5/14)))+ln(1+ln(5/14)*_var[1/ln(ln(x))])+
    1/_var[1/ln(ln(ln(x)))])^(7/6))],0,t_SERIES,[0],infinity,rational,
    _var[1/exp(54/17/(1/x)^(49/45))/exp(21/8/(1/x)^(6/11))],4/9/
    (ln(1+ln(1-14/5*ln(4/3)*_var[1/ln(x)])/(1/_var[
    1/ln(ln(x))]+ln(5/14)))+ln(1+ln(5/14)*_var[1/ln(ln(x))])+
    1/_var[1/ln(ln(ln(x)))])^(7/6))],0,t_SERIES,[-1],infinity,
    rational,_var[1/exp(exp(1/136*(340*x^8*(1/x)^(809/495)+
    357*x^(713/77)*(1/x)^(809/495)+272*x^(5/7)*(1/x)^(809/495)+
    432*x^(3088/315)*(1/x)^(809/495)-432*x^(61/7)*(1/x)^(6/11)-357*x^(61/7)*
    (1/x)^(49/45))/x^(61/7)/(1/x)^(809/495))^8*exp(54/17/(1/x)^(49/45))^8*
    exp(21/8/(1/x)^(6/11))^8)],4/9/(ln(1+ln(1-14/5*ln(4/3)*_var[
    1/ln(x)])/(1/_var[1/ln(ln(x))]+ln(5/14)))+ln(1+ln(5/14)*
    _var[1/ln(ln(x))])+1/_var[1/ln(ln(ln(x)))])^(7/6)/
    _var[1/exp(exp(1/136*(340*x^8*(1/x)^(809/495)+357*x^(713/77)*
    (1/x)^(809/495)+272*x^(5/7)*(1/x)^(809/495)+432*x^(3088/315)*(1/x)^(809/495)-
    432*x^(61/7)*(1/x)^(6/11)-357*x^(61/7)*(1/x)^(49/45))/x^(61/7)/
    (1/x)^(809/495))^8*exp(54/17/(1/x)^(49/45))^8*exp(21/8/(1/x)^(6/11))^8)]));
   unassign('s');

#end test
