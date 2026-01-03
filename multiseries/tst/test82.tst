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

# test82.tst: test for MultiSeries[multiseries] (no. 82)

#test 18
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

   #=============== test nb 82 =====================
   Order:=3;
   s:=multiseries(ln(ln(1/x)*x+1/ln(x+ln(1/(1+x)))), x);
   TRY[compare_SERIES]("82-1",
    SERIES2Series(s),
    Series([ln(1-2*(1/(-2/_var[1/ln(1/x)]-ln(2))+1/2*_var[1/ln(1/x)])/
    _var[1/ln(1/x)])-1/_var[1/ln(ln(1/x))]-ln(2)+I*Pi],ln(1/x)^2,
    algebraic,[0],1,integer,_var[x],ln(1+(1/_var[1/ln(1/x)]*_var[x]+
    1/(ln(1+2*(_var[x]+ln(1/(1+_var[x]))-1/2*_var[x]^2)/
    _var[x]^2)-2/_var[1/ln(1/x)]-ln(2))-1/(-2/_var[1/ln(1/x)]-
    ln(2)))*(-2/_var[1/ln(1/x)]-ln(2)))+ln(1-2*(1/(-2/_var[1/ln(1/x)]-
    ln(2))+1/2*_var[1/ln(1/x)])/_var[1/ln(1/x)])-1/_var[1/ln(ln(1/x))]-
    ln(2)+I*Pi));
   TRY[compare_SERIES]("82-2",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][1..1])),
    Series([Series([Series([-1, -ln(2)+I*Pi],0,algebraic,[-1, 0],infinity,integer,
    _var[1/ln(ln(1/x))],-1/_var[1/ln(ln(1/x))]-ln(2)+I*Pi)],
    Series([],1/4,integer,[],0,integer,_var[1/ln(ln(1/x))],1/4*ln(2)),
    t_SERIES,[0],1,integer,_var[1/ln(1/x)],ln(1+(-2/(-2/_var[1/ln(1/x)]-
    ln(2))-_var[1/ln(1/x)])/_var[1/ln(1/x)])-1/_var[1/ln(ln(1/x))]-
    ln(2)+I*Pi)],
    Series([Series([-2],0,integer,[0],infinity,integer,_var[1/ln(ln(1/x))],-2)],
    Series([],1,integer,[],0,integer,_var[1/ln(ln(1/x))],-ln(2)),t_SERIES,[-2],
    -1,integer,_var[1/ln(1/x)],(2/3/(-2/_var[1/ln(1/x)]-ln(2))^2+1/
    _var[1/ln(1/x)])*(-2/_var[1/ln(1/x)]-ln(2))),t_SERIES,[0],1,
    integer,_var[x],ln(1+(1/_var[1/ln(1/x)]*_var[x]+1/(ln(1+(2*
    _var[x]+2*ln(1/(1+_var[x]))-_var[x]^2)/_var[x]^2)-2/
    _var[1/ln(1/x)]-ln(2))-1/(-2/_var[1/ln(1/x)]-ln(2)))*(-2/
    _var[1/ln(1/x)]-ln(2)))+ln(1+(-2/(-2/_var[1/ln(1/x)]-ln(2))-
    _var[1/ln(1/x)])/_var[1/ln(1/x)])-1/_var[1/ln(ln(1/x))]-ln(2)+I*Pi));
   TRY[compare_SERIES]("82-3",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][2..2])),
    Series([Series([-1/_var[1/ln(ln(1/x))]-ln(2)+I*Pi, -1/2*ln(2), 1/8*ln(2)^2],
    1,algebraic,[0, 1, 2],3,integer,_var[1/ln(1/x)],ln(1+(-2/(-2/_var[
    1/ln(1/x)]-ln(2))-_var[1/ln(1/x)])/_var[1/ln(1/x)])-1/_var[
    1/ln(ln(1/x))]-ln(2)+I*Pi)],
    Series([-2, -ln(2)],-1/2,algebraic,[-2, -1],1,integer,_var[1/ln(1/x)],
    (2/3/(-2/_var[1/ln(1/x)]-ln(2))^2+1/_var[1/ln(1/x)])*(-2/_var[
    1/ln(1/x)]-ln(2))),t_SERIES,[0],1,integer,_var[x],ln(1+(1/_var[1/ln(1/x)]*
    _var[x]+1/(ln(1+(2*_var[x]+2*ln(1/(1+_var[x]))-_var[x]^2)/
    _var[x]^2)-2/_var[1/ln(1/x)]-ln(2))-1/(-2/_var[1/ln(1/x)]-ln(2)))*
    (-2/_var[1/ln(1/x)]-ln(2)))+ln(1+(-2/(-2/_var[1/ln(1/x)]-ln(2))-
    _var[1/ln(1/x)])/_var[1/ln(1/x)])-1/_var[1/ln(ln(1/x))]-ln(2)+I*Pi));
   TRY[compare_SERIES]("82-5",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,exact_order)),
    Series([Series([Series([-1, -ln(2)+I*Pi],0,algebraic,[-1, 0],infinity,
    integer,_var[1/ln(ln(1/x))],-1/_var[1/ln(ln(1/x))]-ln(2)+I*Pi),
    Series([-1/2*ln(2)],0,algebraic,[0],infinity,integer,_var[1/ln(ln(1/x))],
    -1/2*ln(2)),
    Series([1/8*ln(2)^2],0,algebraic,[0],infinity,integer,_var[1/ln(ln(1/x))],
    1/8*ln(2)^2)],Series([],1,integer,[],0,integer,_var[1/ln(ln(1/x))],
    1+1/12*ln(2)^3),t_SERIES,[0, 1, 2],3,integer,_var[1/ln(1/x)],
    ln(1+(-2/(-2/_var[1/ln(1/x)]-ln(2))-_var[1/ln(1/x)])/
    _var[1/ln(1/x)])-1/_var[1/ln(ln(1/x))]-ln(2)+I*Pi),
    Series([Series([-2],0,integer,[0],infinity,integer,_var[1/ln(ln(1/x))],-2),
    Series([-ln(2)],0,algebraic,[0],infinity,integer,_var[1/ln(ln(1/x))],-ln(2)),
    Series([-1/3],0,rational,[0],infinity,integer,_var[1/ln(ln(1/x))],-1/3)],
    Series([],1,integer,[],0,integer,_var[1/ln(ln(1/x))],1),
    t_SERIES,[-2, -1, 1],2,integer,_var[1/ln(1/x)],(2/3/(-2/_var[1/ln(1/x)]-
    ln(2))^2+1/_var[1/ln(1/x)])*(-2/_var[1/ln(1/x)]-ln(2))),
    Series([Series([-2],0,integer,[0],infinity,integer,_var[1/ln(ln(1/x))],-2),
    Series([-2*ln(2)],0,algebraic,[0],infinity,integer,_var[1/ln(ln(1/x))],-2*ln(2)),
    Series([-1/2*ln(2)^2],0,algebraic,[0],infinity,integer,_var[1/ln(ln(1/x))],
    -1/2*ln(2)^2)],
    Series([],1,integer,[],0,integer,_var[1/ln(ln(1/x))],1),t_SERIES,
    [-4, -3, -2],-1,integer,_var[1/ln(1/x)],-5/18/(-2/_var[1/ln(1/x)]-
    ln(2))+4/9/(-2/_var[1/ln(1/x)]-ln(2))^2-1/2*(2/3/(-2/_var[1/ln(1/x)]-
    ln(2))^2+1/_var[1/ln(1/x)])^2*(-2/_var[1/ln(1/x)]-ln(2))^2)],
    Series([],Series([],1,integer,[],0,integer,_var[1/ln(ln(1/x))],-8/3),
    t_SERIES,[],-6,integer,_var[1/ln(1/x)],67/405/(-2/_var[1/ln(1/x)]-
    ln(2))-10/27/(-2/_var[1/ln(1/x)]-ln(2))^2+8/27/(-2/_var[1/ln(1/x)]-
    ln(2))^3+(5/18/(-2/_var[1/ln(1/x)]-ln(2))-4/9/(-2/_var[1/ln(1/x)]-
    ln(2))^2)*(2/3/(-2/_var[1/ln(1/x)]-ln(2))^2+1/_var[1/ln(1/x)])*
    (-2/_var[1/ln(1/x)]-ln(2))+1/3*(2/3/(-2/_var[1/ln(1/x)]-ln(2))^2+
    1/_var[1/ln(1/x)])^3*(-2/_var[1/ln(1/x)]-ln(2))^3),t_SERIES,
    [0, 1, 2],3,integer,_var[x],ln(1+(1/_var[1/ln(1/x)]*_var[x]+
    1/(ln(1+(2*_var[x]+2*ln(1/(1+_var[x]))-_var[x]^2)/
    _var[x]^2)-2/_var[1/ln(1/x)]-ln(2))-1/(-2/_var[1/ln(1/x)]-
    ln(2)))*(-2/_var[1/ln(1/x)]-ln(2)))+ln(1+(-2/(-2/_var[1/ln(1/x)]-
    ln(2))-_var[1/ln(1/x)])/_var[1/ln(1/x)])-1/_var[1/ln(ln(1/x))]-
    ln(2)+I*Pi));
   unassign('s');

#end test
