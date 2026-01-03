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

# test50.tst: test for MultiSeries[multiseries] (no. 50)

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

   #=============== test nb 50 =====================
   Order:=6;
   TRY[compare_SERIES]("50-1",
    op([assign(s=multiseries(op([exp(1/(1/x+exp(-1/x))), x, 6, exact_order]))),
    SERIES2Series(s)]),
    Series([Series([1, 1, 1/2, 1/6, 1/24, 1/120],1,rational,[0, 1, 2, 3, 4, 5],6,
    integer,_var[x],exp(_var[x]))],
    Series([],1,integer,[],2,integer,_var[x],-_var[x]^2),t_SERIES,[0],1,
    integer,_var[1/exp(1/x)],
    exp(_var[x])*exp(1/(1/_var[x]+_var[1/exp(1/x)])-_var[x])));
   TRY[compare_SERIES]("50-2",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,op(1,s)[list][2..2])),
    Series([exp(_var[x]), -_var[x]^2*exp(_var[x]),
    (_var[x]^3+1/2*_var[x]^4)*exp(_var[x]),
    (-_var[x]^4-_var[x]^5-1/6*_var[x]^6)*exp(_var[x]),
    (_var[x]^5+3/2*_var[x]^6+1/2*_var[x]^7+1/24*_var[x]^8)*exp(_var[x]),
    (-_var[x]^6-2*_var[x]^7-_var[x]^8-1/6*_var[x]^9-1/120*
    _var[x]^10)*exp(_var[x])],x^7,algebraic,[0, 1, 2, 3, 4, 5],6,
    integer,_var[1/exp(1/x)],
    exp(_var[x])*exp(1/(1/_var[x]+_var[1/exp(1/x)])-_var[x])));
   TRY[compare_SERIES]("50-3",
member(
    SERIES2Series(multiseries(op(9,s),op(1,s))),
[
    Series([Series([1, 1, 1/2, 1/6, 1/24, 1/120],1,rational,[0, 1, 2, 3, 4, 5],6,
    integer,_var[x],exp(_var[x])),
    Series([-1, -1, -1/2, -1/6, -1/24, -1/120],1,rational,[2, 3, 4, 5, 6, 7],8,
    integer,_var[x],-_var[x]^2*exp(_var[x])),
    Series([1, 3/2, 1, 5/12, 1/8, 7/240],1,rational,[3, 4, 5, 6, 7, 8],9,
    integer,_var[x],(_var[x]^3+1/2*_var[x]^4)*exp(_var[x])),
    Series([-1, -2, -5/3, -5/6, -7/24, -7/90],1,rational,[4, 5, 6, 7, 8, 9],10,
    integer,_var[x],(-_var[x]^4-_var[x]^5-1/6*_var[x]^6)*exp(_var[x])),
    Series([1, 5/2, 5/2, 35/24, 7/12, 7/40],1,rational,[5, 6, 7, 8, 9, 10],11,
    integer,_var[x],(_var[x]^5+3/2*_var[x]^6+1/2*_var[x]^7+1/24*
    _var[x]^8)*exp(_var[x])),
    Series([-1, -3, -7/2, -7/3, -21/20, -7/20],1,rational,[6, 7, 8, 9, 10, 11],12,
    integer,_var[x],(-_var[x]^6-2*_var[x]^7-_var[x]^8-1/6*
    _var[x]^9-1/120*_var[x]^10)*exp(_var[x]))],
    Series([],1,integer,[],7,integer,_var[x],_var[x]^7+5/2*_var[x]^8+
    5/3*_var[x]^9+5/12*_var[x]^10+1/24*_var[x]^11+1/720*
    _var[x]^12),t_SERIES,[0, 1, 2, 3, 4, 5],6,integer,_var[1/exp(1/x)],
    exp(_var[x])*exp(1/(1/_var[x]+_var[1/exp(1/x)])-_var[x])),
# different number of terms
    Series([Series([1, 1, 1/2, 1/6, 1/24, 1/120],1,rational,[0, 1, 2, 3, 4, 5],6,
    integer,_var[x],exp(_var[x])),
    Series([-1, -1, -1/2, -1/6, -1/24, -1/120],1,rational,[2, 3, 4, 5, 6, 7],8,
    integer,_var[x],-_var[x]^2*exp(_var[x])),
    Series([1, 3/2, 1, 5/12, 1/8, 7/240],1,rational,[3, 4, 5, 6, 7, 8],9,
    integer,_var[x],(_var[x]^3+1/2*_var[x]^4)*exp(_var[x])),
    Series([-1, -2],1,integer,[4, 5],6,
    integer,_var[x],(-_var[x]^4-_var[x]^5-1/6*_var[x]^6)*exp(_var[x])),
    Series([1],1,integer,[5],6,
    integer,_var[x],(_var[x]^5+3/2*_var[x]^6+1/2*_var[x]^7+1/24*
    _var[x]^8)*exp(_var[x])),
    Series([-1, -3, -7/2, -7/3, -21/20, -7/20],1,rational,[6, 7, 8, 9, 10, 11],12,
    integer,_var[x],(-_var[x]^6-2*_var[x]^7-_var[x]^8-1/6*
    _var[x]^9-1/120*_var[x]^10)*exp(_var[x]))],
    Series([],1,integer,[],7,integer,_var[x],_var[x]^7+5/2*_var[x]^8+
    5/3*_var[x]^9+5/12*_var[x]^10+1/24*_var[x]^11+1/720*
    _var[x]^12),t_SERIES,[0, 1, 2, 3, 4, 5],6,integer,_var[1/exp(1/x)],
    exp(_var[x])*exp(1/(1/_var[x]+_var[1/exp(1/x)])-_var[x]))
]),true
);
   TRY[compare_SERIES]("50-4",
    SERIES2Series(multiseries(op(9,s),op(1,s),Order,exact_order)),
    Series([Series([1, 1, 1/2, 1/6, 1/24, 1/120],1,rational,[0, 1, 2, 3, 4, 5],
    6,integer,_var[x],exp(_var[x])),
    Series([-1, -1, -1/2, -1/6, -1/24, -1/120],1,rational,[2, 3, 4, 5, 6, 7],8,
    integer,_var[x],-_var[x]^2*exp(_var[x])),
    Series([1, 3/2, 1, 5/12, 1/8, 7/240],1,rational,[3, 4, 5, 6, 7, 8],9,integer,
    _var[x],(_var[x]^3+1/2*_var[x]^4)*exp(_var[x])),
    Series([-1, -2, -5/3, -5/6, -7/24, -7/90],1,rational,[4, 5, 6, 7, 8, 9],10,
    integer,_var[x],(-_var[x]^4-_var[x]^5-1/6*_var[x]^6)*
    exp(_var[x])),
    Series([1, 5/2, 5/2, 35/24, 7/12, 7/40],1,rational,[5, 6, 7, 8, 9, 10],11,
    integer,_var[x],(_var[x]^5+3/2*_var[x]^6+1/2*_var[x]^7+
    1/24*_var[x]^8)*exp(_var[x])),
    Series([-1, -3, -7/2, -7/3, -21/20, -7/20],1,rational,[6, 7, 8, 9, 10, 11],
    12,integer,_var[x],(-_var[x]^6-2*_var[x]^7-_var[x]^8-1/6*
    _var[x]^9-1/120*_var[x]^10)*exp(_var[x]))],
    Series([],1,integer,[],7,integer,_var[x],_var[x]^7+5/2*_var[x]^8+
    5/3*_var[x]^9+5/12*_var[x]^10+1/24*_var[x]^11+1/720*_var[x]^12),
    t_SERIES,[0, 1, 2, 3, 4, 5],6,integer,_var[1/exp(1/x)],
    exp(_var[x])*exp(1/(1/_var[x]+_var[1/exp(1/x)])-_var[x])));
   unassign('s');

#end test
