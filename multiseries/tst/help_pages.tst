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

#test 

with(TestTools):

with(MultiSeries):

SERIES2Series:=proc(s)
    if s=0 then 0 
    else subs(SERIES='Series',op(1,s)=NULL,s) 
    fi
end:

compare_SERIES:=proc(s1,s2)
local i;
    if s1=s2 then true 
    elif type([s1,s2],list(specfunc(anything,'Series'))) then
	evalb(op(3,s1)=op(3,s2) and 
	    (op(5,s1)=op(5,s2) or Testzero(op(5,s1)-op(5,s2))) and
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

############### MultiSeriespackage.i ######################
Try[compare_SERIES]("MultiSeriespackage-1",
op([assign(s=multiseries(GAMMA(x+exp(-x)),x=infinity,2)),
SERIES2Series(s)]),
Series([Series([Series([2^(1/2)*Pi^(1/2), 1/12*2^(1/2)*Pi^(1/2)],1,algebraic,[
1/2, 3/2],5/2,rational,_var[1/x],exp(1/_var[1/ln(x)]*(1/
_var[1/x]-1/2)+lnGAMMA(1/_var[1/x])+ln(_var[1
/x])/_var[1/x]-1/2*ln(_var[1/x])-(1/_var[1/ln
(x)]-1)/_var[1/x]+1/2/_var[1/ln(x)]-1/2*ln(2*Pi))*2^(1/2
)*Pi^(1/2)*_var[1/x]^(1/2))],Series([],1/_var[1/ln(x)]*2
^(1/2)*Pi^(1/2),algebraic,[],1/2,rational,_var[1/x],(1/_var[1/ln(x)]+_var[1/x]+1-(-1/_var[1/x]^2*Psi(1/_var
(x,infinity,1/x))-1/_var[1/x]^2*ln(_var[1/x])-1/2/_var[1/x])*_var[1/x]^2)*exp(1/_var[1/ln(x)]*(1/_var
(x,infinity,1/x)-1/2)+lnGAMMA(1/_var[1/x])+ln(_var[1/x])
/_var[1/x]-1/2*ln(_var[1/x])-(1/_var[1/ln(x)]
-1)/_var[1/x]+1/2/_var[1/ln(x)]-1/2*ln(2*Pi))*2^(1/2)*Pi
^(1/2)*_var[1/x]^(1/2)),t_SERIES,[0],1,integer,_var[1/
exp(x)],exp(lnGAMMA(1/_var[1/x]+_var[1/exp(x)])-1/_var[1/ln(x)]*(1/_var[1/x]-1/2)-lnGAMMA(1/_var[1/x]
)-ln(_var[1/x])/_var[1/x]+1/2*ln(_var[1/x]))*
exp(1/_var[1/ln(x)]*(1/_var[1/x]-1/2)+lnGAMMA(1/_var[1/x])+ln(_var[1/x])/_var[1/x]-1/2*ln(_var[1/x])-(1/_var[1/ln(x)]-1)/_var[1/x]+1/2/_var[1/ln(x)]-1/2*ln(2*Pi))*2^(1/2)*Pi^(1/2)*_var[1/x]^(1/2))],
0,t_SERIES,[-1],infinity,integer,_var[1/exp((ln(x)-1)*x)],2^(1/2)*
Pi^(1/2)*_var[1/x]^(1/2)/_var[1/exp((ln(x)-1)*x)]*exp(1/
_var[1/ln(x)]*(1/_var[1/x]-1/2)+lnGAMMA(1/_var[1/x])+ln(_var[1/x])/_var[1/x]-1/2*ln(_var[1/x])-(1/_var[1/ln(x)]-1)/_var[1/x]+1/2/_var[1/ln(x)]-1/2*ln(2*Pi))*exp(lnGAMMA(1/_var[1/x]+_var[1/exp(x)])-1/_var[1/ln(x)]*(1/_var[1/x]-1/2)-
lnGAMMA(1/_var[1/x])-ln(_var[1/x])/_var[1/x]+
1/2*ln(_var[1/x]))));
scale:=op(1,s):
Try("MultiSeriespackage-2",
scale[list],
[_var[1/ln(x)], _var[1/x], _var[1/exp(x)], 
_var[1/exp((ln(x)-1)*x)]]);
unassign('s');
Try("MultiSeriespackage-3",
LeadingTerm(ln(cot(ln(x)/x)),x=infinity),ln(x));
f := 1/(exp(-1/x)*(1-x)-1):
Try[compare_SERIES]("MultiSeriespackage-4",
op([assign(s=multiseries(f,x=0,3)),SERIES2Series(s)]),
Series([Series([-1],0,integer,[0],infinity,integer,_var[x],-1)],Series([-1
],1,algebraic,[0],1,integer,_var[x],_var[x]-1),t_SERIES,[0],1,integer,
_var[1/exp(1/x)],1/((1-_var[x])*_var[1/exp(1/x)]-1)));
scale:=op(1,s): 
Try("MultiSeriespackage-5",
scale[list],[_var[x], _var[1/exp(1/x)]]);
Try[compare_SERIES]("MultiSeriespackage-6",
SERIES2Series(multiseries(f,scale,3,scale[list][2..2])),
Series([-1, _var[x]-1, -(_var[x]-1)^2],1,algebraic,[0, 1, 2],3,integer
,_var[1/exp(1/x)],1/((1-_var[x])*_var[1/exp(1/x)]-1)));
Try[compare_SERIES]("MultiSeriespackage-7",
SERIES2Series(multiseries(f,scale,3)),
Series([Series([-1],0,integer,[0],infinity,integer,_var[x],-1), Series([-1,
 1],0,integer,[0, 1],infinity,integer,_var[x],_var[x]-1), Series([-1,
  2, -1],0,integer,[0, 1, 2],infinity,integer,_var[x],-(_var[x]-1)^2)],
Series([],1,integer,[],0,integer,_var[x],1),t_SERIES,[0, 1, 2],3,integer,
_var[1/exp(1/x)],1/((1-_var[x])*_var[1/exp(1/x)]-1)),
Series([Series([-1],0,integer,[0],infinity,integer,_var[x],-1), Series([-1
, 1],0,integer,[0, 1],infinity,integer,_var[x],_var[x]-1), Series([-1,
2, -1],1,integer,[0, 1, 2],4,integer,_var[x],-(_var[x]-1)^2)],Series([
],1,integer,[],0,integer,_var[x],1),t_SERIES,[0, 1, 2],3,integer,_var[
1/exp(1/x)],1/((1-_var[x])*_var[1/exp(1/x)]-1)));
unassign('s');

####################### newleadterm.i ###########################
Try("LeadingTerm-1",LeadingTerm(x^x, x=0),1);
Try("LeadingTerm-2",LeadingTerm(sqrt(sin(x)),x),x^(1/2));
Try("LeadingTerm-3",LeadingTerm(GAMMA(x+exp(-x))/GAMMA(x),x=infinity),1);

######################## multiseries.i ######################
Try[compare_SERIES]("multiseries-1",
SERIES2Series(multiseries(cos(x)+sin(x)-1-x,x,6)),
Series([-1/2, -1/6, 1/24, 1/120, -1/720],1,rational,[2, 3, 4, 5, 6],7,integer,
_var[x],cos(_var[x])+sin(_var[x])-1-_var[x]));
Try[compare_SERIES]("multiseries-2",
SERIES2Series(multiseries(cos(x)+sin(x)-1-x,x,6, 'exact_order')),
Series([-1/2, -1/6, 1/24, 1/120, -1/720, -1/5040],1,rational,[2, 3, 4, 5, 6, 7
],8,integer,_var[x],cos(_var[x])+sin(_var[x])-1-_var[x]));
Try[compare_SERIES]("multiseries-3",
SERIES2Series(multiseries(arccosh(x),x,4)),
Series([1/2*I*Pi, -I, -1/6*I],1,algebraic,[0, 1, 3],5,integer,_var[x],
arccosh(_var[x])));
f:=(1-sqrt(1-4*x))/2/x;
Try[compare_SERIES]("multiseries-4",
SERIES2Series(multiseries(f,x=1/4,3)),
Series([2, -2, 2],1,integer,[0, 1/2, 1],3/2,rational,_var[1-4*x],(1/2-1/
2*_var[1-4*x]^(1/2))/(1/4-1/4*_var[1-4*x])));
Try[compare_SERIES]("multiseries-5",
SERIES2Series(multiseries(subs(x=1/4+epsilon,f),epsilon,3)),
Series([2, -2*(-4)^(1/2), -8],1,algebraic,[0, 1/2, 1],3/2,rational,_var[epsilon],(1/2-1/2*(-4*_var[epsilon])^(1/2))/(1/4+_var[epsilon])));
Try[compare_SERIES]("multiseries-6",
SERIES2Series(multiseries(1/(x^3-x^4),x,3)),
Series([1, 1, 1],1,integer,[-3, -2, -1],0,integer,_var[x],1/(_var[x]^3
-_var[x]^4)));
f := ln(x)^2 - ln(x)*ln(x+exp(-x));
Try[compare_SERIES]("multiseries-7",
SERIES2Series(multiseries(f, x=infinity, 2)),
Series([Series([-1/_var[1/ln(x)]],0,algebraic,[1],infinity,integer,
_var[1/x],-_var[1/x]/_var[1/ln(x)])],Series([
],1,integer,[],2,integer,_var[1/x],_var[1/x]^2),t_SERIES
,[1],2,integer,_var[1/exp(x)],1/_var[1/ln(x)]^2-1/_var[1/ln(x)]*(ln(1+_var[1/exp(x)]*_var[1/x])+1/
_var[1/ln(x)])));
Try[compare_SERIES]("multiseries-8",
SERIES2Series(multiseries(exp(I/x+x),x,3)),
Series([exp(I/_var[x]), exp(I/_var[x]), 1/2*exp(I/_var[x])],1/6*
exp(I/_var[x]),algebraic,[0, 1, 2],3,integer,_var[x],exp(I/_var[x]
)*exp(_var[x])));
Try[compare_SERIES]("multiseries-9",
SERIES2Series(multiseries(exp(x^Pi+x^sqrt(2)),x,7)),
Series([1, 1, 1/2, 1, 1/6, 1, 1/24],1,rational,[0, 2^(1/2), 2*2^(1/2), Pi, 3*2
^(1/2), Pi+2^(1/2), 4*2^(1/2)],Pi+2*2^(1/2),algebraic,_var[x],exp(_var[x]^Pi+_var[x]^(2^(1/2)))));
f := 1/(1-z-exp(-1/z)):
Try[compare_SERIES]("multiseries-10",
op([assign(s=multiseries(f,z=0,3)),SERIES2Series(s)]),
Series([Series([1, 1, 1],1,integer,[0, 1, 2],3,integer,_var[z],1/(1-_var[z]))],Series([],1,algebraic,[],0,integer,_var[z],1/(1-_var[z])^2),
t_SERIES,[0],1,integer,_var[1/exp(1/z)],1/(1-_var[z]-_var[1/exp(1/
z)])));
scale:=op(1,s):
Try("multiseries-11",scale[list],
[_var[z], _var[1/exp(1/z)]]);
Try[compare_SERIES]("multiseries-12",
SERIES2Series(multiseries(f,scale,3)),
Series([Series([1, 1, 1],1,integer,[0, 1, 2],3,integer,_var[z],1/(1-_var[z])), Series([1, 2, 3],1,integer,[0, 1, 2],3,integer,_var[z],1/(1-_var[z])^2), Series([1, 3, 6],1,integer,[0, 1, 2],3,integer,_var[z],1/(1-
_var[z])^3)],Series([],1,integer,[],0,integer,_var[z],1),t_SERIES,[0,
1, 2],3,integer,_var[1/exp(1/z)],1/(1-_var[z]-_var[1/exp(1/z)])));
unassign('s');
####################### newasympt.i #########################
Try("newasympt-1",
member(asympt(x/(1-x-x^2),x),[-1/x+1/x^2-2/x^3+3/x^4-5/x^5+8/x^6+O(1/x^7),
        -1/x+1/x^2-2/x^3+3/x^4-5/x^5+O(1/x^6)]),true);
Try("newasympt-2",asympt(n!*exp(n)/n^n/sqrt(2*Pi),n,3),
1/(1/n)^(1/2)+1/12*(1/n)^(1/2)+1/288*(1/n)^(3/2)+O((1/n)^(5/2)));
Try[verify, normal]("newasympt-3",asympt(sqrt(Pi/2)*BesselJ(0,x),x,3),
(1/2*2^(1/2)*cos(x)+1/2*2^(1/2)*sin(x))*(1/x)^(1/2)+
(1/16*2^(1/2)*sin(x)-1/16*2^(1/2)*cos(x))*(1/x)^(3/2)
+(-9/256*2^(1/2)*cos(x)-9/256*2^(1/2)*sin(x))*(1/x)^(5/2)
+O((-75/2048*I*exp(-I*x)*2^(1/2)/Pi^(1/2)*(1/2*2^(1/2)+1/2*I*2^(1/2))
+75/2048*I*exp(I*x)*2^(1/2)/Pi^(1/2)*(1/2*2^(1/2)-1/2*I*2^(1/2)))*(1/x)^(7/2)));

################### newlimit.i ###############################
Try("newlimit-1",limit(sin(x)/x, x=0),1);
Try("newlimit-2",limit(exp(x), x=infinity),infinity);
Try("newlimit-3",limit(exp(x), x=-infinity),0);
Try("newlimit-4",limit(1/x, x=0),undefined);
f:=(sin(x)/x)^(sin(x)/x/(1-cos(x)));
Try("newlimit-5",limit(f,x=0),exp(-1/3));
Try("newlimit-6",limit(sin(x),x=infinity),undefined);

################### newseries.i #############################
Try("newseries-1",member(series(x/(1-x-x^2), x=0),
[series(1*x+1*x^2+2*x^3+3*x^4+5*x^5+8*x^6+O(x^7),x,7),
series(1*x+1*x^2+2*x^3+3*x^4+5*x^5+O(x^6),x,6)]),true);
Try("newseries-2",series(x+1/x, x=1, 3 ),series(2+1*(1-x)^2+O((1-x)^3),x=1,3));
Try("newseries-3",series(exp(x), x=0, 8 ),
series(1+1*x+1/2*x^2+1/6*x^3+1/24*x^4+1/120*x^5+1/720*x^6+1/5040*x^7+O(x^8),x,
8));
Try("newseries-4",series(exp(x)/x, x=0, 8 ),series(1*x^(-1)+1+1/2*x+1/6*x^2+1/24*x^3+1/120*x^4+1/720*x^5+1/5040*x^6+O(x^7)
,x,8));
Try("newseries-5",series(GAMMA(x), x=0, 2 ),series(1*x^(-1)+(-gamma)+O(x),x,2));
Try("newseries-6",member(series(x^3/(x^4+4*x-5),x=infinity),
[1/x-4/x^4+5/x^5+16/x^7-40/x^8+25/x^9+O(1/x^10),
1/x-4/x^4+5/x^5+O(1/x^7)
]),true);
Try("newseries-7",series(x^x, x=0, 3),subs(ln(x)=-ln(1/x),series(1+(-ln(1/x))*x+1/2*ln(1/x)^2*x^2+O(ln(1/x)^3)*x^3,x,3)));
Try("newseries-8",series(sqrt(sin(x)), x=0, 4),x^(1/2)-1/12*x^(5/2)+O(x^(9/2)));

######################## SeriesInfo.i ########################
with(SeriesInfo):
s:=multiseries(sin(x),x):
Try("SeriesInfo-1",ListExponents(s),[1,3,5]);
Try("SeriesInfo-2",ListCoefficients(s),[1, -1/6, 1/120]);
Try("SeriesInfo-3",CoefficientBigO(s),1);
Try("SeriesInfo-4",ExponentBigO(s),7);
Try("SeriesInfo-5",Variable(s),x);
S:=multiseries(GAMMA(x),x=infinity):
Try("SeriesInfo-6",Variable(S),1/exp((ln(x)-1)*x));
Try("SeriesInfo-7",CoefficientBigO(S),0);
Try("SeriesInfo-8",ExponentBigO(S),infinity);
listcoeff:=ListCoefficients(S):
L:=listcoeff[1]:
Try("SeriesInfo-9",Variable(L),1/x);
Try("SeriesInfo-10",ListCoefficients(L),[2^(1/2)*Pi^(1/2), 1/12*2^(1/2)*Pi^(1/2), 1/288*2^(1/2)*Pi^(1/2), -139/51840*2^(1/2)*Pi^(1/2),
-571/2488320*2^(1/2)*Pi^(1/2), 163879/209018880*2^(1/2)*Pi^(1/2)]);
Try("SeriesInfo-11",ListExponents(L),[1/2, 3/2, 5/2, 7/2, 9/2, 11/2]);

######################## scale.i #############################
kernelopts(opaquemodules=false);
forget(MultiSeries:-newscale);
scale:=op(1,multiseries(t,t=infinity)):
Try("scale-1",scale[list],[_var[1/t]]);
Try[compare_SERIES]("scale-2",
SERIES2Series(multiseries(exp(t+exp(-t)),scale,3)),
Series([Series([1],0,integer,[0],infinity,integer,_var[1/t],1)],
Series([],1,integer,[],0,integer,_var[1/t],1),t_SERIES,[-1],0,
integer,_var[1/exp(t)],exp(_var[1/exp(t)])/_var[1/exp(t)]));
Try("scale-3",scale[list],[_var[1/t], _var[1/exp(t)]]);

#end test

