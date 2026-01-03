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

# test121.tst: test for MultiSeries[multiseries] (no. 121)

#test 160

   # Normalizer:=proc(x) normal(x) end:
##   TESTZERO:=proc(x) evalb(normal(x)=0) end:
## Not good enough for this test
##   Testzero:=`limit/mrv/Testzero`:

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
       elif Testzero(s1-s2) then true
       else ERROR("different values:",s1,s2)
       fi
   end:

   #=============== test nb 121 =====================
   Order:=3;
   s:=multiseries(exp(exp(exp(Psi(Psi(Psi(x))))))/x,x=infinity);
   TestTools:-Try[compare_SERIES]("121-1",SERIES2Series(s),Series([
   exp(exp(exp(Psi(1/_var[1/ln(ln(x))])-1/_var[
   1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]-1/_var[
   1/ln(ln(x))]+1/2)*exp(-1/2)*exp(exp(Psi(1/_var[1/ln(ln(x))])-
   1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]*exp(
   ln(1+(Psi(1/_var[1/ln(x)])-1/_var[1/ln(ln(x))])*
   _var[1/ln(ln(x))])+Psi(Psi(1/_var[1/ln(x)]))+ln(1+
   1/_var[1/ln(ln(x))]*(1/Psi(1/_var[1/ln(x)])-_var[1/ln(ln(x))]))-Psi(1/_var[1/ln(ln(x))]))-exp(Psi(
   1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/
   _var[1/ln(ln(x))])/_var[1/ln(x)]-exp(exp(Psi(
   1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/
   _var[1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)*
   exp(-1/2)/_var[1/ln(x)]-(-1/2*_var[1/ln(ln(x))]+
   1/2*_var[1/ln(ln(x))]^2*(-Psi(1,1/_var[1/ln(ln(x))])/
   _var[1/ln(ln(x))]^2+1/_var[1/ln(ln(x))]))*exp(Psi(
   1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/
   _var[1/ln(ln(x))]*exp(exp(Psi(1/_var[1/ln(ln(x))])-
   1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)*exp(-1/2))*exp(-1/2*exp(-1/2))*exp((-1/2*_var[1/ln(ln(x))]+1/2*_var[1/ln(ln(x))]^2*(-Psi(1,1/_var[1/ln(ln(x))])/_var[1/ln(ln(x))]^2+1/_var[
   1/ln(ln(x))]))*exp(Psi(1/_var[1/ln(ln(x))])-1/_var[
   1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]*exp(exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)*exp(-1/2)+1/2*
   exp(-1/2))/_var[1/exp(-exp(-1/2)*ln(x))/exp(exp(exp(
   Psi(ln(ln(x)))-ln(ln(ln(x))))*ln(ln(x))-ln(ln(x))+1/2)*exp(-1/2)*
   ln(x))], (1/(1/_var[1/ln(ln(x))]+Psi(1/_var[
   1/ln(x)])+ln(_var[1/ln(x)]))*(-1/2*_var[
   1/ln(x)]+1/2*_var[1/ln(x)]^2*(-1/_var[
   1/ln(x)]^2*Psi(1,1/_var[1/ln(x)])+1/_var[
   1/ln(x)]))-1/(1/_var[1/ln(ln(x))]+Psi(1/_var[
   1/ln(x)])+ln(_var[1/ln(x)]))^2*(-1/2*_var[
   1/ln(x)]+1/2*_var[1/ln(x)]^2*(-1/_var[1/ln(x)]^2*
   Psi(1,1/_var[1/ln(x)])+1/_var[1/ln(x)]))*(-(1/
   _var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))^2*Psi(1,1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))+1/_var[
   1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+ln(_var[
   1/ln(x)])))*exp(Psi(1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]*exp(ln(
   1+(Psi(1/_var[1/ln(x)])-1/_var[1/ln(ln(x))])*
   _var[1/ln(ln(x))])+Psi(Psi(1/_var[1/ln(x)]))+
   ln(1+1/_var[1/ln(ln(x))]*(1/Psi(1/_var[1/ln(x)])-
   _var[1/ln(ln(x))]))-1/_var[1/ln(ln(ln(x)))]-
   Psi(1/_var[1/ln(ln(x))])-ln(_var[1/ln(ln(x))]))*
   exp(exp(Psi(1/_var[1/ln(ln(x))])-1/_var[
   1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]-1/_var[
   1/ln(ln(x))]+1/2)*exp(-1/2)*exp(exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))]*exp(ln(1+(Psi(1/_var[1/ln(x)])-1/_var[1/ln(ln(x))])*_var[1/ln(ln(x))])+Psi(Psi(1/_var[1/ln(x)]))+ln(1+1/_var[1/ln(ln(x))]*(1/Psi(1/_var[1/ln(x)])-_var[1/ln(ln(x))]))-Psi(1/_var[1/ln(ln(x))]))-exp(Psi(1/_var[1/ln(ln(x))])-
   1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))])/
   _var[1/ln(x)]*exp(exp(exp(Psi(1/_var[1/ln(ln(x))])-
   1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]-
   1/_var[1/ln(ln(x))]+1/2)*exp(-1/2)*exp(exp(Psi(1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]*exp(ln(1+(Psi(1/_var[1/ln(x)])-
   1/_var[1/ln(ln(x))])*_var[1/ln(ln(x))])+Psi(Psi(
   1/_var[1/ln(x)]))+ln(1+1/_var[1/ln(ln(x))]*(
   1/Psi(1/_var[1/ln(x)])-_var[1/ln(ln(x))]))-
   Psi(1/_var[1/ln(ln(x))]))-exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))])/_var[1/ln(x)]-exp(exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)*exp(-1/2)/_var[1/ln(x)]-(-1/2*_var[1/ln(ln(x))]+1/2*_var[1/ln(ln(x))]^2*(-Psi(1,1/_var[1/ln(ln(x))])/
   _var[1/ln(ln(x))]^2+1/_var[1/ln(ln(x))]))*
   exp(Psi(1/_var[1/ln(ln(x))])-1/_var[
   1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]*exp(exp(Psi(
   1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/
   _var[1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)*
   exp(-1/2))*exp(-1/2*exp(-1/2))*exp((-1/2*_var[1/ln(ln(x))]+
   1/2*_var[1/ln(ln(x))]^2*(-Psi(1,1/_var[1/ln(ln(x))])/
   _var[1/ln(ln(x))]^2+1/_var[1/ln(ln(x))]))*
   exp(Psi(1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/
   _var[1/ln(ln(x))]*exp(exp(Psi(1/_var[1/ln(ln(x))])-
   1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]-
   1/_var[1/ln(ln(x))]+1/2)*exp(-1/2)+1/2*exp(-1/2))/
   _var[1/exp(-exp(-1/2)*ln(x))/exp(exp(exp(Psi(ln(ln(x)))-
   ln(ln(ln(x))))*ln(ln(x))-ln(ln(x))+1/2)*exp(-1/2)*ln(x))], (((
   1/(1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+
   ln(_var[1/ln(x)]))*(-1/12*_var[1/ln(x)]-1/8*
   _var[1/ln(x)]^2+_var[1/ln(x)]*(1/12*_var[1/ln(x)]+1/4*_var[1/ln(x)]^2)*(-1/_var[
   1/ln(x)]^2*Psi(1,1/_var[1/ln(x)])+1/_var[1/ln(x)])+
   1/4*_var[1/ln(x)]^4*(1/_var[1/ln(x)]^3*Psi(1,
   1/_var[1/ln(x)])+1/2/_var[1/ln(x)]^4*Psi(2,
   1/_var[1/ln(x)])-1/2/_var[1/ln(x)]^2))-1/2/(
   1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+ln(
   _var[1/ln(x)]))^2*(-1/2*_var[1/ln(x)]+1/2*
   _var[1/ln(x)]^2*(-1/_var[1/ln(x)]^2*Psi(1,
   1/_var[1/ln(x)])+1/_var[1/ln(x)]))^2+1/(
   1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+
   ln(_var[1/ln(x)]))*(-1/(1/_var[1/ln(ln(x))]+
   Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))*
   (-1/12*_var[1/ln(x)]-1/8*_var[1/ln(x)]^2+
   _var[1/ln(x)]*(1/12*_var[1/ln(x)]+1/4*
   _var[1/ln(x)]^2)*(-1/_var[1/ln(x)]^2*
   Psi(1,1/_var[1/ln(x)])+1/_var[1/ln(x)])+
   1/4*_var[1/ln(x)]^4*(1/_var[1/ln(x)]^3*
   Psi(1,1/_var[1/ln(x)])+1/2/_var[1/ln(x)]^4*
   Psi(2,1/_var[1/ln(x)])-1/2/_var[1/ln(x)]^2))+
   1/(1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+
   ln(_var[1/ln(x)]))^2*(-1/2*_var[1/ln(x)]+1/2*
   _var[1/ln(x)]^2*(-1/_var[1/ln(x)]^2*Psi(1,
   1/_var[1/ln(x)])+1/_var[1/ln(x)]))^2)*(-(
   1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+
   ln(_var[1/ln(x)]))^2*Psi(1,1/_var[1/ln(ln(x))]+
   Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))+
   1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+
   ln(_var[1/ln(x)]))+1/(1/_var[1/ln(ln(x))]+
   Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))^4*
   (-1/2*_var[1/ln(x)]+1/2*_var[1/ln(x)]^2*
   (-1/_var[1/ln(x)]^2*Psi(1,1/_var[1/ln(x)])+
   1/_var[1/ln(x)]))^2*((1/_var[1/ln(ln(x))]+
   Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))^3*
   Psi(1,1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+
   ln(_var[1/ln(x)]))+1/2*(1/_var[1/ln(ln(x))]+
   Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))^4*
   Psi(2,1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+
   ln(_var[1/ln(x)]))-1/2*(1/_var[1/ln(ln(x))]+
   Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))^2)+
   1/2*(1/(1/_var[1/ln(ln(x))]+Psi(1/_var[
   1/ln(x)])+ln(_var[1/ln(x)]))*(-1/2*_var[
   1/ln(x)]+1/2*_var[1/ln(x)]^2*(-1/_var[
   1/ln(x)]^2*Psi(1,1/_var[1/ln(x)])+1/_var[
   1/ln(x)]))-1/(1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))^2*(-1/2*_var[1/ln(x)]+1/2*_var[1/ln(x)]^2*(-1/_var[1/ln(x)]^2*Psi(1,1/_var[1/ln(x)])+1/_var[1/ln(x)]))*(-(1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))^2*Psi(1,1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))+1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)])))^2)*exp(Psi(
   1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/
   _var[1/ln(ln(x))]*exp(ln(1+(Psi(1/_var[
   1/ln(x)])-1/_var[1/ln(ln(x))])*_var[
   1/ln(ln(x))])+Psi(Psi(1/_var[1/ln(x)]))+ln(1+
   1/_var[1/ln(ln(x))]*(1/Psi(1/_var[
   1/ln(x)])-_var[1/ln(ln(x))]))-1/_var[
   1/ln(ln(ln(x)))]-Psi(1/_var[1/ln(ln(x))])-ln(_var[1/ln(ln(x))]))+1/2*(1/(1/_var[1/ln(ln(x))]+
   Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))*
   (-1/2*_var[1/ln(x)]+1/2*_var[1/ln(x)]^2*
   (-1/_var[1/ln(x)]^2*Psi(1,1/_var[1/ln(x)])+
   1/_var[1/ln(x)]))-1/(1/_var[1/ln(ln(x))]+
   Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))^2*
   (-1/2*_var[1/ln(x)]+1/2*_var[1/ln(x)]^2*
   (-1/_var[1/ln(x)]^2*Psi(1,1/_var[1/ln(x)])+
   1/_var[1/ln(x)]))*(-(1/_var[1/ln(ln(x))]+
   Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))^2*
   Psi(1,1/_var[1/ln(ln(x))]+Psi(1/_var[
   1/ln(x)])+ln(_var[1/ln(x)]))+1/_var[
   1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+ln(_var[
   1/ln(x)])))^2*exp(Psi(1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])^2/_var[1/ln(ln(x))]^2*
   exp(ln(1+(Psi(1/_var[1/ln(x)])-1/_var[
   1/ln(ln(x))])*_var[1/ln(ln(x))])+Psi(Psi(1/_var[1/ln(x)]))+ln(1+1/_var[1/ln(ln(x))]*(1/Psi(
   1/_var[1/ln(x)])-_var[1/ln(ln(x))]))-1/
   _var[1/ln(ln(ln(x)))]-Psi(1/_var[1/ln(ln(x))])-
   ln(_var[1/ln(ln(x))]))^2)*exp(exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)*exp(-1/2)*
   exp(exp(Psi(1/_var[1/ln(ln(x))])-1/_var[
   1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]*exp(ln(1+(Psi(
   1/_var[1/ln(x)])-1/_var[1/ln(ln(x))])*
   _var[1/ln(ln(x))])+Psi(Psi(1/_var[1/ln(x)]))+
   ln(1+1/_var[1/ln(ln(x))]*(1/Psi(1/_var[1/ln(x)])-
   _var[1/ln(ln(x))]))-Psi(1/_var[1/ln(ln(x))]))-
   exp(Psi(1/_var[1/ln(ln(x))])-1/_var[
   1/ln(ln(ln(x)))])/_var[1/ln(ln(x))])/_var[
   1/ln(x)]+1/2*(1/(1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))*(-1/2*_var[1/ln(x)]+1/2*_var[1/ln(x)]^2*(-1/_var[
   1/ln(x)]^2*Psi(1,1/_var[1/ln(x)])+1/_var[1/ln(x)]))-
   1/(1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+
   ln(_var[1/ln(x)]))^2*(-1/2*_var[1/ln(x)]+1/2*
   _var[1/ln(x)]^2*(-1/_var[1/ln(x)]^2*Psi(1,
   1/_var[1/ln(x)])+1/_var[1/ln(x)]))*(-(1/
   _var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+
   ln(_var[1/ln(x)]))^2*Psi(1,1/_var[1/ln(ln(x))]+
   Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))+
   1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+
   ln(_var[1/ln(x)])))^2*exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])^2/_var[1/ln(ln(x))]^2*exp(ln(1+(Psi(1/_var[1/ln(x)])-
   1/_var[1/ln(ln(x))])*_var[1/ln(ln(x))])+
   Psi(Psi(1/_var[1/ln(x)]))+ln(1+1/_var[
   1/ln(ln(x))]*(1/Psi(1/_var[1/ln(x)])-_var[
   1/ln(ln(x))]))-1/_var[1/ln(ln(ln(x)))]-Psi(1/_var[1/ln(ln(x))])-ln(_var[1/ln(ln(x))]))^2*exp(exp(
   Psi(1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/
   _var[1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)^2*
   exp(-1/2)^2*exp(exp(Psi(1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]*exp(ln(1+
   (Psi(1/_var[1/ln(x)])-1/_var[1/ln(ln(x))])*
   _var[1/ln(ln(x))])+Psi(Psi(1/_var[1/ln(x)]))+
   ln(1+1/_var[1/ln(ln(x))]*(1/Psi(1/_var[1/ln(x)])-
   _var[1/ln(ln(x))]))-Psi(1/_var[1/ln(ln(x))]))-
   exp(Psi(1/_var[1/ln(ln(x))])-1/_var[
   1/ln(ln(ln(x)))])/_var[1/ln(ln(x))])^2/_var[
   1/ln(x)]^2)*exp(exp(exp(Psi(1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)*exp(-1/2)*exp(exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))]*exp(ln(1+(Psi(1/_var[1/ln(x)])-1/_var[1/ln(ln(x))])*_var[1/ln(ln(x))])+Psi(Psi(1/
   _var[1/ln(x)]))+ln(1+1/_var[1/ln(ln(x))]*(1/
   Psi(1/_var[1/ln(x)])-_var[1/ln(ln(x))]))-
   Psi(1/_var[1/ln(ln(x))]))-exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))])/_var[1/ln(x)]-exp(exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)*exp(-1/2)/_var[1/ln(x)]-(-1/2*_var[1/ln(ln(x))]+1/2*_var[1/ln(ln(x))]^2*(-Psi(1,1/_var[1/ln(ln(x))])/
   _var[1/ln(ln(x))]^2+1/_var[1/ln(ln(x))]))*
   exp(Psi(1/_var[1/ln(ln(x))])-1/_var[
   1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]*exp(exp(Psi(
   1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/
   _var[1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)*
   exp(-1/2))*exp(-1/2*exp(-1/2))*exp((-1/2*_var[1/ln(ln(x))]+
   1/2*_var[1/ln(ln(x))]^2*(-Psi(1,1/_var[
   1/ln(ln(x))])/_var[1/ln(ln(x))]^2+1/_var[
   1/ln(ln(x))]))*exp(Psi(1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]*exp(exp(
   Psi(1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(
   ln(x)))])/_var[1/ln(ln(x))]-1/_var[1/ln(
   ln(x))]+1/2)*exp(-1/2)+1/2*exp(-1/2))/_var[1/exp(-exp(-1/2)*
   ln(x))/exp(exp(exp(Psi(ln(ln(x)))-ln(ln(ln(x))))*ln(ln(x))-ln(ln(x))+
   1/2)*exp(-1/2)*ln(x))]],ln(ln(x))^6*ln(x)^3,algebraic,[1-exp(-1/2),
   2-exp(-1/2), 3-exp(-1/2)],4-exp(-1/2),algebraic,_var[1/x],
   exp(exp(exp(Psi(1/_var[1/ln(ln(x))])-1/_var[
   1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]-1/_var[
   1/ln(ln(x))]+1/2)*exp(-1/2)*exp(exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))]*exp(ln(1+(Psi(1/_var[1/ln(x)])-1/_var[1/ln(ln(x))])*_var[1/ln(ln(x))])+Psi(Psi(
   1/_var[1/ln(x)]))+ln(1+1/_var[1/ln(ln(x))]*
   (1/Psi(1/_var[1/ln(x)])-_var[1/ln(ln(x))]))-
   Psi(1/_var[1/ln(ln(x))]))-exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))])/_var[1/ln(x)]-exp(exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)*exp(-1/2)/_var[1/ln(x)]-(-1/2*_var[1/ln(ln(x))]+1/2*_var[1/ln(ln(x))]^2*(-Psi(1,1/_var[1/ln(ln(x))])/
   _var[1/ln(ln(x))]^2+1/_var[1/ln(ln(x))]))*
   exp(Psi(1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(
   ln(x)))])/_var[1/ln(ln(x))]*exp(exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)*exp(-1/2))*exp(-1/
   2*exp(-1/2))*exp((-1/2*_var[1/ln(ln(x))]+1/2*_var[1/ln(ln(x))]^2*(-Psi(1,1/_var[1/ln(ln(x))])/
   _var[1/ln(ln(x))]^2+1/_var[1/ln(ln(x))]))*
   exp(Psi(1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(
   ln(x)))])/_var[1/ln(ln(x))]*exp(exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)*exp(-1/2)+1/2*
   exp(-1/2))/_var[1/exp(-exp(-1/2)*ln(x))/exp(exp(exp(
   Psi(ln(ln(x)))-ln(ln(ln(x))))*ln(ln(x))-ln(ln(x))+1/2)*exp(-1/2)*
   ln(x))]*exp(exp(exp(Psi(1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]-1/_var[1/ln(ln(x))]+1/2)*exp(-1/2)*exp(exp(Psi(1/_var[
   1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/_var[
   1/ln(ln(x))]*exp(ln(1+(Psi(1/_var[1/ln(x)])-1/_var[1/ln(ln(x))])*_var[1/ln(ln(x))])+Psi(Psi(1/
   _var[1/ln(x)]))+ln(1+1/_var[1/ln(ln(x))]*(1/
   Psi(1/_var[1/ln(x)])-_var[1/ln(ln(x))]))-Psi(1/
   _var[1/ln(ln(x))]))-exp(Psi(1/_var[1/ln(ln(x))])-
   1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))])/
   _var[1/ln(x)]*exp(exp(Psi(1/_var[1/ln(ln(x))])-
   1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]*
   exp(ln(1+(Psi(1/_var[1/ln(x)])-1/_var[
   1/ln(ln(x))])*_var[1/ln(ln(x))])+Psi(Psi(1/_var[
   1/ln(x)]))+ln(1+1/_var[1/ln(ln(x))]*(1/Psi(1/_var[
   1/ln(x)])-_var[1/ln(ln(x))]))-1/_var[1/
   ln(ln(ln(x)))]-Psi(1/_var[1/ln(ln(x))])-ln(_var[
   1/ln(ln(x))]))*exp(Psi(Psi(Psi(1/_var[1/x])))-ln(1+(Psi(
   1/_var[1/ln(x)])-1/_var[1/ln(ln(x))])*_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))]-Psi(1/_var[1/ln(ln(x))]+Psi(1/_var[1/ln(x)])+ln(_var[1/ln(x)]))-ln(1/(1/_var[1/ln(ln(x))]+Psi(
   1/_var[1/ln(x)])+ln(_var[1/ln(x)]))))-
   exp(Psi(1/_var[1/ln(ln(x))])-1/_var[
   1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]*exp(ln(1+(Psi(
   1/_var[1/ln(x)])-1/_var[1/ln(ln(x))])*
   _var[1/ln(ln(x))])+Psi(Psi(1/_var[1/ln(x)]))+
   ln(1+1/_var[1/ln(ln(x))]*(1/Psi(1/_var[1/ln(x)])-
   _var[1/ln(ln(x))]))-1/_var[1/ln(ln(ln(x)))]-
   Psi(1/_var[1/ln(ln(x))])-ln(_var[1/ln(ln(x))])))-
   exp(exp(Psi(1/_var[1/ln(ln(x))])-1/_var[
   1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]-1/_var[
   1/ln(ln(x))]+1/2)*exp(-1/2)*exp(exp(Psi(1/_var[1/ln(ln(x))])-
   1/_var[1/ln(ln(ln(x)))])/_var[1/ln(ln(x))]*exp(
   ln(1+(Psi(1/_var[1/ln(x)])-1/_var[1/ln(ln(x))])*
   _var[1/ln(ln(x))])+Psi(Psi(1/_var[1/ln(x)]))+
   ln(1+1/_var[1/ln(ln(x))]*(1/Psi(1/_var[1/ln(x)])-
   _var[1/ln(ln(x))]))-Psi(1/_var[1/ln(ln(x))]))-
   exp(Psi(1/_var[1/ln(ln(x))])-1/_var[1/ln(ln(ln(x)))])/
   _var[1/ln(ln(x))])/_var[1/ln(x)])*
   _var[1/x]^(-exp(-1/2))*_var[1/x]));
   Testzero:=proc(x) evalb(combine(expand(normal(x)),ln,'symbolic')=0) end:
   unassign('s');

#end test
