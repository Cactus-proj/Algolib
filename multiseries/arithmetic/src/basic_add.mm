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

### Name                  : `multiseries/AddSeries/basic`
### Input                 :
###			     co1      list of coefficients in series #1
###                          co2      list of coefficients in series #2
###                          exp1     list of exponents of series #1
###			     exp2     list of exponents of series #2
###			     bigo1    exponent of O() term in series #1
###			     bigo2    exponent of O() term in series #2
### Output                :
###			     cofs::list,exps::list,bigo
###			     respectively the list of coefficients,
###				the list of exponents,
###				the exponent of the O() term
###			     in series #1 + series #2
### Description           :
###                         this procedure is used to add two
###                         SERIES data-structures in the case when the
###			    exponents can be compared with "<", "<=",...
###			    and the coefficients added with "+".
###		
### The computation proceeds in 3 steps:
### 1. locate the exponent in the O() term of the result
### 2. merge the lists of exponents and add the corresponding coefficients
###    only when an addition is necessary.
`multiseries/AddSeries/basic`:=proc(co1,co2,exp1,exp2,bigo1,bigo2)
local exps, cofs, bigo, i, j1, j2, n1, n2, last, j1max, j2max;
	n1:=nops(exp1); n2:=nops(exp2);
	if bigo1<=bigo2 then
	    bigo:=bigo1; j1max:=n1;
	    # this might be reduced by dichotomy, but I expect
	    # that quite often j1 and j2 are close.
	    for j2max from n2 by -1 to 1 while exp2[j2max]>=bigo do od
	else
	    bigo:=bigo2; j2max:=n2;
	    # same comment
	    for j1max from n1 by -1 to 1 while exp1[j1max]>=bigo do od
	fi;
	# merge with removal of duplicates
	j1:=1; j2:=1;i:=0;
	if n1<>0 and n2<>0 then
	     last:=min(exp1[n1],exp2[n2]);
	     for i do
	     	if exp1[j1]=exp2[j2] then
	     		exps[i]:=exp1[j1];
	     		cofs[i]:=co1[j1]+co2[j2];
	     		j1:=j1+1; j2:=j2+1
	     	elif exp1[j1]<exp2[j2] then
	     		exps[i]:=exp1[j1];
	     		cofs[i]:=co1[j1];
	     		j1:=j1+1
	     	else # exp1[j1]>exp2[j2]
	     		exps[i]:=exp2[j2];
	     		cofs[i]:=co2[j2];
	     		j2:=j2+1
	     	fi;
	     	if exps[i]=last then break fi
	     od
	fi;
	if j1>n1 then
	    cofs:=[seq(cofs[j1],j1=1..i),op(j2..j2max,co2)];
	    exps:=[seq(exps[j1],j1=1..i),op(j2..j2max,exp2)]
	elif j2>n2 then
	    cofs:=[seq(cofs[j2],j2=1..i),op(j1..j1max,co1)];
	    exps:=[seq(exps[j2],j2=1..i),op(j1..j1max,exp1)]
	fi;
	cofs,exps,bigo
end:
