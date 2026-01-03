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

#Tests from the new help page of rectoproc.

#test

with(TestTools):
with(gfun):

fiborec:={f(i)=f(i-1)+f(i-2),f(0)=1,f(1)=1};
fib1:=rectoproc(fiborec,f(i),remember);
Try(1,fib1(100),573147844013817084101);

fib2:=rectoproc(fiborec,f(i),list);
Try(2,fib2(10),[1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]);

fib3:=rectoproc(fiborec,f(i));
Try(3,fib3(100),573147844013817084101);
Try(4,fib3(1000),70330367711422815821835254877183549770181269836358732742604905087154537118196933579742249494562611733487750449241765991088186363265450223647106012053374121273867339111198139373125598767690091902245245323403501);


rec_u:={u(n+2)*(n^2+n+1)+u(n+1)*(n-2)+u(n)*n,u(0)=1,u(1)=2};
u_n := rectoproc(rec_u,u(n),remember);
rec_v:=v(n+3)*(n+4)+v(n+1)*(n^2+2*n)=u(n+1)-n*u(n);
ini_v:={v(0)=1,v(1)=2,v(2)=3};
v_n:=rectoproc({op(1,rec_v)}union ini_v,v(n),rhs=subs(u=u_n,op(2,rec_v)),list);
Try(5,v_n(9),[1, 2, 3, 1/2, -7/5, -17/9, 125/49, 6803/1092, -169567/17199, -423697/14105]);

rec := { u(n+2)*(n+1) + u(n)*(n^2+1),u(0)=sin(1),u(1)=cos(1)};
p:=subsop(4=NULL,rectoproc(rec,u(n),'plain'));
Try(6,evalhf(p(100)),.527739153869639746e77);

p2:=rectoproc(rec,u(n),evalfun='evalf');
Digits:=30:
Try(7,p2(100),.527739153869639769206242956896e77);

p3 := rectoproc(rec,u(n),evalfun='evalf',params=[d],postargs=[d]);
Try(8,p3(100,40),.5277391538696397692062429568990170086821e77);

rec := { u(n)*n + u(n+2),u(0)=A,u(1)=B}:
p:=rectoproc(rec,u(n),params=[p],extralocal=[A='f'(p),B='g'(A,p)]);
Try(9,p(3,a),-g(f(a),a));

#end test
