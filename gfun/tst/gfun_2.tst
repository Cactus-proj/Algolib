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

# tests rectoproc

#test 10

with(TestTools):
with(gfun):

########## Test from the help page
res1:= rectoproc(f(i)=f(i-1)+f(i-2),f(i),remember):
Try(1, res1(5) ,5*_C[1]+3*_C[0]);

########### Test from the help page

Try(2, rectoproc({f(i)=f(i-1)+f(i-2),f(0)=0,f(1)=1},f(i))(50),12586269025);

##########
res1:=rectoproc({f(i)=f(i-1)+f(i-2),f(0)=0,f(1)=1},f(i))(100):
res2:=354224848179261915075:
if res1=res2 then okay else res1,res2 fi;
Try(3, rectoproc({f(i)=f(i-1)+f(i-2),f(0)=0,f(1)=1},f(i))(100),354224848179261915075);

########### Test from the help page

Try(4, rectoproc({f(i)=f(i-1)+f(i-2),f(0)=1,f(1)=1},f(i),list)(10),[1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89]);

##########

Try(5, rectoproc({(1-n**2)*u(n+1)+3/2*u(n-1)+(1/2+3/2*n+n**2)*u(n),u(0)=1,u(2)=a}
,u(n))(10), 86773903/3440640*a-41665019/17203200);

###########

Try(6, rectoproc(f(i)=f(i-1)+f(i-2),f(i))(100),354224848179261915075*_C[1]+218922995834555169026*_C[0]);

##########

r_proc:=rectoproc({u(1) = 0, u(3) = 0, u(5) = 0, u(2) = -4/3*u(0),u(4)=u(0),
(2*n+n^2)*u(n)+(14*n+16+3*n^2)*u(2+n)+(30*n+56+4*n^2)*u(n+4)
+(48+2*n^2+20*n)*u(n+6)},u(n),remember):##

res1:=[seq(r_proc(i),i=0..10)]:
res2:=[_C[0], 0, -4/3*_C[0], 0, _C[0], 0, -13/18*_C[0], 0, 25/48*_C[0],
0, -187/480*_C[0]]:
Try(7, res1, res2);


##########

res1:=rectoproc({(-2*nu+2*n)*u(n)+(n^2+3*n+2)*u(n+2),u(0)=1,u(1)=2},u(n),
'remember',params=[nu]):
Try(8, res1(5,2), -1/15);


##########

Try(9,rectoproc({(-2*NU+2*n)*u(n)+(n^2+3*n+2)*u(n+2), u(0)=1, u(1)=1},
 u(n), params=[NU])(1,2), 1);

##########

Try(10, rectoproc({u(0) = a[0], u(1) = a[1], u(2) = a[1],u(3) = 1/6*a[0]+2/3*a[1],
  (n^2-n)*u(n)+u(n+1)+(2*n+6)*u(n+3)+(-n^2-7*n-12)*u(n+4)},u(n),list,
  params=[a[0],a[1]])(5,2,3), [2, 3, 3, 7/3, 17/12, 43/60]);

##########

Try(11,rectoproc({-n*u(n)+(-n-1)*u(n+1)+1, u(0) = a0, u(1) = 1},
   u(n),list,params = [a0])(10,a), [a, 1, 0, 1/3, 0, 1/5, 0, 1/7, 0, 1/9, 0]);
##

#end test
