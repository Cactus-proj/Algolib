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

#######################################################
##### algeqtoseries algfuntoalgeq

#test 5

with(TestTools):
with(gfun):

########################################################################

P:=t+t^2-x:

Try(1,algeqtoseries(P,x,t,6) , [series(-1-1*x+1*x^2-2*x^3+5*x^4-14*x^5+O(x^6),x,6), series(1*x-1*x^2+2*x^3-5*x
^4+14*x^5-42*x^6+O(x^7),x,7)]):

########################################################################

P:=x+x*t+x*t^2/2+(x-1)*t^3/6+x*t^4/24:

Try( 2 ,algeqtoseries(P,x,t,6) ,[series(4*x^(-1)-4-3*x-9/2*x^2-69/8*x^3-75/4*x^4+O(x^5),x,5), RootOf(_Z^3-6)*x^
(1/3)+1/3*RootOf(_Z^3-6)^2*x^(2/3)+x+16/27*RootOf(_Z^3-6)*x^(4/3)+125/324*
RootOf(_Z^3-6)^2*x^(5/3)+3/2*x^2+O(x^(7/3))] ):

########################################################################

P:=x + (x-1)*t + (x-2)*t^2 + (x-3)*t^3:

Try(3  ,algeqtoseries(P,x,t,6) ,[series(RootOf(3*_Z^2+2*_Z+1)+(-1/3+1/3*RootOf(3*_Z^2+2*_Z+1))*x+(11/18*RootOf(
3*_Z^2+2*_Z+1)+13/18)*x^2+(-77/54-151/54*RootOf(3*_Z^2+2*_Z+1))*x^3+(745/648+
5147/648*RootOf(3*_Z^2+2*_Z+1))*x^4+(14677/1944-23041/1944*RootOf(3*_Z^2+2*_Z+1
))*x^5+O(x^6),x,6), series(1*x-1*x^2+1*x^3+3*x^4-23*x^5+83*x^6+O(x^7),x,7)] ):

########################################################################

# Test from the help page
P:=y-(x^2+x^3*y^2+x^6*y^5):

Try[member](4.1, algeqtoseries(P,x,y,4), [
   [series(1*x^2+O(x^6),x,6), -1/x^(3/2)-1/4+O(x^(1/2)), 1/x^(3/2)-1/4+O(x^(1/2)), RootOf(_Z^2+1)/x^(3/2)+1/4+O(x^(1/2))], 
   [series(1*x^2+O(x^6),x,6), RootOf(-1+_Z^4)/x^(3/2)-1/4*RootOf(-1+_Z^4)^2+O(x^(1/2))]
]):

Try(4.2,algeqtoseries(P,x,y,10,true) ,[series(x^2+x^7+O(x^12),x,12)] ):

########################################################################
# bug in versions < 2.54

Try( 6 ,{op(algeqtoseries((z*y^2-y+1)^2,z,y,10))} ,{
series(1+1*z+2*z^2+5*z^3+14*z^4+42*z^5+132*z^6+429*z^7+1430*z^8+4862*z^9+
O(z^10),z,10), series(1+1*z+2*z^2+5*z^3+14*z^4+42*z^5+132*z^6+429*z^7+1430*z^8+
4862*z^9+O(z^10),z,10), series(1*z^(-1)-1-1*z-2*z^2-5*z^3-14*z^4-42*z^5-132*
z^6-429*z^7-1430*z^8+O(z^9),z,9), series(1*z^(-1)-1-1*z-2*z^2-5*z^3-14*z^4-42*
z^5-132*z^6-429*z^7-1430*z^8+O(z^9),z,9)} ):

########################################################################
# Test from the help page
Try(  7,algfuntoalgeq(a*RootOf(_Z^5+1)*x^(2/3),y(x)) ,y^15+a^15*x^10 ):

########################################################################
# Test from the help page

eitherorminus := proc(a, b)
 evalb ((a=b) or (-a=b))
end:

Try[eitherorminus]( 8, algfuntoalgeq(a*RootOf(_Z^5+1)*x^(2/3),y(x),'algebraic') ,y^3-a^3*RootOf(_Z^5+1)^3*x^2 ):

########################################################################

# Test from the help page

Try( 9 ,algfuntoalgeq(5^(1/3)+3*7^(2/3),y(x)) ,y^9-3984*y^6+5112147*y^3-2342039552 ):

########################################################################

# Test from the help page
Try( 10.1 ,algfuntoalgeq(1+RootOf(_Z^3+1)*x,y(x),'res3') ,-x^3-y^3+3*y^2-3*y+1 );
Try( 10.2 ,res3 ,{(D(y))(0) = RootOf(_Z^3+1), y(0) = 1, (D@@2)(y)(0)=0} );
########################################################################
# problem in version < 2.57 which caused a bug in holexprtodiffeq

Try( 11 ,algfuntoalgeq(1-sqrt(1-1/t^2),y(t)) , 1+t^2*y^2-2*t^2*y):

########################################################################
# There were radicals in the result. Fixed BS Aug 04.
f:=
-1/4*(4*u^5+8*u^4-4*u^2+u)/(u^2+8*u^5+4*u^6-4*u^3)-1/4/u*(-(2*u^2-2*u+1)/(2*u^
2+2*u-1))^(1/2)+1/4/u*(-(4*u^2+12*(-(2*u^2-2*u+1)/(2*u^2+2*u-1))^(1/2)*u-4*u+2
+2*(-(2*u^2-2*u+1)/(2*u^2+2*u-1))^(1/2))/(-(2*u^2-2*u+1)/(2*u^2+2*u-1))^(1/2)/
(2*u^2+2*u-1))^(1/2):
Try(12,algfuntoalgeq(f,y(u)),
1+u^2*(4*u^4+8*u^3-4*u+1)*y^4+u*(4*u^4+8*u^3-4*u+1)*y^3+(2*u^2+2*u-1)*(u+2)*u*y^2+(2*u^2+2*u-1)*y):

#end test
