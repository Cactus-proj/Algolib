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

with(TestTools):

$include <testutils.mm>

with(gfun):
with(NumGfun):

eq := proc(f)
  diffeqtohomdiffeq(holexprtodiffeq(f, y(z)), y(z));
end proc:

TryEval := proc(id, deq, path, refval, prec)
    #Try[verify, float(1, test=2, digits=prec+1)](id,
    Try[testabsprec, prec](id,
      analytic_continuation(deq,y(z),path,prec),
      refval
    );
end proc:

## 1. Trivial cases

TryEval(1.1,
    {diff(y(z),z), y(0)=0},
    [0,1/3,1/2],
    0,
    30);

TryEval(1.2,
    {diff(y(z),z), y(0)=42},
    [0,1/3,1/2],
    42,
    30);

TryEval(1.3,
    {y(z)-(z+1)*diff(y(z),z), y(0)=1}, # 1+z
    [0, I, I+1/2],
    I+1.5,
    2);

## 2. Usual elementary and special functions inside their disc of convergence

for f in [ arccos, arccot, 
    #AiryAi,   ### FIXME: bound computations fail
    arcsin, arctan,
    #AiryBi,
    cos, erf, erfc, erfi,
    arccosh, arcsinh, arctanh, cosh, sinh, sin ] do
  TryEval(sprintf("2.%a.lowprec", f),
    eq(f(z)),
    [0, 1/2 + I/3],
    evalf(f(1/2+I/3)),
    1);
  TryEval(sprintf("2.%a.prec100", f),
    eq(f(z)),
    [0, 1/2 + I/3],
    evalf[110](f(1/2+I/3)),
    100);
  TryEval(sprintf("2.%a.ancont", f),
    eq(f(z)),
    [0, I/4, I/4+1/4, 1/2 + I/3],
    evalf[30](f(1/2+I/3)),
    20);
end do;

## 3. More usual functions

TryEval(3.1,
  eq(AiryAi(z)),
  [0, 4*I+4],
  -.343358827560791536078049613586131756921349108468956478933945829240434347\
446043233794674956112263985794844388965543990492e-2
-.478597920471672217114427752840834550965990381077377293417\
381901135312257108143497095764476959471582705972529293632075028e-2*I,
  100);

TryEval(3.2,
  eq(exp(z)),
  [0, -100],
  exp(-100),
  10);

TryEval(3.3,
  eq(arctan(z)),
  [evalf[100](10+1/Pi)],
  1.4741829577346568748838456813651130326671736181592436987981716909790\
40795145809714377878608650199436219563746624920693602358290616449034736\
065826937085,
  150);

## 4. Misc

# too slow for now

# orddeq=11, ordrec=9
# ajouer une limite de temps ?
#TryEval(4.1,
#    eq(arctan(z) + log(1/(1-z)) + exp(z)),
#    [0,1/30,(1+I)/30,I/30],
#    subs(z=I/30, arctan(z) + log(1/(1-z)) + exp(z)),
#    30);

# rec with constant coefficients
TryEval(4.2,
    {y(z)+(z-1)*diff(y(z),z), y(0)=1},
    [0,1/30,(1+I)/30,I/30],
    1/(1-I/30),
    20);

# diffeqtoproc w/o initial values
Try[verify, 'polynom(neighborhood(10^(-10)))'](6.2,
    diffeqtoproc(diff(y(z),z)-y(z),y(z))(1, 10),
    2.718281828459*_C[0]);

# diffeqtoproc w/o initial values
deq := -2*z*diff(y(z),z)+(-1-z^2)*diff(diff(y(z),z),z); # cancels arctan
Try[verify, 'polynom(neighborhood(10^(-10)))'](6.2,
    diffeqtoproc(deq, y(z))(1/2, 50),
    _C[0]+.463647609000806116214256231461214402028537054286120263810933088720*_C[1]);


## 5. Some less usual functions

TryEval(5.1,
  subs({a=1, b=1, c=1, q=1},
    {(1-z^2)*diff(y(z),z,z) - 2*(b+1)*z*diff(y(z),z) + (c-4*q*z^2)*y(z),
    y(0)=1, D(y)(0)=0}), # spheroidal wave equation
  [0, 1/3],
  # evalf[70](HeunC(0,-1/2,b,q,1/4-1/4*b-1/4*c,1/3^2));
  0.9437697112383230899752038579957087283424930461950612560939166467192803,
  50);

## 6. Misc. an. cont.

deq := holexprtodiffeq(arctan(z),y(z));
TryEval(6.1,
    deq,
    [0, 1/2*(1+I), 3/4*(1+I), 1+I, 1/2*(1+3*I),2*I],
    1.5707963267948966192+.54930614433405484570*I,
    10);



## 7. Misc regression tests

TryEval(7.1,
  eq(exp(z/(1-z^2))),
  [0,1/3],
  1.454991414618201336053793691987518508346842020964415681195241328184142,
  50);

deq := { (z+1)*(3*z^2-z+2)*diff(y(z),z$3) + (5*z^3+4*z^2+2*z+4)*diff(y(z),z$2)
    + (z+1)*diff(y(z),z) + (4*z^3+2*z^2+5)*y(z), y(0)=0, D(y)(0)=I, D(D(y))(0)=0 };
Try[testnoerror](7.2,
  diffeqtoproc(deq, y(z))([0,-2/5+3/5*I, -2/5+I, -1/5+7/5*I],10));

test_aceval(z -> AiryAi(z), [0,3/2], 20);

test_aceval(z -> arctan(z), [0,1/2,1,2,4], 20);


TryEval(7.3,
  {diff(diff(y(z),z),z)-z*y(z), y(0) = 1/3*3^(1/3)/GAMMA(2/3),
    D(y)(0) = -1/2*3^(1/6)/Pi*GAMMA(2/3)},  # AiryAi
  [0,3/2],
  0.0717494970081054096735554164897,
  30);

TryEval(7.4,
  {(1+z^2)*diff(y(z),z)-1, y(0) = 0},  # arctan
  [0,1/2,1,2,4],
  1.3258176636680324650592392104284756,
  30);

TryEval(7.5,  # subdivide_path used to loop
  {(1+z^2)*diff(y(z),z)-1, y(0) = 0},  # arctan
  [1+5*I],
  1.53088133393877796128+.19442614214700209103*I,
  10);

## 8. transition_matrix

deq := (1+z^2)*diff(y(z),z)-1;

Try[verify, 'Matrix(neighborhood(10^(-10)))'](8.1,
  transition_matrix(deq, y(z), [0,(1+I)/2,3*(1+I)/4,1+I,1/2+7*I/4,2*I], 10),
  Matrix(2,2, {
    (1, 1) = 1.,
    (1, 2) = 1.57079632679490+.549306144334055*I,
    (2, 2) = -.333333333333333}));

Try[testerror](8.2,
    transition_matrix(deq, y(z), 2));

Try[verify, 'Matrix(neighborhood(10^(-10)))'](8.3,
    transition_matrix(deq, y(z), [2]),  # trivial path!
    Matrix(2,2,{(1, 1) = 1.0000000000, (2, 2) = 1.0000000000}));

Try[verify, 'Matrix(neighborhood(10^(-10)))'](8.4,
    transition_matrix(deq, y(z), [2*I,1+I,0]),
    Matrix(2,2, {
        (1, 1) = 1.,
        (1, 2) = 4.71238898038468985 + 1.64791843300216453709*I,
        (2, 2) = -3.}));


