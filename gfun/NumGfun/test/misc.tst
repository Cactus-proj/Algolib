# Copyright (C) 1991--2013 by INRIA.
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

with(gfun):
with(NumGfun):

####
#### my_abs/RootOf and abs_with_RootOf
####

t := RootOf(_Z^3+2,.6299605249-1.091123636*I);

Try("abs_with_RootOf 1",
    convert(NumGfun:-utilities:-abs_with_RootOf(t), 'radical'),
    2^(1/3));

# did we clean the remember tables of abs?
Try("abs_with_RootOf 1 bis",
    abs(t),
    'abs'(t));

# what do we do with non-RootOf expressions?
Try("abs_with_RootOf 2",
    NumGfun:-utilities:-abs_with_RootOf(1-sqrt(3)),
    sqrt(3)-1);

# larger example, harder to check though
Try[testnoerror]("abs_with_RootOf 3",
    NumGfun:-utilities:-abs_with_RootOf(
        RootOf(_Z^8-2*_Z+1,0.6110876563 + 0.8843359539*I)));

# RootOf subexpression
Try("abs_with_RootOf 4",
    eval(convert(NumGfun:-utilities:-abs_with_RootOf(t*sqrt(5)), 'radical')),
    2^(1/3)*5^(1/2));

# already positive (should be catched by signum)
t := RootOf(71-10*x^5+62*x^4-82*x^3+80*x^2-44*x, 4.816088382);
Try("abs_with_RootOf 5",
    NumGfun:-utilities:-abs_with_RootOf(t),
    t);

# ill-conditioned (infsolvepoly/isroot fails); gives a warning (but I don't
# how to test this)
Try("abs_with_RootOf 6",
    eval(convert(
        NumGfun:-utilities:-abs_with_RootOf(
                                          RootOf(-249377+11*_Z^2,-150.5677135)),
        'radical')),
    1/11*2743147^(1/2));

# used to return unevaluated due to nested RootOf
Try("abs_with_RootOf 7",
    NumGfun:-utilities:-abs_with_RootOf(
                                1/RootOf(_Z^2+_Z+1,-.5000000000+.8660254038*I)),
    1);

####
#### cmp_algeb_abs
####

kernelopts('opaquemodules'=false);
cmp_algeb_abs := NumGfun:-recasympt:-cmp_algeb_abs;

# Elementary cases

Try("cmp_algeb_abs 1",
    cmp_algeb_abs(1, 2),
    -1);

Try("cmp_algeb_abs 2",
    cmp_algeb_abs(2, 1),
    1);

Try("cmp_algeb_abs 3",
    cmp_algeb_abs(1, -2),
    -1);

Try("cmp_algeb_abs 4",
    cmp_algeb_abs(2, -1),
    1);

Try("cmp_algeb_abs 5",
    cmp_algeb_abs(2, -2),
    0);

# Mignotte-like polynomial with near-opposite roots

pol := X^7-2*(1000*X^2-1);

Try("cmp_algeb_abs 6",
    cmp_algeb_abs(RootOf(pol, 'index'=1), RootOf(pol, 'index'=5)),
    1);

Try("cmp_algeb_abs 7",
    cmp_algeb_abs(RootOf(pol, 'index'=5), RootOf(pol, 'index'=1)),
    -1);

Try("cmp_algeb_abs 8",
    cmp_algeb_abs(RootOf(pol, 'index'=4), RootOf(pol, 'index'=6)),
    0);

Try("cmp_algeb_abs 9",  # algebraic and rational very close to it
    cmp_algeb_abs(
        RootOf(pol, 'index'=5),
        316227766019337933200017821973/10000000000000000000000000000000),
    -1);

####
#### complex intervals and related stuff
####

t := I*2^(1/2)-Pi;
iv := (evalrC(RootOf(x^7-3, 'index'=2)) + exp(sqrt(2)))/10^20;
ivref := 123207916058513/2500000000000000000000000000000000;

Try("above_abs 1",
    signum(NumGfun:-utilities:-above_abs(t)-abs(t)),
    1);

Try("above_abs 2",
    signum(NumGfun:-utilities:-above_abs(iv)-ivref),
    1);

Try("below_abs 1",
    signum(NumGfun:-utilities:-below_abs(t)-abs(t)),
    -1);

Try("below_abs 2",
    signum(NumGfun:-utilities:-below_abs(iv)-ivref),
    -1);

Try[testerror]("below_abs 3",
    NumGfun:-utilities:-below_abs(0));

iv := -1/3*I+evalrC(10^15+1/Pi)-10^15;
Try("below_abs 2",
    signum(convert(NumGfun:-utilities:-below_abs(iv), 'rational', 'exact')-1/3),
    -1);

