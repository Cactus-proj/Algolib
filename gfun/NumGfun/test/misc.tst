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

with(gfun):
with(NumGfun):

#### 
#### make_waksman_proc 
####

Try[testnoerror]("waksman",
    make_waksman_proc(4));


#### 
#### my_abs/RootOf and abs_with_RootOf
####

t := RootOf(_Z^3+2,.6299605249-1.091123636*I);

Try("abs_with_RootOf 1",
    convert(NumGfun:-abs_with_RootOf(t), 'radical'),
    2^(1/3));

# did we clean the remember tables of abs?
Try("abs_with_RootOf 1 bis",
    abs(t),
    'abs'(t));

# what do we do with non-RootOf expressions?
Try("abs_with_RootOf 2",
    NumGfun:-abs_with_RootOf(1-sqrt(3)),
    sqrt(3)-1);

# larger example, harder to check though
Try[testnoerror]("abs_with_RootOf 3",
    NumGfun:-abs_with_RootOf(
        RootOf(_Z^8-2*_Z+1,0.6110876563 + 0.8843359539*I)));

# RootOf subexpression
Try("abs_with_RootOf 4",
    eval(convert(NumGfun:-abs_with_RootOf(t*sqrt(5)), 'radical')),
    2^(1/3)*5^(1/2));

# already positive (should be catched by signum)
t := RootOf(71-10*x^5+62*x^4-82*x^3+80*x^2-44*x, 4.816088382);
Try("abs_with_RootOf 5",
    NumGfun:-abs_with_RootOf(t),
    t);

# ill-conditioned (infsolvepoly/isroot fails); gives a warning (but I don't
# how to test this)
Try("abs_with_RootOf 6",
    eval(convert(
        NumGfun[abs_with_RootOf](RootOf(-249377+11*_Z^2,-150.5677135)),
        'radical')),
    1/11*2743147^(1/2));

# used to return unevaluated due to nested RootOf
Try("abs_with_RootOf 7",
    NumGfun:-abs_with_RootOf(1/RootOf(_Z^2+_Z+1,-.5000000000+.8660254038*I)),
    1);

