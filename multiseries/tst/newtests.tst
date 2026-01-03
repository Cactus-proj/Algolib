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

# Those work:
newlimit(sin(1/x),x=0);        # Error message
newlimit(Ci(x),x=infinity);    # Error message
newlimit(arcsin(-x), x=-1);    # Error message
indef:=int((x^2-1)/(x^5-1),x):
newlimit(indef,x=infinity);     # Error message
newlimit(indef,x=-infinity);    # Error message
## Problem with piecewise. Need to check extra evaluation.
newlimit(piecewise( x<1, x,  sin(x)/x),x=0); # Error message

# All errors are the same - only the names of the functions differ:
# Error, (in MultiSeries:-multiseries) MultiSeries:-multiseries/
# function[arctan], improper op or subscript selector

## This one can be see more easily by 
## newlimit(cos(x*t),x=infinity); # the result depends on signum(t)
# Different error message:
f:=exp(I*x*t)/(1+x^2)^2:
res3:= int(evalc(f),x):
newlimit(res3,x=infinity);

# Another error message:
newlimit((2*k-2)*2^(2*k-3)/(k-1)/binomial(2*k-2,k-1), k=1);
newlimit(k/binomial(n,k-1),k=0);

# And another:
newlimit(abs(x)/x,x=0);

# Yet another one:
newlimit(exp(x),x=-infinity,'right'); # this one is easy - one should not
# quote types in the arguments check by ::

newlimit(ln(-1-I*t),t=0);               # Wrong result
newlimit(-(x^(y+1)-1)/(y+1),x = 0);     # Wrong result
newlimit(x^n,x=0);                      # Wrong result
newlimit(t^(n+1)/(t-1)-1/(t-1),t=infinity); # Wrong result
newlimit(t^(n+1)/(t-1)-1/(t-1),t=0); # Wrong result

newlimit(newlimit(f(x),x=infinity),x=1);  # Infinite loop
newlimit(-4/3*(1/4)^(n+1)+1/3,n=infinity);# Infinite loop

newlimit(f[x],x=infinity);     # undefined (why?) limit returns unevaluated
newlimit(f[x],x=a);            # undefined - comment in the test that it is a
# design decision that limit returns f[a]
