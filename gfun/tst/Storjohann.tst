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

MatrixAlea := proc(a,b,d,x) LinearAlgebra[RandomMatrix](a,b,generator=proc()randpoly(x,dense,degree=d) end) end:

test:=proc(n,d)
local M, v, st1,st2, S1,S2, i;
    M := MatrixAlea(n,n,d,x):
    v := MatrixAlea(n,1,d-1,x):
    v := Vector([seq(v[i],i=1..n)]);
    st1:=time();
    S1 := gfun:-Storjohann(M,v,x);
    st1:=time()-st1;
    st2:=time();
    S2 := LinearAlgebra[LinearSolve](M,v);
    st2:=time()-st2;
    for i to n do if normal(S1[i]-S2[i])<>0 then print(M,v,S1,S2,S1[i],S2[i],normal(S1[i]-S2[i])); error "bug" fi od;
    st1,st2
end:

#kernelopts(profile=true);
#writeto("/tmp/toto13");
#res:=test(14,20);
#if res[1]>4*res[2] then error "too slow",res[1],res[2] fi;
quit

