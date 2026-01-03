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

# diffeqtohomdiffeq
# Input:  deq: differential equation
#    y(z): its unknown function
# Output: a homogeneous linear differential equation having the solutions
#         of deq as solutions
#
   diffeqtohomdiffeq:=proc (Deq, yofx)
   option `Copyright (c) 1992-2007 by Algorithms Project, INRIA France. All rights reserved.`;
   local deq, y, x, ini, c, dc, n, i, iszero;
      deq:=formatdiffeq([args],y,x,ini);
      if deq[1]=0 then Deq
      else
         n:=nops(deq);
         if ini<>{} or n=2 then
	    ini,iszero:=`goodinitvalues/diffeq`(deq,y,x,ini,n-2);
	    if iszero then return [0,1] fi end if;
         c:=deq[1];
         dc:=-diff(c,x);
         makediffeq(map(collect,[0,dc*deq[2]+c*diff(deq[2],x),
             seq(dc*deq[i]+c*diff(deq[i],x)+c*deq[i-1],i=3..n),c*deq[n]],x),
                    y,x,ini)
      end if
   end proc: # diffeqtohomdiffeq
