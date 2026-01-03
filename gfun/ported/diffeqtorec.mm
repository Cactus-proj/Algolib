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


#diffeqtorec
# Input:  eqn: differential equation (for example output of algeqtodiffeq)
#    y(z): its unknown function
#    u(n): the name of the sequence of Taylor coefficients at the origin
#    ini: (optional) boolean indicating whether initial conditions should
#     be computed
#    contentrec: (optional) will be assigned the content that was removed from
#      the recurrence. This is useful when the diff. eqn. is only the homogenous part
#      of the actual one.
# Output: the linear recurrence satisfied by u(n)
#
   diffeqtorec:=proc (eqn,yofz,uofk, {ini::boolean:=false,returncontent::boolean:=false,contentrec::name:=dummy})
   option `Copyright (c) 1992-2007 by Algorithms Project, INRIA France. All rights reserved.`;
   local iniconds, f, y, z, u, k, Y, Z, rec, inirec, cont;
      getname(uofk,u,k); # This also checks that >=3 args are passed
      if ini or type(eqn,'set') then f:=formatdiffeq([eqn,yofz],'y','z','iniconds')
      else
         f:=formatdiffeq([eqn,yofz],'y','z');
#         if f[1]<>0 then error "inhomogenous equation" end if
      end if;
      # Avoid problems when u=y or u=z,...
      f:=subs([y=Y,z=Z],f);
      if ini or type(eqn,'set') then iniconds:=subs(y=Y,iniconds)
      else iniconds:=NULL end if;
      rec,inirec,cont:=`diffeqtorec/doit`(f,Y,Z,u,k,iniconds);
      if returncontent then contentrec:=cont fi;
      if iniconds<>{} or iniconds<>NULL then
	inirec:=`goodinitvalues/rec`(rec,u,k,inirec,true) end if;
      makerec(rec,u,k,inirec)
   end proc: # diffeqtorec
