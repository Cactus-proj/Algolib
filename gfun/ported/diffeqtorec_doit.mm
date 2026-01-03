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

  `diffeqtorec/doit` := proc(R,y,z,u,k,iniconds)
   option `Copyright (c) 1992-2007 by Algorithms Project, INRIA France. All rights reserved.`;
   local l, ini, i, rec, j, minordrec, maxordrec, m, r, dr1, inhdeg, inhpart, p, rr, cont;
      if has(R,k) then error k,`cannot appear in the differential equation`
      elif has(R,u) then
      	  error u,`cannot appear in the differential equation` end if;
      # initial conditions
      if nargs=6 then # iniconds is there
         l:={seq(op(2,op(0,op(0,i))),i=indets([iniconds,R],`gfun/initeq`(y))
                 minus {y(0),'D(y)(0)'})};
         ini:=[y(0)=u(0),'D(y)(0)'=u(1),seq(`@@D`(i,y,0)=u(i)*i!,i=l)];
         r:=subs(ini,R); ini:=subs(ini,iniconds)
      else r:=R; ini:={} end if;
      # In very special cases, this loop makes it possible to return
      # an inhomogeneous equation of lower order.
      # Ex: z*(-1+z)^3*(D@@2)(y)(z)+(-1+z)^3*D(y)(z)-(-1+z)^3*y(z)-z*(z-3)
      if r[1]<>0 then for inhdeg from 0 do
	 p:=1-z;
	 for i from 2 to nops(r) while degree(p,z)=1 do p:=gcd(p,r[i]) end do;
	 if not has(p,z) then break end if;
	 r:=[r[1],seq(quo(r[i],1-z,z),i=2..nops(r))]
      end do end if;
      # main loop
      ## this fixes a problem with the change in the definition of degree(0)
      ## in V.5
      rr:=map(proc(x) if x=0 then 1 else x end if end,r);
      minordrec:=min(seq(i-degree(op(i+2,rr),z),i=0..nops(r)-2));
      maxordrec:=max(seq(i-ldegree(op(i+2,rr),z),i=0..nops(r)-2));
      rec:=Array(minordrec..maxordrec,'storage'='sparse');
      for i from 2 to nops(r) do
         for j from ldegree(op(i,r),z) to degree(op(i,r),z) do
            rec[i-2-j]:=rec[i-2-j]+coeff(op(i,r),z,j)*
            expand(mul(k+m,m=1-j..i-2-j)) end do end do;
      # inhomogeneous part of the differential equation
      if r[1]=0 then dr1:=-1; inhpart:=0
      else
         dr1:=degree(r[1],z);
         if inhdeg<>0 then # r[1] stands for r[1]/(1-z)^inhdeg
            inhpart:=expand(add(coeff(r[1],z,i)*mul(k+j,j=1-i..inhdeg-i-1),
		i=0..dr1))/(inhdeg-1)!;
            r:=subsop(1=series(r[1]/(1-z)^inhdeg,z,
                               max(dr1,maxordrec-minordrec)+1),r)
         else
            inhpart:=0
         end if
      end if;
      if ini<>{} or nargs=6 or dr1<>-1 then
         ini:={op(ini),op(map(convert,[seq(subs(k=i,[coeff(r[1],z,i),seq(
            rec[j]*u(i+j),j=max(minordrec,-i)..maxordrec)]),i=0..dr1)],`+`))};
         for i from dr1+1 while i<-minordrec or
         	subs(k=i,{seq(rec[maxordrec-j],
         		j=0..min(i-dr1-1,maxordrec-minordrec))})={0} do
            ini:={op(ini),convert(subs(k=i,[seq(rec[j]*u(i+j),
		j=max(minordrec,-i)..maxordrec),inhpart]),`+`)} end do;
      end if;
      rec:=listprimpart(
         subs(k=k-minordrec,[inhpart,seq(rec[i],i=minordrec..maxordrec)]),k,
	 'cont');
      if has(cont,k) and (ini<>{} or nargs=6) then
	 l:=-1;
	 for j in select(type,myisolve(cont,k),nonnegint) do
	 	if j>=i then # this i is the last one in the previous loop
		    l:=l+1;
		    ini:=ini union {u(j)=_C[l]}
		fi
	 od
      fi;
      while nops(rec)>2 and rec[nops(rec)]=0 do
         rec:=subsop(nops(rec)=NULL,rec) end do;
      map(collect,rec,k),ini,cont
   end proc: # `diffeqtorec/doit`
