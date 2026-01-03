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

# `goodinitvalues/rec`
# Input:  a recurrence in the format returned by formatrec
#    the unknown sequence and its variable
#    some initial conditions
#    a boolean flag cleanup
#    (optional) an integer p
#    (optional) maxsing = nonnegint
# Output: a set of equalities u(k)=v_k, from which all the other values
#    (smaller than maxsing) can be deduced by solving the recurrence for its maximal index.
#    This set is continued up to the pth term if p is given.
#    New variables _C[i] are introduced when a value is arbitrary.
#    When the flag is true, the unnecessary entries u(k)=_C[i] are removed
#    The result is an ERROR when no initial condition can be found.
   `goodinitvalues/rec`:=proc (rec, u, n, ini, flag, arg6)
   option `Copyright (c) 1992-2007 by Algorithms Project, INRIA France. All rights reserved.`;
   local n0, order, i, inds, minind, maxind, sys, r, sol, b, a, j, k, rej, maxsingularity, gb, termorder, dorej, inds2;
      order:=nops(rec)-2;
      maxind:=order-1;
      if type(ini,'set') then inds:=map(op,indets(ini,u('integer')))
      else inds:={}; maxind:=max(maxind,ini) end if;
      maxsingularity:=NULL;
      if nargs=6 then
      	if type(arg6,integer) then maxind:=max(maxind,arg6)
      	elif type(arg6,identical('maxsing')=nonnegint) then
      		maxsingularity:=op(2,arg6)
      	else error "invalid argument",arg6
      	fi
      end if;
      # The initial conditions are u(minind)..u(maxind)
      n0:=firstnonzero(subs(n=n-order,rec[nops(rec)]),n,maxsingularity);
      # u(n0-1) cannot be deduced from the previous ones
      maxind:=max(maxind,op(inds),n0-1);
      minind:=min(op(inds),0);
      r:=makerec(rec,u,n);
      for j to 2 do
	 if j=1 then
	    sys:={op(ini),seq(subs(n=i,r),i=minind..maxind-order)}
	 else
	    sys:={op(ini),seq(subs(n=i-order,r),
		i={$minind+order..maxind} minus inds)}
	 fi;
         if sys={} then
            if not flag then return {seq(u(i)=_C[i],i=minind..maxind)}
            else return {seq(u(i)=_C[i],
                             i={$minind..maxind} minus {$0..order-1})}
            end if
         end if;

         a:=systomatrix(sys,[seq(u(i),i=minind..maxind)],'b');
         try
            sol:=LinearAlgebra:-LinearSolve(a,Vector['column'](b));
         catch "inconsistent system" :
            sol := NULL;
         end try;
         if sol=NULL  and j=2 then
            error "no valid initial conditions"
         elif sol<>NULL then break end if
      end do;
      sol:=convert(sol,list);
      inds:=indets(sol,_t[anything]) union indets(sol,'typeindex'( 'anything', 'suffixed'('_t', 'integer') ));
      #
      # replace the _t[anything] by _C[anything] depending on flag
      dorej:=flag and (j=1);
      j:=max(op(map(op,indets([rec,ini],_C['anything']))));
      if j=-infinity then j:=-1 end if;
      for i in inds do
         if member(i,sol,'k') and (not dorej or nops(select(has,sol,i))>1
                                   or k<minind-1 or k+minind>order) then
            j:=j+1;
            sol:=subs(i=_C[minind+j],sol);
            rej[i]:=NULL
         else rej[i]:=k
         end if
      end do;
      sys:={seq(u(i+minind-1)=sol[i],i={$1..nops(sol)} minus
                {seq(rej[i],i=inds)})};
      # clean the _C[] of initial conditions of the type _C[1]+_C[2],_C[1]-_C[2]
      if hastype(remove(type,sys,u('anything')='name'),_C['anything'])
      # limitations due to the cost of Groebner basis computation:
      and max(op(map(degree,[seq(op(2,i),i=sys)],
                     indets(sys,_C['anything']))))<3 and not has(sys,'RootOf')
      # also it has to be a system of polynomials
      and type([seq(op(2,i),i=sys)],
               'list'('polynom'('rational',indets(sys,_C['anything']))))
      then
         inds:=indets(sys,_C['anything']);
         inds2:=sort([op(map(op,indets(sys,u('anything'))))]);
         sys:=subs([seq(u(i)=u[i],i=inds2)],{seq(op(1,i)-op(2,i),i=sys)}):
	 # find algebraic relations between the u[i]
	 termorder:=lexdeg([op(inds)],[seq(u[i],i=inds2)]);
     gb:=remove(hastype,Groebner:-Basis(sys,termorder),_C['anything']);
	 # workaround for a weakness in Groebner:-HilbertDimension:
	 #  Groebner:-HilbertDimension([],[a[i]$i=1..n]) is exponential in n
	 if gb=[] then sol:=[seq(u[i],i=inds2)] else
	 	sol:=subs(solve({op(gb)},{seq(u[i],
			i=inds2[Groebner:-HilbertDimension(
			gb,termorder)-nops(inds)..-1])}),[seq(u[i],i=inds2)])
	 fi;
         j:=-1;
         for i to nops(sol) do
            if sol[i]=u[inds2[i]] then
               if dorej and nops(select(has,sol,sol[i]))=1 then
                  rej[i]:=i
               else
                  rej[i]:=NULL;
                  j:=j+1;
                  sol:=subs(u[inds2[i]]=_C[j],sol);
               end if
            end if
         end do;
         {seq(u(inds2[i])=sol[i],i={$1..nops(sol)} minus
              {seq(rej[i],i=1..nops(sol))})}
      else sys
      end if
   end proc: # `goodinitvalues/rec`
