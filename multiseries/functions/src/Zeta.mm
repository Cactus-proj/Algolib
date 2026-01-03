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

###    -*-Maple-*-
###
###    Title:   Zeta and Hurwitz Zeta functions
###    Created: Oct 2003
###    Author:  Bruno Salvy & Jürgen Gerhard
###    Contact: jgerhard@maplesoft.com
###
#------------------------------------------------------------------------------
### Name                  : `multiseries/function`[Zeta]
### Input                 :
###                          expr   a SERIES data-structure or a list of 
###                                 SERIES (for derivative / Hurwitz Zeta)
###                          scale  a SCALE data-structure
###                          var    a list of ScaleVar data-structure
###                                 this list contains elements in the 
###                                 asymptotic basis w.r.t. which the expr 
###                                 is expanded.
###                          ord    a nonnegative integer
### Output                :
###                          multiseries expansion of Zeta(expr)
### References            :
###                          Abramowitz and Stegun
### Error Conditions      :
###                          if expr is not a valid argument for Zeta or 
###                          Hurwitz Zeta function (too many arguments)
###

$define T_SERIES {'SERIES','identical'(0)}

FUNCTIONTABLE['Zeta'] := proc(expr, scale, var, ord)::SERIES:
option ALGOCOPYRIGHT;

local L, L2, n, i::integer, h::T_SERIES:

   ASSERT(expr::{T_SERIES,list(T_SERIES)} and scale::'SCALE' and
          var::list('ScaleVar') and ord::nonnegint);
   
   if expr=0 then
      CONVERT2SERIES(-1/2,args[2..-1])
   elif type(expr,'SERIES') then # classial (one-arg) Zeta function
      L := LIMIT(expr,scale,var):
      
      if L = undefined then
         error "unable to compute multiseries"
         
      elif has(L,infinity) then
         if signum(L) = -1 then
            # reflection formula (A&S 23.2.6)
            # 2^s*Pi^(s-1)*sin(Pi*s/2)*GAMMA(1-s)*Zeta(1-s)
            L := MULDOIT(RUN(2^expr*Pi^(expr-1)*sin(Pi*expr/2)*GAMMA(1-expr),
                             args[2..-1]),
                         procname(ADDDOIT(1,MULSERIESCST(expr,-1,'integer')),
                                  args[2..-1]))
            
         elif signum(L) = 1 then
            # Dirichlet series (A&S 23.2.1)
            # 1 + 1/2^s + 1/3^s + ...
            L := RUN(add((i+1)^(-expr), i=0..ord), args[2..-1])
            
         else
            error "unable to compute multiseries"
         end if:

         subsop(EXPR4SERIES='Zeta'(op(EXPR4SERIES,expr)), L)
         
      elif L = 1 then # 1 is a simple pole of Zeta
         # 1/(s-1) + gamma + sum((-1)^i/i!*gamma(i)*(s-1)^i, i=1..ord-1)
         # (A&S 23.2.5)
         
         h := ADDDOIT(expr,-1): # s-1

         L := POWER(h,-1,args[2..-1]):
         if ord>0 then
            L := ADDDOIT(L,
                         COMPOSE(
                            SERIES(
                               scale,
                               [gamma, seq((-1)^i/i!*gamma(i),i=1..ord-1)],
                               1,
                               'algebraic',
                               [seq(i,i=0..ord-1)],
                               ord,
                               'integer',
                               scale['variable'],
                               'Zeta'(1+scale['variable'])),
                            h,
                            args[2 .. -1]))
	 else # ord=0
	    L:=ADDDOIT(L,SERIES(scale,[],gamma,'algebraic',[],0,'integer',
		SCALEVARIABLE,gamma))
         end if;
         
         subsop(EXPR4SERIES='Zeta'(op(EXPR4SERIES,expr)), L)
         
      else # Zeta is analytic; Taylor expansion
         ANALYTIC(Zeta,args[1..-1])
      end if
      
   elif type(expr,[T_SERIES,T_SERIES]) then # derivative of Zeta function
      if expr[1]=0 then n:=0 else n:=`multiseries/Series2Expr`(expr[1]) fi;
      if not type(n,'nonnegint') then
         error "cannot expand Zeta with first argument %1", n
      end if:

      L := LIMIT(expr[2],scale,var);
      if L = undefined then
         error "unable to compute multiseries"

      elif has(L,infinity) then
         if signum(L) = 1 then
            # derivative of Dirichlet series (A&S 23.2.1)
            # 1 + (-ln(2))^n/2^s + (-ln(3)^n)/3^s + ...
            L := RUN(add((-ln(i+1))^n*(i+1)^(-expr[2]),i=1..ord+1),
                     args[2 .. -1])

         else
            # to be implemented: signum(L) = -1
            error "unable to compute multiseries"
         end if;

         subsop(EXPR4SERIES = 'Zeta'(n,op(EXPR4SERIES,expr[2])),L)

      elif L = 1 then # 1 is a pole of order n+1
         # (-1)^n*n!/(s-1)^(n+1)
         # + sum((-1)^(n+i)/i!*gamma(n+i)*(s-1)^i, i=0..ord-1)
         # (A&S 23.2.5)

         h := ADDDOIT(expr[2],-1);

         L := MULSERIESCST(POWER(h,-n-1,args[2..-1]),(-1)^n*n!,'integer');
         if ord>0 then
            L := ADDDOIT(L,
                         COMPOSE(
                            SERIES(
                               scale,
                               [seq((-1)^(n+i)/i!*gamma(n+i),i=0..ord-1)],
                               1,
                               'algebraic',
                               [seq(i,i=0..ord-1)],
                               ord,
                               'integer',
                               scale['variable'],
                               'Zeta'(n,1+scale['variable'])),
                            h,
                            args[2..-1]))
         end if;

         subsop(EXPR4SERIES = 'Zeta'(n,op(EXPR4SERIES,expr[2])),L)

      else # Zeta is analytic; Taylor expansion
         ANALYTIC(Zeta,args[1 .. -1])
      end if:
     
   elif type(expr,[T_SERIES,T_SERIES,T_SERIES]) then
      # Hurwitz Zeta function (or a derivative of it)
      
      if expr[1]=0 then n:=0 else n:=`multiseries/Series2Expr`(expr[1]) fi;
      if not type(n,'nonnegint') then
         error "cannot expand Zeta with first argument %1", n
      end if:
      
      L := LIMIT(expr[3],scale,var):
      L2 := LIMIT(expr[2],scale,var):
      if has([L,L2],{undefined,infinity}) then
         # asymptotic expansions not implemented
         error "unable to compute multiseries"
         
      elif type(L,'nonposint')
      or (type(L,'float') and frac(L)=0 and L<=0) then
         # singularity at nonpositive integer L
         # shift third arg by 1-L: Zeta(n,s,t) =
         # (-ln(t))^n*(t^(-s) + (t+1)^(-s) + ... + (t-L)^s) + Zeta(n,s,t+1-L)
         L := ADDDOIT(RUN((-ln(expr[3]))^n *
                          add((expr[3]+i)^(-expr[2]),i=0..floor(-L)),
                          args[2..-1]),
                      procname([expr[1],expr[2],
                                ADDDOIT(expr[3],1+floor(-L))],
                               args[2..-1]))
         
      elif L2=1 then
         # 1 is a pole, but we know only two terms of the expansion
         if n=0 then
            L := RUN(1/(expr[2]-1) - Psi(expr[3]) + O(expr[2]-1), args[2..-1]):
         else
            L := RUN((-1)^n*n!/(expr[2]-1)^(n+1) + O(1), args[2..-1]):
         end if

      else # Hurwitz Zeta is analytic; Taylor expansion
         return ANALYTIC(Zeta,args[1..-1])
         
      end if:
         
      subsop(EXPR4SERIES =
             'Zeta'(n,op(EXPR4SERIES,expr[2]),op(EXPR4SERIES,expr[3])), L)
         
   else
      error "Zeta expects 1,2, or 3 arguments, got %1", nops(expr)
      
   end if 
   
end proc: # FUNCTIONTABLE['Zeta']
