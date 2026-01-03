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

## Copyright (c) 2010, INRIA & KLMM. All rights reserved.
## Authors: Shaoshi Chen <schen@amss.ac.cn> and Ziming Li <zmli@mmrc.iss.ac.cn>.



Decomposition := module()
   description "this module is to compute squarefree partial fraction decomposition";
   export  SquareFree,
           SquareFreePartialFraction,
           VerifyPartialFraction;
   local 
         AdicExpansion,
         LaurentPart, 
         Position; 
   option package;

 ################################################################
 # Name: AdicExpansion                                          
 # Calling sequence:  
 #            AdicExpansion(F, x, t)              
 # Input: F, a nonzero fraction [a, d, n] representing a simple fraction
 #        a/d^n;                                       
 #        x, a name of variable.                                
 # Output: the list of the d-adic expansion of a/d^n,                           
 #         [[c1, a_1] ..., [cn, a_n]];                          
 # Optional: the third argument if presented is assigned the d-adic
 #           expansion in the usual form                       
 ################################################################

 AdicExpansion := proc(F, x, t)
    local a, c, d, n, i, r, q, T, l, g;
    (a, d, n) := F[1], F[2], F[3]; 
    T := table([]);
    if nargs = 3 then
       g := 0;
    end if;
    for i from n by -1 to 1 do 
       r := rem(a, d, x, 'a');
       r := primpart(r, x, 'c');
       if r <> 0 then
          T[i] := [c, r];
       else
          T[i] := [1, 0];
       end if;  
       if nargs = 3 then
          g := c*r/d^i + g;
       end if;
    end do;
    if nargs = 3 then
       t := g;
    end if;
    l := n;
    for i from n by -1 to 1 do
        if T[i][2] = 0 then
           l := l - 1;
        else
           break;
        end if;
    end do; 
    [seq(T[i], i=1..l)]; 
 end proc:

 ###########################################################################
 # Name: Position
 # Calling sequence:
 #       Position(L, n)
 # Input: L, a list of integers arranged increasingly;
 #        n, an integer
 # Output: l, a positive integer such that L[l-1] < n <= L[l];
 ###########################################################################

 Position := proc(L, n)
    local l, m, i; 
    l := 1; 
    m := nops(L);
    for i from 1 to m do
        if L[i] < n then
           l := l + 1;
        else
           break;
        end if; 
    end do;
    l;
 end proc;
      
 ###########################################################################
 # Name: SquareFree
 # Calling sequence:
 #         SquareFree(P, x)
 # Input: P, a nonzero polynomial
 #        x, a name
 # Output: a triple, c, L, M, where
 #         c is the content of P with respect to x;
 #         L is the list of squarefree factors
 #         M is the list of multiplicities corresponding to the factors in L
 #         (M is arranged increasingly)
 ###########################################################################

  SquareFree := proc(P, x)
     local T, L, M, Lp, Mp, Ls, Ms, c, n, i, f, m, k;  

     if degree(P, x) = 0 then
        return [P, []];
     end if;

     T := sqrfree(P, x);

     # rearrange the output

     c := T[1];
     n := nops(T[2]);
     (L, M) := [T[2][1][1]], [T[2][1][2]];
     for i from 2 to n do
         (f, m) := T[2][i][1], T[2][i][2]; 
         k := Position(M, m);
         Ls := [op(1..k-1, L)];
         Lp := [op(k..nops(L), L)];
         Ms := [op(1..k-1, M)];
         Mp := [op(k..nops(M), M)];
         while Mp <> [] and m = Mp[1] do
               f := f*Lp[1];
               Lp := [op(2..nops(Lp), Lp)];
               Mp := [op(2..nops(Mp), Mp)];
         end do;
         L := [op(Ls), f, op(Lp)];
         M := [op(Ms), m, op(Mp)];
     end do;
     [c, [seq([L[i], M[i]], i=1..nops(M))]];
  end proc;
    
###########################################################
# Name: LaurentPart
# Calling sequence:
#       LaurentPart(a, d, s, x, t)
# Input: a, a polynomial in x;
#        d, a list [u, v, n], representing a polynomial
#           u*v^n with gcd(u, v) = 1 and gcd(a, u*v)=1.
#        s, a polynomial such that s*u = 1 mod v
#        x, a name
# Output: c, L,  where c is the content, and L = [a1, .., an] such that
#         the Laurent serise of a/(u*v^n) at v truncated
#         at the term of order -1 is equal to
#
#         c*(a_1/v_1 + ... + a_n/v^n)
#
# Optional: the fifth argument is assigned a new numerator 
###########################################################

LaurentPart := proc(a, d, s, x, t)
   local nu, u, v, n, T, r, i, cp, w, ds, ns, l, rs, cs;
   nu := a;
   (u, v, n) := d[1], d[2], d[3];
   T := table([]); 
   cp := 1; ds := 1; 
   for i from n by -1 to 1 do
       cp := normal(cp*content(nu, x, 'nu')*(1/ds)); 
       r :=  rem(rem(nu, v, x)*s, v, x);
       if r = 0 then
          T[i] := [1, 0];
       else
          rs := primpart(r, x, 'cs');
          T[i] := [normal(cs*cp), rs];   #modiefied  on July 6
       end if; 
       (ds, ns) := denom(r), numer(r);
       nu := ds*nu - ns*u;
       nu := Division:-ExactDivision(nu, v, 1, x);
   end do;
   if nargs = 5 then
      t := normal(cp/ds)*nu;
   end if;
   l := n;
   for i from n by -1 to 1 do
       if T[i][2] = 0 then
           l := l - 1;
       else
           break;
       end if;
    end do; 
    [seq(T[i], i=1..l)]; 
end proc; 
 
##############################################################################
#Name: SquareFreePartialFraction
#Calling sequence
#         SquareFreePartialFraction(f, x, t)
#Input: f, a nonzero rational function of x;
#       x, a name
#Output: a pair c, G, where c is the content of f with respect to x,
#        and G is the list of partial fractions of f/c, p is the polynomial.
#Optional: the third argument, if presented, is assigned the usual expression
#          of squarefree partial fraction decompositions 
###############################################################################

SquareFreePartialFraction := proc(f, x, t)
   local g, a, b, d, c, p, i, S, L, M, n, Gt, w, r, 
         v, m, u, H, s, G, T, T1, T2, j, dp, cp, ap, bp, Sp, cs;

   # initialize

   g := normal(f);
   (a, d) := numer(g), denom(g);
   c := content(a, x, 'ap')/content(d, x, 'dp');

   # trivial case

   if degree(d, x) = 0 then
      if nargs = 3 then
         t := c*ap;
      end if;
      return(c, [x, ap, []]) # modified by Shaoshi 0703
   end if;

   # compute polynomial part

   p := quo(ap, dp, x, 'bp');
   ap := bp;

   # compute the squarefree factorization of the denominator
   
   Sp := SquareFree(dp, x);
   cs := Sp[1];
   dp := cs*dp;
   
   # compute the Laurent parts one by one

   S := Sp[2];
   n := nops(S);

   S := [seq(S[n-i], i=0..n-1)]; 

   Gt := table([]);

   
   for i from 1 to n-1 do

       # prepare for Laurent expansion

       v := S[i][1];
       m := S[i][2];
       u := Division:-ExactDivision(dp, v, m, x); 

       T := table([]);
       for j from i+1 to n do
           gcdex(S[j][1], v, x, 'w');
           T[j] := w$S[j][2]; 
       end do;
       H := [seq(T[j], j=i+1..n)];
       s := Division:-MergeRemainder(H, v, x);

       # compute Laurent expansion and update the numerator


       Gt[i] := LaurentPart(ap, [u, v, m], s, x, 'bp');
       ap := bp; 
     
       # update the denominator
              
       dp := u;

  end do;
  Gt[n] := AdicExpansion([ap, S[n][1], S[n][2]], x);

  #######################################
  # Prepare for return
  #######################################

  G := [];
  for i from 1 to n do
      if cs <> 1 then
         T1 := Gt[i];
         T2 := [];
         for j from 1 to nops(Gt[i]) do
             T2 := [ op(T2), [T1[j][1]*cs, T1[j][2]]];
         end do;
         Gt[i] := T2;
      end if;
      G := [op(G), [S[i][1], op(Gt[i])]];
  end do;
  G := [x, p, G];
  if nargs = 3 then
     t := c*List:-FromListToPartialFraction(G);
  end if; 
  c, G;
end proc; 


#######################################
# Verify(c, G, r)
# check the correctness of the output of
#  SquareFreePartialFraction
#######################################

VerifyPartialFraction := proc(c, G, r)
  local var, p, F, l, i, f, de, g, d, nu, h, j;
  (var, F) := G[1], G[3];
  l := nops(F);
  for i from 1 to l do 
      f := F[i];
      de := f[1];
      g := gcd(de, diff(de, var));
      if g <> 1 then 
         return false;
      end if; 
      d := degree(de, var);
      for j from 2 to nops(f) do
          nu := f[j][2];
          if degree(nu, var) >= d then
             return false;
          end if;
      end do;
  end do; 
  h := List:-FromListToPartialFraction(G);
  Testzero(c*h-r);
end proc; 
end module: 
                    
       
