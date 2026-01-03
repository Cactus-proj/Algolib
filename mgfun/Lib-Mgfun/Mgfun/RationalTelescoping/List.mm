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

List := module()
   description "handling lists of partial fractions";
   export 
          AddSimpleFraction,
          AddTwoPartialFractions,
          DiffPartialFraction,
          FromListToNormal,
          FromListToPartialFraction,
          ScalarMultiply;
   local 
          AddBranch,
          AddBranch1,
          DiffBranch,
          DiffSimpleFraction,
          NormalizeBranch,
          SumOfSimpleFractions;

   option package;
 



#############################################################
# [y, p, fractionlist]
# fractionlist = [[d_1, [c_11, n_11], ..., [c_1n1, n_1k1]],
#                            .....
#                  [d_m, [c_m1, n_m1], ..., [c_m1, n_mkm]]]
##############################################################

##############################################################
# Name: FromListToPartialFraction
# Calling Sequence
#      
#       FromListToPartialFraction(L)
#
# Input: L, a list for a partial fraction decomposition
# Output: the partial fraction representation of the rational
#         function represented by L
##############################################################

FromListToPartialFraction := proc(L)
    local p, F, R, i, r, M, d, j;
    p := L[2];
    F := L[3];
    if F = [] then
       return p;
    end if;
    R := p;
    for i from 1 to nops(F) do
        r := 0;
        M := F[i];
        d := M[1];
        for j from 2 to nops(M) do
            r := r + M[j][1]*M[j][2]/d^(j-1);
        end do;
        R := R + r;
     end do;
     R;
end proc;

#############################################################
# Name: FromListToNormal
# Calling sequence: 
#                  FromListToNormal(L)
# Input: L, a list for a partial fraction decomposition
# Output: the usual normalized rational form of the function 
#         represented by L
##############################################################
FromListToNormal := proc(L)
    local p, F, R, i, r, M, d, j;
    p := L[2];
    F := L[3];
    if F = [] then
       return p;
    end if;
    R := p;
    for i from 1 to nops(F) do
        r := 0;
        M := F[i];
        d := M[1];
        for j from 2 to nops(M) do
            r := normal(r + normal(M[j][1]*M[j][2]/d^(j-1)));
        end do;
        R := R + r;
     end do;
     R;
end proc;



##############################################################
# Name: ScalarMultiply
# Calling Sequence
#      
#       ScalarMultiply(c, L)
#
# Input:  c, a rational function free of the main variable in L
#         L, a list for a partial fraction decomposition
# Output: c*L, in list representation
##############################################################

ScalarMultiply := proc(c, L)
    local var, p, F, cp, de, nu, G, i, M, T, j, cs, ns, k;
    (var, p, F) := L[1], L[2], L[3]; 
    cp := normal(c);
    (de, nu) := denom(c), numer(c);
    if degree(de, var) > 0 or degree(nu, var) > 0 then
       error "the first input is not a scalar with respect to the second";
    end if;
    if nu = 0 then
       return [var, 0, []];
    end if;
    G := []; 
    for i from 1 to nops(F) do
        M := F[i];
        T := table([]); 
        T[1] := M[1];
        for j from 2 to nops(M) do
            (cs, ns) := M[j][1], M[j][2];
            if ns  = 0 then
               T[j] := [1, 0];
            else
               T[j] := [normal(cp*cs), ns];
            end if;
        end do;
        G := [op(G), [seq(T[k], k=1..nops(M))]];
     end do;
     [var, cp*p, G];
end proc;



###############################################################
# Name: AddSimpleFraction
# Calling Sequence
#      
#       AddSimpleFraction(f, L)
#
# Input:  f, a simple fraction given by [c, de, nu, m]
#            representing c*nu/de^m;
#         where nu is nonzero, degree(nu, v) < degree(de, v)
#         (v the main variable of L), de is a dinominator in L
#         L, a list for a partial fraction decomposition
# Output: f + L, in list representation
###############################################################

AddSimpleFraction := proc(f, L)
    local c, de, nu, n, var, p, F, m, i, M, ds, r, dr, nr, l, G, np,
          N, newf, cp, Fp, k, R; 
    (c, de, nu, n) := f[1], f[2], f[3], f[4];
    (var, p, F) := L[1], L[2], L[3];
    if nu = 0 then
       return L;
    end if;
    Fp := 0;
    m := nops(F);  
    for i from 1 to m do
        M := F[i];
        ds := M[1];
        r := normal(ds/de);
        (dr, nr) := denom(r), numer(r);
        #print(degrees); print(degree(dr, var)); print(degree(nr, var));
        if degree(dr, var) = 0 and degree(nr, var) = 0 then
           #print(find_the_denom);
           c := normal(c/r);
           l := nops(M);
 
           # update a branch

           if l - 1 < n then
             #  G := [op(M), [1, 0]$(n-l-1), [c, nu]]; 
             G := [op(M), [1, 0]$(n-l), [c, nu]]; 
           else
              N := M[n+1];
              np := normal(N[1]*N[2] + c*nu);

              # update a simple fraction

              if np = 0 then
                 newf := [1, 0];
              else
                 cp := content(np, var, 'np');
                 newf := [cp, np];
              end if;

              # update a branch

              if np = 0 and l = n + 1 then
                 print(here);
                 G := [op(1..l-1, M)];
                 k := nops(G);
                 while k > 1 and G[k][2] = 0 do
                    G := [op(1..k-1, G)];
                    k := k - 1;
                 end do;
              else
                 G := [op(1..n, M), newf, op(n+2..l, M)];
              end if;
           end if;

           # update the list

          if nops(G) < 2 then
             Fp := [op(1..i-1, F), op(i+1..m, F)];
          else
             Fp := [op(1..i-1, F), G, op(i+1..m, F)];
          end if; 
          break;
       end if;
    end do;
    if Fp = 0 then
       R := [var, p, [op(F), [de, [1,0]$(n-1), [c, nu]]]];
    else
       R := [var, p, Fp];
    end if;
    R;
end proc;
                 
#########################################################
# Name: DiffSimpleFraction
# Calling sequence
#       DiffSimpleFraction(F, x, var)
# Input: F, a nonzero simple fraction in the list representation
#        x, a variable;
#        var, the main variable;
# Output: The result of diff(F, x)
#########################################################

DiffSimpleFraction := proc(F, x, var)
    local c, de, nu, m, np, cp, cs, ns;
    (c, de, nu, m) := F[1], F[2], F[3], F[4]; 
    if nu = 0 then
       return [[1,0], [1,0]];
    end if; 
    np := rem(-nu*diff(de, x), de, var, 'q');
    np := primpart(np, var, 'cp');
    cp := c*m*cp;
    ns := primpart(diff(c, x)*nu + c*diff(nu, x) + c*m*q, var, 'cs');
    if ns = 0 then
       cs := 1;
    end if;
    if np = 0 then
       cp := 1;
    end if;
    [[cs, ns], [cp, np]];
end proc;

#########################################################
# Name: SumOfSimpleFractions
# Calling sequence
#       SumOfSimpleFractions(F1, F2, r)
# Input: F1, F2, two simple fractions with the same denominators and 
#        multiplicities (in the list representation)
#        var, the main variable;
# Output: The result F1 + F2; 
#########################################################

SumOfSimpleFractions := proc(F1, F2, var)
    local c1, de, nu1, m, c2, nu2, nu, c;
    (c1, nu1) := F1[1], F1[2];
    (c2, nu2) := F2[1], F2[2]; 
    nu := primpart(c1*nu1+c2*nu2, var, 'c');
    if c = 0 then
       c := 1;
    end if;
    [c, nu];
end proc;
    
#########################################################
# Name: DiffBranch
# Calling sequence
#       DiffBranch(B, x, var)
# Input: B, a nonempty branch of a partial fraction 
#        x, a variable;
#        var, the main variable;
# Output: The result diff(B, x)
#########################################################

DiffBranch := proc(B, x, var)
    local de, M, m, T, i, l, F, u, S;
    m := nops(B);
    de := B[1];
    M := [op(2..m, B)];
    m := m - 1;
    T := table([]);
    for i from 1 to m do
        T[i] := DiffSimpleFraction([M[i][1], de, M[i][2], i], x, var);
    end do; 

    F := table([]);
    F[1] := T[1][1];
    u := T[1][2]; 
    for i from 2 to m do
        F[i] := SumOfSimpleFractions(u, T[i][1], var);
        u := T[i][2];  
    end do;
    F[m+1] := [T[m][2][1], T[m][2][2]];

    l := m + 1;
    for i from m+1 by -1 to 1 do
        if F[i][2] = 0 then
           l := l - 1;
        else
           break;
        end if;
    end do;
    [seq(F[i], i=1..l)];
   # [de, seq(F[i], i=1..l)];
end proc; 
             
######################################################################
# Name: DiffPartialFraction
# Calling sequence
#       DiffPartialFraction(F, x)
# Input: F, a partial fraction in list representation;
#        x, a name;
# Output: diff(F, x), in list representation
######################################################################

DiffPartialFraction := proc(F, x)
   local var, p, pp, G, l, H, i, de, r;
   (var, p) := F[1], F[2];
   pp := diff(p, x);
   G := F[3];
   if G = [] then
      return [var, pp, []];
   end if;
   l := nops(G);
   H := table([]);
   for i from 1 to l do
       de := G[i][1];
       r := DiffBranch(G[i], x, var);
       if r = [] then
          H[i] := op([]);
       else
          H[i] := [de, op(r)];
       end if;
   end do; 
   [var, pp, [seq(H[i], i=1..l)]];
end proc;

#test
NormalizeBranch := proc(B)
  local nu, l, de, i;
  nu := 0;
  l := nops(B);
  de := B[1];
  for i from 2 to l do
      nu := normal(nu + B[i][1]*B[i][2]*de^(l-i));
  end do;
  normal(nu/de^(l-1)); 
end proc;

########################################################################
# Name: AddBranch1
# Calling sequence:
#       AddBranch1(B, var)
# Input: B, a list of branches whose denominators are equal
#        var, the main variable
# Output: the sum of these branches
#########################################################################

AddBranch1 := proc(B, var)
   local b, de, nu1, C, nu2, l1, l2, l, T, nu, c, ll, d, i, m;
   b := nops(B);
   if b = 0 then
      error "the input is an empty list";
   end if;
   if b = 1 then
      return B[1];
   end if;
   m := nops(B[1]); 
   de := B[1][1];
   nu1 := [op(2..m, B[1])];
   C := AddBranch1([op(2..b, B)], y);
   if nu1 = [] then
      return C;
   end if;
   m := nops(B[2]);
   nu2 := [op(2..m, C)];
   if nu2 = [] then
      return B[1]; 
   end if;
   l1 := nops(nu1);
   l2 := nops(nu2); 
   l := min(l1, l2);
   ll := max(l1, l2);
   T := table([]);
   for i from 1 to l do
       nu := normal(nu1[i][1]*nu1[i][2]+nu2[i][1]*nu2[i][2]);
       if nu = 0 then
          T[i] := [1, 0];
       else
          nu := primpart(nu, var, 'c');
          T[i] := [c, nu];
       end if;
   end do;
   if l1 > l then
      for i from l+1 to ll do
          T[i] := nu1[i];
      end do;
   end if; 
   if l2 > l then
      for i from l+1 to ll do
          T[i] := nu2[i];
      end do;
   end if; 
   d := ll;
   for i from ll by -1 to 1 do
       if T[i][2] = 0 then
          d := d - 1;
       else
          break;
       end if;
   end do;
   [de, seq(T[i], i=1..d)];
end proc;  

###########################################################
#Name: AddBranch
#Calling sequence:
#      AddBranch(B, L)
#Input: B, a branch
#       L, a list of partial fractions
#Output: B + L in list representation
############################################################
  

AddBranch := proc(B, L)
   local var, p, de, F, l, i, H, dh, G;
   if nops(B) = 1 then
      return L;
   end if;
   (var, p) := L[1], L[2];
   F := L[3];
   if F = [] then
      return [var, p, [B]];
   end if;
   de := B[1];
   l := nops(F);
   for i from 1 to l do
       H := F[i];
       dh := H[1];
       if Testzero(de-dh) then
          G := AddBranch1([B, H], var);
          if nops(G) = 1 then
             return [var, p, [op(1..i-1, F), op(i+1..l, F)]];
          end if;
          return [var, p, [op(1..i-1, F), G, op(i+1..l, F)]];
        end if;
    end do;
    [var, p, [op(F), B]];
end proc; 

###########################################################
#Name: AddTwoPartialFractions
#Calling sequence:
#      AddTwoPartialFractions(B, L)
#Input: P, Q Two partial fractions
#       
#Output: P + Q in list representation
############################################################
  
AddTwoPartialFractions := proc(P, Q)
   local var, p, i, R;
   var := P[1];
   p := normal(P[2] + Q[2]);
   R := Q;
   for i from 1 to nops(P[3]) do
       R := AddBranch(P[3][i], R);
   end do;
   [var, p, R[3]];
end proc;
       
      
end module: