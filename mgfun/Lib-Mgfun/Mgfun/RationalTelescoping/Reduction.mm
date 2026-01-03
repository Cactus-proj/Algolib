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

Reduction := module()
    description "Computing Hermite reduction of a rational function";
    
    export
          Original,
          VerifyOriginal;

    local 
          IntegrationByParts,
          ReduceOneFactor,
          ReduceOneFactor1,
          VerifyReduceOneFactor;
                     
    option package;


######################################################################
# Calling sequence:                                                  
#                   IntegrationByParts(L1, C, x)                     
#                                                                    
#   Input:  L1, A list [Q, V, k] with                                
#               Q, a polynomial;                                       
#               V,  a squarefree polynomial;
#                   degree(nu, x) < degree(f, x);
#               k,  a positive integer; 
#                             
#          C, a polynomial such that 
#
#               C*V = 1 mod diff(V, x);
#         
#           x,  a name                        
#   Output: [g, r], A pair of rational functions such that           
#                 n/f^k = Dx(g) + r,                                   
#           where r has a squarefree denominator.                      
######################################################################

 IntegrationByParts := proc(L1, L2, x)
    local Q, V, k, Vp, g, Qs, Cs, Bs,  C, B, m;

    # Initialize

       (Q, V, k) := L1[1], L1[2], L1[3];
       (C, B) := L2[1], L2[2];
       Vp := diff(V, x);
       g := 0; 
      

    # Integration by parts

      for m from k by -1 to 2 do
        Qs := Q/(1-m);
        Cs, Bs := rem(C*rem(Qs, Vp, x), Vp, x), rem(B*rem(Qs, V, x), V, x);
        #Bs := Division:-ExactDivision(Qs - Cs*V, Vp, 1, x); 
        g := g + Bs/V^(m-1); 
        Q := normal((1-m)*Cs - diff(Bs, x));
        if Q = 0 then
           break;
        end;
      end do;
     
     # prepare for return

       [g, Q, V];  
 end proc;

################################################################
# Calling sequence:                                            
#                   ReduceOneFactor(L, x, t)                      
#                                                              
#   Input:  L, a list [f, n1, ..., ns] ;                       
#           x, a name.                   
#   Output: [g, r], A pair of rational functions such that  
#   
#           n1/f + ... + ns/f^s = diff(g, x) + r,  
#                 
#           where r has a squarefree denominator. 
#  Optional: If nargs = 3 then  t = [C, B] such tat 
#                  C*f + B*diff(f, x)=1.             
################################################################

ReduceOneFactor1:= proc(L, x, t)
     local f, df, g, r, n, B, C, i, L1, R;

   # Initialize
     f  := L[1];
     df := diff(f, x);
     g  := 0;
     r  := L[2];
     n  := nops(L);
     if n = 2 then
        if nargs = 3 then 
           gcdex(f, df, x, 'C', 'B');
           t := [C, B];
        end if;
        return([g, r, f])    
     end if; 

   # Extended Euclidean Algorithm
   
     gcdex(f, df, x, 'C', 'B');

   # Integration by parts

     for i from 3 to n do
         L1 := [L[i], f, i-1];
         R  := IntegrationByParts(L1, [C, B], x);
         g  := normal(g + R[1]);
         r  := r + R[2];
     end do;
     if nargs = 3 then 
        t := [C, B];
     end if;

    [g, r, f];
end proc;


################################################################
# Name: ReduceOneFactor (July 03, 2009)
# Calling sequence:                                            
#                   ReduceOneFactor(L, x, t)                      
#                                                              
#   Input:  L, a list [f, [c1, n1], ..., [cn, ns]] ;                       
#           x, a name.                   
#   Output: [g, r], a pair of rational functions (in list representation)
#           such that  
#   
#           c1*n1/f + ... + cn*ns/f^s = diff(g, x) + r,  
#                 
#           where r has a squarefree denominator. 
#  Optional: If nargs = 3 then  t = [C, B] such tat 
#                  C*f + B*diff(f, x)=1.             
################################################################
ReduceOneFactor := proc(L, x, t)
   local f, df, g, r, n, B, C, i, u, ns, cs, pp, C1, B1, tst;
    
   # Initialize
     f  := L[1];
     df := diff(f, x);
     g  := [];
     r  := L[2];
     n  := nops(L);
     if n = 2 then
        if nargs = 3 then 
           gcdex(f, df, x, 'C', 'B');
           t := [C, B];
        end if;
        return([g, [f, r]])    
     end if; 

   # Extended Euclidean Algorithm
   
     gcdex(f, df, x, 'C', 'B');

   # Integration by parts        
     u := 0;
     for i from n by -1 to 3 do
         ns := u + L[i][1]*L[i][2];
         cs := content(ns, x, 'ns');
         C1 := rem(C*rem(ns, df, x), df, x);
         B1 := rem(B*rem(ns, f, x), f, x);
         g := [[cs*content(B1, x, 'pp')/(2-i), pp], op(g)];
         u := normal(cs*(C1+diff(B1, x)/(i-2)));
     end do;
         r := [f, [content(u+L[2][1]*L[2][2], x, 'pp'), pp]];
     if nargs = 3 then 
        t := [C, B];
     end if;
     g := [f, op(g)];
     [g, r];

end proc:

################################
#    VerifyReduceOneFactor
# Verify the correctness of the 
# output of ReduceOneFactor
################################
VerifyReduceOneFactor := proc(L, G, R, x)
     local f, g, r, n, ng, nr, i;
     n := nops(L);
     f := add(L[i][1]*L[i][2]/L[1]^(i-1), i=2..n);
     ng := nops(G);
     g := add(G[i][1]*G[i][2]/G[1]^(i-1), i=2..ng);
     nr := nops(R);
     r := add(R[i][1]*R[i][2]/R[1]^(i-1), i=2..nr);
     Testzero(diff(g, x)+r-f);
 end proc:

############### Hermite Reduction --- Original Version ###################                               
# Name: Original  (July 03, 2009)                                        
# Calling sequence:                                                  
#                   Original(f, x, opt, Cf)                              
#                                                                    
#   Input:  f, a rational function in x;                              
#           x, a name. 
#   Output: c,  a rational function free of x;
#           g, r, two rational functions, in list representation such that
#              
#                    f = diff(c*g, x) + c*r,
#
#           where degree(numer(r), x) < degree(denom(r), x) and denom(r) is squarefree.
#           Cf, a list of cofactors;
#   optional: if nargs = 3 and lhs(opt) = 'output' then
#                if rhs(opt) = 'normalized' then
#                   return [c, g, r], with g, r in usual normalized rational forms.
#                if rhs(opt) = 'unnormalized' then
#                   return [c, g, r] with g, r in usual unnormalized rational forms.
#                if rhs(opt) = 'parfrac' then
#                   return [c, g, r] with g, r in usual partial fraction forms. 
#              
###########################################################################
Original:= proc(f, x, opt)

   local R, G, c, p, L, g, r, gn, rn, n, i, cs, c1, p1, Rp, t, Cf;

     # Partial fraction decomposition
     R := Decomposition:-SquareFreePartialFraction(f, x);
     (c, G) := R[1], R[2];

     # polynomial part    
     p := int(G[2], x);
     if G[3] = [] then
        if nargs = 3 then
           if type(args[3], `=`) then
              if lhs(opt)='output' and rhs(opt)='normalized' then
                   return([c, p, 0, [[1, 0]]])
              else if lhs(opt)='output' and rhs(opt)='unnormalized' then
                   return([c, p, 0, [[1, 0]]])  
                   else if lhs(opt)='output' and rhs(opt)='parfrac' then
                           return([c, p, 0, [[1, 0]]])
                        end if;
                   end if;
              end if;
           else 
              error ("Original expects its 3th argument, opt, to be of type equation, but received ", opt);
           end if;        
        end if;
        g := [x, p, []];
        r := [x, 0, []];
        return [c, g, r, [[1, 0]]]
     end if;

     
     L := [op(G[3])];
     n := nops(L); 
     g := [];
     r := []; 
     Cf := [];
   # rational part
   
     for i from 1 to n do
         Rp := ReduceOneFactor(L[i], x, 't');   
        #  Cf := [op(Cf), t];
         if Rp[1] <> [] then
            g := [op(g), Rp[1]];
         end if;
         if Rp[2][2][2] <> 0 then
            r := [op(r), Rp[2]];
            Cf := [op(Cf), t];
         end if;
     end do;
     if nargs = 3 then
           if type(args[3], `=`) then
              if lhs(opt)='output' and rhs(opt)='normalized' then
                  gn := List:-FromListToNormal([x, p, g]);
                  rn := List:-FromListToNormal([x, 0, r]);
                  return([c, gn, rn, Cf]);
              else if lhs(opt)='output' and rhs(opt)='unnormalized' then
                       gn := List:-FromListToPartialFraction([x, p, g]);
                       rn := List:-FromListToPartialFraction([x, 0, r]);
                       return([c, gn, rn, Cf]); 
                   else if lhs(opt)='output' and rhs(opt)='parfrac' then
                           return( [c, [x, p, g], [x, 0, r], Cf]);
                        end if;
                   end if;
              end if;
           else 
              error ("Original expects its 3th argument, opt, to be of type equation, but received ", opt);
           end if;        
     end if;
      
     [c, [x, p, g], [x, 0, r], Cf];

end proc;



###################################################
# VerifyOriginal
# Verify the correctness of the output of Original
###################################################
 
VerifyOriginal := proc(R, f, x)
      local g, r, c;
      g := List:-FromListToPartialFraction(R[2]);
      r := List:-FromListToPartialFraction(R[3]);
      Testzero(R[1]*(diff(g, x)+r)-f);
end proc:

end module:     
 
