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

## Copyright (c) 2010, INRIA & KLMM. All rights reserved.
## Authors: Shaoshi Chen <schen@amss.ac.cn> and Ziming Li <zmli@mmrc.iss.ac.cn>.

Division := module()
  description "polynomial extended gcd computation"; 
  export 
         ExactDivision, 
         HalfEuclideanList,
         MergeInverse,
         MergeRemainder;
  option package;



 ####################################################################
 # Name: HalfEuclideanList                                          #
 # Calling sequence:                                                #
 #                    HalfEuclideanList(L, g, x)                    #
 # Input: L, a list of nonzero polynomials in x;                    #
 #        g, a nonzero polynomial in x such that f and g are        #
 #           co-prime for all f in L;                               #
 #        x, a name.                                                #
 # Output: C, a list of polynomials such that C[i]*L[i] =1 mod g;   #
 ####################################################################
 

 HalfEuclideanList := proc(L, g ,x)
     local C, i, h, s;
     C := [];
     for i from 1 to nops(L) do
       h := gcdex(L[i], g, x, 's');
       if degree(h, x) > 0 then
          error "a nontrivial gcd is found";
       end if;
       C := [op(C), s];
     end do; 
 end proc;


 ###############################################################
 # Name: ExactDivision                                         #
 # Calling sequence:                                           #
 #                    ExactDivision(P1, P2, m, x)              #
 # Input: P1, P2, two polynomials in x;                        #
 #        m, a positive integer                                #
 #        x, a name                                            #
 # Output: q,  q is a polynomial such that q is the            #  
 #             quotient of P1 by P2^m.                         #         
 ###############################################################


 ExactDivision :=proc(P1, P2, m, x) 
   local c1, c2, q, i, pp2, t;
  
       q := primpart(P1, x, 'c1');
       pp2 := primpart(P2, x, 'c2');
       for i from 1 to m do
           t := divide(q, pp2, 'q');
           if t = false then 
              error "the division is not exact";
           end if;
       end do;
       q*c1/c2^m;
 end proc;


#Division :=proc(P1, P2, m, x) 
#   local c1, c2, q, i, pp2, t;
# 
#       tt := time(); 
#       q, c1 := numer(P1), denom(P1); 
#       pp2, c2 := numer(P2), denom(P2);
#       print(time_taken_for_primpart);
#       tt := time()-tt;
#       print(tt); 
#       tt := time();
#       for i from 1 to m do
#           t := divide(q, pp2, 'q');
#           if t = false then 
#              error "the division is not exact";
#           end if;
#       end do;
#       tt := time()-tt;
#       print(time_taken_for_divide); print(tt);
#       q*c2^m/c1;
# end proc;
  

 ################## Merge Remainder ##########################
 # Calling sequence:                                         #
 #                   MergeRemainder(L, b, x)                 #
 # Input: L, a list of polynomials;                          #
 #        b, a nonzero polynomial;                           #
 #        x, a name of variable;                             #
 # Output:  the remainder of the product of all elements     #
 #          in L by b w.r.t x.                               #
 #############################################################

MergeRemainder := proc(L, b, x)
    local n, np, r, i, L1, L2;

    n := nops(L);

    if degree(b, x) = 0 then
       return 0;
    end if;

    if n = 1 then
       return rem(L[1], b, x);
    end if;
    np := floor(n/2);
    L1 := [seq(L[i], i=1..np)];
    L2 := [seq(L[i], i=np+1..n)];
    r  := rem(MergeRemainder(L1, b, x)*MergeRemainder(L2, b, x), b, x);
 end proc:


 ####################################################################
 # Name: MergeInverse                                               #
 # Calling sequence:                                                #
 #                   MergeInverse(L, g, C, x,'t')                   #
 #                                                                  #
 # Input: L, a list of polynomials co-prime with g;                 #
 #        g, a nonzero polynomial;                                  #
 #        x, a name;                                                #
 #        C, a list of polynomials such that                        #
 #           C[i]*L[i] =1 mod g;                                    #
 #        t, (optional) unevaluated name.                           #
 # Output: a polynomial s such that                                 #
 #                   s*(Prod_i L[i]) = 1 mod g.                     #
 # Option: if the fifth argument t is presented, then it is         #
 #         assigned the polynomial such that                        #
 #                 s*(Prod_i L[i]) + t*g = 1.                       #
 ####################################################################



 MergeInverse := proc(L, g, C, x, t)

   local s, h, f;
   s := MergeRemainder(C, g, x);
   if nargs = 5 then
      h := mul(L[i], i=1..nops(L));
      f := 1 - h*s;
      t := ExactDivision(f, g, x);
   end if;
   s; 
 end proc:

end module:
