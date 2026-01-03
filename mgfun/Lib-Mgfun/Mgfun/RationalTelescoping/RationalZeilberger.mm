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
## Authors: Shaoshi Chen <schen@amss.ac.cn>.

RationalZeilberger := module()
   description "Creative telescoping for rational functions by Almkvist & Zeilberger's algorithm";
   export 
          VerifyHT,
          RatTelescoping;
   local  
          ApplyL,
          LowerBoundForRank,
          ParadGosper;

   option package;

  ####################################################################
  # This code is from OreTools_ModularGCRDLCLM                          
  # Name: LowerBoundForRank                                              
  # Calling sequence:                                                   
  #                   LowerBoundForRank(M, p)                           
  #                                                                     
  # Input: M, A matrix with entries being multivariate polynomials      
  #           over p;                                                   
  #        p, a prime;                                                  
  # Output: b, a nonnegative integer less than or                       
  #            equal to the rank of M.                                         
  ####################################################################
  LowerBoundForRank := proc(M, p)
        local rn, cn, vars, g, e, N, i, j, en, s, b, de, dei, MM;

        #------------------------------
        # 1. Collect info about matrix
        #------------------------------

        rn, cn := op(1, M);
        vars := convert(indets(M), list);


        #---------------------------------------
        # Calculate rowwise common denominators
        #---------------------------------------
        de := table();
        for i from 1 to rn do
            de[i] := Lcm(seq(denom(M[i, j]), j=1..cn)) mod p;
        end do;
        MM := Matrix([seq([seq(Normal(de[i]*M[i,j]) mod p, j=1..cn)], i=1..rn)]);
       

        #----------------------------------
        # 2. Choose two evaluation points
        #----------------------------------

        g := rand(1..p-1);
        s := {seq(vars[i]=g(), i=1..nops(vars))};
        
        

        #---------------
        # 3. Evaluation
        #---------------
        N := Matrix([seq([seq(Expand(subs(s, MM[i, j])) mod p, j=1..cn)], i=1..rn)]);
        
        #--------------------
        # 4. Compute rank(N)
        #--------------------

        LinearAlgebra:-Modular:-RowReduce(p, N, rn, cn, cn, 0, 
                      0, b, 0, 0, false);
        return b

   end proc;

###############################################################
# Name: ParadGosper
# Calling sequence:
#         ParadGosper(rN, e, del, L, de, x, y, Dx)
#
###############################################################
ParadGosper := proc(rN, e, del, L, de, x, y, Dx)
    local s, c, eqn, var, M, rk, p, tele, cer, pva, pva1, pva2,
          sol, ev, ev1, ev2, roll, j, tt;
    
    roll := rand(1..1000);
    ## linear constraints on e
    if Testzero(del) then
       eqn := [coeffs(collect(rN, y), y)];
       var := e;
       M := LinearAlgebra:-GenerateMatrix(eqn, var)[1];
       p := ithprime(roll());
       rk := LowerBoundForRank(M, p);
       if rk < nops(var) then
          ## tt := time();
          ## print(begin_LinearSolver);
          sol := {SolveTools:-Linear(eqn, var)};
          ## print(time_for_LinearSolver);
          ## print(time()-tt);
          if nops(sol)=0 then
             return [];
          else
             tele := add(e[j]*Dx^(j-1), j=1..nops(e));
             tele := eval(tele, sol[1]);
             pva := convert(indets(tele) minus {x, Dx}, list);
              ev := {seq(pva[j]=1, j=1..nops(pva))};
             return [eval(tele, ev), 0]
          end if;
       else 
          return []
       end if;
    end if;
  
    c := table([]);
    s := add(c[j]*y^j, j=0..del-1);
    eqn := [coeffs(collect(rN-L[1]*diff(s, y)-L[2]*s, y), y)];
    var := [op(e), seq(c[j], j=0..del-1)];
    M := LinearAlgebra:-GenerateMatrix(eqn, var)[1];
    p := ithprime(roll());
    rk := LowerBoundForRank(M, p);
    if rk < nops(var) then
          ## tt := time();
          ## print(begin_LinearSolver);
       sol := {SolveTools:-Linear(eqn, var)};
          ## print(time_for_LinearSolver);
          ## print(time()-tt);
       #print(sol);
       #print(nops(sol));
       
       if nops(sol)=0 then
          return [];
       else
          #print(e);
          tele := add(e[j]*Dx^(j-1), j=1..nops(e));
          # print(tele);
          tele := eval(tele, sol[1]);
          # print(tele);
          if Testzero(tele) then
             return []
          end if;
          cer := eval(s, sol[1]);
          pva := convert(indets(tele) minus {x, Dx}, list);
          ev := {seq(pva[j]=1, j=1..nops(pva))};
          return [eval(tele, ev), eval(cer, ev)/de];
         # pva1 := convert(indets(tele) minus {x, Dx}, list);
         # pva2 := convert(indets(cer) minus {x, y, op(pva1)}, list);
         # print([pva1, pva2]);
         # ev1 := {seq(pva1[j]=1, j=1..nops(pva1))};
         # ev2 := {seq(pva2[j]=0, j=1..nops(pva2))};
         # print(ev);
         # ev := {seq(pva1[j]=1, j=1..nops(pva1))};
         # ev := {seq(e[j]=1, j=1..nops(e)), seq(c[j]=0, j=0..del)};
         # return [eval(tele, ev1), eval(eval(cer, ev1), ev2)/de]
       end if;
    else 
       return []
    end if;
   

end proc;


###############################################################
# Name:  RatTelescoping
# Calling sequence:
#          RatTelescoping(f, x, y, Dx)
# Input: f, a rational function of x and y;
#        x, y, two variables;
#        Dx, a operator name;
# Output: [L, g], a differential Z-pair for f, such that
#
#                 L(f) = diff(g, y)
#
###############################################################
RatTelescoping := proc(f, x, y, Dx)
   local fp, nu, de, re, poly, P, Q, A, pp, B, C, 
         Am, As, Bm, Bs, Cm, Cs, Cd, ACd, Bd, ACs, dACs, ord,
         i, ithDf, N, rN, e, iden, j, R, delL, k, a, b, L, g, cL;
   
   ## Initialize
   fp := normal(f);
   nu, de := numer(fp), denom(fp);
   re := rem(nu, de, y, 'poly');
   poly := int(poly, y);

   ## Case: f is a polynomial in y 
   if Testzero(re) then
      return [1, poly]
   end if;
   fp := normal(re/de);
   P, Q := numer(fp), denom(fp);

   ## Decompose Q=A(x)B(y)C(x, y)
   A := content(Q, y, 'pp');
   B := content(pp, x, 'C');
   Am := gcd(A, diff(A, x));
   As := normal(A/Am);
   Bm := gcd(B, diff(B, y));
   Bs := normal(B/Bm);
   Cm := gcd(C, diff(C, y));
   Cs := normal(C/Cm);
   ACs := As*Cs;
   dACs := diff(ACs, x);
   Bd := normal(Bs*diff(B, y)/B);
   ACd := normal(ACs*diff(Am*Cm, x)/(Am*Cm));
   Cd := normal(Cs*diff(C, y)/C);

   ## Order bound
   ord := degree(Bs, y) + degree(Cs, y);

   ## degree bound formular
   delL := degree(Q, y) + (k-1)*degree(Cs, y) - degree(Bs, y); 

   ## Key equation formular
   a := Bs*Cs;
   b := diff(Bs*Cs, y) -Cs*Bd-Bs*(k*diff(Cs, y)+Cd);
   
   ## prepare for searching a telescoper 
   e := table([]);
   N := table([]);
   N[0] := P;
   rN := e[0]*P;
   iden := A*Cm*Bm;
   R := ParadGosper(rN, [e[0]], eval(delL, k=0), [a, eval(b, k=0)], iden, x, y, Dx);
   if nops(R) = 2 then
          L := primpart(R[1], Dx, 'cL');
	  g := normal((ApplyL(R[1], poly, x, Dx) + R[2])/cL);
          return [L, g]
          #return [1, normal(poly + R[2])]
   end if;
   
   for i from 1 to ord do
       ## print(i);
       N[i] := normal(normal(diff(N[i-1], x)*ACs)-normal(N[i-1]*normal(i*dACs + ACd)));
       rN := normal(e[i]*N[i] + ACs*rN);
       iden := iden*ACs;
       R := ParadGosper(rN, [seq(e[j], j=0..i)], eval(delL, k=i), [a, eval(b, k=i)], iden, x, y, Dx);
       if nops(R) = 2 then
	      L := primpart(R[1], Dx, 'cL');
	      g := normal((ApplyL(R[1], poly, x, Dx) + R[2])/cL);
          return [L, g]
       end if;
   end do; 
   
end proc;

##########################
# ApplyL(L, f, x, Dx)
# compute L(f)
#
##########################
ApplyL := proc(L, f, x, Dx)
   local n, i, fp, df;
   n := degree(L, Dx);
   fp := coeff(L, Dx, 0)*f;
   df := f;
   for i from 1 to n do
       df := diff(df, x);
       fp := normal(fp + coeff(L, Dx, i)*df);     
   end do;
   fp;

end proc; 

############################
# VerifyHT(R, f, x, y, Dx)
# Verify the return of RatTelescoping
#
############################
VerifyHT := proc(R, f, x, y, Dx)
  Testzero(ApplyL(R[1], f, x, Dx)-diff(R[2], y));
end proc;

end module:
