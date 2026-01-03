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

Telescoper := module()
    description "Computing the minimal telescoper for a rational function":
    export
          GeddesLe,
          Hermite,
          ModularVerifyFunction,
          ModularVerifyList, 
          VerifyHermiteFunction,
          VerifyHermiteList;                            
    local
          FindLinearRelation,         
          GenerateCertificate, 
          GenerateTelescoper, 
          GetCoeffs,      
          LowerBoundForRank,
          NewIntegrablePart,
          SimpleReduce, 
          SimpleReduce1,
          SimpleReduce2,          
          VerifySimpleReduce,
          VerifyNewIntegrablePart; 
       
    option package:


  
    
  ###################################################################
  # Name: GetCoeffs
  # Calling sequence:
  #                    GetCoeffs(L, y)
  # Input: L, a list has the form [[c_1, a_1, d_1], ..., [c_n, a_n, d_n]],
  #           where d_i's are pairwisely coprime:
  #
  # Output: [Lp, c], Lp is a list of coefficients of the numerator of 
  #                a/(c*d) = a_1/(c_1*d_1)+...+a_n/(c_n*d_n),
  #         and c is content of denominator w. r. t. y.
  #################################################################### 


  GetCoeffs := proc(L, y)
     local n, C, A, B, i, j, Bs, nu, c:
     n := nops(L):
     (C, A, B) := table([]), table([]), table([]):
     for i from 1 to n do
       (C[i], A[i], B[i]) := L[i][2][1], L[i][2][2], L[i][1]:
     end do:
     Bs := table([]):
     for i from 1 to n do
         Bs[i] := mul(B[j], j=1..i-1)*mul(B[j], j=i+1..n):
     end do:
     nu := add(C[i]*A[i]*Bs[i], i=1..n):
     c := content(nu, y, 'a'):
     if c = 0 then 
        c := 1:
     end if:
     [PolynomialTools:-CoefficientList(a, y), c]:
   end proc:


             
  ####################################################################
  # This code is from OreTools_ModularGCRDLCLM                          
  # Name: LowerBoundForRank                                              
  # Calling sequence:                                                   
  #                   LowerBoundForRank(M, p)                           
  #                                                                     
  # Input: M, A matrix with entries being multivariate polynomials      
  #           over p:                                                   
  #        p, a prime:                                                  
  # Output: b, a nonnegative integer less than or                       
  #            equal to the rank of M.                                         
  ####################################################################

   LowerBoundForRank := proc(M, p)
        local rn, cn, vars, g, e, N, i, j, en, s, b, de, dei, MM:

        #------------------------------
        # 1. Collect info about matrix
        #------------------------------

        rn := op(1, M)[1]:
        cn := op(1, M)[2]:
        vars := [op(indets(M))]:


        #---------------------------------------
        # Calculate rowwise common denominators
        #---------------------------------------

        de := []:
        for i from 1 to rn do
            dei := []:
            for j from 1 to cn do
                dei := [op(dei), denom(M[i,j])]:
            end do:
            de := [op(de), Lcm(op(dei)) mod p]:
        end do:

        MM := Matrix(rn, cn):
        for i from 1 to rn do
            for j from 1 to cn do
                MM[i,j] := Normal(de[i]*M[i,j]) mod p:
            end do:
        end do:


        #----------------------------------
        # 2. Choose two evaluation points
        #----------------------------------

        g := rand(1..p-1):
        e := [seq(g(), i=1..nops(vars))]:
        s := {seq(vars[i]=e[i], i=1..nops(vars))}:

        #---------------
        # 3. Evaluation
        #---------------

        N := Matrix(rn, cn):
        for i from 1 to rn do
                for j from 1 to cn do
                    N[i, j] := Expand(subs(s, MM[i, j])) mod p:
                end do:
        end do:

        #--------------------
        # 4. Compute rank(N)
        #--------------------

        LinearAlgebra:-Modular:-RowReduce(p, N, rn, cn, cn, 0, 
                      0, b, 0, 0, false):
        return(b):

   end proc:
  
  ###############################################################
  # Name: SimpleReduce1
  # Calling sequence:
  #                     SimpleReduce1(r, s, x, y)
  # Input: r, a list of three polynomials c, A and D with c in 
  #           K[x] and D squarefree with respect to y.
  #        s, a polynomial such that s*diff(D, y)=1 mod D:
  #        x, y, variable names.
  # Output: g, a rational function:
  #         rp, a list of cp, Ap, and Dp such that
  #                diff(A/(cD*D), x) = diff(g, y) + Ap/cp*Dp.
  ################################################################

  SimpleReduce1 :=proc(r, s, x, y)
  
      local nu, co, de, R, g, c1, p1, Ap, cp, cs, pp:
    
         #----------------
         # 1. Initializing
         #----------------
         co := r[2][1]:
         nu := r[2][2]:
         de := r[1]:

         if nu = 0 then
            return [[de, [1, 0]], [de, [1, 0]]];
         end if:

         #-----------------------------
         # 2. Hermite reduction for 
         #    diff(nu/(cc*de), x)
         #-----------------------------

          R  := SimpleReduce2([-nu*diff(de, x), de], s, y):
          cs := content(R[1], y, 'pp');
          if cs = 0 then
             g := [de, [1, 0]];
          else
             g  := [de, [normal(co*cs), pp]];
          end if;   
          cp := content(diff(co, x)*nu + co*diff(nu, x) + co*R[2], y, 'Ap'):
          if cp = 0 then
             cp := 1:
          end if:
          [g, [de, [cp, Ap]]]: 
  end proc:

  
   
  ########################################################
  # Name: SimpleReduce2                                   
  # Calling sequence:                                    
  #                  SimpleReduce2(f, s, y)               
  # Input: f, [A, D] represents a ratioanl function of 
  #           the form A/D^2, where D is squarefree in y:
  #        s, a polynomial such that s*diff(D, y)=1 mod D:       
  #        y, a variable name.                           
  # Output: [g, Ap, Dp], a list such that          
  #               f = diff(g, y)+Ap/Dp.                     
  ########################################################

  SimpleReduce2 :=proc(f, s, y)
      local nu, de, B, C, g, Ap, c, d:
         
         #----------------
         # 1. Initializing
         #----------------
         nu := f[1]:
         de := f[2]:

         #--------------------------------- 
         # 2. Computing the Bezout relation 
         #    B*diff(de, x)+C*de=nu
         #---------------------------------
         B := Division:-MergeRemainder([nu, s], de, y):
         C := Division:-ExactDivision(normal(nu-B*diff(de, y)), de, 1, y):
        
         #--------------------------
         # 3. Integrating by parts
         #--------------------------
   
          #g  := normal(-B/de):
          
          Ap := normal(diff(B, y)+C); 
          #Ap := normal(diff(B, y)+C):          
          [-B, Ap]:
 
  end proc:

  ########################################################
  # Name: SimpleReduce                                    
  # Calling sequence:                                    
  #                  SimpleReduce(F, S, x, y)               
  # Input: F,  a list of fractons [de, [c, nu]];
  #        S,  the list of s such that s*diff(de, y)=1 mod de:       
  #        x, y, names.                           
  # Output: [G, R], a list such that          
  #               diff(F[i], x) = diff(G[i], y) + R[i].                     
  ########################################################

  SimpleReduce := proc(F, S, x, y)
     local l, T, i;
     l := nops(F);
     T := table([]);
     for i from 1 to l do
         T[i] := SimpleReduce1(F[i], S[i], x, y);
     end do;
     [[seq(T[i][1], i=1..l)], [seq(T[i][2], i=1..l)]];
  end proc; 
   
  VerifySimpleReduce := proc(L, F, x, y)
    local l, G, R, t, i, de1, c1, nu1, de2, c2, nu2, de3, c3, nu3;
    (G, R) := L[1], L[2];
    l := nops(G);
    for i from 1 to l do
        (de1, c1, nu1) := F[i][1], F[i][2][1], F[i][2][2];
        (de2, c2, nu2) := G[i][1], G[i][2][1], G[i][2][2];
        (de3, c3, nu3) := R[i][1], R[i][2][1], R[i][2][2];
        t := Testzero(diff(c1*nu1/de1, x) - diff(c2*nu2/de2, y) - c3*nu3/de3);
        if t = false then
           return false;
        end if;
    end do;
    true;
  end proc;

  ####################################################################
  # Name: NewIntegrablePart
  # Calling sequence:
  #       NewIntegrablePart(G1, G2, x, y)
  # Input: G1, an old integrable part (a list of partial fractions);
  #        G2, a new integrable part (a list of simple fractions);
  #        x, y, two names
  # Output: diff(G1, x) + G2 as a list of partial fractions
  #####################################################################

  NewIntegrablePart := proc(G1, G2, x)
     local G, i, l, f;
     G := List:-DiffPartialFraction(G1, x);
     l := nops(G2);
     for i from 1 to l do
         f := G2[i];  
         G := List:-AddSimpleFraction([f[2][1], f[1], f[2][2], 1], G);
     end do;
     G;
  end proc; 

  VerifyNewIntegrablePart := proc(Gnew, G1, G2, x)
     local gnew, g1, var, g2; 
     gnew := List:-FromListToPartialFraction(Gnew);
     g1 := List:-FromListToPartialFraction(G1);
     var := G1[1];
     g2 := List:-FromListToPartialFraction([var, 0, G2]);
     Testzero(gnew - diff(g1, x) - g2);
  end proc; 

 #######################################################################
 # Name: GenerateTelescoper
 # Calling sequence
 #     GenerateTelescoper(l, c, W, x, Dx)
 # Input: l, a positive integer (l-1 is the order of the 
 #           minimal telescoper of a rational function f
 #        c, the content of f
 #        W, the sequence of coefficients of a minimal telescoper 
 #           of 1/c*f ( l = nops(W) )
 #        x, a name
 #        Dx, a name         
 # Output: a minimal telescoper of 1/c*f
 #######################################################################

 GenerateTelescoper := proc(l, c, W, x, Dx)
    local T, P, i:
    T := W[0]*1/c:
    P := 1/c:
    for i from 1 to l do
        P := DEtools[mult](Dx, P, [Dx, x]):
        T := normal(T + W[i]*P):
    end do:
    collect(T, Dx):
 end proc:

 #######################################################################
 # Name: GenerateCertificate
 # Calling sequence
 #     GenerateCertificate(l, c, W, x, Dx)
 # Input: l, a positive integer (l-1 is the order of the 
 #           minimal telescoper of a rational function f
 #        c, the content of the telescoper T computed by GenerateTelescoper
 #        W, the sequence of coefficients of a minimal telescoper 
 #           of 1/c*f ( l = nops(W) )
 #        x, a name
 #        Dx, a name         
 # Output: the certificate corresponding to the primitive part of T;  
 #######################################################################

 GenerateCertificate := proc(l, c, W, G)
    local T, H, i; 
    T := table([]);  
    for i from 0 to l do
        T[i] := List:-ScalarMultiply(W[i], G[i]);
    end do; 
    H := T[0]; 
    for i from 1 to l do
       H := List:-AddTwoPartialFractions(T[i], H);
    end do;
    List:-ScalarMultiply(1/c, H);
  end proc; 

 #######################################################################
 # Name: FindLiearRelation
 # Calling sequence
 #       FindLinearRelation(M, n)
 # Input: M, a matrix 
 #        n, the number of columns of M         
 # Output: the left kernel of the transpose of M  
 #######################################################################    

 FindLinearRelation := proc(M, n)
    local i, m, p, r:  
    for i from 1 to 2 do
      m := rand(6..1000):
      p := ithprime(m()):
      r := LowerBoundForRank(M, p):
      if r = n then
         return {}:
      end if:
   end do: 
   LinearAlgebra:-NullSpace(LinearAlgebra:-Transpose(M)):
 end proc: 
 
 ##################################################################################################
 # Name: Hermite
 # Calling sequence
 #        Hermite(f, x, y, Dx, opt)
 # Input: f, a rational function
 #        x, y, Dx, names         
 # Output: [L, G], L is the primitive minimal telescoper of f w.r.t y
 #                 G is the certificate
 # 
 # G is a list of partial fractions if there are 4 arguments.
 #
 # Optional: The fifth argument should be present as an equation of the form
 #           'output' = 'unnormalized', 'normalized', or 'parfrac'.
 #
 #           if rhs = 'unnormalized', then returns G as a sum of fractions without normalization;
 #           if rhs = 'normalized', then returns G as a normalized fraction;
 #           if rhs = 'parfrac', then returns G as a list representation of the partial
 #                                             fraction decomposition of G.
 ################################################################################################## 
 Hermite := proc(f, x, y, Dx, opt)
  local F, de, g, H, l, S, Sp, G, R, c, r, L, P, Cp, ord, sol, M,
        Q, i, T, C, ct, Cert, CertL, g1, g2, gd, lh, rh, U, V, lu, 
        tr, tm, tu, tl, ts, tt: 

  F := normal(f):
  de := denom(F):

  if nargs = 5 then
     if not type(opt, equation) then
        error "the optional argument should be of equation type"
     end if;
     (lh, rh) := lhs(opt), rhs(opt);
  end if;
 

  # polynomial case
  
  if degree(de, y) = 0 then
     g := int(F, y):
     if nargs = 5 then
        if lh = 'output' then
           if rh = 'normalized' or rh = 'unnormalized' then
              return [1, g];
           end if;
        end if;
     end if;
     return [1, [y, g, []]]: 
  end if:

  # Hermite Reduction

  tr := time():

  H := Reduction:-Original(F, y):
  (c, g, r, Sp) := H[1], H[2], H[3], H[4]; 

  ## tr := time() - tr:
  ## print(time_for_reduction): print(tr): 

  # Rational integrable

  if r[3] = [] then
     if nargs = 5 then
        if lh = 'output' then
           if rh = 'unnormalized' then
              return [1, c*List:-FromListToPartialFraction(g)];
           end if;
           if rh = 'normalized' then
             #  return [1, normal(c*List:-FromListToPartialFraction(g))];
              return [1, c*List:-FromListToNormal(g)];
           end if;
        end if;
     end if;
     return [1, List:-ScalarMultiply(c, g)]:
  end if:

  # Prepare for item-by-item search

  l := nops(r[3]):
  S := table([]): 
  for i from 1 to l do
      S[i] := Sp[i][2]:
  end do:

  # initialize

  ord := 0:
  G := table([]): R := Table([]):  Cp := table([]):
  if nargs = 4 then
     G[ord] := g:
  end if;
  if nargs = 5 then
       if lh = 'output' and rh = 'unnormalized' then
          G[ord] := List:-FromListToPartialFraction(g);
       end if;
       if lh = 'output' and (rh = 'normalized' or rh = 'parfrac') then
          G[ord] := g;
       end if;
 end if;  

  R[ord] := r[3]:  
  L := GetCoeffs(R[ord], y):
  P := [L[1]]: Cp[ord] := L[2]; 

  sol := {}:

  # Item-by-item search

  ts := 0: tl := 0: tu := 0; tm := 0;
  while sol = {} do

    # Simple reduction

    tt := time():
    L := SimpleReduce(R[ord], S, x, y):
    (U, V) := L[1], L[2];
    lu := nops(U);
    tt := time() - tt:
    ts := ts + tt:

    # update G
 
    ord := ord + 1:
    #print(current_order): print(ord):
    tt := time(); 
    if nargs = 4 then
       G[ord] := NewIntegrablePart(G[ord-1], L[1], x):
    end if;

    if nargs = 5 then 
       if lh = 'output' and rh  = 'unnormalized' then
          gd := diff(G[ord-1], x);
          g2 :=  add(U[i][2][1]*U[i][2][2]/U[i][1], i=1..lu);
          G[ord] := gd+g2;
       end if;
       if lh = 'output' and (rh = 'normalized' or rh = 'parfrac') then
          G[ord] := NewIntegrablePart(G[ord-1], U, x):
       end if;
    end if;

 
    tu := tu + time() -tt;   
    R[ord] := V;

    # Form matrix
    
    tt := time();
    L := GetCoeffs(R[ord], y):
    P := [op(P), L[1]]:
    Cp[ord] := L[2]:
    M := Matrix(P):
    tm := tm + time()-tt;


    # Solve linear system

    tt := time():
    sol := FindLinearRelation(M, ord+1):
    tt := time() - tt:
    tl := tl + tt:
  end do:

  ##print(time_for_simple_reduction): print(ts):
  ##print(time_for_newG): print(tu):
  ##print(time_for_matrix): print(tm);
  ##print(time_for_linear_relation): print(tl):

  # Prepare for output:
    
    Q := table([]):
    for i from 0 to ord do
        Q[i] := normal(sol[1][i+1]/Cp[i]):
    end do: 

    ##tt := time():    
    T := GenerateTelescoper(ord, c, Q, x, Dx):
    T := primpart(T, Dx, 'ct'):
    ##tt := time() - tt:
    ##print(time_for_telescoper): print(tt):

    ##tt := time():

    if nargs = 5 then 
       if lh = 'output' and rh = 'normalized' then
          CertL := GenerateCertificate(ord, ct, Q, G);
          Cert := List:-FromListToNormal(CertL);
       end if;
       if lh ='output' and rh ='unnormalized' then
         g1 := 0;
         for i from 0 to ord do
             g1 := g1 + Q[i]*G[i];
         end do; 
         Cert := g1/ct; 
       end if;
       if lh = 'output' and rh = 'parfrac' then
          Cert := GenerateCertificate(ord, ct, Q, G);
       end if;
    end if; 

    if nargs = 4 then
       Cert := GenerateCertificate(ord, ct, Q, G);
    end if;
     
    ##tt := time() - tt:
    ##print(time_for_certificate):  print(tt):
    #[T, Cert, [seq(Q[i], i=0..ord)], [seq(G[i], i=0..ord)], ct];
    [T, Cert];
end proc:  


###############################################################
# Name: GeddesLe
# Calling sequence
#         GeddesLe(f, x, y, Dx, opt)
# Input: f, a rational function in x and y
#        x, y, Dx, three names
# Output: [L, G]  where
#         L is the minimal telescoper of f, and
#         G is the cerfificate
# option: if opt is 'output' = 'normalized[, then G is normalized
#         if opt is not present or is 'output' = 'unnormalized'
#         then G is a sum of fractions. 
################################################################

GeddesLe := proc(f, x, y, Dx, opt)
   local fp, h, c, g, r, rp, m, Lt, Gt, i, de, cp, nu, zb, q, gp, L, A, tt;  

   # check input
   
   fp := normal(f);
   if fp = 0 then
      return [1, 1];
   end if;

   # Hermite reduction
   
   h := Reduction:-Original(f, y, 'output' = 'parfrac');
   (c, g, r) := h[1], h[2], h[3];
   if nargs = 4 or rhs(opt) = 'unnormalized' then
      g := c*List:-FromListToPartialFraction(g);
   else
      g := c*List:-FromListToNormal(g);
   end if;

   # initialization for loop  
   rp := r[3];
   m := nops(rp);
   if m = 0 then
      return [1, g];
   end if;
   Lt := table[];
   Gt := table[];
   A := OreTools:-SetOreRing(x, 'differential');

   # apply DEtools[Zeilberger] to each fraction
 
   tt := time();
   for i from 1 to m do
       (de, cp, nu) := rp[i][1], rp[i][2][1], rp[i][2][2];
       zb := DEtools[Zeilberger](c*cp*nu/de, x, y, Dx);
       Lt[i] := OreTools:-Converters:-FromPolyToOrePoly(zb[1], Dx);
       Gt[i] := zb[2];
   end do;
   ##tt := time()-tt;
   ##print(time_for_zb); print(tt);

   # compute the telescoper

   tt := time(); 
   L := OreTools:-LCM(seq(Lt[i], i=1..m), A);
   ##tt := time()-tt;
   ##print(time_for_LCM); print(tt);

   # compute the certificate
 
   gp := 0;
   for i from 1 to m do
       q := OreTools:-Quotient(L, Lt[i], A);
       if nargs = 4 or rhs(opt) = 'unnormalized' then
          gp := gp +  OreTools:-Apply(q, Gt[i], A);
       else
          gp := normal(gp + normal(OreTools:-Apply(q, Gt[i], A)));
       end if;
   end do;
   if nargs = 4 or rhs(opt) = 'unnormalized' then
      g := OreTools:-Apply(L, g, A) + gp;
   else
      g := normal(normal(OreTools:-Apply(L, g, A)) + gp);
   end if;
 
   [OreTools:-Converters:-FromOrePolyToPoly(L, Dx),  g];

end proc; 






#############################################
# VerifyHermiteList(L, G, f, x, y, Dx) 
# Description: verify the output of Hermite, 
# where G is in list representation.
#############################################

VerifyHermiteList :=proc(L, G, f, x, y, Dx)
       local d, i, u, v;
       d := degree(L, Dx);
       u := normal(coeff(L, Dx, 0)*f);
       for i from 1 to d do
           u := normal(u + coeff(L, Dx, i)*diff(f, x$i));
       end do; 
       v := normal(List:-FromListToPartialFraction(G));
       Testzero(u-diff(v, y));
 end proc:  

#############################################
# VerifyHermiteFunction(L, G, f, x, y, Dx) 
# Description: verify the output of Hermite, 
# where G is in usual rational function form.
#############################################

VerifyHermiteFunction := proc(L, g, f, x, y, Dx)
      local S, G, coes, v, apop, left, right; 
      S := L;
      G := g;
      coes := [coeffs(collect(S,Dx),Dx,'v')];
      v := [v];
      apop := map((x,y,z,D)->diff(y,[z$(degree(x,D))]),v,f,x,Dx);
      left := convert(zip((x,y)->x*y,coes,apop),`+`);
      right := diff(G,y);
      if Testzero(left-right) then true else false end if;
   end proc:

#############################################
# ModularVerifyFunction(L, G, p, f, x, y, Dx) 
# Description: verify the output of Hermite 
# modulo a given prime p, where G is in usual 
# rational function form.
#############################################

ModularVerifyFunction := proc(L, G, p, f, x, y, Dx)
        local i, df, dg, mf, mg, d, u, v;

       
        df := denom(f) mod p;
        dg := denom(G) mod p;
        if df = 0 and dg = 0 then
           error "numeric exception: division by zero";
        end if;
        mf := f mod p;
        mg := Normal(G) mod p; 
        d := degree(L, Dx);
        u := Normal(coeff(L, Dx, 0)*mf) mod p;
        for i from 1 to d do
            u := Normal(u + Normal(coeff(L, Dx,i)*Normal(diff(mf, x$i))mod p) mod p) mod p;
        end do; 
        v := Normal(diff(mg, y)) mod p;
        Testzero(Normal(u-v) mod p);
 end proc:
#############################################
# ModularVerifyList(L, G, p, f, x, y, Dx) 
# Description: verify the output of Hermite 
# modulo a given prime p, where G is in list 
# representation.
#############################################

ModularVerifyList := proc(L, G, p, f, x, y, Dx)
       local Gp;
       Gp := List:-FromListToNormal(G);
       ModularVerifyFunction(L, Gp, p, f, x, y, Dx);
end proc:

            
end module:


