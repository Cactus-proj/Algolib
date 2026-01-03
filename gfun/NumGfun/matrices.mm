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

## matrices.mm -- Matrix data structures and multiplication for binary splitting.
## Marc Mezzarobba, Algorithms project, INRIA Rocquencourt


# FIXME: rewrite genmatring using Maple modules

matrices := module()

uses LinearAlgebra;

global `type/matrix_ring`, `type/ndmatrix`, `convert/ndmatrix`;

export
    # names
    LAMain, ReIm, bigint, mvMult, WaksmanInplace, waksman,
    # functions
    ndmatrix_multiply, ndmatrix_norm, genmatring, waksman_product;

`type/matrix_ring` := proc(x)
    type(x,specfunc(anything, matrix_ring));
end proc:

#`type/gfun/NumGfun/matrix_group` := proc(x)
#    type(x, '`module`'(id, pr));
#end proc:
#
#`type/gfun/NumGfun/matrix_ring` := proc(x)
#    type(x, `gfun/NumGfun/matrix_group`) and type(x, '`module`'(ze, ad);
#end proc:


####################################################################################################
## ndmatrix : num::Matrix(Z-algebra) / denom::Integer
####################################################################################################

`type/ndmatrix` := proc(x)
    type(x,specfunc(Or(Matrix,Vector,polynom(integer)),ndmatrix));
end proc:

`convert/ndmatrix` := proc(M, mode:="lcm")::ndmatrix;
    description "(anything convertible to) Matrix of integers/polynomials to ndmatrix";
    local denoms, den, x;
    if type(M, Matrix) then
        denoms := map(denom@rhs, rtable_elems(M));
        if mode = "lcm" then
            # should work for polynomials as well as integers
            den := lcm(op(denoms));
        elif mode = "mul" or mode = "multiply" then
            den := mul(x, x in denoms);
        else
            error("Unrecognised mode");
        end if;
        return(ndmatrix(den*M, den));
    else
        convert(convert(M, Matrix), ndmatrix)
    end if;
end proc:

# Fait en partie double emploi avec genmatring/ndmatrix, mais c'est plus pratique comme ça pour
# l'instant.
ndmatrix_multiply := proc(a, b)::ndmatrix;
    option inline;
    ndmatrix(
        LinearAlgebra:-Multiply(op(1,a),op(1,b)), # LA_Main=> different calling seqs
        op(2,a) * op(2,b));
end proc:

ndmatrix_norm := proc(a)::float;
    Norm(nthterm:-makeitfloat(a,Digits),'Frobenius');
end proc:

genmatring[ndmatrix] := proc(r)::matrix_ring;
    local ad, pr, ze, id;
    pr := proc(U,V)
        option inline;
        ndmatrix(
            mvMultiply(op(1,U),op(1,V)),
            op(2,U) * op(2,V));
    end proc:
    id := ndmatrix(Matrix(r,r,'shape'='identity'),1);
    matrix_ring(ad, pr, ze, id)
end proc:

## Special shape

genmatring[ndseries] := proc(r)
    genmatring[mvMult](r+1); # OK tant qu'on ne se sert pas de l'addition
end proc:

####################################################################################################
## Generic (& integer) matrices
####################################################################################################

## Fast product (used by the above)

genmatring[mvMult] := proc(r)::matrix_ring;
    local ad, pr, ze, id;
    pr := proc(a,b)
        option inline;
        mvMultiply(a,b);
    end proc:
    id := Matrix(r,r,'shape'='identity');
    matrix_ring(ad, pr, ze, id)
end proc:

genmatring[bigint] := genmatring[mvMult];

## Misc.

genmatring[generic] := proc(r)::matrix_ring;
    local ad, pr, ze, id;
    ad := proc(a,b,alpha,beta)
        alpha*a + beta*b;
    end proc:
    pr := proc(a,b)
        a . b
    end proc:
    ze := Matrix(r,r,'shape'='zero');
    id := Matrix(r,r,'shape'='identity');
    matrix_ring(ad, pr, ze, id)
end proc:

genmatring[LAMain] := proc(r)::matrix_ring;
    local ad, pr, ze, id;
    ad := proc(a,b,alpha,beta)
        option inline;
        LA_Main:-MatrixAdd(a, b, alpha, beta, 'inplace'=false,
                'outputoptions'=[]);
        # + est encore deux fois moins efficace environ
    end proc:
    pr := proc(a,b)
        option inline;
        LA_Main:-MatrixMatrixMultiply(a, b, 'inplace'=false,
                'outputoptions'=[]);
    end proc:
    ze := Matrix(r,r,'shape'='zero');
    id := Matrix(r,r,'shape'='identity');
    matrix_ring(ad, pr, ze, id)
end proc:

genmatring[waksman] := proc(r)::matrix_ring;
    local wak, ad, pr, ze, id;
    ad := proc(a,b,alpha,beta)
        local c,i,j;
        c := rtable(1..r, 1..r, NULL, NULL, NULL, 'subtype' = 'Matrix');
        for i from 1 to r do
            for j from 1 to r do
                c[i,j] := alpha*a[i,j] + beta*b[i,j]
            end do;
        end do;
        c;
    end proc:
    wak := waksman_product(r);
    pr := proc(a,b)
        local c;
        c := rtable(1..r, 1..r, NULL, NULL, NULL, 'subtype' = 'Matrix');
        wak(a,b,c);
        c;  
        # j'ai essayé de détecter 0 et 1 avec MatrixOptions(...,shape) :  ça coûte plus que ça ne
        # rapporte
    end proc:
    ze := Matrix(r,r,'shape'='zero');
    id := Matrix(r,r,'shape'='identity');
    matrix_ring(ad, pr, ze, id)
end proc:

####################################################################################################
## imatrix: Real part, Imaginary part, denom
####################################################################################################

# - Inefficace en l'état
# - Réessayer avec mvMult et toutes les opérations en place, non paramétré (-> option inline) ?
# - En vrai on préfèrerait des ndmatrix(complex), mais il ne semble pas facile de changer la façon
#   dont Maple fait le produit sur les scalaires...

genmatring[ReIm] := proc(r)::matrix_ring;
    local 
        ad, pr, ze, id,
        int_ad, int_pr, int_ze, int_id;
    int_ad, int_pr, int_ze, int_id := op(`genmatring/bigint`(r));
    ad := proc()
        error "Nobody wants to add imatrices."
    end proc:
    pr := proc(U,V)
        local U0,U1,denU,V0,V1,denV,W0,W1,denW;
        U0, U1, denU := op(U);
        V0, V1, denV := op(V);
        W0 := int_ad(int_pr(U0,V0),int_pr(U1,V1),+1,-1);
        W1 := int_ad(int_pr(U0,V1),int_pr(U1,V0),+1,+1);
        denW := denU * denV;
        `imatrix`(W0,W1,denW);
    end proc:
    ze := imatrix(int_ze, int_ze, 1);
    id := imatrix(int_id, int_ze, 1);
    matrix_ring(ad, pr, ze, id)
end proc:

# Opérations (partiellement) en place...

genmatring[WaksmanInplace] := proc(r)::matrix_ring;
    local wak, ad, pr, ze, id;
    ad := proc(a,b,alpha,beta)
        local i,j;
        for i from 1 to r do
            for j from 1 to r do
                a[i,j] := alpha*a[i,j] + beta*b[i,j]
            end do;
        end do;
    end proc:
    wak := waksman_product(r);
    pr := proc(a,b)
        local c;
        #c := Matrix(r,r);
        c := rtable(1..r, 1..r, NULL, NULL, 'datatype' = 'integer', 'subtype' = 'Matrix');
        wak(a,b,c);
        c;  
        # j'ai essayé de détecter 0 et 1 avec MatrixOptions(...,shape) :  ça coûte plus que ça ne
        # rapporte
    end proc:
    ze := Matrix(r,r,'shape'='zero');
    id := Matrix(r,r,'shape'='identity');
    matrix_ring(ad, pr, ze, id)
end proc:

genmatring[ReImInplace] := proc(r)::matrix_ring;
    local 
        ad, pr, ze, id,
        int_ad, int_pr, int_ze, int_id;
    int_ad, int_pr, int_ze, int_id := op(genmatring[WaksmanInplace](r));
    ad := proc()
        error "Nobody wants to add imatrices."
    end proc:
    pr := proc(U,V)
        local U0,U1,denU,V0,V1,denV,T,W0,W1,denW;
        U0, U1, denU := op(U);
        V0, V1, denV := op(V);
        # This saves one (matrix) multiplication
        W0 := int_pr(U0,V0);
        T := int_pr(U1,V1);
        int_ad(U0,U1,1,1);
        int_ad(V0,V1,1,1);
        W1 := int_pr(U0,V0);
        int_ad(W1,W0,1,-1);
        int_ad(W1,T,1,-1);
        int_ad(W0,T,1,-1);
        denW := denU * denV;
        `imatrix`(W0,W1,denW);
    end proc:
    ze := imatrix(int_ze, int_ze, 1);
    id := imatrix(int_id, int_ze, 1);
    matrix_ring(ad, pr, ze, id)
end proc:

####################################################################################################
## Efficient multiplication of small matrices with large coefficients
####################################################################################################

## Waksman

# - en l'état, semble moins efficace que mvMultiply dans la majorité des cas
# - codegen[optimize] ne gagne rien sur la séquence que je produis (il se contente de faire étape
#   par étape les calculs que là je fais d'un coup) -- enfin, si, il gagne des calculs d'indice,
#   mais je ne suis pas sûr que ce soit vraiment ce que je veux [Matrix sans hachage ;
#   arrière-pensée vrai langage]
# - garder des tables/arrays ou passer les paramètres un à un ?
# - une limitation de codegen m'empêche d'avoir une ligne c=Matrix(n,n) dans le code généré : je
#   prends donc une matrice en argument pour mettre le résultat (peut-être à changer si je passe à
#   FromInert)
# - ne fonctionne pas sous Maple 9, pour une raison qui m'échappe
waksman_product := proc(n)::procedure;
    local i, j, k, t, u, v, m, a, b, A, B, s1, s2, s, fresh_name, temps, locals, half, c, tmp;
    # Computation sequence in codegen "list of equations" format
    m := iquo(n,2);
    half := proc(x) iquo(x,2) end proc:  # pour bien distinguer produits et divisions par 2
    # u = 2t-1, v = 2t
    s1 := [
        seq(seq(
            A[i,j,t] = (a[i,u] + b[v,j]) * (a[i,v] + b[u,j]),
            j = 1..n), i = 1..n),
        seq(
            B[1,j,t] = A[1,j,t] + (a[1,u] - b[v,j]) * (a[1,v] - b[u,j]),
            j = 1..n),
        seq(
            B[i,i,t] = A[i,i,t] + (a[i,u] - b[v,i]) * (a[i,v] - b[u,i]),
            i = 2..n),
        seq(seq(
            B[i,j,t] = B[i,i,t] - B[1,i,t] + B[1,j,t],
            j in [$1..i-1, $i+1..n]), i = 2..n) ];
    s2 := [
         seq(seq(
            c[i,j] =
                add( A[i,j,t] - half(B[i,j,t]), t = 1..m)
                + `if`(n mod 2 = 1, a[i,n] * b[n,j], 0),
            j = 1..n), i = 1..n) ];
    s := [ seq( op(eval(s1, {t=k, u=2*k-1, v=2*k})), k = 1..m ), op(s2) ];
    # Rename temps
    fresh_name := make_fresh_name_generator(`tmp`);
    temps := convert(remove(has, indets(s, name), {a,b,c}), list);
    locals := map(fresh_name, temps);
    codegen:-makeproc(
        subs({seq(temps[i]=locals[i], i=1..nops(temps))}, s),
        'parameters'=[a,b,c] )
end proc:

end module:
