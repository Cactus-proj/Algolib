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

#
# tentative de réécriture en cours
# · comprendre comment gérer les dépendances en la taille
#

use TypeTools in

    AddType(
        'matrix_group',
        proc(x)
            type(x, '`module`'(Identity, Multiply));
        end proc
    );

    AddType(
        'matrix_ring',
        proc(x)
            type(x, 'matrix_group') and type(x, '`module`'(Zero, Add);
        end proc
    );

end use;

####################################################################################################
## numdenmatrix : num::Matrix(Z-algebra) / denom::Integer
####################################################################################################

`convert/NumGfun/numdenmatrix` := proc(x)
    # FIXME
end proc;

numdenmatrix := module()

    TypeCheck := proc(x)
        type(x,specfunc(Or(Matrix,Vector),'ndmatrix'))
    end proc:
    
    Multiply := proc(U, V)
        option inline;
        'numdenmatrix'(
            mvMultiply(op(1,U),op(1,V)),
            op(2,U) * op(2,V));
    end proc:
    
    Identity := 'numdenmatrix'(Matrix(r,r,'shape'='identity'),1);

    Norm := proc(a::numddenmatrix)::float;
        Norm(makeitfloat(a,Digits+2),2);  # evalf mieux ?
    end proc:
    
end module:

####################################################################################################
##
####################################################################################################

mvMult := module()  ## matrices usuelles entières, sans dénom

    Multiply := proc(a,b)
        option inline;
        mvMultiply(a,b);
    end proc:

    Identity := Matrix(r,r,'shape'='identity');
    
end module:

numdenseries := module ()

    Multiply := eval(mvMult:-Multiply);

    Identity := eval(mvMult:-Identity);

end module:

genericmatrix := module(r)
    Add := proc(a,b,alpha,beta)
        alpha*a + beta*b;
    end proc:
    Multiply := proc(a,b)
        a . b
    end proc:
    Zero := Matrix(r,r,'shape'='zero');
    Identity := Matrix(r,r,'shape'='identity');
end proc:


