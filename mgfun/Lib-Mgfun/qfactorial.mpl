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

# FUNCTIONALITY:
#        qfactorial symbol
#
#        qfactorial(q,n)=(1-q)(1-q^2)...(1-q^n)
#
# INPUT:
#        q,n        algebraic
#
# OUTPUT:
#        unevaluated, unless n is an integer
#
# ERROR:
#        qfactorial is not defined for negative arguments
#
qfactorial:=proc(q,n)
    local k;
    option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
    global nonnegint,negint;
    if type(n,'nonnegint') then
        mul(1-q^k,k=1..n)
    elif type(n,'negint') then
        error "qfactorial not defined for negative arguments"
    else
        'qfactorial'(q,n)
    end if
end proc:

# FUNCTIONALITY:
#        expand qfactorial symbols of the form qfactorial(q,n+i)
#        where i is an integer
#
# INPUT:
#        q,n        algebraic
#
# OUTPUT:
#        product form (i.e., not fully expanded one) corresponding to
#        the input, i.e., qfactorial(q,n) times a rational fractions
#        in n and q
#
`expand/qfactorial`:=proc(q,n)
    local numerical_part,i;
    option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
    global numerical;
    if type(n,`+`) then
        numerical_part:=floor(select(type,n,'numeric'));
        if numerical_part>0 then
            qfactorial(q,n-numerical_part)
                *mul(1-q^i*q^(n-numerical_part),i=1..numerical_part)
        elif numerical_part<0 then
            qfactorial(q,n-numerical_part)
                /mul(1-q^i*q^(n-numerical_part),i=numerical_part+1..0)
        else
            'qfactorial'(q,n)
        end if
    else
        'qfactorial'(q,n)
    end if
end proc:

# FUNCTIONALITY:
#        convert a qfactorial to a qpochhammer form using the formula
#
#        qfactorial(q,n)=qpochhammer(q,q,n)
#
# INPUT:
#        x        an expression of the form qfactorial(q,n)
#
# OUTPUT:
#        the corresponding rational fraction in qpochhammer symbols
#
# ASSUMPTION:
#        x must be a qfactorial symbol
#
`qfactorial/qfactorialtoqpochhammer`:=proc(x)
    option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
    global qfactorial,algebraic;
    ASSERT(type(x,'qfactorial'('algebraic'$2)),"not a qfactorial");
    'qpochhammer'(op(x),op(1,x))
end proc:

#savelib('qfactorial','`expand/qfactorial`','`qfactorial/qfactorialtoqpochhammer`');
