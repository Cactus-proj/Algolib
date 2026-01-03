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

# FUNCTIONALITY:
#        qbinomial symbol
#
#        qbinomial(q,n,m)=qfactorial(q,n)
#                /(qfactorial(m)*qfactorial(n-m))
#
# INPUT:
#        q,n,m        algebraic
#
# OUTPUT:
#        unevaluated, unless n and m are non negative integers, and for
#        some simple cases
#
# ERROR:
#        qbinomial is not defined for negative arguments
#
qbinomial:=proc(q,n,m)
    option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
    global negint,nonnegint;
    if type(n,'negint') or type(m,'negint') then
        error "qbinomial not defined for negative arguments"
    elif type(n,'nonnegint') or type(m,'nonnegint') then
        `qbinomial/qbinomialtoqpochhammer`('qbinomial'(q,n,m))
    else
        'qbinomial'(q,n,m)
    end if
end proc:

# FUNCTIONALITY:
#        expand qbinomial symbols of the form qbinomial(q,n+i,m+j)
#        where i and j are integers
#
# INPUT:
#        q,n,m        algebraic
#
# OUTPUT:
#        product form (i.e., not fully expanded one) corresponding to
#        the input, i.e., qbinomial(q,n,m) times a rational fractions
#        in n, m and q
#
`expand/qbinomial`:=proc(q,n,m)
    local numerical_part,i,new_n,new_m,n_factor,m_factor;
    option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
    global numeric;
    numerical_part:=`if`(type(n,`+`),trunc(select(type,n,'numeric')),0);
    new_n:=n-numerical_part;
    n_factor:=(q^m)^numerical_part*
        `if`(numerical_part>0,
            mul((1-q^i*q^(new_n))/(q^m-q^i*q^(new_n)),i=1..numerical_part),
        `if`(numerical_part<0,
            mul((q^m-q^i*q^(new_n))/(1-q^i*q^(new_n)),i=numerical_part+1..0),
            1));
    numerical_part:=`if`(type(m,`+`),trunc(select(type,m,'numeric')),0);
    new_m:=m-numerical_part;
    m_factor:=1/(q^new_m)^numerical_part*
        `if`(numerical_part>0,
            mul((q^new_m-1/q^i*q^new_n)/(1-q^(i+1)*q^new_m),
                i=0..numerical_part-1),
        `if`(numerical_part<0,
            mul((1-q^i*q^new_m)/(q^new_m-q^(1-i)*q^new_n),
                i=numerical_part+1..0),
            1));
    n_factor*m_factor*qbinomial(q,new_n,new_m)
end proc:

# FUNCTIONALITY:
#        convert a qbinomial to a qpochhammer form using the formula
#
#        qbinomial(q,n,m)=qpochhammer(q,q,n)
#                /(qpochhammer(q,n)*qpochhammer(q,n-m))
#
# INPUT:
#        x        an expression of the form
#                qbinomial(q,n,m)
#
# OUTPUT:
#        the corresponding rational fraction in qpochhammer symbols
#
# ASSUMPTION:
#        x must be a qbinomial symbol
#
`qbinomial/qbinomialtoqpochhammer`:=proc(x)
    local q,n,m;
    option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
    global qbinomial,algebraic;
    ASSERT(type(x,'qbinomial'('algebraic'$3)),"not a qbinomial");
    q:=op(1,x);
    n:=op(2,x);
    m:=op(3,x);
    qpochhammer(q,q,n)/qpochhammer(q,q,m)/qpochhammer(q,q,n-m)
end proc:

#savelib('qbinomial','`expand/qbinomial`','`qbinomial/qbinomialtoqpochhammer`');
