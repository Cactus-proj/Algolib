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
#        qpochhammer symbol
#
#        qpochhammer(q,n,x)=(1-x)(1-qx)...(1-q^(n-1)x)
#
# INPUT:
#        q,n,x        algebraic
#
# OUTPUT:
#        unevaluated, unless n is an integer
#
qpochhammer:=proc(q,n,x)
    local k;
    option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
    global nonnegint,negint;
    if type(n,'nonnegint') then
        mul(1-x*q^k,k=0..n-1)
    elif type(n,'negint') then
        1/mul(1-x/q^k,k=1..-n)
    else
        'qpochhammer'(q,n,x)
    end if
end proc:

# FUNCTIONALITY:
#        expand qpochhammer symbols of the form
#        qpochhammer(q,n+j,q^i*x) where i and j are integers
#
# INPUT:
#        q,n,x        algebraic
#
# OUTPUT:
#        product form (i.e., not fully expanded one) corresponding to
#        the input, i.e., qpochhammer(q,n,x) times a rational fractions
#        in x, n and q
#
`expand/qpochhammer`:=proc(q,n,x)
    local numerical_part,i,res;
    option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
    global identical,numeric;
    numerical_part:=`if`(type(x,`*`),
        select(type,x,{'identical'(q),'identical'(q)^'numeric'}),1);
    if numerical_part=q then
        res:=qpochhammer(q,n,x/q)*(1-q^n*x/q)/(1-x/q)
    elif type(numerical_part,'identical'(q)^'numeric') then
        numerical_part:=op(2,numerical_part);
        res:=expand(qpochhammer(q,n+numerical_part,x/q^numerical_part))
            /qpochhammer(q,numerical_part,x/q^numerical_part)
    else
        numerical_part:=`if`(type(n,`+`),
            floor(select(type,n,'numeric')),0);
        if numerical_part>0 then
            res:=qpochhammer(q,n-numerical_part,x)
                *mul(1-x*q^i*q^(n-numerical_part),i=0..numerical_part-1)
        elif numerical_part<0 then
            res:=qpochhammer(q,n-numerical_part,x)
                /mul(1-x*q^i*q^(n-numerical_part),i=numerical_part..-1)
        else
            res:=qpochhammer(q,n,x)
        end if
    end if;
    # q is assumed to be of module less than 1.
    eval(subs(q^infinity=0,res))
end proc:

# FUNCTIONALITY:
#        convert an expression involving qfactorial and qbinomial
#        symbols to a form with qpochhammer symbols only
#
# INPUT:
#        x        an expression
#
# OUTPUT:
#        its form with qpochhammer only
#
`convert/qpochhammer`:=proc(x)
    local qfactorial_list,qbinomial_list;
    option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
    global qfactorial,qbinomial,algebraic;
    # Convert qfactorial if there can be any in the input.
    qfactorial_list:=`if`(assigned(qfactorial),
        map(b->b=`qfactorial/qfactorialtoqpochhammer`(b),
            indets(x,'qfactorial'('algebraic'$2))),{});
    # Convert qbinomial if there can be any in the input.
    qbinomial_list:=`if`(assigned(qbinomial),
        map(b->b=`qbinomial/qbinomialtoqpochhammer`(b),
            indets(x,'qbinomial'('algebraic'$2))),{});
    subs(qfactorial_list union qbinomial_list,x)
end proc:

#savelib('qpochhammer','`expand/qpochhammer`','`convert/qpochhammer`');
