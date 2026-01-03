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

holon_defsum:=proc(poly_list::{list,set}(polynom),
        Alg::OreAlgebra,x_list::{list,set}(name),TOrd::MonomialOrder)
    local sub_Alg,not_decl_x,x;
    option `Copyright (c) 1996-2005 Frederic Chyzak, INRIA, France`;
    if member(false,map(type,convert(poly_list,'set'),
        'SkewPolynomial'(Alg)))
    then
        error cat("polynomials not in the algebra ",Alg)
    end if;
    sub_Alg:=TOrd["algebra"];
    if not suitable_extension(sub_Alg,Alg,x_list)
    then
        error "term order not over the correct subalgebra"
    end if;
    not_decl_x:=convert(x_list,'set') minus sub_Alg["comm_indets"];
    if not_decl_x<>{} then
        error cat(not_decl_x," not declared in the algebra")
    end if;
    if {seq(Alg["type_of_left",x],x=x_list)}<>{'shift'} then
        error cat(x_list," not all associated to shift operators")
    end if;
    takayama_algorithm(
        # Conversion for the case of a set of input polynomials.
        convert(expand(poly_list),'list'),
        Alg,
        # Conversion for the case of a set of input variables.
        convert(x_list,'list'),
        TOrd,
        [[],[],0,proc(elim_GB,GB,N) elim_GB=[] end])[1]
end proc:
