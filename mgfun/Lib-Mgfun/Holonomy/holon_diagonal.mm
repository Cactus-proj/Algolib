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

# User entry point to compute the diagonal of a holonomic function.
holon_diagonal:=proc(G::{list,set}(polynom),
        x_list::{list,set}(name),s::name,
        TOrd_rat::MonomialOrder,
        TOrd_poly::MonomialOrder,
        TOrd_elim::MonomialOrder)
    local Alg_rat,Alg_poly,Alg_elim;
    option `Copyright (c) 1994-2005 Frederic Chyzak, INRIA, France`;
    Alg_rat:=TOrd_rat["algebra"];
    Alg_poly:=TOrd_poly["algebra"];
    Alg_elim:=TOrd_elim["algebra"];
############### Verifier que ce sont toutes trois des algebres
############### differentielles liees l'une a l'autre.
    if VERIFICATION_ALGEBRAS(Alg_rat,Alg_poly,Alg_elim)=false then
        error "term orders not on suitable algebras"
    end if;
    if nops(x_list)<>nops(convert(x_list,'set')) or nops(x_list)<2
    or not member(s,x_list) then
        error "cannot compute a diagonal with respect to these indeterminates"
    end if;
    diagonal(expand(G),[op(subs(s=NULL,x_list)),s],
        TOrd_rat,TOrd_poly,TOrd_elim)
end proc:
