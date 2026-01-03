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
#        compute the sum of a list of holonomic functions
#
# INPUT:
#        descr_list        a list of lists of the form [G,T], one for
#                        each function to be summed, where G is the
#                        Groebner basis with respect to T of the
#                        annihilating ideal of the corresponding
#                        function
#        TOrd                term order
#
# OUTPUT:
#        a Groebner basis with respect to TOrd for an ideal that
#        defines the sum
#
# WEAKNESS:
#        all ideals have to be zero dimensional---this is not tested
#
# TYPES:
#        all term orders must be on the same algebra
#
# ALGORITHM:
#        reduce sufficiently many derivatives of the sum to be computed
#        and find dependencies
#
# NOTE:
#        when the term order is tdeg or plex, an elimination algorithm
#        is used
#
dfinite_add:=proc(descr_list::list(DFiniteDescr),TOrd::MonomialOrder)
    local i,t,C,K,A,T,new_t;
    option `Copyright (c) 1995-2008 Frederic Chyzak, INRIA, France`;
    if nops(convert(map(proc(d)
            option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
            d[2]["algebra"]
        end,descr_list),'set'))<>1 then
        error "all term orders must be on the same algebra"
    end if;
# Ideally, this code using dfinite_addup should
# disappear.  The point is that I'm not able to get a term order when
# TOrd already is a lexdeg (i.e., I would need something like
# [t[1],t[2],t[3]] >> [x,y,z] >> [u,v,w]).
if not member(TOrd["hack"],['tdeg','plex']) then
    userinfo(1,'hsum',"sum by FGLM algorithm");
    dfinite_addup(map(proc(t)
            option `Copyright (c) 1995-2002 Frederic Chyzak, INRIA, France`;
            [expand(t[1]),op(2..3,t)]
        end,descr_list),TOrd)
else
    userinfo(1,'hsum',"sum by computing intersection of ideals");
    # Extend the algebra by the following new indeterminates.
    new_t:=seq(t[i],i=1..nops(descr_list)-1);
    A:=TOrd["algebra"];
    C:=A["commutation"];
    K:=C["ground_ring"];
    K:=Ore_algebra:-OA_Internals:-ground_ring(K["characteristic"],
        K["all_indets"] union {new_t},K["type_struct"]);
    C:=Ore_algebra:-OA_Internals:-commutation(K,
        C["left_indets"],C["right_indets"],A["type_struct"],
        `if`(assigned(A["localization"]),A["localization"],"undefined"));
    A:=Ore_algebra:-OA_Internals:-algebra(C,
        A["rational_indets"],{},{});
    T:=Groebner:-MonomialOrder(A,`if`(TOrd["hack"]='tdeg',
        'lexdeg'([new_t],TOrd["order_indets"]),
        'plex'(new_t,op(TOrd["order_indets"]))));
    remove(has,Groebner:-Basis([
        seq(op(expand(t[i]*op([i,1],descr_list))),
            i=1..nops(descr_list)-1),
        op(expand((1-convert([new_t],`+`))*op([-1,1],descr_list)))
    ],T),[new_t])
end if
end proc:
