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

# table("tag"=f,"var"=[x,y,z],"algebra"=Alg,"gbasis"=GB,
#        "term_order"=TOrd,"type"=DFinite)
# denotes any d-finite solution of GBasis, tagged by f.
#
# This structure may appear as df in the following composites.
#
# IMPORTANT: GB has to be a Groebner basis with respect to TOrd!

# Create a dfinite structure.
dfinite_create:=proc(f,GB,TOrd)
    local `d-finite`;
    option `Copyright (c) 1997-2002 Frederic Chyzak, INRIA, France`,remember;
    global tag,var,term_order,type,DFinite,
        list;
    `d-finite`["type"]:='DFinite';
    `d-finite`["tag"]:=f;
    `d-finite`["var"]:=convert(TOrd["algebra"]["left_indets"],'list');
    `d-finite`["algebra"]:=TOrd["algebra"];
    `d-finite`["gbasis"]:=GB;
    `d-finite`["term_order"]:=TOrd;
    `d-finite`
end proc:
