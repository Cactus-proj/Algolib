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

suitable_extension:=proc(sub_Alg,Alg,x_list)
    local x_set,sub_Comm,Comm,sub_GR,GR,x;
    option `Copyright (c) 1997-2002 Frederic Chyzak, INRIA, France`;
    global commutation,set;
    x_set:=convert(x_list,'set');
    sub_Comm:=sub_Alg["commutation"];
    Comm:=Alg["commutation"];
    sub_GR:=sub_Comm["ground_ring"];
    GR:=Comm["ground_ring"];
    sub_GR["characteristic"]=GR["characteristic"] and
        sub_GR["all_indets"]=GR["all_indets"] union x_set and
        sub_GR["type_struct"]=GR["type_struct"] and
        sub_Alg["non_alg_poly_indets"] minus x_set=
            Alg["non_alg_poly_indets"]
                minus {seq(Alg["right_of_left",x],x=x_list)} and
        sub_Alg["left_indets"] union x_set=Alg["left_indets"] and
        sub_Alg["right_indets"]=Alg["right_indets"]
            minus {seq(Alg["right_of_left",x],x=x_list)} and
        {seq(evalb(sub_Alg["type_of_left",x]=Alg["type_of_left",x]),
            x=sub_Alg["left_indets"])}={true} and
        # First approximation:
        #        o Alg could be localized in the extra indeterminates
        #          x_list;
        #        o a localization of Alg could refine that of sub_Alg.
        (not assigned(sub_Alg["localization"])
            or not assigned(Alg["localization"])
            or sub_Alg["localization"]=Alg["localization"])
end proc:
