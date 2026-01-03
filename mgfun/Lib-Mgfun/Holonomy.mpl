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

# Were inside Groebner until Maple10; moved here for Maple11.
$include <hilbert_dimension_do_it.mpl>
# Should be integrated into fglm.mm.
$include <fglm_algorithm.mpl>
$include <fglm_termination_proc.mpl>

unprotect('Holonomy');

Holonomy:=module()
    export HO_Internals,algeq_to_dfinite,holon_closure,holon_defint,holon_defqsum,holon_defsum,holon_diagonal,dfinite_add,dfinite_mul,hypergeom_to_dfinite,takayama_algo;
    option `Copyright (c) 2000-2008 Frederic Chyzak, INRIA, France`,package;
    local `Holonomy/my_coeffs_rec`, `Holonomy/my_coeffs`, `Holonomy/normal_form`, `Holonomy/find_dependency`, `Holonomy/termination`, `Holonomy/fglm`, `Holonomy/takayama`, `Holonomy/homogeneous`, `Holonomy/pretreat`;

    HO_Internals:=module()
        export algebraic_to_dfinite,algeq_to_dfinite,dfinite_add,fglm,dfinite_addup,dfinite_create,dfinite_mul,dfinite_multiply,diagonal,extension_contraction,holon_closure,holon_defint,holon_defqsum,holon_defsum,holon_diagonal,hypergeom_to_dfinite,takayama,hypergeometric_to_dfinite,takayama_algorithm,usual_term_orders,suitable_extension,yet_another_gauss;
        option `Copyright (c) 2000-2008 Frederic Chyzak, INRIA, France`;

$include <ApplyOpr_AlgSubs.mi>
$include <compilation.mi>

$include <Holonomy/algebraic_to_dfinite.mm>
$include <Holonomy/algeq_to_dfinite.mm>
$include <Holonomy/dfinite_add.mm>
$include <Holonomy/fglm.mm>
$include <Holonomy/dfinite_addup.mm>
$include <Holonomy/dfinite_create.mm>
$include <Holonomy/dfinite_mul.mm>
$include <Holonomy/dfinite_multiply.mm>
$include <Holonomy/diagonal.mm>
$include <Holonomy/extension_contraction.mm>
$include <Holonomy/holon_closure.mm>
$include <Holonomy/holon_defint.mm>
$include <Holonomy/holon_defqsum.mm>
$include <Holonomy/holon_defsum.mm>
$include <Holonomy/holon_diagonal.mm>
$include <Holonomy/hypergeom_to_dfinite.mm>
$include <Holonomy/takayama.mm>
$include <Holonomy/hypergeometric_to_dfinite.mm>
$include <Holonomy/takayama_algorithm.mm>
$include <Holonomy/usual_term_orders.mm>
$include <Holonomy/suitable_extension.mm>
$include <Holonomy/yet_another_gauss.mm>

        fglm:=eval(`Holonomy/fglm`);
        takayama:=eval(`Holonomy/takayama`);

    end module;

    algeq_to_dfinite:=HO_Internals:-algeq_to_dfinite;
    holon_closure:=HO_Internals:-holon_closure;
    holon_defint:=HO_Internals:-holon_defint;
    holon_defqsum:=HO_Internals:-holon_defqsum;
    holon_defsum:=HO_Internals:-holon_defsum;
    holon_diagonal:=HO_Internals:-holon_diagonal;
    dfinite_add:=HO_Internals:-dfinite_add;
    dfinite_mul:=HO_Internals:-dfinite_mul;
    hypergeom_to_dfinite:=HO_Internals:-hypergeom_to_dfinite;
    takayama_algo:=HO_Internals:-takayama;

end module:

protect(Holonomy);

`Holonomy/version`:=3.4:

#savelib('`Holonomy/version`','`Holonomy/hilbert_dimension_rec`','`Holonomy/hilbert_dimension_do_it`','`Holonomy/next_term`','`Holonomy/fglm_algorithm`','`Holonomy/fglm_termination_proc`','Holonomy');
