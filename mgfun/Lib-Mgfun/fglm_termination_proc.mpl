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

# Procedures that decide the termination of algorithms based on the
# FGLM algorithm.
# fglm_termination_proc[foo] would not
# work.

`Holonomy/fglm_termination_proc`:=proc()
    option `Copyright (c) 2000-2008 Frederic Chyzak, INRIA, France`;
end proc:

`Holonomy/fglm_termination_proc`("zero_dimensional_case"):=
proc(border,monoideal,TOrd)
    option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
    # Test whether a zero-dimensional ideal has been obtained.  This
    # is the case when no more terms don't trivially reduce to zero
    # using the already found relations.
    border<>[]
end proc:

`Holonomy/fglm_termination_proc`("holonomic_case"):=
proc(border,monoideal,TOrd)
    local dim;
    option `Copyright (c) 1996-2008 Frederic Chyzak, INRIA, France`;
    global set,fglm;
    # Test whether the ideal obtained has the number of
    # pseudo-differential indeterminates of the algebra as its
    # dimension.  This procedure should be used as is only to compute
    # holonomic ideals in the case of Weyl algebras.
    dim:=`Holonomy/hilbert_dimension_do_it`(
        convert(monoideal,'set'),TOrd["order_indets"]);
    userinfo(4,'fglm',cat("current dimension is ",dim));
    dim>nops(TOrd["algebra"]["right_indets"])
end proc:

`Holonomy/fglm_termination_proc`("single_equation_case"):=
proc(border,monoideal,TOrd)
    option `Copyright (c) 1997-2002 Frederic Chyzak, INRIA, France`;
    # Test whether an equation has been obtained.
    monoideal=[]
end proc:
