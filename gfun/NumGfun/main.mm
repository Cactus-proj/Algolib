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

## NumGfun -- Recurrence unrolling by binary splitting; analytic continuation of
## D-finite (holonomic) functions; bounds for holonomic sequences.
## Copyright 2008, 2009, 2010, 2011, 2012 Inria.
## Written by Marc Mezzarobba.

# NumGfun is normally distributed and intended to be used as a submodule of
# gfun.  See
# http://algo.inria.fr/libraries/
# http://algo.inria.fr/libraries/papers/gfun.html

# REFERENCES
# ----------
#
# See also ?NumGfun.
#
# [Mez10] Marc Mezzarobba.  NumGfun: a Package for Numerical and Analytic
# Computation with D-finite Functions.  In Wolfram Koepf (editor), ISSAC 2010,
# pages 139-146.  DOI:10.1145/1837934.1837965.  [Short presentation of NumGfun
# focusing on the numerical evaluation features.]
#
# [Mez11] Marc Mezzarobba. Autour de l'évaluation numérique des fonctions
# D-finies. Thèse de doctorat, École polytechnique, 2011.  [Includes a more
# in-depth presentation, many examples, as well as a detailed discussion of
# most relevant algorithms.  In French.]
#
# [MezSal2010] Marc Mezzarobba and Bruno Salvy.  Effective bounds for
# P-recursive sequences. Journal of Symbolic Computation 45(10):1075-1096.
# DOI:10.1016/j.jsc.2010.06.024.  [Discusses the algorithms behind
# bound_rec and bound_diffeq.  If you can read French, the description in the
# thesis above is more complete.]

# USERINFO LEVELS
# ---------------
#
# 1-5 = normal info levels (see gfun doc),
# 6-10 = verbose debug info

NumGfun := module()

description
    "Binary splitting, analytic continuation of holonomic functions, "
    "and bounds";

option package;

export
    fnth_term,

    analytic_continuation,
    evaldiffeq,
    diffeqtoproc, # submodule with ModuleApply
    transition_matrix,

    bound_diffeq,
    bound_rec,
    bound_ratpoly, # submodule with ModuleApply
    bound_diffeq_tail,
    bound_rec_tail,

    local_basis,

### undocumented functions

    dominant_root, # submodule with ModuleApply
    needed_terms,

### hidden exports

    utilities,
    _pexports,
    version,
    Settings # submodule

;


local
    numeric_mode, # see bounds.mm, utilities.mm
    NUMGFUN_HIDDEN,

### submodules with ModuleApply

    bound_normal_diffeq,

### submodules without ModuleApply

    types,
    matrices,
    nthterm,
    ancont,
    bounds,
    numeric_bounds,
    symbolic_bounds,
    regsing,
    recasympt,
    numdenmatrix,  # Actually matrices:-numdenmatrix. Also used as a type name.

### type names local to NumGfun

    # (thanks to TypeTools, these do not pollute the global environment, and can
    # even be used as NumGfun:-typename from the global scope if
    # opaquemodules=false)
    hrdeq,
    hrrec,
    path,
    generalized_rec_matrix,
    regsing_params
;

# Must appear first.  (utilities.mm starts by global/local lines that make
# the exports of the NumGfun:-utilities submodule available from the NumGfun:-
# namespace, to work around severe limitations of the 'use' statement.  This is
# also needed for procedures with 'option inline' to actually get inlined)
$include <utilities.mm>
$include <settings.mm>
$include <matrices.mm>
$include <nthterm.mm>
$include <regsing.mm>
$include <dominant_root.mm>
$include <bound_ratpoly.mm>
$include <bound_normal_diffeq.mm>
$include <bounds.mm>
$include <numeric_bounds.mm>
$include <symbolic_bounds.mm>
$include <ancont.mm>
$include <diffeqtoproc.mm>
$include <recasympt.mm>
# should come last, so that other submodules can define types
$include <types.mm>

# exports that are defined inside submodules
analytic_continuation   := ancont:-analytic_continuation;
evaldiffeq              := ancont:-analytic_continuation: # synonym
transition_matrix       := ancont:-transition_matrix;
bound_rec               := symbolic_bounds:-bound_rec;
bound_diffeq            := symbolic_bounds:-bound_diffeq;
bound_diffeq_tail       := symbolic_bounds:-bound_diffeq_tail;
bound_rec_tail          := symbolic_bounds:-bound_rec_tail;
fnth_term               := nthterm:-fnth_term;
needed_terms            := numeric_bounds:-needed_terms;
local_basis             := regsing:-local_basis_expansions;

# locals defined in submodules
numdenmatrix := matrices:-numdenmatrix;

# local to gfun, since 'option load' seems to be ignored in submodules
NUMGFUN_SETUP := proc($) types:-setup() end proc:
NUMGFUN_CLEANUP := proc($) try types:-cleanup() catch: end end proc:

_pexports:=proc() [op({exports(thismodule)} minus NUMGFUN_HIDDEN)] end:
NUMGFUN_HIDDEN:={
    ':-_pexports',
    ':-version',
    ':-Settings',
    ':-utilities'
}:

version := "0.6";

end module:

