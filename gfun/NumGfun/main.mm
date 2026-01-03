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

## NumGfun -- Recurrence unrolling by binary splitting; analytic continuation of
## holonomic functions; bounds for holonomic sequences.
## Marc Mezzarobba, projet Algorithms, INRIA Paris-Rocquencourt

# userinfo levels:
# 1-5 = normal info levels (cf. gfun doc),
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

### undocumented functions

    abs_with_RootOf,
    dominant_root, # submodule with ModuleApply
    make_waksman_proc,

### hidden exports

    _pexports,
    version,
    Settings # submodule

;


# the name 'ndmatrix' is used by 'inline' functions, which do not support
# lexical scoping(!)
# FIXME: change to NumGfun:-ndmatrix everywhere, and make local?
global ndmatrix;

local
    numeric_mode, # que faire de ça ?
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

### type names local to NumGfun

    # (thanks to TypeTools, these do not pollute the global environment, and can
    # even be used as NumGfun:-typename from the global scope if
    # opaquemodules=false)
    hrdeq,
    hrrec
;


# utilities.mm does NOT define a submodule (because of severe limitations of the
# 'use' construct) but it does contain its own global/local lines, so that it
# **must appear first**
$include <utilities.mm>

# submodules
$include <types.mm>
$include <settings.mm>
$include <matrices.mm>
$include <nthterm.mm>
$include <dominant_root.mm>
$include <bound_ratpoly.mm>
$include <bound_normal_diffeq.mm>
$include <bounds.mm>
$include <numeric_bounds.mm>
$include <symbolic_bounds.mm>
$include <ancont.mm>
$include <diffeqtoproc.mm>

# exports that are defined inside submodules
analytic_continuation   := ancont:-analytic_continuation;
evaldiffeq              := ancont:-analytic_continuation: # synonym
transition_matrix       := ancont:-transition_matrix;
bound_rec               := symbolic_bounds:-bound_rec;
bound_diffeq            := symbolic_bounds:-bound_diffeq;
bound_diffeq_tail       := symbolic_bounds:-bound_diffeq_tail;
bound_rec_tail          := symbolic_bounds:-bound_rec_tail;
fnth_term               := nthterm:-fnth_term;
make_waksman_proc       := matrices:-waksman_product;

# local to gfun, since 'option load' seems to be ignored in submodules
NUMGFUN_SETUP := proc($) types:-setup() end proc:
NUMGFUN_CLEANUP := proc($) types:-cleanup() end proc:

_pexports:=proc() [op({exports(thismodule)} minus NUMGFUN_HIDDEN)] end:
NUMGFUN_HIDDEN:={
    ':-_pexports',
    ':-version',
    ':-Settings'
}:

version := "0.5devel";

end module:

