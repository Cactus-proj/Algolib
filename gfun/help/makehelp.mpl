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

## The variable THE_HDB has to be assigned before running this Maple code.

#basedir := cat("Mathematics/Discrete Mathematics/Combinatorics/gfun-",
#    convert(gfun:-version, 'string'), "/"):

basedir := "Mathematics/Discrete Mathematics/Combinatorics/gfun/":
basedirNumGfun := cat(basedir,"NumGfun Subpackage/"):

## (semi-)automatically generated using i2makedoc.plx
makehelp(
  `gfun[algebraicsubs]`,
  `help/algebraicsubs.mws`,
  THE_HDB,
  'aliases'=["algebraicsubs"],
  'browser'=[cat(basedir, "algebraicsubs")]):
makehelp(
  `gfun[algeqtodiffeq]`,
  `help/algeqtodiffeq.mws`,
  THE_HDB,
  'aliases'=["algeqtodiffeq"],
  'browser'=[cat(basedir, "algeqtodiffeq")]):
makehelp(
  `gfun[algeqtoseries]`,
  `help/algeqtoseries.mws`,
  THE_HDB,
  'aliases'=["algeqtoseries"],
  'browser'=[cat(basedir, "algeqtoseries")]):
makehelp(
  `gfun/algfuntoalgeq`,
  `help/algfuntoalgeq.mws`,
  THE_HDB,
  'aliases'=["algfuntoalgeq"],
  'browser'=[cat(basedir, "algfuntoalgeq")]):
makehelp(
  `gfun[borel]`,
  `help/borel.mws`,
  THE_HDB,
  'aliases'=["borel"],
  'browser'=[cat(basedir, "borel")]):
makehelp(
  gfun[`rec+rec`],
  `help/cauchyproduct.mws`,
  THE_HDB,
  'aliases'=["borel","rec*rec","gfun/`rec*rec`"],
  'browser'=[cat(basedir, "rec+rec"), cat(basedir, "rec*rec"), cat(basedir, "borel")]):
makehelp(
  `gfun[diffeqtohomdiffeq]`,
  `help/diffeqtohomdiffeq.mws`,
  THE_HDB,
  'aliases'=["gfun[rectohomrec]", "diffeqtohomdiffeq", "rectohomrec"],
  'browser'=[cat(basedir, "diffeqtohomdiffeq"), cat(basedir, "rectohomrec")]):
makehelp(
  `gfun[diffeqtorec]`,
  `help/diffeqtorec.mws`,
  THE_HDB,
  'aliases'=["diffeqtorec"],
  'browser'=[cat(basedir, "diffeqtorec")]):
makehelp(
  `gfun/gftypes`,
  `help/gftypes.mws`,
  THE_HDB,
  'browser'=[cat(basedir, "gftypes"), cat(basedir, "diffeqtorec")]):
makehelp(
  `gfun`,
  `help/gfun.mws`,
  THE_HDB,
  'aliases'=["gfun/overview"," overview/gfun"],
  'browser'=[cat(basedir, "Overview")]):
makehelp(
  `gfun[guessgf]`,
  `help/guessgf.mws`,
  THE_HDB,
  'aliases'=["gfun[guesseqn]","guessgf","guesseqn"],
  'browser'=[cat(basedir, "guessgf"), cat(basedir, "guesseqn")]):
makehelp(
  gfun[`diffeq+diffeq`],
  `help/hadamardproduct.mws`,
  THE_HDB,
  'aliases'=["diffeq+diffeq", "gfun[`diffeq*diffeq`]", "diffeq*diffeq", 
            "gfun[hadamardproduct]", "hadamardproduct"],
  'browser'=[cat(basedir, "diffeq+diffeq"), cat(basedir, "diffeq*diffeq"),
            cat(basedir, "hadamardproduct")]):
makehelp(
  `gfun[holexprtodiffeq]`,
  `help/holexprtodiffeq.mws`,
  THE_HDB,
  'aliases'=["holexprtodiffeq"],
  'browser'=[cat(basedir, "holexprtodiffeq")]):
makehelp(
  `gfun[invborel]`,
  `help/invborel.mws`,
  THE_HDB,
  'aliases'=["gfun[Laplace]","invborel","Laplace","laplace","gfun/laplace"],
  'browser'=[cat(basedir, "invborel"), cat(basedir, "laplace")]):
makehelp(
  `gfun[listtoalgeq]`,
  `help/listtoalgeq.mws`,
  THE_HDB,
  'aliases'=["gfun[seriestoalgeq]","listtoalgeq","seriestoalgeq"],
  'browser'=[cat(basedir, "listtoalgeq"), cat(basedir, "seriestoalgeq")]):
makehelp(
  `gfun[listtodiffeq]`,
  `help/listtodiffeq.mws`,
  THE_HDB,
  'aliases'=["gfun[seriestodiffeq]","listtodiffeq","seriestodiffeq"],
  'browser'=[cat(basedir, "listtodiffeq"), cat(basedir, "seriestodiffeq")]):
makehelp(
  `gfun[listtohypergeom]`,
  `help/listtohypergeom.mws`,
  THE_HDB,
  'aliases'=["gfun[seriestohypergeom]","listtohypergeom","seriestohypergeom"],
  'browser'=[cat(basedir, "listtohypergeom"), cat(basedir, "seriestohypergeom")]):
makehelp(
  `gfun[listtoratpoly]`,
  `help/listtoratpoly.mws`,
  THE_HDB,
  'aliases'=["gfun[seriestoratpoly]","listtoratpoly","seriestoratpoly"],
  'browser'=[cat(basedir, "listtoratpoly"), cat(basedir, "seriestoratpoly")]):
makehelp(
  `gfun[listtorec]`,
  `help/listtorec.mws`,
  THE_HDB,
  'aliases'=["gfun[seriestorec]","listtorec","seriestorec"],
  'browser'=[cat(basedir, "listtorec"), cat(basedir, "seriestorec")]):
makehelp(
  `gfun[listtoseries]`,
  `help/listtoseries.mws`,
  THE_HDB,
  'browser'=[cat(basedir, "listtoseries")]):
makehelp(
  `gfun/parameters`,
  `help/parameters.mws`,
  THE_HDB,
  'aliases'=["gfun/Parameters"],
  'browser'=[cat(basedir, "Parameters")]):
makehelp(
  `gfun[poltodiffeq]`,
  `help/poltodiffeq.mws`,
  THE_HDB,
  'aliases'=["polytodiffeq"],
  'browser'=[cat(basedir, "poltodiffeq")]):
makehelp(
  `gfun[poltorec]`,
  `help/poltorec.mws`,
  THE_HDB,
  'aliases'=["poltorec"],
  'browser'=[cat(basedir, "poltorec")]):
makehelp(
  `gfun[ratpolytocoeff]`,
  `help/ratpolytocoeff.mws`,
  THE_HDB,
  'aliases'=["ratpolytocoeff"],
  'browser'=[cat(basedir, "ratpolytocoeff")]):
makehelp(
  `gfun[rectodiffeq]`,
  `help/rectodiffeq.mws`,
  THE_HDB,
  'aliases'=["rectodiffeq"],
  'browser'=[cat(basedir, "rectodiffeq")]):
makehelp(
  `gfun[rectoproc]`,
  `help/rectoproc.mws`,
  THE_HDB,
  'aliases'=["rectoproc"],
  'browser'=[cat(basedir, "rectoproc")]):
## end (semi-)automatically generated entries
makehelp(
  `gfun[nth_term]`,
  `help/nth_term.mws`,
  THE_HDB,
  'aliases'=["NumGfun/fnth_term", "nth_term", "fnth_term"],
  'browser'=[cat(basedir, "nth_term"), cat(basedir, "fnth_term"), cat(basedirNumGfun, "nth_term"), cat(basedirNumGfun, "fnth_term")]):
makehelp(
  `gfun[NumGfun]`,
  `NumGfun/help/NumGfun.mws`,
  THE_HDB,
  'aliases'=["gfun/NumGfun/overview", "NumGfun", " overview/NumGfun"],
  'browser'=[cat(basedirNumGfun, "Overview")]):
makehelp(
  `NumGfun[evaldiffeq]`,
  `NumGfun/help/evaldiffeq.mws`,
  THE_HDB,
  'aliases'=["NumGfun/analytic_continuation", "NumGfun/transition_matrix",
        "evaldiffeq", "analytic_continuation"],
  'browser'=[cat(basedirNumGfun, "evaldiffeq"), cat(basedirNumGfun,
        "analytic_continuation"), cat(basedirNumGfun,"transition_matrix")]):
makehelp(
  `NumGfun[diffeqtoproc]`,
  `NumGfun/help/diffeqtoproc.mws`,
  THE_HDB,
  'aliases'=["NumGfun/diffeqtoproc", "diffeqtoproc"],
  'browser'=[cat(basedirNumGfun, "diffeqtoproc")]):
makehelp(
  `NumGfun[bound_ratpoly]`,
  `NumGfun/help/bound_ratpoly.mws`,
  THE_HDB,
  'aliases'=["bound_ratpoly"],
  'browser'=[cat(basedirNumGfun, "bound_ratpoly")]):
makehelp(
  `NumGfun[bound_diffeq]`,
  `NumGfun/help/bound_diffeq.mws`,
  THE_HDB,
  'aliases'=["bound_diffeq"],
  'browser'=[cat(basedirNumGfun, "bound_diffeq")]):
makehelp(
  `NumGfun[bound_diffeq_tail]`,
  `NumGfun/help/bound_diffeq_tail.mws`,
  THE_HDB,
  'aliases'=["bound_diffeq_tail"],
  'browser'=[cat(basedirNumGfun, "bound_diffeq_tail")]):
makehelp(
  `NumGfun[bound_rec]`,
  `NumGfun/help/bound_rec.mws`,
  THE_HDB,
  'aliases'=["NumGfun/bound_rec_tail", "bound_rec", "bound_rec_tail"],
  'browser'=[cat(basedirNumGfun, "bound_rec"), cat(basedirNumGfun,"bound_rec_tail")]):
