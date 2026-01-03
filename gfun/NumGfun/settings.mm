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

## Settings and default values
## Marc Mezzarobba, projet Algorithms, INRIA Rocquencourt


Settings := module()

################################################################################
# Binary splitting and numerical evaluation
################################################################################

# Note: not all variant actually use these parameters.

# Used when the precision is omitted in evaldiffeq, fnth_term etc.  (We
# purposely do not use Digits here!)
export default_eval_precision := 10;

export binary_splitting_threshold := 200;

# ``moderate'' size below which (+1) bb-evaluation is not used
export bit_burst_threshold := 1;

# When N digits are requested, compute the transition matrix to the normal
# precision, but compute the initial values to precision_ini(required prec)
# digits and return that many (useful to test bounds). See apply_ini for details
# on what this means.
export precision_ini := N -> N;

# when N terms would seem necessary, compute terms_factor*N+terms_delta instead
export terms_factor := 1;
export terms_delta := 0;

#binsplit = `binsplit/inlined`;
#binsplit = `binsplit/generic`;

export enable_absolute_precision_warning := true;

export diffeqtoproc_max_precomp := 1000;

################################################################################
# Bounds
################################################################################

export bound_ratpoly_tail_prec := 1.1;
export bound_ratpoly_max_expand := 20;
export find_constant_max_expand := 10;

export get_rid_of_poly_thr := 30;

export binomial_expand_threshold := 10;

################################################################################
# Emergency stop
################################################################################

# subdivide_path emergency stop threshold
export max_steps := 30;
# emergency stop threshold for symbolic-numeric computations with algebraic
# numbers and the like
export max_digits := 200;
# emergency stop threshold for high-precision computations without bounds
export max_digits_huge := 100000;
# give up computing the sum of a series when that many terms are needed
export max_series_terms := 10^12;
# give up trying to bound an indicial polynomial after looking that far
export max_indicial_eq_tail_index := 3000;
# give up the computation when the bound parameter K is too large
export max_bound_exponent := 200;

################################################################################
# Path rewriting
################################################################################

export split_path_threshold := 0.7;

end module:

