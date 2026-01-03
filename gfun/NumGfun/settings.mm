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

## Settings and default values
## Marc Mezzarobba, projet Algorithms, INRIA Rocquencourt


Settings := module()

export
    binary_splitting_threshold,
    precision_ini,
    terms_factor,
    terms_delta,
    bound_ratpoly_max_expand,
    bound_ratpoly_tail_prec,
    get_rid_of_poly_thr,
    binomial_expand_threshold;

# - le scindage binaire gagne largement, mettre 10000 ci-dessous pour s'en
#   convaincre
# - calibré pour ndmatrix ou ndseries
binary_splitting_threshold := 800;

# When N digits are requested, compute the transition matrix to the normal
# precision, but compute the initial values to precision_ini(required prec)
# digits and return that many (useful to test bounds). See apply_ini for details
# on what this means.
precision_ini := N -> N;

# when N terms would seem necessary, compute terms_factor*N+terms_delta instead
terms_factor := 1;
terms_delta := 0;

#binsplit = `binsplit/inlined`;
#binsplit = `binsplit/generic`;

bound_ratpoly_tail_prec := 1.1;
bound_ratpoly_max_expand := 20;

get_rid_of_poly_thr := 30;

binomial_expand_threshold := 10;

end module:

