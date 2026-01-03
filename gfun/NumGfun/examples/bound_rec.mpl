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

# Some examples of the use of bound_rec, (mostly?) from:
# - Marc Mezzarobba and Bruno Salvy.  Effective Bounds for P-Recursive
#   Sequences.  Journal of Symbolic Computation 45(10):1075–1096, 2010.
# - Marc Mezzarobba.  Autour de l'évaluation numérique des fonctions D-finies.
#   Thèse de doctorat, École polytechnique, 2011.

with(gfun:-NumGfun);

# precision required in the Chudnovsky formula
A := 13591409;
B := 545140134;
C := 640320^3;
rec := { (A+B*n) * (3*n+3)* (3*n+2)*(3*n+1) * (n+1)^3 * C * u(n+1)
         + (6*n+6)*(6*n+5)*(6*n+4)*(6*n+3)*(6*n+2)*(6*n+1) * (A+B*(n+1)) * u(n),
         u(0) = A };
bound_rec(rec, u(n));

# Baxter permutations
rec := { (n+2)*(n+3)*a(n) = (7*n^2+7*n-2)*a(n-1) + 8*(n-1)*(n-2)*a(n-2), a(0)=1, a(1)=1 };
bound_rec(rec, a(n));

# involutions (see Wimp & Zeilberger1985, Example 2.1)
rec := { (n+1)*t(n) + t(n+1) - t(n+2), t(0) = 1, t(1) = 1 };
bound_rec(rec, t(n));

# a sequence of integrals (see Wimp & Zeilberger1985, Example 2.3)
rec := { 2*i(n+3) = (n+2)*i(n+1)+i(n), i(0)=1/5, i(1)=1/5, i(2)=1/5 };
bound_rec(rec, i(n));
