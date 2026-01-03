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

# dfinite_expr_to_rec returns the recurrence satisfied
# by the shift variable in the entered expression.

dfinite_expr_to_rec:=proc(expr,f_of_typed_n::function(name))
option `Copyright (c) 1999-2002 Frederic Chyzak and Cyril Germa, INRIA, France`;
global
# Symbols.
shift;
    dfinite_expr_to_sys(expr,(op(0,f_of_typed_n))(op(1,f_of_typed_n)::'shift'));
end proc: # dfinite_expr_to_rec
