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

## matrices.mm -- Matrix-like data structures used to compute recurrence
## sequences by binary splitting
## Marc Mezzarobba, Algorithms project, INRIA Rocquencourt

matrices := module()

export numdenmatrix, matrix_ops;

# Matrices (square or rectangular, with integer or rational coefficients) with
# separate denominators.
#
# Special stuff I may want to add later (see newmatrices in git):
# - rec_matrix(operator, Sn, n)
# - extract_main_row
numdenmatrix := module()

    export typecheck, from_matrix, to_matrix, row_dimension, column_dimension,
        identity, multiply, bound_norm, from_matrix_float, make;

    # NumDenMatrix objects are represented as pairs (rather than modules) for
    # compatiblity with older code.  This might also be faster.
    typecheck := proc(x, $)
        type(x, 'specfunc(Or(Matrix,Vector,polynom(integer)),NumDenMatrix)');
    end proc:

    # Convert a Matrix (or anything convertible to Matrix) of integers or
    # polynomials into a NumDenMatrix
    from_matrix := proc(mat, algorithm:="lcm", $)
        local denoms, den, x;
        if type(mat, Matrix) then
            denoms := map(denom@rhs, rtable_elems(mat));
            if algorithm = "lcm" then
                # should work for polynomials as well as integers
                den := lcm(op(denoms));
            elif algorithm = "mul" or algorithm = "multiply" then
                den := mul(x, x in denoms);
            else
                error("Unrecognised algorithm");
            end if;
            return(NumDenMatrix(den*mat, den));
        else
            from_matrix(convert(mat, Matrix))
        end if;
    end proc:

    to_matrix := proc(ndm, $)
        option inline;
        op(1, ndm)/op(2, ndm);
    end proc:

    row_dimension := proc(ndm, $)
        option inline;
        LinearAlgebra:-RowDimension(op(1, ndm));
    end proc:

    column_dimension := proc(ndm, $)
        option inline;
        LinearAlgebra:-ColumnDimension(op(1, ndm));
    end proc:

    identity := proc(size, $)
        option inline;
        NumDenMatrix(LinearAlgebra:-IdentityMatrix(size), 1);
    end proc:

    multiply := proc(a, b, $)
        option inline;
        NumDenMatrix(
            mvMultiply(op(1,a), op(1,b)),
            op(2,a) * op(2,b));
    end proc:

    bound_norm := proc(ndm, $)
        local num, den;
        UseHardwareFloats := false;  # insufficient exponent range
        num, den := op(ndm);
        num := rndu(num);
        den := rndz(den);
        Rounding := infinity;
        LinearAlgebra:-Norm(num/den, 'Frobenius');
    end proc:

    from_matrix_float := proc(mat, $)
        local expo, den, num;
        expo := min(0, op(map(
            proc(z) SFloatExponent(Re(z)), SFloatExponent(Im(z)) end proc,
            ListTools:-Flatten(convert(mat, 'listlist')))));
        den := 10^(-expo);
        num := map(
            mapReIm(proc(x)
                SFloatMantissa(x)*10^(SFloatExponent(x)-expo) end proc),
            mat);
        NumDenMatrix(num, den);
    end proc:

    make := proc(num, den, $)
        NumDenMatrix(num, den)
    end proc:

end module:

matrix_ops["numdenmatrix"] := proc(size)
    module()
        export identity, multiply;
        identity := numdenmatrix:-identity(size);
        multiply := numdenmatrix:-multiply;
    end module:
end proc:

matrix_ops["numdenseries"] := proc(size)
    module()
        export identity, multiply;
        identity := LinearAlgebra:-IdentityMatrix(size+1);
        multiply := mvMultiply;
    end module:
end proc:

end module:
