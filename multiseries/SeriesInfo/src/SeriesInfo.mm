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

SeriesInfo:=module()
option package;
export ListCoefficients,ListExponents,CoefficientBigO,ExponentBigO,Variable,Scale;#ClosedForm;
    ListCoefficients:=proc(s::SERIES)
        op(LISTCOEFF,s)
    end proc:
    ListExponents:=proc(s::SERIES)
        op(LISTEXPON,s)
    end proc:
    CoefficientBigO:=proc(s::SERIES)
        op(COEFFBIGO,s)
    end proc:
    ExponentBigO:=proc(s::SERIES)
        op(EXPONBIGO,s)
    end proc:
    Variable:=proc(s::SERIES)
        op(op(EXPANVAR,s))
    end proc:
## Not sure it should be there. If it's returned in terms of the original variable,
## it's not very useful. If it's returned in terms of _var, then more has to be returned.
#    ClosedForm:=proc(s::SERIES)
#        op(EXPR4SERIES,s)
#    end proc:
    Scale:=proc(s::SERIES)
        op(THESCALE,s)
    end proc:
end module;