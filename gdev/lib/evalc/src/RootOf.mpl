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

 # -*-maple-*-
#
# modif BS: two-argument RootOf handled. If second arg numeric then use it.
#            

# this one does not disappear with maple6

# New version for Maple17. BS. June 2013.

`evalc/RootOf`:=proc(p)
local x, y, a, b, v, i, f, polpart, res;
    if type(p, 'polynom(algnum, _Z)') then
        if nargs = 2 then
            if has(args[2], 'label') then return `evalc/unsplit`('RootOf'(args))
            elif type(args[2], identical('index') = integer) then
                f := evalf(RootOf(args));
                if not type(f, complex(float)) then return `evalc/unsplit`('RootOf'(args))
                end if
            else f := evalc(args[2])
            end if
        end if;
        if type(p, 'polynom(numeric, _Z)') and
        sturm(sturmseq(p, _Z), _Z, -infinity, infinity) = degree(p, _Z) then
            polpart := [p, _Z]
        else
            v := `evalc/evalc`(subs(_Z = x + y*I, p));
            a := op(1, v);
            b := op(2, v);
            polpart := [subs(x = _Z, resultant(a, b, y)), subs(y = _Z, resultant(a, b, x))]
        end if;
        if nargs = 1 then res := `evalc/split`(op(map(RootOf, polpart)))
### This is where it differs from the library version
        elif Im(f) = 0 then res := `evalc/split`(RootOf(polpart[1], f), 0)
### end change.
        else res := `evalc/split`(RootOf(polpart[1], Re(f)), RootOf(polpart[2], Im(f)))
        end if;
        if nargs = 2 and type(args[2], identical('index') = integer) then
            try res := map(`evalc/RootOf/index`, res)
            catch "index not tracked": return `evalc/unsplit`('RootOf'(args))
            end try
        end if;
        for i in res do `evalc/remember`(i) := `evalc/split`(i, 0) end do;
        res
    else `evalc/unsplit`('RootOf'(args))
    end if
end proc:

#savelib( `evalc/RootOf`);
