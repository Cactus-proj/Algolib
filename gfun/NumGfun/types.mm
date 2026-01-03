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

types := module()

export typecheck_hr_eq, types_table, setup, cleanup;

# Loosely inspired by formatdiffeq. To be improved.
# FIXME:
#  dans le cas ou il n'y a pas de conditions initiales tu ne fais pas le  
#  test d'indets que tu fais dans l'autre cas, c'est ton souhait ?
#  selectremove ne te sert pas vraiment : remove suffit pour ce que tu fais
#  tu ne te sers pas du lhs-rhs, tu pourrais faire le indets directement  
#  avant
typecheck_hr_eq := proc(l, inicondtype)
    local r, ini, eq;
    if type(l, set) then
        r := selectremove(type, l, inicondtype);
        ini, eq := r[1], r[2];
        if nops(eq) <> 1 then return false end if;
        eq := op(eq);
        if type(eq,'`=`') then eq := lhs(eq) - rhs(eq) end if;
        if indets(eq) = {} then
        #or not type(eq, Or(`+`,`*`,specfunc(anything,diff))) then
            return false
        end if;
        true;
    else
        userinfo(6, 'gfun', "typechecking bare equation");
        true;
    end if;
end proc:

# Type names are currently local to NumGfun itself. I might want to make them
# local to NumGfun:-types (and export it) someday.
types_table := table([

    # human-readable gfun rec
    'hrrec' = 
        proc(eq)
            typecheck_hr_eq(eq, 'typefunc'('nonnegint','name') = 'anything');
        end proc,

    # human-readable gfun deq
    'hrdeq' =
        proc(eq)
            typecheck_hr_eq(eq, 'function'(0) = 'anything');
        end proc,

    # analytic continuation path
    'path' =
        { 'complex'('numeric'),
          'list'('complex'('numeric')) },

    # linear combination with complex(float) coefficient (return value of
    # analytic_continuation and friends)
    # XXX: how to improve this?
    'complex_linear' =
        proc(expr)
            type(expr,
                polynom('complex'('float'), frontend(indets, [expr])));
        end proc,

    # TODO: `type/ndmatrix`, `type/matrix_ring`

    NULL]);

# Define types local to NumGfun (see ?ExtendingMaple)

# This function is meant to be called by gfun's/NumGfun's 'option load'
setup := proc($)
    local mytype;
    for mytype in indices(types_table, 'nolist') do
        TypeTools:-AddType(mytype, types_table[mytype]);
    end do;
end proc:

cleanup := proc($)
    local mytype;
    for mytype in indices(types_table, 'nolist') do
        try TypeTools:-RemoveType(mytype)
        catch: end;
    end do;
end proc:

end module;
