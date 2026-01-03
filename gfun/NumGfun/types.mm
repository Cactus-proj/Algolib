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

types := module()

export typecheck_hr_eq, types_table, new_record, setup, cleanup;

# Loosely inspired by formatdiffeq. To be improved.
typecheck_hr_eq := proc(expr, inicondtype)
    local ini, eq;
    if type(expr, 'set') then
        ini, eq := selectremove(type, expr, inicondtype);
        if nops(eq) <> 1 then return false end if;
        eq := op(eq);
    elif type(expr, 'list') then
        false;  # the list format is not supported in NumGfun
        #if nops(expr) <= 1 then return false end if;
        #eq := op(1, expr);
        #ini := {op(2..-1), l};
    else
        eq := expr;
    end if;
    if type(eq,'`=`') then eq := lhs(eq) - rhs(eq) end if;
    if indets(eq) = {} then return false end if;
    # TODO: add more checks!
    true;
end proc:

# Type names are currently local to NumGfun itself.
# TODO:  Move at least some of them to submodules (including NumGfun:-types,
# which would then probably need to be exported)?
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

    'internal_path' =
        'list'({
            'complex'('numeric'),
            'specfunc'('complex'('numeric'), ancont:-REGSING)}),

    'generalized_rec_matrix' = regsing:-`type/generalized_rec_matrix`,

    regsing_params = regsing:-`type/regsing_params`,

    'numdenmatrix' = matrices:-numdenmatrix:-typecheck,

    NULL]);

# Create an empty record of type 'type_name', based on the type definition
new_record := proc(type_name, $)
    local type_expr;
    type_expr := types_table[type_name];
    if op(0, type_expr) <> 'record' then
        error "expected record type";
    end if;
    Record(op(map(convert, type_expr, 'symbol')))
end proc:


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
