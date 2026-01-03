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

sum_of_sys:=proc(sys,r)
    local opts,n,a,b;
    option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
    global name,algebraic,identical,residues,
        _takayama_algo,_incautious,_natural_boundaries;
    if not typematch(r,n::'name'=a::'algebraic'..b::'algebraic') then
        error "bad summation interval"
    end if;
    opts:={args[3..-1]};
    if opts minus {'_takayama_algo','_incautious','_natural_boundaries'}<>{}
    then
        error "bad options"
    end if;
    anti_partial_of_sys(sys,r,opts,"sum")
end proc:
