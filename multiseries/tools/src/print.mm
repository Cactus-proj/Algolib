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

###    -*-Maple-*-
###
###    Title: 	`print/SERIES`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 : operands of SERIES data structure
### Description           : Utility to print the SERIES data structure.
###

`print/SERIES` := proc(	scale, lcoeffs, coeffO, typeCoeff, lexpons	,
                        exponO,	typeExpon, expanVar, expr4SERIES 	)
option ALGOCOPYRIGHT; 
local i :: nonnegint, output , inds, expvar;

    expvar:=op(expanVar);
    output := add(lcoeffs[i]*expvar^lexpons[i],i=1..nops(lcoeffs)) ;

    if exponO<>infinity or coeffO<>0 then
        if typeCoeff<>'t_SERIES' then output:=output + O(coeffO*expvar^exponO)
        else output:=output+coeffO*expvar^exponO
        fi
    fi;
    if has(output,_var) then
        inds:=indets(output,specindex(anything,_var));
        subs([seq(i=op(i),i=inds)],output)
    else output
    fi

end proc :                                                     # `print/SERIES`
#---------------------------------------------------------------------------

