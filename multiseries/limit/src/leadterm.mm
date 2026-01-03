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
###    Title: 	`multiseries/leadterm`
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###				ser    a SERIES data-structure
### Output                :
###				The leading part of ser encoded by a SERIES 
###                             data-structure
### Description           :
###                        This procedure returns the leading part of a SERIES
###                         data-structure. Here, the coefficient of the
###                         result does not contains any element of the
###                         asymptotic basis, which implies that some
###			    reexpansion of the leading term may take place.
###            
###

`multiseries/leadterm` := proc( ser ) :: {SERIES,identical(0)} :
option ALGOCOPYRIGHT;     
local newser, coeff, expr4coeff, j;
    ASSERT(ser::{'SERIES',identical(0)});
    if ser = 0 then return 0 end if;
    newser:=`multiseries/DominantTerm`(ser,op(THESCALE,ser),[op(EXPANVAR,ser)]);
    # sometimes DominantTerm recognizes 0
    if newser = 0 then return 0 fi;
    coeff:=op(op(LISTCOEFF,newser));
    if type(coeff,'t_SERIES') then
	coeff:=procname(coeff); 
	expr4coeff:=op(EXPR4SERIES,coeff)
    elif has(coeff,op(THESCALE,newser)['list']) and 
	member(op(EXPANVAR,newser),op(THESCALE,newser)['list'],'j') 
        and j>1 then # reexpand because of oscillating coefficients
	    coeff:=procname(`multiseries/getoneterm`(coeff,op(THESCALE,ser),
		op(THESCALE,ser)['list'][1..j-1]));
	    expr4coeff:=op(EXPR4SERIES,coeff)
    else expr4coeff:=coeff
    end if;
    return subsop(LISTCOEFF=[coeff],
	# How is this possible ? (Comment BS. Sep.02)
	`if`(type(coeff,'SERIES') and op(TYPECOEFF,newser)<>'t_SERIES',
	    TYPECOEFF='t_SERIES',NULL),
	EXPR4SERIES=expr4coeff*op(EXPANVAR,newser)^op(op(LISTEXPON,newser)),
	newser)
end proc :                                            # `multiseries/leadterm`
#------------------------------------------------------------------------------

