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
###    Title:   `multiseries/function`[Re] and [Im]
###    Created: Sep 2003
###    Author:  Bruno Salvy & Alexandre Sedoglavic
###    Contact:         Bruno.Salvy@inria.fr
###
### Input                 : 
###                         ser       a SERIES data-structure or 0
### Output                : 
###                         
###                         real or imaginary part of ser
###
### The work is done in RealImPart.

`multiseries/function`[Re] :=proc( ser ) :: {SERIES,identical(0)} ;

local real,im ;

option ALGOCOPYRIGHT; 
    ASSERT(ser::{'SERIES',identical(0)});
        
    REALIMPART(ser,real,im) ;
	        
    return real ;
	        
end proc:                                        # `multiseries/function`[Re]
#----------------------------------------------------------------------------

`multiseries/function`[Im] :=proc( ser ) :: {SERIES,identical(0)} ;

local real,im ;

option ALGOCOPYRIGHT; 
    ASSERT(ser::{'SERIES',identical(0)});
        
    REALIMPART(ser,real,im) ;
	        
    return im ;
        
end proc:                                        # `multiseries/function`[Im]
#----------------------------------------------------------------------------

