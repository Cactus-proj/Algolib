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

##    -*-Maple-*-
##
##    Title: 	Hankel functions
##    Created:	Mon Nov 17 16:49:48 2003
##    Author: 	Bruno Salvy
##		<salvy@pommard.inria.fr>
##
## Description: these are reduced to BesselY & BesselJ.

# HankelH1(n,z) := BesselJ(n,z) + I*BesselY(n,z);
# HankelH2(n,z) := BesselJ(n,z) - I*BesselY(n,z);
FUNCTIONTABLE['HankelH1']:=proc(expr,scale,var,ord)
local n, z;
    n:=expr[1]; z:=expr[2];
    RUN('BesselJ'(n,z)+I*'BesselY'(n,z),args[2..-1])
end:

FUNCTIONTABLE['HankelH2']:=proc(expr,scale,var,ord)
local n, z;
    n:=expr[1]; z:=expr[2];
    RUN('BesselJ'(n,z)-I*'BesselY'(n,z),args[2..-1])
end:

