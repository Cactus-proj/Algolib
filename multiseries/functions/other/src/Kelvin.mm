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
##    Title: 	Kelvin functions
##    Created:	Mon Nov 17 16:49:48 2003
##    Author: 	Bruno Salvy
##		<salvy@pommard.inria.fr>
##
## Description: these are reduced to BesselK, BesselJ, HankelH1 & HankelH2.


#  KelvinKer(n,x) + I*KelvinKei(n,x) = exp(-n*Pi*I/2)*BesselK(n,x*exp(I*Pi/4))
#  KelvinKer(n,x) - I*KelvinKei(n,x) = exp(-n*Pi*I/2)*BesselK(n,x*exp(-I*Pi/4))
FUNCTIONTABLE['KelvinKer']:=proc(expr,scale,var,ord)
local n, x;
    n:=expr[1]; x:=expr[2];
    RUN('exp'(-n*Pi*I/2)/2*
	    ('BesselK'(n,x*exp(I*Pi/4))+'BesselK'(n,x*exp(-I*Pi/4))),
	    args[2..-1])
end:

FUNCTIONTABLE['KelvinKei']:=proc(expr,scale,var,ord)
local n, x;
    n:=expr[1]; x:=expr[2];
    RUN('exp'(-n*Pi*I/2)/2/I*
	    ('BesselK'(n,x*exp(I*Pi/4))-'BesselK'(n,x*exp(-I*Pi/4))),
	    args[2..-1])
end:

#  KelvinBer(n,x) + I*KelvinBei(n,x) = BesselJ(n,x*exp(3*I*Pi/4))
#  KelvinBer(n,x) - I*KelvinBei(n,x) = BesselJ(n,x*exp(-3*I*Pi/4))
FUNCTIONTABLE['KelvinBer']:=proc(expr,scale,var,ord)
local n, x;
    n:=expr[1]; x:=expr[2];
    RUN(('BesselJ'(n,x*exp(3*I*Pi/4))+'BesselJ'(n,x*exp(-3*I*Pi/4)))/2,
	args[2..-1])
end:

FUNCTIONTABLE['KelvinBei']:=proc(expr,scale,var,ord)
local n, x;
    n:=expr[1]; x:=expr[2];
    RUN(('BesselJ'(n,x*exp(3*I*Pi/4))-'BesselJ'(n,x*exp(-3*I*Pi/4)))/2/I,
	args[2..-1])
end:

#  KelvinHer(n,x) + I*KelvinHei(n,x) = HankelH1(n,x*exp(3*I*Pi/4))
#  KelvinHer(n,x) - I*KelvinHei(n,x) = HankelH2(n,x*exp(-3*I*Pi/4))
FUNCTIONTABLE['KelvinHer']:=proc(expr,scale,var,ord)
local n, x;
    n:=expr[1]; x:=expr[2];
    RUN(('HankelH1'(n,x*exp(3*I*Pi/4))+'HankelH2'(n,x*exp(-3*I*Pi/4)))/2,
	args[2..-1])
end:

FUNCTIONTABLE['KelvinHei']:=proc(expr,scale,var,ord)
local n, x;
    n:=expr[1]; x:=expr[2];
    RUN(('HankelH1'(n,x*exp(3*I*Pi/4))-'HankelH2'(n,x*exp(-3*I*Pi/4)))/2/I,
	args[2..-1])
end:

