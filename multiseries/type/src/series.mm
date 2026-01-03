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
##    Title: 	`type/MultiSeries:-series` and :-taylor`
##    Created:	Thu Nov  6 10:29:55 2003
##    Author: 	Bruno Salvy
##		<Bruno.Salvy@inria.fr>
##
## Description: 
## This is necessary so that if the user overrides series or taylor using
## with(MultiSeries), the calls to type(s,series) will behave normally.

# The evaluations are done during the creation of the module.

`type/MultiSeries:-taylor`:=eval(:-`type/taylor`):

`type/MultiSeries:-series`:=proc(e) type(e,':-series') end:

