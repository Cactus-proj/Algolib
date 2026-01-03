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
##    Title: 	taylor
##    Created:	Thu Oct 23 08:46:53 2003
##    Author: 	Bruno Salvy
##		<Bruno.Salvy@inria.fr>
##
## Description: invoke series and test whether the result is a Taylor expansion.
## It has to be a procedure, so that type/taylor works.

taylor := proc()
local res;
  res := MultiSeries:-series(args);
  if res=0 or type(res,'taylor') then res
  else error "does not have a taylor expansion, try series()"
  fi;
end proc:

