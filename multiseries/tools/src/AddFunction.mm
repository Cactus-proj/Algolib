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
###    Title:   AddFunction, FunctionSupported,
###             RemoveFunction, GetFunction
###    Created: Aug 2004
###    Author:  Bruno Salvy & Ha Le
###    Contact:         Bruno.Salvy@inria.fr
###
### Description           :
###                             Library Extension Mechanism
###
### Gloabal Variables     : AddFunction, FunctionSupported, 
###                         RemoveFunction, GetFunction

module()
   description "Library extension mechanism for MultiSeries";
#   export FUNCTIONSTABLE;

   AddFunction := proc()
      FUNCTIONSTABLE[args[1..-2]] := args[-1];
      NULL
   end proc:

   RemoveFunction := proc()
      unassign(evaln(FUNCTIONSTABLE[args]))
   end proc:

   FunctionSupported := proc()
      assigned(FUNCTIONSTABLE[args])
   end proc:

   GetFunction := proc()
      if assigned(FUNCTIONSTABLE[args]) then
         eval(FUNCTIONSTABLE[args])
      end if;
   end proc:

#   inds := sort([indices(`multiseries/function`)]):
#   for i to nops(inds) do
#       AddFunction(op(inds[i]),eval(`multiseries/function`[op(inds[i])],1))
#   end do;

end module:
