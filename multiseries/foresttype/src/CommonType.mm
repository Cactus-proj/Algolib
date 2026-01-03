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
###    Title: 	CommonType
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Input                 :
###                          a set of types present in TypeForest
### Output                :
###                          a type that is an ancestor of all the input types
### Description           :
###                          Suppose that T is a type.
###                          If T can be converted into a type A, 
###                          this type is called an ancestor.
###
###                          CommonType returns the smallest possible common
###                          ancestor.
### Implementation        : 
###                          option remember (cleaned in multiseries).
### Error Conditions      :
###                         An error occurs if the input types have 
###                         no common ancestors
### Global Variables      : 
###                         TypeForest 
###
### Note: this can be improved by using a better graph algorithm.

## the procedure `multiseries/ancestors` returns all the ancestors of
## the input type mytype.

`multiseries/ancestors` := proc(mytype) :: set(type) ;
option remember,ALGOCOPYRIGHT; # the remember table will remain very small
local res :: set(type) :
    ASSERT(mytype :: type);
	if not assigned(TypeForest[mytype]) 
	then {}
	else res := TypeForest[mytype] ;	
             return res union op(map(procname,res))
	end if
end proc :
 
`multiseries/CommonType` := proc(types) :: type ;
option remember,ALGOCOPYRIGHT;
local res :: set(type), i :: type ;
    ASSERT(types :: set(type));
	if types = {} 
	then error "empty arguments"
	elif nops(types)=1
	then return op(types)
	else res := `intersect`(seq({i} union `multiseries/ancestors`(i),
                                     i=types                             )) ;
	     if res={}
	     then error "%1 can not be converted in a common type",types
	     elif nops(res)=1
             then return op(res)
             else for i to nops(res) do
		     if not member( op(i,res),
		`union`(op(map(`multiseries/ancestors`,subsop(i=NULL,res))))
                                  )
		     then return op(i,res)
		     end if
	          end do ;
          error "multiple common types, the table TypeForest is likely incorrect"

            end if 
	end if

end proc :                                           # `multiseries/CommonType`




