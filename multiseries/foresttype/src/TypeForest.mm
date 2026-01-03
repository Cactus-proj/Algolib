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
###    Title: 	TypeForest
###    Created:	Sep 2002
###    Author: 	Bruno Salvy & Alexandre Sedoglavic
###    Contact: 	Bruno.Salvy@inria.fr
###
### Description           :
###				TypeForest is a table that encodes the 
###                             possible conversion between the types defined 
###				for multiseries.
### Gloabal Variables     : TypeForest
### Other                 :
###		             If the type X is known by maple and has to be 
###                          inserted in TypeForest:
###                                - change the table TypeForest;
###                                - change `multiseries/WhatType` 
###                                  (only for new leaves)
### Other		  :
###                         see the type t_SERIES defined series.mpl

TypeForest := table([
(integer)   = {rational} ,
(fraction)  = {rational}, # that way fraction effectively = rational
(rational)  = {algebraic}    ,
(algebraic) = {float},
(float)     = {t_SERIES}
]) :

#---------------------------------------------------------------------------

