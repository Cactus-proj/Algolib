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

`algolib/version` := "14.0" :

_algolibcontent := 'printf("\
Content of algolib (version %s), as of October 2010:\n\n\
+ encyclopedia.\t\t\t[Written by Stéphanie Petit, with contributions by Bruno Salvy and Michèle Soria.]\n\
+ gdev.\t\t\t\t[Written by Bruno Salvy.]\n\
+ gfun (version %.2f).\t\t[Maintained and extended by Bruno Salvy, with contributions by Ludovic Meunier, Marc Mezzarobba, Marni Mishna, and Eithne Murray, original version by Bruno Salvy and Paul Zimmermann.]\n\
+ Holonomy (version %a).\t[Written by Frédéric Chyzak.]\n\
+ MAD (version %a).\t\t[Written by Ludovic Meunier.]\n\
+ Mgfun (version %a).\t\t[Written by Frédéric Chyzak, with contributions by Shaoshi Chen, Cyril Germa, Lucien Pech, and Ziming Li.]\n\
+ MultiSeries.\t\t\t[Written by Bruno Salvy.]\n\
+ regexpcount (version %a).\t[Written by Pierre Nicodème.]\n\
",\
`algolib/version`,\
gfun:-version,\
`Holonomy/version`,\
MAD:-VERSION,\
`Mgfun/version`,\
`regexpcount/version`\
)' :

#savelib('`algolib/version`', '_algolibcontent');
