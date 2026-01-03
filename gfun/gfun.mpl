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

##    Title:   gfun package (for Generating FUNctions)
##    Created: Wed Mar   4 15:13:42 1992
##    Authors: Bruno Salvy <Bruno.Salvy@inria.fr>
##    Paul Zimmermann <Paul.Zimmermann@inria.fr>
##
## Description: converts implicit equations into differential equations,
##    differential equations into recurrences and vice-versa,
##    ordinary into exponential recurrences.
##    also converts lists to linear recurrences or differential
##    equations.
##
##  Some of the ideas used in the first versions of this file are due to
##  S. Plouffe and F. Bergeron.
##
## Modifications:
##
##   Parts included from ported should not be modified without discussing
##     with the DDMF development team
##
##   Starting from 2008, the logs are now kept with svn.
##   I'm still updating the variable version yet, it makes it easier to
##   deal with bug reports from the users.
##   3.24 new pade3. It works well, but is not always fast enough
##   to replace pade2, yet.					B.S. Nov 07
##   3.23 series returning csgn led to pbs with initial conds.  B.S. Sep 07
##   3.22 option homogeneous=true in rectodiffeq                B.S. Aug 07
##   3.21 faster algeqtodiffeq					B.S. Nov 06
##   3.20 recurrences are no longer made primitive 		B.S. Aug 06
##   	  before rectodiffeq. Added polylog to holexprtodiffeq.
##   	  Fixed the holexprtodiffeq entry for hypergeom so that
##   	  initial conditions are returned.
##   3.19 modified formatpoleq so that polynomials in y(x) are  B.S. May 06
##        accepted in the input
##   3.18 fixed ini cond in holexprtodiffeq entry for dilog     B.S. May 06
##   3.17 fixed algebraicsubs when the algebraic series has     B.S. May 06
##        a rational valuation
##   3.16 modified diffeqtorec so that it does not add extra    B.S. May 06
##        initial conditions when none are asked
##   3.15 added code for exp(int(algebraic)) in holexprtodiffeq B.S. May 06
##   3.14 fixed a bug in myisolve that prevented some indicial  B.S. May 06
##        equations to be solved
##   3.13 improved initial conditions in poltodiffeq            B.S. May 06
##   3.12 improved initial conditions in algeqtodiffeq 		B.S. Nov 05
##   3.11 fixed poltorec when more initial conditions than	B.S. Nov 05
##	  the order are necessary
##   3.10 recognize 0 in goodinitvalues/diffeq			B.S. Oct 05
##   3.09 better care of initial conditions at singular pts	B.S. Oct 05
##   3.08 unified computation of series, avoiding limit		B.S. Aug 05
##        This prevents problems arising from limit(x+cos(t+Pi/4),x=0)
##        being different from the constant coeff of its series expansion.
##   3.07 better type checking in algeqtoseries			B.S. Aug 05
##   3.06 compute simplified `@@`(D,i)(y)(x) without calling D; Edgardo Jan 05
##   3.05 various weaknesses reported by mint			B.S. Dec 04
##   3.04 adapted rectoproc to new expectations of FromInert	B.S. Nov 04
##   3.03 added userinfo messages in poltorec and poltodiffeq	B.S. Aug 04
##   3.02 changed the local I into II in rectoproc		B.S. Aug 04
##   3.01 cleaned up the _C[]'s in goodinitvalues/*		B.S. Aug 04
##   3.00 diffeqtable is now an hidden exported table		B.S. Aug 04
##   2.99 made gfun:-gfunParameters an exported table		B.S. Aug 04
##   2.98 fixed initial condition in some singular cases of	B.S. Aug 04
##	  diffeqtorec
##   2.97 fixed initial conditions in rectohomrec		B.S. Aug 04
##   2.96 improved handling of initial conditions in the	B.S. Aug 04
##        inhomogeneous case
##   2.95 fixed a bug with nested radicals in algfuntoalgeq	B.S. Aug 04
##   2.94 fixed a weakness in seriestoseries(.,revogf)		B.S. Aug 04
##   2.93 holexprtodiffeq(hypergeom([1,1],[],z),y(z)) returned a _C B.S. Aug 04
##   2.92 remove option "easy" in pade2, this makes it closer	B.S. Aug 04
##	  to numapprox[hermite_pade] and easy is made
##	  unncessary by kernelopts(cpulimit)
##   2.91 improved guessing with parameters			B.S. Aug 04
##   2.90 replace derivatives by diff(x*.,x) in guessing        B.S. Aug 04
##   2.89 find recurrences with constant coefficients
##	  when maxdegcoeff=0					B.S. Aug 04
##   2.88 avoided bad evaluations of globals in rectoproc	B.S. May 04
##   2.87 same as 2.77 for formatdiffeq				B.S. May 04
##   2.86 better argument checking in rectoproc			B.S. May 04
##   2.85 optimized number of locals in rectoproc		B.S. May 04
##   2.84 hide some of the exported variables using _pexports	B.S. May 04
##   2.83 transformed constant exports into constant valued procs. B.S. May 04
##   2.82 simplification of goodinitvalues/rec			B.S. May 04
##   2.81 new option maximalindex in rectoproc			B.S. Apr 04
##   2.80 optimize is now an option in rectoproc		B.S. Apr 04
##   2.79 rectoproc rewritten to use FromInert instead of procmake B.S. Apr 04
##   2.78 new version of rectoproc with many new functionalities.L.M. Apr 04
##   2.77 formatrec and listprimpart now use an extra parameter
##	to return the content. 			Ludovic Meunier Apr 04
##   2.76 formatrec was adding an extra initial condition for
##        one-term recurrences					B.S. Apr 04
##   2.75 ratpolytocoeff now uses piecewise for the first few values B.S. Apr 04
##   2.74 fix a bug in algebraic subs.				Ha Le Apr 04
##   2.73 two bug fixes						B.S. Mar 04
##   2.72 fixed a problem with the indices of listtoseriestable	B.S. Jul 03
##   2.71 ported to maple 9.				Marni Mishna Jan 03
##   2.70 ported to maple 7 & 8              			B.S. Aug 02
##        and better initial conditions in algfuntoalgeq
##   2.69 restrict changes in 2.66 to homogeneous equations	B.S. Aug 02
##   2.68 replaced version by `gfun/version`          		F.C. Mar 02
##   2.67 exported formatrec, formatdiffeq, makerec,makediffeq	B.S. Sep 01
##   2.66 treat cases with no initial condition as operators	B.S. Apr 00
##   2.65 remove is now in the kernel				B.S. Apr 00
##   2.64 ported to maple 6					B.S. Apr 00
##   2.63 fixed a problem with singularities in holexprtodiffeq	B.S. Jul 99
##   2.62 rectoproc generates a smaller proc in the log case	B.S. Jun 99
##   2.61 removed last change due to another bug in V.5		B.S. Sep 98
##   2.60 added option 'normal' to convert/fullparfrac to get	B.S. Sep 98
##        the expected behaviour of Maple V.4
##   2.59 improved speed of systomatrix				B.S. Aug 98
##   2.58 holexprtodiffeq did not work on exp(algebraic)^rat	B.S. Aug 98
##   2.57 fixed a problem where algfuntoalgeq could return	B.S. Jul 98
##   rational function coefficients instead of polynomials.
##   2.56 case of non-homogeneous recurrence with constant	B.S. Jul 98
##   coefficients in the homogeneous part now handled by
##        rectoproc by the logarithic algorithm.
##   2.55 workaround a bug in solve/linear in the case of	B.S. Jul 98
##        floating point coefficients
##   2.54 fixed a bug for multiple branches in algeqtoseries	B.S. May 98
##   2.53 fixed a bug in rectodiffeq in the inhomogenous case	B.S. May 98
##   2.52 undeclared local in diffeqtable[hypergeom]		B.S. Jan 98
##   2.51 fixed a bug in diffeqtorec in the inhomogeneous case	B.S. Jan 98
##   when a simplification was possible
##   2.50 help pages in the new format				B.S. May 97
##   added diffeqtohomdiffeq and rectohomrec
##   fixed a problem with the order in algfuntoalgeq
##   2.49 changed the handling of initial conditions of		B.S. May 97
##   differential equations so that singular solutions
##   can be considered more easily.
##        added many functions to holexprtodiffeq
##   2.48 added hypergeom to holexprtodiffeq and changed the	B.S. Feb 97
##   way it deals with functions having two arguments
##   2.47 fixed a problem in algeqtoseries due to the		B.S. Feb 97
##        of solve with RootOf's
##   2.46 faster guessing when coefficients are integers	B.S. Sep 96
##   2.45 speedup by using collect less, and coeffs more	B.S. Sep 96
##   2.44 got rid of many D's, hence an important speedup	B.S. Sep 96
##   2.43 fixed a bug in listtorec, added BesselK and BesselY	B.S. Aug 96
##   to holexprtodiffeq
##   2.42 more careful handling of the leading coefficient in	B.S. Jul 96
##   the recurrence given by the user
##   2.41 added handling of pol(x)^(free(x)) to holexprtodiffeq	B.S. Jul 96
##   2.40 fixed a bug in guessgf for algebraic equations	B.S. Jul 96
##   2.39 fixed holexprtodiffeq on expressions not involving	E.M. Mar 96
##   the variable; improved error message/help page
##   2.38 fixed another weakness of rectoproc			B.S. Mar 96
##   2.37 fixed weaknesses of the new options of rectoproc	B.S. Mar 96
##   2.36 improved speed of pade2 for polynomial coefficients	B.S. Mar 96
##   2.35 added options list and params to rectoproc		B.S. Mar 96
##   2.34 improved order of equation returned by algfuntoalgeq	E.M. Mar 96
##   2.33 listtorec now uses pade2 and should be much faster	B.S. Mar 96
##   2.32 improved rejection of solutions from pade2		B.S. Feb 96
##   improved efficiency of pade2 by a huge factor
##   improved help pages
##   a few bug fixes
##   2.31 added holexprtodiffeq and algfuntoalgeq		E.M. Feb 96
##   E. Murray - Eithne.Murray@inria.fr
##   2.3  more type checking					B.S. Jan 96
##   extended poltodiffeq and poltorec to diff and shift
##   algebraicsubs now handles initial conditions
##   added algeqtoseries
##   2.29 Fixed a bug reported by E. Murray in algebraicsubs	B.S. Jan 96
##   2.28 same with poltorec					B.S. Jan 96
##   2.27 poltodiffeq added,					B.S. Jan 96
##   `diffeq[+*]diffeq` rewritten using poltodiffeq
##   2.26 Further improvements on the initial conditions	B.S. Jan 96
##   2.25 Better treatment of free initial conditions		B.S. Dec 95
##   2.24 Small improvement to gfun/isolve			B.S. Dec 95
##   2.23 Bug reported by H. Prodinger. gfun/gausselim was not	B.S. Nov 95
##   returning the smallest possible order.
##   2.22 suppressed the use of 'easy' in the guessing part,	B.S. Nov 95
##   this missed too many cases
##   2.21 fixed a bug in rec*rec in the nonhomogeneous case	B.S. Aug 95
##   2.2  macro MAPLE5.4					B.S. Apr 95
##   New version for the share library.
##   2.19 algeqtodiffeq did not allow functions that were	B.S. Apr 95
##    singular at the origin.
##   further polishing of goodinitvalues/diffeq both in
##    regular and singular case.
##   same modification as version 2.13 for diffeq+diffeq
##   2.18 formatrec did not complain on empty recurrences	B.S. Apr 95
##    plus a bug fix in systomatrix for equations with
##    parameters.
##   2.17 rec+rec used diffeq+diffeq, but the order of		B.S. Mar 95
##    the recurrence was not always optimal. Rewritten
##    to compute directly on recurrences.
##   2.16 diffeq+diffeq had a local variable leak when the	B.S. Feb 95
##    initial conditions involved a linear dependency
##    (like D(y)(0)=y(0))
##   2.15 rectoproc improved in the non-remember case		B.S. Jan 95
##     following an idea of PZ.
##   removed the macros MAPLE5.2 and MAPLE5.3
##     (switching did not work anymore)
##   added a case in diffeqtorec where the order of the
##     recurrence can be lowered by adding an inhomogeneous part
##   added Laplace as an alias to invborel
##   fixed a weakness in rectodiffeq
##   2.14 diffeq+diffeq was returning a non-formatted equation	B.S. Jan 95
##     when called with two rational functions.
##   formatdiffeq reinforced to forbid y(0) in the coefficients.
##   rectodiffeq now returns a homogeneous equation when
##     the inhomogeneous one contains (D@@k)(y)(0).
##   2.13 diffeq*diffeq did not always find all the initial	B.S. Jan 95
##   conditions. This is fixed by giving an extra arg
##   to goodinitvalues/diffeq. Same for goodinitvalues/rec.
##   2.12 reduced the number of RETURN statements		B.S. Dec 94
##   2.11 fixed bug in algebraicsubs				B.S. Dec 94
##   2.1 fixed bugs reported by Ch. Mallinger			P.Z. Nov 94
##   2.07 heavy testing, plus a refinement of the criterion for	B.S. Aug 94
## rejecting overdetermined results from pade2
##   2.06 faster reversion of power series			B.S. Jul 94
##   2.05 guessing part rewritten to use pade2 for rational	B.S. Jun 94
## series, differential and algebraic equations
## plus a bug fixed in the initial conditions of a product
##   2.01 a few extra checks, clarified help for listtolist	B.S. May 94
##   2.0 new version                				B.S. May-Nov 93
##   rectoproc, diffeqtorec, rectodiffeq, rectoproc,
##   borel, invborel rewritten.              			B.S. Dec 92
##   formatdiffeq, formatrec added.          			B.S. Dec 92
##

##########################################################################
## CONVENTION: a recurrence is an expression of the form
## sum(p[i]*u(n+i),i=0..d)         (E)
## or
##    { sum(p[i]*u(n+i),i=0..d), u(0)=a0,..., u(k)=ak }.
##
## The p[i]'s are polynomials in n. The sequence(s) represented by such
## a recurrence are solutions of (E) for n>=k, where k is the largest
## positive integer solution to p[d](n-d)=0, or 0 if this does not cancel.
## For n<k, the values of the sequence are given by the initial conditions,
## and it is part of the convention that u(i)=0 for i<0.
##########################################################################

gfun := module()

# NUMGFUN_SETUP and NUMGFUN_CLEANUP are defined in NumGfun/main.mm ('option
# load' seems to be ignored for submodules)
option package, load=NUMGFUN_SETUP, unload=NUMGFUN_CLEANUP;

export
   algfuntoalgeq,
   algebraicsubs,
   algeqtodiffeq,
   algeqtoseries,
   borel,
   cauchyproduct,
   `diffeq+diffeq`,
   `diffeq*diffeq`,
   diffeqtohomdiffeq,
   diffeqtorec,
   guesseqn,
   guessgf,
   hadamardproduct,
   holexprtodiffeq,
   invborel,
   Laplace,
   listtoalgeq,
   listtodiffeq,
   listtohypergeom,
   listtolist,
   listtoratpoly,
   listtorec,
   listtoseries,
   pade2,
   poltodiffeq,
   poltorec,
   ratpolytocoeff,
   `rec+rec`,
   `rec*rec`,
   rectodiffeq,
   rectohomrec,
   rectoproc,
   seriestoalgeq,
   seriestodiffeq,
   seriestohypergeom,
   seriestolist,
   seriestoratpoly,
   seriestorec,
   seriestoseries,
   Parameters,
   NumGfun,
   nth_term,                            # defined in NumGfun
####### BEGIN HIDDEN LIST
   _pexports,
   diffeqtable,
   `diffeqtorec/doit`,
   formatdiffeq,
   formatrec,
   `goodinitvalues/diffeq`,
   `goodinitvalues/rec`,
   makerec,
   maxdegcoeff,
   maxordereqn,
   maxdegeqn,
   mindegcoeff,
   mindegeqn,
   minordereqn,
   optionsgf,
   version;
####### END HIDDEN LIST

global
   `type/gfun/free`,
   `type/gfun/has2diffeqs2`,
   `type/gfun/has2diffeqs3`,
   `type/gfun/identity`,
   `type/gfun/initeq`,
   `gfun/rectoproc/symbol`;

local
   GFUN_HIDDEN,
   NUMGFUN_SETUP,
   NUMGFUN_CLEANUP,
   `algeqtoseries/doit`,
   `algeqtoseries/prettyprint`,
   algfuntoalgeq2,
   `algfuntoalgeq/formpoly`,
   borelinvborel,
   cheapgausselim,
   expintalg,
   firstnonzero,
   formatpoleq,
   funtodiffeq,
   getname,
   guessandcheck,
   indicialeq,
   infsolvepoly,
   `infsolvepoly/found`,
   `infsolvepoly/isroot`,
   inicond,
   inifromseries,
   isholonomic,
   lindep,
   listprimpart,
   listtoseriestable,
   `l2r/l2r`,
   `l2h/l2h`,
   makediffeq,
   maxindex,
   minindex,
   mygcdex  ,
   myisolve,
   nbinicond,
   powcompose,
   powcomposesimple,
   powcomposesimpledoit,
   powdivide,
   powrevert,
   powtruncate,
   pprimeknowingp,
   `rectodiffeq/doit`,
   `rectohomrec/doit`,
   `rectoproc/checkcond`,
   `gfun/rectoproc/binsplit`,           # defined in NumGfun
   `rectoproc/binsplitparameters`,      # defined in NumGfun
   `gfun_pade2/exmin`,
   `gfun_pade2/doit`,
   `ratpolytocoeff/elmt`,
   `ratpolytocoeff/poly`,
   rectohomrecbis,
   `s2d/s2d`,
   `s2a/s2a`,
   systomatrix,
   typecheck,
   CheckName,
   `@@D`,		# to compute @@(D,i)(y)(x) without calling D
	# variables from ratinterp
finddiffeq,
findalgeq,
rationalinterpolation,
findequation,
findequationgivenorder,
findequationgivenorderratpoly,
findequationgivenorderrational,
do_reduce,
findequationgivenordermodp,
degmat,
degvect,
interpmat,
interpvect,
matmultmodp,
vectmultmodp,
matinterpmodp,
matinterpmodpearlyabort,
initmat,
tryearly,
checkresultnormal,
checkresult
   ;

_pexports:=proc() [op({exports(gfun)} minus GFUN_HIDDEN)] end:
GFUN_HIDDEN:={
    ':-_pexports',
    ':-diffeqtable',
    ':-`diffeqtorec/doit`',
    ':-formatdiffeq',
    ':-formatrec',
    ':-`goodinitvalues/diffeq`',
    ':-`goodinitvalues/rec`',
    ':-makerec',
    #':-maxdegcoeff',
    ':-maxordereqn',
    #':-maxdegeqn',
    #':-mindegcoeff',
    #':-mindegeqn',
    ':-minordereqn',
    ':-optionsgf',
    ':-version'
}:

######################### Parameters ##############################

# The minor number is meant to be a two-digit number, so the
# floating-point value is meant to be incremented by .01 at
# each new version.  It is printed by the format "%.2f".
   version := 3.53:
   optionsgf := ['ogf','egf']:
   maxordereqn := infinity: # default 3rd order
   minordereqn := 1: # default 1st order
## The following ones are now obsolete
#   maxdegcoeff := 4: # default degree 4 coefficients
#   mindegcoeff := 0: # default constant coefficients
   #maxdegeqn   := 3: # default maximum 3rd degree
   #mindegeqn   := 2: # default minimum 2nd degree

############# Starting with version 2.78, rectoproc has its own file ######
$include <rectoproc.mm>

######################### The pade2 package ##########################
# This should not be there. It just simplifies the distribution for this
# version.
## -*-Maple-*-
##
##    Title:   pade2 based on pade2
##    Created: Sep 1993
##    Author:  Harm Derksen <hderksen@sci.kun.nl>
##
## Description: pade-hermite approximants.
##
##    Modified: Oct 1993
##    Author:  Bruno Salvy <Bruno.Salvy@inria.fr>
##    Modification: rewritten for efficiency
##
##    June 94. Added the option 'easy'. BS.
##    Feb. 96. Improved efficiency. BS.

# This does the interface
   pade2:=proc(functionlist::list(algebraic),
                    point::{name,name=algebraic},
                    accuracy::{integer,list(nonnegint)})
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local x, a, n, l, i, acc, m, result;
      if type(point,'`=`') then a:=op(2,point); x:=op(1,point)
      else a:=0; x:=point end if;
      n:=nops(functionlist);
      if type(accuracy,'list') then
         m:=max(op(accuracy));
         l:=[seq(m-i,i=accuracy)];
         acc:=convert(accuracy,`+`)+n-1
      else
         l:=[0$n];
         acc:=accuracy
      end if;
      result:=`gfun_pade2/doit`(map(taylor,subs(x=x+a,functionlist),x,acc),
                                x,l,acc-1); # it has to be taylor and not series
      if result=FAIL then FAIL else subs(x=x-a,result) end if
   end proc: # pade2

#  gfun_pade2/exmin
#  Extended minimum.
# Input: a list, a boolean function, and an optional name.
# Output: the minimum of the list according to the order
# the name being assigned the index of the first occurrence of
# the minimum in the list.
#
# No attempt at efficiency has been made, since this should really be in
# the kernel, with sort.
   `gfun_pade2/exmin`:=proc(l,order,aux)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local res;
      res:=op(1,sort(l,order));
      if nargs=3 then member(res,l,aux) end if;
      res
   end proc: # `gfun_pade2/exmin`

# `gfun_pade2/doit`
# Input: a list of series in the variable x, the variable x, a list of degree
#  bounds and a bound on the number of iterations. This latter bound
#  should be at most the order of the series.
# Output: a list of polynomial coefficients for the series, such that
#  the scalar product of this list with the input has zero Taylor
#  coefficients up to a large order.
   `gfun_pade2/doit`:=proc(ll,x,degs,nbiter)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local y, i, L, n, j, l, ind, l2, pivot, vars, leadcoeff, k, normalize, lk,
      rationalcoeff, tt, finished, dmin, deg, lterm, jj, lterm2, inds,
      polycoeff, ll2, dmin2, ind2, co, cco, cco1, cco2, cco3;
      n:=nops(ll);
      vars:=[seq(y[i],i=1..n),x];
      inds:=indets(ll) minus {x};
      if inds={} then
         normalize:=x->x;
         rationalcoeff:=type({op(map(op,ll))} minus {O(1)},'set'('rational'));
         polycoeff:=false  # polycoeff means non-rational but polynomial
      else
         normalize:=normal;
         rationalcoeff:=false;
         polycoeff:=type({op(map(op,ll))} minus {O(1)},
                         'set'('polynom'('rational',inds)))
      end if;
      for i to n do
         L[i]:=series(y[i]*x^degs[i],x,infinity);
         l[i]:=ll[i]; co[i]:=1
      end do;
      finished:=false;
      for j from 0 to nbiter while not finished do
         userinfo(3,'gfun_pade2',`iteration number`,j);
         dmin:=infinity;
         # Select the pivot
         for i to n do
            leadcoeff[i]:=normalize(coeff(l[i],x,j));
            if leadcoeff[i]<>0 then
               l2[i]:=i;
               deg:=op(nops(L[i]),L[i]);
               if deg>dmin then next
               elif deg<dmin then
                  dmin:=deg; lterm:=op(nops(L[i])-1,L[i]);
                  dmin2:=length(l[i]); ind2:=i
               else
                  lterm2:=op(nops(L[i])-1,L[i]);
                  if length(l[i])<dmin2 then
                  	ind2:=i; dmin2:=length(l[i]) end if;
                  for jj to n while
                  has(lterm,y[jj]) and has(lterm2,y[jj]) do end do;
                  if jj<=n and not has(lterm,y[jj]) or
                  jj>n and leadcoeff[i]<>O(1) and
                  length(leadcoeff[i])>=length(leadcoeff[ind]) then next
                  else lterm:=lterm2 end if
               end if
            else
               l2[i]:=NULL;
               if coeff(l[i],x,j)=0 then next
               else # normalizing did it
                  l[i]:=map(normalize,l[i]);
                  if l[i]<>0 and op(1,l[i])<>O(1) then next end if
               end if
            end if;
            ind:=i;
            pivot:=leadcoeff[ind];
            if pivot=O(1) then break end if
         end do;
         if pivot=O(1) then k:=ind; break end if;
         if dmin=infinity then next end if;
         ll2:=[seq(l2[i],i=1..n)];
         if rationalcoeff and pivot<>1 and pivot<>-1 then
            tt:=abs(icontent(add(leadcoeff[i]*y[i],i=ll2)));
            if tt<>1 then
               pivot:=pivot/tt;
               for i in ll2 do leadcoeff[i]:=leadcoeff[i]/tt end do
            end if
         elif polycoeff then
            tt:=content(add(leadcoeff[i]*y[i],i=ll2),vars);
            if tt<>1 then
               divide(pivot,tt,'pivot');
               for i in ll2 do
               	divide(leadcoeff[i],tt,evaln(leadcoeff[i])) end do
            end if
         end if;
         tt:=l[ind2];
         for i in ll2 do
            if i<>ind then
               l[i]:= map(normalize, # this was commented out and killed
               # the timings for
               # seriestodiffeq(series(hypergeom([a,b],[c],x),x,7),y(x),[ogf])
               series(pivot*l[i]-leadcoeff[i]*l[ind],x,infinity));
               if l[i]=0 or op(1,l[i])=O(1) then
                  L[i]:=map(normalize,series(pivot*co[i]*L[i]
                               -leadcoeff[i]*co[ind]*L[ind],x,infinity));
                  finished:=true
               elif op(2,l[i])=j then l[i]:=subsop(1=0,l[i])
               end if
            end if
         end do;
         if finished then # select the smallest one
            dmin:=infinity;
            for i in ll2 do
               if (l[i]=0 or op(1,l[i])=O(1)) and length(L[i])<dmin then
                  dmin:=length(L[i]); k:=i
               end if
            end do;
            break
         end if;
         l[ind]:=series(x*tt,x,infinity);
         tt:=L[ind2]; cco:=co[ind2];
         for i in ll2 do
            if i<>ind then
               if rationalcoeff then
                  cco2:=pivot*co[i];
                  cco3:=co[ind]*numer(leadcoeff[i]);
                  cco1:=igcd(cco2,cco3);
                  cco2:=cco2/cco1; cco3:=co[ind]*leadcoeff[i]/cco1;
                  L[i]:=series(cco2*L[i]-cco3*L[ind],x,infinity);
                  cco2:=numer(icontent(add(icontent(op(2*jj-1,L[i]))*y[jj],
                                           jj=1..iquo(nops(L[i]),2))));
                  if cco2<>1 and cco2<>-1 then
                     L[i]:=series(L[i]/cco2,x,infinity) end if;
                  co[i]:=cco1*cco2
               elif polycoeff then
                  cco1:=gcd(pivot*co[i],leadcoeff[i]*co[ind],'cco2','cco3');
                  L[i]:=map(normal,series(cco2*L[i]-cco3*L[ind],x,infinity));
                  cco2:=content(convert(L[i],polynom),vars,'cco3');
                  if cco2<>1 and cco2<>-1 then
                     L[i]:=series(cco3,x,infinity); co[i]:=cco1*cco2
                  else co[i]:=cco1
                  end if
               else L[i]:=map(normalize,
                              series(pivot*L[i]-leadcoeff[i]*L[ind],x,infinity))
               end if end if
         end do;
         L[ind]:=series(x*tt,x,infinity); co[ind]:=cco;
         userinfo(5,'gfun_pade2',`current list`,lprint([seq(L[i],i=1..n)]));
      end do;
      if j=nbiter+1 and not finished then
         `gfun_pade2/exmin`([seq(`if`(l[i]<>0,op(2,l[i]),infinity),i=1..n)],
         	numeric,'k')
      end if;
      lk:=collect(convert(L[k],polynom),vars,'distributed',normalize);
      if rationalcoeff then divide(lk,icontent(lk),'lk')
      elif polycoeff then divide(lk,content(lk,vars),'lk') end if;
	  # this was map(expand,.) but was too slow with parameters
      listprimpart(map(collect,[seq(coeff(lk,y[i])*x^(-degs[i]),i=1..n)],x))
   end proc: # `gfun_pade2/doit`

######################### Type Checking ##############################

   `type/gfun/identity`:=proc(x) type(x,'`=`') and op(1,x)=op(2,x) end proc:
   `type/gfun/free`:=proc(x,y) not has(x,y) end proc:

# type(y(0),initeq(y))     -> true
# type(D(y)(0),initeq(y))  -> true
# type((D@@k)(y)(0),initeq(y))   -> true
# otherwise       -> false
   `type/gfun/initeq` := proc(expr,y)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local f;
      if not type(expr,'function'(0)) then
         false
      else
         f := op(0,expr);
         f=y or f='D(y)' or (type(f,'function'('identical'(y)))) and
         type(op(0,f),'`@@`'('identical'(D),'integer'))
      end if
   end proc: # `type/gfun/initeq`

# This procedure avoids several type checks of the same expression.
# Besides, it handles the defaults.
   typecheck:=proc (n)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local i;
      if n=1 then      # l, x, <met>
         if nargs>2 and type([args[2..3]],['list','name']) then
            if nargs=3 then
            	return 'stamped',args[2..3],op(1,optionsgf)
            elif nargs=4 and type(listtoseriestable[args[4]],'procedure')
            then return 'stamped',args[2..4]
            elif nargs>4 then error `too many arguments`
            else error `invalid argument`,args[4]
            end if
         elif nargs<3 then error `too few parameters`
         elif not type(args[2],'list') then error `not a list`,args[2]
         elif not type(args[3],'name') then error `not a name`,args[3]
         else error `invalid arguments`
         end if
      elif n=2 then # l, y(x), <[met]>
         if nargs>2 and type([args[2..3]],['list','function'('name')]) then
            if nargs=3 then return
            	'stamped',args[2..3],optionsgf
            elif nargs=4 and type(args[4],'list') and
            type([seq(listtoseriestable[i],i=args[4])],
                 'list'('procedure')) then return 'stamped',args[2..4]
            elif nargs>4 then error `too many arguments`
            else error `invalid argument`,args[4]
            end if
         elif nargs<3 then error `too few arguments`
         elif not type(args[2],'list') then error `not a list`,args[2]
         elif not type(args[3],'function'('name')) then
            error `invalid unknown function`,args[3]
         else error `invalid arguments`
         end if
      elif n=3 then # l, x, [<met>]
         if nargs>2 and type([args[2..3]],['list','name']) then
            if nargs=3 then
            	return 'stamped',args[2..3],optionsgf
            elif nargs=4 and type(args[4],'list') and
            type([seq(listtoseriestable[i],i=args[4])],
                 'list'('procedure')) then return 'stamped',args[2..4]
            elif nargs>4 then error `too many arguments`
            elif nargs=4 then error `invalid argument`,args[4]
            end if
         elif nargs<3 then error `too few parameters`
         elif not type(args[2],'list') then error `not a list`,args[2]
         elif not type(args[3],'name') then error `not a name`,args[3]
         else error `invalid argments`
         end if
      elif n=4 then # s, x, y
         if nargs=4 and type([args[2..4]],['series','name','name']) then
            return 'stamped',args[2..4]
         elif nargs<>4 then error `wrong number of arguments`
         elif not type(args[2],'series') then error `not a series`,args[2]
         elif not type(args[3],'name') then error `not a name`,args[3]
         elif not type(args[4],'name') then error `not a name`,args[4]
         else error `invalid arguments`
         end if
      elif n=5 then # l, y(x)
         if nargs=3 and type([args[2..3]],['list','function'('name')]) then
            return 'stamped',args[2..3]
         elif nargs<>3 then error `wrong number of arguments`
         elif not type(args[2],'list') then error `not a list`,args[2]
         elif not type(args[3],'function'('name')) then
            error `invalid unknown function`,args[3]
         else error `invalid arguments`
         end if
      elif n=6 then # s, y(x), <[met]>
         if nargs>2 and type([args[2..3]],['series','function'('name')]) then
            if nargs=3 then return
            	'stamped',args[2..3],optionsgf
            elif nargs=4 and type(args[4],'list') and
            type([seq(listtoseriestable[i],i=args[4])],
                 'list'('procedure')) then return 'stamped',args[2..4]
            elif nargs>4 then error `too many arguments`
            else error `invalid argument`,args[4]
            end if
         elif nargs<3 then error `too few arguments`
         elif not type(args[2],'series') then error `not a series`,args[2]
         elif not type(args[3],'function'('name')) then
            error `invalid unknown function`,args[3]
         else error `invalid arguments`
         end if
      elif n=7 then # s, <[met]>
         if nargs>1 and type(args[2],'series') then
            if nargs=2 then return
            	'stamped',args[2],optionsgf
            elif nargs=3 and type(args[3],'list') and
            type([seq(listtoseriestable[i],i=args[3])],
                 'list'('procedure')) then return 'stamped',args[2..3]
            elif nargs>3 then error `too many arguments`
            else error `invalid argument`,args[3]
            end if
         elif nargs=1 then error `too few arguments`
         elif not type(args[2],'series') then error `not a series`,args[2]
         else error `invalid arguments`
         end if
      elif n=8 then # s, <met>
         if nargs>1 and type(args[2],'series') then
            if nargs=2 then return 'stamped',args[2],'ogf'
            elif nargs=3 and type(listtoseriestable[args[3]],'procedure')
            then return 'stamped',args[2..3]
            elif nargs>3 then error `too many arguments`
            else error `invalid argument`,args[3]
            end if
         elif nargs=1 then error `too few arguments`
         elif not type(args[2],'series') then error `not a series`,args[2]
         else error `invalid arguments`
         end if
      elif n=9 then # l, <met>
         if nargs>1 and type(args[2],'list') then
            if nargs=2 then return 'stamped',args[2],'ogf'
            elif nargs=3 and type(listtoseriestable[args[3]],'procedure')
            then return 'stamped',args[2..3]
            elif nargs>3 then error `too many arguments`
            else error `invalid argument`,args[3]
            end if
         elif nargs=1 then error `too few arguments`
         elif not type(args[2],'list') then error `not a list`,args[2]
         else error `invalid arguments`
         end if
      elif n=10 then   # recurrence, function(name), function(name)
         if nargs>4 then error `too many arguments`
         elif nargs<4 then error `too few arguments`
         elif not type([args[3..4]],
         	['function'('name'),'function'('name')]) then
            error `invalid arguments`
         end if
      else error `should not happen`
      end if;
   end proc: # typecheck

   getname:=proc(yofz::function(name), y, z)
      y:=op(0,yofz);
      if type(y,'procedure') then error `not an unassigned name`,y end if;
      z:=op(yofz)
   end proc:

################## Modifications of existing Maple code ############


# this procedure does not do exactly the same as gcdex, but is much faster
# it returns g' and assign s,t such that s*a+t*b=g'
# and g'=lambda gcd(a,b) with lambda a rational fraction independent of y
###################################################################
##########  ONE CAN PROBABLY IMPROVE IT MORE   ####################
###################################################################
   mygcdex := proc(a,b,y,s,t)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local q,r,u,v,du,dv,alpha,beta,oldalpha,oldbeta,tt;
      Order:=infinity; # We are working on polynomials
      u:=series(a,y); v:=series(b,y);
      if u=0 then b
      elif v=0 then a
      else
         oldalpha:=1; oldbeta:=0;
         alpha:=0; beta:=1;
         # u = oldalpha*a + oldbeta*b
         # v = alpha*a + beta*b
         du:=op(nops(u),u);
         dv:=op(nops(v),v);
         do
            if du<dv then
               tt:=dv; dv:=du; du:=tt;
               tt:=v; v:=u; u:=tt;
               if nargs>3 then tt:=alpha; alpha:=oldalpha; oldalpha:=tt end if;
               if nargs>4 then tt:=beta; beta:=oldbeta; oldbeta:=tt end if;
            end if;
            userinfo(3,'gfun',`degrees of the polynomials`,du,dv);
            userinfo(5,'gfun','polynomials',u,v);
            if du=0 and dv=0 then break end if;
            q:=op(nops(u)-1,u)/op(nops(v)-1,v)*y^(du-dv);
            r:=normal(series(u-q*v,y));
            if r=0 then break end if;
            if nargs>3 then
            	tt:=oldalpha-q*alpha; oldalpha:=alpha; alpha:=tt end if;
            if nargs>4 then
            	tt:=oldbeta-q*beta; oldbeta:=beta; beta:=tt end if;
            u:=v;du:=dv;
            v:=r;dv:=op(nops(v),v)
         end do;
         ## These rem's are much too expensive on large examples
         if dv=0 then
#      if nargs>3 then s:=rem(collect(alpha/coeff(v,y,0),y,normal),b,y) end if;
#      if nargs>4 then t:=rem(collect(beta/coeff(v,y,0),y,normal),a,y) end if;
            if nargs>3 then
               s:=convert(normal(series(alpha/coeff(v,y,0),y)),polynom) end if;
            if nargs>4 then
               t:=convert(normal(series(beta/coeff(v,y,0),y)),polynom) end if;
            1
         else
#      if nargs>3 then s:=rem(alpha,b,y) end if;
#      if nargs>4 then t:=rem(beta,a,y) end if;
            if nargs>3 then s:=convert(normal(series(alpha,y)),polynom) end if;
            if nargs>4 then t:=convert(normal(series(beta,y)),polynom) end if;
            v
         end if
      end if
   end proc: # mygcdex

# This is really needed because MapleV's isolve is terrible
# eqn is a polynomial in n
   myisolve:=proc (eqn, n)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local sol, i, d, f, g;
      if not has(eqn,n) then {}
         # These two lines are due to M. Monagan:
      elif type(eqn,'polynom'('rational',n)) then
         select(type,{seq(i[1],i=roots(eqn))},'integer')
      elif type(eqn,'`*`') then
         `union`(op(map(myisolve,[op(eqn)],n)))
      else
         f:=collect(eqn,n);
         g:=primpart(f,n);
         if expand(g-f)<>0 then return myisolve(g,n) end if;
         d:=degree(f,n);
         if d=1 then
            f:=-coeff(f,n,0)/coeff(f,n,1);
            if type(f,'integer') then {f}
            elif not has(f,'RootOf') and not has(f,'`&RootOf`') then {}
            else f:=evala(Normal(f));
               if type(f,'integer') then {f} else {} end if
            end if
         elif d=2 then
            f:=[seq(coeff(f,n,i),i=0..2)];
            d:=sqrt(op(2,f)^2-4*op(3,f)*op(1,f));
            d:=simplify(d,'radical','symbolic'); # otherwise z^2-(c-1)*z is missed
            select(type,map(normal,
                 {(d-op(2,f))/2/op(3,f),-(d+op(2,f))/2/op(3,f)}),'integer')
         elif type(eqn,'`*`') then `union`(op(map(myisolve,[op(eqn)],n)))
         else
            try
               sol:=isolve(expand(eqn),n);
            catch :
               return {};
            end try;
            {seq(op(2,i),i=select(type,map(op,[sol]),
                                       'identical'(n)='integer'))}
         end if
      end if
   end proc: # myisolve

######################### Various Utilities ###################

# index of the first integer n0 such that the polynomial is not 0 for n0+1,...,infinity
# If nmax is given, it's the first such integer for the interval n0+1,...,nmax-1
   firstnonzero:=proc(pol,n,nmax)
   local sols;
   	sols:=myisolve(pol,n);
   	if nargs=3 then sols:=select(proc(t) evalb(t<nmax) end,sols) fi;
   	max(-1,op(sols))+1
   end proc:

# returns the smallest i such that u(n+i) appears in a recurrence
   minindex := proc(rec,u,n)
      min(op(map(op,indets(rec,'specfunc'('linear'(n),u)))))-n
   end proc: # minindex

# returns the largest i such that u(n+i) appears in a recurrence
   maxindex := proc(rec,u,n)
      max(op(map(op,indets(rec,'specfunc'('linear'(n),u)))))-n
   end proc: # maxindex

# systomatrix
#  Input: a system of homogeneous linear equations and a list of variables V,
#    and the name of a vector B.
#  Output: a matrix A such that the system is equivalent to A.V=B.
# Almost like linalg[genmatrix], but more suitable for our purpose.
# Also, if sys is a list instead of a set, then the order will be preserved.
## In Maple12, LinearSolve is buggy when floats and sparse are used.
## Example:
## A:=Matrix([[1.]],storage=sparse):
## b:=Vector([1.]):
## LinearAlgebra:-LinearSolve(A,b); ---> infinite time

   systomatrix:=proc (sys, V, B)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local a, i, j, eqn, t, ind, lco;
      if sys={} or sys=[] then
         B:=Array(1..1,[0]);
         Array(1..1,1..nops(V),[[0$nops(V)]])
      else
#         zero:=[seq(i=0,i=V)];
         a:=Array(1..nops(sys),1..nops(V),`if`(not hastype(sys,float),'storage'='sparse',NULL));
         B:=Array(1..nops(sys),'storage'='sparse');
         for i to nops(V) do ind[V[i]]:=i end do;
         for i to nops(sys) do
            eqn:=op(i,sys);
            if type(eqn,'`=`') then eqn:=op(1,eqn)-op(2,eqn) end if;
##     eqn:=collect(eqn,V,'distributed');
#      eqn:=expand(eqn);
            lco:=[coeffs(eqn,V,'t')];
            t:=[t];
            for j to nops(t) do
               if t[j]<>1 then a[i,ind[t[j]]]:=lco[j] else B[i]:=-lco[j] end if
            end do;
#      for j to nops(V) do
#     c:=coeff(eqn,V[j]);
#     if c<>0 then a[i,j]:=c end if
#      od;
#      B[i]:=-subs(zero,eqn) end do;
         end do;
         Matrix(a)
      end if
   end proc: # systomatrix

#listprimpart
# Input: a list of polynomials and a variable
# Output: the list where common factors have been removed.
# Argument sequence: l::list(polynom),var::anything, optcontent::name
# Output format: list(polynom)
# Description: the procedure removes the content of the
# whole list of polynomials. 'var' is actually unused, but
# it is kept for compatibility. 'optcontent' (optional) is
# assigned the content (added 10/02/02).
   listprimpart:=proc (l, var, optcontent)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local i, T, q, cont, n;
      if hastype(l,'float') then l
      else
	 n:=nops(l);
         cont:=content(add(l[i]*T^(i-1),i=1..n),T,'q');
	 if nargs=3 then optcontent:=cont fi;
         [seq(coeff(q,T,i),i=0..n-1)]
      end if
   end proc: # listprimpart

# This is used to find a linear dependency.
# The result is the last element of the last line which is not 0.
# The pivoting must not be done as gausselim does it.
# This is a fraction free gausselim using the input matrix for the intermediate
# computations. Also, no type checking is performed.
   cheapgausselim:=proc (A, nlin, ncol)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local lcols, c, k, i, u, j, piv;
      lcols:=[$1..ncol];
      for c to nlin do
         for k in lcols while normal(A[c,k])=0 do A[c,k]:=0 end do;
         if k=ncol then return A[c,ncol] end if;
         piv:=A[c,k];
         for i from c+1 to nlin do
            if A[i,k]=0 then next else u:=A[i,k] end if;
#      for j in lcols do A[i,j]:=piv*A[i,j]-u*A[c,j] end do
            for j in lcols do A[i,j]:=normal(piv*A[i,j]-u*A[c,j]) end do
         end do;
         userinfo(5,'gfun',`line `,c,` eliminated`);
         # this line adds some speedup for very large matrices:
         for i in lcols do A[c,i]:=0 end do;
         userinfo(6,'rsolve',`remaining matrix `,print(op(A)));
         lcols:=subs(k=NULL,lcols)
      end do;
      FAIL
   end proc: # cheapgausselim

#lindep
# Input:  a Vector u = [u1 , ... , uk] and a matrix A = Matrix(1..k,1..l)
#    such that u[i] = sum(A[i,j] e[j]) for some e[j].
#    The coefficients are rational functions in x.
# Output: a linear dependency relation between the u[i] if there is one,
#    FAIL otherwise
#    In other words, we are looking for a vector in the kernel of transpose(A).
   lindep := proc(A, u, x)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local k, i, l, v, rel, unk, j;
      k := nops(u);
      if LinearAlgebra:-RowDimension(A)<>k then
      	error `incorrect number of rows`,op(A) end if;
      l := LinearAlgebra:-ColumnDimension(A);
      userinfo(2,'gfun',`looking for a linear dependency in a`,
      	k,' x ',l,'matrix');
      unk:=[seq(v[i],i=1..k)];
      rel := cheapgausselim(
         Matrix([A,Vector(unk)]),k,l+1);
      if rel=FAIL then FAIL
      else
         rel:=primpart(numer(add(normal(subs([i=1,seq(j=0,
              j=subs(i=NULL,unk))],rel))*i,i=unk)),unk);
		 userinfo(4,'gfun',"degree of relation",degree(rel,x));
         add(subs([i=1,seq(j=0,j=subs(i=NULL,unk))],rel)*u[op(i)],i=unk)
      end if
   end proc: # lindep

lindep := proc(A, u, x)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
local k, i, B, dim, tmp;
	k := nops(u);
   	if LinearAlgebra:-RowDimension(A)<>k then error `incorrect number of rows`,op(A) end if;
	## trust LinearAlgebra
  	B:=Matrix([op(LinearAlgebra[NullSpace](LinearAlgebra[Transpose](A)))]);
	dim:=LinearAlgebra:-ColumnDimension(B);
  	if dim=0 then return FAIL fi;
	while dim>1 do # find operator of minimal order
		B:=LinearAlgebra:-Multiply(B,Matrix([op(LinearAlgebra:-NullSpace(B[1-dim..-1,1..-1]))]));
		dim:=LinearAlgebra:-ColumnDimension(B);
	od;
	B:=map(normal,B);
	userinfo(3,'gfun',"degree of the coefficients", degree(denom(B[1,1])));
#		return primpart(numer(add(B[i,1]*u[i],i=1..k)),x)
	B:=primpart(numer(add(B[i,1]*tmp^i,i=1..k)),tmp);
	add(coeff(B,tmp,i)*u[i],i=1..k)
end proc: # lindep


#formatdiffeq
# Input:  a list [diffeq,y(x)] containing a linear differential equation
#       with polynomial coefficients and its unknown
#    (optional) Y and X
#        names to be assigned the unknown function and its variable
#    (optional) iniconds to be assigned the initial conditions
#    (optional) name to be assigned the content of the polynomial coefficients
#        of the differential equation when it is normalized.
# Output: a list of polynomials in x [u(x),p_0(x),...,p_d(x)] meaning
#                     (d)
#       eqn = p_0(x) y(x) + ... + p_d(x) y     (x) + u(x)
#
#   This is where the type checking is done.
#
   formatdiffeq:=proc (l,Y,X,iniconds, den)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local r, y, x, i, difford, j, lvar, Z, cont, locden;
      if nops(l)<>2 then error `wrong number of arguments`,op(l) end if;
      getname(op(2,l),y,x);
      if nargs>1 then X:=x; Y:=y end if;
      if type(op(1,l),'set')  then
         r:=selectremove(has,op(1,l),x);
         if nops(r[1])>1 then
         	error `invalid differential equation`,op(l) end if;
         if nops(r[1])=0 then
            error `the unknown variable does not appear in the equation` end if;
         if nargs>3 then iniconds:=r[2] end if;
         r:=op(r[1])
      else r:=op(1,l); if nargs>3 then iniconds:={} end if end if;
      if nargs>3 and has(iniconds,'D') then D(Y):='D(Y)' end if;
      if type(r,'`=`') then r:=op(1,r)-op(2,r) end if;
      if has(r,'D') then r:=convert(r,'diff') end if;
      if indets(r,'specfunc'('anything',y))<>{y(x)} then
         error `invalid differential equation`,op(l) end if;
      if nargs<5 then r:=expand(numer(normal(r)))
      else
	r:=normal(r);
	cont:=denom(r);
	r:=expand(numer(r))
      fi;
      lvar:=select(has,indets(r,'specfunc'('anything',diff)),y);
      for difford from 0 while lvar<>{} do
         lvar:=subs(diff(y(x),x)=y(x),lvar) minus {y(x)} end do;
      if not type(r,'linear'([y(x),seq(diff(y(x),x$i),i=1..difford)])) then
         error r,`is not a linear differential equation in`,y(x) end if;
      r:=subs([y(x)=Z,seq(diff(y(x),x$j)=Z^(j+1),j=1..difford)],r);
      r:=[subs(Z=0,r),seq(coeff(r,Z,j+1),j=0..difford)];
      if not type(r,'list'('polynom'('anything',x))) then
         error `non-polynomial coefficients`,op(l) end if;
      if nargs<5 then listprimpart(r,x)
      else
	r:=listprimpart(r,x,'locden');
	den:=locden*cont;
	r
      fi
   end proc: # formatdiffeq

# makediffeq
# Input:  a differential equation in the format returned by formatdiffeq
#    the variables y and x, meaning the unknown function is y(x)
#    (optional) a set of initial conditions
# Output: the corresponding differential equation, which is in a set if
#    there are initial conditions.
   makediffeq:=proc (deq, y, x, ini)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local r, i;
      r:=add(deq[i+2]*diff(y(x),[x$i]),i=0..nops(deq)-2)+deq[1];
      if nargs=3 or ini={} then r
      else {r,op(ini)} end if
   end proc: # makediffeq

#formatrec
# Input:  a list [rec,u(n)] containing a linear recurrence with
#       polynomial coefficients and its unknown
#    u, n names to be assigned the unknown function and variable
#    (optional) iniconds to be assigned the initial conditions
#    (optional) name to be assigned then content of the polynomial coefficients
#               of the recurrence when it is normalized.
# Ouput:  a list of polynomials in n: [b(n),p_0(n),...,p_d(n)] meaning
#
#        rec=p_0(n)u(n)+...+p_d(n)u(n+d)+b(n)
#
#  This is where the type checking is done.
#
### History:
# 10/02/02: the old fifth argument was unused and it is replaced by
# 'den', which yields the content of the polynomial coefficients
# of the recurrence when it is normalized.
#
   formatrec:=proc (l, u, n, iniconds, den)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local r, i, U, N, mi, ma, locini, j, locden;
      if nops(l)<2 or nops(l)>5 then
         error `wrong number of arguments`,op(l) end if;
      getname(op(2,l),U,N);
      if nargs>1 then n:=N; u:=U end if;
      if type(op(1,l),'set') then
         r:=selectremove(has,op(1,l),N);
         if nops(r[1])>1 then error `invalid recurrence`,op(l) end if;
         if r[1]={} then error `empty recurrence` end if;
         if nargs>3 then locini:=r[2];
            if not type(locini,'set'(U('integer')='anything')) then
               error `invalid initial conditions`,locini
            end if end if;
         r:=op(r[1])
      else r:=op(1,l); if nargs>3 then locini:={} end if
      end if;
      if type(r,'`=`') then r:=op(1,r)-op(2,r) end if;
      mi:=minindex(r,U,N); ma:=maxindex(r,U,N);
      r:=collect(r,[seq(U(N+i),i=mi..ma)],normal);
      if not type(r,'linear'([seq(U(N+i),i=mi..ma)])) then
         error `Not a linear recurrence`,op(l) end if;
      if mi<>0 then
         if nargs>3 then
            j:=-1;
            for i from 0 to min(mi,ma-1) do
               if not has(locini,U(i)) then
                  j:=j+1; locini:=locini union {U(i)=_C[j]}
               end if
            end do
         end if;
         # The following two lines are not valid for mi>0.
         # Example: (n+1)*u(n+1)+(n+2)*u(n+2) and rectodiffeq.
         if mi<0 then
            r:=subs(N=N-mi,r);
            ma:=ma-mi
         end if
      end if;
      if nargs>3 then iniconds:=locini end if;
      locden:=1;
      if has(map(denom,{op(r)}),N) then
	 r:=normal(r);
	 locden:=denom(r);
         r:=collect(numer(r),[seq(U(N+i),i=0..ma)],normal)
      end if;
      if nargs>4 then den:=locden fi;
      r:=[subs([seq(U(N+i)=0,i=0..ma)],r),seq(coeff(r,U(N+i),1),i=0..ma)];
      if has(r,U) or not type(r,'list'('polynom'('anything',N))) then
         error `invalid recurrence or unknown`,op(l) end if;
      # listprimpart should not be called here because the leading coefficient
      # might vanish at some point, which has an impact on the initial
      # conditions
      #  listprimpart(r,N)
      r
   end proc: # formatrec

# makerec
# Input:  a recurrence in the format returned by formatrec
#    the variables u and n, meaning the unknown sequence is u(n)
#    (optional) a set of initial conditions
# Output: the corresponding recurrence, which is in a set if there are
#    initial conditions.
   makerec:=proc (rec, u, n, ini)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local r, i;
      r:=add(rec[i+2]*u(n+i),i=0..nops(rec)-2)+rec[1];
      if nargs=3 or ini={} then r
      else {r,op(ini)} end if
   end proc: # makerec

# formatpoleq
# Input:  a list [p, y(z)] containing a polynomial in two variables and
#     an unknown function and possibly initial conditions.
#    y, z names to be assigned the unknown function and variable
#    (optional) iniconds to be assigned the initial conditions.
# Output: the polynomial, type checked and without its inital values
   formatpoleq:=proc (l, y, z, iniconds)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local Y, Z, P;
      if nops(l)<2 or nops(l)>4 then
         error `formatpoleq: wrong number of arguments`,op(l) end if;
      getname(op(2,l),Y,Z);
      if nargs>1 then y:=Y; z:=Z end if;
      if type(op(1,l),'`=`') then P:=op(1,op(1,l))-op(2,op(1,l))
      else P:=op(1,l) end if;
      if not type(P,'polynom'('anything',[Y,Z])) then
        # accept an alternate syntax:
        P:=subs(Y(Z)=Y,P);
        if not type(P,'polynom'('anything',[Y,Z])) then
      	error `invalid argument`,P fi
      end if;
      if nargs=4 then
         if nops(l)>2 then
            if type(op(3,l),'set') then iniconds:=op(3,l)
            else error `invalid argument`,op(3,l) end if;
         else iniconds:={} end if end if;
      collect(primpart(P,Y),Y)
   end proc: # formatpoleq


$include <ported/goodinitvalues_rec.mm>


# indicialeq
# Input: a differential equation in the format returned by formatdiffeq
#   the unknown function and its variable
#   a variable for the resulting polynomial
#   (optional) the shift of exponent
# Output: the indicial equation (a polynomial in the variable vanishing
#   for possible valuations of series solutions)
   indicialeq:=proc (deq, z, alpha, val)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local ldeg, i, v, res, j, dd;
      # this is because of the new convention for degree(0) in V.5
      dd:=map(proc(x) if x=0 then 1 else x end if end,deq);
      ldeg:=[seq(ldegree(dd[i],z)-i+2,i=2..nops(deq))];
      v:=min(op(ldeg));
      if nargs=4 then val:=v end if;
      for i to nops(ldeg) do
         if ldeg[i]=v then
            res[i]:=tcoeff(deq[i+1],z)*mul(alpha-j,j=0..i-2)
         else res[i]:=0 end if
      end do;
      collect(add(res[i],i=1..nops(ldeg)),alpha)
   end proc: # indicialeq

# nbinicond
# Input:  a differential equation in the format returned by formatdiffeq
#    the unknown function and its variable
# Output: the number of initial conditions necessary to specify a solution
#    an error if no series solution exist
#    this is mostly useful in the singular case
   nbinicond:=proc(deq,y,z)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local pol, val, res;
      pol:=indicialeq(deq,z,z,val);
	  res:=firstnonzero(pol,z)-1;
      if coeff(deq[1],z,res+val)=0 then res  # contains homogeneous case
      elif subs(z=0,deq[nops(deq)])=0 then 0
      else error `no valid initial conditions`
	  fi
   end proc: # nbinicond

# `goodinitvalues/diffeq`
# Input:  a differential equation in the format returned by formatdiffeq
#    the unknown function and its variable
#    (optional) some initial conditions
#    (optional) an integer p
# Output: a set of equalities (D@@k)(y)(0)=v_k, from which all the others
#     can be computed. This set is continued up to order p if p is given.
#     Also, a boolean telling whether 0 has been recognized
#    The result is an ERROR when no initial condition can be found,
#    except in the case when the origin is a singular point. Then it
#    returns {}.
   `goodinitvalues/diffeq`:=proc (deq, y, z, ini, p)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local u, init, i, sol, maxorder, inds, j, rej, k, sys, v, gb, termorder, goodinds, unkns;
      goodinds:=select(type,myisolve(indicialeq(deq,z,z),z),nonnegint);
      if goodinds={} then
	   if deq[1]=0 then return {},false fi;
	   maxorder:=degree(deq[1],z)
      else
	   goodinds:=sort([op(goodinds)]);
	   maxorder:=goodinds[-1]
      fi;
      if nargs=5 then maxorder:=max(maxorder,p) end if;
      if nargs>3 and type(ini,'set') then
         init:=ini;
         maxorder:=max(maxorder,seq(op(2,op(0,op(0,i))),
               i=indets(init,`gfun/initeq`(y)) minus {y(0),'D(y)(0)'}));
         if maxorder<=0 and has(init,'D(y)(0)') then maxorder:=1 end if
      else init:={} end if;
      u:=series(eval(makediffeq(deq,y,z),y(z)=subs(init,
		add(`@@D`(i,y,0)*z^i/i!,i=0..maxorder)+O(1)*z^(maxorder+1))),
              z,infinity);
	  # This is almost always correct, but broken by
	  # 	series(a*O(1)+t,t) --> a*O(1)+O(t)
      #u:={seq(coeff(u,z,i),i=0..maxorder)} minus {O(1)} union init;
      u:=remove(has,{seq(coeff(u,z,i),i=0..maxorder)},O(1)) union init;
      unkns:={seq(`@@D`(i,y,0),i=0..maxorder)};
      sol:=solve(u,unkns intersect indets(u,'function'(0)));
      if sol=NULL then  # it might be more singular than it looks
#         if ord<>-1 and subs(z=0,op(nops(deq),deq))<>0 then
            error `no valid initial conditions`
#         elif ord=-1 then {}
#         else init
#         end if
#      elif ord=-1 then {}
      else
#         dorej:=evalb(subs(z=0,deq[nops(deq)])<>0);
#         if dorej then sol:=subs(sol,[seq(`@@D`(i,y,0),i=0..ord)])
#         else sol:=subs(sol,[seq(`@@D`(i,y,0),i=0..maxorder)]) end if;
	 if nargs=5 then goodinds:=[$0..maxorder] fi;
	 sol:=subs(sol,[seq(`@@D`(i,y,0),i=goodinds)]);
         inds:=select(has,indets(sol,'function'),unkns);
         j:=max(op(map(op,indets([deq,sol],_C['anything']))));
         if j=-infinity then j:=-1 end if;
         for i in inds do
            if member(i,sol,'k') and (# not dorej or
               nops(select(has,sol,i))>1) then
               j:=j+1;
               sol:=subs(i=_C[j],sol);
            else
		goodinds:=subsop(k=NULL,goodinds);
		sol:=subsop(k=NULL,sol);
            end if
         end do;
         # clean the _C[] for initial conditions of the type
         # _C[1]+_C[2],_C[1]-_C[2]
         if not hastype(deq,_C['anything']) and
         hastype(remove(type,sol,'name'),_C['anything'])
         # limitations due to the cost of Groebner basis computation:
         and max(op(map(degree,sol,indets(sol,_C['anything']))))<3
         and not has(sol,'RootOf')
         # also it has to be a system of polynomials
         and type(sol,'list'('polynom'('rational',indets(sol,_C['anything']))))
         then
            sys:={seq(v[i]-sol[i],i=1..nops(sol))};
            inds:=indets(sys,_C['anything']);
            # find algebraic relations between the u[i]
            termorder:=lexdeg([op(inds)],[seq(v[i],i=1..nops(sol))]);
	    gb:=remove(hastype,Groebner:-Basis(sys,termorder),_C['anything']);
	 # workaround for a weakness in Groebner:-HilbertDimension:
	 #  Groebner:-HilbertDimension([],[a[i]$i=1..n]) is exponential in n
	 if gb=[] then sol:=[seq(v[i],i=1..nops(sol))] else
	    sol:=subs(solve({op(gb)},{seq(v[i],
	    	i=goodinds[Groebner:-HilbertDimension(gb,termorder)
	    		-nops(inds)..-1])}),[seq(v[i],i=1..nops(sol))])
	 fi;
               j:=-1;
               for i to nops(sol) do
                  if sol[i]=v[i] then
                     if nops(select(has,sol,sol[i]))=1 and i<=maxorder then
                        rej[i]:=i
                     else
                        rej[i]:=NULL;
                        j:=j+1;
                        sol:=subs(v[i]=_C[j],sol);
                     end if
                  end if
               end do;
            {seq(`@@D`(goodinds[i],y,0)=sol[i],i={$1..nops(sol)} minus
                 {seq(rej[i],i=1..nops(sol))})},false
         else
	    {seq(`@@D`(goodinds[i],y,0)=sol[i],i=1..nops(goodinds))},
		evalb(deq[1]=0 and {op(sol)}={0})
         end if
      end if;
   end proc: # `goodinitvalues/diffeq`

   # Input: expression, variable, number of desired terms, name, point
   # Output: {unkn(pt)=., D(unkn)(pt)=.,...,(D@@maxord)(unkn)(pt)=.},
   # 	where the values are obtained by series expansion of expr.
   # This will be easier when MultiSeries is there.
   inifromseries:=proc(expr,var,maxord,unkn,pt,ini)
   local s, i, j, h;
	if maxord=-1 then return {} fi; # this is used in poltodiffeq
	for i do
	    s := subs(ini,MultiSeries:-series(expr,var=pt,i*(maxord+1)));
	    # check for "series" in non-integer powers, or series that start with
	    # a negative exponent
	    if not type(s,'series') or not type(op(2,s),'nonnegint') then return {} fi;
	    # make sure we have enough terms - will be easier with MultiSeries
	    # h is the first exponent we can't use
	    if has(s, 'O') then h := op(nops(s),s) else	h := infinity fi;
	    if h > maxord then return {seq(`@@D`(j,unkn,pt)=j!*coeff(s,var-pt,j),j=0..maxord)} fi
	od
   end: # inifromseries


######################### Conversion Routines ###################

   seriestolist:=proc()
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local s, meth, l, x, i;
      if args[1]<>'stamped' then return seriestolist(typecheck(8,args)) end if;
      s:=args[2]; meth:=args[3]; x:=op(0,s);
      if op(nops(s)-1,s)=O(1) then l:=[seq(coeff(s,x,i),i=0..op(nops(s),s)-1)]
      else l:=[seq(coeff(s,x,i),i=0..op(nops(s),s))] end if;
      if meth='ogf' then l
      else seriestolist('stamped',listtoseries('stamped',l,x,meth),'ogf')
      end if
   end proc: # seriestolist

   listtolist:=proc()
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local x;
      if args[1]<>'stamped' then listtolist(typecheck(9,args))
      elif args[3]='ogf' then args[2]
      else seriestolist('stamped',
                        listtoseries('stamped',args[2],x,args[3]),'ogf')
      end if
   end proc: #listtolist

   seriestoseries:=proc ()
      if args[1]<>'stamped' then seriestoseries(typecheck(8,args))
      else listtoseries('stamped',seriestolist('stamped',args[2],'ogf'),
                        op(0,args[2]),args[3])
      end if
   end proc: # seriestoseries

   listtoseries:=proc ()
      if args[1]<>'stamped' then listtoseries(typecheck(1,args))
      else map(normal,listtoseriestable[args[4]](args[2],args[3])) end if
   end proc: # listtoseries

   listtoseriestable[egf]:=proc(l,x)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local i;
      series(add(op(i,l)*x^(i-1)/(i-1)!,i=1..nops(l))+O(x^(nops(l))),x,nops(l))
   end proc:

   listtoseriestable[Laplace]:=proc(l,x)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local i;
      series(add(op(i,l)*x^(i-1)*(i-1)!,i=1..nops(l))+O(x^(nops(l))),x,nops(l))
   end proc:
   listtoseriestable[:-Laplace]:=eval(listtoseriestable[Laplace]):

   listtoseriestable[ogf]:=proc(l,x)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local i;
      series(add(op(i,l)*x^(i-1),i=1..nops(l))+O(x^(nops(l))),x,nops(l))
   end proc: # `listtoseries/ogf`

	## For the next 2, it is necessary that L[1]=0 and L[2]<>0.

   listtoseriestable[revogf]:=proc(L,x)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local i, nl;
      nl:=nops(L);
      powrevert(series(add(L[i]*x^(i-1),i=1..nl)+O(x^nl),x,nl),x,nl-1)
   end proc: # `listtoseries/revogf`

   listtoseriestable[revegf]:=proc(L,x)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local i, nl;
      nl:=nops(L);
      powrevert(series(add(L[i+1]*x^i/i!,i=0..nl-1)+O(x^nl),x,nl),x,nl-1)
   end proc: # `listtoseries/revegf`

   listtoseriestable[lgdogf]:=proc(L,x)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local i, nl;
      nl:=nops(L);
      series(add(i*L[i+1]*x^(i-1),i=1..nl-1)/
             add(L[i]*x^(i-1),i=1..nl),x,nl-1)
   end proc: # `listtoseries/lgdogf`

   listtoseriestable[lgdegf]:=proc(L,x)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local i, nl;
      nl:=nops(L);
      series(add(L[i]*x^(i-2)/(i-2)!,i=2..nl)/
             add(L[i]*x^(i-1)/(i-1)!,i=1..nl),x,nl-1)
   end proc: # `listtoseries/lgdegf`

   listtodiffeq:=proc()
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local result, ex, methods, method, y, x, s, unkn, expr;
      if args[1]<>'stamped' then return listtodiffeq(typecheck(2,args)) end if;
      expr:=args[2];unkn:=args[3];methods:=args[4];
      y:=op(0,unkn);x:=op(unkn);ex:=expr;
      for method in methods do
         try
         	# Two methods that require a special treatment
         	if (method='revogf' or method='revegf') and ex[1]<>0 then
         		s:=listtoseries('stamped',[1,op(ex)],x,method)
         	else
	            s:=listtoseries('stamped',ex,x,method)
	        fi
         catch :
            next;
         end try;

         userinfo(3,'gfun',`Trying the `,method,s);
         result:=`s2d/s2d`(s,x,y);
         if result<>FAIL then
            userinfo(2,'gfun','The',method,'`seems to satisfy`',result);
            return [inicond(s,result,y,x),method]
         end if
      end do;
      FAIL
   end proc: # listtodiffeq

   seriestodiffeq:=proc ()
   local L;
        if args[1]<>'stamped' then seriestodiffeq(typecheck(6,args))
        else
#                listtodiffeq('stamped',seriestolist('stamped',args[2],'ogf'),args[3],args[4])

		L:=seriestolist('stamped',args[2],'ogf');
		# catch polynomials
                #moe:=Parameters('maxordereqn');
                #try
			if not has(args[2],O(1)) then
                                #Parameters('maxordereqn'=max(Parameters('minordereqn'),nops(L)-2));
				L:=[op(L),0$(max(10,nops(L)))]
			fi;
                        #res:=
			listtodiffeq('stamped',L,args[3],args[4]) ;
                #       Pamameters('maxordereqn'=moe)
                #catch: Pamameters('maxordereqn'=moe)
                #end try;
                #res
                #
        end if
   end proc: # seriestodiffeq

   listtoalgeq:=proc()
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local result, ex, methods, method, y, x, s, unkn, expr;
      if args[1]<>'stamped' then return listtoalgeq(typecheck(2,args)) end if;
      expr:=args[2];unkn:=args[3];methods:=args[4];
      y:=op(0,unkn);x:=op(unkn);ex:=expr;
      for method in methods do
         try
         	# Two methods that require a special treatment
         	if (method='revogf' or method='revegf') and ex[1]<>0 then
         		s:=listtoseries('stamped',[1,op(ex)],x,method)
         	else
	            s:=listtoseries('stamped',ex,x,method)
	        fi
         catch :
            next;
         end try;

         userinfo(3,'gfun',`Trying the `,method,s);
         result:=`s2a/s2a`(s,x,y);
         if result<>FAIL then
            userinfo(2,'gfun','The',method,'`seems to satisfy`',result);
            return [result,method]
         end if
      end do;
      FAIL
   end proc: # listtoalgeq

   seriestoalgeq:=proc ()
	local L;
      if args[1]<>'stamped' then seriestoalgeq(typecheck(6,args))
      else
	    		#listtoalgeq('stamped',
	    		#                       seriestolist('stamped',args[2],'ogf'),args[3],args[4]) end if
		L:=seriestolist('stamped',args[2],'ogf');
		# catch polynomials
                #moe:=Parameters('maxordereqn');
                #try
			if not has(args[2],O(1)) then
                                #Parameters('maxordereqn'=max(Parameters('minordereqn'),nops(L)-2));
				L:=[op(L),0$(max(10,nops(L)))]
			fi;
            listtoalgeq('stamped',L,args[3],args[4])
	  fi
   end proc: # seriestoalgeq

# guessandcheck
# Input:
#   listseries a list of series in x
#   x    name of the variable
#   ord     order of the Pade-Hermite approximant
#   nbnonzero  max number of nonzero coefficients expected in the result
#   tryfactors boolean (try whether factoring produces less nonzero coefficients)
#   ordcheck   number of zero terms in the final series
   guessandcheck:=proc(listseries,x,ord,nbnonzero,tryfactors,ordcheck)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local lpol, k, j, check;
      lpol:=map(collect,pade2(listseries,x,ord),x);
      if lpol=FAIL then return FAIL end if;
      if nops(subs(0=NULL,map(coeffs,lpol,x)))>nbnonzero then
         if not tryfactors then return FAIL end if;
         lpol:=map(factors,lpol);
         if nops(subs(0=NULL,[seq(coeffs(op(1,k),x),k=seq(op(op(2,k)),
                k=lpol))]))+nops(remove(has,lpol,x))>nbnonzero then
            return FAIL end if;
         lpol:=map(convert,[seq([k[1],seq(j[1]^j[2],j=k[2])],k=lpol)],`*`)
      end if;
      check:=map(normal,series(add(lpol[j]*listseries[j],j=1..nops(listseries))
	  ,x,ordcheck));
      if check=0 or op(1,check)=O(1) then return lpol end if;
      FAIL
   end proc: # guessandcheck

   listtoratpoly:=proc()
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local result, ex, methods, method, s, x, bigO, ord, nbz, tryfactors, nbzeros;
      if args[1]='stamped' then ex:=args[2];x:=args[3];methods:=args[4]
      else return listtoratpoly(typecheck(3,args)) end if;
      tryfactors:=type(ex,'list'('rational'));
      bigO:=nops(ex);
      nbzeros:=nops(ex)-nops(subs(0=NULL,ex));
      for method in methods do
         try
           	# Two methods that require a special treatment
         	if (method='revogf' or method='revegf') and ex[1]<>0 then
         		s:=listtoseries('stamped',[1,op(ex)],x,method)
			else
	            s:=listtoseries('stamped',ex,x,method)
	        fi
         catch :
            next;
         end try;

         userinfo(3,'gfun',`Trying the `,method,s);
         if type(s,'series') then ord:=order(s); nbz:=max(op(2,s),nbzeros)
         else ord:=bigO-1; nbz:=nbzeros end if;
         result:=guessandcheck([1,s],x,ord,ord-nbz-1,tryfactors,ord+3);
         if result<>FAIL then
            userinfo(2,'gfun','The',method,'`seems to be`',result);
            return [-result[1]/result[2],method]
         end if
      end do;
      FAIL
   end proc: # listoratpoly

   seriestoratpoly:=proc () # yes, it's stupid to convert it to a list now.
      if args[1]<>'stamped' then seriestoratpoly(typecheck(7,args))
      else listtoratpoly('stamped',
          seriestolist('stamped',args[2],'ogf'),op(0,args[2]),args[3]) end if
   end proc: # seriestoratpoly

$include <ratinterp.mi>

`s2d/s2d` := proc(s, x, y)
local L, i;
	L:=[seq(coeff(s,x,i),i=0..order(s)-1)];
	finddiffeq(L,y,x)
end proc: # `s2d/s2d`

# s2a/s2a
# Input: a series s, its variable x, a name y for the function y(x) whose
#   series it is.
# Output: a polynomial equation satisfied by y(x), if possible.
`s2a/s2a` := proc(s, x, y)
local L, i;
	L:=[seq(coeff(s,x,i),i=0..order(s)-1)];
	findalgeq(L,y,x)
end proc: # `s2a/s2a`

   listtorec:=proc()
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local result, methods, method, u, n, s, unkn, expr;
      if args[1]<>'stamped' then return listtorec(typecheck(2,args)) end if;
      expr:=args[2];unkn:=args[3];methods:=args[4];
      u:=op(0,unkn);n:=op(unkn);
      for method in methods do
         try
         	# Two methods that require a special treatment
         	if (method='revogf' or method='revegf') and expr[1]<>0 then
         	    s:=listtolist('stamped',[1,op(expr)],method)
		else
	            s:=listtolist('stamped',expr,method)
	        fi
         catch :
            next;
         end try;

         userinfo(3,'gfun',`Trying the `,method,s);
         result:=`l2r/l2r`(s,n,u);
         if result<>FAIL then
            userinfo(2,'gfun','The',method,'`seems to satisfy`',result);
            return [result,method]
         end if
      end do;
      FAIL
   end proc: # listtorec

   seriestorec:=proc ()
	local L, moe, res;
      if args[1]<>'stamped' then seriestorec(typecheck(6,args))
      else
		L:=seriestolist('stamped',args[2],'ogf');
		# catch polynomials
		moe:=Parameters('maxordereqn');
		try
			if not has(args[2],O(1)) then
				Parameters('maxordereqn'=max(Parameters('minordereqn'),nops(L)-2));
				L:=[op(L),0$(max(10,nops(L)))]
			fi;
			res:=listtorec('stamped',L,args[3],args[4]) ;
			Pamameters('maxordereqn'=moe)
		catch:
			Pamameters('maxordereqn'=moe);
			error
		end try;
		res
	  end if
   end proc: # seriestorec

`l2r/l2r`:=proc(L,n,u)
local res, i, ordereqn, inicoeff;
	res:=findequation(L,n,"recurrence");
	if res=FAIL then FAIL
	else
		ordereqn:=nops(res)-1;
		inicoeff:=subs(n=n-ordereqn,res[-1]);
		nbinicond:=max(ordereqn,firstnonzero(inicoeff,n))-1;
		{add(res[i+1]*u(n+i),i=0..ordereqn),seq(u(j)=L[j+1],j=0..min(nbinicond,nops(L)-1))}
	fi
end proc: # `l2r/l2r`

   inicond:=proc (s, eqn, y ,x)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local order, deq, i;
      deq:=select(has,indets(eqn,'diff(anything,identical(x))'),y(x));
      if deq={} then eqn
      else
         for order while deq<>{y(x)} do deq:=subs(diff(y(x),x)=y(x),deq) end do;
         {eqn,seq(`@@D`(i,y,0)=coeff(s,x,i)*i!,i=0..order-2)}
      end if
   end proc: # inicond

   listtohypergeom:=proc()
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local result, methods, method, s, unkn, expr;
      if args[1]<>'stamped' then return
      	listtohypergeom(typecheck(3,args)) end if;
      expr:=args[2];unkn:=args[3];methods:=args[4];
      for method in methods do
         try
         	# Two methods that require a special treatment
         	if (method='revogf' or method='revegf') and expr[1]<>0 then
       		    s:=listtolist('stamped',[1,op(expr)],method)
		else
	            s:=listtolist('stamped',expr,method)
	        fi
         catch :
            next;
         end try;

         userinfo(3,'gfun',`Trying the `,method,s);
         result:=`l2h/l2h`(s,unkn);
         if result<>FAIL then
            userinfo(2,'gfun','The',method,'`seems to satisfy`',result);
            return [result,method]
         end if
      end do;
      FAIL
   end proc: # listtohypergeom

   seriestohypergeom:=proc ()
      if args[1]<>'stamped' then seriestohypergeom(typecheck(7,args))
      else listtohypergeom('stamped',
                           seriestolist('stamped',args[2],'ogf'),op(0,args[2]),args[3]) end if
   end proc: # seriestohypergeom

   `l2h/l2h`:=proc (l, x)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local a, a0, k, eqn, u, v, w, den, i, z, c;
      a:=l;
      for k while op(1,a)=0 do a:=subsop(1=NULL,a) end do;
      a0:=op(1,a);k:=k-1;
      if nops(a)<5 then return FAIL end if;
      a:=[seq(op(i,a)/a0,i=2..nops(a))];
      eqn:=normal((6*a[4]*a[1]**2*a[2]+9*a[2]*a[3]**2+6*a[4]*a[1]*a[3]-6*
           a[3]**2*a[1]**2+a[2]**2*a[3]*a[1]-16*a[4]*a[2]**2)*x**2+(-32*a[4]
           *a[2]**2+5*a[2]**2*a[3]*a[1]+6*a[4]*a[1]*a[3]+18*a[4]*a[1]**2*
           a[2]+27*a[2]*a[3]**2-24*a[3]**2*a[1]**2)*x+6*a[2]**2*a[3]*a[1]-
           18*a[3]**2*a[1]**2+12*a[4]*a[1]**2*a[2]);
      if eqn=0 then v:=1
      elif degree(eqn,x)=0 then return FAIL
      else v:=op(1,[solve(eqn,x)])
      end if;
      den:=normal(4*a[2]**2*v**2-3*a[3]*v**2*a[1]-a[2]*v**2*a[1]**2-3*a[2]*v*
                  a[1]**2+8*a[2]**2*v-3*a[3]*v*a[1]-2*a[2]*a[1]**2);
      if den=0 then return FAIL end if;
      w:=normal(-2*(2*a[2]**2*v+4*a[2]**2-3*a[3]*v*a[1]-3*a[3]*a[1])*v)/den;
      if type(w,'negint') or w=0 then return FAIL end if;
      z:=normal(-3*a[3]*v*a[1]**2+a[1]*a[2]**2*v+3*a[3]*v*a[2]-3*a[3]*
                a[1]**2+2*a[2]**2*a[1]);
      if z=0 then return FAIL end if;
      u:=-normal(2*a[2]**2*v+4*a[2]**2-3*a[3]*v*a[1]-3*a[3]*a[1])*a[1]/z;
      z:=2*z/den;
      userinfo(3,'gfun',`candidate: hypergeom(`,[u,v],[w],z*x,`)`);
      c:=u*(u+1)*(u+2)*(u+3)*v*(v+1)*(v+2)*(v+3)/w/(w+1)/(w+2)/(w+3)*z^4/24;
      for i from 5 to nops(a) do
         c:=c*(u+i-1)*(v+i-1)*z/(w+i-1)/i;
         if c<>op(i,a) then return FAIL end if;
      end do;
      userinfo(2,'gfun',`hypergeom found, parameters:`,[u,v],[w],z*x);
      return simplify(a0*x^k*hypergeom([u,v],[w],z*x),hypergeom)
   end proc: # `l2h/l2h`

#ratpolytocoeff
# Input: a rational function of x
#   x its variable
#   n a name
# Output: the nth coefficient of the Taylor expansion at the origin of f.
   ratpolytocoeff:=proc(f,x,n)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local g, poly;
      g:=convert(f,'fullparfrac',x,'sqrfree');
      if type(g,'`+`') then g:=[op(g)] else g:=[g] end if;
      poly,g:=selectremove(type,g,'polynom'(anything,x));
      g:=convert(map(`ratpolytocoeff/elmt`,g,x,n),`+`);
      if poly=[] then g
      else
	 g+`ratpolytocoeff/poly`(convert(poly,`+`),x,n)
      fi
   end proc: # ratpolytocoeff

   `ratpolytocoeff/poly`:=proc (pol, x, n)
   local cofs, mons, i;
	cofs:=[coeffs(collect(pol,x),x,mons)];
	piecewise(seq(op([n=degree(mons[i],x),cofs[i]]),i=1..nops(cofs)),0)
   end: # `ratpolytocoeff/poly`

   `ratpolytocoeff/elmt`:=proc(g,x,n)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local k, a, c, i;
      if type(g,'function') then
         op(0,g)(`ratpolytocoeff/elmt`(op(1,g),x,n),op(2..nops(g),g))
      elif type(g,'polynom'('anything',x)) then 0
      else
         # g must be c(a)*(x-a)^(-k)
         k:=select(has,indets(g,`^`),x);
         if nops(k)<>1 then error "report this as a bug: g=%1, x=%2, n=%3",g,x,n end if;
         k:=op(k);
         a:=x-op(1,k);
         c:=g/k;
         k:=-op(2,k);
         c/(-a)^k*a^(-n)*mul(n+i,i=1..k-1)/(k-1)!
      end if
   end proc: # `ratpolytocoeff/elmt`

   guesseqn:=proc ()
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local y, result, l, x, methods, ll, i;
      if args[1]<>'stamped' then return guesseqn(typecheck(2,args)) end if;
      l:=args[2];y:=op(0,args[3]);x:=op(args[3]);methods:=args[4];
      # First try to find a rational function
      userinfo(1,'gfun',`Trying to find a rational generating function`);
      for i in methods do
         try
			# Two methods that require a special treatment
			if (i='revogf' or i='revegf') and l[1]<>0 then
				ll[i]:=listtolist([0,op(l)],i)
			else
	            ll[i]:=listtolist(l,i)
	        fi
         catch :
            next;
         end try;

         result:=listtoratpoly('stamped',ll[i],x,['ogf']);
         if result=FAIL then next end if;
         return [denom(result[1])*y(x)-numer(result[1]),i]
      end do;
      # Then an algebraic equation
      userinfo(1,'gfun',`Trying to find an algebraic generating function`);
      for i in methods do
         result:=listtoalgeq('stamped',ll[i],y(x),['ogf']);
         if result<>FAIL then return [result[1],i] end if
      end do;
      # Then a linear differential equation
      userinfo(1,'gfun',`Trying to find a linear differential equation`);
      for i in methods do
         result:=listtodiffeq('stamped',ll[i],y(x),['ogf']);
         if result<>FAIL then return [result[1],i] end if
      end do;
      FAIL
   end proc: # guesseqn

   guessgf:=proc ()
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local interres, y, result, l, x, methods, inds, s, i, ll, j, sol, tmp;
      if args[1]<>'stamped' then return guessgf(typecheck(3,args)) end if;
      l:=args[2];x:=args[3];methods:=args[4];
      # First try to find a rational function
      userinfo(1,'gfun',`Trying to find a rational generating function`);
      for i in methods do
         try
         	# Two methods that require a special treatment:
         	if (i='revogf' or i='revegf') and l[1]<>0 then
				ll[i]:=listtolist([0,op(l)],i)
			else
	            ll[i]:=listtolist(l,i)
	        fi
         catch :
            next;
         end try;

         result:=listtoratpoly('stamped',ll[i],x,['ogf']);
         if result<>FAIL then return [result[1],i] end if
      end do;
      # Then trap easy hypergeometrics
      userinfo(1,'gfun',`Trying to find an hypergeometric generating function`);
      for i in methods do
         result:=listtohypergeom('stamped',ll[i],x,['ogf']);
         if result<>FAIL then return [result[1],i] end if
      end do;
      # Then algebraic functions
      userinfo(1,'gfun',`Trying to find an algebraic generating function`);
      for i in methods do
         result:=listtoalgeq('stamped',ll[i],y(x),['ogf']);
         if result=FAIL then next end if;
         userinfo(1,'gfun',`Trying to solve the equation`);
         sol:=[solve(result[1],y(x))];
         if nops(sol)=1 then return [sol,i] end if;
	 s:=listtoseries(ll[i],x);
         for j to nops(sol) do
            tmp:=series(sol[j]-s,x,nops(ll[i]));
            if tmp=0 or type(tmp,'series') and op(2,tmp)>=nops(ll[i]) then
		return [sol[j],i]
            end if
         end do;
      end do;
      # Then a linear differential equation
      userinfo(1,'gfun',`Trying to find a linear differential equation`);
      for i in methods do
         interres:=listtodiffeq('stamped',ll[i],y(x),['ogf']);
         if interres=FAIL then next end if;
         userinfo(1,'gfun',`Trying to solve it`);
         result:=dsolve(op(1,interres),y(x));
         if result<>FAIL and result<>NULL then
            inds:=(indets(op(2,result),'name') minus indets(l,'name'))
            minus {x,constants};
            if inds={} then return [op(2,result),i] end if;
            s:=series(op(2,result),x,nops(l)+1);
            s:=solve({seq(coeff(s,x,j-1)-op(j,l),j=1..nops(l))},inds);
            if s<>NULL and type(s,'set') then
               return subs(s,[op(2,result),i]) end if
         end if
      end do;
      FAIL
   end proc: # guessgf

####################### Power Series Reversion #######################

# p should be a series with no constant term and a non-zero linear term.
   powrevert := proc (s::series,x,o)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local p, v, k, pv, ppv;
      if op(2,s)<>1 then
	  error "expected a series with valuation 1, got %1",s fi;
      v:=x/op(1,s);
      p:=convert(s,polynom);
      k:=1;
      while 2*k+1<=o do
         pv:=powcompose(p,v,x,2*k+1);
         ppv:=pprimeknowingp(pv,v,x,2*k+1);
         v:=v-powdivide(pv-x,ppv,x,2*k+1);
         k:=2*k+1
      end do;
      if k<o then
         pv:=powcompose(p,v,x,o);
         ppv:=pprimeknowingp(pv,v,x,o);
         v:=v-powdivide(pv-x,ppv,x,o)
      end if;
      series(v+O(x^(o+1)),x,o+1)
   end proc: # powrevert

# pol, pol -> pol
   powcompose:=proc (Q,P,x,n) # this assumes P(0)=0.
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local m, pm, pr, pr1, l, i, s, p, q;
      p:=powtruncate(P,x,n);
      q:=powtruncate(Q,x,n);
      if n<9 then
         s[0]:=coeff(Q,x,0);
         for i to degree(Q,x) do
            s[i]:=collect(coeff(Q,x,i)*powtruncate(P,x,n-i+1)^i,x)
         end do;
         powtruncate(add(s[i],i=0..degree(Q,x)),x,n)
      else
         m:=isqrt(trunc(3.32192809*n/length(n)));
         pm:=powtruncate(p,x,m);
         pr:=1;pr1:=p-pm;
         l:=powcomposesimple(q,pm,x,n);
         s[0]:=l;
         for i to iquo(n,m)+1 do
            l:=pprimeknowingp(l,pm,x,n-i);
            pr:=powtruncate(collect(pr*pr1,x),x,n);
            s[i]:=collect(l*pr/i!,x)
         end do;
         powtruncate(add(s[i],i=0..iquo(n,m)+1),x,n)
      end if
   end proc: # powcompose

# pol, pol -> pol
   powcomposesimple:=proc (q,p,x,n)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local s, j, pk, i;
      j:=degree(q,x);
      s:=1; while s<j do s:=s*2 end do;
      pk[1]:=powtruncate(p,x,n);
      i:=1; while i<s do
               pk[2*i]:=powtruncate(collect(pk[i]^2,x),x,n); i:=2*i end do;
      powcomposesimpledoit(q,s,pk,x,n)
   end proc: # powcomposesimple

# pol, pol -> pol
   powcomposesimpledoit:=proc (q, s, pk, x, n)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local q1, q2;
      if s=8 then
         powtruncate(collect(coeff(q,x,0)+pk[1]*(coeff(q,x,1)+
             coeff(q,x,3)*pk[2]+pk[4]*(coeff(q,x,5)+coeff(q,x,7)*pk[2]))+
             pk[2]*(coeff(q,x,2)+coeff(q,x,6)*pk[4])+coeff(q,x,4)*pk[4]+
             coeff(q,x,8)*pk[8],x),x,n)
      elif s>8 then
         q1:=powtruncate(q,x,s/2-1);
         q2:=collect((q-q1)/x^(s/2),x);
         powtruncate(powcomposesimpledoit(q1, s/2, pk, x, n)+
              collect(pk[s/2]*
                  powcomposesimpledoit(q2, s/2, pk, x, n-s/2),x),x,n)
      elif s=4 then
         powtruncate(collect(coeff(q,x,0)+pk[1]*(coeff(q,x,1)+
            coeff(q,x,3)*pk[2])+coeff(q,x,2)*pk[2]+coeff(q,x,4)*pk[4],x),x,n)
      elif s=2 then
         collect(coeff(q,x,0)+coeff(q,x,1)*pk[1]+coeff(q,x,2)*pk[2],x)
      else # ASSERTION s=1
         subs(x=pk[1],q)
      end if
   end proc: # powcomposesimpledoit

# pol, pol -> pol
   pprimeknowingp:=proc (poff, f, x, n)
      powdivide(diff(poff,x),diff(f,x),x,n)
   end proc: # pprimeknowingp

# pol, pol -> pol
   powdivide:=proc (p, q, x, n)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local u, b, i, j;
      # assuming q[0]<>0, otherwise a shift of this version is all we need
      # This is slower that convert(series(p/q,x,n+1),polynom), but
      # uses less memory.
      if subs(x=0,q)=0 then error "not implemented" end if;
      for i from 0 to n do
         b[i]:=coeff(q,x,i);
         u[i]:=(coeff(p,x,i)-add(u[j]*b[i-j],j=0..i-1))/b[0]
      end do;
      add(u[i]*x^i,i=0..n)
   end proc: # powdivide

   powtruncate:=proc (pol, x, n)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local i;
      if degree(pol,x)<=n then pol
      elif ldegree(pol,x)>n then 0
      else add(coeff(pol,x,i)*x^i,i=0..n)
      end if
   end proc: # powtruncate

######################## Puiseux Expansions ##########################

   algeqtoseries:=proc(Pol::polynom(anything,[x,y]),x::name,y::name,ord::nonnegint,optional_positive_slopes)
      map(`algeqtoseries/prettyprint`,`algeqtoseries/doit`(args),x)
   end proc: # algeqtoseries

   `algeqtoseries/doit`:=proc (Pol,x,y,ord,opt)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local pol, a, u, i, j, pts, alpha, mini, deg, theta, jmin, sl, lastpt, p, pu, nb2, res, r, nb, q, a0, normalizer, eq, leadcoeffs;
      _EnvExplicit:=false; # otherwise some RootOf's will be perturbed
      pol:=collect(Pol,[y,x],evala);
      # Newton polygon
      deg:=degree(pol,y);
      ## Added simplify here because it is crucial to recognize 0. BS Aug 05.
#      pts:=select(proc(x) x[2]<>0 end,[seq([i,collect(coeff(pol,y,i),x,simplify)],i=0..deg)]);
      pts:=select(proc(x) x[2]<>0 end,[seq([i,coeff(pol,y,i)],i=0..deg)]);
      pts:=[seq([i[1],ldegree(i[2],x)],i=pts)];
      nb:=0;
      lastpt:=pts[1];
      for i from 2 to nops(pts) do
         mini:=infinity;
         for j from i to nops(pts) do
            theta:=(pts[j][2]-lastpt[2])/(pts[j][1]-lastpt[1]);
            if theta<mini then mini:=theta; jmin:=j end if
         end do;
         nb:=nb+1; alpha[nb]:=-mini; i:=jmin; lastpt:=pts[i]
      end do;
      # Treat each slope
      nb2:=0;
      alpha:={seq(alpha[i],i=1..nb)};
      if nargs=5 then alpha:=select(type,alpha,'nonnegative') end if;
      for sl in alpha do
         r:=denom(sl);
         p:=collect(subs(x=x^r,y=x^numer(sl)*y,pol),x);
         if ldegree(p,x)<>0 then p:=collect(p/x^ldegree(p,x),x) end if;
         q:=collect(coeff(p,x,0),y);
         if ldegree(q,y)<>0 then q:=collect(q/y^ldegree(q,y),y) end if;
         for u in sqrfree(q,y)[2] do
            leadcoeffs:=[RootOf(u[1],y)];
            while leadcoeffs<>[] do
               try # changed handling of errors for maple7
                  a0:=leadcoeffs[1];
                  leadcoeffs:=subsop(1=NULL,leadcoeffs);
                  if u[2]=1 then   # regular case
                     if type(a0,'RootOf') then normalizer:=evala
                     else normalizer:=normal end if;
                     a[0]:=a0; pu:=p;
                     for i to ord-1 do # ord relatively small
                        pu:=collect(subs(y=a[i-1]+x*y,pu),x);
                        # do not use solve: it does strange things with
                        # RootOf's
                        # a[i]:=normalizer(solve(coeff(pu,x,i),y))
                        eq:=coeff(pu,x,i); # assumed to be linear in y
                        if degree(eq,y)=1 then
                           a[i]:=normalizer(-coeff(eq,y,0)/coeff(eq,y,1))
                        else error "unforecast case" end if
                     end do; # in this loop, normal saves memory
                     nb2:=nb2+1;
                     res[nb2]:=[seq([a[i],sl+i/r],i=0..ord-1),
                                [O(1),sl+ord/r]]
                  elif ord=0 then # several branches, O() term
                     to u[2] do
                        nb2:=nb2+1;
                        res[nb2]:=[[O(1),sl]]
                     end do
                  else    # several branches
                     for i in `algeqtoseries/doit`(
                        subs([x=x^u[2],y=a0+x*y],p),x,y,ord-1,1) do
                        nb2:=nb2+1;
                        res[nb2]:=[[a0,sl],seq([j[1],sl+(j[2]+1)/u[2]/r],
                                               j=i)]
                     end do
                  end if
               catch "reducible RootOf detected.  Substitutions are":
                  leadcoeffs:=[op(map(subs,lastexception[3],RootOf(u[1],y))),
                               op(leadcoeffs)];
               end try;
            end do
         end do
      end do;
      [seq(res[i],i=1..nb2)]
   end proc: # gfun/algeqtoseries/doit

   `algeqtoseries/prettyprint`:=proc (l, x)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local i;
      series(add(i[1]*x^i[2],i=l),x,max(4,ceil(l[nops(l)][2])))
   end proc: # `algeqtoseries/prettyprint`


######################## Holonomic Functions #########################


$include <ported/diffeqtohomdiffeq.mm>


# $include <gfun/src/algeqtodiffeq.mpl>
algeqtodiffeq := proc(p::depends({polynom(anything,yofz),polynom(anything,yofz)=polynom(anything,yofz)}),yofz::function(name),inits::set:={},{homogeneous::boolean:=false,ini_cond::boolean:=true})
  option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
  local P,y, z, g, u, d, i, Y, deq, j, r, inity, P0, iszero, gg, st, mat,st0, remP, den, maxd, st1, newinit;
	y:=op(0,yofz);z:=op(yofz);
	if type(p,`=`) then P:=op(1,p)-op(2,p) else P:=p fi;
	P:=subs(yofz=y,P);
     	userinfo(2,'algeqtodiffeq',"entering algeqtodiffeq with polynomial", P,assign(st1=time()));
  	g:=gcdex(diff(P,y),P,y,'u');
     	userinfo(2,'algeqtodiffeq',"modular inverse of P_y computed in time:",time()-st1,
		assign('st1'=time()-st1,'st'=time(),'st0'=time()));
     	if has(g,y) then return algeqtodiffeq(normal(P/g),y(z),inits,homogeneous) end if;
     	d := degree(P,y); userinfo(3,'algeqtodiffeq',`degree is `,d);
	if homogeneous then maxd:=d else maxd:=d-1 fi;
     	if d<=1 then deq:=subs(y=y(z),P)
     	elif not has(P,z) then deq:=y(z)-RootOf(P,y)
     	else
       		Y[1]:=rem(-u/g*diff(P,z),P,y);
        	userinfo(3,'algeqtodiffeq',"row of index",1,"computed in time",time()-st,assign('st'=time()));
        	for i from 2 to maxd do # compute Y[i] = diff(y,z$i) mod P
       			Y[i]:=rem((diff(Y[i-1],z)+diff(Y[i-1],y)*Y[1]),P,y);
           		userinfo(3,'algeqtodiffeq',"row of index",i,"computed in time",time()-st,assign('st'=time()));
	 	end do;
	 	mat:=Matrix(1..d+1,1..d,([`if`(homogeneous,NULL,[1,0$(d-1)]),[0,1,0$(d-2)],
              		seq([seq(coeff(Y[i],y,j),j=0..d-1)],i=1..maxd)]));
	  	userinfo(2,'algeqtodiffeq',"matrix computed in time",time()-st0,assign('st0'=time()-st0,'st'=time()));
    		deq:=lindep(mat,[`if`(homogeneous,NULL,1),seq(diff(y(z),[z$i]),i=0..maxd)],z);
	 	userinfo(2,'algeqtodiffeq',"linear dependency computed in time",time()-st,assign('st'=time()-st))
     	end if;
	userinfo(4,'algeqtodiffeq',
		sprintf("###    deg    mod_inverse    matrix    lindep"));
	userinfo(4,'algeqtodiffeq',sprintf("###     %d      %g          %g     %g",d,st1,st0,st));
     	userinfo(5,'algeqtodiffeq',`differential equation is`,deq);
##	if inits={} then return deq fi; ## this is not the previous behaviour, but helps testing
     	P0:=normal(subs(z=0,y=y(0),inits,P));
     	if not has(P0,y) and P0<>0 then # this means that the origin
        	# is a singular point
        	if inits={} then return deq
        	else error "invalid initial conditions" end if
     	end if;
	newinit:={};
    	if P0<>0 then
 		# make it square-free
 		P0:=subs(y(0)=y,P0);
 		gg:=gcd(P0,diff(P0,y));
 		if gg<>1 then P0:=quo(P0,gg,y) fi;
		#     This will be the correct way when Maple does not insist on the argument
		#     of RootOf being irreducible.
		#  inits:=inits union {y(0)=RootOf(subs(y(0)=_Z,P0))} end if;
        	try
           		evala(RootOf(P0,y));
           		newinit:={y(0)=RootOf(P0,y)};
        	catch :
        	end try;
     	end if;
     	inity:=y=subs(inits union newinit,y(0));
     	newinit:={y(0)=op(2,inity)} union subs(y(0)=op(2,inity),
    		remove(type,inits union newinit,identical(y(0))='anything'));
	for i to d-1 do
  		try
     			r:=`@@D`(i,y,0)=subs([z=0,inity],Y[i]);
     			newinit:=newinit union {r}
  		catch :
     			break;
  		end try;
	end do;
	if not ini_cond then return deq fi;
	newinit,iszero:=`goodinitvalues/diffeq`(formatdiffeq([deq,y(z)]),y,z,
     		select(type,inits union newinit,'anything'=`gfun/free`(y(0))));
	if newinit={} then return deq
	elif iszero then return y(z)
	else return {deq,op(newinit)} end if
end proc: # algeqtodiffeq


$include <ported/diffeqtorec.mm>

$include <ported/diffeqtorec_doit.mm>

#rectohomrec
# Input: a recurrence and its unknown function u(n)
# Output: a homogeneous recurrence cancelling the solutions of the original one
#
   rectohomrec:=proc (Rec,uofk)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local rec, u, k, ini, n;
      rec:=formatrec([args],u,k,ini);
      if rec[1]=0 then Rec
      else
         n:=nops(rec);
         if ini<>{} or type(Rec,set) then
         	ini:=`goodinitvalues/rec`(rec,u,k,ini,true,
         		max(n-2,firstnonzero(rec[1],k)))
         end if;
	     rec:=`rectohomrec/doit`(rec,k);
         makerec(rec,u,k,ini)
      end if
   end proc: # rectohomrec

   `rectohomrec/doit`:=proc(rec,k)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local c, n, dc, i;
      n:=nops(rec);
      c:=rec[1];
      dc:=-subs(k=k+1,c);
      map(collect,[0,dc*rec[2],seq(dc*rec[i]+c*subs(k=k+1,rec[i-1]),i=3..n),
                   c*subs(k=k+1,rec[n])],k)
   end proc: # rectohomrec/doit

# This one is for the case of constant coefficients
# except possibly in the inhomogeneous part. It returns
# a homogeneous recurrence with constant coefficients and order
# the order of the original one plus the degree of the inhomogeneous term.
   rectohomrecbis:=proc(Rec,uofk)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local rec, u, k, ini, n, i, c, dc, co, j;
      rec:=formatrec([args],u,k,ini);
      if rec[1]=0 then Rec
      elif has(subsop(1=NULL,rec),k) then error "invalid recurrence: %1",Rec
      else
         n:=nops(rec);
         c:=collect(rec[1],k);
         dc:=degree(c,k);
         if ini<>{} then
         	ini:=`goodinitvalues/rec`(rec,u,k,ini,true,n-1+dc) end if;
         co[-1]:=0;
         for i from 0 to n-2 do co[i]:=rec[i+2] end do;
         for i from 0 to dc do
            co[n+i-1]:=co[n+i-2];
            for j from n+i-2 by -1 to 0 do co[j]:=co[j-1]-co[j] end do
         end do;
         makerec([0,seq(co[j],j=0..n+dc-1)],u,k,ini)
      end if
   end proc: # rectohomrecbis

#rectodiffeq
# Input:  expr: a linear recurrence (with or without initial conditions)
#    a(n): its unknown function
#    f(t): the function sum(a(n)*t^n,n=0..infinity)
#    ini: (optional) boolan indicating whether initial conditions should
#     be computed. This is not documented yet. It is not clear
#               what it should do in all cases.
# Output: the linear differential equation satisfied by f(t).
#
   rectodiffeq := proc(expr,aofn,foft,{ini::boolean:=true,homogeneous::boolean:=false})
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local r, a, n, f, t, iniconds, A, N;
      getname(foft,f,t); # This also checks that >=3 args are passed
      if ini then r:=formatrec([expr,aofn],'a','n','iniconds')
      else r:=formatrec([expr,aofn],'a','n');
         # Commented out Aug. 07. BS. I do not remember why it's useful
         # if r[1]<>0 then error "inhomogeneous equation" end if;
      end if;
      if homogeneous and r[1]<>0 then r:=`rectohomrec/doit`(r,n) fi;
      if ini then
         iniconds:=`goodinitvalues/rec`(r,a,n,iniconds,false)
      else iniconds:=NULL end if;
      # Avoid problems when a=f or a=t or n=f or ...
      if has(r,f) then error "%1 cannot appear in the recurrence",f
      elif has(r,t) then error "%1 cannot appear in the recurrence",t end if;
      # removed listprimpart: this prevented (n+1)*n*u(n+1)-n^2*u(n) to work
      # for ln(1-z)
      # `rectodiffeq/doit`(listprimpart(r,N),A,N,f,t,iniconds)
      `rectodiffeq/doit`(r,a,n,f,t,homogeneous,iniconds)
   end proc: # rectodiffeq

   `rectodiffeq/doit`:=proc (r,u,n,f,z,homogeneous,iniconds)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local order, diffeq, P, k, p, a, rr, returninis, ini, i, k0, inds, res, l, c, aa, j, iszero;
      order:=max(op(map(degree,r,n)));
      returninis:=evalb(nargs=7);
      diffeq:=Array(-1..order,'storage'='sparse');
      k0:=nops(r)-2;
      D(f):='D(f)';
      # To keep polynomial coefficients, multiply by z^(nops(r)-2)
      for k from 0 to k0 do
         P:=op(k+2,r);
         for p from 0 to degree(P,n) do
#            a:=subs(n=p-k,P);
#            P:=quo(P-a,n+k-p,n);
            P:=quo(P,n+k-p,n,'a');
            diffeq[p]:=diffeq[p]+a*z^(k0+p-k);
            # This is always needed. Ex: u(n+1)-(n+1)*u(n)
#            if returninis then
               diffeq[-1]:=diffeq[-1]-collect(a*z^(k0+p-k)*
                  diff(add(`@@D`(i,f,0)*z^i/i!,i=p..k-1),[z$p]),z)
#            end if
         end do end do;
      # Non-homogeneous part of the recurrence
      P:=op(1,r);
      for p from 0 to degree(P,n) do
         rr[p]:=subs(n=-p-1,P);
         P:=quo(P-rr[p],(n+p+1)/(p+1),n)
      end do;
      diffeq:=[diffeq[-1]*(1-z)^p+z^k0*add(rr[k]*(1-z)^(p-k-1),k=0..p-1),
	seq((1-z)^p*diffeq[k],k=0..order)];
#    # remove apparent singularity at the origin
#    k:=min(op(map(ldegree,diffeq,z)));
#    if k>0 then diffeq:=map(quo,diffeq,z^k,z) end if;
      # initial conditions
      if returninis then
         inds:=map(op,indets(iniconds,u('anything')));
         ini:=solve(subs([seq(u(i)=`@@D`(i,f,0)/i!,i=inds)],iniconds),
                    {seq(`@@D`(i,f,0),i=inds)});
         diffeq:=subs(ini,diffeq);
         # some initial conditions may correspond to polynomial inhomogeneities
         # at least when the equation is not singular at the origin.
         inds:=max(op(inds));
         if # subs(z=0,diffeq[nops(diffeq)])<>0 and
         inds<>-infinity and inds>=order
         then
            diffeq:=subsop(1=collect(diffeq[1],z)-convert(series(eval(subs(
               f(z)=add(`@@D`(i,f,0)*z^i/i!,i=0..inds)+O(1)*z^(inds+1),
		    ini,makediffeq(diffeq,f,z))),z,infinity),
                                                          polynom),diffeq)
         end if
      end if;
      # Case when the inhomogeneous part contains (D@@k)(f)(0)
      # Then make the equation homogeneous.
      if has(diffeq[1],f) or homogeneous then
         l:=select(has,indets(diffeq[1],'function'),f);
         if l<>{} and not type(diffeq[1],'linear'(l)) then
            error "invalid inhomogeneous part" end if;
         diffeq:=subs({seq(l[i]=aa[i],i=1..nops(l))},diffeq);
         l:=[seq(aa[i],i=1..nops(l))];
         diffeq:=subsop(1=collect(diffeq[1],l),diffeq);
         for i in l do
            if has(diffeq[1],i) then
               c:=coeff(diffeq[1],i,1);
               res:=-diff(c,z);
               diffeq:=[collect(res*diffeq[1]+c*diff(diffeq[1],z),l,normal),
                        res*diffeq[2]+c*diff(diffeq[2],z),
                        seq(res*diffeq[j]+c*diff(diffeq[j],z)+
                            c*diffeq[j-1],j=3..nops(diffeq)),
                        c*diffeq[nops(diffeq)]]
            end if
         end do;
         if homogeneous and diffeq[1]<>0 then # does not depend on the (D@@k)(f)(0)
                 c:=diffeq[1];
                 res:=-diff(c,z);
                 diffeq:=[0,
                          res*diffeq[2]+c*diff(diffeq[2],z),
                          seq(res*diffeq[j]+c*diff(diffeq[j],z)+
                              c*diffeq[j-1],j=3..nops(diffeq)),
                          c*diffeq[nops(diffeq)]];
         fi;
         if has(diffeq,aa) then error "some assertion was wrong" end if;
         diffeq:=map(collect,diffeq,z)
      end if;
      diffeq:=listprimpart(diffeq,z);
      if returninis then ini,iszero:=`goodinitvalues/diffeq`(diffeq,f,z,ini);
              if iszero then return f(z) fi
      else ini:={} end if;
      res:=makediffeq(subs(ini,diffeq),f,z);
      if ini<>{} then {res,op(ini)} else res end if
   end proc: # `rectodiffeq/doit`

# poltodiffeq
#Input: a polynomial P in n variables y1, y2, ..., yn
#       with rational coefficients in z
#  a list of n differential equations
#  a list of n variables y1, y2, ..., yn
#  a variable y(z)
#Output: a differential equation in y(z) satisfied by P(z,y1(z),...,yn(z))
#        where yi is a solution to the ith differential equation.
#
# It is important to avoid using D, otherwise too much time is spent
# converting from diff to D, and back.
#
   poltodiffeq:=proc (p, ldeq::list, ly::list, yofz)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local i, deqs, ord, ini, y, z, lco, vars, n, h, u, tosubs, origdiff, j, k, l2, l, yy, dorder, jmax, inds, pp, linds, v, mon, lind, lcoef, lmax, basis, back, uu, sing, iszero;
      if nops(ldeq)<>nops(ly) then
         error "not the same number of elements: %1 and %2",ldeq,ly end if;
      getname(yofz,y,z);
      n:=nops(ldeq);
      tosubs:=subsop(4=NULL,proc() args end):
      userinfo(4,'gfun',"entering poltodiffeq with args",args);
      for i to n do
         deqs[i]:=formatdiffeq([ldeq[i],ly[i]],'yy','z',ini[i]);
         ord[i]:=nops(deqs[i])-2;
         lco:=deqs[i][nops(deqs[i])];
         mon:=diff(y[i](z),[z$ord[i]]);
         v:=-deqs[i][1]/lco-
         add(deqs[i][j]/lco*diff(y[i](z),[z$(j-2)]),j=2..ord[i]+1);
         tosubs(mon):=v;
         lcoef[mon]:=[coeffs(v,[seq(diff(y[i](z),[z$j]),
         	j=0..ord[i]-1)],lind[mon])]
      end do;
      if has(p,'D') then pp:=convert(p,diff) else pp:=p end if;
      u:=subs([seq(op(0,ly[i])=y[i],i=1..n)],pp);
      linds:={seq(diff(y[i](z),[z$ord[i]]),i=1..n)};
      origdiff:=[seq(i=tosubs(i),i=linds)]; # initial rewriting rules
      # treat the diff's in u
      pp:=select(has,indets(u,'specfunc'('anything',diff)),{seq(y[i],i=1..n)});
      for lmax from 0 while pp<>{} do pp:=select(has,map(op,pp),diff) end do;
      while has(u,linds) do u:=eval(subs(origdiff,u)) end do;
      pp:=u;
      vars:={seq(seq(diff(y[i](z),[z$j]),j=0..ord[i]-1),i=1..n)};
      if not type(u,'polynom'('ratpoly'('anything',z),vars)) then
         if nops(ly)=1 then
            error "%1 is not a polynomial in %2 and its derivatives", p, op(ly)
         else error "%1 is not a polynomial in %2 and their derivatives", p, op(ly)
         end if
      end if;
      basis:=vars;
      linds:={1} union linds;
      for dorder do # this loop includes an incremental Gaussian elimination
         # first the derivatives of high order are rewritten
         # and the polynomial is collected in the smaller order derivatives
	 userinfo(3,'gfun',"computing and reducing derivative of order",dorder);
         u:=collect(subs(origdiff,u),vars,'distributed',normal);
         u:=[coeffs(u,vars,'l')];l:=[l];
         # then those monomials corresponding to previous lines of the Gaussian
         # elimination are eliminated
         u:=collect(add(u[j]*tosubs(l[j]),j=1..nops(u)),vars,
                    'distributed',normal);
         # the list of remaining monomials will be stored in l:
         lco:=[coeffs(u,vars,'l')];
         # linds is the set of monomials corresponding to previous lines
         l2:={l} minus linds;
         # if no other monomial remains, then elimination is finished
         if l2={} then break end if;
         l:=[l];
         # the monomial which will be selected as a pivot might be involved in
         # previous lines. In this case, we also compute the back substitution.
         back:=evalb(l2 minus basis={});
         if not back then mon:=op(1,l2 minus basis) else mon:=l2[1] end if;
         member(mon,l,'k');
         # new line
         tosubs(mon):=collect((diff(h(z),[z$(dorder-1)])-
	   add(lco[j]*tosubs(l[j]),j=1..k-1)-
	   add(lco[j]*tosubs(l[j]),j=k+1..nops(l)))/lco[k],vars,'distributed');
         lcoef[mon]:=[coeffs(tosubs(mon),vars,lind[mon])];
         basis:=basis union {lind[mon]};
         # back substitution
         if back then
            for i in linds do
               if member(mon,[lind[i]],'k') then
                  lind[i]:=[lind[i]];
                  tosubs(i):=collect(add(lcoef[i][j]*lind[i][j],j=1..k-1)+
                     lcoef[i][k]*tosubs(mon)+
                     add(lcoef[i][j]*lind[i][j],j=k+1..nops(lcoef[i])),
		     vars,'distributed');
                  lcoef[i]:=[coeffs(tosubs(i),vars,evaln(lind[i]))]
               end if
            end do;
         end if;
         linds:=linds union {mon};
         u:=diff(u,z)
      end do;
      # final equation
      u:=subs(h=y,collect(primpart(numer(diff(h(z),[z$(dorder-1)])-u)),
                          [seq(diff(h(z),[z$j]),j=0..dorder-1)]));
      ## initial conditions
      # if one of the differential equations was not given with initial
      # conditions and it is singular, it may mean that a singular solution
      # is considered. Then don't try to return initial conditions.
      sing:=false;
      for i to n while not sing do
         if ini[i]={} and subs(z=0,deqs[i][nops(deqs[i])])=0 then
            sing:=true end if end do;
      if sing then ini:={}
      else
         jmax:=-1;
         for i to n do
            D(y[i]):=evaln(D(y[i]));
            ini[i]:=`goodinitvalues/diffeq`(deqs[i],op(0,ly[i]),op(ly[i]),
                                            ini[i],dorder-2+lmax)[1];
            inds:=indets(ini[i],_C['anything']);
            if inds<>{} then
               ini[i]:=subs([seq(inds[j]=_C[jmax+j],j=1..nops(inds))],ini[i]);
               jmax:=jmax+nops(inds)
            end if
         end do;
         ini:=`union`(seq(subs(op(0,ly[i])=y[i],ini[i]),i=1..n));
         jmax:=max(seq(op(2,op(0,op(0,op(1,i)))),i=select(has,ini,`@@`)),
                   seq(1,i=select(has,ini,{seq(evaln(D(y[i])),i=1..n)})),0);
         uu:=formatdiffeq([u,y(z)]);
         jmax:=max(jmax,nbinicond(uu,y,z));
#         try
#            v[0]:=limit(pp,z=0,'right');
#            if has(v[0],infinity) then jmax:=-1 end if;
#         catch :
#            jmax := -1;
#         end try;
##        for j to jmax do
##            pp:=convert(diff(pp,z),'D');
##            v[j]:=subs(z=0,pp);
##         end do;
        try
         ini,iszero:=`goodinitvalues/diffeq`(uu,y,z,
#             remove(has,subs(ini,{seq(`@@D`(j,y,0)=v[j],j=0..jmax)}),
             remove(has,inifromseries(pp,z,jmax,y,0,ini),
                                             {seq(y[i],i=1..n)}));
         if iszero then return y(z) fi
        catch "no valid initial conditions":
            ini:={}
        end try;
      end if;
      if ini={} then u else {u,op(ini)} end if
   end proc: # poltodiffeq

# diffeq+diffeq
#Input: two differential equations Eq1 and Eq2 in the variable y(z)
#Output: a differential equation satisfied by the sum of a solution of Eq1 and
#   a solution of Eq2.
   `diffeq+diffeq` := proc(eq1,eq2,yofz)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local y1,y2,y,z;
     getname(yofz,y,z);
     poltodiffeq(y1(z)+y2(z),[subs(y=y1,eq1),subs(y=y2,eq2)],[y1(z),y2(z)],yofz)
   end proc:

# diffeq*diffeq
#Input: two differential equations Eq1 and Eq2 in the variable y(z)
#Output: a differential equation satisfied by the product of a solution of Eq1
#   and a solution of Eq2.
   `diffeq*diffeq` := proc(eq1,eq2,yofz)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local y1,y2,y,z;
      getname(yofz,y,z);
     poltodiffeq(y1(z)*y2(z),[subs(y=y1,eq1),subs(y=y2,eq2)],[y1(z),y2(z)],yofz)
   end proc:

# cauchyproduct
#Input: two linear recurrences rec1 and rec2 in the variable uofn (u(n))
#Output: a linear recurrence satisfied by \sum_{k=0}^n{u_kv_{n-k}}, where
#      u is a solution of rec1 and v is a solution of rec2.
   cauchyproduct := proc(rec1,rec2,uofn)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local y1, y2, y, z, d1, d2, inds, j, i;
      d1:=rectodiffeq(rec1,uofn,y1(z));
      d2:=rectodiffeq(rec2,uofn,y2(z));
      inds:=indets(d1,_C['anything']) intersect indets(d2,_C['anything']);
      if inds<>{} then
         j:=max(op(map(op,
              indets(d1,_C['anything']) union indets(d2,_C['anything']))));
         d2:=subs([seq(inds[i]=_C[j+i],i=1..nops(inds))],d2)
      end if;
      diffeqtorec(poltodiffeq(y1(z)*y2(z),[d1,d2],[y1(z),y2(z)],y(z)),y(z),uofn)
   end proc: # cauchyproduct

# poltorec
#Input: a polynomial P in k+1 variable n, u1, u2, ..., uk
#  a list of k recurrence equations
#  a list of k variables u1, u2, ..., uk
#  a variable u(n)
#Output: a recurrence equation in u(n) satisfied by P(n,u1(n),...,uk(n)) where
# ui is a solution to the ith recurrence equation.
   poltorec:=proc (p, lrec::list, lu::list, uofn, {computeini::boolean:=true})
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local u, n, k, tosubs, i, rec, uu, v, lcoef, lind, j, vars, linds, w, rorder, l, l2, m, jmax, inds, ini, lco, mon, ord, n0, pp, lmax, basis, back;
      if nops(lrec)<>nops(lu) then
         error "not the same number of elements: %1 and %2",lrec,lu end if;
      getname(uofn,u,n);
      k:=nops(lrec);
      tosubs:=subsop(4=NULL,proc() args end):
      userinfo(4,'gfun',"entering poltorec with args",args);
      for i to k do
         rec[i]:=formatrec([lrec[i],lu[i]],'uu','n',ini[i]);
         ord[i]:=nops(rec[i])-2;
         lco:=rec[i][nops(rec[i])];
         mon:=u[i](n+ord[i]);
         if ord[i]>0 then
            v:=convert(map(normal,[-rec[i][1]/lco,
                    seq(-rec[i][j]/lco*u[i](n+j-2),j=2..ord[i]+1)]),`+`)
         else v:=-normal(rec[i][1]/lco)
         end if;
         tosubs(mon):=v;
         lcoef[mon]:=[coeffs(v,[seq(u[i](n+j),j=0..ord[i]-1)],lind[mon])]
      end do;
      pp:=subs([seq(op(0,lu[i])=u[i],i=1..k)],p);
      w:=pp; lmax:=0;
      # a few checks on the polynomial
      for i to k do
         l2:=maxindex(w,u[i],n);
         if l2<>-infinity-n then
            lmax:=max(lmax,l2);
            for j from l2 by -1 to ord[i] do
               w:=subs(u[i](n+j)=subs(n=n+j-ord[i],
                                      tosubs(u[i](n+ord[i]))),w)
            end do
         end if
      end do;
      vars:={seq(seq(u[i](n+j),j=0..ord[i]-1),i=1..k)};
      if not type(w,'polynom'('ratpoly'('anything',n),vars)) then
         error "Not a polynomial: %1", p end if;
      basis:={seq(seq(u[i](n+j),j=0..ord[i]-1),i=1..k)};
      linds:={1,seq(u[i](n+ord[i]),i=1..k)};
      for rorder do # this loop includes an incremental Gaussian elimination
	 userinfo(3,'gfun',"computing and reducing shift of order",rorder);
         w:=collect(subs([seq(u[i](n+ord[i])=tosubs(u[i](n+ord[i])),
                              i=1..k)],w),vars,'distributed');
         w:=[coeffs(w,vars,'l')];l:=[l];
         w:=collect(add(w[j]*tosubs(l[j]),j=1..nops(w)),vars,
                    'distributed',normal);
         lco:=[coeffs(w,vars,'l')];
         l2:={l} minus linds;
         if l2={} then break end if;
         l:=[l];
         back:=evalb(l2 minus basis={});
         if not back then mon:=op(1,l2 minus basis) else mon:=l2[1] end if;
         member(mon,l,'m');
         tosubs(mon):=collect((h(n+rorder-1)-
	   add(lco[j]*tosubs(l[j]),j=1..m-1)-
	   add(lco[j]*tosubs(l[j]),j=m+1..nops(l)))/lco[m],vars,'distributed');
         lcoef[mon]:=[coeffs(tosubs(mon),vars,lind[mon])];
         basis:=basis union {lind[mon]};
         # back substitution
         if back then
            for i in linds do
               if member(mon,[lind[i]],'m') then
                  lind[i]:=[lind[i]];
                  tosubs(i):=collect(
                     add(lcoef[i][j]*lind[i][j],j=1..m-1)
                     +lcoef[i][m]*tosubs(mon)
                     +add(lcoef[i][j]*lind[i][j],j=m+1..nops(lcoef[i])),
                     vars,'distributed');
                  lcoef[i]:=[coeffs(tosubs(i),vars,evaln(lind[i]))]
               end if
            end do
         end if;
         linds:=linds union {mon};
         w:=subs(n=n+1,w)
      end do;
      # final equation
      w:=subs(h=u,collect(primpart(numer(h(n+rorder-1)-w)),
                          [seq(h(n+j),j=0..rorder-1)]));
      if not computeini then return w end if;
      ## initial conditions
      n0:=0;
      for i to k do
	  if assigned(ini[i]) then
	     n0:=max(n0,max(seq(op(op(1,j)),j=ini[i]))+1-ord[i])
	  fi
      od;
      n0:=max(n0+lmax+rorder-2,
		# singular part
		firstnonzero(subs(n=n-rorder+2,
                                         coeff(w,u(n+rorder-1),1)),n));
      jmax:=-1;
      for i to k do
         ini[i]:=`goodinitvalues/rec`(rec[i],op(0,lu[i]),
                                      op(lu[i]),ini[i],true,n0);
         inds:=indets(ini[i],_C['anything']);
         if inds<>{} then
            ini[i]:=subs([seq(inds[j]=_C[jmax+j],j=1..nops(inds))],ini[i]);
            jmax:=jmax+nops(inds)
         end if
      end do;
#      n1:=max(seq(op(op(1,i)),i=seq(op(ini[i]),i=1..k)),lmax+n0);
#      if n1>n0 then # this happens when one of the recurrence equations
#         # is valid only for large values of the index
#         jmax:=-1; # this has to be redone in case one of the ini was {}
#         for i to k do
#            ini[i]:=`goodinitvalues/rec`(rec[i],op(0,lu[i]),
#                                         op(lu[i]),ini[i],true,n1);
#            inds:=indets(ini[i],_C['anything']);
#            ini[i]:=subs([seq(inds[j]=_C[jmax+j],j=1..nops(inds))],ini[i]);
#            jmax:=jmax+nops(inds)
#         end do
#      end if;
      ini:=`union`(seq(subs(op(0,lu[i])=u[i],ini[i]),i=1..k));
      ini:=`goodinitvalues/rec`(formatrec([w,u(n)],'u','n'),u,n,
                                remove(has,subs(ini,{seq(u(i)=subs(n=i,pp),
                   i={seq(op(op(1,i)),i=ini)})}),{seq(u[i],i=1..k)}),true);
      if ini={} then w else {w,op(ini)} end if
   end proc: # poltorec

# rec+rec
#Input: two linear recurrences rec1 and rec2 in the variable uofn (u(n))
#Output: a linear recurrence satisfied by the sum of a solution of rec1 and
#   a solution of rec2.
### This was broken in Maple8 and thus replaced by copy-paste.
### `rec+rec`:=subs(poltodiffeq=poltorec,eval(`diffeq+diffeq`)):
   `rec+rec` := proc(eq1,eq2,yofz)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local y1,y2,y,z;
      getname(yofz,y,z);
      poltorec(y1(z)+y2(z),[subs(y=y1,eq1),subs(y=y2,eq2)],[y1(z),y2(z)],yofz)
   end proc:

# rec*rec
#Input: two linear recurrences rec1 and rec2 in the variable uofn (u(n))
#Output: a linear recurrence satisfied by the product of a solution of
#     rec1 by a solution of rec2.
### This was broken in Maple8 and thus replaced by copy-paste.
### `rec*rec` := subs(poltodiffeq=poltorec,eval(`diffeq*diffeq`)):
   `rec*rec` := proc(eq1,eq2,yofz, {ini::boolean:=true})
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local y1,y2,y,z;
      getname(yofz,y,z);
      poltorec(y1(z)*y2(z),[subs(y=y1,eq1),subs(y=y2,eq2)],[y1(z),y2(z)],yofz,
            'computeini'=ini)
   end proc:


# borel, invborel
# Input:  a linear recurrence or differential equation
#    u(n) or y(x) the variable
#    (optional) a flag 'diffeq' saying that it's a differential equation
#        by default it is a recurrence
# Output: the linear recurrence or differential equation in u(n) (or y(x))
#    satisfied by the sequence u(n)/n! in the borel case, u(n)*n! in the
#    invborel case. For differential equations, the equation is the
#    equation satisfied by the generating function of the borel/invborel
#    transform of the sequence of Taylor coefficients.
   invborel:=proc() borelinvborel(false,args) end proc:
   Laplace:=proc() borelinvborel(false,args) end proc:
   borel:=  proc() borelinvborel(true, args) end proc:

   borelinvborel := proc(borel,expr,aofn)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local a, b, n, rec2;
      if nargs=3 then
         getname(aofn,a,n);
         if borel then rec2:={n*b(n)=b(n-1),b(0)=1}
         else rec2:={b(n)=n*b(n-1),b(0)=1} end if;
         poltorec(a(n)*b(n),[expr,rec2],[a(n),b(n)],a(n))
      elif args[4]<>'diffeq' then error "invalid argument: %1",args[4]
      else
         rectodiffeq(procname(borel,
                              diffeqtorec(expr,aofn,a(n)),a(n)),a(n),aofn)
      end if
   end proc: # borelinvborel

# hadamardproduct
#Input: two linear differential equations eq1 and eq2 in the variable yofz
#Output: a linear differential equation satisfied by the Hadamard product
# of any solution of eq1 with any solution of eq2.
   hadamardproduct := proc(eq1,eq2,yofz)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local u, u1, u2, n, r1, r2, inds, i, j;
      r1:=diffeqtorec(eq1,yofz,u1(n));
      r2:=diffeqtorec(eq2,yofz,u2(n));
      inds:=indets(r1,_C['anything']) intersect indets(r2,_C['anything']);
      if inds<>{} then
         j:=max(op(map(op,
              indets(r1,_C['anything']) union indets(r2,_C['anything']))));
         r2:=subs([seq(inds[i]=_C[j+i],i=1..nops(inds))],r2)
      end if;
      rectodiffeq(poltorec(u1(n)*u2(n),[r1,r2],[u1(n),u2(n)],u(n)),u(n),yofz)
   end proc: # hadamardproduct

# algebraicsubs
#Input: a linear differential equation Deq in the variable yofz (which is y(z))
#  a polynomial eq in y and z
#  the variable y(z)
#  (optional) initial conditions for y(z) in the polynomial
#Output: a linear differential equation satisfied by f(y(z)) for any solution
# f of Deq and y of eq.
   algebraicsubs := proc(Deq,eq,yofz,inipol)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local y, z, deq, P, u, d, i, d1, k, A, C, Dg, g, j, ord_eqn, c, f, inhomog, eqn, inip, inid, deq0, ini, P0, v, deq1, P2, tosubs, lvars, reduce,Y, iszero, ord;
      P:=formatpoleq([args[2..nargs]],'y','z',inip); d:=degree(P,y);
      deq0:=subs(z=y,formatdiffeq([Deq,yofz],'y','z',inid)); d1:=nops(deq0)-2;
      if d=0 then error "%1 is not a polynomial in the variable %2",eq,y
      elif d=1 then # special case to speedup things
         P2:=-coeff(P,y,0)/coeff(P,y,1)-z;
         if P2=0 then return Deq
         elif not has(P2,z) then return
         	makediffeq(map(collect,subs(y=z+P2,deq0),z),y,z)
         elif d1=0 then return
         	makediffeq(subs(y=-coeff(P,y,0)/coeff(P,y,1),deq0),y,z)
         end if
      end if;
      g:=mygcdex(diff(P,y),P,y,'u');
      if has(g,y) then return algebraicsubs(Deq,normal(P/g),yofz) end if;
      Dg:=rem(-u/g*diff(P,z),P,y);
      g:=mygcdex(deq0[d1+2],P,y,'u');
      if has(g,y) then return algebraicsubs(Deq,normal(P/g),yofz) end if;
      deq:=map(rem,[seq(-deq0[i]*u/g,i=1..d1+1)],P,y);
      inhomog:=evalb(op(1,deq)<>0);
      # if inhomog then ord_eqn:=d*d1 else ord_eqn:=d*(d1+1) end if;
      # Previous order was wrong. Fixed BS Dec 94.
      if inhomog then ord_eqn:=d*(d1+1) else ord_eqn:=d*d1 end if;
      tosubs:=diff(f(y),[y$d1])=makediffeq(deq,f,y);
      lvars:=[seq(diff(f(y),[y$k]),k=0..d1-1)];
      eqn[0]:=f(y);
      reduce:=subs([_P=P,_Y=y],proc(Q) rem(Q,_P,_Y) end);
      for k to ord_eqn do
         eqn[k]:=collect(diff(eqn[k-1],z)+subs(tosubs,diff(eqn[k-1],y))*Dg,
                         lvars,'distributed',reduce)
      end do;
      tosubs:=[seq(diff(f(y),[y$k])=Y^(k+1),k=0..d1-1)];
      for k from 0 to ord_eqn do eqn[k]:=subs(tosubs,eqn[k]) end do;
      A:=Matrix(1..ord_eqn+1,1..ord_eqn,'storage'='sparse');
      C:=Vector(1..ord_eqn+1,'storage'='sparse');
      # f^{(i)}(g).g^j (i.e. F^i y^j) -> column j*d1+i+1, 0<=i<d1, 0<=j<d
      A[1,1]:=1;
      for k to ord_eqn+1 do
	for i from 0 to d1-1 do
	    c:=collect(coeff(eqn[k-1],Y,i+1),y);
            for j from 0 to d-1 do A[k,j*d1+i+1]:=coeff(c,y,j) end do
	end do
      end do;
      if inhomog then # g^j -> column d*d1+j, 0<j<d; g^0 -> C
         for k to ord_eqn+1 do
            c:=coeff(eqn[k-1],Y,0);
            for j to d-1 do A[k,d*d1+j]:=normal(coeff(c,y,j)) end do;
            C[k]:=coeff(c,y,0)
         end do
      end if;
      deq:=subs([seq(v[i]=diff(y(z),[z$i]),i=0..ord_eqn)],collect(primpart(
         numer(lindep(A,[seq(v[i]-C[i+1],i=0..ord_eqn)],z)),
         [seq(v[i],i=0..ord_eqn)]),[seq(v[i],i=0..ord_eqn)]));
      # initial conditions
      deq1:=formatdiffeq([deq,y(z)]);
      ord_eqn:=firstnonzero(indicialeq(deq1,z,z),z);
      if ord_eqn=0 then
      	ini:={} # singular case, don't look for initial conditions
      else
         ini:={};
         for i from 0 while has(inip,`@@D`(i,y,0)) do end do;
         P0:=convert(subs(inip,[seq(`@@D`(j,y,0)/j!*z^j,j=0..i-1)]),`+`);
         if i<ord_eqn then
            P:=subs(y=P0+z^i*y,P);
            inip:=algeqtoseries(P,z,y,ord_eqn-i,true);
            if nops(inip)=0 then
               if i>0 then inip:=P0
               else inip:=infinity end if
            elif nops(inip)>1 then inip:=P0 # do not choose
            else
               if not type(inip[1],series) and has(inip[1],O) then
                   ord:=op(op(indets(inip[1],specfunc('anything',O))));
                   if type(ord,'name') then ord:=1 else ord:=op(2,ord) fi;
                   if ord<ord_eqn-i then
                        inip:=algeqtoseries(P,z,y,2*(ord_eqn-i)-floor(ord),true)
                   fi
               fi;
               inip:=P0+z^i*inip[1];
               # this is due to a bug in V.3
               inip:=subs(seq(i=O(z^ceil(op(2,op(i)))),i=indets(inip,
                   'specfunc'('identical'(z)^'rational',O))),inip)
            end if
         else inip:=P0 end if;
         P0:=eval(inip,[O=0,z=0]);
         if P0=0 then
            inid,iszero:=`goodinitvalues/diffeq`(subs(y=z,deq0),y,z,
                                          inid,ord_eqn-1);
	    if iszero then return y(z) fi;
         elif P0=infinity then
            inid:={}
         else # forget the initial conditions which were given
            inid,iszero:=`goodinitvalues/diffeq`(map(collect,subs(y=P0+z,deq0),z),
                                          y,z,{},ord_eqn-1)
         end if;
         if inid<>{} and inip<>infinity then
            ini:=subs(O(0)=0,series(subs(v=inip-P0,convert(subs(inid,
#              [seq(`@@D`(j,y,0)*v^j/j!,j=0..ord_eqn-1),O(v^ord_eqn)])
#	       `@@D` should not be used here, since it breaks the desired subs. BS. Aug 05.
              [seq((D@@j)(y)(0)*v^j/j!,j=0..ord_eqn-1),O(v^ord_eqn)])
                                                           ,`+`)),z,infinity)); # O(0) --> 0 for maple8
            if not has(ini,O(1)) then ini:=convert(ini,polynom) end if
         end if;
         if type(ini,'series') then
            ini,iszero:=`goodinitvalues/diffeq`(deq1,y,z,
                {seq(`@@D`(j,y,0)=coeff(ini,z,j)*j!,
		    j=0..min(ord_eqn-1,op(nops(ini),ini)-1))});
	    if iszero then return y(z) fi
	 elif type(ini,'polynom'(algebraic,op(yofz))) then
            ini,iszero:=`goodinitvalues/diffeq`(deq1,y,z,
		    {seq(`@@D`(j,y,0)=coeff(ini,z,j)*j!,j=0..ord_eqn-1)});
	    if iszero then # could be a singular point
	         ini:={}
            fi
         elif hastype(ini,'identical'(z)^'rational') then ini:={}
         end if
      end if;
      if ini={} then deq else {deq,op(ini)} end if
   end proc: # algebraicsubs

   # expintalg
   #Input:
   #  a polynomial eq in y and z
   #  the variable y(z)
   #  (optional) initial conditions for y(z) in the polynomial
   #  (optional) constant of integration in int(y(z),z) when initial conditions
   #    are given.
   #Output: a linear differential equation satisfied by exp(int(y(z))) for any solution
   # y of eq.
   # This is not very different from algebraicsubs.
      expintalg := proc(eq,yofz,inipol,iniconst)
      option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
      local y, z, deq, P, u, d, i, d1, k, A, Dg, g, j, ord_eqn, eqn, inip, deq0, ini, P0, v, deq1;
         P:=formatpoleq([eq,yofz,`if`(nargs>=3,inipol,NULL)],'y','z',inip);
         d:=degree(P,y);
         if nargs=2 then inip:={} fi;
         if d=0 then error "%1 is not a polynomial in the variable %2",eq,y fi;
         g:=mygcdex(diff(P,y),P,y,'u');
         if has(g,y) then return procname(normal(P/g),args[2..-1]) end if;
         Dg:=rem(-u/g*diff(P,z),P,y);
         g:=mygcdex(deq0[d1+2],P,y,'u');
         if has(g,y) then return procname(normal(P/g),args[2..-1]) end if;
         ord_eqn:=d;
         eqn[0]:=1;
         for k to ord_eqn do
            eqn[k]:=rem(y*eqn[k-1]+diff(eqn[k-1],y)*Dg,P,y)
         end do;
         A:=Matrix(1..ord_eqn+1,1..ord_eqn);
         for k from 0 to ord_eqn do
             for j from 0 to ord_eqn-1 do A[k+1,j+1]:=coeff(eqn[k],y,j) end do
   	     end do;
         deq:=subs([seq(v[i]=diff(y(z),[z$i]),i=0..ord_eqn)],collect(primpart(
            numer(lindep(A,[seq(v[i],i=0..ord_eqn)],z)),
            [seq(v[i],i=0..ord_eqn)]),[seq(v[i],i=0..ord_eqn)]));
         ############ initial conditions #############################
         deq1:=formatdiffeq([deq,y(z)]);
         ord_eqn:=firstnonzero(indicialeq(deq1,z,z),z);
         if ord_eqn=0 then
         	ini:={} # singular case, don't look for initial conditions
         else
            ini:={};
            for i from 0 while has(inip,`@@D`(i,y,0)) do end do;
            P0:=convert(subs(inip,[seq(`@@D`(j,y,0)/j!*z^j,j=0..i-1)]),`+`);
            if i<ord_eqn then
               P:=subs(y=P0+z^i*y,P);
               inip:=algeqtoseries(P,z,y,ord_eqn-i,true);
               if nops(inip)=0 then
                  if i>0 then inip:=P0
                  else inip:=infinity end if
               elif nops(inip)>1 then inip:=P0 # do not choose
               else
                  inip:=series(P0+z^i*inip[1],z,ord_eqn+1);
                  # this is due to a bug in V.3
                  inip:=subs(seq(i=O(z^ceil(op(2,op(i)))),i=indets(inip,
                      'specfunc'('identical'(z)^'rational',O))),inip)
               end if
            else inip:=P0 end if;
            P0:=eval(inip,[O=0,z=0]);
            if inip<>infinity then
                inip:=int(inip,z);
                if nargs=4 then inip:=iniconst+inip fi;
                ini:=series(exp(inip),z,ord_eqn);
                ini:={seq(`@@D`(j,y,0)=coeff(ini,z,j)*j!,j=0..ord_eqn-1)}
            end if
         end if;
         if ini={} then deq else {deq,op(ini)} end if
      end proc: # expintalg

# the following code is used by the holexprtodiffeq function
# added by E. Murray, Feb, 1996

# a table referenced by the various holonomic functions giving the
# differential equations they satisfy and singularity information.
# Entries are of the form
# 'toto' = ( (y,z)->
# [diffeq in y(z), {singular points}, {[sing. point of diffeq,
# order of first derivative to take for initial condions at the point],...}] )
#
# the diffeq does NOT include initial conditions
# the singular points are the singular points of the function
# the entry for BesselJ is an example of a function whose diffeq has
# singularity
#
# To access this information
# you must call diffeqtable['toto'](y,z). This eliminates problems with
# y and z being local/global variables.

# for the functions which take 2 or more arguments (eg. the Bessel's),
# the assumption is that these extra argument comes first in the
# function call (i.e. toto(nu,x)), and in the entries of this table
# these extra values come last.
   diffeqtable := table([
      'exp' = ((y,z)->[diff(y(z),z) - y(z),{infinity},{}]),
      'ln' = ((y,z)->[diff(y(z),z)*z-1,{0,infinity},{}]),
      'sin' = ((y,z)->[diff(y(z),z,z) + y(z),{infinity},{}]),
      'cos' = ((y,z)->[diff(y(z),z,z) + y(z),{infinity},{}]),
      'sinh' = ((y,z)->[y(z) - diff(y(z),z,z),{infinity},{}]),
      'cosh' = ((y,z)->[y(z) - diff(y(z),z,z),{infinity},{}]),
      'arctan' = ((y,z)->[(1+z^2)*diff(y(z),z) - 1,{I,-I},{}]),
      'arctanh' = ((y,z)->[1+(z^2-1)*diff(y(z),z),{1,-1},{}]),
      'arccot' = ((y,z)->[(1+z^2)*diff(y(z),z) + 1, {I,-I},{}]),
      'arccoth' = ((y,z)->[1+(z^2-1)*diff(y(z),z),{1,-1},{}]),
      'arcsin' = ((y,z)->[ diff(y(z),z,z)*(1-z^2) - diff(y(z),z)*z,
                           {1,-1,infinity},{}]),
      'arcsinh' = ((y,z)->[z*diff(y(z),z)+(z^2+1)*diff(diff(y(z),z),z),
                           {I,-I,infinity},{}]),
      'arccos' = ((y,z)->[z*diff(y(z),z)+(z^2-1)*diff(diff(y(z),z),z),
                          {1,-1,infinity},{}]),
      'arccosh' = ((y,z)->[z*diff(y(z),z)+(z^2-1)*diff(diff(y(z),z),z),
                           {1,-1,infinity},{}]),
      'arccsch' = ((y,z)-> [(2*z^2+1)*diff(y(z),z)+(z^3+z)*diff(diff(y(z),z),z),
                            {0,I,-I},{}]),
      'arcsech' = ((y,z)-> [(2*z^2-1)*diff(y(z),z)+(z^3-z)*diff(diff(y(z),z),z),
                            {0,1,-1},{}]),
      'arccsc' = ((y,z)-> [ (2*z^2-1)*diff(y(z),z)+(z^3-z)*diff(diff(y(z),z),z),
                            {0,1,-1},{}]),
      'arcsec' = ((y,z)-> [ (2*z^2-1)*diff(y(z),z)+(z^3-z)*diff(diff(y(z),z),z),
                            {0,1,-1},{}]),
      'dilog' = ((y,z)->[diff(y(z),z)*z+(-z+z^2)*diff(diff(y(z),z),z)+1,
                         {0,infinity},{[1,1]}]),
      'erf' = ((y,z)->[2*z*diff(y(z),z)+diff(y(z),z,z), {infinity},{}]),
      'erfc' = ((y,z)->[2*z*diff(y(z),z)+diff(y(z),z,z),{infinity},{}]),
      'erfc2' = ((y,z,nu)->[diff(diff(y(z),z),z)+2*z*diff(y(z),z)-2*nu*y(z),
                            {infinity},{}]),
      'erfi' = ((y,t)->[-2*t*diff(y(t),t)+diff(diff(y(t),t),t),{infinity},{}]),
      'hypergeom' = proc(y,z,lnum,lden) option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
          local j, n, deq, sing, u;
          deq:=rectodiffeq({u(n+1)*(n+1)*mul(n+j,j=lden)
          	-u(n)*mul(n+j,j=lnum),u(0)=1},u(n),y(z));
#          if type(deq,'set') then deq:=op(select(has,deq,z)) end if;
          if nops(lnum)>nops(lden)+1 then sing:={0,infinity}
          elif nops(lnum)=nops(lden)+1 then sing:={1,infinity}
          else sing:={infinity} end if;
         [deq,sing,{[0,0]}] end,
      'polylog' = proc(y,z,a) option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
	local u,n,fracrat;
		if not type(a,integer) then error "not a holonomic expression" fi;
		if a<=0 then
		    fracrat:=z/(1-z);
		    to -a do fracrat:=z*diff(fracrat,z) od;
		    fracrat:=normal(diff(fracrat,z)/fracrat);
		    [{denom(fracrat)*diff(y(z),z)-numer(fracrat)*y(z),y(0)=0,D(y)(0)=1},{1},{[0,0]}]
		else
		    [rectodiffeq({n*(n+1)^a*u(n+1)-n^(a+1)*u(n),u(0)=0,u(1)=1},u(n),y(z)),{1},{[0,0]}]
		fi
	end,
      'ln' = ((y,z) -> [z*diff(y(z),z)-1,{0,infinity},{}]),
      'AiryAi' = ((y,z)->[diff(y(z),z,z)-z*y(z),{infinity},{}]),
      'AiryAi2' = proc(y,z,k::nonnegint) option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
          local deq;
          deq:=poltodiffeq(diff(y(z),[z$k]),
          	[diff(y(z),z,z)-z*y(z)],[y(z)],y(z));
          if type(deq,'set') then deq:=op(select(has,deq,y(z))) end if;
          [deq,{infinity},{}] end,
      'AiryBi' = ((y,z)->[diff(y(z),z,z)-z*y(z),{infinity},{}]),
      'AiryBi2' = proc(y,z,k::nonnegint) option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
                  local deq;
                     deq:=poltodiffeq(diff(y(z),[z$k]),[diff(y(z),z,z)-z*y(z)],[y(z)],y(z));
                     if type(deq,'set') then deq:=op(select(has,deq,y(z))) end if;
                     [deq,{infinity},{}] end,
      'AngerJ' = ((y,z,nu)->
            [z^2*diff(y(z),z,z)+z*diff(y(z),z)+(z^2-nu^2)*y(z)=
            	(z-nu)*sin(nu*Pi)/Pi,{infinity},{}]),
# these should work for nu an integer (both positive and negative) because
# maple will automatically evaluate BesselI(-nu,x)->BesselI(nu,x) and
# BesselJ(-nu,x) -> -BesselJ(nu,x) or BesselJ(nu,x) as required
# if there can be cases where this evaluation does -not- take place,
# then in those cases this code will barf.
      'BesselJ' = ((y,z,nu)->[z^2*'(D@@2)(y)(z)' + z*'D(y)(z)'+(z^2-nu^2)*y(z),
                              {infinity},{[0,nu]}]),
      'BesselI' = ((y,z,nu)->[z^2*'(D@@2)(y)(z)' + z*'D(y)(z)'-(z^2+nu^2)*y(z),
                              {infinity},{[0,nu]}]),
      'BesselY' = ((y,z,nu)->[z^2*'(D@@2)(y)(z)' + z*'D(y)(z)'+(z^2-nu^2)*y(z),
                              {0,infinity},{}]),
      'BesselK' = ((y,z,nu)->[z^2*'(D@@2)(y)(z)' + z*'D(y)(z)'-(z^2+nu^2)*y(z),
                              {0,infinity},{}]),
      'Chi' = ((y,z) -> [z*diff(diff(diff(y(z),z),z),z)+2*diff(diff(y(z),z),z)
                         -diff(y(z),z)*z,{0,infinity},{}]),
      'Ci' = ((y,z) -> [diff(y(z),z)*z+2*diff(diff(y(z),z),z)+
                        z*diff(diff(diff(y(z),z),z),z),{0,infinity},{}]),
      'Ei' = ((y,z) -> [(1-z)*diff(y(z),z)+z*diff(y(z),z,z),{0,infinity},{}]),
      'FresnelC' = ((y,z)->[Pi^2*z^3*diff(y(z),z)-
      	diff(diff(y(z),z),z)+z*diff(diff(diff(y(z),z),z),z),
      	{infinity},{[0,0]}]),
      'FresnelS' = ((y,z)->[Pi^2*z^3*diff(y(z),z)
      	-diff(diff(y(z),z),z)+z*diff(diff(diff(y(z),z),z),z),
      	{infinity},{[0,0]}]),
      'HankelH1' = ((y,z,nu)->[z^2*'(D@@2)(y)(z)' + z*'D(y)(z)'+(z^2-nu^2)*y(z),
                               {0,infinity},{}]),
      'HankelH2' = ((y,z,nu)->[z^2*'(D@@2)(y)(z)' + z*'D(y)(z)'+(z^2-nu^2)*y(z),
                               {0,infinity},{}]),
      'KelvinBer' = ((w,x,nu)->[x^4*diff(w(x),x$4)+2*x^3*diff(w(x),x$3)
      	-(1+2*nu^2)*(x^2*diff(w(x),x$2)-x*diff(w(x),x))+(nu^4
      	-4*nu^2+x^4)*w(x),{infinity},{[0,nu]}]),
      'KelvinBei' = ((w,x,nu)->[x^4*diff(w(x),x$4)+2*x^3*diff(w(x),x$3)-
      	(1+2*nu^2)*(x^2*diff(w(x),x$2)-x*diff(w(x),x))+(nu^4
      	-4*nu^2+x^4)*w(x),{infinity},{[0,nu]}]),
      'KelvinKer' = ((w,x,nu)->[x^4*diff(w(x),x$4)+2*x^3*diff(w(x),x$3)-
      	(1+2*nu^2)*(x^2*diff(w(x),x$2)-x*diff(w(x),x))+(nu^4
      	-4*nu^2+x^4)*w(x),{0,infinity},{}]),
      'KelvinKei' = ((w,x,nu)->[x^4*diff(w(x),x$4)+2*x^3*diff(w(x),x$3)-
      	(1+2*nu^2)*(x^2*diff(w(x),x$2)-x*diff(w(x),x))+(nu^4
      	-4*nu^2+x^4)*w(x),{0,infinity},{}]),
      'KelvinHer' = ((y,z,nu)->[(-4*nu^2+nu^4+z^4)*y(z)+(-2*z^2*nu^2
      	-z^2)*diff(diff(y(z),z),z)+(2*z*nu^2+z)*diff(y(z),z)
      	+diff(diff(diff(diff(y(z),z),z),z),z)*z^4
      	+2*diff(diff(diff(y(z),z),z),z)*z^3,{0,infinity},{}]),
      'KelvinHei' = ((y,z,nu)->[(-4*nu^2+nu^4+z^4)*y(z)
      	+(-2*z^2*nu^2-z^2)*diff(diff(y(z),z),z)+(2*z*nu^2+z)*diff(y(z),z)
      	+diff(diff(diff(diff(y(z),z),z),z),z)*z^4
      	+2*diff(diff(diff(y(z),z),z),z)*z^3,{0,infinity},{}]),
      'KummerM' = ((y,z,mu,nu)->[z*diff(y(z),z,z)+(nu-z)*
      	diff(y(z),z)-mu*y(z),{infinity},{[0,0]}]),
      'KummerU' = ((y,z,mu,nu)->[z*diff(y(z),z,z)+(nu-z)*
      	diff(y(z),z)-mu*y(z),{0,infinity},{}]),
      'LommelS1'=proc(y,z,mu::integer,nu)
          [z^2*diff(y(z),z,z)+z*diff(y(z),z)+(z^2-nu^2)*y(z)
          	-z^(mu+1),{infinity},{[0,0]}] end,
      'LommelS2'=proc(y,z,mu::integer,nu)
                    [z^2*diff(y(z),z,z)+z*diff(y(z),z)+(z^2-nu^2)
                    *y(z)-z^(mu+1),{0,infinity},{}] end,
      'Shi' = ((y,z) -> [z*diff(diff(diff(y(z),z),z),z)+2*diff(diff(y(z),z),z)
                         -diff(y(z),z)*z,{infinity},{[0,0]}]),
      'Si' = ((y,z) -> [diff(y(z),z)*z+2*diff(diff(y(z),z),z)+
                        z*diff(diff(diff(y(z),z),z),z),{infinity},{[0,0]}]),
      'Ssi' = ((y,z) -> [diff(y(z),z)*z+2*diff(diff(y(z),z),z)+
                         z*diff(diff(diff(y(z),z),z),z),{infinity},{[0,0]}]),
      'StruveH'=((y,z,nu::integer)->[z^2*diff(y(z),z,z)+z*diff(y(z),z)
      	+(z^2-nu^2)*y(z)=4*(z/2)^(nu+1)/Pi^(1/2)/
      	GAMMA(nu+1/2),{0,infinity},{}]),
      'StruveL'=((y,z,nu::integer)->[z^2*diff(y(z),z,z)+z*diff(y(z),z)
      	-(z^2+nu^2)*y(z)=4*(z/2)^(nu+1)/Pi^(1/2)/
      	GAMMA(nu+1/2),{0,infinity},{}]),
      'WeberE' = ((y,z,nu)->
                  [z^2*diff(y(z),z,z)+z*diff(y(z),z)+(z^2-nu^2)*y(z)=
                   ((nu-z)*cos(nu*Pi)-(nu+z))/Pi,{infinity},{}]),
      'WhittakerM'=((y,z,mu,nu)->[diff(y(z),z,z)+(-1/4+mu/z
      	+(1/4-nu^2)/z^2)*y(z),{0,infinity},{}]),
      'WhittakerW'=((y,z,mu,nu)->[diff(y(z),z,z)+(-1/4+mu/z+
      	(1/4-nu^2)/z^2)*y(z),{0,infinity},{}])#,
   ## Orthogonal polynomials & fcns
#      'LegendreP2'= ((y,x,v,u) ->
#        [(v*(v+1)*(1-x^2)-u^2)*y(x)-2*x*(1-x^2)*diff(y(x),x)+(1-x^2)^2*diff(y(x),x,x),
#                {1,-1,infinity},{}]),
#        'LegendreP'= ((y,x,v) ->
#          [(v*(v+1)*(1-x^2))*y(x)-2*x*(1-x^2)*diff(y(x),x)+(1-x^2)^2*diff(y(x),x,x),
#                  {1,-1,infinity},{}])
                               ]):

   `type/gfun/has2diffeqs2` := proc(x)
      member(x,{'AiryAi','AiryBi','erfc'})
   end proc:
   `type/gfun/has2diffeqs3` := proc(x)
      member(x,{'LegendreP'})
   end proc:

# given a holonomic expression, return the differential equation it
# satifies, giving initial conditions when we can
   holexprtodiffeq := proc(expr,yofx)
# global diffeqtable;
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local y,x,deqs, newexpr, funs,nf,i,j,sortbylen,eq,inits, pows, np, ini, deq, nbsub, ints;
      getname(yofx, y,x);
      if has(expr, y) then
         error "first argument %1 should not contain %2", expr, y
      elif not isholonomic(expr,x) then error
        "expression is not holonomic (or involves a function which is not implemented yet): %1",
        expr
      end if;
      if not has(expr, x) then
         return yofx - expr
      end if;
      sortbylen := (a,b) -> evalb(length(a)>=length(b));

      newexpr:=expr;
	  nbsub:=0;

	  # Int(f,x)
	  funs := select(has,indets(newexpr,'specfunc'('anything',Int)),x);
	  for i while funs<>{} do
         # find the longest one - guaranteed to be at the outermost level
		 nbsub:=nbsub+1;
         funs := sort([op(funs)],sortbylen);
         eq := procname(op(1,funs[1]),y(x));
		 # we do not deal with initial conditions yet
		 if type(eq,set) then eq:=op(remove(type,eq,`=`)) fi;
         deqs[nbsub] := subs(y(x)=diff(y[nbsub](x),x),eq);
         newexpr := subs(funs[1]=y[nbsub](x),newexpr);
	  	 funs := select(has,indets(newexpr,'specfunc'('anything',Int)),x)
      end do;

      # special case for exp(anything)^rational
      # added select(has,.,x). BS. Mar04.
      funs:=select(has,indets(newexpr,
	   'specfunc'('anything',exp)^'rational'),x);
      newexpr:=subs([seq(i=exp(op(2,i)*op(op(1,i))),i=funs)],newexpr);

      # exp(int(algebraic))
      funs:=[op(indets(newexpr,'specfunc'('anything',exp)))];
      funs:=remove(type,funs,function({'radfun','algfun'}));
      if not type(map(diff,map(op,funs),x),list({'radfun','algfun'})) then
           error "expression is not holonomic: %1", funs
      fi;
	  for i to nops(funs) do
		  nbsub:=nbsub+1;
		  deqs[nbsub]:=subs(y=y[nbsub],funtodiffeq(funs[i],y(x)));
		  newexpr:=subs(funs[i]=y[nbsub](x),newexpr)
	  od;

      # deal with holonomic functions f(expr)
      ## added select(has,.,x). BS. Mar04.
      funs := [op(select(has,remove(type,indets(newexpr,'function'),
		'RootOf'),x) minus {seq(y[i](x),i=1..nbsub)})];
	  for i to nops(funs) do
		  nbsub:=nbsub+1;
		  deqs[nbsub]:=subs(y=y[nbsub],funtodiffeq(funs[i],y(x)));
		  newexpr:=subs(funs[i]=y[nbsub](x),newexpr)
	  od;

      # special case for polynomial(x)^(freeof(x))
      ## added select(has,.,x). BS. Mar04.
      pows := remove(type,select(has,indets(newexpr,
		'polynom'('anything',x)^`gfun/free`(x)),x),'anything'^'rational');
      for i to nops(pows) do
		 nbsub:=nbsub+1;
         ini:=subs(x=0,op(1,pows[i]));
         deq:=op(1,pows[i])*diff(y[nbsub](x),x)
         	-op(2,pows[i])*diff(op(1,pows[i]),x)*y[nbsub](x);
         if ini=0 then deqs[nbsub]:=deq
         else deqs[nbsub]:={deq,y[nbsub](0)=ini^op(2,pows[i])}
         end if;
		 newexpr:=subs(pows[i]=y[nbsub](x),newexpr)
      end do;

      # pick out the non-rational algebraic expressions,
      # the bits that are rational in x will be handled by poltodiffeq
      funs  := remove(type,
           indets(newexpr, {'radfun'('anything',x),'algfun'('anything',x)}),
                      'ratpoly'('anything',x));
      for i while funs <> {} do
         # find the longest one - guaranteed to be at the outermost level
		 nbsub:=nbsub+1;
         funs := sort([op(funs)],sortbylen);
         eq := algfuntoalgeq(funs[1],y(x),'inits','algebraic');
         deqs[nbsub] := subs(y=y[nbsub],algeqtodiffeq(eq,y(x),inits));
         newexpr := subs(funs[1]=y[nbsub](x),newexpr);
         funs  := remove(type,
            indets(newexpr, {'radfun'('anything',x),'algfun'('anything',x)}),
                         'polynom'('anything',x));
      end do;

      poltodiffeq(newexpr, [seq(deqs[j],j=1..nbsub)],[seq(y[j](x),j=1..nbsub)],y(x))
   end proc: # holexprtodiffeq

# determine if an expression is holonomic
   isholonomic := proc(expr,x)
# global diffeqtable;
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local i;
      if not has(expr, x) then true
      elif type(expr, 'function') then
         if op(0,expr)='exp' then # special case for exp(int(algebraic))
            type(diff(op(expr),x),
                {'radfun'('anything',x),'algfun'('anything',x)})
		 elif type(expr,specfunc(anything,'Int')) and op(2,expr)=x then
			procname(op(1,expr),x)
         elif not type(expr, 'RootOf') then
            # holonomic function with argument algebraic in x
            evalb(assigned(diffeqtable[op(0,expr)]) and
                  type(op(nops(expr),expr),{'radfun'('anything',x),
                  'algfun'('anything',x)}))
         else
            # this will reject things like RootOf(arcos(_Z))
            type(op(1,expr),{'algfun'('anything',_Z),'radfun'('anything',_Z)});
         end if;
      elif type(expr, {'radfun'('anything',x),'algfun'('anything',x)}) then
         true
      elif type(expr, {'`+`','`*`'}) then
         for i to nops(expr) while procname(op(i,expr),x) do end do;
         evalb(i=nops(expr)+1);
      elif type(expr, '`^`') then
         # polynomial^nice
         type(op(1,expr),'polynom'('anything',x)) and
         not has(op(2,expr),x) or
         # holonomic^posint (i.e. multiplication)
         type(op(2,expr), 'posint') and procname(op(1,expr),x) or
         # exp(algebraic)^rational
         type(op(2,expr),'rational') and
         type(op(1,expr),'specfunc'({'radfun'('anything',x),
         	'algfun'('anything',x)},exp))
      else
         false
      end if
   end proc: # isholonomic

# Given a holonomic expression of the form fun(arg(s)), returns the
# differential equation it satisfies.
# Removed the following restriction (BS Aug.96).
## Returns an error if it encounters a singularity.

   funtodiffeq := proc(expr, yofx)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local x,y,fun,deq,lim,deq2,ord,initconds,i,initpts,tabref,nu,info,eq,
      pinit, limpt, newx, sing, issing;
# global diffeqtable;
      getname(yofx, y,x);
      fun := op(0,expr);
      if not has(expr, x) then return yofx - expr end if;
      tabref := fun;
      # things like erfc have 2 diff eqs, depending on the
      # number of arguments.  The second one is called erfc2.
      if type(tabref, `gfun/has2diffeqs2`) and nops(expr)=2 then
         tabref := cat(tabref,2)
      end if;
      # worry about having 2 or 3 arguments
      nu:=op(1..nops(expr)-1,expr);

      # invoking explicitly MultiSeries:-limit instead of limit prevents a bug
      # in the initial conditions for
      # gfun[holexprtodiffeq](sin(w[1]*x+cos(tt+Pi/4)),y(w[1]));
      lim := MultiSeries:-limit(op(nops(expr),expr),x=0, 'right');
      if has(lim,infinity) then lim := infinity end if;

      # exp(int(algebraic))
      if op(0,expr)=exp and not type(op(expr),{'radfun','algfun'}) then
              if lim <> infinity then
                 eq := algfuntoalgeq(diff(op(expr),x),yofx,'pinit','algebraic');
                 return expintalg(eq,yofx,pinit,lim)
              else
                 eq := algfuntoalgeq(diff(op(expr),x),yofx,'algebraic');
                 return expintalg(eq,yofx)
              end if;
       fi;

      info := diffeqtable[tabref](y,x,nu);
      deq := info[1];

      issing:=member(lim,info[2]);
      if lim = 0 then deq2:=deq
      elif lim <> infinity then
         # find diffeq for fun(lim+x)
         deq2 := algebraicsubs(deq, y-(lim+x),y(x));
      else
         # find diffeq for fun(1/x)
         deq2 := algebraicsubs(deq, y*x-1, y(x));
      end if;

      # how many initial conditions do we need?
      if issing or nops(expr)=2 and not type(nu, 'integer') then
         ord := 0  # we can't give any
      else
         ord := nops(formatdiffeq([deq2,yofx]))-2;
      end if;

      if lim = infinity then
	 limpt := 0;
	 newx := 1/x
      else
	 limpt := lim;
	 newx := x;
      end if;

      # where do we take them? - is the diff eq singular at this point?
      sing := select((a,l)->evalb(op(1,a)=l), info[3],lim);
      if sing = {} then
         initconds := inifromseries(fun(nu,limpt+newx),x,ord-1,y,0,{})
      else
         initpts := {seq(i+op(2,op(sing)),i=0..ord-1)};
         initconds := {seq(`@@D`(i,y,0)=limit(diff(fun(nu,newx),[x$i]),x=limpt),i=initpts)}
      end if;

      if not type(deq2,'set') then
         deq2 := {deq2};
      end if;
      deq2 := deq2 union simplify(initconds,infinity);

      if lim <> infinity then
         # find eq for op(expr), diff eq for fun(lim + (op(expr)-lim))
         eq := algfuntoalgeq(op(nops(expr),expr),yofx,'pinit','algebraic');
         # and modify the initial conditions of eq
         # y(0)=lim  in op(expr) becomes y(0)=0 in op(expr)-lim
         algebraicsubs(deq2,
             subs(y=y+lim,eq),y(x),{y(0)=0} union remove(has,pinit,y(0)));
      else
         # find eq for 1/ op(expr), diff eq for fun( 1/ 1/(op(expr)) )
         eq := algfuntoalgeq(1/op(nops(expr),expr),yofx,'algebraic');
         # y(0)=infinity in op(expr) becomes y(0)=0 in 1/op(expr)
         algebraicsubs(deq2,eq,y(x),{y(0)=0});
      end if;
   end proc: # funtodiffeq

# given an algebraic expression expr in the variable x, return a polynomial
# (possibly with algebraic number coefficients) in
# x and y which has that expression as a root
# iniconds (optional) - will be assigned the initial conditions of the equation
# if possible
# a possible 4th argument (or 3rd if there is no iniconds) determines the
# sort of coefficients of the resulting polynomial. The possible choices are
# 'rational' or 'algebraic'.  'rational' is the default, since this allows
# the user to obtain a "minimal" polynomial for an algebraic number, but
# internally we will always use 'algebraic'
   algfuntoalgeq :=proc (expr, yofx, iniconds)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local y,x,eq,ord,typ,findconds, inits;
      getname(yofx,y,x);
      # what is type of the resulting poly and do we need initial conditions?
      typ := 'rational';
      findconds := false;
      if nargs = 4 then
         typ := args[4];
         if typ <> 'algebraic' and typ <> 'rational' then
            error "type must be one of %1", {'rational','algebraic'}
         end if;
         findconds := true;
      elif nargs = 3 then
         if args[3] = 'rational' or args[3] = 'algebraic' then
            typ := args[3]
         else
            findconds := true
         end if;
      end if;

      eq := algfuntoalgeq2(expr,y,x,typ);

      if findconds then  # fill in initial conditions
         ord := degree(eq,y) +degree(lcoeff(eq,y),x);
	 inits:=inifromseries(expr,x,ord-1,y,0,{});
	 if has(inits,x) then # csgn #@!
	         iniconds:=NULL
         else iniconds:=inits
         fi
      end if;
      eq;
   end proc: # algfuntoalgeq

# given an algebraic expression in the variable x, return a polynomial
# with typ cofficients in
# x and y which has that expression as a root
   algfuntoalgeq2 := proc(expr, y, x, typ)
   option `Copyright (c) 1992-2008 by Algorithms Project, INRIA France. All rights reserved.`;
   local Y, p, replace, minpols, rads, rootofs, bases, sortbylen, i,
      inds,b,baseb,d, radtable,subslist,t,j,numreplace;

      inds := indets(expr);
      if has(inds,y) then
         error "expression %2 cannot contain %1", y, expr
      elif not convert(map(type, inds,
      	{'algfun'('anything'),'radfun'('anything')}),
                       `and`) then
      end if;
      if typ = 'algebraic' and type(expr,'polynom'('anything',x))
      or typ = 'rational' and type(expr,'polynom'('rational')) then
         y - expr
      else
         p := numer( y - expr);
         rads := indets(p, 'anything'^'fraction');
         rootofs := indets(p, 'RootOf');
         if typ = 'algebraic' then  # we can leave algebraic constants alone
            rads := select(has,rads,x);
            rootofs := select(has,rootofs,x);
         end if;
         bases := map2(op,1, rads);
         if nops(bases) <> nops(rads) then
            # we have the same base to more than one power
            for b in bases while nops(bases) <> nops(rads) do
               baseb := select((t,b)->evalb(op(1,t)=b), rads, b);
               if nops(baseb) > 1 then
                  # keep a list of the exponents for this base
                  radtable[b] := [seq(op(2,t),t=baseb)];
                  # replace the b^(i/j)'s by a single b^(1/lcm(j's))
                  d := ilcm(seq(op(2,op(2,t)), t=baseb));
                  rads := rads minus baseb union {b^(1/d)};
               end if;
            end do;
         end if;
         sortbylen := (a,b) -> evalb(length(a)>=length(b));
         # find the longest one - guaranteed to be at the outermost level
         replace := sort([op(rads), op(rootofs)],sortbylen);
         numreplace := nops(replace);
         replace := table(replace);

         # form the subs list
         for i to numreplace do
            if type(replace[i], 'RootOf') or
            not assigned(radtable[op(1,replace[i])]) then
               subslist[i] := replace[i]=Y[i]
            else
               # each f^(i/j) gets replace by f^(i/j) * lcm(j's for that f)
               subslist[i] := seq(op(1,replace[i])^j=
                                  Y[i]^(j*op(2,op(2,replace[i]))),
                                  j=radtable[op(1,replace[i])])
            end if
         end do;
         p := subs([ seq(subslist[i],i=1..numreplace)], p);

         # form a table of polys satisfied by each rad and RootOf
         minpols := table([seq(`algfuntoalgeq/formpoly`(replace[i],Y[i]),
                               i=1..numreplace)]);

         # replace the rads and rootofs in minpols by the corresponding vars
         for i from numreplace to 2 by -1 do  # most interior first
#            minpols := subs(replace[i]=Y[i], eval(minpols));
#            replace := subs(replace[i]=Y[i], eval(replace))
			 minpols:=subs(subslist[i],eval(minpols));
#			 replace:=subs(subslist[i],eval(replace))
         end do;

		 p:=numer(p);

         for i to numreplace do
            p := resultant(p, numer(minpols[i]), Y[i]);
         end do;

         # find the square-free part
         if type(p, 'polynom'('rational')) then
            quo(p, gcd(p, diff(p,y)),y);
         else
            quo(p, gcdex(p, diff(p,y), y), y);
         end if;

      end if;
   end proc: # algfuntoalgeq2

# given f^(p/q) return y^q - f^p, and given
# RootOf(f(x)) return f(y)
   `algfuntoalgeq/formpoly` := proc(t,Yi)
      if type(t,'RootOf') then
         frontend(subs,['_Z'=Yi,op(t)],[{`+`,`*`},{'_Z'=Yi}])
      else
         Yi^op(2,op(2,t))-op(1,t)^op(1,op(2,t))
      end if
   end proc:

   Parameters := proc(x)
      local old, glob, p, q, ii;

      if nargs > 1 then
         return op(map(procname,[args]));
      end if;
      if x::'`=`' then
         glob := CheckName(lhs(x));
         old := thismodule[glob];
         if member(glob,
                   {
                      #':-maxdegcoeff',
                      ':-maxordereqn',
                      #':-maxdegeqn',
                      #':-mindegcoeff',
                      #':-mindegeqn',
                      ':-minordereqn'
                   }) then
            if rhs(x) :: 'nonnegint' then
               p:=[exports(thismodule)];
               q:=[exports(thismodule,'instance')];
               member(glob,p,'ii');
               assign(q[ii],rhs(x));
               old;
            else
               error "invalid value for %1: %2", glob, rhs(x);
            end if;
         elif glob = ':-optionsgf' then
            if rhs(x) :: 'list'({'identical'('ogf'),'identical'('egf'),
                                 'identical'('revogf'),'identical'('revegf'),
                                 'identical'('lgdogf'),'identical'('lgdegf'),
                                 'identical'('Laplace')}) then
               optionsgf := rhs(x);
               old;
            else
               error "invalid value for %1: %2", glob, rhs(x);
            end if;
         else
            error "can't set version";
         end if;
      else
         thismodule[CheckName(x)];
      end if;
   end proc:

   CheckName := proc(x)
      local glob;
      glob := convert(x, '`global`');
      if not member(glob,
                    {
                       #':-maxdegcoeff',
                       ':-maxordereqn',
                       #':-maxdegeqn',
                       #':-mindegcoeff',
                       #':-mindegeqn',
                       ':-minordereqn',
                       ':-optionsgf',
                       ':-version'
                    }) then
         error "invalid option: %1", glob;
      else
         glob
      end if;
   end proc:
#####################################################################
`@@D` := proc(i,y,x)
if i = 0 then
	'y(x)'
elif i = 1 then
	'D(y)(x)'
else
	'`@@`(D,i)(y)(x)'
fi;
end:
#####################################################################

# This is used only in NumGfun
$include <infsolvepoly.mm>

######################## NumGfun #########################

# submodule definition
$include <NumGfun/main.mm>

end module:

unprotect('gfun:-diffeqtable'): # so that users can define their own functions
#unprotect('gfun:-maxdegcoeff'):
unprotect('gfun:-maxordereqn'):
#unprotect('gfun:-maxdegeqn'):
#unprotect('gfun:-mindegcoeff'):
unprotect('gfun:-minordereqn'):
#unprotect('gfun:-mindegeqn'):
unprotect('gfun:-optionsgf'):

#savelib(gfun,`type/gfun/free`,`type/gfun/has2diffeqs2`,`type/gfun/has2diffeqs3`,`type/gfun/identity`,`type/gfun/initeq`,'`gfun/rectoproc/symbol`');

# FIXME: comment polluer moins ?
#savelib(`convert/ndmatrix`, `type/ndmatrix`, `type/matrix_ring`, `value/Coeftayl`);

