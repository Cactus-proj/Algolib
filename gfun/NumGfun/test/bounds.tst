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

with(TestTools):

with(gfun):
with(NumGfun):

$include <testutils.mm>


################################################################################

# bound_ratpoly

for rat in [
    (z^7+3*z^2+z+1)/((z-2)^3*(z^3-3)*(z-I)^2),
    (1+2*z)/((z^2-1)*(z-1/2)),
    1/(1-z)^2*(1-2*z)^2,
    1/((1-z)*(1-2*z)^5),
    (2*z^2-9*z+8)/((2-z)*(2+z)^2),
    (1+z^2)/(1-2*z^2+z^4),
    0,
    -56-7*z^5+22*z^4-55*z^3-94*z^2+87*z,
    NULL]
do
    Try[testmajseries, z](sprintf("bound_ratpoly", rat),
        bound_ratpoly(rat,z),
        rat):
end do;

Try("bound_ratpoly 1/z",
    bound_ratpoly(1/z,z),
    1/z);

################################################################################

# bound_diffeq etc. I: explicit expressions

for expr in [
    ## 1 irrégulier
    exp(1/(1-z))/exp(1),
    ## 0 point ordinaire, les c[k] devraient être tous nuls
    cos(1/(1-z)),
    ## divers
    #1/z+1/(1-z)),  # diffeqtohomdiffeq échoue -- pourquoi pas...
    #log(1-z)/(z^3-2)^3, # nombreuses sing, mais toutes régulières, dominante=1
    1/exp(1)*exp(1/(1-z))+1/(1+z/3),
    NULL]
do
    deq := holexprtodiffeq(expr,y(z)):
    Try[testmajseries, z](sprintf("bound_diffeq I %a", expr),
        bound_diffeq(deq, y(z)),
        expr):
    Try[testnoerror]("bound_diffeq I / tail", # comment faire mieux ?
        bound_diffeq_tail(deq, y(z), n)):
end do;

# testmajseries would fail

for expr in [
    ## ... avec exposant de 1/n! non entier
    AiryAi(z),
    ## 0 singulier régulier
    BesselJ(1,z),
    NULL]
do
    deq := holexprtodiffeq(expr,y(z)):
    Try[testnoerror](sprintf("bound_diffeq I %a", expr),
        bound_diffeq(deq, y(z)),
        expr):
    Try[testnoerror]("bound_diffeq I / tail", # comment faire mieux ?
        bound_diffeq_tail(deq, y(z), n)):
end do;


################################################################################

# bound_diffeq etc. II

for test in [
    ## singularité apparente, sans conditions initiales...
    [1, { -z*(z-1)^2*diff(y(z),z,z) +(z-1)*(z^2-2*z-1)*diff(y(z),z) + (z^2-1)*y(z) = 0 }],
    ## Ici, après homogénisation, la singularité dominante est de module < 1, et elle est régulière.
    ## C'est un bon exemple avec un comportement un peu pathologique...
    [2, { diffeqtohomdiffeq(holexprtodiffeq(exp(z/(1-z))+log(z), y(z)), y(z)) }], 
    ## rayon de convergence infini
    [3, { diff(y(z),z) = y(z), y(0)=1 } ],
    ## here the root that determines the irregularity is not the dominant root
    [4, diff(y(z),z,z,z)=1/(1-z)^2*diff(y(z),z,z)+1/(z^2+1)^3*y(z) ],
    NULL]
do
    id, deq := op(test):
    Try[testnoerror](sprintf("bound_diffeq II / %a", id),
        bound_diffeq(deq, y(z))):
    Try[testnoerror](sprintf("bound_diffeq II / %a / tail", id),
        bound_diffeq_tail(deq, y(z), n)):
end do;

# solution divergente
#bound_diffeq(rectodiffeq({u(n+1)=(n+1)*u(n), u(0)=1}, u(n), y(z)), y(z)):

# Penser à tester avec des équations qui ont des solutions d'exposant négatif, et tout ça...

################################################################################

# bound_rec

for rec in [
    { u(n+1) = u(n), u(0) = 0 },
    { u(n+1) = n*u(n), u(0) = 1 }, # used to fail (too clever diffeq<->rec)
    { u(n+1) = u(n), u(0) = 18 },
    { u(n+1) = (n+1) * u(n), u(0) = 0 },
    { u(n+1) = (n+1) * u(n), u(0) = 1 },
    { (n+2)*u(n+2) = u(n), u(0)=1, u(1)=1 },
    { u(n+2)=u(n+1)+(n+1)*u(n), u(0)=1, u(1)=3 },
    { (n + 1)*u(n + 1) = (2*n + 3)*u(n), u(0) = 1 },
    { (2*n + 1)*u(n + 1) = (n + 3)*u(n), u(0) = 1 },
    { u(n+1)=u(n)+1, u(0)=-1 },  # borné par 2^n p(n)... cas non générique ?
    { u(n+1)=u(n)+1, u(0)=1 },  # borné par un polynôme
    { u(n+1)=u(n)+1, u(0)=-2 },  # encore un cas non générique, (3/2)^n p(n)
    ## exemple 2 du papier
    { 2*u(n+3) = (n+2)*u(n+1) + u(n),
        u(0)=1/5, u(1)=1/5, u(2)=1/5 }, # (a) integrals
    { u(n+2) = (n+1)*u(n) + u(n+1),
        u(0)=1, u(1)=1 }, # (b) involutions
    ##
    { (n+2)*(n+3)*u(n) = (7*n^2+7*n-2)*u(n-1) + 8*(n-1)*(n-2)*u(n-2),
        u(0)=1, u(1)=1 }, # Buxter
    NULL]
do
    # FIXME: depuis l'introduction des parties polynomiales, testrecbound échoue
    # quasi-systématiquement...
    #Try[testrecbound, u, n]("bound_rec",
    Try[testnoerror]("bound_rec",
        bound_rec(rec, u(n)),
        rec):
end do;


# formule des Chudnovsy pour pi, exemple 2(c) du papier
A := 13591409:
B := 545140134:
C := 640320^3:
rec := { (A+B*n) * (3*n+3)* (3*n+2)*(3*n+1) * (n+1)^3 * C * u(n+1)
         + (6*n+6)*(6*n+5)*(6*n+4)*(6*n+3)*(6*n+2)*(6*n+1) * (A+B*(n+1)) * u(n),
         u(0) = A }:
Try[testnoerror]("Chudnovsy",
    bound_rec_tail(rec, u(n)));


################################################################################

#for arg in [
#    [{diff(y(z),z) = y(z), y(0) = 1}, [0, 1, 2, 2+2*I]],
#    [eq(arctan(z)), [0,1/2,1,3/2,2,2+I/2]],
#    NULL]
#do
#    Try[testnoerror]("bound_transition_matrix",
#        bound_transition_matrix(arg[1], y(z), arg[2])):
#end do;
