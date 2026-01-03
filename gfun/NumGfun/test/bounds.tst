# Copyright (C) 1991--2013 by INRIA.
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
    Try[testmajseries, z](sprintf("bound_diffeq with ratpoly", rat),
        bound_diffeq(y(z)=rat,y(z)),
        rat):
end do;

Try("bound_ratpoly 1/z",
    bound_ratpoly(1/z,z),
    1/z);

################################################################################

# bound_diffeq etc. I: explicit expressions

for expr in [
    ## 1 is irregular singular
    exp(1/(1-z))/exp(1),
    cos(1/(1-z)),
    ## divers
    log(1-z)/(z^3-2)^3,
    1/exp(1)*exp(1/(1-z))+1/(1+z/3),
    NULL]
do
    deq := holexprtodiffeq(expr,y(z)):
    Try[testmajseries, z](sprintf("bound_diffeq I %a", expr),
        bound_diffeq(deq, y(z)),
        expr):
    Try[testnoerror]("bound_diffeq I / tail", # simple way to test the result?
        bound_diffeq_tail(deq, y(z), n)):
end do;

## used to fail due to a bad rounding although the non-homogeneous version
## worked
Try[testmajseries, z]("regression",
    bound_diffeq(
        {-2*z*diff(y(z),z)+(-1-z^2)*diff(y(z),`$`(z,2)), y(0) = 0, D(y)(0) = 1},
        y(z)),
    arctan(z));

Try[testerror]("bound_diffeq testerror",
    bound_diffeq(1+(-z+z^2)*y(z), y(z)));  # No power series sol

# testmajseries would fail

for expr in [
    ## noninteger Gevrey order
    AiryAi(z),
    ## regular singular point at the origin
    BesselJ(1,z),
    NULL]
do
    deq := holexprtodiffeq(expr,y(z)):
    Try[testnoerror](sprintf("bound_diffeq I %a", expr),
        bound_diffeq(deq, y(z)),
        expr):
    Try[testnoerror]("bound_diffeq I / tail",
        bound_diffeq_tail(deq, y(z), n)):
end do;


################################################################################

# bound_diffeq etc. II

for test in [
    ## apparent singularity, no initial values
    [1, { -z*(z-1)^2*diff(y(z),z,z) +(z-1)*(z^2-2*z-1)*diff(y(z),z) + (z^2-1)*y(z) = 0 }],
    ## apparent dominant singularity at z=-0.618033...
    ## => the current version of bound_diffeq does *not* return a tight bound
    ## (but we can desingularize the equation)
    [2, { diffeqtohomdiffeq(holexprtodiffeq(exp(z/(1-z))+log(z), y(z)), y(z)) }], 
    ## no finite singularity
    [3, { diff(y(z),z) = y(z), y(0)=1 } ],
    ## here the root that determines the irregularity is not the dominant root
    [4, diff(y(z),z,z,z)=1/(1-z)^2*diff(y(z),z,z)+1/(z^2+1)^3*y(z) ],
    ## non-positive dominant sing
    [5, (1-z^3)*diff(y(z),z) = y(z)],
    ## Used to loop up to Settings:-max_indicial_eq_tail_index
    [6, -z^2*(z-5)*diff(diff(diff(y(z),z),z),z) -
        z*(2*z-15)*diff(diff(y(z),z),z) + (-z^3+
        5*z^2+25*z-120)*diff(y(z),z)-(z-5)^2*y(z)],
    NULL]
do
    id, deq := op(test):
    Try[testnoerror](sprintf("bound_diffeq II / %a", id),
        bound_diffeq(deq, y(z))):
    Try[testnoerror](sprintf("bound_diffeq II / %a / tail", id),
        bound_diffeq_tail(deq, y(z), n)):
end do;

# This one should return a divergent series after printing a warning.  I don't
# know how to test for this behaviour.
#bound_diffeq(rectodiffeq({u(n+1)=(n+1)*u(n), u(0)=1}, u(n), y(z)), y(z)):

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
    { u(n+1)=u(n)+1, u(0)=-1 },
    { u(n+1)=u(n)+1, u(0)=1 },  # borné par un polynôme
    { u(n+1)=u(n)+1, u(0)=-2 },
    ## Example 2 from Mezzarobba & Salvy (2010)
    { 2*u(n+3) = (n+2)*u(n+1) + u(n),
        u(0)=1/5, u(1)=1/5, u(2)=1/5 }, # (a) integrals
    { u(n+2) = (n+1)*u(n) + u(n+1),
        u(0)=1, u(1)=1 }, # (b) involutions
    ##
    { (n+2)*(n+3)*u(n) = (7*n^2+7*n-2)*u(n-1) + 8*(n-1)*(n-2)*u(n-2),
        u(0)=1, u(1)=1 }, # Baxter
    ## this used to trigger a bug related to multiplicities in dominant_root
    { 25*u(n)+(-30*n-36-6*n^2)*u(n+2)+(128+14*n^3+72*n^2+160*n+n^4)*u(n+4),
        u(0) = 1, u(1) = 0, u(2) = 3/4, u(3) = 0},
    NULL]
do
    Try[testrecbound, u, n]("bound_rec",
        bound_rec(rec, u(n)),
        rec):
end do;


## Example 2(c) from Mezzarobba & Salvy (2010): Chudnovsky & Chudnovsky's
## formula for computing π
A := 13591409:
B := 545140134:
C := 640320^3:
rec := { (A+B*n) * (3*n+3)* (3*n+2)*(3*n+1) * (n+1)^3 * C * u(n+1)
         + (6*n+6)*(6*n+5)*(6*n+4)*(6*n+3)*(6*n+2)*(6*n+1) * (A+B*(n+1)) * u(n),
         u(0) = A }:
Try[testnoerror]("Chudnovsy",
    bound_rec_tail(rec, u(n)));

################################################################################

# degenerate cases: deqs of order zero, one-term recs

testgeqwithindets := proc(computed, expected)
    global _C, z;
    local inds;
    inds := indets(expected);
    if indets(computed) = {} and inds = {} then
        evalb(computed >= expected)
    elif member(_C, inds) then
        type(computed, 'linear'(_C));
            #and evalb(degree(computed, z) >= degree(expected, z));
    elif member(z, inds) then
        testmajseries(computed, expected, z);
    else
        false
    end if;
end proc:

for data in [
    # order zero
    [y(z),          0,  0],
    [z^2*y(z),      0,  0],
    [(z^2+1)*y(z),  0,  0],
    # deqs corresponding to one-term recs
    [diff(y(z), z), _C, _C],
    [{diff(y(z), z, z), y(0)=1}, _C*(1+z), _C],
    [{diff(y(z), z, z), y(0)=1, D(y)(0)=42}, z+42, 43],
        # purerectodiffeq((n^2-9)*u(n+6), u(n), y(z));
    [27*y(z)-11*diff(y(z),z)*z+diff(diff(y(z),z),z)*z^2, _C, _C],
    NULL ]
do
    deq, expected_bound, expected_sum_bound := op(data);
    Try[testgeqwithindets]("degenerate bound_diffeq",
        bound_diffeq(deq, y(z)),
        expected_bound);
    Try[testgeqwithindets]("degenerate bound_diffeq_tail sum",
        eval(bound_diffeq_tail(deq, y(z), 0), z=1),
        expected_sum_bound);
    Try("degenerate bound_diffeq_tail zero",
        bound_diffeq_tail(deq, y(z), 20),
        0);
end do;

Try("degenerate bound_diffeq",
    has(bound_diffeq_tail(
        27*y(z)-11*diff(y(z),z)*z+diff(diff(y(z),z),z)*z^2, y(z), 6), _C),
    true);

################################################################################

# catch hardcoded variable names


Try[testnoerror]("varnames",
    bound_ratpoly(1/(u^2+3*u+1)^2, u));

deq := (1+w^2)*diff(g(w),w)-1;

Try[testnoerror]("varnames",
    bound_diffeq(deq, g(w)));

Try[testnoerror]("varnames",
    bound_diffeq_tail(deq, g(w), k));

rec := d(i+2)=i^2*d(i);

Try[testnoerror]("varnames",
    bound_rec(rec, d(i)));

Try[testnoerror]("varnames",
    bound_rec_tail(rec, d(i), j));
