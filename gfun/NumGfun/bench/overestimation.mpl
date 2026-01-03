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

# Check how many superfluous digits we compute due to overestimation of the
# various errors.  See also the comments in settings.mm for more on how this
# works.

interface(quiet=true):
kernelopts(assertlevel=2):

with(gfun): with(NumGfun):

precfactor := 2;  # overestimations by a factor larger than this are truncated
NumGfun:-Settings:-precision_ini := proc(N) precfactor*N end proc:

nbt := proc(f, a,
{ deq := diffeqtohomdiffeq(holexprtodiffeq(f(z),y(z)),y(z)),
testname := subs(z=a, f(z)) })
    local rec, val, err, digits, checkval;
    global results, prec;
    print("Test : ", testname);
    val := evaldiffeq(deq,y(z),[0,a],prec);
    checkval := evaldiffeq(deq,y(z),[0,a],prec*precfactor);
    err := abs(evalf[prec*precfactor](val - checkval));
    digits := `if`(err=0, infinity, floor(-log10(err)));
    results := [ [
            "testcase" = testname,
            "requested precision" = prec,
            "correct digits" = digits],
        op(results)];
    ASSERT(digits >= prec);
end proc:

trace(nbt);

results := []:

for prec in [100, 1000] do
  print("Precision : ", prec);
  nbt(z -> arctan(z),1/2);
  nbt(z -> arctan(z),3/4);
  nbt(z -> cos(z)/(1-z), 1/3);
#  nbt(
#    z->-MathieuSPrime(0,1,1/2*Pi)/(MathieuS(0,1,1/2*Pi)*MathieuCPrime(0,1,1/2*Pi)-MathieuSPrime(0,1,1/2*Pi)*MathieuC(0,1,1/2*Pi))*MathieuC(0,1,arccos(z))+1/(MathieuS(0,1,1/2*Pi)*MathieuCPrime(0,1,1/2*Pi)-MathieuSPrime(0,1,1/2*Pi)*MathieuC(0,1,1/2*Pi))*MathieuCPrime(0,1,1/2*Pi)*MathieuS(0,1,arccos(z)),
#    1/3,
#    deq = {(1-z^2)*diff(y(z),z,z)-z*diff(y(z),z)+2*(1-2*z^2)*y(z), y(0)=1, D(y)(0)=0});
  nbt(z -> exp(z/(1-z^2)), 1/3);
  nbt(z -> erf(z/(1-z)), 1/3);
  nbt(z -> exp(z),-100);
  nbt(z -> AiryAi(z),4*I+4);
end do;

print(results):
