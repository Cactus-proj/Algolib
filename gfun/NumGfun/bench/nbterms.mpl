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

interface(quiet=true);
kernelopts(assertlevel=2);

with(gfun); with(NumGfun);

infolevel[bounds] := 10;
infolevel[nth_term] := 10;
infolevel[settings] := 10;

precfactor := 2;
NumGfun:-Settings:-precision_factor := precfactor;

nbt := proc(f, a,
{ deq := diffeqtohomdiffeq(holexprtodiffeq(f(z),y(z)),y(z)),
testname := subs(z=a, f(z)) })
    local rec, val, err, digits;
    global results, prec;
    print("Test : ", testname);
    val := evaldiffeq(deq,y(z),[0,a],prec);
    checkval := evaldiffeq(deq,y(z),[0,a],prec*precfactor);
    err := abs(evalf[prec*precfactor](val - checkval));
    digits := `if`(err=0, infinity, floor(-log10(err)));
    results := [[testname, prec, digits]];
    ASSERT(digits >= prec);
end proc;

trace(nbt);

for prec in [100, 1000] do
  print("Precision : ", prec);
  nbt(z -> arctan(z),1/2);
  nbt(z -> arctan(z),3/4);
  nbt(z -> cos(z)/(1-z), 1/3);
  nbt(
    z->-MathieuSPrime(0,1,1/2*Pi)/(MathieuS(0,1,1/2*Pi)*MathieuCPrime(0,1,1/2*Pi)-MathieuSPrime(0,1,1/2*Pi)*MathieuC(0,1,1/2*Pi))*MathieuC(0,1,arccos(z))+1/(MathieuS(0,1,1/2*Pi)*MathieuCPrime(0,1,1/2*Pi)-MathieuSPrime(0,1,1/2*Pi)*MathieuC(0,1,1/2*Pi))*MathieuCPrime(0,1,1/2*Pi)*MathieuS(0,1,arccos(z)),
    1/3,
    deq = {(1-z^2)*diff(y(z),z,z)-z*diff(y(z),z)+2*(1-2*z^2)*y(z), y(0)=1, D(y)(0)=0});
  nbt(z -> exp(z/(1-z^2)), 1/3);
  nbt(z -> erf(z/(1-z)), 1/3);
  nbt(z -> exp(z),-100);
  nbt(z -> AiryAi(z),4*I+4);
end do;

print(results);
