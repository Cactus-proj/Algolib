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

rnd := rand(10000..20000);

# fabrique rapidement des nombres d'approximativement q chiffres
rng := proc(size)
    rnd()^iquo(size,4)
end proc;

randmat := proc(r,e)
    eval(Matrix([[''rng(e)''$r]$r]))
end proc;

ntest := 20;
    
kernelopts(opaquemodules=false); # waksman_product is not exported

tests_produit := proc(q, r)
    local m1, m2, mi1, mi2, mult, mwak, minner, mla, msqm, mmvm, t;
    global
        reswak, resinner, resla, ressqm, resmvm;

    m1 := randmat(r,q): m2:= randmat(r,q): 

    mult := gfun:-NumGfun:-waksman_product(r):
    gc();
    t := time();
    mwak := Matrix(r, r);
    for i from 1 to ntest do
      mult(m1, m2, mwak)
    end do;
    reswak[q, r] := (time() - t)/ntest*1000;

    gc();
    t := time();
    for i from 1 to ntest do
      mmvm := mvMultiply(m1,m2);
    end do;
    resmvm[q, r] := (time() - t)/ntest*1000;

    gc();
    t := time();
    for i from 1 to ntest do
      mla := LinearAlgebra:-LA_Main:-MatrixMatrixMultiply(m1,m2,inplace=false,outputoptions=[]);
    end do;
        # see ?LinearAlgebra,Efficient
    resla[q,r] := (time() - t)/ntest*1000;
    
#    mi1 := convert(m1,matrix); mi2 := convert(m2, matrix);
#    gc();
#    t := time();
#    minner := inner(mi1,mi2);
#    resinner[q, r] := time() - t;

#    if r = 2 then
#        t := time();
#        msqm := squarematrixmul(m1,m2,2);
#        ressqm[q,r] := time() - t;
#    end if;

end proc;

qlist := [(1000*i)$i=1..20];
rlist := [2,3,4,8];
reslist := [reswak, resla, resmvm];

for q in qlist do
  for r in rlist do
    tests_produit(q,r);
  end do;
end do;

#with(plots);
#
#pl := proc(r)
#    Statistics:-PointPlot(
#        [ seq([ seq(res[q,r], q=qlist) ], res = reslist) ],
#        color = [blue, black, red],
#        style = line,
#        font=[TIMES,ROMAN,6])
#end proc;
#
#plotsetup(ps,plotoptions=`color,width=12cm,height=10cm,portrait,noborder,leftmargin=2cm,bottommargin=1cm`);
#
#for r in rlist do
#    interface(plotoutput=`wak1000-`||r||`.eps`);
#    pl(r);
#end do;
