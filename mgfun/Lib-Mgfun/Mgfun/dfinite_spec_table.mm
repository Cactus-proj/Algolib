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

# Univariate functions taken from gfun/diffeqtable and reshaped with
# the following code in order to be used with Mgfun
#
#T:=`gfun/diffeqtable`:
#L:=sort(map(op,[indices(T)])):
#L_diff:=select(i->nops([op(1,eval(T[i]))])=2,L);
#O_diff:=map(sort,subs([seq(diff(f(x),[x$i])=Dx^i,i=0..10)],
#    map(i->T[i](f,x)[1],L_diff)),Dx);
#for i to nops(L_diff) do
#    printf("%s=\n    [[diff],(x,Dx)->[[%a],tdeg(Dx)]],\n",L_diff[i],O_diff[i])
#end do;
#

dfinite_spec_table:= table([
NULL=`Copyright (c) 1999-2009 Frederic Chyzak and Cyril Germa, INRIA, France`,
AiryAi=
    [[diff],(x,Dx)->[[Dx^2-x],tdeg(Dx)]],
AiryBi=
    [[diff],(x,Dx)->[[Dx^2-x],tdeg(Dx)]],
Chi=
    [[diff],(x,Dx)->[[x*Dx^3+2*Dx^2-x*Dx],tdeg(Dx)]],
Ci=
    [[diff],(x,Dx)->[[x*Dx^3+2*Dx^2+x*Dx],tdeg(Dx)]],
# Presently, the second syntax Ei(n,x) is not recognized.
Ei=
    [[diff],(x,Dx)->[[x*Dx^2+(1-x)*Dx],tdeg(Dx)]],
FresnelC=
    [[diff],(x,Dx)->[[x*Dx^3-Dx^2+`Mgfun/Pi`^2*x^3*Dx],tdeg(Dx)]],
FresnelS=
    [[diff],(x,Dx)->[[x*Dx^3-Dx^2+`Mgfun/Pi`^2*x^3*Dx],tdeg(Dx)]],
Shi=
    [[diff],(x,Dx)->[[x*Dx^3+2*Dx^2-x*Dx],tdeg(Dx)]],
Si=
    [[diff],(x,Dx)->[[x*Dx^3+2*Dx^2+x*Dx],tdeg(Dx)]],
Ssi=
    [[diff],(x,Dx)->[[x*Dx^3+2*Dx^2+x*Dx],tdeg(Dx)]],
arccos=
    [[diff],(x,Dx)->[[(x^2-1)*Dx^2+x*Dx],tdeg(Dx)]],
arccosh=
    [[diff],(x,Dx)->[[(x^2-1)*Dx^2+x*Dx],tdeg(Dx)]],
arccot=
    [[diff],(x,Dx)->[[Dx+(1+x^2)/(2*x)*Dx^2],tdeg(Dx)]],
arccoth=
    [[diff],(x,Dx)->[[Dx-(1-x^2)/(2*x)*Dx^2],tdeg(Dx)]],
arccsc=
    [[diff],(x,Dx)->[[(x^3-x)*Dx^2+(2*x^2-1)*Dx],tdeg(Dx)]],
arccsch=
    [[diff],(x,Dx)->[[(x^3+x)*Dx^2+(2*x^2+1)*Dx],tdeg(Dx)]],
arcsec=
    [[diff],(x,Dx)->[[(x^3-x)*Dx^2+(2*x^2-1)*Dx],tdeg(Dx)]],
arcsech=
    [[diff],(x,Dx)->[[(x^3-x)*Dx^2+(2*x^2-1)*Dx],tdeg(Dx)]],
arcsin=
    [[diff],(x,Dx)->[[(1-x^2)*Dx^2-x*Dx],tdeg(Dx)]],
arcsinh=
    [[diff],(x,Dx)->[[(1+x^2)*Dx^2+x*Dx],tdeg(Dx)]],
arctan=
    [[diff],(x,Dx)->[[Dx+(1+x^2)/(2*x)*Dx^2],tdeg(Dx)]],
arctanh=
    [[diff],(x,Dx)->[[Dx-(1-x^2)/(2*x)*Dx^2],tdeg(Dx)]],
cos=
    [[diff],(x,Dx)->[[Dx^2+1],tdeg(Dx)]],
cosh=
    [[diff],(x,Dx)->[[-Dx^2+1],tdeg(Dx)]],
dilog=
    [[diff],(x,Dx)->[[(x-x^2)*Dx^3+(1-3*x)*Dx^2-Dx],tdeg(Dx)]],
erf=
    [[diff],(x,Dx)->[[Dx^2+2*x*Dx],tdeg(Dx)]],
erfc=
    [[diff],(x,Dx)->[[Dx^2+2*x*Dx],tdeg(Dx)]],
erfi=
    [[diff],(x,Dx)->[[Dx^2-2*x*Dx],tdeg(Dx)]],
ln=
    [[diff],(x,Dx)->[[x*Dx^2+Dx],tdeg(Dx)]],
sin=
    [[diff],(x,Dx)->[[Dx^2+1],tdeg(Dx)]],
sinh=
    [[diff],(x,Dx)->[[-Dx^2+1],tdeg(Dx)]]
    ]):

# Auxiliary table of multivariate special functions taken (the relations they
# satisfy are picked up in litterature).
spec_table_aux:=table([
`Mgfun/IDENTITY`=
    [[diff],(x,Dx)->[[x*Dx-1],tdeg(Dx)]],
`Mgfun/EXP`=
    [[diff],(x,Dx)->[[Dx-1],tdeg(Dx)]],
`Mgfun/POWER`=
    [[shift,diff],(n,x,Sn,Tn,Dx)->[[Sn-x,x*Dx-n,x*Tn-1],
        lexdeg([Tn],[Sn,Dx])]],
`Mgfun/GAMMA`=
    [[shift,parameter],(n,Sn,Tn,param)->[`if`(param>0,
            [Sn-n^param,expand((n-1)^param)*Tn-1],
            [n^(-param)*Sn-1,Tn-expand((n-1)^(-param))]),
        lexdeg([Tn],[Sn])]],
`Mgfun/qPOCHHAMMER`=
    [[qshift,qdilat,non_root_of_one,parameter],
        (n,x,Sn,Tn,Hx,Kx,q,qn,param)->[`if`(param>0,
            [Sn-(1-qn*x)^param,(q-qn*x)^param*Tn-q^param,
                (1-x)^param*Hx-(1-qn*x)^param,
                (q-qn*x)^(-param)*Kx-(q-x)^(-param)],
            [(1-qn*x)^(-param)*Sn-1,q^(-param)*Tn-(q-qn*x)^(-param),
                (1-qn*x)^(-param)*Hx-(1-x)^(-param),
                (q-x)^(-param)*Kx-(q-qn*x)^(-param)]),
            lexdeg([Tn,Kx],[Sn,Hx])]],

##### Obsolete: have been replaced with `Mgfun/qPOCHHAMMER`.
# q-calculus.
# `Mgfun/qfactorial`=
#     [[qshift,non_root_of_one],(n,Sn,Tn,q,qn)->[[Sn-1+q*qn,Sn*Tn-1],lexdeg([Tn],[Sn])]],
# `Mgfun/qbinomial`=
#     [[qshift,qshift,non_root_of_one],(n,m,Sn,Tn,Sm,Tm,q,qn,qm)->[[Sn+(-qm+qm*q*qn)/(qm-q*qn),Sm+(qm-qn)/(q*qm^2-qm),Sn*Tn-1,Sm*Tm-1],lexdeg([Tn,Tm],[Sn,Sm])]],
# `Mgfun/qpochhammer`=
#     [[qshift,qdilat,non_root_of_one],(n,x,Sn,Tn,Hx,Kx,q,qn)->[[Sn-(1-qn*x),Hx+(1-qn*x)/(x-1),Sn*Tn-1,Hx*Kx-1],lexdeg([Tn,Kx],[Sn,Hx])]],

# Orthogonal polynomials (refer to Abramowitz and Stegun).
HermiteH=
    [[shift,diff],(n,x,Sn,Tn,Dx)->[[Sn^2-2*x*Sn+2*(n+1),Dx^2-2*x*Dx+2*n,Sn*Dx-2*n-2,Sn*Tn-1],lexdeg([Tn],[Sn,Dx])]],
ChebyshevT=
    [[shift,diff],(n,x,Sn,Tn,Dx)->[[Sn^2-2*x*Sn+1,(1-x^2)*Dx^2-x*Dx+n^2,(1-x^2)*Sn*Dx+(n+1)*x*Sn-(n+1),Sn*Tn-1],lexdeg([Tn],[Sn,Dx])]],
ChebyshevU=
    [[shift,diff],(n,x,Sn,Tn,Dx)->[[Sn^2-2*x*Sn+1,(1-x^2)*Dx^2-3*x*Dx+n*(n+2),(1-x^2)*Sn*Dx+(n+1)*x*Sn-(n+2),Sn*Tn-1],lexdeg([Tn],[Sn,Dx])]],
# LegendreP is rewritten in terms of JacobiP.
JacobiP=
    [[shift,shift,shift,diff],(n,a,b,x,Sn,Tn,Sa,Ta,Sb,Tb,Dx)->[[
    (1-x^2)*Dx^2+(b-a-(a+b+2)*x)*Dx+n*(n+a+b+1),
    (n+a/2+b/2+1)*(1-x)*Sa+(n+1)*Sn-(n+a+1),
    (n+a/2+b/2+1)*(1+x)*Sb-(n+1)*Sn-(n+b+1),
    2*(n+2)*(n+a+b+2)*(2*n+a+b+2)*Sn^2
        -(2*n+a+b+3)*(a^2-b^2+(2*n+a+b+2)*(2*n+a+b+4)*x)*Sn
        +2*(n+a+1)*(n+b+1)*(2*n+a+b+4),
    (2*n+a+b+2)*(1-x^2)*Dx*Sn
        -(n+1)*((a-b)-(2*n+a+b+2)*x)*Sn-2*(n+a+1)*(n+b+1),
    (2*n+a+b+3)*Sn-(n+a+b+2)*Sa*Sn+(n+b+1)*Sa,
    (2*n+a+b+3)*Sn-(n+a+b+2)*Sb*Sn-(n+a+1)*Sb,
    Sn*Tn-1,Sa*Ta-1,Sb*Tb-1],lexdeg([Tn,Ta,Tb],[Sn,Sa,Sb,Dx])]],
LaguerreL=
    [[shift,shift,diff],(n,a,x,Sn,Tn,Sa,Ta,Dx)->[[Sn^2-((2*n+a+3-x)/(n+2))*Sn+(n+a+1)/(n+2),Sn+Sa-Sn*Sa,x*Dx^2+(a+1-x)*Dx+n,x*Sn*Dx-(n+1)*Sn+n+a+1,Sn*Tn-1,Sa*Ta-1],lexdeg([Tn,Ta],[Sn,Sa,Dx])]],
GegenbauerC=
    [[shift,shift,diff],(n,a,x,Sn,Tn,Sa,Ta,Dx)->[[
        #Sn^2-2*x*((n+a+1)/(n+2))*Sn+(n+2*a)/(n+2),
        2*a*(1-x^2)*Sa+(n+1)*x*Sn-(2*a+n),
        (1-x^2)*Dx^2-(2*a+1)*x*Dx+n*(n+2*a)
        ,(1-x^2)*Sn*Dx+(n+1)*x*Sn-(n+2*a),Sn*Tn-1,Sa*Ta-1],
    lexdeg([Tn,Ta],[Sn,Sa,Dx])]],
    
# Other special functions
# Abramowitz and Stegun (9.1.27)
BesselJ=
    [[shift,diff],(n,x,Sn,Tn,Dx)->[[1+Sn^2-2*(n+1)*Sn/x,Dx+Sn-n/x,Sn*Tn-1],lexdeg([Tn],[Sn,Dx])]],
BesselY=
    [[shift,diff],(n,x,Sn,Tn,Dx)->[[1+Sn^2-2*(n+1)*Sn/x,Dx+Sn-n/x,Sn*Tn-1],
        lexdeg([Tn],[Sn,Dx])]],

# Abramowitz (9.6.26)
BesselI=
    [[shift,diff],(n,x,Sn,Tn,Dx)->[[x^2*Dx^2+x*Dx-(x^2+n^2),Dx-Sn-(n/x),
        Sn*Tn-1],lexdeg([Tn],[Sn,Dx])]],
BesselK=
    [[shift,diff],(n,x,Sn,Tn,Dx)->[[x^2*Dx^2+x*Dx-(x^2+n^2),Dx+Sn-(n/x),
        Sn*Tn-1],lexdeg([Tn],[Sn,Dx])]],

# Abramowitz (9.1.27)
HankelH1=
    [[shift,diff],(n,x,Sn,Tn,Dx)->[[1+Sn^2-2*(n+1)*Sn/x,Dx+Sn-n/x,Sn*Tn-1],
        lexdeg([Tn],[Sn,Dx])]],
HankelH2=
    [[shift,diff],(n,x,Sn,Tn,Dx)->[[1+Sn^2-2*(n+1)*Sn/x,Dx+Sn-n/x,Sn*Tn-1],
        lexdeg([Tn],[Sn,Dx])]],

# The Kelvin functions (KelvinBer, KelvinBei, KelvinKer, KelvinKei,
# KelvinHer, and KelvinHei) are replaced by linear combination of the
# above prior to any calculation, and so are not described here.

# Abramowitz 13.1.1 and KummerM -> 13.4.1, 13.4.2, 13.4.8
#                        KummerU -> 13.4.15, 13.4.16, 13.4.21
KummerM=
    [[shift,shift,diff],(a,b,x,Sa,Ta,Sb,Tb,Dx)->[[
    x*Dx^2+(b-x)*Dx-a,
    (2*a-b+x+2)*Sa-(a+1)*Sa^2+(b-a-1),
    (b+1)*(-b-x)*Sb+x*(b-a+1)*Sb^2+b*(b+1)
    ,b*(a+x)+x*(a-b)*Sb-a*b*Sa,
    Dx-(a/b)*Sa*Sb,Sa*Ta-1,Sb*Tb-1
    ],lexdeg([Ta,Tb],[Sa,Sb,Dx])]],

KummerU=
    [[shift,shift,diff],(a,b,x,Sa,Ta,Sb,Tb,Dx)->[[
    x*Dx^2+(b-x)*Dx-a,
    (b-2*a-x-2)*Sa+(a+1)*(a+2-b)*Sa^2+1,
    x*Sb^2+(-b-x)*Sb+(b-a)
    ,a+x+a*(b-a-1)*Sa-x*Sb,
    Dx+a*Sa*Sb,Sa*Ta-1,Sb*Tb-1
    ],lexdeg([Ta,Tb],[Sa,Sb,Dx])]],

# Higher transcendental functions (Erdelyi)
LommelS1=
    [[shift,shift,diff],(a,b,x,Sa,Ta,Sb,Tb,Dx)->[[Sa^3-x*Sa^2+((a+2)^2-b^2)*Sa-x*((a+1)^2-b^2),Sa*Sb*Dx+(b+1)/x*Sa*Sb-(1+a+b),
# Sa*Dx+b/x*Sa+(b-a)*Sb,
-x^3/(a+1)*Dx^3+x^2*(1-3/(a+1))*Dx^2+x*(1-(1+x^2-b^2)/(a+1))*Dx+x^2*(1-2/(a+1))-b^2,Sa*Ta-1,Sb*Tb-1],lexdeg([Ta,Tb],[Sa,Sb,Dx])]],
LommelS2=
    [[shift,shift,diff],(a,b,x,Sa,Ta,Sb,Tb,Dx)->[[Sa^3-x*Sa^2+((a+2)^2-b^2)*Sa-x*((a+1)^2-b^2),Sa*Sb*Dx+(b+1)/x*Sa*Sb-(1+a+b),
# Sa*Dx+b/x*Sa+(b-a)*Sb,
-x^3/(a+1)*Dx^3+x^2*(1-3/(a+1))*Dx^2+x*(1-(1+x^2-b^2)/(a+1))*Dx+x^2*(1-2/(a+1))-b^2,Sa*Ta-1,Sb*Tb-1],lexdeg([Ta,Tb],[Sa,Sb,Dx])]],
# This is confirmed by the representation
#
#      LommelS1(a,b,x)=
#               x^a*x/(a-b+1)/(a+b+1)*hypergeom([1,(a-b+3)/2],[(a+b+3)/2],-x^2/4),
#
# from which I could compute:
#LommelS1=
#    [[shift,shift,diff],(a,b,x,Sa,Ta,Sb,Tb,Dx)->[[
#-4*x*(b+3)*(b+2)*(b+1)*(b+1+a)*Dx+(b+3)*(b+1+a)*(a*x^2+4*b^3+12*b^2+8*b-b*x^2
#-x^2)+2*(b+2)*(-b-1+a)*(-a*x^2+2*a*b^2+6*a+8*a*b+30*b+18+14*b^2+2*b^3)*Sb^2+x^
#2*(b+1)*(-b-1+a)*(a-3-b)*Sb^4,
#-x*(b+1+a)*Dx+b*(b+1+a)+x*(-b-1+a)*Sb^2*Dx+(b+2)*(-b-1+a)*Sb^2,
#-2*x*(b+1)*(b+1+a)*Dx+(b+1+a)*(2*a*b+2*a+2-x^2+2*b)+x^2*(-b-1+a)*Sb^2+(2*b+2)*Sa^2,
#2*x^2*(b+1)*Dx^2-2*x*(b+1)*(b+a)*Dx+b*x^2+2*b+2*a*b^2-a*x^2+2*b^2+x^2+2*a*b+x^2*(-b-1+a)*Sb^2
#,Sa*Ta-1,Sb*Tb-1],lexdeg([Ta,Tb],[Sa,Sb,Dx])]],

# Abramowitz 12.1.9 (H) 12.2.4 (L) and Maple's description
StruveH=
    [[shift,diff],(n,x,Sn,Tn,Dx)->[[-Sn^2-Sn*Dx+((n+1)/x+x/(2*n+3))*Sn+x/(2*n+3)*Dx-n/(2*n+3),Sn^3-(2*(n+2)/x+x/(2*n+5))*Sn^2+(1+(2*n+2)/(2*n+5))*Sn-x/(2*n+5),Sn*Tn-1],lexdeg([Tn],[Sn,Dx])]],
StruveL=
    [[shift,diff],(n,x,Sn,Tn,Dx)->[[Sn^2-Sn*Dx+((n+1)/x-x/(2*n+3))*Sn+x/(2*n+3)*Dx-n/(2*n+3),-Sn^3-(2*(n+2)/x-x/(2*n+5))*Sn^2+(1+(2*n+2)/(2*n+5))*Sn-x/(2*n+5),Sn*Tn-1],lexdeg([Tn],[Sn,Dx])]],

# Maple's description and Higher Transcendental Functions (Erdelyi)
WeberE=
    [[shift,diff],(n,x,Sn,Tn,Dx)->[[Sn^2+2*Sn*Dx-1,Sn^3+Sn^2*Dx-(n+2)/x*Sn^2-Sn-Dx+n/x,Sn*Tn-1],lexdeg([Tn],[Sn,Dx])]],
AngerJ=[[shift,diff],(n,x,Sn,Tn,Dx)->[[Sn^2+2*Sn*Dx-1,Sn^3+Sn^2*Dx-(n+2)/x*Sn^2-Sn-Dx+n/x,Sn*Tn-1],lexdeg([Tn],[Sn,Dx])]],

# Basing on Abramowitz 13.1.31, 13.4.32 (M), 13.4.33 (W),
# 13.4.28 (M), 13.4.30 (W), we build two auxilliary functions to
# enable the calculation of Ann(W) and Ann(M) without sqrt(Sa) and
# sqrt(x):
#
#        WhittakerM(or W)(a,b,x)=`Mgfun/WhittakerM`(or W)(2a,2b,sqrt(x))
#    
#`Mgfun/WhittakerM`=
#    [[shift,shift,diff],(a,b,x,Sa,Ta,Sb,Tb,Dx)->[[1/(x^2)*Dx^2-1/(x^3)*Dx-1+a/(2*x^2)+(1-b^2)/x^4,-(b+1)*Sa^2-x*Sa*Sb+1+b,
#    #(3+a+b)*Sa^4-2(b-x^2)*Sa^2-(3+a-b),
#    x*Dx-(1+a+b)*Sa^2-x^2+a],lexdeg([Ta,Tb],[Sa,Sb,Dx])]],
#`Mgfun/WhittakerW`=[[shift,shift,diff],(a,b,x,Sa,Ta,Sb,Tb,Dx)->[[1/(x^2)*Dx^2-1/(x^3)*Dx-1+a/(2*x^2)+(1-b^2)/x^4,Sa^2-x*Sa*Sb+1/2*(a+b+1),Sa^4+(a-x^2-2)*Sa^2+1/4*(a-b+1)*(a+b+1),
#    #x*Dx+2*Sa^2-x^2+a,
#    Sa*Ta-1,Sb*Tb-1],
#    lexdeg([Ta,Tb],[Sa,Sb,Dx])]]
############# These two functions are in fact translated in terms of
############# hypergeom and KummerU.

NULL]):

# Calculate the Groebner basis of each function described in
# spec_table_aux and add it in dfinite_spec_table.

for i in map(op,[indices(spec_table_aux)]) do
lprint(i);
    signature:=spec_table_aux[i][1];
    non_param_number:=nops(subs({'parameter'=NULL,'non_root_of_one'=NULL},
        signature));
    # Number of parameter or non_root_of_one (they can't appear in the
    # same signature).
    param_number:=nops(signature)-non_param_number;
    inverse_number:=nops(select(member,signature,
        {'shift','qshift','qdilat'}));
    if param_number<>0 and has(signature,'parameter') then
        dfinite_spec_table[i]:=spec_table_aux[i]
    else
        the_proc:=spec_table_aux[i][2];
        args_exprseq:=op(1,eval(the_proc));
        q_calculus:=false;
        # Points to the x associated to j-th element in signature.
        x_offset:=0;
        # Points to the dx associated to j-th element in signature.
        dx_offset:=non_param_number;
        # Points to the q associated to q-calculus elements in
        # signature.
        q_offset:=2*non_param_number+inverse_number+1;
        # Points to the qx associated to j-th element in signature.
        qx_offset:=q_offset;
        # (Each pointer points to last "eaten"; q is considered already
        # "eaten", which is why q_offset=qx_offset at this point.)
        for j to nops(signature) do
            if signature[j]='diff' then
                x_offset:=x_offset+1;
                dx_offset:=dx_offset+1;
                type_table[j]:='diff'=[
                    args_exprseq[dx_offset],args_exprseq[x_offset]]
            elif signature[j]='shift' then
                x_offset:=x_offset+1;
                dx_offset:=dx_offset+2;
                type_table[j]:='`shift+dual_shift`'=[
                    args_exprseq[dx_offset-1..dx_offset],
                    args_exprseq[x_offset]]
            elif signature[j]='parameter' then
                type_table[j]:=NULL
            elif signature[j]='non_root_of_one' then
                type_table[j]:=NULL;
                q_calculus:=true
            elif signature[j]='qshift' then
                x_offset:=x_offset+1;
                dx_offset:=dx_offset+2;
                qx_offset:=qx_offset+1;
                type_table[j]:='`qdilat+dual_qdilat`'=[
                    args_exprseq[dx_offset-1..dx_offset],
                    args_exprseq[qx_offset]=
                        args_exprseq[q_offset]^args_exprseq[x_offset]]
            elif signature[j]='qdilat' then
                x_offset:=x_offset+1;
                dx_offset:=dx_offset+2;
                qx_offset:=qx_offset+1;
                type_table[j]:='`qdilat+dual_qdilat`'=[
                    args_exprseq[dx_offset-1..dx_offset],
                    args_exprseq[x_offset],
                    args_exprseq[q_offset]]
            else
                error "wrong argument in table"
            end if
        end do;
        type_list:=seq(type_table[j],j=1..nops(signature));
        # Definition of the algebra.
        descr:=the_proc(args_exprseq);
        Alg:=Ore_algebra:-skew_algebra(type_list,comm={`if`(q_calculus,q,NULL),
            `if`(has(descr[1],`Mgfun/Pi`),`Mgfun/Pi`,NULL)});
        TOrd:=Groebner:-MonomialOrder(Alg,descr[2]);
        GB:=Groebner:-Basis(descr[1],TOrd);
        hdim:=Groebner:-HilbertDimension(GB,TOrd);
        if hdim=0 then
            dfinite_spec_table[i]:=[spec_table_aux[i][1],
                subs([
                    _X=op(1,eval(spec_table_aux[i][2])),
                    _GB=GB,
                    _TORD=descr[2]
                ],_X->[_GB,_TORD])]
        else
            printf("Warning, %s not stored (wrong non-zero dimension %a)\n",
                i,hdim)
        end if
    end if
end do:
