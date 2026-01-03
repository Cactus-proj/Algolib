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

# Both following functions are called with a name as argument.
`Holonomy/new_dx`:=proc()
    local dx;
    option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`,remember;
    dx
end proc:
`Holonomy/new_qx`:=proc()
    local qx;
    option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`,remember;
    qx
end proc:

# This procedure is still under development.  In particular, it has
# the following weaknesses:
#
# 1. THE q IS NOT RECOGNIZED IF NOT PRESENT IN A q^x:
#
# > recognize_operator_algebra({f(x+1)-q^x*f(x)});
#                                   user_system
#
# > recognize_operator_algebra({f(q*x)-q^x*f(x)});
#                                   user_system
#
# > recognize_operator_algebra({f(q*x)-f(x)});    
# Error, (in unknown) incorrect number of extra arguments in select
#
# 2. MULTI-BASIC q-CALCULUS IS NOT RECOGNIZED
#
recognize_operator_algebra:=proc(sys)
    local sysD,indN,indF,indFnoD,indP,args_of_F,shift_names,
        qcalc_pairs,qcalc_names,i,j,q,expon_names,args_of_diff,
        fun_indets,q_indets,qdilat_indets,qshift_indets,shift_indets,
        diff_indets,L,dx,qx,opts,Alg,OP,
        convert_functions,convert_powers,final_sys,
        user_system;
    option `Copyright (c) 1996-2005 Frederic Chyzak, INRIA, France`;
    # Normalize the input.  The expansion is necessary to recognize
    # that q^(n+k+1)=q^n*q^k*q (particular important for the
    # substitution at the end of the code).
    sysD:=expand(convert(sys,'D'));
    # Analyse the system to search for:
    #        o variable names (indN);
    #        o function calls (indF);
    #        o function calls except for derivatives (indFnoD);
    #        o symbolic powers (indP);
    #        o names that are exponents (expon_names).
    indN:=indets(sysD,'name');
    indF:=indets(sysD,'function');
    indFnoD:=map(f->op(op(0,f))(op(f)),select(has,indF,'D'))
        union remove(has,indF,'D');
    indP:=remove(type,indets(expand(sysD),'name'^'anything'),'name'^'posint');
    expon_names:=map(op,map(indets,map2(op,2,indP),'name'));
    # Set of all shifted, q-shifted... arguments of the functions
    # calls.
    args_of_F:=map(op,indF);
    # From the set of possibly shifted and q-shifted variables, work
    # out the set of q's involved in q-calculus operators.
    # qcalc_names becomes a set of lists [x,q] where x has a
    # q-commutation with a q-operator.
# Temporarily (?) work around a bug in type/&+.
#    shift_names:=indets(select(type,args_of_F,'name'&+'posint'),'name');
    shift_names:=indets(select(type,args_of_F,
        'Or'('name'&+'posint','posint'&+'name')),'name');
    qcalc_pairs:=map2(map,op@indets,map(convert,select(type,args_of_F,
# Temporarily (?) work around a bug in type/&*.
#        {'name','name'^'posint'}&*'name'),'set'));
        Or('name'&*'name','name'&*'name'^'posint','name'^'posint'&*'name')),'set'));
    qcalc_names:={};
    for i to nops(qcalc_pairs) do for j from i+1 to nops(qcalc_pairs) do
        q:=qcalc_pairs[i] intersect qcalc_pairs[j];
        if nops(q)=1 then
            qcalc_names:={map(op,[qcalc_pairs[i] minus q,q])}
                union qcalc_names;
            qcalc_names:={map(op,[qcalc_pairs[j] minus q,q])}
                union qcalc_names
        end if
    end do end do;
    # Extract values at which derivatives are evaluated (i.e.,
    # diff(f(u(x)),x) yields u(x)).
    args_of_diff:={};
    for i in indF minus indFnoD do
        OP:=op([0,0],i);
        OP:=`if`(
            OP='D',D[1],
            `if`(op(0,OP)=`@@` and op(1,OP)='D',D[1$op(2,OP)],
                OP));
        args_of_diff:=args_of_diff union {seq(op(j,i),j=OP)}
    end do;
    # End of the analysis: we easily obtain:
    #        o functions involved in the system (fun_indets);
    #        o q's used for q-calculus (q_indets);
    #        o variables involved in q-dilations (qdilat_indets);
    #        o variables involved in q-shifts (qdilat_indets);
    #        o variables involved in shifts;
    #        o variables with respect to which derivations are taken.
    fun_indets:=map(op,map2(op,0,indF minus indFnoD))
        union map2(op,0,indFnoD);
    q_indets:=map2(op,2,qcalc_names) union map2(op,1,indP);
    qdilat_indets:=map2(op,1,qcalc_names);
    qshift_indets:=shift_names intersect expon_names;
    shift_indets:=shift_names minus expon_names;
    diff_indets:=indets(args_of_diff,'name');
    # Reject several types of functional equations.
    L:=[fun_indets,qdilat_indets,qshift_indets,shift_indets,
        diff_indets];
    for i to nops(L) do for j from i+1 to nops(L) do
        if L[i] intersect L[j]<>{} then
            error "too complicated functional equations",i,j,L[i],L[j]
        end if
    end do end do;
    if fun_indets={} then
        error "no function present in the input"
    end if;
    if nops(fun_indets)>1 then
        error "multiple functions not dealt with yet"
    end if;
    # Build the Ore algebra according to the previously guessed
    # commutation.
    dx:=table();
    qx:=table();
    opts:=NULL;
    # In the case of qdilat operators, we use the syntax:
    #
    #        qdilat=[Hx,x,q]
    #
    for i in qdilat_indets do
        dx[i]:=`Holonomy/new_dx`(i);
# Il faudra choisir le bon q !
        opts:=opts,'qdilat'=[dx[i],i,op(q_indets)]
    end do;
    # In the case of qshift operators, we can't use the syntax:
    #
    #        qshift=[Sn,q^n]
    #
    # because the Groebner package will not be able to handle the
    # corresponding algebras properly.  Instead, we use the syntax:
    #
    #        qdilat=[Sn,Q=q^n]
    #
    # for a newly created Q.
    for i in qshift_indets do
        dx[i]:=`Holonomy/new_dx`(i);
        qx[i]:=`Holonomy/new_qx`(i);
# Il faudra choisir le bon q !
        opts:=opts,'qdilat'=[dx[i],qx[i]=op(q_indets)^i]
    end do;
    for i in shift_indets do
        dx[i]:=`Holonomy/new_dx`(i);
        opts:=opts,'shift'=[dx[i],i]
    end do;
    for i in diff_indets do
        dx[i]:=`Holonomy/new_dx`(i);
        opts:=opts,'diff'=[dx[i],i]
    end do;
    Alg:=Ore_algebra:-OA_Internals:-skew_algebra(opts,'comm'=indN
        minus `union`(qdilat_indets,qshift_indets,shift_indets,diff_indets));
    convert_functions:={};
    for i in indF do
        if has(i,'D') then
            OP:=op([0,0],i);
            OP:=`if`(
                OP='D',D[1],
                `if`(op(0,OP)=`@@` and op(1,OP)='D',D[1$op(2,OP)],
                    OP));
            OP:=mul(dx[op(j,i)],j=OP)
        else
            OP:=1
        end if;
        OP:=OP*convert(map(proc(s,dx)
            local x;
            option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
            global name;
            x:=op(indets(s,'name'));
            dx[x]^(s-x)
        end,select(type,[op(i)],`+`),dx),`*`);
        OP:=OP*convert(map(proc(p,dx,q_indets)
            local x,power,the_q;
            option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
            global name,`^`;
            x:=indets(p,'name');
            the_q:=op(x intersect q_indets);
            power:=select(has,p,the_q);
            dx[op(x minus q_indets)]^`if`(type(power,`^`),op(2,power),1)
        end,select(type,[op(i)],`*`),dx,q_indets),`*`);
        convert_functions:=convert_functions union {i=OP}
    end do;
# Il faudra choisir le bon q !
    convert_powers:={seq(op(q_indets)^i=qx[i],i=qshift_indets)};
    # It remains to determine the function name and its arguments.
    L:={seq(op(op(0,i))(op(i)),i=select(has,indF,'D')),op(indFnoD)};
    L:=subs(map(q->q=1,q_indets),L);
    L:=map(proc(func)
            option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
            global name;
            map2(select,type,func,'name')
        end,L);
    if nops(L)<>1 then
        error "unable to determine the function or its arguments",L
    end if;
    final_sys:=collect(subs(convert_functions union convert_powers,
        collect(sysD,{'D',op(op(L))},'distributed')),Alg["right_indets"],
        'distributed',expand);
    user_system["algebra"]:=Alg;
    user_system["expr_to_dx"]:=convert_functions;
    user_system["expr_to_qx"]:=convert_powers;
    user_system["qx_to_expr"]:=map(
        proc(eq)
            option `Copyright (c) 1996-2002 Frederic Chyzak, INRIA, France`;
            op(2,eq)=op(1,eq)
        end,convert_powers);
    user_system["system"]:=final_sys;
    user_system["type"]:="system";
    user_system["function"]:=op(L);
    user_system
end proc:
