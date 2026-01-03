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

$include <ApplyOpr_AlgSubs.mi>

# APPLYOPR(df,[x,y,z],Dx^alpha*Dy^beta*Dz^gamma,Alg) denotes
# diff(df(x,y,z),[x$alpha,y$beta,z$gamma]).
APPLYOPR:=proc(df,X,DX,Alg)
    local r,s,cf_list,tm_list,i;
    option `Copyright (c) 1997-2009 Frederic Chyzak, INRIA, France`,remember;
    global DFinite,term_order,list,distributed;
    if type(df,'DFinite') then
        r := `Mgfun/old_behaviour_of_Groebner_Reduce`(
          DX, df["gbasis"], df["term_order"], s) ;
        cf_list:=[coeffs(r,Alg["right_indets"],tm_list)]
    else
        cf_list:=[coeffs(DX,Alg["right_indets"],tm_list)];
        s:=1
    end if;
    tm_list:=[tm_list];
    collect(convert([seq(cf_list[i]/s*'APPLYOPR'(df,X,tm_list[i],Alg),
        i=1..nops(tm_list))],`+`),'APPLYOPR','distributed',normal)
end proc:

`diff/`||APPLYOPR:=proc(df,X,DX,Alg,x)
    APPLYOPR(df,X,DX*Alg["right_of_left",x],Alg)
end proc:

# ALGSUBS(df,[x,y,z],Dx^alpha*Dy^beta*Dz^gamma,Alg,{x=u,y=v,z=w})
# denotes subs({x=u,y=v,z=w},diff(df(x,y,z),[x$alpha,y$beta,z$gamma])).
ALGSUBS:=proc(df,X,DX,Alg,U)
    local the_DX,the_U,x,real_x,x_of_U,real_x_of_U,shift_val,r,s,
        cf_list,tm_list,i,the_q;
    option `Copyright (c) 1997-2009 Frederic Chyzak, INRIA, France`,remember;
    global DFinite,term_order,list,distributed,numeric,
        shift,dual_shift,`shift+dual_shift`,`qdilat+dual_qdilat`;
    the_DX:=DX;
    the_U:=U;
    for x in X do
        x_of_U:=subs(the_U,x);
        if type(x_of_U,`+`) then
            shift_val:=floor(select(type,x_of_U,'numeric'));
            if Alg["type_of_left",x]='shift' then
                if shift_val<0 then
                    error "backward shifts forbidden with commutation 'shift'"
                end if;
                the_DX:=Alg["right_of_left",x]^shift_val*the_DX
            elif Alg["type_of_left",x]='dual_shift' then
                if shift_val>0 then
                    error "forward shifts forbidden with commutation 'dual_shift'"
                end if;
                the_DX:=Alg["right_of_left",x]^(-shift_val)*the_DX;
                the_U:=remove((eq,x)->op(1,eq)=x,the_U,x)
                    union {x=x_of_U-shift_val}
            elif Alg["type_of_left",x]='`shift+dual_shift`' then
                the_DX:=`if`(shift_val>0,
                    Alg["right_of_left",x]^shift_val,
                    Alg["inverse_of",Alg["right_of_left",x]]^(-shift_val))
                        *the_DX;
                the_U:=remove((eq,x)->op(1,eq)=x,the_U,x)
                    union {x=x_of_U-shift_val}
##### Purely qdilat and dual_qdilat cases still missing.
            elif Alg["type_of_left",Alg["left_of_auxiliary_info",x]]
                    ='`qdilat+dual_qdilat`' then
                real_x:=Alg["left_of_auxiliary_info",x];
                real_x_of_U:=subs(the_U,real_x);
                the_DX:=`if`(shift_val>0,
                    Alg["right_of_left",real_x]^shift_val,
                    Alg["inverse_of",Alg["right_of_left",real_x]]^(-shift_val))
                        *the_DX;
                the_q:=subs(U,
                    op([1,2,-1],select(has,Alg["type_struct"],real_x)));
                the_U:=remove((eq,S)->member(op(1,eq),S),the_U,[x,real_x])
                    union {x=x_of_U-shift_val,
                        # Would combine(...,power,symbolic) be better/faster?
                        real_x=expand(real_x_of_U/the_q^shift_val)}
            end if
        end if
    end do;
    if type(df,'DFinite') then
        r := `Mgfun/old_behaviour_of_Groebner_Reduce`(
          the_DX, df["gbasis"], df["term_order"], s) ;
        cf_list:=[coeffs(r,Alg["right_indets"],tm_list)];
    else
        cf_list:=[coeffs(the_DX,Alg["right_indets"],tm_list)];
        s:=1
    end if;
    tm_list:=[tm_list];
    cf_list:=subs(the_U,cf_list);
    s:=subs(the_U,s);
# BAD!
#    the_U:={seq(i=subs(the_U,i),i=map2(op,1,U))};
    collect(convert([seq(cf_list[i]/s*'ALGSUBS'(df,X,tm_list[i],Alg,the_U),
        i=1..nops(tm_list))],`+`),'ALGSUBS','distributed',normal)
end proc:
    
`diff/`||ALGSUBS:=proc(df,X,DX,Alg,U,x)
    local i;
    option `Copyright (c) 1997-2002 Frederic Chyzak, INRIA, France`;
    convert([seq(diff(op([i,2],U),x)*
        ALGSUBS(df,X,DX*Alg["right_of_left",op([i,1],U)],Alg,U),
            i=1..nops(U))],`+`)
end proc:

#savelib('APPLYOPR',evaln(`diff/`||APPLYOPR),'ALGSUBS',evaln(`diff/`||ALGSUBS));
