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

### This file contains the implementation of rectoproc
### as rewritten by Ludovic Meunier in 2002--2004.
###
### It contains:
# - rectoproc
# - `rectoproc/checkcond`
# - `rectoproc/copyright`
# - `gfun/rectoproc/symbol`
# The implementation also relies on formatrec and listprimpart
# assigning the content in an optional argument, as modified in gfun2.77.
###
# better argument checking BS. May 04.
# made copyright an option BS Apr 04.
# Made optimize an option. BS. Apr 04.
# Now use FromInert instead of procmake. BS. Apr 04.
# Added option maximalindex to avoid large singularities. BS. Apr 04.
# Modified to adjust to new expectations of FromInert for procedures. BS. Nov 04
# Modified to use binary splitting when possible. MM. Jan 09.


############################################################
# Description: table of prefix of symbols that are used as
# variables in the generated procedures. This table is
# GLOBAL, such that users can modify it.
`gfun/rectoproc/symbol`["arg_main"] := 'n';
`gfun/rectoproc/symbol`["arg_param"] := 'b';
`gfun/rectoproc/symbol`["loc_loop"] := 'i';
`gfun/rectoproc/symbol`["loc_generic"] := 'loc';
`gfun/rectoproc/symbol`["loc_extra"] := 'xloc';
`gfun/rectoproc/symbol`["loc_tmp"] := 'tmp';
############################################################
### Function name: rectoproc
### Argument sequence:
#	expr::R
#	yofn::function(name)
#	'remember'	(optional)
#	'list'		(optional)
#	'params'=[n1,n2,...]	(optional)
#	'evalfun'=name	(optional)
#	'preargs'=[a1,...] 	(optional, require 'evalfun')
#	'postargs'=[b1,...] 	(optional, require 'evalfun')
#	'whilecond'=boolean 	(optional)
#	'errorcond'=boolean 	(optional)
#	'index'	(optional)
#	'rhs'	(optional)
#	'evalinicond'	(optional)
#	'extralocal'=[a=$a,b=$b...]	(optional)
#	'nosymbolsubs'	(optional)
#       'optimize'	(optional)
#	'copyright'=string (optional)
#	'maximalindex = nonnegint' (optional)
#	'plain' (optional)
### Output format:
#	procedure
### Global variables: NONE
### Errors: NONE
### Notes: variables that are reserved for the produced
# code are lowercase; variables for the generator are
# uppercase; names prefixed with underscore are macros
# ($define).
### Description: 'rectoproc' generates a procedure
# from a recurrence that computes the terms of the
# recurrence. Three complexity schemes apply:
#  -time: O(n), space: O(n): compute n terms (option 'list')
#  -time: O(n), space: O(1): compute n-th term, general case
#  -time: O(log(n)), space: O(log(n)): compute n-th term,
#	recurrence with constant coefficients
# the time complexity is in terms of number of arithmetic
# operations, the space complexity in terms of pointers to 
# actual objects being computed.
### Options:
# If 'remember' is given, then the procedure uses
# option remember to compute values of the sequence
# in linear time and linear space. Otherwise, if the
# recurrence has constant coefficients, then binary
# powering is used to make the procedure logarithmic
# time/logarithmic space. If the coefficients are not
# constants, then the procedure is a simple loop that
# will compute the values in linear time/constant space.
# The same loop is used in the 'list' case.
#
# The option 'evalfun' provides a way to force Maple to
# use the specified evaluation rule. For instance, it 
# could be 'evalf' for a numerical evaluation, or 'expand'
# when dealing with polynomials. 'postargs' and 'preargs'
# are the lists of extra arguments that are required by
# the evaluation function; for instance, 'evalfun'='evalf',
# 'postargs'=[12] computes with 12 digits. Note that 'preargs'
# and 'postargs' may be expressions involving 'n'.
# 'evalfun' is now mapped over initial conditions as well
# and it is evaluated before procedure generation if the
# option 'evalinicond' is supplied.
#
# The option 'whilecond' allows to define a condition
# (represented as a boolean expression, see below) that
# stops the execution of the procedure when the condition
# turns out to be true. The 'whilecond' doesn't effect on
# initial conditions. 'n' is no longer in the argument list 
# of the returned procedure (therefore, 'remember' affects
# only 'params').
#
# The option 'errorcond' stops the loop and throws an
# exception when it's true; 'errorcond' is restricted to 
# shifts lower than the order of the recurrence. This option
#  may be interesting to avoid infinite loops (when an inappropriate
# 'whilecond' is used).
#
# The option 'index' makes the procedure return the
# list [n0,u(n0)], instead of u(n0); it's useful when
# 'whilecond' is specified.
#
# The option 'rhs' may be used to specify right handside
# of recurrence when it is not polynomial in n
# 
# The option 'extralocal'=list({name,name=anything}) allows to
# specify extra local variables. The order in the list matters,
# since it is used for initialization. Note that all other options
# may now use the names that are declared as local variable.
#
# The option 'nosymbolsubs' disables the name substitution for both
# parameters and local variables. Then, the names that are passed
# in the argument sequence are the actual names of the variables in
# the generated code. The names that are used in rectoproc are taken
# from `gfun/rectoproc/symbol`:
#	"loc_generic" *
#	"loc_loop" 1..5
#	"loc_tmp" 1..2
#	"arg_main"
# If the option 'nosymbolsubs' is not used, then the following
# names are used:
#	"arg_param" *
#	"loc_extra" *
#
# The option 'optimize'	is used to determine whether the initial conditions
# should be optimized using codegen[optimize]. This is useful when the 
# resulting procedure is ultimately translated into another language.
#
# The option 'maximalindex = nonnegint' is used to avoid dealing with a singularity
# of the recurrence which is larger than the maximal argument that will ever
# be given to the procedure. This is useful for instance when using rectoproc
# to compute series solutions of an LDE.
#
# If the optional argument plain is supplied, the procedure returned will unroll
# the recurrence relation using a plain loop, even in cases where more efficient
# algorithms are supported. This is allows to use the resulting procedure with
# evalhf, the  CodeGeneration package, or other functions that do not support
# all Maple language features.


### History:
# 06/12/02: added option 'rhs'
# 06/13/02: fixed BUG1
# 07/25/02: fixed BUG2
# 08/01/02: evalfun mapped over initial conditions
# 08/05/02: rhs is multiplied by common denominator
#  if recurrence given with rational function
# 08/08/02: rhs is divided by content of R
# 08/08/02: evalinicond
# 10/13/02: extralocal
# 10/15/02: nosymbolsubs
############################################################
rectoproc := proc(expr,yofn)
    option `Copyright (c) 1992-2004 by Algorithms Project, INRIA France. All rights reserved.`;
    local Y,N,R,INITS,ARGLIST,RECSTAT,U,II,REMBR,
    ORDER,N0,INIVECT,INIMAT,RECLOOP,
    THRESHOLD,LIST,PARAMS,TOSUBS,J,EVALFUN,PREARGS,POSTARGS,WHILECOND,ERRORCOND,
    ERRORIND,WHILEIND,INDEX,EXTRAS,MAX,STARTIND,LOC,
    OPT,RHS,TMP,DENOM,ARGUMENT,
    CONTENT,EVALINICOND,EXTRALOCAL,
    XLOCVARS,XLOCBODY,NOSYMBOLSUBS,
    PARAMSEQ,OPTIONSEQ,APPLYEVALFUN, DOINIT, LASTEXTRA,
    ENDLOOP, LOCGEN, RESULT, RESULTELT, INITAB, VALRECSTAT,
    FIRSTINI,INIT2,BODY2,RESULT2,OPTIMIZE,MAXINDEX,COPYRIGHT,CLOSE,
    BSPARAMS, PLAIN, BSINITS,
    # generated procedure
    i,l, 	# loop indices (local)
    res,a,	# binary powering (local)
    u,		# PREFIX (local)
    n;		# indice (argument)
    
    ### 10/20/02: the lines below are not magic! They
    # clarify the conventions on names in the generated
    # procedure.
    ## 05/04: added eval(.,1) because those names may have been assigned
    i,l := seq(cat(eval(`gfun/rectoproc/symbol`["loc_loop"],1),II),II=1..2);
    res,a := seq(cat(eval(`gfun/rectoproc/symbol`["loc_tmp"],1),II),II=1..2);
    u := eval(`gfun/rectoproc/symbol`["loc_generic"],1);
    n := eval(`gfun/rectoproc/symbol`["arg_main"],1);
    
    if nargs<2 then error "wrong number of parameters" fi;

    ######## default values ######################################
    LIST:=false; REMBR:=false; PARAMS:=NULL; TOSUBS:=NULL; RHS:=0; INDEX:=false;
    OPTIMIZE:=false; EVALINICOND:=false; NOSYMBOLSUBS:=false; EVALFUN:=NULL;
    COPYRIGHT:=NULL; WHILECOND:=NULL; ERRORCOND:=NULL; EXTRALOCAL:=NULL; PREARGS:=NULL;
    POSTARGS:=NULL; MAXINDEX:=NULL; PLAIN:=false;
    ######### begin parse extra arguments ########################
    for ARGUMENT in args[3..nargs] do 
    	if ARGUMENT='list' then LIST:=true
    	elif ARGUMENT='remember' then REMBR:=true
        elif ARGUMENT='plain' then PLAIN:=true
    	elif type(ARGUMENT,identical('params')=anything) then
    	    PARAMS:=op(2,ARGUMENT);
    	    TOSUBS := seq(PARAMS[II]= # eval because b can be assigned
		cat(eval(`gfun/rectoproc/symbol`["arg_param"],1),II),II=1..nops(PARAMS));
            PARAMS := op(map2(op,2,[TOSUBS]))
	elif type(ARGUMENT,identical('rhs')=anything) then RHS:=op(2,ARGUMENT)
	elif ARGUMENT='index' then INDEX:=true
	elif ARGUMENT='optimize' then OPTIMIZE:=true
	elif ARGUMENT='evalinicond' then EVALINICOND:=true
	elif ARGUMENT='nosymbolsubs' then NOSYMBOLSUBS:=true
	elif type(ARGUMENT,identical('evalfun')=anything) then EVALFUN:=op(2,ARGUMENT)
	elif type(ARGUMENT,identical('copyright')=anything) then COPYRIGHT:=op(2,ARGUMENT)
	# conditions are checked below
	elif type(ARGUMENT,identical('whilecond')=anything) then WHILECOND:=op(2,ARGUMENT)
	elif type(ARGUMENT,identical('errorcond')=anything) then ERRORCOND:=op(2,ARGUMENT)
	elif type(ARGUMENT,identical('extralocal')=anything) then EXTRALOCAL:=op(2,ARGUMENT)
	elif type(ARGUMENT,identical('preargs')=anything) then PREARGS:=op(2,ARGUMENT)
	elif type(ARGUMENT,identical('postargs')=anything) then POSTARGS:=op(2,ARGUMENT)
	elif type(ARGUMENT,identical('maximalindex')=nonnegint) then MAXINDEX:='maxsing'=op(2,ARGUMENT)
	else error "unknown or badly specified option(s): %1",ARGUMENT
        fi
    od;
    ### compatibility of options
    if EVALFUN = NULL and (POSTARGS <> NULL or PREARGS <> NULL or EVALINICOND) then
        error "missing option evalfun";
    fi;
    ######### end parse extra arguments ###########################

    
    ### format conversion
    R:=formatrec([args[1..2]],'Y','N','INITS','DENOM');
    ORDER:=nops(R)-2;
        
    ### 10/13/02    
    ### extra local variables
    if EXTRALOCAL = NULL then XLOCVARS := NULL;	XLOCBODY := NULL
    else
	# EXTRALOCAL=list({name,name=init})
	
	# TMP yields set of names of extra local variables
	TMP := {op(select(type,EXTRALOCAL,'name')),
	    op(map2(op,1,select(type,EXTRALOCAL,`=`)))};
	if NOSYMBOLSUBS then
	    XLOCVARS := op(TMP)
	else
	    # sequence of name = newname
	    XLOCVARS := seq(op(II,TMP)=
		cat(`gfun/rectoproc/symbol`["loc_extra"],II),II=1..nops(TMP));
	    TOSUBS := TOSUBS,XLOCVARS;
	    # sequence of new names
	    XLOCVARS := op(map2(op,2,[XLOCVARS]));
	fi;
	# TMP yields [name=init]
	TMP := subs(TOSUBS,N=n,select(type,EXTRALOCAL,`=`));
	XLOCBODY := seq(map(ToInert,_Inert_ASSIGN(op(1,II),op(2,II))),II=TMP)
    fi;
    
    ### name substitution: names of extra local variables are also substitued
    ### 'n' is substituted later
    RHS := subs(TOSUBS,RHS);
    if PREARGS <> NULL then PREARGS := subs(TOSUBS,PREARGS) fi;
    if POSTARGS <> NULL then POSTARGS := subs(TOSUBS,POSTARGS) fi;
    INITS:=subs([TOSUBS],`goodinitvalues/rec`(R,Y,N,INITS,false,MAXINDEX));
    N0:=nops(INITS);
    
    ### apply evalfun
    if EVALFUN=NULL then APPLYEVALFUN:=proc(ind,val) val end
    else
	APPLYEVALFUN:=proc(ind, val, donoteval)
	local FUN, PRE, POST;
	    if PREARGS <> NULL then PRE := op(subs(n=ind,PREARGS)) else PRE:=NULL fi;
	    if POSTARGS <> NULL then POST := op(subs(n=ind,POSTARGS)) else POST:=NULL fi;

	    # the mechanism subs(FUN=...) is used to force
	    # evaluation of argument sequence (eg evalf
	    # would throw error).
	    if nargs=2 or donoteval then subs(FUN=EVALFUN,FUN(PRE,val,POST))
	    else eval(FUN(PRE,val,POST),FUN=EVALFUN) fi
	 end
    fi;

    ### map 'evalfun' over initial conditions
    INITS:={seq(op(1,II)=APPLYEVALFUN(
		op([1,1],i),op(2,II), not EVALINICOND),II=INITS)};
    if INITS={} or LIST or EVALFUN<>NULL or PARAMS<>NULL then INITAB:=_Inert_EXPSEQ() else
	INITAB:=_Inert_HASHTAB(seq(map(ToInert,
		_Inert_HASHPAIR(op([1,1],II),op(2,II))),II=INITS))
    fi;
	
    ### check while conditions
    if WHILECOND <> NULL then
	WHILECOND,WHILEIND :=
	    `rectoproc/checkcond`(WHILECOND,Y,N,INITS,{TOSUBS});
    fi;

    if LIST then MAX:=N 
    elif WHILECOND=NULL then MAX:=max(1,ORDER)
    else MAX:=max(1,ORDER,WHILEIND+1) fi;

    ### name converter
    # In the list case, the names are u[i]
    # otherwise, there are few names in use (loc||0.. loc||MAX)
    if LIST then LOCGEN:=proc(i) u[i] end
    elif REMBR then LOCGEN:=proc(i) PROCNAME(i,PARAMS) end
    else LOCGEN:=proc(i) cat(`gfun/rectoproc/symbol`["loc_generic"],i) end fi;

    ### final while condition
    if WHILECOND<>NULL then
	WHILECOND:=ToInert(subs(seq(Y(N-II)=LOCGEN(MAX-II-1),II=0..WHILEIND),
	    N=i,WHILECOND))
    fi;

    # argument list of the produced procedure
    if WHILECOND=NULL then ARGLIST := [n::'nonnegint',PARAMS]
    else ARGLIST := [PARAMS]
    fi;
    PARAMSEQ:=_Inert_PARAMSEQ(op(map(ToInert,ARGLIST)));
    CLOSE:=_Inert_DESCRIPTIONSEQ(),_Inert_GLOBALSEQ(),_Inert_LEXICALSEQ(),
    	_Inert_EOP(_Inert_EXPSEQ(_Inert_INTPOS(nops(PARAMSEQ))));
    ### compute error condition
    if ERRORCOND <> NULL then
	# error condition allowed to 'stretch' in [0..ORDER]
	ERRORCOND,ERRORIND :=
	    `rectoproc/checkcond`(ERRORCOND,Y,N,INITS,{TOSUBS},ORDER);
	ERRORCOND:=subs(seq(Y(N-II)=LOCGEN(MAX-II),II=0..ERRORIND),N=i,ERRORCOND);
	ERRORCOND:=_Inert_IF(_Inert_CONDPAIR(ToInert(ERRORCOND),
	    _Inert_STATSEQ(_Inert_ERROR(map(ToInert,_Inert_EXPSEQ(
		"error condition true at %1 = %2",N,'i'))))))
    fi;

    ### options
    if REMBR then OPTIONSEQ:='remember',COPYRIGHT
    else OPTIONSEQ:=COPYRIGHT fi;
    OPTIONSEQ:=map(ToInert,_Inert_OPTIONSEQ(OPTIONSEQ));

    ### code generation
    
    # RECSTAT:  u(n) as expression of U[II-k] and N
    # 'listprimpart' can be used here, since
    # initial conditions have been handled already.
    # However, the content is needed for RHS.
    R:=subs([N=N-ORDER,TOSUBS],listprimpart(R,N,'CONTENT'));
    
    # DENOM and CONTENT in rhs
    RECSTAT:=(subs(N=N-ORDER,RHS*DENOM/CONTENT)-convert(convert([R[1],seq(R[II+2]*U[II],II=0..ORDER-1)],`+`),'horner',N))/convert(R[nops(R)],'horner',N);

    VALRECSTAT:=proc(n,shiftind)
    local i;
	APPLYEVALFUN(n,
	    subs([N=n,seq(U[i]=LOCGEN(i-ORDER+shiftind),i=0..ORDER-1)],RECSTAT))
    end:

    if REMBR then
    	LOC := NULL;
	EXTRAS:=NULL;
	RECLOOP:=_Inert_STATSEQ(subs(_Inert_NAME("PROCNAME")=_Inert_PROCNAME(),
	    ToInert(VALRECSTAT(n,n))));
	RESULT:=NULL;
	if PARAMS=NULL and EVALFUN=NULL then DOINIT:=NULL
	else
	    INITAB:=_Inert_EXPSEQ();
	    DOINIT:=seq(_Inert_CONDPAIR(ToInert(n=op([1,1],II)),
		_Inert_STATSEQ(ToInert(op(2,II)))),II=INITS);
	    RECLOOP:=_Inert_IF(DOINIT,RECLOOP);
	    DOINIT:=NULL
	fi
    else
	### optimization of initial conditions
	# Initial conditions are optimized (using codegen[optimize]) 
	# INITS = { Y(0) = value,...}
	# optimized SORTED sequence for initial conditions
	if MAX=N or MAX>N0-1 then FIRSTINI:=0
	else FIRSTINI:=N0-MAX fi;
	OPT:=subs(INITS,[seq(u[II]=Y(II),II=FIRSTINI..N0-1)]);
	if not OPTIMIZE then LOC:=i
	else
	    OPT := [codegen:-optimize(OPT)];
	    # extra local variables
	    LOC :=i,op({seq(op(1,II),II=OPT)} minus {seq(u[II],II=FIRSTINI..N0-1)})
	fi;
	DOINIT:=subs([seq(u[II]=LOCGEN(II-FIRSTINI),II=FIRSTINI..N0-1)],OPT);
	DOINIT:=seq(_Inert_ASSIGN(ToInert(op(1,II)),ToInert(op(2,II))),II=DOINIT);
    	if MAX=N or MAX<=N0-1 then LASTEXTRA:=N0-1
	else LASTEXTRA:=MAX-1
	fi;
	EXTRAS := seq(map(ToInert,_Inert_ASSIGN(LOCGEN(J),
    		VALRECSTAT(J,J))),J=N0..LASTEXTRA);
	STARTIND := LASTEXTRA;
	if WHILECOND=NULL then
	    ENDLOOP:=ToInert(n-1); WHILECOND:=ToInert(true)
	else ENDLOOP:=_Inert_EXPSEQ() fi;
	
	### various kinds of main loop
	if LIST then
	    LOC:=LOC,u;
	    RECLOOP := _Inert_FORFROM(ToInert(i),ToInert(STARTIND),ToInert(1),
	    	ENDLOOP,WHILECOND,
	    	_Inert_STATSEQ(
		    map(ToInert,_Inert_ASSIGN(LOCGEN(i+1),VALRECSTAT(i+1,i+1))),
		    # check error condition
		    ERRORCOND));
	    if INDEX then RESULTELT:=[i,u[i]] else RESULTELT:=u[i] fi;
	    RESULT:=ToInert(['seq'(RESULTELT,i=0..n)])
	else
	    LOC:=LOC,seq(LOCGEN(II),II=0..MAX);
	    RECLOOP := _Inert_FORFROM(ToInert(i),ToInert(STARTIND),ToInert(1),
	    	ENDLOOP,WHILECOND,
	    	_Inert_STATSEQ(
		    map(ToInert,_Inert_ASSIGN(LOCGEN(MAX),VALRECSTAT(i+1,MAX))),
		    seq(map(ToInert,_Inert_ASSIGN(LOCGEN(II),LOCGEN(II+1))),II=0..MAX-1),
		    # check error condition
		    ERRORCOND));
	    if N0>1 then
		RECLOOP:=_Inert_IF(seq(_Inert_CONDPAIR(ToInert(n=II),_Inert_STATSEQ(_Inert_RETURN(ToInert(LOCGEN(II))))),II=0..N0-2),
			_Inert_STATSEQ(RECLOOP))
	    fi;
	    if not INDEX then RESULT:=ToInert(LOCGEN(MAX-1))
	    else RESULT:=ToInert([n,LOCGEN(MAX-1)]) fi;
	fi
    fi;
    # If 'remember' was given and 'whilecond' was not, then WHILECOND is still 
    # NULL at this point, hence the branch taken is 'normal case'.
    if PLAIN or LIST or EVALFUN <> NULL or ERRORCOND <> NULL or RHS <> 0
            or WHILECOND <> ToInert(true) or ORDER = 0
            or not type(R, 'list'('polynom'('complex'('rational'),N))) then # normal case
	FromInert(_Inert_PROC(PARAMSEQ,
	    _Inert_LOCALSEQ(op(map(ToInert,[LOC,XLOCVARS]))),
	    OPTIONSEQ,INITAB,
	    _Inert_STATSEQ(
		XLOCBODY,
		DOINIT,
		EXTRAS,
		RECLOOP,
		RESULT),CLOSE))
    elif not has(subsop(1=NULL,R),N) then # binary powering
        if R[1]<>0 then return procname(rectohomrecbis(expr,yofn),args[2..-1]) fi;
	THRESHOLD:=round(evalf(-2*ORDER^2/ln(2)*
	    LambertW(-1,-1/2*ln(2)/ORDER^2)));
	INIMAT:=[[seq(-R[nops(R)-II]/R[nops(R)],II=1..ORDER)],
		seq([0$(II-1),1,0$(ORDER-II)],II=1..ORDER-1)];
	INIVECT:=subs(INITS,[seq(Y(N0-II),II=1..ORDER)]);

	INIT2:=map(ToInert,_Inert_ASSIGN(a,Matrix(INIMAT))),
	       map(ToInert,_Inert_ASSIGN(res,Vector(INIVECT))),
	       map(ToInert,_Inert_ASSIGN(l,'convert'(n-N0+1,'base',2))),
	       _Inert_IF(_Inert_CONDPAIR(ToInert(l[1]=1),
		    _Inert_STATSEQ(map(ToInert,_Inert_ASSIGN(res,
			LinearAlgebra:-Multiply(Matrix(INIMAT),
			    Vector(INIVECT)))))));
	BODY2:=_Inert_FORIN(ToInert(i),ToInert('subsop'(1=NULL,l)),
				ToInert(true),
		    _Inert_STATSEQ(
			_Inert_ASSIGN(ToInert(a),
			  ToInert('LinearAlgebra:-MatrixMatrixMultiply'(a,a))),
			_Inert_IF(_Inert_CONDPAIR(ToInert(i=1),
			    _Inert_STATSEQ(map(ToInert,_Inert_ASSIGN(res,
				'LinearAlgebra:-MatrixVectorMultiply'(
				a,res))))))));

	if not INDEX then RESULT2:=ToInert(res[1])
	else RESULT2:=ToInert([n,res[1]]) fi;

	FromInert(_Inert_PROC(PARAMSEQ,
	    _Inert_LOCALSEQ(op(map(ToInert,[LOC,a,res,l,XLOCVARS]))),
	    OPTIONSEQ,INITAB,
	    _Inert_STATSEQ(
		_Inert_IF(_Inert_CONDPAIR(ToInert(n<=THRESHOLD),
		    _Inert_STATSEQ(
			XLOCBODY,
			DOINIT,
			EXTRAS,
			RECLOOP,
			RESULT)),
		    _Inert_STATSEQ(
			INIT2,
			BODY2,
			RESULT2))),CLOSE))
    else # binary splitting
        if R[1]<>0 then return procname(rectohomrec(expr,yofn),args[2..-1]) fi;
       	THRESHOLD:= N0 + 5*ORDER^2; # N0 = max(0, largest sing + 1) + order (?)
        BSINITS := [seq(Y(N0-ORDER+II),II=0..ORDER-1)];
        # BSPARAMS is an _Inert_EXPSEQ
        BSPARAMS := `rectoproc/binsplitparameters`(op(subs(n=l,[expr,yofn])),
            N0 - ORDER, n, Vector(BSINITS));
        BODY2 := _Inert_STATSEQ(
            _Inert_ASSIGN(ToInert(res),
                _Inert_FUNCTION(ToInert('`gfun/rectoproc/binsplit`'), BSPARAMS)),
            _Inert_ASSIGN(ToInert(res),
                _Inert_FUNCTION(ToInert('subs'),
                _Inert_EXPSEQ(
                    # makes the generated proc more readable if N0 >> 0
                    ToInert(select(has, INITS, BSINITS)),
                    ToInert(res)))));
        if not INDEX then RESULT2:=ToInert(res) else RESULT2:=ToInert([n,res]) fi;
	FromInert(_Inert_PROC(PARAMSEQ,
	    _Inert_LOCALSEQ(op(map(ToInert,[LOC,a,res,l,XLOCVARS]))),
	    OPTIONSEQ,INITAB,
	    _Inert_STATSEQ(
                XLOCBODY,
		_Inert_IF(_Inert_CONDPAIR(ToInert(n<=THRESHOLD),
		    _Inert_STATSEQ(
			DOINIT,
			EXTRAS,
			RECLOOP,
			RESULT)),
		    _Inert_STATSEQ(
			BODY2,
			RESULT2))),CLOSE));
    fi
end proc:
############################################################

############################################################
### Function name: `rectoproc/checkcond
### Argument sequence:
#	cond::boolean
#	u::name
#	n::name
#	inits::set(function(integer)=anything)
#	tosubs::set(name=name): declared parameters are renamed
#	ord::integer: throw error if backshift > ord (optional)
### Output format:
#	sequence: condition, maxshift
### Dependencies: NONE
### Global variables: NONE
### Errors: incorrect shift
### Description: see 'whilecond' and 'stopcond' options.
# A condition is an expression that evaluates to
# a boolean. A condition may be function of:
# -'n': independent variable,
# -'u': dependent variable. Only backward shift
# of the form u(n-nonnegint) and constant values
# of the form u(n0), where u(n0) is one of the 
# initial condition are allowed,
# -names declared by the option 'params'
############################################################
`rectoproc/checkcond` := proc(cond,u,n,inits,tosubs)
    option `Copyright (c) 1992-2004 by Algorithms Project, INRIA France. All rights reserved.`;
    local c,sh,mx,i;

    # type check    
    if not type(cond,boolean) then
	error "expecting boolean expression, received %1",args[1];
    end if;
    
    # cond: u(n-n0), u(n0), n, params
    c := subs(inits,tosubs,cond);			# replaces names and initial conditions
    # c: u(n0 not in initial conditions), u(n-n0), n
    
    sh := map(op,indets(c,specfunc(anything,u)));	# yield arguments of 'u' (shift)
    # sh: n-n0, n0 not in initial conditions
    
    # check shifts: valid shifts=u(n-nonnegint);
    # valid u(n0) (n0 in initial conditions) already suvstitued
    sh := {seq(n-i,i=sh)};
    if not type(sh,set(nonnegint)) then
	error "invalid shifts";
    fi;
    
    mx := max(0,op(sh));	# maximum shift of the set

    # optional argument (used by 'errorcond')
    if nargs = 6 and mx > args[6] then
	error "in errorcond: shift must be lower than %1",args[6];
    fi;
    
    # c is condition; only contains u(n-n0) and n. Params and initial
    # conditions substitued.
    c,mx;	# Seq
    
end proc:	# `rectoproc/checkcond`
############################################################
