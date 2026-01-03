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

`regexpcount/version`:=1.5:
# 1.1 -> 1.2 autotoXstarauto was not saved in 1.1 
# 1.2 -> 1.3 bug of doc fixed in regexpcount.mws
# 1.3 -> 1.4 port to Maple 6
#            port to Maple 7: FC, slightly modified with same version number.
#                             (Changed `solve/linear` to SolveTools:-Linear.)
# 1.4 -> 1.5 port to Maple 8
#            modification in eqnstoasympexp following evolution of combstruct
regexpcount:=table([
autocarprod=`regexpcount/autocarprod`,
autocomplete=`regexpcount/autocomplete`,
autodeterminize=`regexpcount/autodeterminize`,
autoerror=`regexpcount/autoerror`,
automarkov=`regexpcount/automarkov`,
autominimize=`regexpcount/autominimize`,
autoprune=`regexpcount/autoprune`,
autorenumber=`regexpcount/autorenumber`,
autotogram=`regexpcount/autotogram`,
drawword=`regexpcount/drawword`,
eqnstoasympexp=`regexpcount/eqnstoasympexp`,
gramerror=`regexpcount/gramerror`,
grammarkov=`regexpcount/grammarkov`,
gramtoauto=`regexpcount/gramtoauto`,
gramweight=`regexpcount/gramweight`,
randregexp=`regexpcount/randregexp`,
regexpstowaitgram=`regexpcount/regexpstowaitgram`,
regexptoauto=`regexpcount/regexptoauto`,
regexptogram=`regexpcount/regexptogram`,
regexptomatchesgram=`regexpcount/regexptomatchesgram`,
starnormalform=`regexpcount/starnormalform`
]):


# global variables

macro(ns=`regexpcount/ns`):

# procedures
macro(countl=`regexpcount/countl`):
macro(BL=`regexpcount/BL`):
macro(BW=`regexpcount/BW`):
macro(BuildGlushkov=`regexpcount/BuildGlushkov`):
macro(EraseProd=`regexpcount/EraseProd`):
macro(EraseUnion=`regexpcount/EraseUnion`):
macro(ExpandR=`regexpcount/ExpandR`):
macro(GenMarkLet=`regexpcount/GenMarkLet`):
macro(Gfirst=`regexpcount/Gfirst`):
macro(Gfirstlast=`regexpcount/Gfirstlast`):
macro(Glast=`regexpcount/Glast`):
macro(MarkLetters=`regexpcount/MarkLetters`):
macro(alphaclosure=`regexpcount/alphaclosure`):
macro(atomtoauto=`regexpcount/atomtoauto`):
macro(autocarprod=`regexpcount/autocarprod`):
macro(autocheck=`regexpcount/autocheck`):
macro(autocheckentry=`regexpcount/autocheckentry`):
macro(autocomplete=`regexpcount/autocomplete`):
macro(autoconcat=`regexpcount/autoconcat`):
macro(autocopy=`regexpcount/autocopy`):
macro(autodeterminize=`regexpcount/autodeterminize`):
macro(autoepssuppress=`regexpcount/autoepssuppress`):
macro(autoerror=`regexpcount/autoerror`):
macro(autofastconcat=`regexpcount/autofastconcat`):
macro(autofastunion=`regexpcount/autofastunion`):
macro(automarkov=`regexpcount/automarkov`):
macro(autominimize=`regexpcount/autominimize`):
macro(autonot=`regexpcount/autonot`):
macro(autoprune=`regexpcount/autoprune`):
macro(autorenewal=`regexpcount/autorenewal`):
macro(autorenumber=`regexpcount/autorenumber`):
macro(autoreverse=`regexpcount/autoreverse`):
macro(autoseq=`regexpcount/autoseq`):
macro(autotogram=`regexpcount/autotogram`):
macro(autotoXstarauto=`regexpcount/autotoXstarauto`):
macro(autounion=`regexpcount/autounion`):
macro(bersettoauto=`regexpcount/bersettoauto`):
macro(checkprod=`regexpcount/checkprod`):
macro(drawword=`regexpcount/drawword`):
macro(epsclosure=`regexpcount/epsclosure`):
macro(epsclosureset=`regexpcount/epsclosureset`):
macro(epsilontoauto=`regexpcount/epsilontoauto`):
macro(eqnstoasympexp=`regexpcount/eqnstoasympexp`):
macro(erasenu=`regexpcount/erasenu`):
macro(follow=`regexpcount/follow`):
macro(getmarks=`regexpcount/getmarks`):
macro(gramerror=`regexpcount/gramerror`):
macro(grammarkov=`regexpcount/grammarkov`):
macro(gramtoauto=`regexpcount/gramtoauto`):
macro(gramweight=`regexpcount/gramweight`):
macro(loadtrans=`regexpcount/loadtrans`):
macro(markgram=`regexpcount/markgram`):
macro(minirenumber=`regexpcount/minirenumber`):
macro(mysolvedoit=`regexpcount/mysolvedoit`):
macro(newstate=`regexpcount/newstate`):
macro(nullable=`regexpcount/nullable`):
macro(pruneunion=`regexpcount/pruneunion`):
macro(randgramtoregexp=`regexpcount/randgramtoregexp`):
macro(randregexp=`regexpcount/randregexp`):
macro(recregexpnottoauto=`regexpcount/recregexpnottoauto`):
macro(regexpnottoauto=`regexpcount/regexpnottoauto`):
macro(regexpstowaitgram=`regexpcount/regexpstowaitgram`):
macro(regexptoauto=`regexpcount/regexptoauto`):
macro(regexptogram=`regexpcount/regexptogram`):
macro(regexptomatchesgram=`regexpcount/regexptomatchesgram`):
macro(renameinit=`regexpcount/renameinit`):
macro(selectateps=`regexpcount/selectateps`):
macro(sortbyindets=`regexpcount/sortbyindets`):
macro(starnormalform=`regexpcount/starnormalform`):
macro(statesplit=`regexpcount/statesplit`):


#############################################################
# regexptogram: regular expression
#                 to non-ambiguous deterministic combstruct grammar
# user-level 
# Input:
#  G: a possibly ambiguous combstruct grammar
#  E: a non-terminal of the grammar
#  error: 'Error'[k,errors]  (optional)
#       k : number of errors allowed
#       errors: subset of {'subs','ins',del'}
# Output:
#  a non-ambiguous deterministic combstruct grammar 
#  given by an automaton
#  Init state corresponding to non terminal E is labelled E
# Comment: if the grammar contains the Not operator, classical
#  construction with epsilon-transitions is used, elsewhere
#  the Berry-Sethy construction of the NDFA Glushkov automaton
#  is used
##############################################################

regexptogram:=proc(Gram::set, E, err)
    local  R, S, erlev, errs;

    R:=regexptoauto(Gram,E);

    if nargs=3 then
       if op(0,err)<>'Error' 
          or type(op(1,err),integer)=false
          or op(2,err) minus {'subst','ins','del'} <> {} then
          error "bad specification of error parameter %1", err
       else 
          erlev:=op(1,err); errs:=op(2,err);
	  R:=autoerror(R,erlev,errs);
          S:=autoprune(R); 
       end if
    else S:=R;
    end if;

    S['checked']:=true;
    autotogram(S,E);
end proc:


#############################################################
# regexptoauto: regular expression to table automaton
# user-level 
# Input:
#  G: a possibly ambiguous combstruct grammar
#  E: a non-terminal of the grammar
# Output:
#  a non-ambiguous deterministic table automaton
# Comment: if the grammar contains the Not operator, classical
#  construction with epsilon-transitions is used, elsewhere
#  the Berry-Sethy construction of the NDFA Glushkov automaton
#  is used
##############################################################

regexptoauto:=proc(Gram, E)
    local  DFA, NDFA;

    if has(Gram,Not) then NDFA:=regexpnottoauto(Gram,E);
    else                  NDFA:=bersettoauto(Gram,E);  end if;

    DFA:=autodeterminize(NDFA);   
    autominimize(DFA);
end proc:

############################################################
# autotogram: table automaton to combstruct gram
# Input: 
#   T: table for automaton
#   E: name for 'init'
#   num_marks : list of lists [[number of mark, name of mark],..]
# output:
#   G: combstruct grammar
###########################################################

autotogram:=proc(T::table,E::name,num_marks::list)
    local  A, H, atomset, s, l, u, i, n, m, w, wr;

    if not assigned(T['checked']) then autocheck(T) end if;

    A:=T['alphabet'];

    atomset:={seq(i='Atom',i=A)};

    for s in T['liststates']  do 
        if s <> 'init' then   
            if type(s,indexed)= true then     wr(s):=cat(w,op(0,s))[op(s)]
            else	  		      wr(s):=cat(w,s);            end if
        else                    	      wr(s):='init'; 		  end if
    end do;

    for s in T['liststates'] do
        if not assigned(T[s]['trans']) or T[s]['trans']={} then
            u[s]:=NULL; # possible case of {finals}
        else 
	    n:=nops(T[s]['trans']);
	    if n=1 then 
		   l:=op(T[s]['trans']);
		   m:=nops(T[s][l])
	    else m:=1;
	    end if;
	    if n=0           then u[s]:=NULL
	    elif n=1 and m=1 then u[s]:=wr(s)=Prod(l,wr(op(T[s][l])))
	    else
		 u[s]:=wr(s)=Union(seq(seq(Prod(l,wr(i)),i=T[s][l]),l=T[s]['trans']))
	    end if;
	end if;

	if member(s,T['final']) then
	        if u[s]=NULL then u[s]:=wr(s)='Epsilon';
	        else u[s]:=wr(s)=Union('Epsilon',op(2,u[s]));
	        end if;
	end if;
	if assigned(T['mark']) then
            for i in map(op,[indices(T['mark'])]) do
	      if member(s,T['mark'][i]) then
	        if u[s]=NULL then u[s]:=wr(s)='Epsilon'[i];
	        else u[s]:=wr(s)=Union('Epsilon'[i],op(2,u[s]));
	        end if;
	      end if;  
	    end do;
	end if;
    end do;

    H:=subs('init'=E,{seq(u[s],s=T['liststates'])}) union atomset;

    # mark if necessary
    if nargs=3 then markgram(H,[seq(['Epsilon'[op(1,i)],op(2,i)],i=num_marks)]) 
    else  H  end if
end proc:

############################################################
# gramweight
# Input: G grammar for an automaton
# Output: 
#        W equivalent grammar where 
#          - each letter l is replaced by Prod(l,Atom) 
#          - each l=Atom is replaced by l=Epsilon
# Comment: the output may be processed by gfeqns
#############################################################

gramweight:=proc(G::set)
     local alpha, atomset, W;

   atomset:=select(type,G,anything=identical('Atom'));
   alpha:=map2(op,1,atomset);

   W:=subs(seq(i=Prod(i,Atom),i=alpha), G minus atomset);

   W union {seq(i='Epsilon',i=alpha)}
end proc:

############################################################
# sortbyindets: sort a grammar by indeterminates
# Input: G - grammar
# Output : L - sorted list corresponding to the grammar
# Comment : L is sorted in such a way that the indeterminates
# of the right member of level i are computable by equations
# 1 to i-1. The process is initialized by the Atoms which
# are known indeterminates
############################################################

sortbyindets:=proc(G)
    local i, K, J, GW, Leq;
	
    Leq:=[];
    GW:=G;
    K:={'Atom', 'Epsilon'};
    while GW <> {} do
      J:={};
      for i in GW do
	if indets(op(2,i),name) minus (K union {op(1,i)}) = {} then
	    K:=K union {op(1,i)};
	    J:=J union {i};
	    Leq:=[op(Leq),i]
	end if;
      end do;
      if J={} then # cycle detected
	 # select one at random (this could be refined)
	 J:=J union {op(1,GW)};
	 K:=K union {op([1,1],GW)};
	 Leq:=[op(Leq),op(1,GW)]
      end if;
      GW:=GW minus J;
    end do;
    Leq
end proc:

############################################################
# automarkov
# transform a Bernoulli automaton to the equivalent order(n)
# Markov automaton
# Input: 
#   A : table of input automaton
#   n : order of the Markov chain for output
#   M : name of the markov letters
# Output
#   O : markov automaton
# Comment:
#   nfa and uncomplete automata allowed
############################################################

automarkov:=proc(A::table, n::integer, M::name)
     local alpha, T, Y, W, O, l, i, s, t, h, w, v, oldinit, Ealpha, Malpha, 
           L, # markov letter are M[L]
           ML,# markov letter
	   U, # sets of words (lists) of size increasing to n
	      # that are input transitions to a state
	   V; # preimages by words in U
           
     alpha:=A['alphabet'];

     oldinit:=newstate(); 

     T:=renameinit(A,oldinit,'init'); 

     # add a new init state with a nu loop

     T['liststates']:=T['liststates'] union {'init'};
     T['init']['trans']:={'_nu'};
     T['init']['_nu']:={'init',oldinit};
     T['alphabet']:= T['alphabet'] union {'_nu'};

     Ealpha := alpha union {'_nu'};

     W:=autoreverse(T);

     # for each state s, build U[s,1] 
     for s in T['liststates'] do
        U[s,1]:={};
        for l in Ealpha do 
	   if assigned(W[l][s]) then
	      U[s,1]:=U[s,1] union {[l]}; V[s][l]:=W[l][s];
	   end if
        end do;
     end do;   

     # for each state s, build U[s,i]
     for i from 2 to n do
        for s in T['liststates'] do
	   U[s,i]:={};
           for w in U[s,i-1] do
              # delta(t,w)=s
	      for t in V[s][op(w)] do
	         for l in map(op,U[t,1]) do
		    v:=[l,op(w)]; 
		    U[s,i]:=U[s,i] union {v};
		    if not assigned(V[s][op(v)]) then
                                  V[s][op(v)]:=W[l][t]
		    else          V[s][op(v)]:=V[s][op(v)] union W[l][t] end if;
		 end do
	      end do
	   end do
        end do
     end do;

     # build final automaton by duplicating states as many times as they
     # are different input "word"-transitions
     
     O['liststates']:={seq(seq(s[op(v)],v=U[s,n]),s=T['liststates'] minus {'init'})};
 
     Malpha:={};

     # handle all input transitions except those to init
     for s in T['liststates'] minus {'init'} do
        for l in Ealpha do
          if assigned(W[l][s]) and W[l][s]<>{} then
	   for t in W[l][s] do
	      for w in U[t,n] do
	         L:=op(w),l;    # Markov letter of order n+1
		 v:=subsop(1=NULL,[L]);   Malpha:=Malpha union {M[L]};
                 if t <> 'init' then h:=t[op(w)];
                 else                h:=t         end if;
                 if O[h][M[L]]=evaln(O[h][M[L]]) then
                        O[h][M[L]]:={s[op(v)]};
		 else	O[h][M[L]]:=O[h][M[L]] union {s[op(v)]}  end if;
		 if not member(s[op(v)], O['liststates']) then
		    ERROR("internal",O,s,v);
		 end if;
		 if not assigned(O[h]['trans']) then
		        O[h]['trans']:={M[L]}
		 else   O[h]['trans']:=O[h]['trans'] union {M[L]} end if;
	      end do
	   end do
          end if
        end do
     end do;

     O['final']:={seq(seq(s[op(v)],v=U[s,n]),s=T['final'])};

     if assigned(T['mark']) then
        for i in map(op,[indices(T['mark'])]) do
     	   O['mark'][i]:={seq(seq(s[op(v)],v=U[s,n]),s=T['mark'][i])};
        end do;
     end if;

     # rename oldinit[seq('_nu',i=1..n)] by init

     O['init']:=evaln(O['init']);

     O['alphabet']:= Malpha minus {M[seq('_nu',i=1..n+1)]};


     Y:=renameinit(O,'init',oldinit[seq('_nu',i=1..n)]);

     erasenu(Y, '_nu')
end proc:

##########################################################
# erasenu : subs nu by NULL in a Markov automaton
# Input:  U - automaton with letter nu
# Output: V - automaton without nu
##########################################################

erasenu:=proc(U::table, nu::name)
    local V, trs, trl, l, s, i;

    for l in U['alphabet']   do trl(l):=subs(nu=NULL,l) end do;
    for s in U['liststates'] do	trs(s):=subs(nu=NULL,s) end do;

    V['alphabet']:=map(trl,U['alphabet']);
    V['liststates']:=map(trs,U['liststates']);

    for s in U['liststates'] do
        if assigned(U[s]['trans']) then 
	   V[trs(s)]['trans']:=map(trl,U[s]['trans']);
	   for l in U[s]['trans'] do V[trs(s)][trl(l)]:=map(trs,U[s][l]) end do;
        end if
    end do;

    V['final']:=map(trs,U['final']);

    if assigned(U['mark']) then
        for i in map(op,[indices(U['mark'])]) do
     	   V['mark'][i]:=map(trs,U['mark'][i]);
        end do;
    end if;
    V
end proc:

##########################################################
# renameinit
# Input: T automaton
#        newinit
#        oldini
# Output: T with renamed oldini by newinit
# Comment: nfa allowed
##########################################################
renameinit:=proc(T::table,newinit,oldini)
     local s, l, i;

     T[newinit]['trans']:=T[oldini]['trans'];
     for l in T[oldini]['trans'] do T[newinit][l]:=T[oldini][l] end do;

     T['liststates']:=subs(oldini=newinit,T['liststates']);
     T[oldini]:=evaln(T[oldini]);

     for s in T['liststates'] do
         if assigned(T[s]['trans']) then 
           for l in T[s]['trans'] do
             T[s][l]:=subs(oldini=newinit,T[s][l])
           end do
         end if
     end do;
     T['final']:=subs(oldini=newinit,T['final']);

     if assigned(T['mark']) then
        for i in map(op,[indices(T['mark'])]) do
	   T['mark'][i]:=subs(oldini=newinit,T['mark'][i]);
	end do
     end if;

     T;
end proc:
     

###################################################################
# autoprune : prune out 
#               1) not co-accessible states 
#                  these are states with no path to a final state
#               2) not accessible states
#                  these states are not accessible from 'init' state
# Input: T - table of the automaton
#        F - optional parameter - finals
#               if assigned, co-accessible states are computed
#               related to this set
# Output: T - not co-accessible states pruned out
# Comment: nfa allowed
###################################################################

autoprune:=proc(T::table, F::set)
    local clos, s, l, P, i, winit, fin;

    if nargs = 1 then fin:=T['final'] else fin:=F end if;

    for s in T['liststates'] do
	if not assigned(T[s]['trans']) then T[s]['trans']:={} end if
    end do;

    if member('{init}',T['liststates']) then winit:='{init}';
    else				     winit:='init';   end if;

    for s in T['liststates'] do clos[s]:=alphaclosure(T,s,{}); end do;

    P:={};
    for s in T['liststates'] do
	if clos[s] intersect fin = {}
           or not member(s,clos[winit])      then P:= P union {s} end if;
    end do;

    for s in P  do 
	for l in T[s]['trans'] do    T[s][l]:=evaln(T[s][l]);  end do;
	T[s]['trans']:=evaln(T[s]['trans']);
	T[s]:=evaln(T[s]);
    end do;
    for s in T['liststates'] minus P do
	for l in T[s]['trans'] do
	    T[s][l] := T[s][l] minus P;
	    if T[s][l] = {} then    
	       T[s][l]:=evaln(T[s][l]);
	       T[s]['trans']:=T[s]['trans'] minus {l}
	    end if;
	end do;
    end do;
    
    T['liststates'] := T['liststates'] minus P;
    T['final'] := T['final'] minus P;
    if assigned(T['mark']) then
       for i in map(op,[indices(T['mark'])]) do
          T['mark'][i] := T['mark'][i] minus P
       end do;
    end if; 
    T['pruned']:=true;
    T
end proc:

#################################################################
# bersettoauto
# Berry-Sethi method
# Input: G - grammar  
#        R - name of the regular expression in the grammar
# Output: non-deterministic epsilon-free Glushkov automaton
##################################################################
bersettoauto:=proc(G,R::name)
   local ExpR, MarkR, i, alphabet, BigAlphaE, BigAlpha;

   alphabet:=map2(op,1,select(type,G,anything=identical('Atom')));

   if not member(R,{seq(op(1,i),i=G)})
	then ERROR(R,`is not a list of symbols of the input grammar`);
   end if;

   BigAlphaE:={'Epsilon'};
   ExpR:=starnormalform(ExpandR(R,G),alphabet);
   MarkR:=MarkLetters(ExpR,G, 'BigAlphaE');
   BigAlpha:=BigAlphaE minus {'Epsilon'};
   BuildGlushkov(MarkR,alphabet, BigAlpha)
end proc:

#################################################################
# starnormalform
# User Level
# Input:  R regular expression
#         alphabet
# Output: S regular expression equivalent to R but in star normal
#         form - 
#         ie: there is no Sequence applied to a nullable expression 
# Comment: Berry-Sethy construction is quadratic for expressions
#          in star normal form
# Reference: "Regular expression into finite automata"
#            Anne Brueggemann-Klein
#            Theoretical Computer Science, 120 (1993), pp 197-213
# Notation
#    BL for Bru.-Klein recursive notation BlackCircle
#    BW for Bru.-Klein recursive notation BlackCircle-WhiteCircle
#
# Algorithm
#    BL(R)         = R if R = {}, Epsilon, l    (l Atom)
#    BL(F+G+...+H) = BL(F)+BL(G)+...+BL(H)
#    BL(FG...H)    = BL(F)BL(G)+...+BL(H)
#    BL(F^*)       = [BW(F)]^*
#
#    BW(R)         = {} if R = {}, Epsilon
#    BW(l)         = l  if l Atom
#    BW(F+G+...+H) = BW(F)+BW(G)+...+BW(H)
#    BW(FG...H)    = BW(F)+BW(G)+...+BW(H) if nullable(intersect(F,G,..,H))
#                  = BL(F)BL(G)...BL(H)    elsewhere (!!!different from B.-K.)
#    BW(F^*)       = BW(F)
#
# Example
#    BL((a*b*)^*)    = [BW(a*b*)]^*   = [BW(a^*)+BW(b^*)]^* = [a+b]^*
#    BL(((a+E)b)^*)  = [BW((a+E)b)]^* = [BL(a+E)BL(b)]^*    = [(a+E)b]^*
#################################################################

starnormalform:=proc(R,alphabet)
   local S;

   if has(R,Not)  then R
   else
      S:=subs({}=NULL,BL(alphabet,R));

      pruneunion(S)
   end if
end proc:

BL:=proc(alphabet,R)
   if   R={} or R='Epsilon' or member(R,alphabet) then  R
   elif op(0,R)=Union or op(0,R)=Prod             then map2(BL,alphabet,R)
   elif op(0,R)=Sequence                          then Sequence(BW(alphabet,op(R)))
   else ERROR("unrecognizable expression",R)
   end if
end proc:

BW:=proc(alphabet,R)
   if   R={} or R='Epsilon'  then {}
   elif member(R,alphabet)   then R
   elif op(0,R)=Union        then map2(BW,alphabet,R)
   elif op(0,R)=Prod         then
        if  convert(map(nullable,R,alphabet),`and`)=true then 
                  subsop(0=Union,map2(BW,alphabet,R))
	else      map2(BL,alphabet,R) end if
   elif op(0,R)=Sequence     then BW(alphabet,op(R))
   else ERROR("unrecognizable expression",R)
   end if
end proc:   

##########################################################################
# pruneunion - simplify nested Unions
#
# Input : R
# Output: equivalent expression without nested Unions
#         Union(a,Union(b,c),..) ==> Union(a,b,c,...)
########################################################################
pruneunion:=proc(R)
   local T;

   T:=op(0,R);
   if   T = Union then 
       if nops(R) > 1 then Union(op(map(pruneunion,{EraseUnion(op(R))}))) 
       else                pruneunion(op(1,R))    end if;
   elif T = Prod or T = Sequence then map(pruneunion,R)
   else R end if
end proc:

EraseUnion:=proc(R) 
   local i,j;
   for j to nargs do
       if op(0,args[j])=Union then i[j]:=EraseUnion(op(args[j]))
       else                        i[j]:=args[j]                 end if;
   end do;
   seq(i[j],j=1..nargs);
end proc:


drawword:=proc(L,P)   EraseProd(readlib(`combstruct/draw`)(L,P)) end proc:


EraseProd:=proc(R) 
   local i,j;
   for j to nargs do
       if op(0,args[j])=Prod    then i[j]:=EraseProd(op(args[j]))
       elif args[j]<> 'Epsilon' then i[j]:=args[j] 
       else                          i[j]:=NULL              end if;
   end do;
   seq(i[j],j=1..nargs);
end proc:


#################################################################
# BuildGlushkov
# Input: R Expression with marked letters
#        alpha:  original alphabet
#        BigAlpha : alphabet of marked letters
# Output: GA Glushkov Automaton
# Comment: S={MarkLet(i,a),...}
#################################################################


BuildGlushkov:=proc(R,alpha::set, BigAlpha)
   local GA, S, V, s;

   GA['liststates']:= {'init'} union BigAlpha;

   GA['init']['trans']:={};
   for S in Gfirst(R,BigAlpha) do
       for s in op(2,S) do 
           GA['init']['trans']:= GA['init']['trans'] union {s};

           if not assigned(GA['init'][s]) then  GA['init'][s]:={S}
           else                                 GA['init'][s]:=GA['init'][s] union {S} end if
       end do
   end do;

   for S in BigAlpha do
       GA[S]['trans']:={};
       for V in follow(R,S,BigAlpha) do
	    for s in op(2,V) do 
               GA[S]['trans']:= GA[S]['trans'] union {s};

               if not assigned(GA[S][s]) then  GA[S][s]:={V}
               else                            GA[S][s]:=GA[S][s] union {V} end if
	    end do
       end do
   end do;
   GA['final']:=Glast(R,BigAlpha);
   if nullable(R,BigAlpha)  then  GA['final']:=GA['final'] union {'init'} end if;
   
   subsop(5=NULL,op(Gfirstlast)); # forget
   GA['alphabet']:=alpha;
   GA
end proc:

##################################################################
# Gfirstlast
# Input:  E Expression
#         k = +/- 1
#         BigAlpha : alphabet of marked letters
# Output: First or Last of E (set)
##################################################################
Gfirstlast:=proc(E,k,BigAlpha) # k = plus or minus 1
    option remember;
    local  res;

    if E='Epsilon'             then    res:={};
    elif member(E,BigAlpha)    then res:={E}
    elif op(0,E)='Union'  then
         if nops(E)>1 then res:=`union`(op(map(Gfirstlast,E,k,BigAlpha)))
	 else              res:= Gfirstlast(op(E),k,BigAlpha);  end if;
    elif op(0,E)='Prod'   then
         res:=Gfirstlast(op(k,E),k,BigAlpha);
         if nops(E) > 1 and nullable(op(k,E),BigAlpha) then
             res:=res union Gfirstlast(subsop(k=NULL,E),k,BigAlpha)
         end if;
	 res
    elif op(0,E)='Sequence' then res:=Gfirstlast(op(1,E),k,BigAlpha)
    else ERROR("unrecognizable expression",E)
    end if;

    if member('Epsilon',res) then ERROR("Gfirstlast",E,k,BigAlpha,res); end if;
    res;
end proc:

Gfirst:=proc(E,BigAlpha) Gfirstlast(E, 1,BigAlpha) end proc:
Glast :=proc(E,BigAlpha) Gfirstlast(E,-1,BigAlpha) end proc:

##################################################################
# nullable
# Input:  E Expression
#         alphabet
# Output: true or false
##################################################################
nullable:=proc(E, alphabet::set)
    option remember;
    if E='Epsilon' then true
    elif member(E,alphabet) then false
    elif op(0,E)='Union' then convert(map(nullable,E, alphabet),`or`)
    elif op(0,E)='Prod' then convert(map(nullable,E, alphabet),`and`)
    elif op(0,E)='Sequence' then true
    end if
end proc:

##################################################################
# follow
# Input:  E Expression
#         x position (marked letter)
#         BigAlpha : alphabet of marked letters
# Output: set of following positions
##################################################################
follow:=proc(E,x,BigAlpha)
    local res, i;

    if member(E,BigAlpha union {'Epsilon'})   then res:={}
    elif op(0,E)='Union' then res:=`union`(op(map(follow,E,x,BigAlpha)))
    elif op(0,E)='Prod'  then
	res:=follow(op(1,E),x,BigAlpha);
        if nops(E)>1 then
	    if member(x,Glast(op(1,E),BigAlpha)) then 
                 res:=res union Gfirst(subsop(1=NULL,E),BigAlpha)
	    else res:=res union follow(subsop(1=NULL,E),x,BigAlpha) 
	    end if
	end if
    elif op(0,E)='Sequence'  then
         res:=follow(op(E),x,BigAlpha);
         if member(x,Glast(op(E),BigAlpha)) then res:=res union Gfirst(op(E),BigAlpha) end if
	 
	 ######### TEST ##################
	 ## if member(x,Glast(op(E),BigAlpha)) then 
	 ##   if res intersect Gfirst(op(E)) <> {} 
	 ##         then printf("WARNING; not in star-normal-form\n");
	 ##	  print(res intersect Gfirst(op(E),BigAlpha));
	 ##     fi;
	 ##     res:=res union Gfirst(op(E),BigAlpha) 
         ## fi;
	 ########## END PROC TEST ##############
    else ERROR("unrecognizable expression",E)
    end if;

    if member('Epsilon',res) then ERROR("follow",E,x,BigAlpha,res) end if;
    res;
end proc:
               
            
##################################################################
# ExpandR
# Input: R, expression
#        G, grammar
# Output: R expanded
# Iterative substitution of non-terminals
##################################################################

ExpandR:=proc(R,G)
   local S,T1,T2;

   S:= remove(type,G,anything=identical('Atom'));

   T1:=R; T2:=0;

   while T2 <> T1 do
      T2:=T1;
      T1:=subs(S,T2);
   end do;
   T1
end proc:


countl:=proc() option remember; 0 end proc:

##############################################################################
# GenMarkLet
# Input: A - set of atoms
#        BigAlphaE: alphabet of marked letters
# Output: increased BigAlphaE
##############################################################################

GenMarkLet:=proc(A::set, BigAlphaE::name)
    local c;
    global countl;

    c:=countl(A)+1;
    countl(A):=c;
    BigAlphaE := eval(BigAlphaE) union {Marklet(c,A)}; # {A[countl(A)]};
    Marklet(c,A)
end proc:

#############################################################################
# MarkLetters
# Input: R regular expression
#        G grammar
#        BigAlphaE : name of big alphabet in construction
# Output: S regular expression with marked letters
# Side Effect: increases BigAlphaE
#############################################################################

MarkLetters:=proc(R,G,BigAlphaE::name)
    if R='Epsilon' then 'Epsilon'
    elif type(R,name) then GenMarkLet({R},BigAlphaE)
    elif op(0,R)=Union and subs(G,{op(R)})={Atom} then
                           GenMarkLet({op(R)},BigAlphaE)
    else map(MarkLetters,R,G,'BigAlphaE')
    end if
end proc:


####################################################################
#
# CLASSICAL CONSTRUCTIONS WITH EPSILON TRANSITIONS
#
####################################################################

####################################################################
# autounion : union of a list of automata (general case)
# Input:
#    L - a Table list (one Table for each Automaton)
#    W - a Table (one automaton)
# Output: NULL
# Side effect:   W - Table (Automaton) contains the union at the end proc
# Assumption : {out} has no transitions
####################################################################

autounion:=proc(L::list,W::table)
    local auto,
    F;  # set of final states
    
    W['liststates']:={'init'};
    W['init']['epsilon']:={};
    W['init']['trans']:={'epsilon'};
    W['init']['epsilon']:={seq(autocopy(auto,W,'F[auto]'),auto=L)};
    W['final']:=`union`(seq(F[auto],auto=L));
    W['alphabet']:= `union`({'epsilon'},seq(auto['alphabet'],auto=L));
    NULL
end proc:


####################################################################
# autofastunion : union of a list of Atoms and Epsilon (general case)
# Input:
#    L - a list of Atoms (and Epsilon)
#    W - a Table (one automaton)
# Output: NULL
# Side effect:   W - Table (Automaton) contains the union at the end proc
# Assumption : {out} has no transitions
####################################################################

autofastunion:=proc(L::list,W::table)
    local auto, l,
    F;  # final state op(F);
    F:=newstate();
    W['liststates']:={'init',F};
    W['init']['trans']:={op(L)};
    for l in L do W['init'][l]:={F} end do;
    W['final']:={F};
    W['alphabet']:= {seq(l,l=L)};
    NULL
end proc:

############################################################
# autoconcat : concatenation of several automata
# Input:
#    L - a Table list (one Table for each Automaton)
#    W - Table (Automaton)
# Output: NULL
# Side effect: W contains the concatenation of the automata of L
# at the end proc
# Copy each automata form last to first
# final states are final states of last automaton
# put an epsilon transition for each final state of
# automaton i to the initial state of automaton i+1
# create a new {init} with epsilon transition to the old
# init of the first automaton
# Optimization: if automaton i has only 2 states, put
# directly transitions from this initial state to the initial state
# of automaton i+1
###########################################################
autoconcat:=proc(L::list,W::table)
    local i, j, l, n, newinit, oldinit,
    oldLS, # old list of states
    S,  # set of states of automaton i
    f,  # final state when alone
    F;  # set of final states

    W['liststates']:={'init'};
    i:=1; n:=nops(L);
    oldinit:=autocopy(L[n],W,'F');
    W['final']:=F;
    oldLS:=W['liststates'];

    for i from n-1 by -1 to 1 do
      newinit:=autocopy(L[i],W,'F');
      S:=eval(L[i]['liststates']);
      f:=op(eval(L[i]['final']));
      if nops(S) = 2 
        and nops({f}) = 1
        and not assigned(L[i][f]['trans']) then   
        for l in W[newinit]['trans'] do W[newinit][l]:={oldinit}; end do;
	W['liststates']:=oldLS union {newinit};
      else
	for j in F do
	    if not assigned(W[j]['trans']) then W[j]['trans']:={'epsilon'};
	    else W[j]['trans'] := W[j]['trans'] union {'epsilon'}  end if;
	    if not assigned(W[j]['epsilon']) then W[j]['epsilon']:={oldinit}
	    else W[j]['epsilon'] := W[j]['epsilon'] union {oldinit}  end if;
	end do;
      end if;
      oldinit:=newinit;
      oldLS:=W['liststates'];
    end do;
    W['init']['epsilon']:={oldinit};
    W['init']['trans']:={'epsilon'};
    W['alphabet']:= `union`({'epsilon'},seq(i['alphabet'],i=L));

    NULL
end proc:



############################################################
# autofastconcat : product of Atoms
# Input:
#    L - a list of Atoms
#    W - Table (Automaton)
# Output: NULL
# Side effect: W contains the automata of the input Product
###########################################################
autofastconcat:=proc(L::list,W::table)
    local l, curs, nexs, i, n;
    n:=nops(L);
    W['liststates']:={'init'};
    curs:='init';
    for i from 1 to n do
        nexs:=newstate();
        W[curs]['trans']:={L[i]};
        W[curs][L[i]]:={nexs};
        W['liststates']:= W['liststates'] union {nexs};
        curs:=nexs;
    end do;
    W['final']:={curs};
    W['alphabet']:= {seq(i,i=L)};
    NULL
end proc:


############################################################
# autoseq : sequence of one automaton (Star operation)
# Input:
#    U - a Table (one automaton)
#    W - Table (Automaton)
# Output: NULL
# Side effect: copy U in W and add epsilon transitions
# for the star operation
###########################################################
autoseq:=proc(U::table,W::table)
    local f, newinit,
    F;  # set of final states

    W['liststates']:={'init'};
    newinit:=autocopy(U,W,'F');
    W['init']['epsilon']:={newinit};
    W['init']['trans']:={'epsilon'};
    for f in F do
	if not assigned(W[f]['trans']) then W[f]['trans']:={'epsilon'};
	else W[f]['trans'] := W[f]['trans'] union {'epsilon'}  end if;
	if not assigned(W[f]['epsilon']) then W[f]['epsilon']:={newinit}
	else W[f]['epsilon'] := W[f]['epsilon'] union {newinit} end if;
    end do;
    
    W['final']:=F union {'init'};
    W['alphabet']:=U['alphabet'] union {'epsilon'};
    NULL
end proc:

#################################################################
# autonot : Negation of a regular expression
# Input:
#    U - a Table (one possibly non deterministic automaton)
#    R - Table (Automaton)
#    alpha - alphabet
# Output: NULL
# Side effect: load in R a deterministic automaton
#    equivalent to U; invert the terminal and non terminal states
#    in this automaton R
# Comment: the determinisation of the NDFA is done exhaustively.
#          If the automaton is not complete for the alphabet,
#          an empty state is added and the automaton is completed
##################################################################
autonot:=proc(U::table,R::table, alpha)
    local s, l, J, Y, V, W;

    V:=autodeterminize(U);

    # complete the DFA if necessary 
    W:=autocomplete(V,alpha);

    W['final'] := W['liststates'] minus W['final'];
    W['alphabet']:=alpha;

    Y:=autoprune(W);
    R:=autorenumber(Y,newstate());
    NULL
end proc:


##################################################
# autocopy : copy of an automaton into another one
# Input: U::automaton to copy, W::automaton
#        'final' = name for the set of final states
#        'liststates' = list of renamed states (optional)
# Output: new name of state 'init'
# Side Effect: rename and copy states of U in W
#              assign the set of final states to 'final'
##################################################

autocopy:=proc(U::table,W::table,F::name,tr::name)
    local s, l, transfer;

    if not assigned(W['liststates'])  then W['liststates']:={}; end if;
        
    for s in U['liststates']  do
	transfer(s):=newstate();
	W['liststates']:=W['liststates'] union {transfer(s)}
    end do;

    for s in U['liststates']  do
	if assigned(U[s]['trans']) then
	    for l in U[s]['trans'] do
		W[transfer(s)][l] := map(transfer,U[s][l])
	    end do;
	    W[transfer(s)]['trans']:=U[s]['trans']
	end if
    end do;
    W['alphabet']:=U['alphabet'];
    F:=map(transfer,U['final']);

    if nargs=4 then tr:=op(4,op(transfer)) end if;

    transfer('init')
end proc:

##############################################################
# gramtoauto
#  from combstruct grammar for automaton to internal representation
#  as table
# Input:
#  C : combstruct grammar for an automaton
#      {..., w_i=R_i, ...}
#      where R_i =  Epsilon
#                  or
#		    Union([Epsilon],Prod(l_k,w_k),Prod(..,..),...)
#                   l_k in alphabet, w_k state
#  start: name of the start state
#  marks: (optional) list of marks
# Output :
#  T : table for an automaton
# Comment : Marks forbidden, NFA allowed
##############################################################

gramtoauto:=proc(C::set, start::name, marks::list)
    local G, i, j, k, l, s, t, H, K, T, atomset, marklist, alpha;

    if nargs=3 then   marklist:=marks  else   marklist:=[];   end if;

    G:= C minus {seq(i='Epsilon',i=marklist)};

    atomset:=select(type,G,anything=identical('Atom'));
    alpha:=map2(op,1,atomset);

    H:={seq(op(1,i)=pruneunion(op(2,i)), i=G)};

    for i in H do
      if   op(2,i)=Prod  then checkprod(i,[op(op(2,i))],alpha,marklist)
      elif op(2,i)=Union then
        for j in op(op(2,i)) do
           if op(0,j)=Prod then checkprod(i,[op(j)],alpha,marklist)  
           elif j<>'Epsilon' then error "uncorrect specification %1", i: end if
        end do
      end if
    end do;

    T:=table([]);

    K:= subs(start='init',H minus atomset);

    T['liststates']:={seq(op(1,i),i=K)};
    T['final']:={};
    
    for i in K do
       s:=op(1,i);
       if has(op(2,i),'Epsilon') then T['final']:=T['final'] union {s} end if;

       j:=op(2,i);
       if j<>'Epsilon' then
          if op(0,j) = Prod then  loadtrans(T,s,j,marklist);
          else 
            for k in op(j) do
              if k<>'Epsilon' then  loadtrans(T,s,k,marklist);  end if
            end do
          end if
       end if
    end do;
    T['alphabet']:=alpha;     
    autorenumber(T,1);
end proc:

######################################################
# coherence control of the parameters of a Prod
# must be "letter, sequence of marks, state"
######################################################

checkprod:=proc(i,L::list,alphabet,marklist)
      local j;
   if not member(L[1],alphabet) then
      error "uncorrect specification %1",i
   elif nops(L)>2 then
	if {sequence(L[j],j=2..nops(L)-1)} minus {op(marklist)} <> {}
	     then error "mark non specified in the mark list %1, %2",i, marklist
	end if
   end if
end proc:

#############################################################
# loadtrans
# Input: T automaton in construction
#        s : state
#        j :
#        there is a transition with letter op(1,j)
#        from state s to state op(nops(j),j)
#        marks are in positions 2..nops(j)-1
# Output: Null
# Side effect: load the transition in T
############################################################

loadtrans:=proc(T::table,s, j, marklist::list)
   local l, t, i, k, p, m;
   l:=op(1,j); t:=op(nops(j),j); 
   if not assigned(T[s][l]) then T[s][l]:={t}
   else				 T[s][l]:=T[s][l] union {t} end if;
   if not assigned(T[s]['trans']) then T[s]['trans']:={l}
   else				       T[s]['trans']:=T[s]['trans'] union {l} end if;

   # handle marks
   if nops(j)>2 then
      for i from 1 to nops(marklist) do	 p[marklist[i]]:=i end do;
      for i from 2 to nops(j)-1 do
	  m:=op(i,j); 
	  k:=p[m]; 
          if not assigned(T['mark'][k])  then T['mark'][k]:={t}
	  else			              T['mark'][k]:=T['mark'][k] union {t} end if;
      end do
   end if
end proc:

##############################################################
# regexpnottoauto :
#  from combstruct grammar representing regular expressions
#  to table description of a regular expression
# Input:
#  G: Combstruct grammar
#  E: a non-terminal symbol of the grammar
# Output:
#  T: table
# Comment: Marks are not allowed 
#          handles grammars with Not operator
##############################################################
regexpnottoauto:=proc(G,E)
    local i, j, k, R, allE,
    K,   # set of symbols already rewritten
    Leq; # list of sorted rewriting rules with increasing complexity
         # from Atom productions to complex productions

    # check that no expression contains letters of size zero
    for i in G do
        if nops(i)=2  and i='Epsilon' then
	    ERROR(i, `Epsilon in a grammar for regular expression`)
        end if;
    end do;

    # if  {E} minus {seq(op(1,i),i=G)} <>{} 
    if not member(E,{seq(op(1,i),i=G)})
	then ERROR(E,`is not a list of symbols of the input grammar`);
    end if;

    Leq:=sortbyindets(G);
    allE:={E};
    # for k to nops(Leq) while allE<>{} do allE:=allE minus {op(1,Leq[k])} od;
    for k to nops(Leq) while not member(E, {op(1,Leq[k])}) do end do; #k:=k+1;
    
    for j to min(k,nops(Leq)) do
        i:=Leq[j];
	if op(2,i)='Atom' then 
		recregexpnottoauto(G,op(1,i)):=atomtoauto(op(1,i));
	else    recregexpnottoauto(G,op(1,i)):=recregexpnottoauto(G,op(2,i)); end if;
    end do;
    R:=recregexpnottoauto(G,E);
    forget(recregexpnottoauto);
    forget(atomtoauto);
    R
end proc:

###########################################################
# atomtoauto
# Input: an Atom
# Output: the corresponding  table
###########################################################
atomtoauto:=proc(X::name)
   option remember;
   local T, out;
   out:=newstate();
   T['alphabet']:={X};
   T['init'][X]:={out};
   T['init']['trans']:={X};
   T['liststates']:={'init',out};
   T['final']:={out};
   T
end proc:

###########################################################
# epsilontoauto
# Input: an Epsilon - empty word
# Output: the corresponding  table
###########################################################
epsilontoauto:=proc(T::table)
   option remember;

   T['liststates']:={'init'};
   T['final']:={'init'};
   T['alphabet']:={'epsilon'};
   T
end proc:


###########################################################
# recregexpnottoauto : combstruct description to table description
# Input: a non-recursive regular expression
# Output: the corresponding table
###########################################################
recregexpnottoauto:=proc(G,E) # CHECK TYPE EXPRESSION?
  option remember;
  local T, i, f, alphabet;
  
  T[E]:=table([]);
  if E='Epsilon'
    then epsilontoauto(T[E]);
  else
    if not type(E,function)
     then error "unrecognizable item %1",E;
    end if;
    if op(0,E)=Prod then
       if subs(G,{op(E)}) = {Atom}
            then autofastconcat([op(E)],T[E]);
       else      autoconcat([seq(recregexpnottoauto(G,i),i=[op(E)])],T[E]); end if;
    elif op(0,E)=Union then
       if subs(G,{op(E)}) = {Atom} 
            then autofastunion([op(E)],T[E]);
       else      autounion([seq(recregexpnottoauto(G,i),i=[op(E)])],T[E]); end if;
    elif op(0,E)=Sequence
       then autoseq(recregexpnottoauto(G,op(E)),T[E]);
    elif op(0,E)=Not  then 
	 alphabet:=map2(op,1,select(type,G,anything=identical('Atom')));
	 autonot(recregexpnottoauto(G,op(E)),T[E],alphabet);
    else error "unknown constructor %1", E
    end if;
  end if;
  T[E]
end proc:

#########################################################
# autoepssuppress : Clean automaton
# States with an epsilon transition to only one state
# and no other transition are merged with the corresponding 
# next state (this is a possibly recursive merging)
# Input: a table describing an automaton
# Output: the same table after elimination of epsilon transitions
# and possible state merging
#########################################################

autoepssuppress:=proc(T::table)
    local clos, i, j, k, l, States_Computed;
    
    States_Computed:={};

    for i in T['liststates'] do clos[i]:=epsclosure(T,i,'States_Computed'); end do;
    for i in T['liststates'] do
	for j in clos[i] do 
	    if assigned(T[j]['trans']) then
		for l in T[j]['trans'] minus {'epsilon'} do
		    T[i]['trans']:=T[i]['trans'] union {l};
		    if not assigned(T[i][l]) then 
		           T[i][l]:=`union`(        seq(clos[k],k=T[j][l]))
		    else   T[i][l]:=`union`(T[i][l],seq(clos[k],k=T[j][l])) end if
		end do
	    end if
	end do
    end do;
    # Remark: final states are those that reach final states by epsilon closure
    # Beware that epsilon successors of final states may not be final
    for i in T['liststates'] do
        if clos[i] intersect T['final'] <> {}
	    then T['final']:=T['final'] union {i};
	end if
    end do;
    # eliminate epsilon transition
    for i in T['liststates'] do
	if assigned(T[i]['trans']) and member('epsilon',T[i]['trans']) then
	    unassign(evaln(T[i]['epsilon']));
	    T[i]['trans'] := T[i]['trans'] minus {'epsilon'};
	end if
    end do;

    T['alphabet'] := T['alphabet'] minus {'epsilon'};
    
    forget(epsclosure);
    forget(epsclosureset);
end proc:


#################################################################
# epsclosure: closure of a set of states with epsilon transitions
# Input: T - table of the automaton
#        i - state considered
# Output: epsilon closure of i
#################################################################
epsclosure:=proc(T::table,i, States_Computed::name)
    option remember;
    local res, s;
    
    if assigned(T[i]['trans']) and member('epsilon',T[i]['trans']) then
            res := {seq(op(epsclosureset(T,s,{},States_Computed)),s=T[i]['epsilon'])};
    else    res := {i};  end if;

    States_Computed := eval(States_Computed) union {i};
    res union {i};
end proc:

#################################################################
# epsclosureset:
#        auxiliary function for closure of a set of states with epsilon 
#        transitions
# Input: T - table of the automaton
#        state - state considered
#        J - set in construction for epsilon closure of a
#            given state
# Output: augmented set J
#################################################################
epsclosureset:=proc(T::table,state,J::set,States_Computed::name)
    option remember; # prevents loops in cycles of epsilon
    local res, i;
    
    if member(state,J) then J;
    elif not assigned(T[state]['trans'])
      or not member('epsilon',T[state]['trans']) then J union {state};
    else
	res:= J union {state};
	for i in T[state]['epsilon'] do
            if member(i,eval(States_Computed)) then
                    res:= res union epsclosure(T,i,States_Computed);
            else    res:=epsclosureset(T,i,res,States_Computed);
            end if
	end do;
	res
    end if
end proc:


##############################################################
# END PROC OF CODE FOR EPSILON CONSTRUCTION
##############################################################


#################################################################
# regexptomatchesgram: regular expression to marked grammar
#    for the automata recognizing Sigma^* R_i
# Input:
#   Gram - input grammar
#   E - name of the output start state
#   L - list of lists
#     L[i][1] - non terminal of the grammar
#     L[i][2] - name for the corresponding mark
#     L[i][3] - `renewal` or `overlap`
#     L[i][4] -  error: 'Error'[k,errors] (optional)
#                   k : number of errors allowed
#                   errors: subset of {'subs','ins',del'}
# 
# Output:
#   A specification for an automaton recognizing texts
#   where each match with regular expression i outputs
#   mark i
# Comment
#   Processing the result with the Chomski-Schutzenberger
#   algorithm outputs a multivariate generating function
#   counting in each parameter the number of occurrences
#   of the corresponding regular expression in a random text.
#################################################################### 

regexptomatchesgram:=proc(Gram,E,L::list)
    local XRGram, alpha, i, k, l, u, U, D, DFA, R, Gout, O, T,
          j, W, 
          S; # NDFA for X^*reg or X^*err(reg) (for current reg)

    if `union`({seq(op(3,U),U=L)}) minus `union`({'renewal'},{'overlap'}) <> {} then
        error "bad specification of overlapping parameter, should be renewal or overlap"
    end if;

    alpha:=map2(op,1,select(type,Gram,anything=identical('Atom')));
    
    for k from 1 to nops(L) do

       if nops(L[k])>4 then error "bad specification of 3rd parameter %1", i 

       # pattern with errors
       elif nops(L[k])=4 then
           if op(0,L[k][4]) <> 'Error'
              or type(op(1,L[k][4]),integer)<>true
              or type(op(2,L[k][4]),set)    <>true
	      or      op(2,L[k][4]) minus {'subst','ins','del'} <> {} then 
               error "bad specification of error parameter %1", L[k][4]
           end if;
	   R:=regexptoauto(Gram,op(1,L[k]));
           S:=autoerror(R,op(1,L[k][4]),op(2,L[k][4]));
           # build transition with alpha from init to init for NDFA X^*err(reg)
           S['init']['trans']:=S['alphabet'];
	   for l in S['alphabet'] do 
               if assigned(S['init'][l]) then S['init'][l]:=S['init'][l] union {'init'}
               else			      S['init'][l]:={'init'}  end if;
	   end do
       else
          XRGram:= Gram union {X=Union(op(alpha))} union {XR=Prod(Sequence(X),op(1,L[k]))};

          if has(Gram,Not) then S:=regexpnottoauto(XRGram,XR);
          else                  S:=bersettoauto(XRGram,XR,alpha); end if;
       end if;
       W:=autodeterminize(S);   DFA[k]:=autominimize(W);
    end do;

    if nops(L) > 1 then
        W:=autocarprod([seq(DFA[k],k=1..nops(L))]);
        W['final']:=W['liststates'];
    else	        W:=DFA[1]; W['mark'][1]:=W['final'];	end if;

    if has(L,'renewal') then
      if nops(L)=1 then j:=[0]
      else
        j:=[]; 
        for i to nops(L) do
             if op(3,L[i]) = 'renewal' then j:=[op(j),i]; end if
        end do;
      end if;
      autorenewal(W,j);
    end if;
 
    W['final']:=W['liststates'];
    W['checked']:=true;
    autotogram(W,E,[seq([k,L[k][2]],k=1..nops(L))]);

end proc:

###############################################################
# grammarkov : bernoulli grammar to markov grammar
# Input:
#    G - gram
#    R - start name
#    morder - order of the Markov chain
#    mlet - name of Markov letter
# Output
#    Markov grammar
# Restriction: marks are not allowed in the grammar
###############################################################

grammarkov:=proc(G::set, R::name, morder::integer, mlet::name)
    local M, T, atomset, i, marklist;

    marklist:=getmarks(G);

    atomset:=select(type,G,anything=identical('Atom'));

    T:=gramtoauto(G,R, marklist);
    M:=automarkov(T,morder,mlet);
    M['checked']:=true;
    autotogram(M,R,[seq([i,marklist[i]],i=1..nops(marklist))]) 
      union {seq(i=Atom, i=M['alphabet'])} minus atomset
      union {seq(i='Epsilon', i=marklist)}
end proc:

###############################################################
# markgram : mark a grammar obtained for a Sigma^*R expression
#            to count number of occurrences of R
# Input:
#    G: gram
#    EpsMark: list of lists = [..,[Eps_i,Mark_i],..]
#             where Eps_i is 
#                  an indexed Epsilon[k], k integer
# Output:
#    O: marked grammar
##############################################################

markgram:=proc(G::set,EpsMark::list)
    local i, j, u, e, m, r, W, S, O, tr;

    # build the set of final states
    # put a mark when entering a final state

    for i in op(G) do
       j:=op(1,i);  tr(j):={};
       
       for u in EpsMark do
          e:=op(1,u);  m:=op(2,u);
          if has(i,e) then tr(j) := tr(j) union {m} end if;
       end do;
       tr(j):=op(tr(j)),j;
    end do;

    S:={seq(op(1,i),i=G)};
        
    W:=subs(seq(op(1,i)=NULL,i=EpsMark),G);

    O := {};
    for i in op(W) do
       if op(2,i) = Atom   then  O := O union {i}
       elif op(2,i)<>NULL then
            r:= pruneunion(op(2,i));  
	    r:= subs(seq(j=tr(j),j=S),r); 
            O := O union {op(1,i)=r}
       end if
    end do;             

    subs(Union('Epsilon')='Epsilon', O union {seq(op(2,u)='Epsilon',u=EpsMark)})
end proc:

#################################################################
# regexpstowaitgram: 
#   provides an automaton where only valid transitions
#   from matches to RE1 to matches to RE2 are referred to as Atom
#   There may be fake transitions in this automaton, referred to
#   as Epsilon
# Input:
#   G: input grammar
#   RE1 : list [name, error]
#                name: name of 1rst expression (possibly Epsilon)
#                error: 'Error'[k,errs] (optional)
#                      k - number of errors
#                      errs - subset of {'subst','ins','del'}
#   RE2 : list [name, error]
#                name: name of 1rst expression (possibly Epsilon)
#                error: 'Error'[k,errs] (optional)
#                      k - number of errors
#                      errs - subset of {'subst','ins','del'}
#   w : present if RE1<>Epsilon, absent elsewhere
#       name of the mark for waiting transitions
#   rmt :
#      present if RE1<>Epsilon, absent elsewhere
#      `rematch` - after 1rst match with RE1, other matches are
#		   allowed until 1rts match with RE2
#      `norematch` - no match with RE1 is allowed between 1st match
#		     with RE1 and 1rst match with RE2
#
# Output:
#   O: grammar for automaton
# Comment: 
#   if RE1=Epsilon, the output automaton provides
#      the first match with R2
#   if norematch, compute the product automaton for
#   RE1 * XRE1 * XRE2 (X=alphabet)
#################################################################

regexpstowaitgram:=proc(G::set,LRE1::list,LRE2::list,mrk::name,rmt)
    local S1, XS2, XRE2, XGW2, M1, M2, W, RE1, RE2, WS1, WS2,
          j, k, J, err, ER, s, l, t, tr, U, remtch,
          alpha, XRE1, XGW1, XS1;

    RE1:=LRE1[1]; RE2:=LRE2[1];


    if RE1<>'Epsilon' then 
        if   nargs<>5 then error "not enough input parameters" end if;
	if   args[5]=  'rematch' then remtch:=true
	elif args[5]='norematch' then remtch:=false
	else 
          error "bad specification of flag, should be rematch or norematch, %1", args[5]
	end if
    end if;

    J:=[LRE1,LRE2];
    for k to 2 do
      j:=J[k];
      if   nops(j)>2 then  error "bad specification of parameter %1", j
      elif nops(j)=2 then
           err:=j[2];
           if op(0,err)<>'Error' 
              or type(op(1,err),integer)=false
              or op(2,err) minus {'subst','ins','del'} <> {} then
              error "bad specification of error parameter %1", err
           end if;
           ER[k]:=err;
      else ER[k]:=NULL;
      end if
    end do;

    alpha:=map2(op,1,select(type,G,anything=identical('Atom')));

    XGW2:=G union {X=Union(op(alpha)),XRE2=Prod(Sequence(X),RE2)};

    if ER[2]=NULL then XS2:=regexptoauto(XGW2,XRE2)
    else	    
       WS2:=autoerror(regexptoauto(G,RE2),op(1,ER[2]),op(2,ER[2]));
       XS2:=autotoXstarauto(WS2)
    end if;

    if RE1='Epsilon' then
        for s in XS2['final'] do
	    for l in XS2[s]['trans'] do XS2[s][l]:=evaln(XS2[s][l]); end do;
	    XS2[s]['trans']:=evaln(XS2[s]['trans'])
        end do;
	XS2['checked']:=true;
        autotogram(XS2,RE2)
    else
        if ER[1]=NULL then  S1:=regexptoauto(G,RE1);
        else                S1:=autoerror(regexptoauto(G,RE1),op(1,ER[1]),op(2,ER[1]))
	end if; 

	# if norematch, compute automaton X.Re1
	if remtch=false then
	    if ER[1]=NULL then 
		XGW1:=G union {X=Union(op(alpha)),XRE1=Prod(Sequence(X),RE1)};
		XS1:=regexptoauto(XGW1,XRE1);
            else 
		WS1:=autoerror(regexptoauto(G,RE1),op(1,ER[1]),op(2,ER[1]));
		XS1:=autotoXstarauto(WS1)
            end if;

	    W:=autocarprod([S1,XS2,XS1]);
	else 
	    W:=autocarprod([S1,XS2]);
	end if;

        # duplicate automaton W
        newstate();
        for s in W['liststates'] do tr(s):=newstate(); end do;

        for s in W['liststates'] do
	    for l in W[s]['trans'] do  W[tr(s)][l]:={tr(op(W[s][l]))} end do;
            W[tr(s)]['trans']:=W[s]['trans'];
        end do;

        # move transitions from RE1 final states into the duplicated
	# automaton
        for s in W['mark'][1] do
	    for l in W[s]['trans'] do  W[s][l]:={tr(op(W[s][l]))} end do;
	end do;


        # prune out transitions from ['mark'][2] in copied automaton
	W['mark'][2]:={seq(tr(s),s=W['mark'][2])};
        for s in W['mark'][2] do
            for l in W[s]['trans'] do W[s][l]:=evaln(W[s][l]) end do;
            W[s]['trans']:=evaln(W[s]['trans']);
	end do;

	# for marking, all copied states are mark[1]
	W['mark'][1]:={seq(tr(s),s=W['liststates'])};

        # set RE2 (final) marked states of duplicated automaton to final states
        # and prune automaton

	W['final']:= W['mark'][2]; W['mark'][2]:=evaln(W['mark'][2]);

	W['liststates']:=W['liststates'] union {seq(tr(s),s=W['liststates'])};


        # if norematch, erase transitions to final states of XRE1 in the
	# copied automaton and from finals of RE1
	if remtch=false then
            W['mark'][3]:=map(tr,W['mark'][3]);
	    for s in W['liststates'] do
		for l in W[s]['trans'] do
		    if member(op(W[s][l]),W['mark'][3]) then
			    W[s][l]:=evaln(W[s][l]);
			    W[s]['trans']:=W[s]['trans'] minus {l}
		    end if
		end do
	    end do;
	    W['mark'][3]:=evaln(W['mark'][3]);
	end if;

        U:=autoprune(W);
	U['checked']:=true;
 
        autotogram(U,RE2,[[1,mrk]]);
    end if
end proc:



################################################################
# Input: automaton recognizing R
# Output: automaton recognizing Sigma^*R
################################################################

autotoXstarauto:=proc(T::table)
       local O, l;
        
       for l in T['alphabet'] do
	  if assigned(T['init'][l]) then T['init'][l]:=T['init'][l] union {'init'}
          else			         T['init'][l]:={'init'}   end if;
       end do;
       O:=autodeterminize(T);
       autominimize(O)
end proc:


#################################################################
# autorenewal : handles renewal cases
# Input: W - table for automaton
#        mk - list of mark (or final) to renewal
#             0 -> W['final'], i -> W['mark'][i] (i>0)
# Output: NULL
# Side effect: modifies transitions of W in case of renewal
# Comment: in case of conflicts 'renewal' <-> 'overlap'
#          the 'renewal' command is processed
###########################################################
autorenewal:=proc(W, mk::list)
    local i, s, l, ovtype, U, alpha;

  # RENEWAL AFTER MATCH? 
  alpha:=W['alphabet'];

  for i from 1 to nops(mk) do
    if mk[i] = 0 then U:=W['final'] else U:=W['mark'][i] end if;

    if type(U,set)<>true then error "not assigned %1", U end if;

    for s in W['liststates'] do
        if  member(s, U) then 
	   for l in alpha do  
             if  assigned(W['init'][l]) then  W[s][l] := W['init'][l]; 
             else                             W[s][l] := {'init'}
             end if;
           end do;
        end if;
    end do;
  end do;
  NULL
end proc:




######################################################################
# autominimize: Automaton minimization from Hopcroft in n*log(n)
# Input:  Y - table of the (non-minimal) automaton
# Output: Q - table of a minimal automaton equivalent to T
# References:
# 1) Hopcroft (1971) "An n log(n) algorithm for minimizing states in a finite
#    automaton", The theory of machines and computations, edited by Z. Kohavi,
#    Academic Press
# 2) Aho, Hopcroft, Ullman (1974) "The design and analysis of computers
#    algorithms", p. 159
# Remark : the maple implementation is n log(n) except for the time in
#    the gc
######################################################################

autominimize:=proc(Y::table)
    local T,
          W,  # reversed automaton
          i, j, l, m, s, U, J, K, Q, O, alpha, P, R,
	  k,  # number of classes
          L,  # list of classes to use as discriminants
	  tr, # used to sort the classes by cardinal
          Slist, # used for sort
          kmin, kmax, # first and last number of classes in L
	      # in the main loop, the first class is removed, and everytime
	      # a class is split, a new class is append proced with kmax as number
          c,  # class number
          B,  # B[i] is class i in partition of the states of T
	      # B[1] non-final states, B[2]..[3].. final states
          Bsort, # indices of B sorted by increasing cardinality
          C,  # delta^(-1)(B(.)) antecedants de B(.)
          BI, # i if that state s is in set B[i]
	  F;

    # Set eventually a check that the automaton is complete

 alpha:=Y['alphabet'];

 # check that the automaton is deterministic
 if not assigned(Y['checked']) or Y['checked']<>true then
   for s in Y['liststates'] do 
    for l in Y[s]['trans'] do
        if nops(Y[s][l])>1 then
	   error "the input automaton is not deterministic"
        end if
    end do
   end do
 end if;
 T:=autocomplete(Y); 

 W:=autoreverse(T);

 if not assigned(T['mark']) then
      k:=2; 
      if assigned(T['final'])  then  B[2]:=T['final'];
      else                           B[2]:={}         end if;
      B[1]:=T['liststates'] minus B[2]; 
 else k:=statesplit(T,'B');              end if;

 if k=2 and B[1] = {} then  # R=Sigma^*
    O['liststates']:={'init'};
    O['init']['trans']:=alpha;
    O['alphabet']:=alpha;
    for s in alpha do O['init'][s]:={'init'}; end do;
    O['final']:={'init'};
    if assigned(T['mark']) then
       for i in map(op,[indices(T['mark'])]) do
          O['mark'][i]:={'init'}
       end do
    end if;
    O
 elif k=2 and B[2] = {} then # R={}
    O['liststates']:={};
    O['final']:={};
    O['alphabet']:={};
    O
 else
    for j from 1 to k do  for s in B[j] do BI[s]:=j end do    end do; 
    
    if k=2 then
      if nops(B[1]) < nops(B[2]) then L[1]:=1; else L[1]:=2; end if;
    else
      # sort the B[j] by cardinality and load k-1 indices in queue L
      Slist:={};
      for j to k do
        i:=nops(B[j]);  Slist := Slist union {i}; 
	if not assigned(tr[i]) then  tr[i]:=            {j};
	else                         tr[i]:=tr[i] union {j} end if;
      end do;
      Slist:=sort([op(Slist)]);
      Bsort:=[seq(op(tr[i]),i=Slist)];
      for j to k-1 do L[j]:=Bsort[j] end do;
    end if;

    for kmin while kmin<k do
      c:=L[kmin];
      for l in alpha do
	   # this is a crucial step to compute the union with a good complexity
	   # C:= `union`(seq(W[l][s],s=B[c])
	   K:=table();
           for s in B[c] do 
	      if assigned(W[l][s]) then	 for j in W[l][s] do  K[j]:=true end do end if 
           end do;
	   C:= map(op,{indices(K)});

           for j in {seq(BI[s],s=C)} do

              U   :=B[j] intersect C;

              B[j]:=B[j] minus     U;

              if U <> {} then
                if B[j] <> {} then
		   k:=k+1;
                   if nops(U) <= nops(B[j]) then              B[k]:=U
	           else                          B[k]:=B[j];  B[j]:=U    end if;

                   for s in B[k] do BI[s]:=k end do;
                   L[k-1]:=k;

                else B[j]:= U  end if; # RESTORE INITIAL VALUES OF B[j]
              end if;
           end do;
      end do;
    end do;

    Q:=minirenumber(k,B,T,BI,alpha);
    Q['alphabet']:=T['alphabet'];


    if not assigned(T['pruned']) or T['pruned']=false then 
       F:=Q['final'];
       if assigned(Q['mark']) then
	  F:=`union`(F,seq(Q['mark'][i],i=map(op,[indices(Q['mark'])])))
       end if;
       Q:=autoprune(Q,F);
    end if;
    Q
 end if
end proc:

######################################################################
# statesplit: splitting states in subsets homogeneous in relation
#             with the terminal states
#             States i and j belong to the same subset if for each 
#             index of the terminals, they are both terminals or
#             non-terminals
# Input:  T - table of automaton
#         B - name of the splitted set
# Output: k - number of the subset in the splitted set
# Side effect: B is loaded with the splitted set
######################################################################
statesplit:=proc(T::table,B::name)
      local i, k, s, S, U, K;

      for s in T['liststates'] minus T['final'] do
	  if not assigned(U[s]) then U[s]:={0}
	  else                       U[s]:=U[s] union {0} end if;
      end do;
      
      for i in map(op,[indices(T['mark'])]) do
          for s in T['mark'][i] do
	      if not assigned(U[s]) then U[s]:={i}
	      else                       U[s]:=U[s] union {i} end if;
	  end do
      end do;
      k:=1; B[1]:={};
      for s in T['liststates'] do
          if not assigned(U[s])  then B[1]      := B[1]       union {s}
	  elif assigned(K[U[s]]) then B[K[U[s]]]:= B[K[U[s]]] union {s}
          else k:=k+1; K[U[s]]:=k;    B[k]      :=                  {s}
	  end if
      end do;
      k
end proc:
      
######################################################################
# autoreverse: reverse the transitions of an automaton
# Input:  T : table of an automaton
# Output: W : table with reversed transitions
#         W[l][s] = {s1,...,sk}
#         l:letter, s:state, s1,..,sk: predecessors of s
# Comment: nfa allowed
######################################################################
autoreverse:=proc(T::table)
    local W, s, j, l, alpha;

    alpha:=T['alphabet'];

    for s in T['liststates'] do
        for l in alpha do
            if assigned(T[s][l]) then
                for j in T[s][l] do
	          if W[l][j] = evaln(W[l][j]) then
		    W[l][j]:={s} else W[l][j]:={s} union W[l][j] 
                  end if
                end do
            end if
        end do;
    end do;
    W;
end proc:

#####################################################################
# minirenumber: renumber states
# Input:
#	T	automaton
#	B	partitions of its states
#	k 	size of B
#	alphabet 
#       N       N[s] is the class of state s
# Output:
#	An automaton whose states are the parts in the partitions
#	and whose transitions come from T.
#	This automaton is equivalent to T.
#####################################################################
minirenumber:=proc(k,B,T,N,alpha)
    local j, l, s, O,
          tr;   # transfer table for the names of the states due to state 'init'
    for j from 1 to k do tr[j]:=j end do;

    tr[N['init']]:='init';

    O['alphabet']:=alpha;

    O['liststates']:={seq(tr[j], j=1..k)};
    for s in O['liststates'] do O[s]['trans']:={} end do;

    # take one representant in each class
    # from this state, compute the state accessed through transition l
    # take the class and renumber it
    for j from 1 to k do
      if B[j] <> {} then
        O[tr[j]]['trans']:=alpha;
        s:=op(1,B[j]);
        for l in alpha do
            O[tr[j]][l]:={tr[ N[ op(T[s][l]) ] ]};
        end do;
      end if
    end do;

    O['final']:={seq(tr[N[s]],s=T['final'])}; 

    if assigned(T['mark']) then
       for j in map(op,[indices(T['mark'])]) do
          O['mark'][j]:={seq(tr[N[s]],s=T['mark'][j])};
       end do;
    end if;
    O
end proc:


######################################################################
# autodeterminize	: transform a NDFA to an equivalent DFA
# Input: T - table of a NDFA automaton
# Output: W - equivalent DFA
# Comments: - the transformation is known as the subset construction
#           - only accessible states of the DFA are generated
######################################################################
autodeterminize:=proc(T::table)
    local W, state,  # state of the DAF, also a set of states of the NDFA
          s,         # state of the NDFA
	  i, l, st,
          processed; # assigned for a state of the DFA if this state is or has
                     # been in the stack

    W:=table([]);
    if member('epsilon',T['alphabet']) then  autoepssuppress(T);  end if;

    for s in T['liststates'] do 
	if not assigned(T[s]['trans']) then T[s]['trans']:={}; end if
    end do;
    W['liststates']:={};
    st:=readlib(stack)[new]({'init'});
    processed[{'init'}]:=true;
    while not stack[empty](st) do
	state:=stack[pop](st);
	W['liststates'] := W['liststates'] union {state};
	W[state]['trans'] := `union`(seq(T[s]['trans'],s=state));
	for l in W[state]['trans'] do
	    W[state][l] := {};
	    for s in state do
		if assigned(T[s][l]) then 
		    W[state][l]:=W[state][l] union T[s][l]
		end if;
	    end do;
	    if not assigned(processed[W[state][l]]) then
		stack[push](W[state][l],st);
		processed[W[state][l]] := true;
	    end if;
	    # beware a state of the DFA is a set of states of the NDFA
	    # and therefore of the form {{sequence of state of the NDFA}}
	    # W[state][letter] is a set of states of the DFA. It is reduced
	    # to a single state because of the determinism of the DFA
	    W[state][l]:={W[state][l]};
	end do;
    end do;

    W['final'] := {};
    for state in W['liststates'] do
	if state intersect T['final'] <> {} then
	    W['final'] := W['final'] union {state}
	end if
    end do;
    if assigned(T['mark']) then
        for i in map(op,[indices(T['mark'])]) do
	    W['mark'][i]:={};
	    for state in W['liststates'] do
		if state intersect T['mark'][i] <> {} then
		    W['mark'][i] := W['mark'][i] union {state}
		end if
	    end do
	end do
    end if;

    W['alphabet']:=T['alphabet'];
    renameinit(W,'init',{'init'});
    W['checked']:=true;

    autorenumber(W,1);
end proc:



######################################################################
# autocarprod	: Product of automata
# Input: L - list of tables of DFA automata
# Output: W is the  product DFA automaton
# Comments: - the transformation is similar the subset construction
#             used for determinisation
#             However the complexity is at most the product of the
#             sizes of the automatas
#           - only accessible states of the DFA are generated
######################################################################
autocarprod:=proc(L::list)
    local state,  # state of the DFA, also a set of states of the NDFA
	  alpha,
          T, j, i, W, U, 
          s,      # state of the NDFA
	  l, st, u,
          processed; # assigned for a state of the DFA if this state is or has
                     # been in the stack


  alpha:=`union`(seq(i['alphabet'],i=L));

  W:=table([]);
  if nops(L) = 1 then
    W:=eval(L[1]);
    W['mark'][1]:=W['final'];
  else
    for i from 1 to nops(L) do T[i]:=autocomplete(L[i]); end do;
    
    W['liststates']:={};
    st:=readlib(stack)[new]([seq('init',i=1..nops(L))]);
    processed[{[seq('init',i=1..nops(L))]}]:=true;
    while not stack[empty](st) do
	state:=stack[pop](st);
	W['liststates'] := W['liststates'] union {state};
	
	W[state]['trans'] := alpha;
	j:={};
	for l in alpha do
	    W[state][l] := {[seq(op(T[i][op(i,state)][l]),i=1..nops(L))]};
	    if not assigned(processed[W[state][l]]) then
		stack[push](op(W[state][l]),st);
		processed[W[state][l]] := true;
	    end if;
	end do;
    end do;
    for i to nops(L) do W['mark'][i]:={}: end do;

    for state in W['liststates'] do
        for i from 1 to nops(L) do
	    if {op(i,state)} intersect T[i]['final'] <> {} then
	        W['mark'][i] := W['mark'][i] union {state}
	    end if
	end do
    end do;
    W['final']:=`intersect`(seq(W['mark'][i],i=1..nops(L)));
    W['alphabet']:=alpha;
  end if;
  U:=renameinit(W,'init',[seq('init',i=1..nops(L))]);
  W:=autorenumber(U,1);
  autominimize(W)
end proc:

###################################################################
# autocomplete
# Input : O automaton
#         A - alphabet (optional argument)
# Output: O  completed
# Comment: the minimization requires a complete automaton
###################################################################
autocomplete:=proc(O::table, A::set)
    local  s, l, alpha, U, Y;

   # complete the automaton if necessary (beware of languages with Epsilon)
   # this handles cases as 'Epsilon+a'

   if nargs=1 then alpha:=O['alphabet']
   else		
       if O['alphabet'] minus A <> {}
           then ERROR("the alphabet must contain the alphabet of the input automaton",A)
       end if;
       alpha:=A; O['alphabet']:=alpha;
   end if;
 
   if `intersect`(seq(O[s]['trans'],s=O['liststates'])) <> alpha then
       O['prune']:=true;
       for s in O['liststates'] do
           if not assigned(O[s]['trans']) then O[s]['trans']:={} end if;
           if O[s]['trans'] <> alpha then
              for l in alpha minus O[s]['trans'] do  O[s][l]:={{}}; end do;
	      O[s]['trans']:=alpha;
	   end if
       end do;
       O['liststates']:=O['liststates'] union {{}};
       O[{}]['trans']:=alpha;
       for l in alpha do O[{}][l]:={{}} end do;
   else  O['prune']:=false;   end if;

   if member({},O['liststates']) then Y:=autorenumber(O,newstate())
   else O     end if
end proc:

###################################################################
# alphaclosure: closure of a state by any sequence of letter
# Input: T - table of the automaton
#        state - state considered
#        J - set in construction for the alphabet closure of a
#            given state
# Output: augmented set J
###################################################################

alphaclosure:=proc(T::table,state,J::set)
    local res,i,l,K, successors;

    if not assigned(T[state]['trans']) then J union {state};
    else
	successors:= `union`(seq(T[state][l],l=T[state]['trans']));
	if `union`(successors, {state}) minus J = {} then J
	else
	    res := `union`({state},successors,J);
	    for i in successors do
		res := alphaclosure(T,i,res);
	    end do;
	    res;
	end if
    end if
end proc:



###########################################################
# autorenumber : copy of an automaton into a renumbered one
# Input: U::automaton
#        b:: initial value for numbering
# Output: W:: renumbered automaton
##################################################

autorenumber:=proc(U::table, b::integer)
    local s, l, W, transfer, i;
    global ns;
    ns:=b-1;

    transfer('init'):='init';
    W['liststates']:={'init'};
    for s in U['liststates'] minus {'init'} do
	transfer(s):=newstate();
	W['liststates']:=W['liststates'] union {transfer(s)}
    end do;

    for s in U['liststates']  do
	if assigned(U[s]['trans']) then
	    for l in U[s]['trans'] do
		W[transfer(s)][l] := map(transfer,U[s][l])
	    end do;
	    W[transfer(s)]['trans']:=U[s]['trans']
	end if
    end do;

    if assigned(U['mark']) then
        for i in map(op,[indices(U['mark'])]) do
            W['mark'][i]:=map(transfer,U['mark'][i]);
	end do;
    end if;
    W['final']:=map(transfer,U['final']);
    W['alphabet']:=U['alphabet'];

    if assigned(U['checked']) then W['checked']:=U['checked'] end if;

    newstate();
    op(W)
end proc:

######################################################################
# gramerror : insert errors (substitution, insertion, deletion)
#             in a language
# Input: G - grammar for an automaton
#        R - start state
#        k - number of errors allowed
#        err - subset of {'subst','ins','del'}
# Output: O - a grammar for aminimized automaton recognizing the language with 
#             errors
# Comment: the input automaton may not be markov
######################################################################

gramerror:=proc(G::set,R::name,k::integer,err::set)
     local A, B, i, marks;

     marks:=getmarks(G);

     A:=gramtoauto(G,R,marks);

     B:=autoerror(A,k,err);

     autotogram(B,R,[seq([i,marks[i]],i=1..nops(marks))]);
end proc:

############################################################
# returns a list of marks of a grammar automaton
############################################################
getmarks:=proc(G::set)
     local i, j, lG, marks;
     
     marks:={};
     lG:={seq(EraseUnion(EraseProd(op(2,i))),i=G)};
     for i in lG do
	if op(0,i)=Prod then
	   if nops(i)>2 then
	        for j from 2 to nops(i)-1 do
		   marks:=marks union {op(j,i)}
		end do
	   end if
	end if
     end do;
     [op(marks)]
end proc:

######################################################################
# autoerror : insert errors (substitution, insertion, deletion)
#             in a language
# Input: A - table for an automaton
#        k - number of errors allowed
#        err: subset of {'subst','ins','del'}
# Output: O - a minimized automaton recognizing the language with 
#             errors
######################################################################

autoerror:=proc(A::table,k::integer,err::set)
     local C, W, U, s, l, l2, i, j, F, tr, B, alpha;

     alpha:=A['alphabet'];

     if err minus {'subst','ins','del'} <> {}
	then ERROR("unknown error edit command", err)
     end if;

     C:=autodeterminize(A);
     B:=autominimize(C);
     B:=autoprune(B);

     W:=table([]);

     W['alphabet']:=alpha;

     for i from 1 to k+1 do autocopy(B, W, 'F', evaln(tr[i])); end do;


     if member('subst',err) then
      for s in B['liststates'] do
       for l in B[s]['trans'] do
	for l2 in alpha minus {l} do
	 for i from 1 to k do
          # W[tr[i][s]]['trans'] is not empty (contains l)
          W[tr[i][s]]['trans']:= W[tr[i][s]]['trans'] union {l2};
	  if not assigned(W[tr[i][s]][l2]) then 
		W[tr[i][s]][l2]:={tr[i+1][op(B[s][l])]}
	  else  W[tr[i][s]][l2]:=W[tr[i][s]][l2] union {tr[i+1][op(B[s][l])]} end if
	 end do
        end do
       end do
      end do
     end if;

     if member('ins',err) then
      for s in B['liststates'] do
	for i from 1 to k do
          W[tr[i][s]]['trans']:= alpha;
	  for l2 in alpha do
	     if not assigned(W[tr[i][s]][l2]) then 
		   W[tr[i][s]][l2]:={tr[i+1][s]}
	     else  W[tr[i][s]][l2]:=W[tr[i][s]][l2] union {tr[i+1][s]} end if
	  end do
        end do
      end do
     end if;

     if member('del',err) then
      W['alphabet']:=W['alphabet'] union {'epsilon'};
      for s in B['liststates'] do
       for l in B[s]['trans'] do
	 for i from 1 to k do
	  W[tr[i][s]]['trans']:=W[tr[i][s]]['trans'] union {'epsilon'};
	  if not assigned(W[tr[i][s]]['epsilon']) then 
		W[tr[i][s]]['epsilon']:={tr[i+1][op(B[s][l])]}
	  else  W[tr[i][s]]['epsilon']:=W[tr[i][s]]['epsilon'] union {tr[i+1][op(B[s][l])]} end if
	 end do
       end do
      end do
     end if;

     W['final']:={seq(seq(tr[i][s],i=1..k+1),s=B['final'])};

     if assigned(B['mark']) then
	for j in map(op,[indices(B['mark'])]) do
     	    W['mark'][j]:={seq(seq(tr[i][s],i=1..k+1),s=B['mark'][j])};
	end do
     end if;

     U:=renameinit(W,'init',tr[1]['init']);

     C:=autodeterminize(U);
     autominimize(C)
end proc:

#################################################
# global numbering of states
#################################################

newstate:=proc() 
    global ns; 
    if not assigned(ns)  then ns:=1
    else                      ns:=ns+1  end if
end proc:

#################################################
# autocheck : coherence test for an automaton
# Input: A - automaton
# Output:
#    NULL if no errors detected
#    ERROR message elsewhere
#################################################

autocheck:=proc(A::table)
   local alpha,i,l,s,t,U;

   if member(`nu`,A['alphabet'])
        then ERROR("reserved letter nu is not allowed to belong to the alphabet",A['alphabet'])
   end if;
   for i in ['alphabet','liststates','final'] do
	autocheckentry(A,i)
   end do;

   if A['liststates']<> {} and not member('init',A['liststates']) then
	ERROR("'init' does not belong to the list of states", A['liststates'])
   end if;
   for s in A['liststates'] do
     if assigned(A[s]['trans']) then
	if not type(A[s]['trans'],set)=true
	   then ERROR("type should be set", evaln(A[s]['trans']),A[s]['trans'])
	end if;
	for l in A[s]['trans'] do
	   if not member(l,A['alphabet']) then
	      ERROR("transition not belonging to the alphabet", l)
	   end if;
           if not assigned(A[s][l]) then
		ERROR("transition from state should be assigned",s,l)
	   end if;
	   if type(A[s][l],set)=false then
		ERROR("should be of type set", evaln(A[s][l]),A[s][l])
	   end if;
	   for t in A[s][l] do
		if not member(t,A['liststates']) then
		    ERROR("state not belonging to the list of states",t):
		end if
	   end do
	end do
     end if
   end do;
   for s in A['final'] do
	if not member(s,A['liststates']) then
	   ERROR("state in 'final' does not belong to list of states", 
                  A['final'], A['liststates'])
        end if
   end do;
   if assigned(A['mark']) then
        for i in map(op,[indices(A['mark'])]) do
	  if type(i,integer)=false
		then ERROR("indices for set of marked states must be integer",i)
	  end if;
	  if type(A['mark'][i],set)=false then
	    ERROR("should be of type set", evaln(A['mark'][i]),A['mark'][i]):
	  end if;
	  for s in A['mark'][i] do
	    if not member(s,A['liststates']) then
	      ERROR("state in mark does not belong to the list of states",
		     evaln(A['mark'][i]),A['mark'][i])
	    end if
	  end do
	end do;
   end if;
	
   NULL
end proc:

autocheckentry:=proc(A::table,entry::name)

   if A[entry]=evaln(A[entry]) then
        ERROR("entry not assigned", entry, A, eval(A))
   end if;
   if type(A[entry],set)=false then
	ERROR("entry should be of type set", entry, A)
   end if
end proc:


#############################################################################
# Generation of random regular expressions in Combstruct specifications
# randregexp
# Input: R - name of the regular expression
#        alphabet
#        n - size of the random expression to generate
#        const - constructors 
#        probaeps - optional, inserts Epsilons in Unions 
#                             with probability probaeps
# Output
#        A grammar for the regular expression in Combstruct specification
#############################################################################

# Constructors
# _Env_UUU       Unions
# _Env_PPP       Products
# _Env_SSS       Sequences
# _Env_NNN       Not

randregexp:=proc(R::name,alphabet::set,n::integer,const::set,probaeps::numeric)
    local GR, RandG, ii, atomsgr, J, preps;
    
    if nargs=5 then
       preps:=convert(probaeps,rational);
       if preps > 1 then error "bad specification for empty word, should be Epsilon %1",e end if
    else    preps:=0;  end if;
    
    if const minus {'Union','Prod','Sequence','Not'} <> {}
       then ERROR("unknown constructor in constructor list", const)
    end if;
    
    if member(R,alphabet) then 
        error "The name of the regular expression %1 is not allowed to belong to the alphabet %2",R,alphabet
    end if;

    J:={};  atomsgr:={};

    if member('Sequence',const)  then 
	J:=J union {Prod(_Env_SSS,R)}; 
	atomsgr:= atomsgr union {_Env_SSS=Atom};
    end if;

    if member('Not',const)  then 
	J:=J union {Prod(_Env_NNN,R)}; 
	atomsgr:= atomsgr union {_Env_NNN=Atom};
    end if;

    if member('Union',const)  then 
	if member('Prod',const)  then
	   J:=J union {Prod(Union(_Env_UUU,_Env_PPP),Sequence(R,card>=2))}; 
	   atomsgr:= atomsgr union {_Env_UUU=Atom, _Env_PPP=Atom} 
	else
	   J:=J union {Prod(_Env_UUU,Sequence(R,card>=2))}; 
	   atomsgr:= atomsgr union {_Env_UUU=Atom}
        end if 
    elif member('Prod',const) then
	   J:=J union {Prod(_Env_PPP,Sequence(R,card>=2))}; 
	   atomsgr:= atomsgr union {_Env_PPP=Atom} 
    end if;

    GR:={R=Union(op(alphabet),op(J)),seq(ii=Atom,ii=alphabet)} union atomsgr;

    RandG:=readlib(`combstruct/draw`)([R,GR],size=n); 

    # {R=pruneunion(randgramtoregexp([alphabet,preps],RandG))} union {seq(ii=Atom,ii=alphabet)}

    {R=randgramtoregexp([alphabet,preps],RandG)} union {seq(ii=Atom,ii=alphabet)}
end proc:

#################################################################
# random grammar to regexp
# Input
#   - [alphabet, Epsilon-boolena]
#   - grammar
# Output
#   - regular expression
#################################################################

randgramtoregexp:=proc(alphaeps::list)
    local i,j, K, die, alphabet, preps; 

    alphabet:=alphaeps[1]; preps:=alphaeps[2];

    if preps>0 then die:=rand(1..denom(preps)) end if;

    if  member(args[2],alphabet) then
          if nargs>2 then ERROR("Atom and more than one argument",args) 
	  else args[2] end if
    elif  op(0,args[2])=Sequence then  seq(randgramtoregexp(alphaeps,i),i=op(args[2]))
    elif  op(0,args[2])=Prod     then  randgramtoregexp(alphaeps,op(args[2]))
    elif  args[2]=_Env_SSS then    	       Sequence(randgramtoregexp(alphaeps,args[3])); 
    elif  args[2]=_Env_NNN then    	       Not(randgramtoregexp(alphaeps,args[3])); 
    elif  args[2]=_Env_PPP then             map2(randgramtoregexp,alphaeps,Prod(args[3]));
    elif  args[2]=_Env_UUU then
	K:=map2(randgramtoregexp,alphaeps,Union(op(args[3]))); 
        if preps>0 then 
            if die()<=numer(preps) then  K:= Union('Epsilon',op(K)) end if;
        end if;
	if nops(K)=1        then op(K)   else K; end if;
    end if
end proc:




###################################################################
# Returns expectation & variance
###################################################################
eqnstoasympexp:=proc(esyst,REGEXP,z,u,n)
local system, listvar, subscom, 
    N, sys, CT, CT_1, CT_2, i, A0, A1, A2, sol, 
    expectation, variance, dsys, indregexp;

    system:={op(esyst)};

    listvar:={seq(op(1,i),i=esyst)};

    subscom:={seq(i=z,i=selectateps(system,z)),op(selectateps(system,u))=u};
    system:=system minus subscom;
    system:=subs(op(subscom),system);
    system:=subs(seq(i=op(0,i),i=listvar),system);
    listvar:=subs(seq(i=op(0,i),i=listvar),listvar);
    system:={seq(op(1,i)-op(2,i),i=system)};
    
    N:=nops(listvar);
    if not member(REGEXP,listvar,'indregexp') then
	error "not in list of indeterminates %1",REGEXP
    end if;
    sys:=[op(system)]; 			    	# A.S+ct=0 (S(1,z)=1/(1-z).1)
    ## 1. compute d(A.S+ct)/du at u=1
    sys:=sys-subs([seq(i=0,i=listvar)],sys);	# ct -> 0
    dsys:=subs(u=1,map(diff,sys,u)):	     	# A'
    CT:=subs([seq(i=-1,i=listvar)],dsys);	# A'.S up to a factor 1/(z-1)
    sys:=subs(u=1,sys);			     	# AS'  at u=1 (unkn = S')
    ## 2. expand AS'+A'S=0 in the neighborhood of z=1 
    # sys.F+(1-z)^2.CT=0 with
    # sys=A0+A1(z-1)+..., F=B0+B1(z-1)+... and A0.(const.1)=0
    sys:=map(series,sys,z=1,3);
    A0:=map(coeff,sys,z-1,0);
    A1:=map(coeff,sys,z-1,1);
    A2:=map(coeff,sys,z-1,2);
    if has(A2,O) then error "not enough terms" end if;
    CT:=map(series,CT,z=1,3);
    #  CT_0=0
    CT_1:=map(coeff,CT,z-1,0);
    CT_2:=map(coeff,CT,z-1,1);
    ## 3. solve
    sol:=mysolvedoit(A0,A1,A2,CT_1,CT_2,listvar,REGEXP);
    expectation:=sol[1][indregexp]*(n+1)-sol[2][indregexp];
    ## 4. d^2/du^2
    # AF''+2A'F'+A''F=0, where A''=0, unkn=F''.
    # sys.F+(1-z)^3.CT=0 with 
    CT:=[seq(sol[1][i]+sol[2][i]*(z-1),i=1..N)]:
    CT:=map(series,subs([seq(listvar[i]=CT[i],i=1..N)],-2*dsys),z=1,3);
    # CT_0=0
    CT_1:=map(coeff,CT,z-1,0);
    CT_2:=map(coeff,CT,z-1,1);
    # 5. solve
    sol:=mysolvedoit(A0,A1,A2,CT_1,CT_2,listvar,REGEXP);
    variance:=sol[1][indregexp]*(n^2/2+3*n/2)-sol[2][indregexp]*n
    -expand(expectation^2)+expectation;
    # give 2 asymptotic terms
    # variance:=variance-coeff(variance,n,0);
    [expectation,variance]
end proc:

mysolvedoit:=proc(A0,A1,A2,CT_1,CT_2,listvar,REGEXP)
    local B0, B1, i, c0, c1, sys, sol;
    # A0.B0=0 --> B0 = c0.1
    B0:=[seq(i=c0,i=listvar)];
    # A0.B1+A1.B0+CT_1=0
    sys:=A0+subs(B0,A1)+CT_1;
    # COMPUTE WITH RATIONAL
    # sol:=readlib(`solve/linear`)([REGEXP,op(evalf(sys))],listvar union {c0});
    # sol:=readlib(`solve/linear`)([REGEXP,op(sys)],listvar union {c0});
    sol:=SolveTools:-Linear([REGEXP,op(sys)],listvar union {c0});
    B0:=subs(c0=subs(sol,c0),B0);
    B1:=subs(sol,[seq(i+c1,i=listvar)]);# c1 to be determined
    B1:=[seq(listvar[i]=B1[i],i=1..nops(listvar))];
    # A0.B2+A1.B1+A2.B0+CT_2=0
    sys:=A0+subs(B1,A1)+subs(B0,A2)+CT_2;
    
    
    # COMPUTE WITH RATIONAL
    # sol:=`solve/linear`([REGEXP,op(evalf(sys))],listvar union {c1});
    # sol:=`solve/linear`([REGEXP,op(sys)],listvar union {c1});
    sol:=SolveTools:-Linear([REGEXP,op(sys)],listvar union {c1});
    [map2(op,2,B0),map2(op,2,subs(sol,B1))]
end proc:

selectateps:=proc(eqns,v) 
    local ii,res; 
    res:={}; 
    for ii in eqns do if op(2,ii)=v then res := res union {op(1,ii)} end if end do;
    res
end proc:



#savelib(regexpcount,'`regexpcount/version`',countl,erasenu,BL,BW,BuildGlushkov,EraseProd,EraseUnion,ExpandR,GenMarkLet,Gfirst,Gfirstlast,Glast,MarkLetters,alphaclosure,atomtoauto,autocarprod,autocheck,autocheckentry,autocomplete,autoconcat,autocopy,autodeterminize,autoepssuppress,autoerror,autofastconcat,autofastunion,automarkov,autominimize,autonot,autoprune,autorenewal,autorenumber,autoreverse,autoseq,autotogram,autounion,bersettoauto,checkprod,drawword,epsclosure,epsclosureset,epsilontoauto,eqnstoasympexp,follow,getmarks,gramerror,grammarkov,gramtoauto,gramweight,loadtrans,markgram,minirenumber,mysolvedoit,newstate,nullable,pruneunion,randgramtoregexp,randregexp,recregexpnottoauto,regexpnottoauto,regexpstowaitgram,regexptoauto,regexptogram,regexptomatchesgram,renameinit,selectateps,sortbyindets,starnormalform,statesplit,autotoXstarauto);
