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

# DocumentGenerator - a Maple library for generating documents
# Author: Ludovic Meunier
# Copyright (C) 2001-2007 Inria.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Library General Public License for more details.
# 
# You should have received a copy of the GNU Library General Public
# License along with this library; if not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA  02111-1307, USA.
# 
# Ludovic Meunier
# Project Algorithmes, INRIA Rocquencourt
# Domaine de Voluceau-Rocquencourt - B.P. 105
# 78153 Le Chesnay Cedex France
# Tel: (33) 1 39 63 52 44
# Fax: (33) 1 39 63 55 96 
# Mail: ludovic.meunier@inria.fr
# 
# 
############################################################
### Package level description
############################################################
### Introduction: the package 'DocumentGenerator' provides
# the framework for building up documents with mathematical
# content, for translating them into various formats and
# for managing a collection of such documents that make
# cross-references to each other.
# A document is represented by a tree with names nodes; this
# is the DOC format. This format is a Maple expression (it
# can thus can be manipulated as any other expression) and it
# expresses the logical architecture of the document,
# independently of any output format. The mathematical content
# is expressed by the relative positioning of the operands
# of the mathematical expressions (in particular, the semantic
# is lost). In other words, the DOC format is a LaTeX clone.
# The package 'DocumentGenerator' is meant to be used with
# translators. A translator is implemented as a module and
# it provides ONLY the format-specific implementation for the
# translation of the keywords and some other format-specific
# data. Indeed, the design of having 'DocumentGenerator' plus
# translators aims to split the code between what is format
# dependent (translator) and what is not ('DocumentGenerator').
# As a matter of fact, most of the code resides in 'DocumentGenerator'.
# The package 'DocumentGenerator' does the following things:
# - conversion of Maple expressions to DOC format (see
# math.exprtomath.mpl and math.*.mpl),
# - syntax checking for the DOC format (see doc.checkdoc.mpl),
# - constructors for the DOC format (see doc.constructor.mpl),
# - support for cross references and automatic numbering
# (see numbering.mpl, resolveref.mpl),
# - support for translators (see doc.translate.mpl),
# - export to files (see output.mpl),
# - mechanism for customization (see math.Define.mpl).
############################################################
### Logical document: a document is understood as the top-level
# logical (semantic) entity. A logical document is represented
# by &document and it may contain a header and a body. The
# header is designed to contain meta information on the document.
# This includes the title of the document and relations with
# other documents. Such a "semantic" header is represented by
# &header. On the other hand, a body (&body) is merely a place
# holder. A document is structured as a sequence of nested
# sections (&section) that contain sequences of paragraphs.
# A paragraph (&paragraph) may contain objects, such as plain
# text and math mode. A section is labelled, while a paragraph
# is not.
# A physical file is represented by &file and it is defined
# by a file descriptor (see file.mpl). A physical file may also
# have a header (&fileheader) and a body (&filebody). In practice,
# a logical document is associated to a physical file (see
# 'Document').
# Labels and file descriptors are now optional and they are
# supplied automatically.
############################################################
### How does it work? A document is expressed in terms of
# the keywords that define the DOC format (see below). Pieces
# of documents are provided by constructors. A document is
# translated in 2 passes. The first pass consists in computing
# the numbering, making entries to the table of references
# and slighty TRANSFORMING the original DOC. This transformation
# merely consists in passing "global" pieces of information 
# (such as the answer of: what is "my" enclosing file?, where
# "my" is the keyword being recursed over) to the keywords.
# Keywords of the transformed DOC are declared the same way
# as the DOC keywords are (doc.GRAMMAR.mpl), which means that
# both DOC and transformed DOC can be syntax-checked.
# Elements of the DOC format are lowercase and prefixed by "&";
# the transformed elements are uppercase (and also prefixed
# by "&"). A translator MUST implement lowercase keywords that
# don't have an uppercase version, and the uppercase keywords.
# The second pass is the translation itself, with calls to
# the translator. The entire process of translation is then:
# DOC -> resolveref -> translate -> printoutput
# (see doc.GRAMMAR.mpl, resolveref.mpl, doc.translate.mpl and
# output.mpl respectively).
############################################################
###			DOC FORMAT
############################################################
### Implementation: the DOC format has been split into a
# text mode, a math mode and switches to math mode. The 2
# latter modes are discussed in math.GRAMMAR.mpl, while the
# text mode is presented in doc.GRAMMAR.mpl.
############################################################
###			Translator
############################################################
# As said above, a translator must implement all lowercase
# keywords that don't have an uppercase version, and the
# all uppercase keywords.
############################################################
###		Style of a document
############################################################
### TO BE UPDATED
# The style can be defined in several ways:
# 1) writing constructors for the MAD format that include
# pieces of raw code (to be enclosed in &only),
# 2) adding pieces of raw code in the header (&header, &rawheader
# and &localrawheader),
# 3) overriding the translators by providing a custom
# implementation of entries of the table 'TranslateText'.
# The style is carried by the top-level constructors, which
# are defined by 'DocumentGenerator' or redefined in MAD.mpl.
############################################################
############################################################
### Decription: Maple preprocessor macros
### Conventions: $define _<NAME>_<NAME>
############################################################

### Defaults
$define _DEF_OUTPUT	'stdout'

### Standard error messages
$define _ERR_NARGS	error "wrong number of arguments"
$define _ERR_BADARGS	error "invalid arguments"
$define _ERR_BADVAL	error "invalid argument value"
$define _ERR_NOTIMP	error "not yet implemented"
$define _ERR_TYPE(x,t)	if not type(x,t) then error "%1: invalid type [::%2]",x,t; fi;

### Copyright
$define _COPYRIGHT	`Copyright (c) 2001-2007 INRIA. All rights reserved.`
$define _OPTION		option _COPYRIGHT

#<<<###########################################
### @exit
### Generated: 04/16/2007 19:33:33
### Module name: DocumentGenerator
#>>>###########################################

#<<<########################################################
# @macro
$define _REF_TBL	`DocumentGenerator/RefTable`

$define _LBL_EXIST(x)		assigned(_REF_TBL[x])
$define _LBL_TRANSLATORS(x)	_REF_TBL[x]
$define _LBLTR_EXIST(x,t)	assigned(_REF_TBL[x,t])
$define _LBLTR_FILEDESC(x,t)	_REF_TBL[x,t][1]
$define _LBLTR_TAG(x,t)		_REF_TBL[x,t][2]
$define _LBLTR_HASTOCENTRY(x,t)	evalb(nops(_REF_TBL[x,t][3])>0)
$define _LBLTR_TOCENTRY(x,t)	_REF_TBL[x,t][3]	# list [type,numbering,entry] OR []
$define _TOCENT_OPTYPE		1
$define _TOCENT_OPNUMB		2
$define _TOCENT_OPENTRY		3
#>>>########################################################
#<<<########################################################
# @macro
$define _FILEDESC_THIS	`file/filestack`[-1]
#>>>########################################################
#<<<########################################################
# @macro
# list of arguments over which recursive
$define _KEYW_RECARGS(x)	[op(dockeyword[op(0,x)][2],x)]
# list of parameters
$define _KEYW_PARAMS(x)		[op(1..dockeyword[op(0,x)][1],x)]
#>>>########################################################
#<<<########################################################
# @macro
# current numbering
$define _NUMBERING_THIS	`numbering/stack`[-1]
$define _NUMBERING_DEFAULT	[[],[]]
#>>>########################################################
### BEGIN MODULE ##############################
DocumentGenerator := module()
	option `Copyright (c) 2001-2007 INRIA. All rights reserved.`,package, load=moduleinit;
### BEGIN DECLARATION #########################
local
	checkdoc,
	checkmath,
	cleanlist,
	declaredockeyword,
	declarekeyword,
	declaremathkeyword,
	declaremathswitch,
	dockeyword,
	exprtomath,
	inmathmode,
	intlistorder,
	labeltotag,
	mathalias,
	mathextrakeyword,
	mathextratype,
	mathkeyword,
	mathswitch,
	mathtype,
	moduleinit,
	newlabel,
	newtocitem,
	norecursiveoutput,
	printoutput,
	resolveref,
	tableinsert,
	thetranslator,
	translate,
	typetomath,
	uniqueid,
	`RawTableOfContents/doit`,
	`checkmath/doit`,
	`crossref/declareditems`,
	`crossref/lastdeclaredlabel`,
	`crossref/resolveref`,
	`crossref/resolveref/init`,
	`crossref/translate`,
	`dockeyword/check`,
	`exprtomath/doit`,
	`file/address`,
	`file/filestack`,
	`file/localrawheader`,
	`file/rawheader`,
	`file/resolveref`,
	`file/resolveref/init`,
	`file/translate/init`,
	`filearch/basename`,
	`math/Int`,
	`math/Sum`,
	`math/complex`,
	`math/diff`,
	`math/diff/format`,
	`math/exp`,
	`math/exponent`,
	`math/flatprod`,
	`math/indexed`,
	`math/indexed/format`,
	`math/quotient`,
	`math/quotient/numerdenom`,
	`math/series`,
	`math/sum`,
	`mathkeyword/check`,
	`mathswitch/check`,
	`mathtype/function`,
	`numbering/down`,
	`numbering/lastinit`,
	`numbering/next`,
	`numbering/popstack`,
	`numbering/pushstack`,
	`numbering/resolveref`,
	`numbering/resolveref/init`,
	`numbering/stack`,
	`numbering/up`,
	`option/nosqrt`,
	`option/solvealgnum`,
	`resolveref/doit`,
	`style/relativestypeinclude`,
	`style/toctypeinclude`,
	`toc/back`,
	`toc/chapter`,
	`toc/down`,
	`toc/list`,
	`toc/lock`,
	`toc/make`,
	`toc/next`,
	`toc/sortpredicate`,
	`toc/table`,
	`toc/translate/close`,
	`toc/translate/init`,
	`toc/up`,
	`translate/doit`;
export
	Chapter,
	DeclareTextKeyword,
	DefineMathAlias,
	DefineMathKeyword,
	DefineMathType,
	DocStyle,
	Document,
	Equation,
	Export,
	FILEARCH,
	HasLeadingMinus,
	InlineMath,
	LabelToTag,
	Math,
	MathStyle,
	NeedParensAsExponent,
	NeedParensAsProduct,
	Nth,
	Paragraph,
	Plot,
	RawTableOfContents,
	Ref,
	RefInfo,
	RelativeRef,
	RemoveLeadingMinus,
	ResolveRef,
	SaveRef,
	Section,
	SingPlur,
	SyntaxCheck,
	TableOfContents,
	UndefineMathAlias,
	UndefineMathKeyword,
	UndefineMathType;
global
	`DocumentGenerator/RefTable`,
	`type/DocumentGenerator/Atomic`,
	`type/DocumentGenerator/EquationSeparator`,
	`type/DocumentGenerator/FileDescriptor`,
	`type/DocumentGenerator/Int`,
	`type/DocumentGenerator/Label`,
	`type/DocumentGenerator/Numbering`,
	`type/DocumentGenerator/Sum`,
	`type/DocumentGenerator/Translator`,
	`type/DocumentGenerator/complex`,
	`type/DocumentGenerator/diff`,
	`type/DocumentGenerator/exp`,
	`type/DocumentGenerator/flatprod`,
	`type/DocumentGenerator/quotient`;
### END DECLARATION ###########################

### BEGIN FUNCTIONS ###########################
#<<<########################################################
# @scope=export
### Description: syntax checker
### Argument sequence:
#	x::DOC
### Output format::true (error may be raised).
############################################################
SyntaxCheck := proc(x)
    _OPTION;
    checkdoc(x);
    true;
end;	# SyntaxCheck
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::anything
#	recproc::procedure
### Description: look up table in 'mathtype' and 'mathextratype'
# that converts an typed Maple object into the DOC format.
# The second argument is the recursive procedure.
############################################################
typetomath := proc(x,recproc)
    local i;
    # implemented types
    for i in map(op,[indices(mathtype)]) do
	if type(x,i) then
	    return recproc(mathtype[i](x));#::DOC or Maple
	fi;
    od;
    # user types
    for i in map(op,[indices(mathextratype)]) do
	if type(x,i) then
	    return recproc(mathextratype[i](x));#::DOC or Maple
	fi;
    od;
    # function
    if type(x,'function') then
	recproc(`mathtype/function`(x));
    else
	error "%1: invalid for math mode",x;
    fi;
end;	# typetomath
#>>>########################################################
#<<<########################################################
# @scope=global
############################################################
`type/DocumentGenerator/Sum` := proc(x)
    member(op(0,x),{'sum','Sum'});
end;	# `type/DocumentGenerator/Sum`
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	x::anything
### Output format::boolean
### Description: test wheter the expression needs parens when
# it is into `^`.
############################################################
NeedParensAsExponent := proc(x)
    _OPTION;
    if nops(x) > 1 and 
	not type(x,'fraction') and 
	not type(x,'function') then true;
    else HasLeadingMinus(x); fi;
end:	# NeedParensAsExponent
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: code for 'translate', which is executed when
# references are resolved.
### Output format::{MAD|false}
############################################################
`crossref/translate` := proc(x)
    local str,fd;

    if op(0,x) = `&DECLAREITEM` then
	# the type-specific entry is first converted to string...
	str := `translate/doit`(`&sequence`(op(_KEYW_RECARGS(x))));
	str := [printoutput('string',str)];
	if nops(str)>0 then
	    str := str[1];# only take what not in file
	else
	    str := "";
	fi;
	# ... and then REadded to the toc
	newtocitem(op(1..2,x),[op(3..4,x),str]);
	NULL;
	
    elif member(op(0,x),{`&label->tag`,`&label->fileaddress`}) then
	if not _LBLTR_EXIST(op(1,x),thetranslator) then
	    error "%1: no such label",op(1,x); fi;
	    
	if op(0,x) = `&label->tag` then
	    _LBLTR_TAG(op(1,x),thetranslator);
	else# `&label->fileaddress`
	    fd := _LBLTR_FILEDESC(op(1,x),thetranslator);
	    `file/address`(fd,op(2..3,x),_FILEDESC_THIS);
	fi;
	
    elif member(op(0,x),{`&item->numbering`,`&item->entry`}) then
	if not (_LBLTR_EXIST(op(1,x),thetranslator) and _LBLTR_HASTOCENTRY(op(1,x),thetranslator)) then
	    error "%1: no such label",op(1,x); fi;
	
	if op(0,x) = `&item->numbering` then
	    `translate/doit`(`&NUMBERING`(_LBLTR_TOCENTRY(op(1,x),thetranslator)[_TOCENT_OPNUMB]));
	else# `&tocitem->entry`
	    _LBLTR_TOCENTRY(op(1,x),thetranslator)[_TOCENT_OPENTRY];
	fi;
    
    elif op(0,x) = `&file->address` then
	`file/address`(op(x),_FILEDESC_THIS);
	    
    else# quiet error
	false;
    fi;
end;	# `crossref/translate`
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	depth::{posint,infinity}
#	(optional) `DocumentGenerator/label`
############################################################
TableOfContents := proc(depth,label)
    local i;
    _OPTION;
    _ERR_TYPE(depth,{'posint','infinity'});
    
    if nargs = 1 then# full toc
	`&toc`(depth);
    elif nargs = 2 then# partial toc
	`&subtoc`(depth,label);
    else _ERR_NARGS; fi;
end;	# TableOfContents
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence
#	x::`DocumentGenerator/complex`
############################################################
`math/complex` := proc(x)
    Re(x) + 'imaginaryunit'*Im(x);
end;	# `math/complex`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::`DocumentGenerator/Int`
############################################################
`math/Int` := proc(x)
    local arg,X;
    if not type(op(2,x),`=`) then
	arg := op(2,x);
	X := op(2,x);
    else# op(2,x): name=range
	arg := op([2,2,1],x),op([2,2,2],x);
	X := op([2,1],x);
    fi;
    
    `&mathseq`(`&decoratedsym`('Int',arg),op(1,x),'d',X);

end;	# `math/Int`
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	p::{PLOT,PLOT3D}
#	(optional)::DOC: caption
### Options:
# - 'label'::`DocumentGenerator/Label`
############################################################
Plot := proc(p)
    local optdcl,mylabel,largs;
    _OPTION;
    _ERR_TYPE(p,{'PLOT','PLOT3D'});
    mylabel := 'VOID';
    optdcl := {'label'=[{`DocumentGenerator/Label`,'VOID'},'mylabel']};
    largs := CommonLib:-RemoveYieldOptions([args[2..-1]],optdcl);
    if mylabel = 'VOID' then
	`&plot`(mylabel,p,op(largs));
    else
	`&sequence`(
	    `&declareitem`(mylabel,`&plot`,`&sequence`("Graph ",`&thisnumbering`())),
	    `&plot`(mylabel,p,op(largs)),
	    `&numbering++`());
    fi;
end;	# Plot
#>>>########################################################
#<<<########################################################
### Description: remove redudancies in a SORTED list while
# keeping the order.
cleanlist := proc(l)
    local last,i,j,tbl;
    last := NULL;
    tbl := table();
    j := 0;
    for i to nops(l) do
	if l[i] = last then next;
	else j:=j+1; tbl[j] := l[i]; last := l[i]; fi;
    od;
    [seq(tbl[i],i=1..j)];
end;	# cleanlist
#>>>########################################################
#<<<########################################################
# @scope=global
############################################################
`type/DocumentGenerator/FileDescriptor` := proc(x)
    # the type 'FileDescriptor' is defined in 'CommonLib'
    type(x,'FileDescriptor');
end;	# `type/DocumentGenerator/FileDescriptor`
#>>>########################################################
#<<<########################################################
### Description: this procedure pushes a new numbering into 
# the stack of numberings. Note that a new stack of numbering
# (should) correspond to a new chapter.
### Argument sequence:
#	num::{`DocumentGenerator/Numbering`,VOID}
############################################################
`numbering/pushstack` := proc(num)
    local mynum;
    # initialization
    if num = 'VOID' then
	mynum := `numbering/next`(`numbering/lastinit`,'onlychapter');
    else
	mynum := num;
    fi;
    # push stack
    `numbering/stack` := [op(`numbering/stack`),mynum];
    `numbering/lastinit` := mynum;# update
end;	# `numbering/pushstack`
#>>>########################################################
#<<<########################################################
`numbering/popstack` := proc()
    # the stack is assumed to be initialized
    `numbering/stack` := `numbering/stack`[1..-2];
end;	# `numbering/popstack`
#>>>########################################################
#<<<########################################################
# @scope=global
### Description: a object of type `DocumentGenerator/Numbering`
# represents the numbering of an object. It is a list of 2
# elements. The first element (posint | string | NULL) represents
# the numbering of the current chapter; the second element
# is a list of posint (0 excluded), which represents the rest
# of the numbering; see doc.numbering.mpl
############################################################
`type/DocumentGenerator/Numbering` := proc(x)
    _OPTION;
    type(x,'list') and evalb(nops(x)=2) and type(x[1],
    'list'({'posint','string'})) and evalb(nops(x[1])<=1)
    and type(x[2],'list'('posint'));
end;	# `type/DocumentGenerator/Numbering`
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	x::anything
### Description: remove leading minus (in a consistent way
# with HasLeadingMinus).
############################################################
RemoveLeadingMinus := proc(x)
    local ord;
    _OPTION;
    if HasLeadingMinus(x) then
	if type(x,`*`) then
	    subsop(1=-op(1,x),x);
	elif type(x,'series') then
	    # yield order
	    if has([op(x)],'O') then
		ord := op(-1,x);
	    else ord := 'infinity'; fi;
	    series(-x,op(0,x),ord);
	else
	    -x;
	fi;
    else _ERR_BADVAL; fi;
end;	# RemoveLeadingMinus
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: recursive walk within document; the shared
# variables are initialized in 'resolveref' (see below).
### Argument sequence:
#	x::MAD
### Output format::MAD (see doc.translate.mpl)
############################################################
`resolveref/doit` := proc(x)
    local res,i;
    if nargs <> 1 then _ERR_NARGS; fi;# should never occur

    # recurstion stop
    if type(x,'string') then return x;
    
    ### Conditional walk
    elif op(0,x) = `&only` then
	if member(thetranslator,op(1,x)) then
	    return `&sequence`(seq(procname(i),i=_KEYW_RECARGS(x)));
	else return NULL; fi;
	
    elif op(0,x) = `&but` then
	if member(thetranslator,op(1,x)) then return NULL;
	else
	    return `&sequence`(seq(procname(i),i=_KEYW_RECARGS(x))); fi;
    fi;
    
    ### File: see doc.file.mpl
    res := `file/resolveref`(x);
    if res <> false then return res; fi;
    
    ### Numbering: see doc.numbering.mpl
    res := `numbering/resolveref`(x);
    if res <> false then return res; fi;
  
    ### Cross-references: see doc.cross-ref.mpl
    res := `crossref/resolveref`(x);
    if res <> false then return res; fi;
    
    ### Remaining stuffs
    if assigned(thetranslator:-TextSymbol[x]) then
	# special symbols
	return x;
    elif assigned(dockeyword[op(0,x)]) then
	# other declared keywords: these keywords address directly either
	# 'translate' or the translator; here, one just recurses over the
	# arguments
	return op(0,x)(op(_KEYW_PARAMS(x)),seq(procname(i),i=_KEYW_RECARGS(x)));
    elif assigned(mathswitch[op(0,x)]) then
	# don't recurse over math mode and forward to 'translate'
	return x;
    else
	error "%1: invalid in text mode [resolveref]",x;
    fi;
 
end;	# `resolveref/doit`
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	x::anything
### Output format::boolean
### Description: test whether the expression has a leading
# minus.
############################################################
HasLeadingMinus := proc(x)
    _OPTION;
    # see `latex/isneg`
    (type(x,'numeric') and x < 0) or# ~ realcons
    (type(x,'imaginary') and Im(x) < 0) or# realcons*I
    (type(x,'nonreal') and Re(x) < 0) or# realcons + realcons*I
    (type(x,`+`) and procname(op(1,x))) or
    (type(x,`*`) and procname(op(1,x))) or
    (type(x,'series') and procname(op(1,x)));
end:	# HasLeadingMinus
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: this procedure interfaces the tables of
# the toc and it is meant for being used within the translators
# (lock).
### Argument sequence:
#	l::`DocumentGenerator/Label`
#	x::symbol ('_next','_back','_up','_down')
### Output format::{`DocumentGenerator/Label`,NULL}
############################################################
RelativeRef := proc(l,x)
    local tmp;
    _OPTION;
    if `toc/lock` = true then
	error "this procedure must be used in the translators";
    fi;

    if x = '_next' then
	if not assigned(`toc/next`[l]) or `toc/next`[l] = 'VOID' then
	    NULL;
	else `toc/next`[l]; fi
    elif x = '_back' then
	if not assigned(`toc/back`[l]) or `toc/back`[l] = 'VOID' then
	    NULL;
	else `toc/back`[l]; fi
    elif x = '_up' then
	if not assigned(`toc/up`[l]) or `toc/up`[l] = 'VOID' then
	    NULL;
	else `toc/up`[l]; fi
    elif x = '_down' then
	if not assigned(`toc/down`[l]) or `toc/down`[l] = 'VOID' then
	    NULL;
	else `toc/down`[l]; fi
    else
	error "%1: no such query",x;
    fi;
end;	# RelativeRef
#>>>########################################################
#<<<########################################################
### Description: initialization for 'resolveref'
############################################################
`numbering/resolveref/init` := proc()
    `numbering/stack` := [];# stack of numberings
    # BY DEFAULT, THE STACK HAS ONE ELEMENT, which is
    # initialized for sections (since chapters have different
    # stacks)
    `numbering/pushstack`([[],[1]]);
end;	# `numbering/resolveref/init`
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: opposite of 'DefineMathKeyword'
############################################################
UndefineMathKeyword := proc(s)
    _OPTION;
    mathextrakeyword[s] := evaln(mathextrakeyword[s]);
    NULL;
end;	# UndefineMathKeyword
#>>>########################################################
#<<<########################################################
`crossref/resolveref/init` := proc()
    `crossref/lastdeclaredlabel` := NULL;
    `crossref/declareditems` := [];
end;
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: compute a unique STRING identifier
### Output format::string
############################################################
uniqueid := proc()
    convert(CommonLib:-UniqueId(),'string');
end;	# uniqueid
#>>>########################################################
#<<<########################################################
# @scope=local
### 22.02.03: #? update for `&delimiter`
############################################################
`math/indexed` := proc(x)
    local fmt,a,b;
    fmt := `math/indexed/format`(x);
    # no flat version
    a := `&mathseq`(fmt[1],`&subscript`(
	`&sepsequence`('comma',op(fmt[2]))));
    # flat version
#?    b := `&mathseq`(fmt[1],`&delimiter`(
#?	'leftbracket','rightbracket',`&sepsequence`('comma',op(fmt[2]))));
    b := `&mathseq`(fmt[1],`&delimiter`(
	'leftbracket','rightbracket',false,`&sepsequence`('comma',op(fmt[2]))));
    # output
    `&alternate`(a,b);
end;	# `math/indexed`
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: this function allows to define a keyword
# (with argument) for the math engine.
### Argument sequence:
#	s::symbol
#	p::procedure that returns DOC
############################################################
DefineMathKeyword := proc(s,p)
    _OPTION;
    _ERR_TYPE(s,'symbol');
    _ERR_TYPE(p,'procedure');
    
    mathextrakeyword[s] := p;
    NULL;
end;	# DefineMathKeyword
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	x::anything
### Output format::boolean
### Description: test wheter the expression needs parens when
# it is into `*`.
############################################################
NeedParensAsProduct := proc(x)
    _OPTION;
    type(x,{`+`,'series'}) or
    (type(x,'complex') and Re(x) <> 0 and Im(x) <> 0) or
    HasLeadingMinus(x);
#    or (type(x,`^`) and op(2,x)=-1);
end:	# NeedParensAsProduct
#>>>########################################################
#<<<########################################################
### Description: this procedure makes entry to _REF_TBL
############################################################
newlabel := proc(label)
    ### THIS PROCEDURE MUST BE USED WITHIN 'resolveref'
    tableinsert(_REF_TBL,label,thetranslator);
    _REF_TBL[label,thetranslator] := [
	_FILEDESC_THIS,# see doc.file.mpl
	labeltotag(label),# see utils.mpl
	[]];# empty entry
    `crossref/lastdeclaredlabel` := label;
end;	# newlabel
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence
#	x::exp(...)
### Description: write exp in the superscript form, instead
# of the function form.
### 22.02.03: #? update for `&delimiter`
############################################################
`math/exp` := proc(x)
    local tmp,a,b;
    # parens
    if NeedParensAsProduct(op(x)) then
#?	tmp := `&delimiter`('leftparens','rightparens',op(x));
	tmp := `&delimiter`('leftparens','rightparens',false,op(x));
    else tmp := op(x); fi;
    
    # no flat version
#    a := `&mathseq`('e',`&superscript`(tmp));    
    a := `&mathseq`(`&operatorname`('e'),`&superscript`(tmp));

    # flat version
#?    b := `&mathseq`(`&operatorname`('exp'),`&delimiter`('leftparens',
#?	'rightparens',`&sepsequence`('comma',op(x)),'bindleft'));
    b := `&mathseq`('exp',`&delimiter`('leftparens',
	'rightparens',true,`&sepsequence`('comma',op(x))));
    
    `&alternate`(a,b);
end;	# `math/exp`
#>>>########################################################
#<<<########################################################
### Description: yield address of a file
### Argument sequence:
#	fd::`DocumentGenerator/FileDescriptor`
#	base::{'absolute','relative','VOID',} or string
#	ext::string
#	fdfrom::`DocumentGenerator/FileDescriptor`
############################################################
`file/address` := proc(fd,base,ext,fdfrom)
    local fd2;
    if ext = 'VOID' then# default extension
	fd2 := CommonLib:-FileExtension(fd,thetranslator:-FileExtension);
    else
	fd2 := CommonLib:-FileExtension(fd,ext);
    fi;
    if base = 'absolute' then
	CommonLib:-FileName(fd2);
    elif base = 'relative' then
	CommonLib:-RelativePath(fdfrom,fd2);
    elif base = 'VOID' then
	CommonLib:-FileName(fd2,`filearch/basename`);
    else# string
	CommonLib:-FileName(fd2,base);
    fi;
end;	# `file/address`
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: opposite of 'DefineMathType'
############################################################
UndefineMathType := proc(t)
    _OPTION;
    mathextratype[t] := evaln(mathextratype[t]);
    NULL;
end;	# UndefineMathType
#>>>########################################################
#<<<########################################################
### Description: provide initializations to `translate/doit`
############################################################
translate := proc(x)
    local res;
    `file/translate/init`();# see doc.file.mpl: stack init
    `toc/translate/init`();# see doc.toc.mpl: make the toc, lock
    res := `translate/doit`(x);
    `toc/translate/close`();# see doc.toc.mpl: remove lock
    res;
end;	# translate
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`math/diff` := proc(x)
    local fmt,den,i,a,b;
    fmt := `math/diff/format`(x);
    # denominator
    den := `&mathseq`(seq(`&mathseq`('partial',op(1,i)^op(2,i)),i=fmt[2]));
    # no cut
#    a := `&frac`(`&sepsequence`('thinspace','partial'^fmt[3],
#	`if`(`needparens/prod`(fmt[1]),`&delimiter`('leftparens',
#	'rightparens',fmt[1]),fmt[1])),den);
### spacers for the numerator removed
    a := `&frac`(`&mathseq`('partial'^fmt[3],
#?	`if`(NeedParensAsProduct(fmt[1]),`&delimiter`('leftparens',
#?	'rightparens',fmt[1]),fmt[1])),den);
	`if`(NeedParensAsProduct(fmt[1]),`&delimiter`('leftparens',
	'rightparens',false,fmt[1]),fmt[1])),den);
    # cut version
#?    b := `&mathseq`(`&frac`('partial'^fmt[3],den),
#?	`&delimiter`('leftparens','rightparens',fmt[1]));
    b := `&mathseq`(`&frac`('partial'^fmt[3],den),
	`&delimiter`('leftparens','rightparens',false,fmt[1]));
    # output
    `&alternate`(a,b);
end;	# `math/diff`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: recursive walk for translation. This procedure
# is to be called AFTER 'resolveref'.
### Argument sequence:
#	x::DOC
### Output format::{&file,&string,string}
### History:
# 04/16/03: added &rawheader; 'resolveref' bufferize the
# raw header, which is appened to &header (if present).
# 04/22/03: added &localrawheader, which is local to a file:
# added 'filestack'
# 04/26/03: added &tableofcontents
############################################################
`translate/doit` := proc(x)
    local i,res,oldopt;
    if nargs <> 1 then _ERR_NARGS; fi;
    
    ### Cross-reference: see doc.cross-ref.mpl
    res := `crossref/translate`(x);
    if res <> false then return res; fi;

    ### Recursion stop
    if type(x,'string') then x;
    
    ### Text symbols
    elif assigned(thetranslator:-TextSymbol[x]) then
	thetranslator:-TextSymbol[x];

    ### Built-in keywords
    elif member(op(0,x),{`&sequence`,`&string`}) then
	`&string`(seq(procname(i),i=[op(x)]));
	
    elif op(0,x) = `&setoption` then
	# op(1,x)::procedure
	# op(2,x)::list('equation')
	# old options: [opt=val]
	oldopt := [seq(op([2,i,1],x)=eval(op(1,x)(op([2,i,1],x))),i=1..nops(op(2,x)))];
	# set options
	seq(eval(op(1,x)(op([2,i],x))),i=1..nops(op(2,x)));
	# recurse
	res := `&string`(seq(procname(i),i=_KEYW_RECARGS(x)));
	# reset options
	seq(eval(op(1,x)(i)),i=oldopt);
	res;# output
	
    # &header, &rawheader, &localrawheader
    elif op(0,x) = `&header` then
	### SEE doc.file.mpl
	# add `file/rawheader` and the content of `file/localrawheader`
	# to the argument sequence of `&header`
	procname(`&HEADER`(# the output MUST be {&string,&file,string}
	    # &header has NO argument to forward
	    # add stuffs from 'resolveref'
	    procname(`file/rawheader`),# &rawheader
	    `if`(assigned(`file/localrawheader`[_FILEDESC_THIS]),# &localrawheader
		procname(`file/localrawheader`[_FILEDESC_THIS]),NULL)
	    ));
	
    elif op(0,x) = `&FILE` then
	`file/filestack` := [op(`file/filestack`),op(1,x)];
	# this is the &file for output.mpl
	res := `&file`(op(1,x),seq(procname(i),i=_KEYW_RECARGS(x)));
	`file/filestack` := `file/filestack`[1..-2];
	res;
	
    ### Math mode
    elif assigned(mathswitch[op(0,x)]) then
	# macro expansion for math mode: 'exprtomath'
	thetranslator:-TranslateMath(
	    # mathswitch[op(0,x)][1]: number of parameters
	    # mathswitch[op(0,x)][2]: range of recursive arguments
	    op(0,x)(
		op(1..mathswitch[op(0,x)][1],x),
		seq(exprtomath(i),i=[op(mathswitch[op(0,x)][2],x)])));
				
    ### Forward to translator
    elif assigned(dockeyword[op(0,x)]) then
	thetranslator:-TranslateText[op(0,x)](op(0,x)(
	    op(_KEYW_PARAMS(x)),seq(procname(i),i=_KEYW_RECARGS(x))));
    else
	error "%1: invalid in text mode [translate]",x;
    fi;
	    
end;	# translate
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: declaration of switches to math mode
############################################################
declaremathswitch := proc()
    declarekeyword(mathswitch,`mathswitch/check`,args);
end;	# declaremathswitch
#>>>########################################################
#<<<########################################################
# @scope=export
############################################################
Paragraph := proc()
    _OPTION;
    `&paragraph`(args);
end;	# Paragraph
#>>>########################################################
#<<<########################################################
# @scope=export
### Options:
# - 'label'::`DocumentGenerator/Label`
# - 'displaynumbering'::boolean
############################################################
Section := proc(title)
    local mylabel,dspnum,optdcl,largs;
    _OPTION;
    mylabel := 'VOID';
    dspnum := true;
    optdcl := {'label'=[{`DocumentGenerator/Label`,'VOID'},'mylabel'],
	'displaynumbering'=['boolean','dspnum']};
    largs := CommonLib:-RemoveYieldOptions([args[2..-1]],optdcl);
    
    if mylabel = 'VOID' then mylabel := [uniqueid()]; fi;
    ### MAD
    `&sequence`(
	`&declareitem`(mylabel,`&section`,title),
	`&section`(mylabel,dspnum,title,`&subnumbering`(op(largs)))
    );
end;	# Section
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: this procedure inserts the entry 'ent' in
# the set of that is associated to the index 'ind' in the
# table 'tbl'.
############################################################
tableinsert := proc(tbl,ind,ent)
    if assigned(tbl[ind]) then
	tbl[ind] := tbl[ind] union {ent};
    else
	tbl[ind] := {ent};
    fi;
end;	# tableinsert
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: this function allows to define a new type
# for the math mode.
### Argument sequence:
#	t::type
#	p::procedure that returns DOC
############################################################
DefineMathType := proc(t,p)
    _OPTION;
    _ERR_TYPE(t,'type');
    _ERR_TYPE(p,'procedure');

    mathextratype[t] := p;
    NULL;
end;	# DefineMathType
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: syntax checker for math mode (including both
# the nestable math mode and the switches to math mode).
### Argument sequence:
#	x::DOC
### Output format::NULL (error may be raised)
############################################################
checkmath := proc(x)
    inmathmode := false;
    `checkmath/doit`(x);
    NULL;
end;	# checkmath
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: top-level constructor for a file.
### Argument sequence::MAD
### Options:
# - 'file'::{`DocumentGenerator/FileDescriptor`,'VOID'}
# - 'setoption'::list (see below)
# - 'header'::list (see below)
### History:
# - 04/26/03: added 'setoption', which is a list of procedures,
# each of them being followed by its argument sequences
# in the form option::symbol=value::anything. For instance:
# setoption=[HTMX:-MathStyle,width=20,...,MADLaTeX:-DocStyle,...].
# These options apply to the current document; local options
# are to be set locally within the document with &setoption.
# - 04/27/03: added 'header', which is a list of lists of
# translators, each of these being followed by a sequence
# of MAD format, which is meant to go into the header.
############################################################
Document := proc()
    local mad,optdcl,largs,myfiledesc,
    mysetoption,myrawheader,i,tmp;
    _OPTION;
    
    # default values for options
    myfiledesc := 'VOID';
    mysetoption := 'VOID';
    myrawheader := 'VOID';
    
    # declaration of options
    optdcl := {
	'file'=[{`DocumentGenerator/FileDescriptor`,'VOID'},'myfiledesc'],
	'setoption'=[{'VOID','list'},'mysetoption'],
	'header'=[{'VOID','list'},'myrawheader']
	};
	
    # yield options
    largs := CommonLib:-RemoveYieldOptions([args],optdcl);
    
    # file
    if myfiledesc = 'VOID' then
	# automatic name provided here, instead of doc.file.mpl
	myfiledesc := [uniqueid()];
    fi;

    # rawheader (can be put anywhere within the document)
    if myrawheader <> 'VOID' then
	tmp := [CommonLib:-SplitList('list'(`DocumentGenerator/Translator`),myrawheader)];
	myrawheader := `&sequence`(
	    seq(`&localrawheader`(i[1],op(i[2..-1])),i=tmp));
    else
	myrawheader := NULL;
    fi;
    
    ### MAD
    # &file(filedesc,Seq(DOC))
    mad := `&file`(myfiledesc,
	`&fileheader`(),
	`&filebody`(
	    myrawheader,# can be put anywhere
	    `&header`(),
	    `&body`(op(largs))));

    # setting options
    if mysetoption <> 'VOID' then
	# mysetoption: [procedure,`=`,`=`,...,procedure,`=`,`=`,...]
	# remove empty list of options
	tmp := select(X->nops(X)>1,[CommonLib:-SplitList('procedure',mysetoption)]);
	for i in tmp do
	    mad := `&setoption`(
		i[1],# procedure
		i[2..-1],# list of option=value
		mad);
	od;
    fi;
    mad;
end;	# Document
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: top level function for converting a Maple
# expression into 2D math mode DOC format.
### Options:
# - 'label'::`DocumentGenerator/Label`
# - 'separator'::`DocumentGenerator/EquationSeparator`
### Argument sequence::Seq(DOC) + options
############################################################
Equation := proc()
    local llabel,largs,lsep,i;
    _OPTION;
    llabel := 'VOID';
    lsep := [`&nothing`,`&nothing`];
    largs := CommonLib:-RemoveYieldOptions([args],
	{'label'=[`DocumentGenerator/Label`,'llabel'],
	'separator'=[`DocumentGenerator/EquationSeparator`,'lsep']});
	
    if llabel = 'VOID' then
	`&equation`(llabel,lsep,seq(exprtomath(i),i=largs));
    else
	`&sequence`(
	    `&declareitem`(llabel,`&equation`,`&sequence`("Equation ",`&thisnumbering`())),
	    `&equation`(llabel,lsep,seq(exprtomath(i),i=largs)),
	    `&numbering++`());
    fi;
end;	# Equation
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: compute a tag from a label
### Argument sequence:
#	label::`DocumentGenerator/Label`
### Output format::string
############################################################
labeltotag := proc(label)
    cat(CommonLib:-SepSeq(":",op(map(CommonLib:-AlphaNumeric,label))));
end;	# labeltotag
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: declaration of nestable math keywords
############################################################
declaremathkeyword := proc()
    declarekeyword(mathkeyword,`mathkeyword/check`,args);
end;	# declaremathkeyword
#>>>########################################################
#<<<########################################################
# @scope=global
############################################################
`type/DocumentGenerator/quotient` := proc(x)
    type(x,`*`) and not type(x,`DocumentGenerator/flatprod`);
end:	# `type/DocumentGenerator/quotient`
#>>>########################################################
#<<<########################################################
# @scope=local
### Output format: sequence
### Description: split numer and denom into 2 objects of
# type `DocumentGenerator/flatproduct`.
############################################################
`math/quotient/numerdenom` := proc(x)
    local n,d,l,i,N,D;
    # leading minus treated apart
    if op(1,x) = -1 then l := [op(2..-1,x)];
    else l := [op(x)]; fi;
    d,n := selectremove(X->type(X,`^`) and HasLeadingMinus(op(2,X)),l);
    d := mul(op(1,i)^(-op(2,i)),i=[op(d)]);
    n := convert(n,`*`);
    
    N := numer(n) * denom(d);
    D := numer(d) * denom(n);
    
    N,D;
end:	# `math/quotient/numerdenom`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: initialization of the numbering; this procedure
# MUST be called BEFORE any other procedure of this file
# (see `resolveref/doit`, in resolveref.mpl)
############################################################
`file/resolveref/init` := proc()
    `file/filestack` := [];
    `file/rawheader` := `&string`();
    `file/localrawheader` := table();
end;	# `file/resolveref/init`
#>>>########################################################
#<<<########################################################
# @scope=global
############################################################
`type/DocumentGenerator/exp` := proc(x)
    evalb(op(0,x)='exp');
end:	# `type/DocumentGenerator/exp`
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`math/quotient` := proc(x)
    local n,d,lead,a,b;
    if op(1,x) = -1 then
	lead := 'leadingminus';
    else
	lead := NULL;
    fi;

    n,d := `math/quotient/numerdenom`(x);
    # \frac version
    a := `&sepsequence`('`&nothing`',lead,`&frac`(n,d));
    # flat version
    if NeedParensAsProduct(n) or type(d,`*`) then
#?	n := `&delimiter`('leftparens','rightparens',n); fi;
	n := `&delimiter`('leftparens','rightparens',false,n); fi;
    if NeedParensAsProduct(d) or type(d,`*`) then
#?	d := `&delimiter`('leftparens','rightparens',d); fi;
	d := `&delimiter`('leftparens','rightparens',false,d); fi;	
    b := `&sepsequence`('`&nothing`',lead,n,`&sizedsep`('divideop',n,d),d);
    # output
    `&alternate`(a,b);
end;	# `math/quotient`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence
#	x::series
### History:
# 02/22/03: #? update for `&delimiter`
# 04/28/03: #! fixed bug for negative exponents
############################################################
`math/series` := proc(x)
    local i,z,iend,bigo,tbl,c,zp,myop;
    z := op(0,x);
    if has(x,'O') then
	iend := nops(x)/2 -1;
	bigo := `&mathseq`('plusop',`&operatorname`('O')(z^op(-1,x)));
#	bigo := `&mathseq`('plusop','O'(z^op(-1,x)));
    else
	iend := nops(x)/2;
	bigo := NULL;
    fi;
    for i to iend do
	c := normal(op(2*i-1,x));
	zp := op(2*i,x);

	# operator
	if i = 1 then
	    myop := NULL;
	else
	    if HasLeadingMinus(c) then
		myop := 'minusop';
#		c := -c;
		c := RemoveLeadingMinus(c);
	    else
		myop := 'plusop';
	    fi;
	fi;

	# parens for coefficient
	if zp <> 0 and NeedParensAsProduct(c) = true then
	    c := `&delimiter`('leftparens','rightparens',false,c);
	fi;

	# assemble
	if zp = 0 then
	    tbl[i] := [myop,c];
	else
	    if c = 1 then
		tbl[i] := [myop,z^zp];
	    else
		tbl[i] := [myop,c,'timesop',z^zp];
	    fi;
	fi;
    od;
    `&mathseq`(seq(op(tbl[i]),i=1..iend),bigo);
end;	# `math/series`
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: top level function for converting a Maple
# expression into inline math mode DOC format.
### Argument sequence::Seq(DOC)
############################################################
InlineMath := proc()
    local i;
    _OPTION;
    `&imath`(seq(exprtomath(i),i=args));
end;	# InlineMath
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence
#	x::`+`
############################################################
`math/sum` := proc(x)
    local tbl,i;
    tbl[1] := [op(1,x)];
    for i from 2 to nops(x) do
	if HasLeadingMinus(op(i,x)) then
#	    tbl[i] := ['minusop',-op(i,x)];
	    tbl[i] := ['minusop',RemoveLeadingMinus(op(i,x))];
	else
	    tbl[i] := ['plusop',op(i,x)];
	fi;
    od;
    `&mathseq`(seq(op(tbl[i]),i=1..nops(x)));
end;	# `math/sum`
#>>>########################################################
#<<<########################################################
`toc/translate/close` := proc()
    `toc/lock` := true;
end;	# `toc/translate/close`
#>>>########################################################
#<<<########################################################
# @scope=global
### Note: only math mode (see math.exprtomath.mpl)
############################################################
`type/DocumentGenerator/Atomic` := proc(x)
    type(x,{'integer','symbol','float','fraction'});
end;	# `type/DocumentGenerator/Atomic`
#>>>########################################################
#<<<########################################################
# @scope=export
### Options:
# - 'label'::`DocumentGenerator/Label`
# - 'numbering'::`DocumentGenerator/Numbering`
# - 'displaynumbering'::boolean
############################################################
Chapter := proc(title)
    local mylabel,mynumb,dspnum,optdcl,largs;
    _OPTION;
    mylabel := 'VOID';
    mynumb := 'VOID';
    dspnum := true;
    
    optdcl := {'label'=[{`DocumentGenerator/Label`,'VOID'},'mylabel'],
	'numbering'=[{`DocumentGenerator/Numbering`,'VOID'},'mynumb'],
	'displaynumbering'=['boolean','dspnum']
	};
    largs := CommonLib:-RemoveYieldOptions([args[2..-1]],optdcl);
    
    if mylabel = 'VOID' then mylabel := [uniqueid()]; fi;
    ### MAD
    `&newnumbering`(mynumb,
	`&declareitem`(mylabel,`&chapter`,title),
	`&chapter`(mylabel,dspnum,title,`&subnumbering`(op(largs))));
end;	# Chapter
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: opposite of 'DefineMathAlias'
############################################################
UndefineMathAlias := proc(t)
    _OPTION;
    mathalias[t] := evaln(mathalias[t]);
    NULL;
end;	# UndefineMathAlias
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: get label info, meant for being used from
# translator. This procedure actually interfaces the 
# table _REF_TBL.
### Argument sequence:
#	l::`DocumentGenerator/Label`
#	(optional) t::`DocumentGenerator/Translator`
#	x::symbol
############################################################
RefInfo := proc(l,t,x)
    local tmp;
    _OPTION;
    if nargs = 0 then# list all labels
	map2(op,1,{indices(_REF_TBL)});
    elif nargs = 1 then
	# return the set of translators
	if _LBL_EXIST(l) then
	    _LBL_TRANSLATORS(l);
	else {}; fi;
    elif nargs = 2 then# return possible queries
	if not _LBLTR_EXIST(l,t) then
	    error "%1: no such label for the translator %2",l,t; fi;
	    
	if _LBLTR_HASTOCENTRY(l,t) then# numbering
	    {'tag','filedesc','numbering','entry','type'};
	else# label
	    {'tag','filedesc'};
	fi;
    else
	if not _LBLTR_EXIST(l,t) then
	    error "%1: no such label for the translator %2",l,t; fi;

	if x = 'tag' then
	    _LBLTR_TAG(l,t);
	elif x = 'filedesc' then
	    _LBLTR_FILEDESC(l,t);
	elif member(x,{'numbering','entry','type'}) then
	    tmp := _LBLTR_TOCENTRY(l,t);
	    if nops(tmp) = 0 then
		error "%1: no such item for the translator %2",l,t;
	    elif x = 'numbering' then
		tmp[_TOCENT_OPNUMB];
	    elif x = 'type' then
		tmp[_TOCENT_OPTYPE];
	    else# x = 'entry'
		tmp[_TOCENT_OPENTRY];
	    fi;
	fi;
    fi;
end;	# RefInfo
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: top-level procedure. It inputs a document
# in the DOC format and outputs the translated document. The
# output is printed as specified by the option 'output'. This
# option can be set to {'string','file','stdout'}: this is
# the physical medium of the output of the translator. The
# option 'syntax'=true (resp. false) enables (resp. disables)
# the syntax checking. The option 'format'=set(symbol) specifies
# the other formats that are derived from the translator's; if
# this option is set, then the output is always done to file.
# These other formats are generated from the output of the
# translator by OS-specific commands.
### Argument sequence
#	d::DOC
#	t::`DocumentGenerator/Translator`
############################################################
Export := proc(d,t)
    local out,fmt,synchk,optdcl,i,j,fdl,d2,fdl_name,res;
    _OPTION;
    _ERR_TYPE(t,`DocumentGenerator/Translator`);
    thetranslator := t;
        
    # default values for options
    out := _DEF_OUTPUT;
    fmt := NULL;
    synchk := true;
    fdl_name := NULL;
    
    # process options
    optdcl := {'output'=[proc(x) member(x,{'string','file','stdout'}); end,'out'],
	'format'=[{'symbol',{'set','list'}('symbol')},'fmt'],
	'syntaxcheck'=['boolean','synchk'],
	'filelist'=['symbol','fdl_name']
	 };
    CommonLib:-RemoveYieldOptions([args],optdcl);
    
    ### Translation
    if synchk then
	checkdoc(d); fi;# syntax checking
    d2 := resolveref(d);# resolve reference
    
    if fmt <> NULL then
	if type(fmt,'symbol') then
	    fmt := [fmt]; fi;
	# output to file, whatever 'out' could be
	printoutput('file',translate(d2),fdl);# translation + ouput
	if fdl_name <> NULL then
	    assign(fdl_name,fdl);
	fi;
	# post-processing
	for i in fmt do
#	    if not pacman:-pmember('DerivedFormat',thetranslator) then
	    if not member('DerivedFormat',thetranslator) then
		error "no format available for this translator"; fi;
	    if not assigned(thetranslator:-DerivedFormat[i]) then
		error "%1: format not available",i; fi;
	    seq(CommonLib:-FileCommand(j,thetranslator:-DerivedFormat[i]),j=fdl);
	od;
	NULL;
    else
	res := printoutput(out,translate(d2),fdl);# translation + output
	if fdl_name <> NULL then
	    assign(fdl_name,fdl);
	fi;
	res;
    fi;
end;	# Export
#>>>########################################################
#<<<########################################################
### Description: this procedure makes entry to _REF_TBL
### Argument sequence:
#	ent: entry
############################################################
newtocitem := proc(label,filedesc,ent)
    tableinsert(_REF_TBL,label,thetranslator);
    _REF_TBL[label,thetranslator] := [
	filedesc,
	labeltotag(label),
	ent];# ent::[type,numb,str]
    `crossref/lastdeclaredlabel` := label;
end;	# newtocitem
#>>>########################################################
#<<<########################################################
`toc/translate/init` := proc()
    `toc/make`();
    `toc/lock` := false;
end;	# `toc/translate/init`
#>>>########################################################
#<<<########################################################
### Description: provides initializations for `resolveref/doit`
### Argument sequence:
#	x::DOC
############################################################
resolveref := proc(x)
    local res;
    `file/resolveref/init`();# see doc.file.mpl
    `numbering/resolveref/init`();# see doc.numbering.mpl
    `crossref/resolveref/init`();# see doc.cross-ref.mpl

    res := `resolveref/doit`(x);
    # Add the declaration of item at the BEGINNING of the
    # document, such that &toc can be invoked from anywhere.
    `&sequence`(op(`crossref/declareditems`),res);# see doc.cross-ref.mpl
end;	# resolveref
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	rectbl::table[symbol]::[integer,range]
#	chktbl::table[symbol]::procedure
#	keyw::symbol
#	pty::list{type,procedure}
#	(optional) nrecargs::integer
### Description: declaration of a keyword. The declaration of
# a keyword merely consists in specifying its argument sequence.
# By convention, a keyword has a FIXED number of parameters
# that are the first of its argument sequence; the remaining
# arguments are (recursive) grammar statements. This declaration
# mechanism was introduced to clarify the grammar that was
# being implemented and it is also used to produce automatically
# code for the syntax checker.
# The argument sequence of a keyword is specified by:
# - pty::list(type): (ordered) list of the types of the parameters
# of the keywords; the recursion loop won't recurse on them.
# - (optional) nrecargs::integer: number of recursive arguments;
# if nrecargs=NULL, then the recursion loop recurses over
# the remaining of the argument sequence of the keyword; if
# nrecargs>=0, then it recurses on exactly nrecargs; if
# nrecargs<0, then it recurses on at least nrecargs. These
# latter subtleties are used to produce a better syntax checker.
# The procedure 'declarekeyword' makes entries to the tables
# 'rectbl' and 'chktbl'. The table 'rectbl' associates a keyword
# to the number of its (non recursive) parameters and to the
# range in its argument sequence of its recursive arguments.
# This is used in the recursion loops to know where to recurse
# when a keyword is encountered. The table 'chktbl' associates
# a keyword to a procedure that performs the syntax checking.
# This procedure is automatically generated, outputs NULL
# and raises error when a syntax error is detected.
############################################################
declarekeyword := proc(rectbl,chktbl,keyw,pty)
    local i,badnargs,npty,
    # in generated procedure
    x;

    npty := nops(pty);
    
    # where to recurse::[integer,range]
    if nargs > 4 and args[5] >= 0 then
	rectbl[keyw] := [npty,npty+1..npty+args[5]];
	badnargs := `&function`(nops,`&expseq`(x)) <> npty + args[5];
    else
	rectbl[keyw] := [npty,npty+1..-1];
	badnargs := `&function`(nops,`&expseq`(x)) < 
	    npty + `if`(nargs>4,-args[5],0);
    fi;
    
    # syntax checker::NULL
    chktbl[keyw] := procmake(`&proc`([x],[],[],`&statseq`(
	`&if`(badnargs,`&ERROR`(cat(keyw,": wrong number of arguments"))),
	seq(
	    `if`(type(pty[i],'type'),
		`&if`(
		    not `&function`(type,`&expseq`(`&function`(op,`&expseq`(i,x)),pty[i])),
		    `&ERROR`(cat("",keyw,": args[",i,"]: invalid type [::",convert(pty[i],'string'),"]"))
		),
		`&if`(not `&function`(pty[i],`&expseq`(`&function`(op,`&expseq`(i,x)))),
		    `&ERROR`(cat("",keyw,": args[",i,"]: invalid value"))
		)
	    ),i=1..nops(pty))
	)));
end;	# declarekeyword
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: conversion to nestable math mode (without
# switch to math mode)
### Argument sequence:
#	x::DOC
############################################################
Math := proc(x)
    _OPTION;
    exprtomath(x);
end;	# Math
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::`DocumentGenerator/Sum`
############################################################
`math/Sum` := proc(x)
    local arg;
    if not type(op(2,x),`=`) then
	arg := op(2,x);
    else
	if type(op([2,2],x),'range') then
	    arg := op([2,1],x)=op([2,2,1],x),op([2,2,2],x);
	else arg := op(2,x); fi;
    fi;
    
    `&mathseq`(`&decoratedsym`('Sum',arg),op(1,x));

end;	# `math/Sum`
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	x::DOC
#	t::`DocumentGenerator/Translator`
### Output format::NULL
### Description: user interface. This procedure may be used
# by itself for updating the table of references from existing
# documents, without translating these documents.
# NOTE: 'ResolveRef' is to be invoked on the MAD format, which
# imposes that ALL labels and file descriptors have an actual
# value (instead of 'VOID').
############################################################
ResolveRef := proc(x,t)
    _OPTION;
    _ERR_TYPE(t,`DocumentGenerator/Translator`);
    thetranslator := t;
    checkdoc(x);# needed ?
    resolveref(x);
    NULL;# no output, since no exported procedure would input it
end;	# ResolveRef
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: recursion loop for converting a Maple expression
# into the (nestable) math mode format. This is where the
# aliases and extra math keywords are expanded.
# Note that this loop used to be parameterized by a set of
# procedures such that it was used by both 'exprtomath'
# and 'checkmath'.
############################################################
`exprtomath/doit` := proc(x)
    local i;
    _OPTION;
    if nargs <> 1 then _ERR_NARGS;# should never occur
    else# one argument
	### Dynamic expansion of aliases (::symbol) and extra
	# keywords. Aliases MUST be sought BEFORE the atomic
	# types, which include the type symbol
	if assigned(mathalias[x]) then
	    procname(mathalias[x]);#::DOC or Maple
	elif assigned(mathextrakeyword[op(0,x)]) then
	    procname(mathextrakeyword[op(0,x)](x));#::DOC or Maple
	### Recursion stop
	elif type(x,`DocumentGenerator/Atomic`) then
	    x;
	### Loop over keywords
	elif assigned(mathkeyword[op(0,x)]) then
	    ### Note: 'x' is a keyword here. The arguments of 'x'
	    # could be assumed to be already in the DOC format.
	    # This assumption implies that the procedures that
	    # implements the types must make explicit calls to
	    # this recursive procedure, which makes the implementation
	    # heavy. Thus, this loop recurses where it has been
	    # declared to, throught the declaration mechanism
	    # (see declarekeyword.mpl).
	
	    # mathkeyword[op(0,x)][1]: number of parameters
	    # mathkeyword[op(0,x)][2]: range of recursive arguments
	    op(0,x)(
		op(1..mathkeyword[op(0,x)][1],x),
		seq(procname(i),i=[op(mathkeyword[op(0,x)][2],x)]));
	### NO LOOP OVER SWITCHES TO MATH MODE
	### Conversion from Maple types to DOC format
	else
	    typetomath(x,procname);# see math.type.mpl
	fi;
    fi;
end;	# `exprtomath/doit`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: code for 'resolveref'
### Output format::{MAD|false}
############################################################
`crossref/resolveref` := proc(x)
    local i,ent;
    if op(0,x) = `&declarelabel` then
	newlabel(op(1,x));
	NULL;
	
    elif op(0,x) = `&declareitem` then
	# op(1..2,x): label,symbol (type of the item)
	# The keyword is DECLARED here, such that all
	# references are resolved when entering 'translate',
	# with a junk entry. The actual entry is overwritten
	# in 'translate' (see `crossref/translate`).
	ent := [op(2,x),_NUMBERING_THIS,""];
	newtocitem(op(1,x),_FILEDESC_THIS,ent);

	# Note: one can't translate the entry now (it is too
	# late), since a toc may have been invoked. Thus,
	# all declared items are in the list
	# `crossref/declareditems` and its content is added
	# on the top of the document at the end of 'resolveref'.
	`crossref/declareditems` := [op(`crossref/declareditems`),
	`&DECLAREITEM`(op(1,x),# label
	    _FILEDESC_THIS,
	    # the following is the entry
	    op(2,x),_NUMBERING_THIS,
	    seq(`resolveref/doit`(i),i=_KEYW_RECARGS(x)))];
	NULL;
	    
    elif op(0,x) = `&ref` then
	`&REF`(_FILEDESC_THIS,op(_KEYW_PARAMS(x)));
	
    elif op(0,x) = `&link` then
	`&LINK`(_FILEDESC_THIS,op(_KEYW_PARAMS(x)),seq(
	    `resolveref/doit`(i),i=_KEYW_RECARGS(x)));
	    
    elif op(0,x) = `&plot` then
	`&PLOT`(_FILEDESC_THIS,op(_KEYW_PARAMS(x)),seq(
	    `resolveref/doit`(i),i=_KEYW_RECARGS(x)));
	    
    elif op(0,x) = `&toc` then
	`&TOC`(_FILEDESC_THIS,op(_KEYW_PARAMS(x)));
    elif op(0,x) = `&subtoc` then
	`&SUBTOC`(_FILEDESC_THIS,op(_KEYW_PARAMS(x)));

    else# quiet error
	false;
    fi;
end;	# `crossref/resolveref`
#>>>########################################################
#<<<########################################################
# @scope=global
############################################################
`type/DocumentGenerator/flatprod` := proc(x)
    local hasnegexp,hasfrac;
    if type(x,`*`) then
	# negative exponent
	hasnegexp := evalb(
	    map(HasLeadingMinus,map2(op,2,select(type,{op(x)},`^`))) 
	    intersect {true} = {true} );

	# fraction
	hasfrac := evalb(
	    map(type,{op(x)},'fraction') intersect {true} = 
	    {true} );
	not evalb(hasfrac or hasnegexp);
    else false; fi;
end:	# `type/DocumentGenerator/flatprod`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: recursion loop for checking the syntax of
# both text and math modes. The output of this procedure is
# not significant; error may be raised. Note that the code
# for the syntax checker (`dockeyword/check` and `mathkeyword/check`)
# has been produced automatically from 'declarekeyword'.
### Argument sequence:
#	x::DOC
############################################################
checkdoc := proc(x)
    local i;
    if nargs <> 1 then _ERR_NARGS;# should never occur
    else# one argument
	# 'symbol' has been added for 'TextSymbol'
	if type(x,{'symbol','string'}) then NULL;
	elif assigned(dockeyword[op(0,x)]) then
    	    # dockeyword[op(0,x)][1]: number of parameters
	    # dockeyword[op(0,x)][2]: range of recursive arguments
	    `dockeyword/check`[op(0,x)](x);
	    seq(procname(i),i=_KEYW_RECARGS(x));
	elif assigned(mathswitch[op(0,x)]) then
	    # math mode
	    checkmath(x);
	else
	    error "%1: invalid in text mode [checkdoc]",x;
	fi;
    fi;
end;	# checkdoc
#>>>########################################################
#<<<########################################################
# @scope=export
# @sharevar=`filearch/basename`
############################################################
FILEARCH := proc()
    local opt,val;
    _OPTION;
    
    if nargs = 0 then {'rootdir','basename'};
    elif nargs = 1 then
	if type(args[1],'equation') then
	    opt,val := op(args[1]);
	elif type(args[1],'symbol') then
	    opt := args[1]; val := NULL;
	else _ERR_BADARGS; fi;
	
	if opt = 'rootdir' then
	    if val = NULL then CommonLib:-FileOptions('rootdir');
	    else CommonLib:-FileOptions('rootdir'=val); fi;
	    
	elif opt = 'basename' then
	    if val = NULL then `filearch/basename`;
	    elif type(val,'string') then `filearch/basename` := val; NULL;
	    else _ERR_BADARGS; fi;
	    
	else _ERR_BADARGS; fi;
    else _ERR_NARGS; fi;
end;	# FILEARCH
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: go one step upper into the nesting
############################################################
`numbering/up` := proc()
    local c,n;
    c,n := op(_NUMBERING_THIS);
    # this procedure is meant to be called AFTER `numbering/down`,
    # which ensures that nops(n)>0
    `numbering/stack` := subsop(-1=[c,n[1..-2]],`numbering/stack`);
end;	# `numbering/up`
#>>>########################################################
#<<<########################################################
# @scope=local
# @load
############################################################
moduleinit := proc()
    `filearch/basename` := CommonLib:-FileOptions('rootdir');
end;	# moduleinit
#>>>########################################################
#<<<########################################################
### description: the toc is computed AFTER 'resolveref' has
# been invoked. This procedure initializes ALL toc data
# structures.
############################################################
`toc/make` := proc()
    local i,tbl,N,chap,cont,num,upstk,up,j,lbl,tmp,diff;
    # init
    `toc/list` := [];# numberings
    `toc/table` := table();# numbering => label
    `toc/next` := table();# label => label
    `toc/back` := table();# label => label
    `toc/up` := table();# label => label
    `toc/down` := table();# label => label
    `toc/chapter` := [];# labels
    
    if type(_REF_TBL,'table') then# _REF_TBL may not be initialized
	# loop over all labels of the current translator
	tbl := table();# for 'entries' below
	for i in [indices(_REF_TBL)] do
	    if nops(i) = 2 and i[2] = thetranslator and _LBLTR_HASTOCENTRY(i[1],thetranslator) then
		# yield numbering
		tbl[i] := _LBLTR_TOCENTRY(i[1],thetranslator)[_TOCENT_OPNUMB];
		`toc/table`[tbl[i]] := i[1];# numbering => label
	    else next; fi;
	od;
	# sorted list of ALL numberings
	`toc/list` := sort(map(op,[entries(tbl)]),`toc/sortpredicate`);
	N := nops(`toc/list`);
	
	if N > 0 and `toc/list`[1][1] = [] then
	    # This case occurs when there is no enclosing chapter.
	    # A "virtual" lalel is thus added. Note that this virtual
	    # label MUST be 'VOID', such that cross-references
	    # are not messed up.
	    _REF_TBL['VOID',thetranslator] := [0,0,[0,[[],[]],""]];# ONLY NUMBERING NEEDED
	    `toc/table`[[[],[]]] := 'VOID';
	    `toc/list` := [[[],[]],op(`toc/list`)];
	    N := N + 1;
	fi;

	### Logical relations between labels.

	chap := [];# ORDERED list of labels of chapters
	cont := table();# uplabel => content (list of enclosed labels)
	up := table();# `toc/up` only for sections
	
	for i to N do

	    num := `toc/list`[i];# this numbering
	    lbl := `toc/table`[num];# this label
	
	    # entering new numbering (which is characterized by a new chapter numbering)?
	    if nops(chap) = 0 or
	    # compare the chapter parts of the numberings
	    _LBLTR_TOCENTRY(chap[-1],thetranslator)[_TOCENT_OPNUMB][1] <> num[1] then
		chap := [op(chap),lbl];
		`toc/up`[lbl] := 'VOID';# chapters have no "up"
		upstk := [lbl];# stack of "up" labels	
	    else
		`toc/up`[lbl] := upstk[-1];
		
		### Below is built the table 'cont'. 'cont'
		# associates to a "up" label the list of the
		# labels that are enclosed.
		up[lbl] := upstk[-1];
		# "up" label serves here as index
		if assigned(cont[upstk[-1]]) then
		    cont[upstk[-1]] := [op(cont[upstk[-1]]),lbl];
		else cont[upstk[-1]] := [lbl]; fi;
		
		# update stack for next execution of the loop

		if i <> N and # not a new chapter
		_LBLTR_TOCENTRY(chap[-1],thetranslator)[_TOCENT_OPNUMB][1] = `toc/list`[i+1][1] then
		    if nops(num[2]) < nops(`toc/list`[i+1][2]) then
			upstk := [op(upstk),lbl];
		    elif nops(num[2]) > nops(`toc/list`[i+1][2]) then
			# go up in nesting: maybe several step at once.
			diff := nops(num[2]) - nops(`toc/list`[i+1][2]);
			upstk := upstk[1..-(1+diff)];
		    fi;

		fi;
	    fi;

	    # `toc/down`

	    if i < N and nops(`toc/list`[i+1][2]) > nops(num[2]) then
		`toc/down`[lbl] := `toc/table`[`toc/list`[i+1]];
	    else
		`toc/down`[lbl] := 'VOID';
	    fi;
	od;

	### At this point, `toc/down` and `toc/up` are built.
	# `toc/next` and `toc/back` remains to be done.
	# Chapter
	`toc/chapter` := chap;
	chap := ['VOID',op(chap),'VOID'];# guards
	for i from 2 to nops(chap)-1 do
	    `toc/back`[chap[i]] := chap[i-1];
	    `toc/next`[chap[i]] := chap[i+1];
	od;

	# Section
	for i in {indices(up)} minus {'VOID'} do# i[1]::label
	    tmp := cont[up[i[1]]];# list of labels
	    N := nops(tmp);
	    for j to N do
		if i[1] = tmp[j] then break; fi;
	    od;
	    tmp := ['VOID',op(tmp),'VOID'];# guards
	    `toc/back`[i[1]] := tmp[j];
	    `toc/next`[i[1]] := tmp[j+2];
	od;
    fi;
end;	# `toc/make`
#>>>########################################################
#<<<########################################################
# @scope=global
############################################################
`type/DocumentGenerator/complex` := proc(x)
    type(x,'complex') and type(x,'nonreal');
end;	# `type/DocumentGenerator/complex`
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	n::nonnegint
### Output format::string
### Description: returns first, second , ...
############################################################
Nth := proc(n)
    local str,l1,l2,ext;
    _OPTION;
    _ERR_TYPE(n,'nonnegint');
    
    if n = 0 then "zeroth";
    elif n = 1 then "first";
    elif n = 2 then "second";
    elif n = 3 then "third";
    elif n = 4 then "fourth";
    elif n = 5 then "fifth";
    else
	str := convert(n,'string');
	l1 := str[-1];
	l2 := `if`(length(str)>1,str[-2..-1],str[-1]);
	if l1 = "1" and l2 <> "11" then ext := "st";
	elif l1 = "2" and l2 <> "12" then ext := "nd";
	elif l1 = "3" and l2 <> "13" then ext := "rd";
	else ext := "th";
	fi;
	cat(str,ext);
    fi;
end:	# Nth
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: this function allows to define an alias
# for the math mode (for convenience only). Note that
# the alias is not checked.
### Argument sequence:
#	s::symbol
#	d::DOC
############################################################
DefineMathAlias := proc(s,d)
    _OPTION;
    _ERR_TYPE(s,'symbol');
    
    mathalias[s] := d;
    NULL;
end;	# DefineMathType
#>>>########################################################
#<<<########################################################
`toc/sortpredicate` := proc(a,b)
    # chapter part of the numbering
    if nops(a[1]) = nops(b[1]) then
	if a[1] <> b[1] then
	    if type(a[1][1],'numeric') and type(b[1][1],'numeric') then evalb(a[1][1]<b[1][1]);
	    elif type(a[1][1],'numeric') and type(b[1][1],'string') then true;
	    elif type(a[1][1],'string') and type(b[1][1],'numeric') then false;
	    else lexorder(a[1][1],b[1][1]); fi;
	else# section part of the numbering
	    intlistorder(a[2],b[2]);# see utils.mpl
	fi;
    else
	evalb(nops(a[1])<nops(b[1]));
    fi;
end;	# `toc/sortpredicate`
#>>>########################################################
#<<<########################################################
# @scope=local
# @sharevar=inmathmode
### Description: recursion loop for checking the syntax of
# the math mode. This loop also performs dynamic expansion
# of aliases and conversion to DOC math mode (as 'exprtomath').
### Output format::NULL (error may be raised)
### Argument sequence:
#	x::DOC
############################################################
`checkmath/doit` := proc(x)
    local i;
    if nargs <> 1 then _ERR_NARGS;# should never occur
    else# one argument
	### Dynamic expansion of aliases (::symbol) and extra
	# keywords. Aliases MUST be sought BEFORE the atomic
	# types, which include the type symbol
	if assigned(mathalias[x]) then
	    procname(mathalias[x]);#::DOC or Maple
	elif assigned(mathextrakeyword[op(0,x)]) then
	    procname(mathextrakeyword[op(0,x)](x));#::DOC or Maple
	### Recursion stop
	elif type(x,`DocumentGenerator/Atomic`) then NULL;
	### Loop over keywords
	elif assigned(mathkeyword[op(0,x)]) then
	    # mathkeyword[op(0,x)][1]: number of parameters
	    # mathkeyword[op(0,x)][2]: range of recursive arguments
	    `mathkeyword/check`[op(0,x)](x);
	    seq(procname(i),i=[op(mathkeyword[op(0,x)][2],x)]);
	### Loop over switches
	elif assigned(mathswitch[op(0,x)]) then
	    # mathswitch[op(0,x)][2]: range of recursive arguments
	    if inmathmode then
		error "%1: math mode does not nest",op(0,x); fi;
	    inmathmode := true;
	    `mathswitch/check`[op(0,x)](x);
	    seq(procname(i),i=[op(mathswitch[op(0,x)][2],x)]);
	    inmathmode := false;
	### Conversion from Maple types to DOC format
	else
	    typetomath(x,procname);
	fi;
    fi;
end;	# `checkmath/doit`
#>>>########################################################
#<<<########################################################
### Description: conversion of the type 'function'. This type
# has been removed from the table 'mathtype', since it MUST
# be called AFTER other function-like types.
############################################################
`mathtype/function` := proc(x)
    `&mathseq`(op(0,x),`&delimiter`('leftparens',
	'rightparens',true,`&sepsequence`('comma',op(x))));
end;	# `mathtype/function`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: go one step deeper into the nesting
############################################################
`numbering/down` := proc()
    local c,n;
    c,n := op(_NUMBERING_THIS);
    # This only concerns the sections, since nesting of
    # chapter is considered as sequencing.
    `numbering/stack` := subsop(-1=[c,[op(n),1]],`numbering/stack`);
end;	# `numbering/down`
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	(optional) x::string
### Description: save the tables of references into library
# that is specified by 'x'. If the procedure is called without
# argument, then the tables are saved into 'savelibname'.
############################################################
SaveRef := proc(x)
    global savelibname;
    local oldlib;
    _OPTION;
    if nargs <> 0 then
	oldlib := savelibname;
	savelibname := x;
    elif not assigned(savelibname) then
	error "no library specified";
    fi;
    # Maple's preprocessor expands macros that are
    # in single quotes (this might be a bug?!).
    savelib('_REF_TBL');
    savelibname := oldlib;
    NULL;
end;	# SaveRef
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::diff/Diff
### Output format::list
#	expr: expression being differentiated
#	[[var,order],[var,order]]: variables/order
#	order: total order
### 22.02.03: #? update for `&delimiter`
############################################################
`math/diff/format` := proc(x)
    local r,v,tbl,tord,i;
    r := x;
    tord := 0;
    while member(op(0,r),{'diff','Diff'}) do
	# list of variables
	if type(op(2,r),'list') then
	    v := op(2,r);
	else
	    v := [op(2..-1,r)];
	fi;
	tord := tord + nops(v);
	for i in v do
	    if assigned(tbl[i]) then tbl[i] := tbl[i] + 1;
	    else tbl[i] := 1; fi;
	od;
	r := op(1,r);
    od;
    # output
    [r,[seq([i,tbl[i]],i=map(op,[indices(tbl)]))],tord];
end:	# `math/diff/format`
#>>>########################################################
#<<<########################################################
# @scope=global
############################################################
`type/DocumentGenerator/Translator` := proc(x)
    # 'DerivedFormat' is an optional member
    type(x,`module`('FileExtension','TextSymbol',
	'TranslateText','TranslateMath'));
end;	# `type/DocumentGenerator/Translator`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: the output of a translator is a nested
# structure of &file and &string "over" strings. This procedure
# prints such a structure as a string or in files. The list
# of the file descriptors is assigned to the optional argument.
### Argument sequence:
#	out::{'file','string','stdout'}
#	x::{&file,&string,string}
#	(optional) fdl::name
############################################################
printoutput := proc(out,x,fdl)
    local tbl,i,j,notinfile;

    # "anti-recurse"; entries of 'tbl' are Seq(string)
    # 'notinfile' yields everything that's not enclosed in a &file
    tbl := table();# for 'indices'
    notinfile := norecursiveoutput(tbl,x);
    if nargs > 2 then
	# yield the list of `DocumentGenerator/FileDescriptor`
	fdl := map(op,[indices(tbl)]);
    fi;
    
    # printed output
    if out = 'string' then
	cat(notinfile),seq(cat(tbl[op(i)]),i=[indices(tbl)]);
    else# 'stdout' or 'file'
	for i in [indices(tbl)] do
	    if out = 'stdout' then
		seq(printf(j),j=tbl[op(i)]);
	    else# 'file'
		# 'FileNew' is used to have WRITE, instead of APPEND
		CommonLib:-FileNew(op(i));
		# 'FilePrint' creates and opens the file if needed
		seq(CommonLib:-FilePrint(op(i),j),j=tbl[op(i)]);
		CommonLib:-FileClose(op(i));# only one file opened at a time
	    fi;
	od;
	# this goes to 'stdout' (since no file is specified)
	seq(printf(j),j=notinfile);
	NULL;
    fi;
end;	# printoutput
#>>>########################################################
#<<<########################################################
# @scope=global
############################################################
`type/DocumentGenerator/diff` := proc(x)
    member(op(0,x),{'diff','Diff'});
end;	# `type/DocumentGenerator/diff`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: increase the numbering
############################################################
`numbering/next` := proc(num)
    local c,n;
    c,n := op(num);
    
    if nops(n) = 0 or (nargs>1 and args[2] = 'onlychapter') then
	c := op(c);
	if c = NULL then c := 1;
	elif type(c,'posint') then c := c + 1;
	else# string
	    c := cat("Ch",uniqueid());# what else to do??
	fi;
	[[c],[]];
    else# section
	[c,[op(1..-2,n),n[-1]+1]];
    fi;
end;	# `numbering/next`
#>>>########################################################
#<<<########################################################
# @scope=export
############################################################
DeclareTextKeyword := proc(keyw,params,nrecargs)
    _OPTION;
    _ERR_TYPE(keyw,'symbol');
    _ERR_TYPE(params,'list'({'procedure','type'}));
    if nargs > 2 then
	_ERR_TYPE(nrecargs,'integer');
    fi;
    declaredockeyword(args);
    NULL;
end;	# DeclareTextKeyword
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: set and read the options for the math mode.
############################################################
MathStyle := proc()
    local optdcl,i,largs;
    _OPTION;
    optdcl := {
	'nosqrt'=['boolean','`option/nosqrt`'],
	'solvealgnum'=['boolean','`option/solvealgnum`']
	};
	
    if nargs = 0 then# list options
	map2(op,1,optdcl);
    elif nargs = 1 and not type(args[1],`=`) then
	for i in optdcl do
	    if op(1,i) = args[1] then
		return eval(op([2,2],i));
	    fi;
	od;
	error "%1: invalid option",args[1];
    else
	largs := CommonLib:-RemoveYieldOptions([args],optdcl);
	if nops(largs) = 0 then NULL;
	else error "%1: invalid arguments",largs; fi;
    fi;
end;	# MathStyle
#>>>########################################################
#<<<########################################################
# @scope=global
############################################################
`type/DocumentGenerator/Int` := proc(x)
    member(op(0,x),{'int','Int'});
end;	# `type/DocumentGenerator/Int`
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	val::posint
#	a::string
#	(optional) b::string
### Description: singular or plural, according to 'val'. 'b'
# is the plural form; if it is omitted, then the plural is
# gotten by appending "s" to 'a'.
############################################################
SingPlur := proc(val,a,b)
    _OPTION;
    _ERR_TYPE(val,'posint');
    _ERR_TYPE(a,'string');
    
    if val = 1 then a;
    elif nargs = 2 then cat(a,"s");
    else
	_ERR_TYPE(b,'string');
	b;
    fi;
end:	# SingPlur
#>>>########################################################
#<<<########################################################
# @scope=global
### Note: only math mode
############################################################
`type/DocumentGenerator/EquationSeparator` := proc(x)
    type(x,list('symbol')) and nops(x) = 2;
end;	# `type/DocumentGenerator/EquationSeparator`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: initialization for 'translate'
############################################################
`file/translate/init` := proc()
    `file/filestack` := [];
    ### DO NOT INITIALIZE `file/rawheader` NOR `file/localrawheader`
    ### SINCE THEY ARE USED BY `translate/doit`
end;	# `file/translate/init`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: declaration of text mode keywords. ANY NEW
# KEYWORD MUST BE DECLARED BY THIS FUNCTION, which initializes
# the tables and generates the code for the syntax checker.
############################################################
declaredockeyword := proc()
    declarekeyword(dockeyword,`dockeyword/check`,args);
end;	# declaredockeyword
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: code for 'resolveref'
### Output format::{MAD|false}
############################################################
`file/resolveref` := proc(x)
    local fd,res,i;
    
    if op(0,x) = `&file` then
	# if missing, the file descriptor is provided.
	
	### IMPORTANT NOTE: VOID used to be a valid file
	# descriptor, in which case an automatic name
	# was produced here. However, by doing this,
	# sucessive calls of 'resolveref' on the SAME
	# MAD document gave different names, which was
	# not convenient for making references across
	# translators.
	
#	# arguments
#	if op(1,x) = 'VOID' then
#	    # automatic file descriptor
#	    if nops(`file/filestack`) = 0 then
#		fd := [uniqueid()];
#	    else# nested document goes in a sub-directory
#		fd := [op(_FILEDESC_THIS),uniqueid()];
#	    fi;
#	else fd := op(1,x); fi;
	fd := op(1,x);
	# push stack
	`file/filestack` := [op(`file/filestack`),fd];# stack of file descriptors

	# recurse
	res := `&FILE`(fd,seq(`resolveref/doit`(i),i=_KEYW_RECARGS(x)));
	# pop stack
	`file/filestack` := `file/filestack`[1..-2];
	# output
	res;

    elif op(0,x) = `&rawheader` then
	# this header will be duplicated in EVERY files: usefull?
	if member(thetranslator,op(1,x)) then
	    # bufferize
	    `file/rawheader` := `&string`(`file/rawheader`,seq(`resolveref/doit`(i),i=_KEYW_RECARGS(x)));
	fi;
	NULL;

    elif op(0,x) = `&thisfile` then
	# op(1,x): base::{string,'absolute','VOID'}
	# op(2,x): extension:{VOID,string}
	`file/address`(_FILEDESC_THIS,op(1..2,x),_FILEDESC_THIS);

    elif op(0,x) = `&localrawheader` then
	# this header that is local to a &file
	if member(thetranslator,op(1,x)) then
	    # bufferize per file
	    `file/localrawheader`[_FILEDESC_THIS] := `&string`(
		`if`(assigned(`file/localrawheader`[_FILEDESC_THIS]),
		    `file/localrawheader`[_FILEDESC_THIS],NULL),
		    seq(`resolveref/doit`(i),i=_KEYW_RECARGS(x)));
	fi;
	NULL;

    else# quiet error
	false;
    fi;
    
end;	# `file/resolveref`
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	label::`DocumentGenerator/Label`
#	(optional) MADseq
############################################################
Ref := proc(label)
    _OPTION;
    _ERR_TYPE(label,`DocumentGenerator/Label`);
    if nargs = 1 then
	`&ref`(label);
    else
	`&link`(args);
    fi;
end;	# Ref
#>>>########################################################
#<<<########################################################
# @scope=global
############################################################
`type/DocumentGenerator/Label` := proc(x)
    # `DocumentGenerator/Label` and `DocumentGenerator/FileDescriptor`
    # are actually the same type (list of anything).
    type(x,'FileDescriptor');
end;	# `type/DocumentGenerator/Label`
#>>>########################################################
#<<<########################################################
### Description: compute partial toc from a label
### Output format::list(`DocumentGenerator/Label`)
############################################################
`RawTableOfContents/doit` := proc(maxdepth,label,depth,donext)
    local i,j,lbl,nxt,dwn,tbl;
    if depth > maxdepth then return []; fi;
    
    lbl := label;
    for i do
	dwn := `toc/down`[lbl];
	if dwn <> 'VOID' then
	    tbl[i] := [lbl,op(procname(maxdepth,dwn,depth+1,true))];
	else tbl[i] := [lbl]; fi;

	if donext then
	    nxt := `toc/next`[lbl];
	    if nxt <> 'VOID' and 
		nxt <> lbl # HACK 
		then
		lbl := nxt;
	    else
		break;
	    fi;
	else break; fi;
    od;
    [seq(op(tbl[j]),j=1..i)];
end;	# `RawTableOfContents/doit`
#>>>########################################################
#<<<########################################################
### Description: upper level procedure
############################################################
exprtomath := proc(x)
    local tmp,s,i,tbl,res;
    _OPTION;
    
    if `option/solvealgnum` and type('iRootOf','package') then
	# Solve indexed RootOf's.
	s := select(type,indets(x,'RootOf'),'iRootOf');
	for i in s do
	    tmp := iRootOf:-Solve(i);# features remember option
	    if tmp = FAIL then tbl[i] := i;
	    else tbl[i] := tmp; fi;
	od;
	res := subs([seq(i=tbl[i],i=s)],x);
    else
	res := x;
    fi;
    
    `exprtomath/doit`(res);
end;	# exprtomath
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`math/exponent` := proc(x)
    local a,b,op1,op2;
    if op(2,x) = -1 then
	# the SYMBOL `1` is used
	`1`/op(1,x);
    else
	# parens
 	if NeedParensAsProduct(op(1,x)) or type(op(1,x),`DocumentGenerator/quotient`) then
	    op1 := `&delimiter`('leftparens','rightparens',false,op(1,x));
	else op1 := op(1,x); fi;
	if NeedParensAsExponent(op(2,x)) then
	    op2 := `&delimiter`('leftparens','rightparens',false,op(2,x));
	else op2 := op(2,x); fi;

	# no flat version
	if type(op(2,x),'fraction') and numer(op(2,x)) = 1
	and not `option/nosqrt` then
	    a := `&sqrt`(denom(op(2,x)),op(1,x));
	else
	    a := `&sepsequence`('`&nothing`',op1,`&superscript`(op2));
	fi;
	# flat version
	b := `&sepsequence`('`&nothing`',op1,'exponentop',op2);
	`&alternate`(a,b);
    fi;
end;	# `math/exponent`
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`math/flatprod` := proc(x)
    local i,i0,tbl,lead,tmp;
    
    if op(1,x) = -1 then
	i0 := 2;
	lead := 'leadingminus';
    elif HasLeadingMinus(op(1,x)) then
	i0 := 2;
	tmp := RemoveLeadingMinus(op(1,x));
	if NeedParensAsProduct(tmp) then
	    tmp := `&()`(tmp);
	fi;
	lead := 'leadingminus',tmp;
#	lead := 'leadingminus',-op(1,x);
    else
	i0 := 1;
	lead := NULL;
    fi;

    for i from i0 to nops(x) do
	if NeedParensAsProduct(op(i,x)) then
#?	    tbl[i] := `&delimiter`('leftparens','rightparens',op(i,x));
	    tbl[i] := `&delimiter`('leftparens','rightparens',false,op(i,x));	    
	else
	    tbl[i] := op(i,x);
	fi;
    od;
    `&sepsequence`('`&nothing`',lead,`&sepsequence`('timesop',seq(tbl[i],i=i0..nops(x))));
end;	# `math/flatprod`
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
### Argument sequence:
#	x::indexed
### Output format::list
#	symbol
#	indices::list
############################################################
`math/indexed/format` := proc(x)
    local x2,ind;
    ind := NULL;
    x2 := x;
    while type(x2,'indexed') do
	ind := op(x2),ind;    
	x2 := op(0,x2);
    od;
    [x2,[ind]];
end:	# `math/indexed/format`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: code for 'resolveref'
### Output format::{MAD|false}
############################################################
`numbering/resolveref` := proc(x)
    local res,i;
    
    if op(0,x) = `&newnumbering` then
	`numbering/pushstack`(op(_KEYW_PARAMS(x)));
	# recurse
	res := `&sequence`(seq(`resolveref/doit`(i),i=_KEYW_RECARGS(x)));
	`numbering/popstack`();
	res;
	
    elif op(0,x) = `&subnumbering` then
	# into
	`numbering/down`();
	res := `&sequence`(seq(`resolveref/doit`(i),i=_KEYW_RECARGS(x)));
	# up
	`numbering/up`();
	# increment numbering
	`numbering/stack` := subsop(-1=`numbering/next`(_NUMBERING_THIS),`numbering/stack`);
	res;
	
    elif op(0,x) = `&numbering++` then
	# increment numbering
	`numbering/stack` := subsop(-1=`numbering/next`(_NUMBERING_THIS),`numbering/stack`);
	NULL;
	
    elif op(0,x) = `&thisnumbering` then
	# forwarding
	`&NUMBERING`(_NUMBERING_THIS);
	
    else# quiet error
	false;
    fi;
end;	# `numbering/resolveref`
#>>>########################################################
#<<<########################################################
### Description: compare list of integers
intlistorder := proc(a,b)
    if nops(a)>0 and nops(b)>0 then
	if a[1] = b[1] then procname(a[2..-1],b[2..-1]);
	else evalb(a[1]<b[1]); fi;
    else evalb(nops(a)<nops(b)); fi;
end;	# intlistorder
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: compute the table of contents; this procedure
# is meant to be used from within the translators. If the
# optional label is provided, then the returned toc only
# concerns the subtree whose node is the label
### Argument sequence:
#	depth::{posint,infinity}
#	(optional) `DocumentGenerator/label`
### Output format::list(`DocumentGenerator/Label`)
############################################################
RawTableOfContents := proc(depth,label)
    local i;
    _OPTION;
    _ERR_TYPE(depth,{'posint','infinity'});
    
    if `toc/lock` = true then
	error "this procedure must be used in the translators";
    fi;
    
    if nargs = 1 then# full toc
	[seq(op(`RawTableOfContents/doit`(depth,i,1,false)),i=`toc/chapter`)];
    elif nargs = 2 then# partial toc
	if not _LBLTR_EXIST(label,thetranslator) then
	    error "%1: no such label",label; fi;
	`RawTableOfContents/doit`(depth,label,1,false);
    else _ERR_NARGS; fi;
end;	# RawTableOfContents
#>>>########################################################
#<<<########################################################
# @scope=local
### Output format::{NULL,Seq(string)}
### Argument sequence:
#	tbl::table
#	Seq({&string,&file,string})
### Description: this procedure makes the output of a
# translator not a recursive structure. The content of each
# &file is put into the table 'tbl' at the index that is the
# name of the file. This content is represented by a sequence
# of string. Things that are not enclosed into a &file are
# returned by the procedure as a sequence of string.
### Note: if several files have the same file descriptor,
# then they are overwriten. A version that prevents overwriting
# can be found in trash/output.mpl.
############################################################
norecursiveoutput := proc(tbl,x)
    local i;
    if nargs = 1 then NULL;
    elif nargs > 2 then seq(procname(tbl,args[i]),i=2..nargs);
    else# nargs = 2
	if op(0,x) = `&file` then
	    # op(1,x)::`DocumentGenerator/FileDescriptor`
	    tbl[CommonLib:-FileExtension(op(1,x),thetranslator:-FileExtension)] := 
		procname(tbl,op(2..-1,x));
	    # the recursive &file is replaced by
	    NULL;
	elif op(0,x) = `&string` then
	    procname(tbl,op(x));
	elif type(x,'string') then# end recursion
	    x;
	else
	    error "invalid in text mode [output]",x;
	fi;
    fi;
end;	# norecursiveoutput
#>>>########################################################
### END FUNCTIONS #############################
### BEGIN RAW INCLUSION #######################
### 11/02/02: @rawinclude is used in practice for
# initializations, which must be done after the
# definitions and declarations.
#<<<########################################################
# @rawinclude
### Description: declaration of the keywords
declaredockeyword(`&declarelabel`,[`DocumentGenerator/Label`],0);
declaredockeyword(`&declareitem`,[`DocumentGenerator/Label`,'symbol']);# remaining arguments are taken for the entry
declaredockeyword(`&DECLAREITEM`,[`DocumentGenerator/Label`,
    `DocumentGenerator/FileDescriptor`,'symbol',`DocumentGenerator/Numbering`]);# remaining arguments are taken for the entry
declaredockeyword(`&label->tag`,[`DocumentGenerator/Label`],0);
declaredockeyword(`&label->fileaddress`,[`DocumentGenerator/Label`,{'string',
    'VOID','identical'('absolute'),'identical'('relative')},{'string','VOID'}],0);
declaredockeyword(`&item->numbering`,[`DocumentGenerator/Label`],0);
declaredockeyword(`&item->entry`,[`DocumentGenerator/Label`],0);
declaredockeyword(`&file->address`,[`DocumentGenerator/FileDescriptor`,
    {'VOID','string','identical'('absolute'),'identical'('relative')},
    {'string','VOID'}],0);
declaredockeyword(`&ref`,[`DocumentGenerator/Label`],0);
declaredockeyword(`&link`,[`DocumentGenerator/Label`]);
declaredockeyword(`&plot`,[{'VOID',`DocumentGenerator/Label`},{'PLOT','PLOT3D'}]);# it is required, since there may be no label provided
declaredockeyword(`&toc`,[{'posint','infinity'}],0);# the filedesc is added (see doc.GRAMMAR.mpl)
declaredockeyword(`&subtoc`,[{'posint','infinity'},`DocumentGenerator/Label`],0);# the filedesc is added (see doc.GRAMMAR.mpl)
#>>>########################################################
#<<<########################################################
# @rawinclude
### Description: declaration of the keywords
declaredockeyword(`&file`,[`DocumentGenerator/FileDescriptor`]);
declaredockeyword(`&rawheader`,[list(`DocumentGenerator/Translator`)]);
declaredockeyword(`&localrawheader`,[list(`DocumentGenerator/Translator`)]);
declaredockeyword(`&thisfile`,[{'string','identical'('absolute'),'VOID'},{'string','VOID'}],0);
#>>>########################################################
#<<<########################################################
# @rawinclude
### see doc.translate.mpl, resolveref.mpl
### File: see doc.file.mpl
### Numbering: see doc.numbering.mpl
### Cross-references: see doc.cross-ref.mpl

### Keywords that address the translators ONLY
declaredockeyword(`&fileheader`,[],0);
declaredockeyword(`&filebody`,[]);
declaredockeyword(`&body`,[]);
declaredockeyword(`&commentline`,[]);
declaredockeyword(`&chapter`,[`DocumentGenerator/Label`,'boolean'],-1);# -1: title
declaredockeyword(`&section`,[`DocumentGenerator/Label`,'boolean'],-1);# -1: title
declaredockeyword(`&paragraph`,[]);
declaredockeyword(`&em`,[]);
declaredockeyword(`&tt`,[]);
declaredockeyword(`&bf`,[]);
declaredockeyword(`&fontsize`,[proc(x) member(x,{'xxsmall','xsmall','small','medium','large','xlarge','xxlarge'}); end]);
declaredockeyword(`&dq`,[]);
declaredockeyword(`&linkurl`,['string']);
declaredockeyword(`&ierror`,[]);
declaredockeyword(`&error`,[]);

### Keywords that eventually address the translators
declaredockeyword(`&HEADER`,[]);# see doc.translate.mpl
declaredockeyword(`&NUMBERING`,[`DocumentGenerator/Numbering`],0);# see doc.file.mpl
declaredockeyword(`&REF`,[`DocumentGenerator/FileDescriptor`,`DocumentGenerator/Label`],0);# see doc.cross-ref.mpl
declaredockeyword(`&LINK`,[`DocumentGenerator/FileDescriptor`,`DocumentGenerator/Label`]);# see doc.cross-ref.mpl
declaredockeyword(`&PLOT`,[`DocumentGenerator/FileDescriptor`,`DocumentGenerator/Label`,{'PLOT','PLOT3D'}]);# see doc.cross-ref.mpl
declaredockeyword(`&TOC`,[`DocumentGenerator/FileDescriptor`,{'posint','infinity'}],0);# see doc.cross-ref.mpl
declaredockeyword(`&SUBTOC`,[`DocumentGenerator/FileDescriptor`,{'posint','infinity'},`DocumentGenerator/Label`],0);# see doc.cross-ref.mpl
#>>>########################################################
#<<<########################################################
# @rawinclude
### Description: declaration of the keywords
declaredockeyword(`&newnumbering`,[{`DocumentGenerator/Numbering`,'VOID'}]);
declaredockeyword(`&subnumbering`,[]);
declaredockeyword(`&numbering++`,[],0);
declaredockeyword(`&thisnumbering`,[],0);
#>>>########################################################
#<<<########################################################
# @sharevar=`toc/lock`
# @rawinclude
`toc/lock` := true;
#>>>########################################################
#<<<########################################################
### Desclaration of the keywords that are handled in the
# body of `translate/doit`:
# @rawinclude
declaredockeyword(`&sequence`,[]);
declaredockeyword(`&string`,[]);
declaredockeyword(`&setoption`,['procedure',list('equation')]);
declaredockeyword(`&FILE`,[`DocumentGenerator/FileDescriptor`]);
declaredockeyword(`&header`,[],0);
#>>>########################################################
#<<<########################################################
### Description: set and read options for the text mode.
# @exportvar=DocStyle
# @sharevar=`style/toctypeinclude`
# @sharevar=`style/relativestypeinclude`
# @rawinclude
### Default values
`style/toctypeinclude` := {`&chapter`,`&section`};
`style/relativestypeinclude` := {`&chapter`,`&section`};

DocStyle := CommonLib:-MakeProcOptions({
    'toctypeinclude'=[{'set','list'}('symbol'),'`style/toctypeinclude`'],
    'relativestypeinclude'=[{'set','list'}('symbol'),'`style/relativestypeinclude`']
    });
#>>>########################################################
#<<<########################################################
# @sharevar=`option/nosqrt`
# @rawinclude
`option/nosqrt` := false;
#>>>########################################################
#<<<########################################################
# @rawinclude
# @sharevar=`option/solvealgnum`
`option/solvealgnum` := false;
#>>>########################################################
#<<<########################################################
# @sharevar=mathalias
# @rawinclude
### Description: see 'DefineMathAlias'
############################################################
#mathalias['e'] 		:= `&operatorname`('e');# math.exp.mpl
#mathalias['O'] 		:= `&operatorname`('O');# math.series.mpl
mathalias['exp']	:= `&operatorname`('exp');    
mathalias['log']	:= `&operatorname`('log');
mathalias['ln']		:= `&operatorname`('ln');
mathalias['sin']	:= `&operatorname`('sin');
mathalias['cos']	:= `&operatorname`('cos');
mathalias['tan']	:= `&operatorname`('tan');
mathalias['arcsin']	:= `&operatorname`('arcsin');
mathalias['arccos']	:= `&operatorname`('arccos');
mathalias['arctan']	:= `&operatorname`('arctan');
mathalias['sinh']	:= `&operatorname`('sinh');
mathalias['cosh']	:= `&operatorname`('cosh');
mathalias['tanh']	:= `&operatorname`('tanh');
mathalias['arcsinh']	:= `&operatorname`('arcsinh');
mathalias['arccosh']	:= `&operatorname`('arccosh');
mathalias['arctanh']	:= `&operatorname`('arctanh');
mathalias[`&N`]		:= `&mathbb`('N');
mathalias[`&Z`]		:= `&mathbb`('Z');
mathalias[`&R`]		:= `&mathbb`('R');
mathalias[`&C`]		:= `&mathbb`('C');
#>>>########################################################
#<<<########################################################
# @sharevar=mathextrakeyword
# @rawinclude
### Description: see 'DefineMathKeyword'
############################################################
mathextrakeyword[`&mathseq`] := proc(x)
    `&sepsequence`('nullspace',op(x));
end;

mathextrakeyword[`&()`] := proc(x)
    `&delimiter`('leftparens','rightparens',false,`&mathseq`(op(x)));
end;

mathextrakeyword[`&[]`] := proc(x)
    `&delimiter`('leftbracket','rightbracket',false,`&mathseq`(op(x)));
end;

mathextrakeyword[`&{}`] := proc(x)
    `&delimiter`('leftbrace','rightbrace',false,`&mathseq`(op(x)));
end;

#>>>########################################################
#<<<########################################################
# @rawinclude
declaremathkeyword(`&sepsequence`,['symbol']);
declaremathkeyword(`&delimiter`,['symbol','symbol','boolean'],1);
declaremathkeyword(`&frac`,[],2);
declaremathkeyword(`&subscript`,[],1);
declaremathkeyword(`&superscript`,[],1);
declaremathkeyword(`&sqrt`,['posint'],1);
declaremathkeyword(`&alternate`,[],2);
declaremathkeyword(`&sizedsep`,['symbol'],2);
declaremathkeyword(`&decoratedsym`,['symbol'],-1);
declaremathkeyword(`&nocut`,[]);
declaremathkeyword(`&relation`,['symbol'],2);
declaremathkeyword(`&operatorname`,['symbol'],0);
declaremathkeyword(`&mathbb`,['symbol'],0);
declaremathkeyword(`&mathcal`,['symbol'],0);
#>>>########################################################
#<<<########################################################
# @rawinclude
declaremathswitch(`&imath`,[]);
declaremathswitch(`&equation`,[{`DocumentGenerator/Label`,'VOID'},
    `DocumentGenerator/EquationSeparator`]);
#>>>########################################################
#<<<########################################################
# @rawinclude
### Description: initialization of the table 'mathtype'
# (moduleinit.mpl). The types listed in 'mathtype' are NOT
# meant to be overriden by 'DefineMathType'.
### History:
# 03/06/03: initialization is done in the body of the module
############################################################
mathtype[`DocumentGenerator/complex`] := `math/complex`;
mathtype[`DocumentGenerator/Sum`] := `math/Sum`;    
mathtype[`DocumentGenerator/Int`] := `math/Int`;        
mathtype['series'] := `math/series`;    
mathtype['indexed'] := `math/indexed`;
mathtype[`+`] := `math/sum`;
mathtype[`DocumentGenerator/flatprod`] := `math/flatprod`;# `*`
mathtype[`DocumentGenerator/quotient`] := `math/quotient`;# `*`
mathtype[`^`] := `math/exponent`;
mathtype[`DocumentGenerator/diff`] := `math/diff`;
mathtype[`DocumentGenerator/exp`] := `math/exp`;
#>>>########################################################
#<<<########################################################
# @rawinclude
### Description: initialization of the table 'mathextratype'
# (moduleinit.mpl). The types listed in 'mathextratype' can
# be overriden by 'DefineMathType'.
### History:
# 03/06/03: initialization is done in the body of the module
############################################################
mathextratype['set'] := proc(x)
    `&delimiter`('leftbrace','rightbrace',false,`&sepsequence`('comma',op(x)));
end;

mathextratype['list'] := proc(x)
    `&delimiter`('leftbracket','rightbracket',false,`&sepsequence`('comma',op(x)));
end;

mathextratype['range'] := proc(x)
    `&mathseq`(op(1,x),'ldots',op(2,x));
end;

mathextratype['relation'] := proc(x)
    `&relation`(op(0,x),op(x));
end;

mathextratype['RootOf'] := proc(x)
    local ind;

    # get index (or label)
    if nops(x) = 2 then
	ind := op([2,2],x);
    else ind := NULL; fi;
    
    subs(_Z='zeta',
    `&nocut`(`&mathseq`(`&operatorname`('RootOf'),`&subscript`(_Z),
	`if`(ind=NULL,NULL,`&superscript`(ind)),`&()`(op(1,x))))
    );
end;
#>>>########################################################
#<<<########################################################
### Description: this file implements the routine 'resolveref',
# which actually makes the first pass in the entire process
# of converting a MAD tree into a stream of strings. This
# first pass basically consists in resolving the references
# and computing the numbering of the sectioning.
# Note that 'resolveref' outputs a modified MAD tree, which
# is meant to be processed by 'translate'. 'resolveref'
# need to know the current translator:
# @sharevar=thetranslator
### Declaration of the kewords that are handled in the body
# of `resolveref/doit`:
# @rawinclude
declaredockeyword(`&only`,[list(`DocumentGenerator/Translator`)]);
declaredockeyword(`&but`,[list(`DocumentGenerator/Translator`)]);
#>>>########################################################
#<<<########################################################
# @exportvar=LabelToTag
# @rawinclude
LabelToTag := labeltotag;
#>>>########################################################
### END RAW INCLUSION #########################

end:
### END MODULE #################################
#SAVELIBNAME
#savelib('`type/DocumentGenerator/Sum`','`type/DocumentGenerator/FileDescriptor`','`type/DocumentGenerator/Numbering`','`type/DocumentGenerator/quotient`','`type/DocumentGenerator/exp`','`type/DocumentGenerator/Atomic`','`type/DocumentGenerator/flatprod`','`type/DocumentGenerator/complex`','`type/DocumentGenerator/Translator`','`type/DocumentGenerator/diff`','`type/DocumentGenerator/Int`','`type/DocumentGenerator/EquationSeparator`','`type/DocumentGenerator/Label`','`DocumentGenerator/RefTable`','DocumentGenerator'):
