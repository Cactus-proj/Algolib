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

# LaTeX - a Maple library for generating LaTeX
# Author: Ludovic Meunier
# Copyright (C) 2001-2007 Inria
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
############################################################
### Package level description
############################################################
### The package 'LaTeX' is a subset of the unmaintained
# package 'amslatex' (see ../old/amslatex) and it implements
# a translator for the DOC format into LaTeX (requiring the
# AMS LaTeX packages). The DOC format is described and
# partially implemented in 'DocumentGenerator'. Although the
# package 'LaTeX' is standalone, it is designed to be used
# from within 'DocumentGenerator' as a translator. In
# particular, the output of the routines of 'LaTeX' 
# (tree of strings) requires some processing.
# As the package 'DocumentGenerator', 'LaTeX' is divided into
# a math mode and a text mode.
############################################################
### Math mode: the math mode features automatic line-breaking
# and a fine tuning of the sizes of the delimiters. This is
# achieved by computing the approximate widths and heights of
# TeX boxes from Maple expressions. The width is expressed in
# terms of a number of characters (and it is represented by a
# float): a digit or a character have width 1. The height
# ranges between 1 and 6 (and it is also represented by a
# float), which is mapped to the LaTeX magnifiers "", "\\big",
# "\\Big", "\\bigg", "\\Bigg", and "\\Bigg". For instance,
# a digit has height 1, the type fraction has height 4. Note
# that the pair "\\left" "\\right" is used only when the
# enclosed expression doesn't span over several lines
# (otherwise, LaTeX would not compile). Both width and height
# are somehow internal to Maple: the actual LaTeX sizes depend
# on the LaTeX styles and fonts. Note that the height is also
# called bigness within the source code.
# The DOC format is a tree with named nodes (also called
# keywords), whose children are symbols and atomic types.The
# translation to LaTeX and the computation of TeX boxes are
# thus done recursively. Symbols are stored in a table (see
# knl.symboltable.mpl and lib.symbol.mpl): to each symbol are
# associated its LaTeX representation, its width and its
# height. The width of a DOC is computed by `doctolatex/width`
# (doctolatex.mpl): this is the width of the INLINED
# represensation of the DOC, even though it is greater than
# the maximal width of the line (`line/width`). The height of
# a DOC is known AFTER the DOC has been translated to LaTeX
# (in other words, the translation computes the height and
# returns it trough the data structure that is described in
# line.mpl).
# The translation to LaTeX is computed by `doctolatex/string`.
# Basically, this latter routine checks whether there is room
# enough in the current line to insert the sub-DOC (using
# `doctolatex/width`). If so, then it inserts it, otherwise,
# it checks whether the sub-DOC fits into a new line. If so,
# then it inserts it, otherwise the sub-DOC is spanned over
# several lines. This linebreaking mechanism attempts to
# represent the sub-DOC without cutting it if possible. In
# fact, one can disable the second checking (does the sub-DOC
# fit into a new line?) by setting the variable `shared/cutbyobject`
# to false, but the result may look strange (too much cutting,
# that's why this option is disabled in 'MathStyle'). The
# function `doctolatex/string` inputs and outputs a "line",
# which is the main data structure: it represents a line of
# LaTeX text and it is manipulated by the routines that are
# in file.mpl. Note: the whole implementation uses either
# macros (see macros.mi) or the routines that are defined in
# file.mpl for manipulating a line: THIS DATA STRUCTURE SHOULD
# NOT BE ACCESSED DIRECTLY.
# The implementation of the keywords and atomic types
# resides in the files whose names start by "knl." (meaning
# kernel). The implementation is a little tricky and only 
# "built-in" (or "fundamental") keywords should be implemented
# this way. New fundamental keywords are added to the package
# by adding it in the main lookup table (`type/kernel`, in
# doctolatex.mpl). Note that keywords may also be defined
# in terms of existing ones (see mathenvtable.mpl and
# 'DefineMathKeyword'), in which case the source code doesn't
# need any update.
############################################################
### Text mode: the framework is implemented by the package
# 'DocumentGenerator' and the implementation for 'LaTeX'
# is straightforward (see TranslateText.mpl).
############################################################
############################################################
### Decription: Maple preprocessor macros
### Conventions: $define _<NAME>_<NAME>
############################################################

### Standard error messages
$define _ERR_NARGS	error "wrong number of arguments"
$define _ERR_BADARGS	error "invalid arguments"
$define _ERR_BADVAL	error "invalid argument value"
$define _ERR_NOTIMP	error "not yet implemented"
$define _ERR_TYPE(x,t)	if not type(x,t) then error "%1 expected to be of type %2",x,t; fi

### Copyright
$define _COPYRIGHT	`Copyright (c) 2001-2007 INRIA. All rights reserved.`
$define _OPTION		option _COPYRIGHT

### Cross references
$define _REF_TABLE		`DocumentGenerator/RefTable`
$define _LABEL_NUMBERING(x,t)	_REF_TABLE[x,t][1]	# ::`DocumentGenerator/Numbering`
$define _LABEL_FILEDESC(x,t)	_REF_TABLE[x,t][2]	# ::`DocumentGenerator/FileDescriptor`
$define _LABEL_TYPE(x,t)	_REF_TABLE[x,t][3]	# ::symbol
$define _LABEL_TAG(x)		cat(CommonLib:-SepSeq(":",op(map(CommonLib:-AlphaNumeric,x))))

### Place holder
$define _OUT_STRING	`&string`

############################################################

### Hard-coded constants for atomic types
$define _INTEGER_CUTSTR		" \\backslash"
$define _INTEGER_CUTWID		1
$define _INTEGER_BIG		1
$define _SYMBOL_UNDERSCORESTR	"\\_"
$define _SYMBOL_UNDERSCOREWID	1
$define _SYMBOL_BIG		1
$define _FRACTION_BIGFLAT	1
$define _FRACTION_BIG		4
$define _FRACTION_DIVSTR	"/"
$define _FRACTION_DIVWID	2
$define _FRACTION_LEFT		"\\frac{"
$define _FRACTION_MID		"}{"
$define _FRACTION_RIGHT		"}"
$define _FLOAT_BIGFLAT		1
$define _FLOAT_BIG		2
$define _FLOAT_10STR		"\\,10"
$define _FLOAT_10WID		3
$define _FLOAT_LEFT		"^{"
$define _FLOAT_RIGHT		"}"
$define _FLOAT_FACTOR		0.8

### Tables
$define _SYM_TBL	symboltable
$define _MATHENV_TBL	mathenvtable

### Table of symbols (knl.symboltable.mpl)
$define _SYM_EXIST(x)	assigned(_SYM_TBL[x])
$define _SYM_STR(x)	op(1,_SYM_TBL[x])
$define _SYM_WID(x)	op(2,_SYM_TBL[x])
$define _SYM_HASBIG(x)	evalb(nops(_SYM_TBL[x])=3)
$define _SYM_BIG(x)	op(3,_SYM_TBL[x])
$define _SYM_ASSERT(x)	ASSERT(_SYM_EXIST(x))

### Symbols for locale (see locale.mpl, knl.symboltable.mpl)
$define _LOCALE_US	'_us'
$define _LOCALE_FR	'_fr'
$define _LOCALE_SET	{_LOCALE_US,_LOCALE_FR}

### Default values for parameters (see latexopts.mpl)
$define _DEF_MINDIGITS	6
$define _DEF_LOCALE	_LOCALE_US
$define _DEF_NEGEXP	-5
$define _DEF_POSEXP	5
$define _DEF_NOTRZEROS	false
$define _DEF_CUTBYOBJ	true
$define _DEF_WIDTH	60
$define _DEF_INDENT	2

### Line
$define _O_STR		1
$define _O_OFF		2
$define _O_BIG		3
$define _O_CUT		4
$define _L_STR(x)	op(_O_STR,x)
$define _L_OFF(x)	op(_O_OFF,x)
$define _L_BIG(x)	op(_O_BIG,x)
$define _L_CUT(x)	op(_O_CUT,x)
$define _L_MINWID	10

### Bigness (see line.mpl, knl.delimiter.mpl, knl.sepsequence.mpl)
$define _BIG_DEFAULT	1
$define _BIG_MIN	1
$define _BIG_MAX	6
$define _BIG_MAXINCUT	5
$define _BIG_LEFTMAGN	["","\\bigl","\\Bigl","\\biggl","\\Biggl","\\left"]
$define _BIG_RIGHTMAGN	["","\\bigr","\\Bigr","\\biggr","\\Biggr","\\right"]
$define _BIG_MAGN	["","\\big","\\Big","\\bigg","\\Bigg","\\Bigg"]
$define _BIG_DELIMINC	1

### `&frac`
$define _FRAC_LEFT	"\\frac{"
$define _FRAC_MID	"}{"
$define _FRAC_RIGHT	"}"
$define _FRAC_BIGINC	3
$define _FRAC_BIGFAC	0.75

### knl.script.mpl
$define _SUP_BEGSTR	"^{"
$define _SUB_BEGSTR	"_{"
$define _SCR_ENDSTR	"}"
$define _SCR_WID	1
$define _SCR_WIDFAC	0.8
$define _SCR_BIGINC	1
$define _SCR_BIGFAC	0.75

### `&sqrt`
$define _SQRT_2		"\\sqrt{"
$define _SQRT_N(n)	cat("\\sqrt[",n,"]{")
$define _SQRT_ENDSTR	"}"
$define _SQRT_WID	2

### `&mathenv`
$define _ENV_BEG	"{"
$define _ENV_END	"}"

############################################################


#<<<###########################################
### @exit
### Generated: 04/16/2007 19:12:24
### Module name: LaTeX
#>>>###########################################

#<<<########################################################
# @macro
$define _LATEX_COMMENT(X)	`&sequence`("%% ",X,"\n")
$define _LATEX_COMMENTSTR(X)	`&string`("%% ",X,"\n")
#>>>########################################################
### BEGIN MODULE ##############################
LaTeX := module()
	option `Copyright (c) 2001-2007 INRIA. All rights reserved.`,package, load=moduleinit;
### BEGIN DECLARATION #########################
local
	doctolatex,
	formatbigness,
	mathenvtable,
	moduleinit,
	resetindent,
	rmtrailingzeros,
	setindent,
	setlocale,
	symboltable,
	`&alternate/string`,
	`&alternate/width`,
	`&decoratedsym/string`,
	`&decoratedsym/width`,
	`&delimiter/string`,
	`&delimiter/width`,
	`&frac/string`,
	`&frac/width`,
	`&mathenv/string`,
	`&mathenv/width`,
	`&nocut/string`,
	`&nocut/width`,
	`&script/string`,
	`&script/width`,
	`&sepsequence/string`,
	`&sepsequence/width`,
	`&sizedsep/string`,
	`&sizedsep/width`,
	`&sqrt/string`,
	`&sqrt/width`,
	`&subscript/string`,
	`&subscript/width`,
	`&superscript/string`,
	`&superscript/width`,
	`atomic/string`,
	`atomic/width`,
	`doctolatex/string`,
	`doctolatex/tostring`,
	`doctolatex/width`,
	`float/negexp`,
	`float/notrailingzeros`,
	`float/posexp`,
	`float/string`,
	`float/width`,
	`fontsize/table`,
	`fraction/string`,
	`fraction/width`,
	`integer/forcesameline`,
	`integer/leadingzeros`,
	`integer/mindigits`,
	`integer/string`,
	`integer/width`,
	`line/autobreak`,
	`line/bigness`,
	`line/break`,
	`line/cat`,
	`line/insert`,
	`line/iscut`,
	`line/line`,
	`line/new`,
	`line/offset`,
	`line/rawinsert`,
	`line/setbigness`,
	`line/setoffset`,
	`line/string`,
	`line/width`,
	`math/&equation`,
	`math/&imath`,
	`math/equationgroup`,
	`shared/bignessfactor`,
	`shared/cutbyobject`,
	`shared/indent`,
	`shared/locale`,
	`style/captionwidth`,
	`style/documentclass`,
	`style/documentheader`,
	`style/documentoptions`,
	`style/plotwidth`,
	`symbol/string`,
	`symbol/width`,
	`typeid/extra`,
	`typeid/kernel`;
export
	DefineMathKeyword,
	DefineMathSymbol,
	DerivedFormat,
	DocStyle,
	FileExtension,
	FormatNumbering,
	MathStyle,
	TextSymbol,
	TranslateMath,
	TranslateText,
	UndefineMathKeyword,
	UndefineMathSymbol,
	latex;
global
	`type/LaTeX/atomic`;
### END DECLARATION ###########################

### BEGIN FUNCTIONS ###########################
#<<<########################################################
# @scope=local
### Argument sequence:
#	l::line
#	wid::float
### Output format::line
### Description: this piece of code is to be put in all
# functions: it ensures that if the object of width 'wid'
# fits in a new line but doesn't fit in 'l' the line
# is broken. This is where `shared/cutbyobject` is used.
############################################################
`line/autobreak` := proc(l,wid)
    # 11/11/02
    if wid + _SYM_WID('lineindent') <= `line/width` and
	`line/offset`(l) + wid > `line/width` and 
	`shared/cutbyobject`=true then
	`line/break`(l);
    else l; fi;
end:	# `line/autobreak`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	l::line
#	x::`&delimiter`
### Note: this is where the bigness is used.
############################################################
`&delimiter/string` := proc(l,x)
    local l2,wid,bindleft,leftn,rightn,expr,leftw,off,lbuf,big,
    lefts,rights,rightw;

    wid := `&delimiter/width`(x);
    l2 := `line/autobreak`(l,wid);
    # yield arguments
#?    leftn,rightn,expr := op(1..3,x);
    leftn,rightn := op(1..2,x);
    expr := op(4,x);
#?    bindleft := `if`(nops(x)=4,'nocut',NULL);# dummy
    bindleft := `if`(op(3,x)=true,'nocut',NULL);

    # convert into a buffer (offset 'off' is approximate; accurate
    # offset requires to know the bigness)
    leftw := _SYM_WID(leftn);
    off := `line/offset`(l2) + leftw;
    lbuf := `doctolatex/string`(`line/new`(off),expr);

    # handle bigness
    big := `line/bigness`(lbuf);
    big := `shared/bignessfactor` * big;# context
    big := formatbigness(big);# integer in _BIG_MIN.._BIG_MAX
    if big = _BIG_MAX and `line/iscut`(lbuf) then
	# \left-\right don't span on multiline
	big := _BIG_MAXINCUT;
    fi;

    # yield magnifier
#!    lefts := cat(_BIG_LEFTMAGN[big],_SYM_STR(leftn));
    lefts := _OUT_STRING(_BIG_LEFTMAGN[big],_SYM_STR(leftn));
#!    rights := cat(_BIG_RIGHTMAGN[big],_SYM_STR(rightn));
    rights := _OUT_STRING(_BIG_RIGHTMAGN[big],_SYM_STR(rightn));
    rightw := _SYM_WID(rightn);
    
    # output
    l2 := `line/rawinsert`(l2,lefts,bindleft);
    l2 := `line/cat`(l2,lbuf);
    l2 := `line/rawinsert`(l2,rights,rightw);
    # the delimiters increase the bigness
    `line/setbigness`(l2,max(big + _BIG_DELIMINC,`line/bigness`(l2)));
end:	# `&delimiter/string`
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`&superscript/string` := proc(l,x)
    `&script/string`(l,x,_SUP_BEGSTR);
end:	# `&superscript/string`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: switch indent between 0 and `shared/indent`
# times the 'defaultlineindent'. If a second argument is
# provided, the an 'alignmark' is added at the beginning of
# the line.
### Argument sequence:
#	n::{0,anything}
#	(optional) anything
############################################################
resetindent := proc(n)
    local ind,str,di;
    di := _SYM_TBL['defaultlineindent'];
    ind := `if`(n=0,0,`shared/indent`);
    str := `if`(nargs=2,_SYM_STR('alignmark'),"");
    _SYM_TBL['lineindent'] := [cat(str,di[1]$ind),ind*di[2]];
end;	# resetindent
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`&subscript/width` := proc(x)
    `&script/width`(x);
end:	# `&subscript/width`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequnce:
#	l1::line
#	l2::line
### Description: concatenate line 'l2' to 'l1'; the max of
# bigness is used as the bigness of the concatenated line.
############################################################
`line/cat` := proc(l1,l2)
    local str,off,big,cut;
#!    str := cat(_L_STR(l1),_L_STR(l2));
    str := _OUT_STRING(_L_STR(l1),_L_STR(l2));
    off := _L_OFF(l2);
    big := max(_L_BIG(l1),_L_BIG(l2));
    cut := evalb(_L_CUT(l1) or _L_CUT(l2));
    `line/line`(str,off,big,cut);
end:	# `line/cat`
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: format `DocumentGenerator/Numbering`
# into LaTeX.
### Output format::string
############################################################
FormatNumbering := proc(x)
    local sep;
    _OPTION;
    sep := ".";
    
    # "" forces conversion to string
    cat("",CommonLib:-SepSeq(sep,
	op(map(CommonLib:-AlphaNumeric,map(op,x)))));
end;	# FormatNumbering
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::`&sepsequence`
### Output format::float
############################################################
`&sepsequence/width` := proc(x)
    local i,largs,wid;
    largs := [op(x)];
    wid := add(`doctolatex/width`(i),i=largs[2..-1]);
    if type(largs[1],'procedure') then
	wid := wid + add(_SYM_WID(largs[1](largs[i],largs[i+1])),
	i=2..nops(largs)-1);
    else# symbol
	_SYM_ASSERT(largs[1]);
	wid := wid + (nops(largs)-1)*_SYM_WID(largs[1]);
   fi;
   wid;
end:	# `&sepsequence/width`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	l::line
#	x::`&sepsequence`
### Output format::line
############################################################
`&sepsequence/string` := proc(l,x)
    local i,wid,l2,largs,big;
    wid := `&sepsequence/width`(x);
    l2 := `line/autobreak`(l,wid);
    largs := [op(x)];
    big[1] := `line/bigness`(l2);
    for i from 2 to nops(largs) do
	l2 := `line/setbigness`(l2,big[1]);
	if i > 2 then
	    if type(largs[1],'procedure') then
		l2 := `line/insert`(l2,largs[1](largs[i-1],largs[i]));
	    else# symbol
		l2 := `line/insert`(l2,largs[1]);
	    fi;
	fi;
	l2 := `doctolatex/string`(l2,largs[i]);
	# bigness increase
	big[i] := `line/bigness`(l2)-big[1];
    od;
    `line/setbigness`(l2,big[1]+max(seq(big[i],i=2..nops(largs))));
end:	# `&sepsequence/string`
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`line/setoffset` := proc(l,o)
    option inline;
    subsop(_O_OFF=o,l);
end:	# `line/setoffset`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence
#	l::line
#	x::`&sqrt`
############################################################
`&sqrt/string` := proc(l,x)
    local l2,oldlw,wid;
    wid := `&sqrt/width`(x);
    l2 := `line/autobreak`(l,wid);
    oldlw := `line/width`;		# disable line-break
    `line/width` := 'infinity';		# disable line-break
    if nops(x)=1 or op(1,x)=2 then
	l2 := `line/rawinsert`(l2,_SQRT_2,_SQRT_WID);
    else
	l2 := `line/rawinsert`(l2,_SQRT_N(op(1,x)),_SQRT_WID);
    fi;
    l2 := `doctolatex/string`(l2,op(-1,x));
    l2 := `line/rawinsert`(l2,_SQRT_ENDSTR);
    `line/width` := oldlw;		# reset line-break
    l2;
end:	# `&sqrt/string`
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`&subscript/string` := proc(l,x)
    `&script/string`(l,x,_SUB_BEGSTR);
end:	# `&subscript/string`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence
#	x::`&decoratedsym`
############################################################
`&decoratedsym/width` := proc(x)
    local wsub,wsup;
    wsub := `doctolatex/width`(`&subscript`(op(2,x)));
    if nops(x) = 3 then
	wsup := `doctolatex/width`(`&superscript`(op(3,x)));
    else wsup := 0; fi;
    max(_SYM_WID(op(1,x)),wsup,wsub);
end:	# `&decoratedsym/width`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence
#	l: line
#	x::`LaTeX/atomic`
### Output format::line
### Description: wrapper for atomic types with new interface
############################################################
`atomic/string` := proc(l,x)
    local str,arg,X;
    
    # old argument sequence; X is assigned to the list
    # [newoffset::float,bigness::posint,iscut::boolean]
    arg := x,`line/offset`(l),`line/width`,X;

    # branching
    if type(x,'integer') then str := `integer/string`(arg);
    elif type(x,'fraction') then str := `fraction/string`(arg);
    elif type(x,'float') then str := `float/string`(arg);
    elif type(x,'symbol') then str := `symbol/string`(arg);
    else
	_ERR_BADARGS;
    fi;

    # `line/line` := proc(str,off,big,cut)
    `line/cat`(l,`line/line`(str,op(X)));
end:	# `atomic/string`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	l::line
#	x::`&frac`
### Description: updates `shared/bignessfactor`
############################################################
`&frac/string` := proc(l,x)
    local l2,oldlw,wid,off,oldbf;
    wid := `&frac/width`(x);
    l2 := `line/autobreak`(l,wid);

    oldlw := `line/width`;		# disable line-break
    `line/width` := 'infinity';		# disable line-break
    oldbf := `shared/bignessfactor`;	# set context
    `shared/bignessfactor` := 
	`shared/bignessfactor` * _FRAC_BIGFAC;# set context

    off := `line/offset`(l2);
    l2 := `line/rawinsert`(l2,_FRAC_LEFT);
    l2 := `doctolatex/string`(l2,op(1,x));
    l2 := `line/rawinsert`(l2,_FRAC_MID);
    l2 := `doctolatex/string`(l2,op(2,x));
    l2 := `line/rawinsert`(l2,_FRAC_RIGHT);
    l2 := `line/setoffset`(l2,off+wid);
    l2 := `line/setbigness`(l2,`line/bigness`(l2)+_FRAC_BIGINC);

    `line/width` := oldlw;		# reset line-break
    `shared/bignessfactor` := oldbf;	# reset context
    l2;
end:	# `&frac/string`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::{_LOCALE_US,_LOCALE_FR}
### Description: set locale-dependent elements. This procedure
# is part of module initialization (see moduleinit.mpl).
############################################################
setlocale := proc(x)
    `shared/locale` := x;
    # in knl.symboltable.mpl
    _SYM_TBL['float0']	:= _SYM_TBL[cat('`float0/`',`shared/locale`)];
    _SYM_TBL['floatcomma'] := _SYM_TBL[cat('`floatcomma/`',`shared/locale`)];
    # output
    NULL;
end;	# setlocale
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	l::line
#	x::`&nocut`(expr)
############################################################
`&nocut/string` := proc(l,x)
    local l2,oldw;
    oldw := `line/width`;
    `line/width` := 'infinity';
    l2 := `doctolatex/string`(l,op(x));
    `line/width` := oldw;
    l2;
end;	# `&nocut/string`
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	s::symbol
### Description: opposite of 'DefineMathKeyword'
############################################################
UndefineMathKeyword := proc(s)
    _MATHENV_TBL[s] := evaln(_MATHENV_TBL[s]);
    NULL;
end:	# UndefineMathKeyword
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	l::line
#	x::name (MUST be index of _SYM_TBL)
#	(optional) dummy
### Output format::line
### Description: add a symbol from _SYM_TBL; the third optional
# argument disables line-break
############################################################
`line/insert` := proc(l,x)
    local big,l2;
    _SYM_ASSERT(x);
    l2 := l;
    if _SYM_WID(x) + _L_OFF(l2) > `line/width` and nargs=2 then
	l2 := `line/break`(l2);
    fi;
    if _SYM_HASBIG(x) then
	big := _SYM_BIG(x);
    else
	big := _BIG_DEFAULT;
    fi;
    
    `line/line`(
#!	cat(_L_STR(l2),_SYM_STR(x)),
	_OUT_STRING(_L_STR(l2),_SYM_STR(x)),
	_L_OFF(l2) + _SYM_WID(x),
	max(_L_BIG(l2),big),
	_L_CUT(l2));
end:
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	s::symbol
#	p::procedure
### Description: make entry to _MATHENV_TBL
############################################################
DefineMathKeyword := proc(s,p)
    _OPTION;
    _ERR_TYPE(s,'symbol');
    _ERR_TYPE(p,'procedure');
    _MATHENV_TBL[s] := p;
    NULL;
end:	# DefineMathKeyword
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::&imath(Seq(DOC))
### Description: inline math mode
############################################################
`math/&imath` := proc(x)
    local str,oldw;
    oldw := `line/width`;	# disable line break
    `line/width` := 'infinity';	# disable line break
#    str := cat("\\begin{math}",exprtolatex(op(x)),"\\end{math}");
#!    str := cat("$",exprtolatex(op(x)),"$");
    str := _OUT_STRING("$",doctolatex(op(x)),"$");
    `line/width` := oldw;	# reset line break
    # output
    str;
end:	# `math/&imath`
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`&script/width` := proc(x)
    `doctolatex/width`(op(x)) * _SCR_WIDFAC + _SCR_WID;
end:	# `&script/width`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::`&frac`
############################################################
`&frac/width` := proc(x)
    max(`doctolatex/width`(op(1,x)),`doctolatex/width`(op(2,x)));
end:	# `&frac/width`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: convert the output of 'doctolatex' into
# a string. See also DocumentGenerator, 'norecursiveoutput'.
### Argument sequence:
#	x::'doctolatex'
############################################################
`doctolatex/tostring` := proc(x)
    local i;
    if type(x,'string') then
	x;
    else# op(0,x) = _OUT_STRING
	cat(seq(procname(i),i=[op(x)]));
    fi;
end;	# `doctolatex/tostring`
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`line/string` := proc(l)
    option inline;
    _L_STR(l);
end:	# `line/string`
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`&superscript/width` := proc(x)
    `&script/width`(x);
end:	# `&superscript/width`
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	s::symbol
#	l::list
### Description: this function adds the symbol 's' in _SYM_TBL
# (see knl.symboltable.mpl).
############################################################
DefineMathSymbol := proc(s,l)
    _OPTION;
    _ERR_TYPE(s,'symbol');
    _ERR_TYPE(l,'list');
    if not member(nops(l),{2,3}) then _ERR_BADVAL; fi;
    if not (type(l[1],'string') and type(l[2],'numeric') and l[2]>=0) then
	_ERR_BADVAL; fi;
    if nops(l) = 3 and not (type(l[3],'numeric') and l[3]>=0) then
	_ERR_BADVAL; fi;
    _SYM_TBL[s] := l;
    NULL;
end:	# DefineMathSymbol
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	s::symbol
### Description: remove entry form _SYM_TBL
############################################################
UndefineMathSymbol := proc(s)
    _OPTION;
    _SYM_TBL[s] := evaln(_SYM_TBL[s]);
    NULL;
end:	# UndefineMathSymbol
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`line/iscut` := proc(l)
    option inline;
    _L_CUT(l);
end:	# `line/iscut`
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`line/bigness` := proc(l)
    option inline;
    _L_BIG(l);
end:	# `line/bigness`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence
#	l::line
#	x::`&mathenv`
############################################################
`&mathenv/string` := proc(l,x)
    local l2,oldlw,wid;
    wid := `&mathenv/width`(x);
    l2 := `line/autobreak`(l,wid);
    oldlw := `line/width`;		# disable line-break
    `line/width` := 'infinity';		# disable line-break
    l2 := `line/rawinsert`(l2,op(1,x));
    l2 := `line/rawinsert`(l2,_ENV_BEG);
    l2 := `doctolatex/string`(l2,op(2,x));
    l2 := `line/rawinsert`(l2,_ENV_END);
    `line/width` := oldlw;		# reset line-break
    l2;
end:	# `&mathenv/string`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	l::line
#	s::string
#	(optional) width of the string::float
#	(optional) bigness of the string::float
############################################################
`line/rawinsert` := proc(l,s)
    local l2;
#!    l2 := subsop(_O_STR=cat(_L_STR(l),s),l);
    l2 := subsop(_O_STR=_OUT_STRING(_L_STR(l),s),l);
#    if nargs = 3 then
    if nargs > 2 then
	l2 := subsop(_O_OFF=_L_OFF(l)+args[3],l2);
    fi;
#    if nargs = 4 then
    if nargs > 3 then
	l2 := subsop(_O_BIG=max(_L_BIG(l),args[4]),l2);
    fi;
    l2;
end:	# `line/rawinsert`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	sep::`DocumentGenerator/EquationSeparator`
#	x::list
### Description: 'sep' is the pair of separators, while 'x'
# is a list of expressions. This procedure computes the nestable
# AMS-LaTeX construct "\split" that encloses them. Note that
# there is no label to manage at this level.
############################################################
`math/equationgroup` := proc(sep,x)
#    local label,intsep,endsep,exprs,l,i;
    local intsep,endsep,exprs,l,i;    
    # yield arguments
    intsep,endsep := op(sep);
    exprs := x;


    # 'l' is a line without anything at its beginning
    l := `line/new`();

    # If the widest left-hand side is smaller that
$define	_EQN_LHSRATIO	0.45
    # times `line/width` or if all relations fit in
    # the line, then the alignmark is put next to the
    # relation symbol. Otherwise, the general scheme is used.
    if {op(map2(op,0,exprs))} = {`&relation`} and (
    max(seq(`doctolatex/width`(op(2,i)),i=exprs)) <= 
    _EQN_LHSRATIO * `line/width` or 
    max(seq(`doctolatex/width`(i),i=exprs)) <= `line/width` ) then
	for i to nops(exprs) do
	    l := `doctolatex/string`(l,`&nocut`(`&sepsequence`(`&nothing`,
		op(2,exprs[i]),'alignmark',op(1,exprs[i]))));
	    resetindent('default','alignmark');
	    l := `doctolatex/string`(l,op(3,exprs[i]));
	    resetindent(0);# no indent, no alignmark
	    # separator
	    if i <> nops(exprs) then
		l := `line/insert`(l,intsep,'nobreak');
		l := `line/break`(l);
	    else
		l := `line/insert`(l,endsep,'nobreak');
	    fi;
	od;
    else# general scheme
	for i to nops(exprs) do
	    l := `line/insert`(l,'alignmark');
	    if op(0,exprs[i]) = `&relation` then
		# lhs with no indent
		resetindent(0,'alignmark');
		l := `doctolatex/string`(l,op(2,exprs[i]));
		# rhs with indent
		resetindent('default','alignmark');
		l := `doctolatex/string`(l,`&sepsequence`(`&nothing`,
		    op(1,exprs[i]),op(3,exprs[i])));
	    else
		resetindent('default','alignmark');
		l := `doctolatex/string`(l,exprs[i]);
	    fi;
	    resetindent(0);# no indent, no alignmark
	    # separator
	    if i <> nops(exprs) then
		l := `line/insert`(l,intsep,'nobreak');
		l := `line/break`(l);
	    else
		l := `line/insert`(l,endsep,'nobreak');
	    fi;
	od;
    fi;
    resetindent('default');
	
    # LaTeX environment
    _OUT_STRING("\\begin{split}\n",`line/string`(l),
	"\n\\end{split}");
end;	# `math/equationgroup`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	l::line
#	x::`&alternate`
############################################################
`&alternate/string` := proc(l,x)
    local l2,wid;
    wid := `&alternate/width`(x);
    l2 := `line/autobreak`(l,wid);
    
    if `line/offset`(l2) + wid <= `line/width` then
	# it fits
	`doctolatex/string`(l2,op(1,x));
    else
	# it doesn't fit
	`doctolatex/string`(l2,op(2,x));
    fi;
end;	# `&alternate/string`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::`&alternate`
############################################################
`&alternate/width` := proc(x)
    `doctolatex/width`(op(1,x));
end:	# `&alternate/width`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: constructor
############################################################
`line/line` := proc(str,off,big,cut)
	option inline;
    [str,off,big,cut];
end:	# `line/line`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::anything
### Output format::string
############################################################
doctolatex := proc(x)
    if nargs = 0 then "";
    elif nargs = 1 then
	`line/string`(`doctolatex/string`(`line/new`(),x));
    else
	_ERR_NARGS;
    fi;
end:	# doctolatex
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::fraction
### Output format::posint
### Description: compute length of fraction
############################################################
`fraction/width` := proc(x)
    local lmwid;
    if x < 0 then lmwid := _SYM_WID('leadingminus');
    else lmwid := 0; fi;
    max(
	`integer/width`(op(1,abs(x))),
	`integer/width`(op(2,x))) + lmwid;
end:	# `fraction/width`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::anything
### Output format::{list(procedure),false}
### Description: the function returns the procedures for
# computing the width of 'x' and to convert 'x' to string.
############################################################
`typeid/kernel` := proc(x)
    # atomic types
    if type(x,`LaTeX/atomic`) then
	[`atomic/width`,`atomic/string`];
    # environments
    elif op(0,x) = `&decoratedsym` then
	[`&decoratedsym/width`,`&decoratedsym/string`];
    elif op(0,x) = `&mathenv` then
	[`&mathenv/width`,`&mathenv/string`];
    elif op(0,x) = `&sepsequence` then
	[`&sepsequence/width`,`&sepsequence/string`];
    elif op(0,x) = `&nocut` then
	[`&nocut/width`,`&nocut/string`];
    elif op(0,x) = `&delimiter` then
	[`&delimiter/width`,`&delimiter/string`];
    elif op(0,x) = `&frac` then
	[`&frac/width`,`&frac/string`];
    elif op(0,x) = `&subscript` then
	[`&subscript/width`,`&subscript/string`];
    elif op(0,x) = `&superscript` then
	[`&superscript/width`,`&superscript/string`];
    elif op(0,x) = `&sqrt` then
	[`&sqrt/width`,`&sqrt/string`];
    elif op(0,x) = `&alternate` then
	[`&alternate/width`,`&alternate/string`];
    elif op(0,x) = `&sizedsep` then
	[`&sizedsep/width`,`&sizedsep/string`];
    else
	# quiet error
	false;
    fi;
end:	# `typeid/kernel`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::symbol
#	off::integer
#	max::integer
#	infolist::name (assigned to a list:[newoffset,bigness,iscut])
### Output format::string
### Description: convert symbol to string.
### History:
# 11/11/02: added `shared/cutbyobject`
# 19.02.03: #! modif for the output
############################################################
`symbol/string` := proc(x,off,mx,infolist)
    local str,linecut,n,off2,wid;

    # no look up in the table here; this is done by `exprtolatex/string`
    # ANSI-C identifiers
    str := convert(x,'string');
    if not StringTools['AndMap'](X -> StringTools['IsAlphaNumeric'](X) or X = "_",str) then
    # this test is now disabled
    #	or StringTools['IsDigit'](str[1]) then
	error "only ANSI-C compliant symbols are handled";
    fi;

    # underscore
    n := nops({StringTools['Search']("_",str)});
    wid := length(str) - n + n * _SYMBOL_UNDERSCOREWID;
#!    str := StringTools['SubstituteAll'](str,"_",_SYMBOL_UNDERSCORESTR);
    str := _OUT_STRING(StringTools['SubstituteAll'](str,"_",_SYMBOL_UNDERSCORESTR));
    if off + wid > mx then# `shared/cutbyobject` doesn't effect here
#!	str := cat(_SYM_STR('linebreak'),_SYM_STR('lineindent'),str);
	str := _OUT_STRING(_SYM_STR('linebreak'),_SYM_STR('lineindent'),str);
	off2 := _SYM_WID('lineindent');
	linecut := true;
    else
	off2 := off;
	linecut := false;
    fi;
#    if off2+wid > mx then
#	userinfo(1,'LaTeX',`symbol don't fit`);
#    fi;

    infolist := [off2+wid,_SYMBOL_BIG,linecut];
    return str;
end:
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::anything
### Output format::float
### Description: compute the width of 'x'
############################################################
`doctolatex/width` := proc(x)
    local tmp;
    if _SYM_EXIST(x) then
	return _SYM_WID(x); fi;
    tmp := `typeid/kernel`(x);# list of procedures
    if tmp <> false then
	return tmp[1](x); fi;
    tmp := `typeid/extra`(x);# DOC
    if tmp <> false then
	return procname(tmp); fi;
    _ERR_NOTIMP;
end;	# `doctolatex/width`
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`line/offset` := proc(l)
    option inline;
    _L_OFF(l);
end:	# `line/offset`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: compute width of an object of type 
# `LaTeX/atomic`
############################################################
`atomic/width` := proc(x)
    if type(x,'integer') then `integer/width`(x);
    elif type(x,'fraction') then `fraction/width`(x);
    elif type(x,'float') then `float/width`(x);
    elif type(x,'symbol') then `symbol/width`(x);
    else
	_ERR_BADARGS;
    fi;
end:	# `atomic/width`
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`&script/string` := proc(l,x,begstr)
    local l2,oldlw,off,oldbf;
    l2 := l;
    # no `line/autbreak` here

    oldlw := `line/width`;		# disable line-break
    `line/width` := 'infinity';		# disable line-break
    oldbf := `shared/bignessfactor`;	# set context
    `shared/bignessfactor` := 
	`shared/bignessfactor` * _SCR_BIGFAC;# set context

    l2 := `line/rawinsert`(l2,begstr,_SCR_WID);
    off := `line/offset`(l2);
    l2 := `doctolatex/string`(l2,op(x));
    l2 := `line/rawinsert`(l2,_SCR_ENDSTR);
    l2 := `line/setoffset`(l2,off + (`line/offset`(l2)-off)*_SCR_WIDFAC);
    l2 := `line/setbigness`(l2,`line/bigness`(l2)+_SCR_BIGINC);

    `line/width` := oldlw;		# reset line-break
    `shared/bignessfactor` := oldbf;	# reset context
    # output
    l2;
end:
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence
#	x::`&sqrt`
############################################################
`&sqrt/width` := proc(x)
    `doctolatex/width`(op(-1,x)) + _SQRT_WID;
end:	# `&sqrt/width`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::&equation(label|VOID,separator,MADseq)
### Description: 2D math mode
############################################################
`math/&equation` := proc(x)
    local num,mytag;
    # numbering
    if op(1,x) = 'VOID' then
	num := NULL;
	mytag := NULL;
    else
	num := DocumentGenerator:-RefInfo(op(1,x),LaTeX,'numbering');
	num := cat("\\tag{",FormatNumbering(num),"}");
	mytag := cat("\\label{",DocumentGenerator:-RefInfo(op(1,x),LaTeX,'tag'),"}\n");
    fi;
    _OUT_STRING("\n\\begin{equation*}\n",mytag,
	`math/equationgroup`(op(2,x),[op(3..-1,x)]),num,"\n\\end{equation*}\n");
end;	# `math/&equation`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::integer
### Output format::posint
### Description: compute length of integer
############################################################
`integer/width` := proc(x)
    local w;
    if x = 0 then return 1;
    elif x < 0 then
	w := _SYM_WID('leadingminus');
    else
	w := 0;
    fi;
    w + length(x) + `integer/leadingzeros`;
end:	# `integer/width`
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: translate DOC math mode AND switches to
# math mode in LaTeX.
### Argument sequence:
#	x::DOC
############################################################
TranslateMath := proc(x)
    _OPTION;
    if op(0,x) = `&imath` then
	`math/&imath`(x);
    elif op(0,x) = `&equation` then
	`math/&equation`(x);
    else
	doctolatex(x);
    fi;
end;	# TranslateMath
#>>>########################################################
#<<<########################################################
# @scope=local
# @load
### Decription: initialization
############################################################
moduleinit := proc()
    setindent(_DEF_INDENT);
    setlocale(_DEF_LOCALE);
    unprotect('TextSymbol');
    unprotect('TranslateText');
    unprotect('DerivedFormat');
end:	# moduleinit
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence::float that is NOT 0
### Output format::sequence (mantissa,exponent)
### Description: remove trailing zeros in the mantissa
############################################################
rmtrailingzeros := proc(x)
    local i,r,m,nm;
    m := abs(op(1,x));
    for i from 0 do
	nm := iquo(m,10,evaln(r));
	if r <> 0 then
	    return m*sign(x),op(2,x)+i;
	fi;
	m := nm;
    od;
end:	# rmtrailingzeros
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::`&sizedsep`
############################################################
`&sizedsep/width` := proc(x)
     # approximate
     _SYM_WID(op(1,x));
end;	# `&sizedsep/width`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence
#	l::line
#	x::`&decoratedsym`
### Description: never cut
############################################################
`&decoratedsym/string` := proc(l,x)
    local l2,oldlw,wid,ssub,ssup;
    wid := `&decoratedsym/width`(x);
    l2 := `line/autobreak`(l,wid);
    oldlw := `line/width`;		# disable line-break
    `line/width` := 'infinity';		# disable line-break
    ssub := _L_STR(`doctolatex/string`(`line/new`(),`&subscript`(op(2,x))));
    ssup := `if`(nops(x)=3,_L_STR(`doctolatex/string`(`line/new`(),`&superscript`(op(3,x)))),"");
    l2 := `line/rawinsert`(l2,_SYM_STR(op(1,x)));
    l2 := `line/rawinsert`(l2,ssub);
    l2 := `line/rawinsert`(l2,ssup,wid);
    `line/width` := oldlw;		# reset line-break
    l2;
end:	# `&decoratedsym/string`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::symbol
### Output format::posint
### Description: compute length of symbol
############################################################
`symbol/width` := proc(x)
    local X;
    `symbol/string`(x,0,infinity,X);
    op(1,X);
end:
#>>>########################################################
#<<<########################################################
# @scope=global
### Output format::boolean
### Description: type checking
############################################################
`type/LaTeX/atomic` := proc(x)
    # This MUST be the same type as `DocumentGenerator/Atomic`;
    # this latter type is not used however, such that the
    # package 'LaTeX' is standalone.
    type(x,{'integer','fraction','float','symbol'});
end:	# `type/LaTeX/atomic`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: 'lineindent' represents the symbol that is
# used for the current value of the indent. In fact, this
# current value is a multiple of 'defaultlineindent' (see
# knl.symboltable.mpl). This procedure is used only for
# initialization (moduleinit.mpl and option.mpl).
### Argument sequence:
#	n::nonnegint
############################################################
setindent := proc(n)
    local di;
    `shared/indent` := n;
    di := _SYM_TBL['defaultlineindent'];
    # 06/03/03: added "", which prevents bug when n = 0
    _SYM_TBL['lineindent'] := [cat("",di[1]$n),n*di[2]];
end:	# setindent
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::`&delimiter`
############################################################
`&delimiter/width` := proc(x)
    local largs;
    largs := [op(x)];
#?    _SYM_WID(largs[1]) + `doctolatex/width`(largs[3]) + 
#?    _SYM_WID(largs[2]);
    _SYM_WID(largs[1]) + `doctolatex/width`(largs[4]) + 
    _SYM_WID(largs[2]);
end:	# `&delimiter/width`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::float
### Output format::posint
### Description: compute length of float
### History:
# 11/11/02: added `shared/cutbyobject`
# 11/29/02: 0 and comma are locale-dependent (see knl.symboltable.mpl)
############################################################
`float/width` := proc(x)
    local man,exp,manwid,expwid,sh;
    
    # stupid convention used by Maple 0.0000=0.=0 !!!!
    if x = 0 then
#	return _FLOAT_0WID;# 11/29/02
	return _SYM_WID('float0');
    fi;		

    if `float/notrailingzeros` then
	man,exp := rmtrailingzeros(x);
    else
	man,exp := op(x);
    fi;
	
    # display options
    # sh is the number of digits to the left of comma (0 when 4.2234)
    sh := floor(evalf(log10(abs(x))));
    if sh < 0 and sh >= `float/negexp` then
	# zero filling
	manwid := -sh + `integer/width`(man);
	expwid := 0;

    elif sh >= 0 and sh <= `float/posexp` then
	manwid := `integer/width`(man);
	expwid := 0;
    
    else
	# exponent form
	manwid := `integer/width`(man);
	exp := exp + (length(man)-1);
#	expwid := _FLOAT_10WID + _SYM_WID('float10sep') +
#	    _SYM_WID('superscriptleft') +
#	    _FLOAT_FACTOR * `integer/width`(exp) +
#	    _SYM_WID('superscriptright');
	expwid := _FLOAT_10WID + _FLOAT_FACTOR * `integer/width`(exp);
    fi;
#    _FLOAT_COMMAWID + manwid + expwid;# 11/29/02
     _SYM_WID('floatcomma') + manwid + expwid;
end:	# `float/width`
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	x::{NULL,name,name=anything}
### Description: set/read options for style in math mode
############################################################
MathStyle := proc(x)
    _OPTION;
    local opt,val;
    if nargs = 0 then
	# list options
	return {
#	'mindigits',
	'width',
	'floatstyle',
	'notrailingzero',
	'locale',
#	'cutbyobject',
	'indent'
	    };
    fi;
    if nargs > 1 then _ERR_NARGS; fi;
    if type(x,'name') then
	opt := x; 
	val := NULL;
    elif type(x,'name'='anything') then 
	opt := op(1,x);
	val := op(2,x);
    else
	_ERR_BADARGS;
    fi;
    
    ### options
#    if opt = 'mindigits' then
#	if val = NULL then `integer/mindigits`;
#	elif val > 0 then
#	    `integer/mindigits` := val;
#	    NULL;
#	else _ERR_BADVAL; fi;
#    elif opt = 'width' then
    if opt = 'width' then
	if val = NULL then `line/width`;
	elif val >= _L_MINWID then
	    `line/width` := val;
	    NULL;
	else _ERR_BADVAL; fi;
    elif opt = 'floatstyle' then
	if val = NULL then [`float/negexp`,`float/posexp`];
	elif type(val,list) and nops(val)=2 and 
	    op(1,val) <= 0 and op(2,val) >= 0 then
		`float/negexp` := op(1,val);
		`float/posexp` := op(2,val);
		NULL;
	else _ERR_BADVAL; fi;
    elif opt = 'notrailingzero' then
	if val = NULL then `float/notrailingzeros`;
	elif type(val,'boolean') then
	    `float/notrailingzeros` := val;
	    NULL;
	else _ERR_BADVAL; fi;
#    elif opt = 'cutbyobject' then
#	if val = NULL then `shared/cutbyobject`;
#	elif type(val,'boolean') then
#	    `shared/cutbyobject` := val;
#	    NULL;
#	else _ERR_BADVAL; fi;
    elif opt = 'locale' then
	if val = NULL then `shared/locale`;
        elif member(val,_LOCALE_SET) then
	    setlocale(val);
	    NULL;
	else _ERR_BADVAL; fi;
    elif opt = 'indent' then
	if val = NULL then `shared/indent`;
	elif type(val,'nonnegint') then
	    setindent(val);
	    NULL;
	else _ERR_BADVAL; fi;
    else
	_ERR_BADARGS;
    fi;
end:	# MathStyle
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::anything
### Output format: DOC or false
### Description: look up in _MATHENV_TBL
############################################################
`typeid/extra` := proc(x)
    if assigned(_MATHENV_TBL[op(0,x)]) then
	_MATHENV_TBL[op(0,x)](x);
    else false; fi;
end;	# `typeid/extra`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: add line break
############################################################
`line/break` := proc(l)
    local str;
#!    str := cat(_L_STR(l),_SYM_STR('linebreak'),_SYM_STR('lineindent'));
    str := _OUT_STRING(_L_STR(l),_SYM_STR('linebreak'),_SYM_STR('lineindent'));
    `line/line`(str,_SYM_WID('lineindent'),_L_BIG(l),true);
end:	# `line/break`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	l::line
#	x::anything
### Output format::line
### Description: convert 'x' to string and add it to the
# line 'l'.
############################################################
`doctolatex/string` := proc(l,x)
    local tmp;
    if _SYM_EXIST(x) then
	return `line/insert`(l,x); fi;
    tmp := `typeid/kernel`(x);# list of procedures
    if tmp <> false then
	return tmp[2](l,x); fi;
    tmp := `typeid/extra`(x);# DOC
    if tmp <> false then
	return procname(l,tmp); fi;
    _ERR_NOTIMP;
end:	# `doctolatex/string`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	l::line
#	x::`&sizedsep`
############################################################
`&sizedsep/string` := proc(l,x)
    local l2,big,wid;
    wid := `&sizedsep/width`(x);
    l2 := `line/autobreak`(l,wid);

    # bigness
    big := max(`line/bigness`(`doctolatex/string`(`line/new`(),op(2,x))),
	`line/bigness`(`doctolatex/string`(`line/new`(),op(3,x))));
    big := formatbigness(big);

    # output
    `line/rawinsert`(l2,cat(_BIG_MAGN[big],_SYM_STR(op(1,x))));
end;	# `&sizedsep/width`
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: new version of 'latex'
### Argument sequence:
#	x::expr
#	(optional) fmt::{'string','stdout'}
############################################################
latex := proc(x)
    local str,out;
    _OPTION;
    str := `doctolatex/tostring`(doctolatex(DocumentGenerator:-Math(x)));
    
    if nargs = 1 then
	out := 'stdout';
    elif nargs > 1 and member(args[2],{'string','stdout'}) then
	out := args[2];
    else _ERR_BADARGS; fi;
    
    if out = 'stdout' then
	printf("%s\n",str);
    else str; fi;
    
end;	# latex
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::integer (or `&nothing`)
#	off::integer
#	max::integer
#	infolist::name (assigned to a list:[newoffset,bigness,iscut])
### Output format::string
### Description: convert integer to string. If the special
# symbol '&nothing' is used, then null string is returned.
### History:
# 11/11/02: added `shared/cutbyobject`
# 19.02.03: #! modification such that the output is a list
# of string.
############################################################
`integer/string` := proc(x,off,max,infolist)
    local str,wid,off2,max2,remwid,lmstr,lmwid,s;
    
    # special
    if x = '`&nothing`' then
	infolist := [off,_INTEGER_BIG,false];
#!	return "";
	return _OUT_STRING("");
    fi;

    # leading zeros
    str := cat(
	"0"$`integer/leadingzeros`,
	convert(abs(x),'string'));
    
    wid := `integer/width`(x);	# include leading minus and leading zeros
    
    off2 := off + wid;
    if off2 <= max then
	# room enough in current line
	infolist := [off2,_INTEGER_BIG,false];
#!	return cat(
#!	    `if`(x<0,_SYM_STR('leadingminus'),""),
#!	    str
#!	    );
	return _OUT_STRING(`if`(x<0,_SYM_STR('leadingminus'),""),str);
    else
	off2 := _SYM_WID('lineindent') + wid;
	if off2 <= max and `integer/forcesameline`=false and `shared/cutbyobject`=true then
	    # room enough in next line
	    infolist := [off2,_INTEGER_BIG,true];
#!	    return cat(
#!		_SYM_STR('linebreak'),
#!		_SYM_STR('lineindent'),
#!		`if`(x<0,_SYM_STR('leadingminus'),""),
#!		str
#!		);
	    return _OUT_STRING(_SYM_STR('linebreak'),_SYM_STR('lineindent'),
		`if`(x<0,_SYM_STR('leadingminus'),""),str);
	else		
	    # cut integer: at least `integer/mindigits` should fit
	    
	    # leading minus
	    lmwid,lmstr := op(`if`(x<0,
		[_SYM_WID('leadingminus'),_SYM_STR('leadingminus')],
		[0,""]));
	    
	    if off + lmwid + `integer/mindigits` + _INTEGER_CUTWID > max 
		and `integer/forcesameline` = false then
#!		s := cat(
#!		    _SYM_STR('linebreak'),
#!		    _SYM_STR('lineindent'),
#!		    lmstr
#!		    );
		s := _OUT_STRING(_SYM_STR('linebreak'),_SYM_STR('lineindent'),lmstr);
		off2 := _SYM_WID('lineindent') + lmwid;
	    else
#!		s := lmstr;
		s := _OUT_STRING(lmstr);
		off2 := off + lmwid;
	    fi;

	    if lmwid + _SYM_WID('lineindent') + 1 + _INTEGER_CUTWID > max then
		# increase width
#		userinfo(1,'LaTeX',`integer don't fit: increasing width`);
		max2 := lmwid + _SYM_WID('lineindent') + 1 + _INTEGER_CUTWID;
	    else
		max2 := max;
	    fi;

	    do
		wid := length(str);
		if wid = 0 then
		    infolist := [off2,_INTEGER_BIG,true];
#!		    return s;
		    return _OUT_STRING(s);
		fi;
		remwid := max2 - off2;
		if wid <= remwid then
		    infolist := [off2 + wid,_INTEGER_BIG,true];
#!		    return cat(s,str);
		    return _OUT_STRING(s,str);
		else
		    remwid := floor(remwid - _INTEGER_CUTWID);
		    remwid := `if`(remwid<0,0,remwid);
#!		    s := cat(s,
#!			`if`(remwid=0,"",cat(str[1..remwid],_INTEGER_CUTSTR)),
#!			_SYM_STR('linebreak'),
#!			_SYM_STR('lineindent')
#!			);
#!		    s := _OUT_STRING(s,`if`(remwid=0,"",[str[1..remwid],_INTEGER_CUTSTR]),
		    s := _OUT_STRING(s,`if`(remwid=0,"",_OUT_STRING(str[1..remwid],_INTEGER_CUTSTR)),
			_SYM_STR('linebreak'),
			_SYM_STR('lineindent'));
		    str := str[remwid+1..-1];
		    off2 := _SYM_WID('lineindent');
		fi;
	    od;
	fi;
    fi;
end:	# `integer/string`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::float
#	off::integer
#	max::integer
#	infolist::name (assigned to a list:[newoffset,bigness,iscut])
### Output format::string
### Description: convert float to string.
### History: added `shared/cutbyobject`
# 19.02.03: #! modif for the output
############################################################
`float/string` := proc(x,off,mx,infolist)
    local off2,man,exp,sh,str,lmwid,lmstr,X,lman,
	rman,wid,old1,istall,linecut;

    # 0 treated apart
    if op(1,x) = 0 then
#	infolist := [off+_FLOAT_0WID,_FLOAT_BIGFLAT,false];# 11/29/02
	infolist := [off+_SYM_WID('float0'),_FLOAT_BIGFLAT,false];
#	return _FLOAT_0STR;# 11/29/02
#!	return _SYM_STR('float0');
	return _OUT_STRING(_SYM_STR('float0'));
    fi;

    # get mantissa and exponent
    if `float/notrailingzeros` then
	man,exp := rmtrailingzeros(x);
    else
	man,exp := op(x);
    fi;

    # leading minus
    lmwid,lmstr := op(`if`(x<0,[_SYM_WID('leadingminus'),_SYM_STR('leadingminus')],[0,""]));

    # split mantissa wrt comma
    sh := floor(evalf(log10(abs(x))));
    if sh < 0 and sh >= `float/negexp` then
	# zero filling
	lman := 0;
	rman := [abs(man),-sh-1];
	exp := NULL;
#	istall := false;
	istall := _FLOAT_BIGFLAT;
    elif sh >= 0 and sh <= `float/posexp` then
	# get the first sh+1 digits of mantissa
	lman := iquo(abs(man),10^(length(man)-sh-1),rman);
	# rman may need to be 0-filled
	rman := [rman,length(man)-sh-1-`if`(rman=0,1,length(rman)) ];
	rman := `if`(op(1,rman)=0 and op(2,rman)=-1,[`&nothing`,0],rman);
	exp := NULL;
#	istall := false;
	istall := _FLOAT_BIGFLAT;
    else
	# exponent form
	# get the first 1 digit of mantissa
	lman := iquo(abs(man),10^(length(man)-1),rman);
	# rman may need to be 0-filled
	rman := [rman,length(man)-1-`if`(rman=0,1,length(rman)) ];
	exp := exp + (length(man)-1);
#	istall := true;
	istall := _FLOAT_BIG;
    fi;

    # display
    wid := `float/width`(x);

    if (off + wid <= mx) or (wid + _SYM_WID('lineindent') <= mx and `shared/cutbyobject`=true) then
	# no cut
	if off + wid <= mx then
	    # current line
#!	    str := "";
	    str := _OUT_STRING("");
	    off2 := off;
	    linecut := false;
	else
	    # next line
#!	    str := cat(_SYM_STR('linebreak'),_SYM_STR('lineindent'));
	    str := _OUT_STRING(_SYM_STR('linebreak'),_SYM_STR('lineindent'));
	    off2 := _SYM_WID('lineindent');
	    linecut := true;
	fi;
#!	str := cat(str,lmstr,`integer/string`(lman,off2+lmwid,mx,evaln(X)));
	str := _OUT_STRING(str,lmstr,`integer/string`(lman,off2+lmwid,mx,evaln(X)));
#	str := cat(str,_FLOAT_COMMASTR);# 11/29/02
#!	str := cat(str,_SYM_STR('floatcomma'));
	str := _OUT_STRING(str,_SYM_STR('floatcomma'));
#	off2 := op(1,X) + _FLOAT_COMMAWID;# 11/29/02
	off2 := op(1,X) + _SYM_WID('floatcomma');

	# zero filling
	`integer/leadingzeros` := op(2,rman);
#!	str := cat(str,`integer/string`(op(1,rman),off2,mx,evaln(X)));
	str := _OUT_STRING(str,`integer/string`(op(1,rman),off2,mx,evaln(X)));
	`integer/leadingzeros` := 0;
	
	if exp <> NULL then
#	    str := cat(str,_SYM_STR('float10sep'),_FLOAT_10STR,_FLOAT_LEFT);
#	    off2 := op(1,X) + _SYM_WID('float10sep') + _FLOAT_10WID + _SYM_WID('superscriptleft');
#	    # starting pos no significant
#	    str := cat(str,`integer/string`(exp,0,mx,evaln(X)),_FLOAT_RIGHT);
#	    off2 := off2 + op(1,X) * _FLOAT_FACTOR+_SYM_WID('superscriptright');
#!	    str := cat(str,_FLOAT_10STR,_FLOAT_LEFT);
	    str := _OUT_STRING(str,_FLOAT_10STR,_FLOAT_LEFT);
	    off2 := op(1,X) + _FLOAT_10WID;
	    # starting pos no significant
#!	    str := cat(str,`integer/string`(exp,0,mx,evaln(X)),_FLOAT_RIGHT);
	    str := _OUT_STRING(str,`integer/string`(exp,0,mx,evaln(X)),_FLOAT_RIGHT);
	    off2 := off2 + op(1,X) * _FLOAT_FACTOR;
	else
	    off2 := op(1,X);
	fi;
	infolist := [off2,istall,linecut];
	return str;

    else
	# need to cut; exponent NOT flattened

	# leading minus by hand
	if off + lmwid + `integer/mindigits` + _INTEGER_CUTWID <= mx then
#!	    str := lmstr;
	    str := _OUT_STRING(lmstr);
	    off2 := off + lmwid;
	else
#!	    str := cat(_SYM_STR('linebreak'),_SYM_STR('lineindent'),lmstr);
	    str := _OUT_STRING(_SYM_STR('linebreak'),_SYM_STR('lineindent'),lmstr);
	    off2 := _SYM_WID('lineindent') + lmwid;
	fi;

	# mantissa
	old1 := `integer/mindigits`;

	# insert comma even if shouldn't fit
#	str := cat(str,`integer/string`(lman,off2,mx,evaln(X)),_FLOAT_COMMASTR);# 11/29/02
#!	str := cat(str,`integer/string`(lman,off2,mx,evaln(X)),_SYM_STR('floatcomma'));
	str := _OUT_STRING(str,`integer/string`(lman,off2,mx,evaln(X)),_SYM_STR('floatcomma'));
#	off2 := op(1,X) + _FLOAT_COMMAWID;# 11/29/02
	off2 := op(1,X) + _SYM_WID('floatcomma');
#	if off2 > mx then
#	    userinfo(1,'LaTeX',`comma float don't fit`);
#	fi;
	`integer/mindigits` := 1;
	`integer/forcesameline` := true;
	`integer/leadingzeros` := op(2,rman);
#!	str := cat(str,`integer/string`(op(1,rman),off2,mx,evaln(X)));
	str := _OUT_STRING(str,`integer/string`(op(1,rman),off2,mx,evaln(X)));
	off2 := op(1,X);
	`integer/leadingzeros` := 0;		# default value
	`integer/forcesameline` := false;	# default value
	`integer/mindigits` := old1;
	
	# exponent
	if exp <> NULL then
#	    wid := _FLOAT_10WID + _SYM_WID('float10sep') + _SYM_WID('superscriptleft') +
#	    _FLOAT_FACTOR * `integer/width`(exp) +
#	    _SYM_WID('superscriptright');
	    wid := _FLOAT_10WID + _FLOAT_FACTOR * `integer/width`(exp);
	    if off2 + wid <= mx then
#		str := cat(str,_SYM_STR('float10sep'),_FLOAT_10STR);
#!		str := cat(str,_FLOAT_10STR);
		str := _OUT_STRING(str,_FLOAT_10STR);
		off2 := off2 + wid;
	    else
#!		str := cat(str,_SYM_STR('linebreak'),_SYM_STR('lineindent'),_FLOAT_10STR);
		str := _OUT_STRING(str,_SYM_STR('linebreak'),_SYM_STR('lineindent'),_FLOAT_10STR);
#		off2 := _SYM_WID('lineindent') + wid - _SYM_WID('float10sep');
		off2 := _SYM_WID('lineindent') + wid;
	    fi;
#!	    str := cat(str,_FLOAT_LEFT);
	    str := _OUT_STRING(str,_FLOAT_LEFT);

#!	    str := cat(str,`integer/string`(exp,0,mx,evaln(X)));# beg position not significant
	    str := _OUT_STRING(str,`integer/string`(exp,0,mx,evaln(X)));# beg position not significant
#!	    str := cat(str,_FLOAT_RIGHT);
	    str := _OUT_STRING(str,_FLOAT_RIGHT);
	fi;
	infolist := [off2,istall,true];
	return str;
    fi;
end:	# `float/string`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::fraction
#	off::integer
#	max::integer
#	infolist::name (assigned to a list:[newoffset,bigness,iscut])
### Output format::string
### Description: convert fraction to string.
### History:
# 11/11/02: added `shared/cutbyobject`
# 19.02.03: #! modif. for the output
############################################################
`fraction/string` := proc(x,off,max,infolist)
    local off2,wid,X,Y,s;

    wid := `fraction/width`(x);
    
    # room enough on current line
    if off + wid <= max then
	infolist := [off + wid,_FRACTION_BIG,false];
#!	return cat(
	return _OUT_STRING(
	    `if`(x<0,_SYM_STR('leadingminus'),""),
	    _FRACTION_LEFT,
	    `integer/string`(abs(op(1,x)),0,max,X),
	    _FRACTION_MID,
	    `integer/string`(op(2,x),0,max,Y),
	    _FRACTION_RIGHT
#!	    );	    
	    );

    else
	# room enough on next line
	if wid + _SYM_WID('lineindent') <= max and `shared/cutbyobject`=true then
		infolist := [wid + _SYM_WID('lineindent'),_FRACTION_BIG,true];
#!		return cat(
		return _OUT_STRING(
		    _SYM_STR('linebreak'),
		    _SYM_STR('lineindent'),
		    `if`(x<0,_SYM_STR('leadingminus'),""),
		    _FRACTION_LEFT,
		    `integer/string`(abs(op(1,x)),0,max,X),
		    _FRACTION_MID,
		    `integer/string`(op(2,x),0,max,Y),
		    _FRACTION_RIGHT
#!		    );
		    );
	else
	    # need to flatten the quotient

	    # numer
	    s := `integer/string`(op(1,x),off,max,X);
	    off2 := op(1,X);
	    if off2 + _FRACTION_DIVWID > max then
#!		s := cat(s,
		s := _OUT_STRING(s,
		    _SYM_STR('linebreak'),
		    _SYM_STR('lineindent'),
		    _FRACTION_DIVSTR
#!		    );
		);
		off2 := _SYM_WID('lineindent') + _FRACTION_DIVWID;
	    else
#!		s := cat(s,_FRACTION_DIVSTR);
		s := _OUT_STRING(s,_FRACTION_DIVSTR);
		off2 := off2 + _FRACTION_DIVWID;
	    fi;
	    # denom
#!	    s := cat(s,`integer/string`(op(2,x),off2,max,Y));
	    s := _OUT_STRING(s,`integer/string`(op(2,x),off2,max,Y));
	    infolist := [op(1,Y),_FRACTION_BIGFLAT,true];
	    # params
	    return s;
	fi;
    fi;
end:	# `fraction/string`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	(optional) offset::float
### Description: initialization
############################################################
`line/new` := proc()
    local off;
    if nargs = 1 then off := args[1];
    else off := 0; fi;
#!    `line/line`("",off,_BIG_DEFAULT,false);
    `line/line`(_OUT_STRING(""),off,_BIG_DEFAULT,false);
end:	# `line/new`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence
#	x::`&mathenv`
############################################################
`&mathenv/width` := proc(x)
    `doctolatex/width`(op(2,x));
end:	# `&mathenv/width`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::float
### Output format::integer
### Description: the bigness is represented by a float, but
# it is eventually converted into an integer to yield the
# magnifier (see macros.mi).
############################################################
formatbigness := proc(x)
    local big;
    big := round(x);
    if big < _BIG_MIN then big := _BIG_MIN;
    elif big > _BIG_MAX then big := _BIG_MAX;
    fi;
    big;
end:	# formatbigness
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	x::`&nocut`(expr)
############################################################
`&nocut/width` := proc(x)
    `doctolatex/width`(op(x));
end;	# `&nocut/width`
#>>>########################################################
#<<<########################################################
# @scope=local
############################################################
`line/setbigness` := proc(l,b)
    option inline;
    subsop(_O_BIG=b,l);
end:	# `line/setbigness`
#>>>########################################################
### END FUNCTIONS #############################
### BEGIN RAW INCLUSION #######################
### 11/02/02: @rawinclude is used in practice for
# initializations, which must be done after the
# definitions and declarations.
#<<<########################################################
# @sharevar=`shared/bignessfactor`
# @rawinclude
`shared/bignessfactor` := 1;
#>>>########################################################
#<<<########################################################
### Description: the implementation of the elements of the
# DOC format (see TranslateMath.mpl and TranslateText.mpl)
# uses standard LaTeX commands, whose actual display is set
# by a LaTeX style. This procedure allows to set a style
# for the document(s) being exported.
############################################################
# @exportvar=DocStyle
# @sharevar=`style/documentclass`
# @sharevar=`style/documentoptions`
# @sharevar=`style/documentheader`
# @sharevar=`style/plotwidth`
# @sharevar=`style/captionwidth`
# @rawinclude
### Default values
`style/documentclass` := "amsbook";
`style/documentoptions` := "";# twocolumn
`style/documentheader` := "";# ADDITIONAL header
`style/plotwidth` := 'default';# LaTeX unit or 'default'
`style/captionwidth` := "4cm";# LaTeX unit

DocStyle := CommonLib:-MakeProcOptions({
    'documentclass'=['string','`style/documentclass`'],
    'documentoptions'=['string','`style/documentoptions`'],
    # STATIC CONSTANT HEADER
    'documentheader'=['string','`style/documentheader`'],
    'plotwidth'=[{'identical'('default'),'string'},'`style/plotwidth`'],
    'captionwidth'=['string','`style/captionwidth`']
    });
#>>>########################################################
#<<<########################################################
### Description: initialization and declaration
# @rawinclude
# @sharevar=`float/notrailingzeros`
# @sharevar=`float/negexp`
# @sharevar=`float/posexp`
`float/notrailingzeros` := _DEF_NOTRZEROS;	# user
`float/negexp` := _DEF_NEGEXP;		# user
`float/posexp` := _DEF_POSEXP;		# user
#>>>########################################################
#<<<########################################################
### Description: initialization and declaration
# @rawinclude
# @sharevar=`integer/leadingzeros`
# @sharevar=`integer/mindigits`
# @sharevar=`integer/forcesameline`
`integer/leadingzeros` := 0;		# internal, def value
`integer/mindigits` := _DEF_MINDIGITS;	# user
`integer/forcesameline` := false;	# internal, def value
#>>>########################################################
#<<<########################################################
# @rawinclude
# @sharevar=symboltable		(MUST BE THE SAME AS IN macros.mi)
### Description: this file gathers all LaTeX symbols that
# are needed by the kernel. Other symbols are defined by
# 'DefineMathSymbol'.
# Indices of the table are names, while entries are lists
# (see macros.mi, _SYM_*). The first element of the list is
# the LaTeX string, the second element is its width (float)
# and the third optional element is its bigness (float).
### Note: the LaTeX strings should be a complete LaTeX constructs,
# since the automatic line-breaking mechanism would insert
# a line-break (containing a "\n") that would probably give
# rise to a LaTeX compilation error.
### Related file: moduleinit.mpl
### History:
# 03/06/03: initialization done in the body of the module
# (see below).
############################################################
#`symboltable/init` := proc()
#    _SYM_TBL := table();# may be needed for reinitialization

### List of used symbols:
# in knl.integer.mpl: '`&nothing`', 'leadingminus', 'linebreak', 'indent'
# in knl.symbol.mpl: 'linebreak', 'indent'
# in knl.fraction.mpl: 'leadingminus', 'linebreak', 'indent'
# in knl.float.mpl: 'float0', 'floatcomma'
# in indent.mpl: 'alignmark', 'lineindent'

    ### Special symbols
    _SYM_TBL['`&nothing`']		:= ["",0];

    ### Line related
    _SYM_TBL['linebreak'] 		:= [" \\\\\n",0];
    _SYM_TBL['defaultlineindent'] 	:= ["\\quad{}",2];
    _SYM_TBL['lineindent'] 		:= _SYM_TBL['defaultlineindent'];	# variable
    _SYM_TBL['alignmark']		:= ["& ",0];# DON'T REMOVE THE " "

    ### Float
    _SYM_TBL[cat('`float0/`',_LOCALE_FR)]		:= ["0,",2];
    _SYM_TBL[cat('`float0/`',_LOCALE_US)]		:= ["0.",2];
    # _SYM_TBL['float0']: see locale.mpl
    _SYM_TBL[cat('`floatcomma/`',_LOCALE_FR)]	:= [",",1];
    _SYM_TBL[cat('`floatcomma/`',_LOCALE_US)]	:= [".",1];
    #_SYM_TBL['floatcomma']: see locale.mpl

    ### Math operators
    _SYM_TBL['leadingminus']	:= ["-",1];

#end;	# `symboltable/init`
#>>>########################################################
#<<<########################################################
# @rawinclude
### Description: library of symbols that are not needed by
# the kernel (see knl.symboltable.mpl).
### History:
# 03/06/03: initialization done in the body of the module;
# the declaration of _SYM_TBL is done in knl.symboltable.mpl
############################################################
#`lib/symbol` := proc()

    ### Math operators
    _SYM_TBL['minusop']		:= ["-",1.5];
    _SYM_TBL['plusop']		:= ["+",1.5];
    _SYM_TBL['timesop']		:= [" ",0.5];
    _SYM_TBL['divideop']	:= ["/",1.5];	# variable size
    _SYM_TBL['exponentop']	:= ["\\Hat{ }",1.5];

    ### Delimiters
    _SYM_TBL['leftbracket']	:= ["[",1];
    _SYM_TBL['rightbracket']	:= ["]",1];
    _SYM_TBL['leftparens']	:= ["(",1];
    _SYM_TBL['rightparens']	:= [")",1];
    _SYM_TBL['leftbrace']	:= ["\\{",1];
    _SYM_TBL['rightbrace']	:= ["\\}",1];

    ### Relations
    _SYM_TBL['`=`']		:= ["=",1];
    _SYM_TBL['`<`']		:= ["<",1];
    _SYM_TBL['`>`']		:= [">",1];
    _SYM_TBL['`<=`']		:= ["\\leq",1];
    _SYM_TBL['`>=`']		:= ["\\geq",1];
    _SYM_TBL['`<>`']		:= ["\\neq",1];

    ### Punctuation
    _SYM_TBL['comma']		:= [",",1];
    _SYM_TBL['point']		:= [".",1];
    _SYM_TBL['semicolon']	:= [";",1];

    ### Spacers
    _SYM_TBL['thinspace']	:= ["\\,",0.5];
    _SYM_TBL['medspace']	:= ["\\:",0.8];
    _SYM_TBL['thickspace']	:= ["\\;",1];
    _SYM_TBL['nullspace']	:= [" ",0];

    ### Misc. symbols
    _SYM_TBL['imaginaryunit']	:= ["i",1];
    _SYM_TBL['Pi']		:= ["\\pi",1];
    _SYM_TBL['infinity']	:= ["\\infty",1];
    _SYM_TBL['partial']		:= ["\\partial",1];
    _SYM_TBL['setminus']	:= ["\\setminus",1];
    _SYM_TBL['approx']		:= ["\\approx",1];
    _SYM_TBL['propto']		:= ["\\propto",1];
    _SYM_TBL['notin']		:= ["\\not\\in",1];
    _SYM_TBL['ldots']		:= ["\\ldots",2];
    _SYM_TBL['cdots']		:= ["\\cdots",2];
    _SYM_TBL['Sum']		:= ["\\sum",3];
    _SYM_TBL['Int']		:= ["\\int",2];
    _SYM_TBL['prime']		:= ["\\prime",1];
	
    ### Lower-case greek
    _SYM_TBL['alpha']		:= ["\\alpha",1];
    _SYM_TBL['beta']		:= ["\\beta",1];
    _SYM_TBL['gamma']		:= ["\\gamma",1];	
    _SYM_TBL['delta']		:= ["\\delta",1];
    _SYM_TBL['epsilon']		:= ["\\epsilon",1];
    _SYM_TBL['zeta']		:= ["\\zeta",1];
    _SYM_TBL['eta']		:= ["\\eta",1];
    _SYM_TBL['theta']		:= ["\\theta",1];
    _SYM_TBL['iota']		:= ["\\iota",1];
    _SYM_TBL['kappa']		:= ["\\kappa",1];
    _SYM_TBL['lambda']		:= ["\\lambda",1];
    _SYM_TBL['mu']		:= ["\\mu",1];
    _SYM_TBL['nu']		:= ["\\nu",1];
    _SYM_TBL['xi']		:= ["\\xi",1];
    _SYM_TBL['omicron']		:= ["\\omicron",1];
    _SYM_TBL['pi']		:= ["\\pi",1];
    _SYM_TBL['rho']		:= ["\\rho",1];
    _SYM_TBL['sigma']		:= ["\\sigma",1];
    _SYM_TBL['tau']		:= ["\\tau",1];
    _SYM_TBL['upsilon']		:= ["\\upsilon",1];
    _SYM_TBL['phi']		:= ["\\phi",1];
    _SYM_TBL['chi']		:= ["\\chi",1];
    _SYM_TBL['psi']		:= ["\\psi",1];
    _SYM_TBL['omega']		:= ["\\omega",1];

    ### Upper-case greek
    _SYM_TBL['Alpha']		:= ["\\Alpha",1];
    _SYM_TBL['Beta']		:= ["\\Beta",1];
    _SYM_TBL['Gamma']		:= ["\\Gamma",1];
    _SYM_TBL['GAMMA']		:= ["\\Gamma",1];
    _SYM_TBL['Delta']		:= ["\\Delta",1];
    _SYM_TBL['Epsilon']		:= ["\\Epsilon",1];
    _SYM_TBL['ZETA']		:= ["\\Zeta",1];
    _SYM_TBL['Eta']		:= ["\\Eta",1];
    _SYM_TBL['Theta']		:= ["\\Theta",1];
    _SYM_TBL['Iota']		:= ["\\Iota",1];
    _SYM_TBL['Kappa']		:= ["\\Kappa",1];
    _SYM_TBL['Lambda']		:= ["\\Lambda",1];
    _SYM_TBL['Mu']		:= ["\\Mu",1];
    _SYM_TBL['Nu']		:= ["\\Nu",1];
    _SYM_TBL['Xi']		:= ["\\Xi",1];
    _SYM_TBL['Omicron']		:= ["\\Omicron",1];
    _SYM_TBL['PI']		:= ["\\Pi",1];
    _SYM_TBL['Rho']		:= ["\\Rho",1];
    _SYM_TBL['Sigma']		:= ["\\Sigma",1];
    _SYM_TBL['Tau']		:= ["\\Tau",1];
    _SYM_TBL['Upsilon']		:= ["\\Upsilon",1];
    _SYM_TBL['Phi']		:= ["\\Phi",1];
    _SYM_TBL['CHI']		:= ["\\Chi",1];
    _SYM_TBL['Psi']		:= ["\\Psi",1];
    _SYM_TBL['Omega']		:= ["\\Omega",1];
    
#end;	# `lib/symbol`
#>>>########################################################
#<<<########################################################
# @rawinclude
# @exportvar=TextSymbol
### Description: text-mode symbols
############################################################
TextSymbol[`&sect`] := "\\S";
TextSymbol[`&copyright`] := "\\copyright";
TextSymbol[`&sp`] := " ";
#>>>########################################################
#<<<########################################################
### Description: initialization and declaration
# @rawinclude
# @sharevar=`line/width`
# @sharevar=`shared/cutbyobject`
`line/width` := _DEF_WIDTH;
`shared/cutbyobject` := _DEF_CUTBYOBJ;
#>>>########################################################
#<<<########################################################
# @rawinclude
### Description: initialization of _MATHENV_TBL, see also
# 'DefineMathKeyword'.
### History:
# 03/06/03: initialization done in the body of the module
############################################################

    _MATHENV_TBL := table();# see knl.symboltable.mpl
    
    _MATHENV_TBL[`&mathseq`] := proc(x) `&sepsequence`('nullspace',op(x)); end;
    _MATHENV_TBL[`&relation`] := proc(x) `&mathseq`(op(2,x),op(1,x),op(3,x)) end;
    
    # styles
    _MATHENV_TBL[`&mathbb`] := proc(x) `&mathenv`("\\mathbb",op(x)); end;
    _MATHENV_TBL[`&mathcal`] := proc(x) `&mathenv`("\\mathcal",op(x)); end;
    _MATHENV_TBL[`&operatorname`] := proc(x) `&mathenv`("\\operatorname",op(x)); end; 
   
#>>>########################################################
#<<<########################################################
# @exportvar=FileExtension
# @rawinclude
FileExtension := "tex";
#>>>########################################################
#<<<########################################################
# @exportvar=DerivedFormat
# @rawinclude
### Description: the translator 'LaTeX' outputs .tex files.
# These files may be post-processed by some OS commands,
# which are declared in this table. Note that %F stands for
# the .tex file with its extension, this is why one may
# use 'basename %F .tex' to get rid of it.
DerivedFormat['dvi'] := "latex %F";
DerivedFormat['pdf'] := "pdflatex %F";
DerivedFormat['ps'] := "latex %F;dvips $(basename %F .tex).dvi -o";
#>>>########################################################
#<<<########################################################
# @rawinclude
# @exportvar=TranslateText
# @sharevar=`fontsize/table`
### Description: initialization of the table 'TranslateText'
# that associates a keyword to its implementation.
############################################################

############################################################
### PRIMITIVES FOR FILE

# &fileheader()
TranslateText[`&fileheader`] := proc()
    `&string`();
end;

# &filebody(MADseq)
TranslateText[`&filebody`] := proc(x) `&string`(op(x)); end;

# &HEADER(Seq(DOC))
TranslateText[`&HEADER`] := proc(x)
    `&string`(
	"\\documentclass[",`style/documentoptions`,"]{",`style/documentclass`,"}\n",
	### MINIMAL header
	"\\usepackage{amsmath,amsopn}\n",
	"\\usepackage{graphicx}\n",
	"\\usepackage{hyperref,url}\n",
	_LATEX_COMMENTSTR("BEGIN SPECIAL COMMANDS"),
	"\\newcommand{\\MADerror}[1]{\\begin{flushleft}\\fbox{\\begin{minipage}{\\textwidth}{\\tt #1}\\end{minipage}}\\end{flushleft}}\n",
	"\\newcommand{\\MADierror}[1]{\\fbox{\\tt #1}}\n",
	_LATEX_COMMENTSTR("END SPECIAL COMMANDS"),
	### ADDITIONAL header
	`&string`(
		_LATEX_COMMENTSTR("BEGIN STATIC HEADER"),
		`style/documentheader`,# DocStyle('documentheader')
		"\n",
		_LATEX_COMMENTSTR("END STATIC HEADER")),
		
	_LATEX_COMMENTSTR("BEGIN DYNAMIC HEADER"),
	op(x),"\n",
	_LATEX_COMMENTSTR("END DYNAMIC HEADER")
	);
end;

# &body(MADseq)
TranslateText[`&body`] := proc(x)
    `&string`("\\begin{document}\n",op(x),"\\end{document}\n");
end;

# &commentline(DOC)
TranslateText[`&commentline`] := proc(x)
    # "%%" is printf-ed and yields "%"
    `&string`("%% ",op(x),"\n");
end;

############################################################
### NUMBERING

# &NUMBERING
TranslateText[`&NUMBERING`] := proc(x)
    # RAW numbering, whithout any formatting
    `&string`(FormatNumbering(op(1,x)));
end;

############################################################
### LOGICAL SECTIONING

# &chapter(label,boolean,MADseq)
TranslateText[`&chapter`] := proc(x)
    local mytag,num;
    #  yield label info
    mytag := DocumentGenerator:-RefInfo(op(1,x),LaTeX,'tag');
    num := DocumentGenerator:-RefInfo(op(1,x),LaTeX,'numbering');
    
    `&string`(
	"\\chapter*{",
	`if`(op(2,x),# numbering displayed
	    `&string`(FormatNumbering(num)," "),
	    NULL),
	op(3,x),"}\n",
	"\\label{",mytag,"}\n",# label used
	op(4..-1,x))
end;

# &section(label,boolean,MADseq)
TranslateText[`&section`] := proc(x)
    local mytag,num,sbeg,nest;
    # yield label info
    mytag := DocumentGenerator:-RefInfo(op(1,x),LaTeX,'tag');
    num := DocumentGenerator:-RefInfo(op(1,x),LaTeX,'numbering');
    
    nest := nops(num[2]);# nesting
    
    if nest = 1 then
	sbeg := "\\section*{";
    elif nest = 2 then 
	sbeg := "\\subsection*{";
    elif nest = 3 then
	sbeg := "\\subsubsection*{";
    elif nest = 4 then
	sbeg := "\\paragraph*{";
    else
	sbeg := "\\subparagraph*{";
    fi;
    `&string`(sbeg,
	`if`(op(2,x),
	    `&string`(FormatNumbering(num)," "),# numbering
	    NULL),
	op(3,x),"}\n",
	"\\label{",mytag,"}\n",# label used
	op(4..-1,x));
end;
    
# &paragraph(MADseq)
TranslateText[`&paragraph`] := proc(x)
    `&string`("\n\n",op(x),"\n");
end;

############################################################
### STYLES

# &em(MADseq)
TranslateText[`&em`] := proc(x)
    `&string`("{\\em ",op(x),"}");
end;

# &tt(MADseq)
TranslateText[`&tt`] := proc(x)
    `&string`("{\\tt ",op(x),"}");
end;

# &bf(MADseq)
TranslateText[`&bf`] := proc(x)
    `&string`("{\\bf ",op(x),"}");
end;

# LaTeX font sizes
# \tiny \scriptsize \footnotesize \small \normalsize \large \Large \LARGE \huge \Huge
`fontsize/table`['xxsmall'] := "\\scriptsize";
`fontsize/table`['xsmall'] := "\\footnotesize";
`fontsize/table`['small'] := "\\small";
`fontsize/table`['medium'] := "\\normalsize";
`fontsize/table`['large'] := "\\Large";
`fontsize/table`['xlarge'] := "\\huge";
`fontsize/table`['xxlarge'] := "\\Huge";

# &fontsize(symbol,MADseq)
TranslateText[`&fontsize`] := proc(x)
    `&string`(" {",`fontsize/table`[op(1,x)]," ",op(2..-1,x),"} ");
end;

# &dq(MADseq)
TranslateText[`&dq`] := proc(x)
    `&string`("\"",op(x),"\"");
end;

############################################################
### ERRORS

# &ierror(MADseq)
TranslateText[`&ierror`] := proc(x)
    `&string`(" \\MADierror{",op(x),"}");
end;

# &error(MADseq)
TranslateText[`&error`] := proc(x)
    `&string`(" \\MADerror{",op(x),"}");
end;

############################################################
### CROSS-REFERENCES

# &linkurl(url::string,MADseq)
TranslateText[`&linkurl`] := proc(x)
    if nops(x) = 1 then
	`&string`("\\href{",op(x),"}{\\url{",op(x),"}}");# \usepackage{hyperref} in &HEADER
    else
	`&string`("\\href{",op(1,x),"}{",op(2..-1,x),"}");# \usepackage{hyperref} in &HEADER
    fi;
end;

# &REF(fd,label)
TranslateText[`&REF`] := proc(x)
    # check label
    if member(LaTeX,DocumentGenerator:-RefInfo(op(2,x))) and
    member('numbering',DocumentGenerator:-RefInfo(op(2,x),LaTeX)) then
	FormatNumbering(DocumentGenerator:-RefInfo(op(2,x),LaTeX,'numbering'));
    else
	`&string`("??");
    fi;
end;

# &LINK(fd,label,MADseq)
TranslateText[`&LINK`] := proc(x)
    local href;
    # check label in HTMX
    if type(HTMX,`DocumentGenerator/Translator`) and member(HTMX,DocumentGenerator:-RefInfo(op(2,x))) then
	href := `&string`(CommonLib:-FileName(CommonLib:-FileExtension(DocumentGenerator:-RefInfo(op(2,x),HTMX,'filedesc'),"html"),DocumentGenerator:-FILEARCH('basename')),"#",DocumentGenerator:-RefInfo(op(2,x),HTMX,'tag'));
	`&string`("\\href{",href,"}{",op(3..-1,x),"}");
    else
	`&string`(op(3..-1,x));
    fi;
end;

# &TOC(fd,depth)
TranslateText[`&TOC`] := proc(x)
    `&string`();
end;

# &SUBTOC(fd,depth,label)
TranslateText[`&SUBTOC`] := proc(x)
    `&string`();
end;

############################################################
### MAPLE OBJECTS (see also TranslateMath.mpl)

# &PLOT(fd,label,PLOT|PLOT3D,MADseq)
TranslateText[`&PLOT`] := proc(x)
    local thisfd,tofd,mytag,num;
    # The generated LaTeX is processed by 'latex' and 'pdflatex';
    # see: http://www.cv.nrao.edu/~abridle/l2h4nrao/node31.shtml
    thisfd := op(1,x);
    tofd := [op(thisfd),CommonLib:-UniqueId()];# go to subdirectory
    
    # Ensure that the directory is created
    CommonLib:-FileNew(tofd);
    CommonLib:-FileClose(tofd);
    CommonLib:-FileRemove(tofd);

    plotsetup('ps','plotoutput'=CommonLib:-FileName(
	CommonLib:-FileExtension(tofd,"ps")),
	'plotoptions'="color,portrait,noborder");
    print(op(3,x));# latex

    plotsetup('jpeg','plotoutput'=CommonLib:-FileName(
	CommonLib:-FileExtension(tofd,"jpg")));
    print(op(3,x));# pdflatex

    if op(2,x) = 'VOID' then
	mytag := NULL;
	num := NULL;
    else
	mytag := "\\label{",DocumentGenerator:-RefInfo(op(2,x),LaTeX,'tag'),"}\n";
	num := FormatNumbering(DocumentGenerator:-RefInfo(op(2,x),LaTeX,'numbering'))," ";
    fi;

    `&string`("\\begin{center}\n",mytag,
	"\\includegraphics",`if`(`style/plotwidth`='default',NULL,
	    `&string`("[width=",`style/plotwidth`,"]")),"{",
	# no extension
	CommonLib:-RelativePath(thisfd,tofd),"}\n",
	
	# \caption always provide numbering: so use \parbox
	`if`(nops(x)> 3 or num <> NULL,# caption
	    `&string`("\\parbox{",`style/captionwidth`,"}{",num,
	    `if`(nops(x)>3,op(4..-1,x),NULL),"}\n"),NULL),
	"\\end{center}\n");
end;
#>>>########################################################
### END RAW INCLUSION #########################

end:
### END MODULE #################################
#SAVELIBNAME
#savelib('`type/LaTeX/atomic`','LaTeX'):
