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

# HTMX - a Maple library for generating HTML
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
############################################################
### Package level description
############################################################
### The package 'HTMX' implements a translator from the DOC
# format to HTMX. HTMX is a format that mixes HTML and LaTeX
# (http://www.math.uic.edu/~fields/htmx). The math mode is
# deferred to the package 'MADLaTeX'; options for the math mode
# ('MathStyle') can be set independently of 'MADLaTeX', however 
# the table of MADLaTeX symbols is shared across both packages.
# The text mode produces HTML4.0; see html.macros.mpl.
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

### HTML
$define _HTML_FRAGMENT	"#"

#<<<###########################################
### @exit
### Generated: 04/12/2007 17:17:56
### Module name: HTMX
### Version: 1.106
#>>>###########################################

#<<<########################################################
# @macro
$define _CSS_TBL	`HTMX/CSSTable`
#>>>########################################################
#<<<########################################################
# @macro
$define _RGB_RED		"#FF0000"	# 255 0 0
$define _RGB_GRAY		"#CCCCCC"
$define _RGB_GRAY_LIGHT		"#F0F0F0"
$define _RGB_BLUE		"#3333FF"	# 51 51 255
$define _RGB_BLUE_LIGHT		"#DDDDFF"
$define _RGB_GREEN		"#339933"	# 51 153 51
$define _RGB_ORANGE		"#FF3300"	# 255 51 0
$define _RGB_ORANGE_LIGHT	"#FF2E2B"	# 255 226 178
$define _RGB_YELLOW		"#FFCC33"	# 255 204 51
$define _RGB_BLACK		"#000000"	# 0 0 0
$define _RGB_WHITE		"#FFFFFF"	# 255 255 255
$define _RGB_YELLOW_FLASHY	"#FFFF00"
#>>>########################################################
#<<<########################################################
# @macro
### Note: Maple's preprocessor expands macros in strings,
# which MAY prevent from casting the arguments into strings.
$define _TOSTR(X)	convert(X,'string')
### META
$define _HTTP_META(N,C)	`&sequence`("<meta http-equiv=\"",N,"\" content=\"",C,"\">\n")
$define _HTTP_METASTR(N,C)	`&string`("<meta http-equiv=\"",N,"\" content=\"",C,"\">\n")
$define _HTML_META(N,C)	`&sequence`("<meta name=\"",N,"\" content=\"",C,"\">\n")
$define _HTML_METASTR(N,C)	`&string`("<meta name=\"",N,"\" content=\"",C,"\">\n")
### Rectangles with ABSOLUTE positioning (no part of the text flow)
# T:top; B:bottom; L:left; R:right; W:width; H:height; C:color; U:url; X:anything; L:css class
$define _HTML_RECT_LEFT(T,L,W,H,C)	`&string`("<div style=\"position:absolute;font-size:0pt;top:",_TOSTR(T),"px;left:",_TOSTR(L),"px;width:",_TOSTR(W),"px;height:",_TOSTR(H),"px;background-color:",_TOSTR(C),";\"></div>\n")
$define _HTML_RECT_RIGHT(T,R,W,H,C)	`&string`("<div style=\"position:absolute;font-size:0pt;top:",_TOSTR(T),"px;right:",_TOSTR(R),"px;width:",_TOSTR(W),"px;height:",_TOSTR(H),"px;background-color:",_TOSTR(C),";\"></div>\n")
$define _HTML_IMAGE_LEFT(T,L,W,H,U)	`&sequence`("<div style=\"position:absolute;font-size:0pt;top:",_TOSTR(T),"px;left:",_TOSTR(L),"px;width:",_TOSTR(W),"px;height:",_TOSTR(H),"px;background-image:url(",U,");\"></div>\n")
$define _HTML_IMAGE_RIGHT(T,R,W,H,U)	`&sequence`("<div style=\"position:absolute;font-size:0pt;top:",_TOSTR(T),"px;right:",_TOSTR(R),"px;width:",_TOSTR(W),"px;height:",_TOSTR(H),"px;background-image:url(",U,");\"></div>\n")
$define _HTML_BOX_LEFT(T,L,W,X)	`&sequence`("<div style=\"position:absolute;top:",_TOSTR(T),"px;left:",_TOSTR(L),"px;width:",_TOSTR(W),"px\">\n",X,"</div>\n")
### Rectangles with RELATIVE positioning (part of the text flow)
$define _HTML_VSKIP(H)	`&string`("<div style=\"position:relative;height:",_TOSTR(H),"px\">&nbsp;</div>\n")
### Styles
$define _HTML_DIV(L,X)		`&sequence`("<div class=\"",L,"\">",X,"</div>")
$define _HTML_CLASS(L,X)	`&sequence`("<span class=\"",L,"\">",X,"</span>")
$define _HTML_CLASSSTR(L,X)	`&string`("<span class=\"",L,"\">",X,"</span>")
$define _HTML_LINK(L,U,X)	`&sequence`("<a class=\"",L,"\" href=\"",U,"\">",X,"</a>")
$define _HTML_LINKSTR(L,U,X)	`&string`("<a class=\"",L,"\" href=\"",U,"\">",X,"</a>")
$define _HTML_TAG(X)		`&sequence`("<a name=\"",X,"\"></a>")
$define _HTML_TAGSTR(X)		`&string`("<a name=\"",X,"\"></a>")
$define _HTML_COMMENT(X)	`&sequence`("<!-- ",X," -->\n")
$define _HTML_COMMENTSTR(X)	`&string`("<!-- ",X," -->\n")
$define _DIVBEG(L)		`&string`("<div class=\"",L,"\">\n")
$define _DIVEND			"</div>\n"
$define _SPANBEG(L)		`&string`("<span class=\"",L,"\">")
$define _SPANEND		"</span>"
$define _NAMEDBEG(X)		`&string`("<a name=\"",X,"\">")
$define _NAMEDEND		"</a>"
$define _HTML_TITLE(X)		`&sequence`("<h1 class=\"chapter\">",X,"</h1>")
### Misc. symbols
$define _HTML_BR		"<br>\n"
$define _HTML_SP		"&nbsp;"
#>>>########################################################
### BEGIN MODULE ##############################
HTMX := module()
	option `Copyright (c) 2001-2007 INRIA. All rights reserved.`,package, load=moduleinit;
### BEGIN DECLARATION #########################
local
	formattoc,
	moduleinit,
	mylatexmathstyle,
	`HTMX/CSSTable`,
	`fontsize/table`,
	`relatives/doit`,
	`style/bodyfooter`,
	`style/bodyheader`,
	`style/csstable`,
	`style/documentheader`,
	`style/documenttitle`,
	`style/equationnumbering`,
	`style/plotnumbering`,
	`style/plotwidth`,
	`style/relatives`,
	`style/toccssbasename`,
	`style/tocindent`,
	`style/tocmaxindent`,
	`style/tocnumbering`,
	`style/toctitle`;
export
	DefineMathKeyword,
	DefineMathSymbol,
	DerivedFormat,
	DocStyle,
	FileExtension,
	FormatNumbering,
	HTMLEncode,
	MathStyle,
	TextSymbol,
	TranslateMath,
	TranslateText,
	UndefineMathKeyword,
	UndefineMathSymbol,
	VERSION;
### END DECLARATION ###########################

### BEGIN FUNCTIONS ###########################
#<<<########################################################
# @scope=export
### Argument sequence:
#	x::`DocumentGenerator/Numbering`
############################################################
FormatNumbering := proc(x)
    _OPTION;
    MADLaTeX:-FormatNumbering(x);
end;	# FormatNumbering
#>>>########################################################
#<<<########################################################
### Description: relatives of a label to string
`relatives/doit` := proc(label)

$define _LEFT_REL_SEP	""
$define _RIGHT_REL_SEP	""

    local fd,rel,i,lbl,tbl,top,bottom;
    fd := DocumentGenerator:-RefInfo(label,HTMX,'filedesc');
    rel := [['_up',"up"],['_back',"back"],['_next',"next"],['_down',"into"]];
    for i in rel do
    
	lbl := DocumentGenerator:-RelativeRef(label,i[1]);
	
	if lbl = NULL or not member(DocumentGenerator:-RefInfo(lbl,HTMX,'type'),DocumentGenerator:-DocStyle('relativestypeinclude')) then
	    tbl[i] := `&string`(_LEFT_REL_SEP,_HTML_CLASSSTR("relatives-dimmed",i[2]),_RIGHT_REL_SEP);
	else
	    tbl[i] := `&string`(_LEFT_REL_SEP,_HTML_LINKSTR("relatives",cat(CommonLib:-RelativePath(fd,CommonLib:-FileExtension(DocumentGenerator:-RefInfo(lbl,HTMX,'filedesc'),"html")),"#",DocumentGenerator:-RefInfo(lbl,HTMX,'tag')),i[2]),_RIGHT_REL_SEP);
	fi;
    od;
    # adding top and bottom: see &body
    top := `&string`(_LEFT_REL_SEP,_HTML_LINKSTR("relatives",cat(CommonLib:-RelativePath(fd,CommonLib:-FileExtension(fd,"html")),"#top"),"top"),_RIGHT_REL_SEP);
    bottom := `&string`(_LEFT_REL_SEP,_HTML_LINKSTR("relatives",cat(CommonLib:-RelativePath(fd,CommonLib:-FileExtension(fd,"html")),"#bottom"),"bottom"),_RIGHT_REL_SEP);

    `&string`(_SPANBEG("relatives"),CommonLib:-SepSeq(_HTML_SP,top,seq(tbl[i],i=rel),bottom),_SPANEND);
    
$undef _LEFT_REL_SEP
$undef _RIGHT_REL_SEP    
end;
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: encode HTML (ISO8859-1)
### Argument sequence:
#	x::string
### Output format::string
############################################################
HTMLEncode := proc(x)
    local iso,str,i;
    
    ### SUBSTITUTE & FIRST
    str := StringTools:-SubstituteAll(x,"&","&#38;");
    
    # ISO 8859-1 table
    iso["\""] := "&#35;";
    iso["<"] := "&#60;";
    iso[">"] := "&#62;";

    for i in map(op,[indices(iso)]) do
	str := StringTools:-SubstituteAll(str,i,iso[i]);
    od;
    str;
end;	# HTMLEncode
#>>>########################################################
#<<<########################################################
### Description: format table of contents
# @export
formattoc := proc(thisfd,depth,label)
    local toc,i,tbl,num,fd,tg,nest,diffnest,tmp,indent,minnest,css,cssnum,csstit;
    toc := DocumentGenerator:-RawTableOfContents(args[2..-1]);
    
    if `style/tocindent` then
	for i in toc do
	    if member(DocumentGenerator:-RefInfo(i,HTMX,'type'),DocumentGenerator:-DocStyle('toctypeinclude')) then
		nest[i] := nops(map(op,DocumentGenerator:-RefInfo(i,HTMX,'numbering')));
	    else nest[i] := NULL; fi;
	od;
	tmp := seq(nest[i],i=toc);
	minnest := min(tmp);
	diffnest := max(tmp)-minnest;

	indent := proc(X)
	    if diffnest = 0 or nest[X] = minnest or `style/tocmaxindent`=0 then NULL;
	    else
		`&string`("<td style=\"width:",
		_TOSTR(trunc(evalf(`style/tocmaxindent`*(nest[i]-minnest)/diffnest))),
		"%%\">",_HTML_SP,"</td>");
	    fi;
	end;
	
    else
	indent := proc() NULL; end;
    fi;
    
    # css class names
    css := `style/toccssbasename`;
    cssnum := cat(css,"-numbering");
    csstit := cat(css,"-title");

    for i in toc do
	if member(DocumentGenerator:-RefInfo(i,HTMX,'type'),DocumentGenerator:-DocStyle('toctypeinclude')) then
	    num := DocumentGenerator:-RefInfo(i,HTMX,'numbering');
	    fd := DocumentGenerator:-RefInfo(i,HTMX,'filedesc');
	    tg := DocumentGenerator:-RefInfo(i,HTMX,'tag');

	    tbl[i] := `&string`("<tr><td>\n<table cellspacing=\"0\" cellpadding=\"0\" style=\"width:100%%\">\n",
		"<tr>",indent(i),"<td>",
		`if`(`style/tocnumbering`,`&string`(_SPANBEG(cssnum),FormatNumbering(num),_SPANEND,_HTML_SP),NULL),
		_HTML_LINKSTR(css,cat(CommonLib:-RelativePath(thisfd,CommonLib:-FileExtension(fd,"html")),"#",tg),DocumentGenerator:-RefInfo(i,HTMX,'entry')),
		"</td></tr></table></td></tr>\n");

	else tbl[i] := NULL; fi;
    od;
    `&string`(
	_DIVBEG(css),
	`if`(`style/toctitle`="",NULL,`&string`(
	    "<div class=\"",csstit,"\">",`style/toctitle`,"</div>")),
	
	"<table cellspacing=\"0\" cellpadding=\"0\" style=\"width:100%%\" class=\"",css,"\">\n",
	seq(tbl[i],i=toc),
	"</table>\n",_DIVEND);
end;	# formattoc
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: translate DOC math mode AND switches to
# math mode into HTMX.
### Argument sequence:
#	x::DOC
############################################################
TranslateMath := proc(x)
    local beganc,endanc,num,begblk,endblk,y;
    _OPTION;
    
    ### &equation(label|VOID,separator,MADseq)
    if op(0,x) = `&equation` then
    
	# tag; the anchor surrounds the image only
	if op(1,x) = 'VOID' then
	    beganc := NULL;
	    endanc := NULL;
	else# label
	    beganc := "<a name=\"",DocumentGenerator:-RefInfo(op(1,x),HTMX,'tag'),"\">";
	    endanc := "</a>";
	fi;
	
	# HTML block
	# this 100% ensures that the available width is spanned
	begblk := "<table style=\"width:100%%\">\n",
	    "<tr>\n";

	# numbering
	if op(1,x) = 'VOID' or `style/equationnumbering` = 'none' then
	    # no numbering
	    num := NULL;
	    begblk := begblk, "<td class=\"equation\">\n";# CSS .equation
	    endblk := "</td>\n</tr>\n</table>\n";
	else
	    num := DocumentGenerator:-RefInfo(op(1,x),HTMX,'numbering');	
	    num := "<td>\n",
		"<span class=\"equation-numbering\">",
		FormatNumbering(num),
		"</span>\n</td>\n";
	    begblk := begblk,
		`if`(`style/equationnumbering`='left',num,NULL),
		# The 100% ensures that the numbering is flushed left or right.
		"<td class=\"equation\" style=\"width:100%%\">\n";# CSS .equation
	    endblk := "</td>\n",`if`(`style/equationnumbering`='right',num,NULL),
		"</tr>\n</table>\n";
	fi;
	# remove label for MADLaTeX
	y := subsop(1='VOID',x);
	
	_OUT_STRING(begblk,beganc,"<latex class=\"equation\">",CommonLib:-WithLocalOptions(mylatexmathstyle,
	    MADLaTeX:-MathStyle,MADLaTeX:-TranslateMath,y),"</latex>",endanc,endblk);

    else### &imath(MADseq)
	_OUT_STRING("<latex class=\"equation-inline\">",CommonLib:-WithLocalOptions(mylatexmathstyle,
	    MADLaTeX:-MathStyle,MADLaTeX:-TranslateMath,x),"</latex>");
    fi;
end;	# TranslateMath
#>>>########################################################
#<<<########################################################
# @scope=local
# @load
### Decription: initialization
############################################################
moduleinit := proc()
    unprotect('TextSymbol');
    unprotect('TranslateText');
    unprotect('DerivedFormat');
end:	# moduleinit
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: the function 'HTMX:-TranslateMath' is
# implemented on top of 'MADLaTeX:-TranslateMath'. In particular,
# when the options are set trough 'MADLaTeX:-MathStyle', they also
# apply for 'HTMX:-TranslateMath'. Thus, the following procedure
# allows to set options for both packages independently.
############################################################
MathStyle := proc()
    _OPTION;
    # 'mylatexmathstyle' MUST passed by NAME (assignment)
    CommonLib:-WithLocalOptions('mylatexmathstyle',
	MADLaTeX:-MathStyle,MADLaTeX:-MathStyle,args);
end;	# MathStyle
#>>>########################################################
### END FUNCTIONS #############################
### BEGIN RAW INCLUSION #######################
### 11/02/02: @rawinclude is used in practice for
# initializations, which must be done after the
# definitions and declarations.
#<<<########################################################
# @rawinclude
# @exportvar=DefineMathSymbol
# @exportvar=UndefineMathSymbol
# @exportvar=DefineMathKeyword
# @exportvar=UndefineMathKeyword
DefineMathSymbol := MADLaTeX:-DefineMathSymbol;
UndefineMathSymbol := MADLaTeX:-UndefineMathSymbol;
DefineMathKeyword := MADLaTeX:-DefineMathKeyword;
UndefineMathKeyword := MADLaTeX:-UndefineMathKeyword;
#>>>########################################################
#<<<########################################################
# @sharevar=`HTMX/CSSTable`::table[string]::{string,VOID}
# @rawinclude
### Description: the implementation of the elements of the
# DOC format (see TranslateMath.mpl and TranslateText.mpl)
# uses standard HTML tags with CSS instructions. The style
# of a HTML document is thus (mainly) set by CSS definitions.
# CSS is passed as a table to 'DocStyle', which is used to
# override the defaults that are provided by _CSS_TBL.
# Considered as parameters, the entries of _CSS_TBL are not
# parsed. Dynamic CSS can be done trough &rawheader.
# Entries of the CSS table can also be set of VOID, in which
# case they will not be into the header.
############################################################
_CSS_TBL["body"] := "";# body

_CSS_TBL["div.main"] := "margin: 1em 3em 3em 3em";# top,right,bottom,left

_CSS_TBL["div.chapter"] := "";
_CSS_TBL["h1.chapter"] := "text-align:center;";
_CSS_TBL["span.chapter-numbering"] := "";

_CSS_TBL["div.section1"] := "margin: 0em 1em";# top and bottom,left and right
_CSS_TBL["div.section2"] := "margin: 0em 1em";# top and bottom,left and right
_CSS_TBL["div.section3"] := "margin: 0em 1em";# top and bottom,left and right
_CSS_TBL["div.section4"] := "margin: 0em 1em";# top and bottom,left and right
_CSS_TBL["div.section5"] := "margin: 0em 1em";# top and bottom,left and right
_CSS_TBL["div.section6"] := "margin: 0em 1em";# top and bottom,left and right
_CSS_TBL["span.section1-numbering"] := "";
_CSS_TBL["span.section2-numbering"] := "";
_CSS_TBL["span.section3-numbering"] := "";
_CSS_TBL["span.section4-numbering"] := "";
_CSS_TBL["span.section5-numbering"] := "";
_CSS_TBL["span.section6-numbering"] := "";
_CSS_TBL["h1.section1"] := "";
_CSS_TBL["h2.section2"] := "";
_CSS_TBL["h3.section3"] := "";
_CSS_TBL["h4.section4"] := "";
_CSS_TBL["h5.section5"] := "";
_CSS_TBL["h6.section6"] := "";

_CSS_TBL["p.paragraph"] := "margin: 0.5em 1em";# top and bottom,left and right

_CSS_TBL["span.emphasize"] := "font-style:italic";
_CSS_TBL["span.typewriter"] := "font-family:monospace";
_CSS_TBL["span.bold"] := "font-weight:bold";

_CSS_TBL["a.error"] := cat("background-color:",_RGB_YELLOW_FLASHY,";font-family:monospace;color:",_RGB_BLACK,";");
_CSS_TBL["span.error-inline"] := _CSS_TBL["a.error"];
_CSS_TBL["div.error-block"] := cat(_CSS_TBL["a.error"],"padding:5px");

_CSS_TBL["span.relatives"] := cat("color:",_RGB_BLACK,";font-size:small");
_CSS_TBL["span.relatives-dimmed"] := cat("color:",_RGB_BLUE_LIGHT,";");
_CSS_TBL["a.relatives"] := cat("color:",_RGB_BLUE,";");
_CSS_TBL["a.relatives:hover"] := cat("background-color:",_RGB_BLUE_LIGHT,";");

_CSS_TBL["img.equation"] := "border-width:0px";
_CSS_TBL["td.equation"] := "text-align:center";
_CSS_TBL["img.equation-inline"] := "vertical-align:middle;border-width:0px";
_CSS_TBL["span.equation-numbering"] := "";

_CSS_TBL["img.plot"] := "border-width:0px";
_CSS_TBL["td.plot"] := "text-align:center";
_CSS_TBL["div.plot-caption"] := "";
_CSS_TBL["span.plot-numbering"] := "";

_CSS_TBL["div.tableofcontents"] := cat("border-width:1px;border-color:",_RGB_BLACK,";border-style:solid;background-color:",_RGB_GRAY_LIGHT,";");
_CSS_TBL["div.tableofcontents-title"] := "font-weight:bold;margin-top:1em;margin-left:1em";
_CSS_TBL["table.tableofcontents"] := "margin:1em";
_CSS_TBL["span.tableofcontents-numbering"] := "margin:0";
_CSS_TBL["a.tableofcontents"] := cat("margin:0;color:",_RGB_BLACK,";");
_CSS_TBL["a.tableofcontents:hover"] := cat("margin:0;background-color:",_RGB_GRAY,";");

_CSS_TBL["a.url"] := "";
_CSS_TBL["a.url:hover"] := "";
_CSS_TBL["a.link"] := "";
_CSS_TBL["a.link:hover"] := "";

#>>>########################################################
#<<<########################################################
### Description: set and read options for the text mode.
# @exportvar=DocStyle
# @sharevar=`style/documentheader`::string
# @sharevar=`style/equationnumbering`::{'left','right','none'}
# @sharevar=`style/bodyheader`::boolean
# @sharevar=`style/bodyfooter`::boolean
# @sharevar=`style/plotwidth`::string || 'default'
# @sharevar=`style/csstable`::table
# @sharevar=`style/documenttitle`::string
# @sharevar=`style/relatives`::nonnegint || infinity
# @sharevar=`style/plotnumbering`::{'left','right','none'}
# @sharevar=`style/tocnumbering`::boolean
# @sharevar=`style/tocindent`::boolean
# @sharevar=`style/tocmaxindent`::integer
# @sharevar=`style/toccssbasename`::string
# @sharevar=`style/toctitle`::string
# @rawinclude
### Default values
`style/documentheader` := "";# see TranslateText[&HEADER]
`style/documenttitle` := "";# see TranslateText[&HEADER]
`style/bodyheader` := false;# see TranslateText[&body]
`style/bodyfooter` := false;# see TranslateText[&body]
`style/csstable` := _CSS_TBL;# see TranslateText[&HEADER]
`style/relatives` := 'infinity';# see TranslateText[&chapter] and TranslateText[&section]
`style/equationnumbering` := 'right';# see TranslateMath
`style/plotnumbering` := 'right';# see TranslateText[&PLOT]
`style/plotwidth` := 'default';# "300px": HTML unit OR the symbol 'default'; see TranslateText[&PLOT]
`style/tocnumbering` := true;# see 'formattoc' (in TranslateText.mpl)
`style/tocindent` := true;# see 'formattoc' (in TranslateText.mpl)
`style/tocmaxindent` := 20;# integer (percent)
`style/toccssbasename` := "tableofcontents";# base for css class
`style/toctitle` := "Contents";# string

DocStyle := CommonLib:-MakeProcOptions({
    # STATIC CONSTANT HEADER (not parsed)
    'documentheader'=['string','`style/documentheader`'],
    'documenttitle'=['string','`style/documenttitle`'],
    'equationnumbering'=[proc(x) member(x,{'left','right','none'}); end,'`style/equationnumbering`'],
    'bodyheader'=['boolean','`style/bodyheader`'],
    'bodyfooter'=['boolean','`style/bodyfooter`'],
    'plotwidth'=[{'identical'('default'),'string'},'`style/plotwidth`'],
    'csstable'=['table','`style/csstable`'],
    'relatives'=[{'nonnegint','infinity'},'`style/relatives`'],
    'plotnumbering'=[proc(x) member(x,{'left','right','none'}); end,'`style/plotnumbering`'],
    'tocnumbering'=['boolean','`style/tocnumbering`'],
    'tocindent'=['boolean','`style/tocindent`'],
    'tocmaxindent'=[proc(x) type(x,'integer') and x>=0 and x<=100 end,'`style/tocmaxindent`'],
    'toccssbasename'=['string','`style/toccssbasename`'],
    'toctitle'=['string','`style/toctitle`']
    });
#>>>########################################################
#<<<########################################################
# @rawinclude
# @exportvar=TextSymbol
### Description: text-mode symbols. Note that the actual table
# is global, such that new entries can be done and saved
# outside of HTMX.
# (see http://www.ramsch.org/martin/uni/fmi-hp/iso8859-1.html)
############################################################
TextSymbol[`&sect`] := "&sect;";
TextSymbol[`&copyright`] := "&copy;";
TextSymbol[`&sp`] := " ";
#>>>########################################################
#<<<########################################################
# @sharevar=mylatexmathstyle
### Initialization:
# @rawinclude
mylatexmathstyle := CommonLib:-CopyOptions(MADLaTeX:-MathStyle);
#>>>########################################################
#<<<########################################################
# @exportvar=FileExtension
# @rawinclude
FileExtension := "htmx";
#>>>########################################################
#<<<########################################################
# @exportvar=DerivedFormat
# @rawinclude
### Description: the translator 'HTMX' outputs .htmx files.
# These files may be post-processed by some OS commands,
# which are declared in this table. Note that %F stands for
# the .htmx file with its extension, this is why one may
# use 'basename %F .htmx' to get rid of it.
DerivedFormat['html'] := "htmx -p %F";
DerivedFormat['nohtmx'] := "cp %F $(basename %F .htmx).html";
#>>>########################################################
#<<<########################################################
# @exportvar=TranslateText
# @sharevar=`fontsize/table`
# @rawinclude
### Description: initialization of the table 'TranslateText'
# that associates a keyword to its implementation.
############################################################

############################################################
### PRIMITIVES FOR FILE

# &fileheader()
TranslateText[`&fileheader`] := proc()
    # see http://www.w3.org/TR/REC-html40/sgml/dtd.html
    `&string`("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n");
end;

# &filebody(MADseq)
TranslateText[`&filebody`] := proc(x)
    `&string`("<html>\n",op(x),"</html>\n");
end;

# &HEADER(MADseq)
TranslateText[`&HEADER`] := proc(x)
    local i,csstbl,defcss,css,tmp;
    
    ### CSS
    csstbl := `style/csstable`;# same as DocStyle('csstable')
    defcss := _CSS_TBL;# see DocStyle.mpl
    css := `&string`();
    # ALL entries of both CSS tables are used    
    for i in sort([op(map(op,{indices(defcss),indices(csstbl)}))]) do
	# look first the in passed table
	if assigned(csstbl[i]) then
	    if csstbl[i] = 'VOID' then tmp := NULL;
	    else tmp := [i,csstbl[i]]; fi;
	else# default table
	    if defcss[i] = 'VOID' then tmp := NULL;
	    else tmp := [i,defcss[i]]; fi;
	fi;

	if tmp <> NULL then
	    css := `&string`(css,
		tmp[1],"{",tmp[2],"}\n");
	fi;
    od;
    css := `&string`(_HTML_COMMENTSTR("BEGIN CSS TABLE"),
	"<style type=\"text/css\">\n",css,"</style>\n",
	_HTML_COMMENTSTR("END CSS TABLE"));
    
    # output
    `&string`(
	"<head>\n",
	# document character encoding
	_HTTP_METASTR("content-type","text/html; charset=iso-8859-1"),### MANDATORY
	# default style language for INLINE CSS,
	# see: http://www.w3.org/TR/REC-html40/present/styles.html#h-14.2.2
	_HTTP_METASTR("content-style-type","text/css"),### MANDATORY
	_HTML_METASTR("date",CommonLib:-Date('ISO8601')),
	_HTML_METASTR("generator","MAD"),
	
	# <title> seems to be mandatory in HTML4.0
	`&string`("<title>",
	    `style/documenttitle`,
	    "</title>\n"),
	# CSS
	css,

	# header
	`&string`(_HTML_COMMENTSTR("BEGIN STATIC HEADER"),
		`style/documentheader`,# DocStyle('documentheader'), not parsed
		"\n",
		_HTML_COMMENTSTR("END STATIC HEADER")),
	
	_HTML_COMMENTSTR("BEGIN DYNAMIC HEADER"),
	op(x),# extra header (parsed)
	"\n",
	_HTML_COMMENTSTR("END DYNAMIC HEADER"),
	"</head>\n");
end;

# &body(MADseq)
TranslateText[`&body`] := proc(x)
    local header,footer,largs;
    # see DocStyle.mpl; the option 'bodyheader' (resp.
    # 'bodyfooter') indicates whether the first (resp.
    # last) argument is to be put before (resp. after)
    # the <div class="main">.
    largs := [op(x)];
    if `style/bodyheader` and nops(largs) > 0 then
	header := _HTML_COMMENTSTR("BEGIN BODY HEADER"),largs[1],
	    _HTML_COMMENTSTR("END BODY HEADER");
	largs := largs[2..-1];
    else
	header := NULL;
    fi;
    if `style/bodyfooter` and nops(largs) > 0 then
	footer := _HTML_COMMENTSTR("BEGIN BODY FOOTER"),largs[-1],
	    _HTML_COMMENTSTR("END BODY FOOTER");
	largs := largs[1..-2];
    else
	footer := NULL;
    fi;
    `&string`("<body>\n",
	"<div style=\"position:absolute\">",_NAMEDBEG("top"),_NAMEDEND,"</div>",# container mandatory
	header,
	"<div class=\"main\">\n",
	op(largs),
	"</div>\n",
	footer,
	"<div style=\"position:absolute\">",_NAMEDBEG("bottom"),_NAMEDEND,"</div>",# container mandatory
	"</body>\n");
end;

# &commentline(MADseq)
TranslateText[`&commentline`] := proc(x)
    `&string`("<!-- ",op(x)," -->\n");
end;

############################################################
### NUMBERING

# &NUMBERING(numbering)
TranslateText[`&NUMBERING`] := proc(x)
    # RAW numbering that comes without any CSS formatting
    `&string`(FormatNumbering(op(1,x)));
end;

############################################################
### LOGICAL SECTIONING

# <div class="$CLASS">
# <h$NEST class="$CLASS"><span class="$CLASS-numbering">$NUMBERING</span>$TITLE</hX>
# $CONTENT
# </div>

# &chapter(label,boolean,MADseq)
TranslateText[`&chapter`] := proc(x)
    local mytag,num;
    #  yield label info
    mytag := DocumentGenerator:-RefInfo(op(1,x),HTMX,'tag');
    num := DocumentGenerator:-RefInfo(op(1,x),HTMX,'numbering');
    
    `&string`(
	# the relative links are put upper right
	`if`(`style/relatives`>0,`&string`(
	    "<table cellspacing=\"0\" cellpadding=\"0\" style=\"width:100%%\">\n<tr>\n<td style=\"width:100%%\">&nbsp;</td>\n<td>\n",
	    "<div style=\"position:relative;top:-15px;left:20px\">",
	     `relatives/doit`(op(1,x)),"</div></td>\n</tr>\n</table>"),NULL),
   
	_DIVBEG("chapter"),# enclosing entity
	
#	`if`(`style/relatives`>0,`&string`(
#	    "<table cellspacing=\"0\" cellpadding=\"0\" style=\"width:100%%\">\n<tr>\n<td style=\"width:100%%\">\n"),NULL),

	"<h1 class=\"chapter\">",_NAMEDBEG(mytag),# anchor
	`if`(op(2,x),# numbering displayed
	    `&string`(_SPANBEG("chapter-numbering"),FormatNumbering(num),_SPANEND," "),
	    NULL),
	op(3,x),
	_NAMEDEND,"</h1>",
	
#	`if`(`style/relatives`>0,`&string`(
#	    "\n</td>\n<td>\n<div style=\"position:relative\">\n",
#	    "<div style=\"position:absolute;right:160px\">\n",
#	    `relatives/doit`(op(1,x)),"</div>\n</div>\n</td>\n</tr>\n</table>"),NULL),
	    
	op(4..-1,x),
	_DIVEND);
end;

# &section(label,boolean,MADseq)
TranslateText[`&section`] := proc(x)
    local mytag,num,hbeg,hend,nest;
    # yield label info
    mytag := DocumentGenerator:-RefInfo(op(1,x),HTMX,'tag');
    num := DocumentGenerator:-RefInfo(op(1,x),HTMX,'numbering');
    
    # The nesting only concerns the section numbering
    nest := nops(num[2]);
    
    if nest > 6 then nest := 6; fi;
    hbeg := cat("<h",nest," class=\"section",nest,"\">");
    hend := cat("</h",nest,">");
    
    `&string`(
	_DIVBEG(cat("section",nest)),# enclosing entity

	`if`(`style/relatives`>nest-1,`&string`(
	    "<table cellspacing=\"0\" cellpadding=\"0\" style=\"width:100%%\">\n<tr>\n<td style=\"width:100%%\">\n"),NULL),

	hbeg,_NAMEDBEG(mytag),# anchor
	`if`(op(2,x),# numbering displayed
	    `&string`(_SPANBEG(cat("section",nest,"-numbering")),FormatNumbering(num),_SPANEND," "),
	    NULL),
	op(3,x),
	_NAMEDEND,hend,
	
#	`if`(`style/relatives`>nest-1,`&string`(
#	    "\n</td>\n<td>\n<div style=\"position:relative\">\n",
#	    "<div style=\"position:absolute;right:160px\">\n",
#	    `relatives/doit`(op(1,x)),"</div>\n</div>\n</td>\n</tr>\n</table>"),NULL),
	
	`if`(`style/relatives`>nest-1,`&string`(
	    "\n</td>\n<td>\n\n",
	    `relatives/doit`(op(1,x)),"</td>\n</tr>\n</table>"),NULL),
	op(4..-1,x),
	_DIVEND);
end;

# &paragraph(MADseq)
TranslateText[`&paragraph`] := proc(x)
    `&string`("<p class=\"paragraph\">",op(x),"\n");
end;

############################################################
### STYLES

# &em(MADseq)
TranslateText[`&em`] := proc(x)
    `&string`(_SPANBEG("emphasize"),op(x),_SPANEND);
end;

# &tt(MADseq)
TranslateText[`&tt`] := proc(x)
    `&string`(_SPANBEG("typewriter"),op(x),_SPANEND);
end;

# &bf(MADseq)
TranslateText[`&bf`] := proc(x)
    `&string`(_SPANBEG("bold"),op(x),_SPANEND);
end;

`fontsize/table`['xxsmall'] := "xx-small";
`fontsize/table`['xsmall'] := "x-small";
`fontsize/table`['small'] := "small";
`fontsize/table`['medium'] := "medium";
`fontsize/table`['large'] := "large";
`fontsize/table`['xlarge'] := "x-large";
`fontsize/table`['xxlarge'] := "xx-large";

# &fontsize(symbol,MADseq)
TranslateText[`&fontsize`] := proc(x)
    `&string`("<span style=\"font-size:",`fontsize/table`[op(1,x)],"\">",op(2..-1,x),"</span>");
end;

# &dq(MADseq)
TranslateText[`&dq`] := proc(x)
    `&string`("\"",op(x),"\"");
end;

############################################################
### ERRORS

# &ierror(MADseq)
TranslateText[`&ierror`] := proc(x)
    `&string`(_SPANBEG("error-inline"),op(x),_SPANEND);
end;

# &error(MADseq)
TranslateText[`&error`] := proc(x)
    `&string`(_DIVBEG("error-block"),op(x),_DIVEND);
end;

############################################################
### CROSS-REFERENCES

# &linkurl(url::string,MADseq)
TranslateText[`&linkurl`] := proc(x)
    if nops(x) = 1 then
	_HTML_LINKSTR("url",`&string`(op(x)),`&string`(op(x)));
    else
	_HTML_LINKSTR("link",`&string`(op(1,x)),`&string`(op(2..-1,x)));
    fi;
end;

# &REF(fd,label)
TranslateText[`&REF`] := proc(x)
    local href;
    # check label
    if member(HTMX,DocumentGenerator:-RefInfo(op(2,x))) and
    member('numbering',DocumentGenerator:-RefInfo(op(2,x),HTMX)) then
	href := `&string`(CommonLib:-RelativePath(op(1,x),CommonLib:-FileExtension(DocumentGenerator:-RefInfo(op(2,x),HTMX,'filedesc'),"html")),"#",DocumentGenerator:-RefInfo(op(2,x),HTMX,'tag'));
	_HTML_LINKSTR("link",href,FormatNumbering(DocumentGenerator:-RefInfo(op(2,x),HTMX,'numbering')));
#	`&string`("<a href=\"",href,"\">",FormatNumbering(DocumentGenerator:-RefInfo(op(2,x),HTMX,'numbering')),"</a>");
    else
	`&string`("??");
    fi;
end;

# &LINK(fd,label,MADseq)
TranslateText[`&LINK`] := proc(x)
    local href;
    # check label
    if member(HTMX,DocumentGenerator:-RefInfo(op(2,x))) then
	href := `&string`(CommonLib:-RelativePath(op(1,x),CommonLib:-FileExtension(DocumentGenerator:-RefInfo(op(2,x),HTMX,'filedesc'),"html")),"#",DocumentGenerator:-RefInfo(op(2,x),HTMX,'tag'));
	# `&string`("<a href=\"",href,"\">",op(3..-1,x),"</a>");
	_HTML_LINKSTR("link",href,`&string`(op(3..-1,x)));
    else
	`&string`(op(3..-1,x));
    fi;
end;

# &TOC(fd,depth)
TranslateText[`&TOC`] := proc(x)
    formattoc(op(x));
end;

# &SUBTOC(fd,depth,label)
TranslateText[`&SUBTOC`] := proc(x)
    formattoc(op(x));
end;

############################################################
### MAPLE OBJECTS (see also TranslateMath.mpl)

# &PLOT(fd,label,PLOT|PLOT3D,MADseq)
TranslateText[`&PLOT`] := proc(x)
    local thisfd,tofd,src,num;
    thisfd := op(1,x);
    tofd := [op(thisfd),CommonLib:-UniqueId()];# go to subdirectory
    # relative path to gif
    src := cat(CommonLib:-RelativePath(thisfd,CommonLib:-FileExtension(tofd,"gif")));
    
    # Ensure that the directory is created
    CommonLib:-FileNew(tofd);
    CommonLib:-FileClose(tofd);
    CommonLib:-FileRemove(tofd);
    
    plotsetup('gif','plotoutput'=CommonLib:-FileName(
	CommonLib:-FileExtension(tofd,"gif")));
    print(op(3,x));# export plot to file
    
    if op(2,x) = 'VOID' then
	num := NULL;
    else
	num := "<td>\n",_SPANBEG("plot-numbering"),FormatNumbering(
	    DocumentGenerator:-RefInfo(op(2,x),HTMX,'numbering')),_SPANEND,"\n</td>\n";
    fi;
    
    `&string`(
	"<table style=\"width:100%%\">\n<tr>\n",
	
	`if`(`style/plotnumbering`='left',num,NULL),
	"<td class=\"plot\">",# begin cell
	`if`(op(2,x)='VOID',NULL,_NAMEDBEG(DocumentGenerator:-RefInfo(op(2,x),HTMX,'tag'))),# begin anchor
	
	# image
	"<img class=\"plot\"",`if`(`style/plotwidth`='default',NULL,
	    `&string`(" width=\"",`style/plotwidth`,"\"")),
	" src=\"",src,"\" alt=\"",src,"\">\n",
	
	`if`(op(2,x)='VOID',NULL,_NAMEDEND),# end anchor
	`if`(nops(x)>3,# caption
	    `&string`(_DIVBEG("plot-caption"),op(4..-1,x),_DIVEND),NULL),
	"</td>",# end cell
	`if`(`style/plotnumbering`='right',num,NULL),
	"</tr>\n</table>\n"
    );
end;
#>>>########################################################
VERSION := 1.106;
### END RAW INCLUSION #########################

end:
### END MODULE #################################
#SAVELIBNAME
#savelib('HTMX'):
