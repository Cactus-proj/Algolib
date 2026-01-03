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

# MAD - a Maple library for generating documents
# Author: Ludovic Meunier
# Copyright (C) 2001-2007 INRIA
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
### Description: the package 'MAD' implements a default style,
# which is brought by 'Document', an interface to CGI.
############################################################
############################################################
### Decription: Maple preprocessor macros
### Conventions:  _<NAME>_<NAME>
############################################################

### Standard error messages
$define _ERR_NARGS	error "wrong number of arguments"
$define _ERR_BADARGS	error "invalid arguments"
$define _ERR_BADVAL	error "invalid argument value"
$define _ERR_NOTIMP	error "not yet implemented"
$define _ERR_TYPE(x,t)	if not type(x,t) then error "%1 expected to be of type %2",x,t; fi
$define _ERR_BADOS	error "OS not supported"

### Copyright
$define _COPYRIGHT	`Copyright (c) 2001-2007 INRIA. All rights reserved.`
$define _OPTION		option _COPYRIGHT

#<<<###########################################
### @exit
### Generated: 04/12/2007 17:17:48
### Module name: MAD
### Version: 1.444
#>>>###########################################

#<<<########################################################
### Description: the variables that are defined in the HTML
# forms are passed to the CGI as a pair name=value. The names
# of the variables are listed here.
# @macro
$define _CGIVAR_CGIURL		"cgiurl"	# from maple-cgi
$define _CGIVAR_HTTPMETHOD	"httpmethod"	# from maple-cgi

$define _CGIVAR_COMMAND		"command"
$define _CGIVAR_MADEXPR		"madexpr"
#>>>########################################################
#<<<########################################################
# @macro
$define _HTTP_HEADER		printf("Content-Type: text/html\n\n")
$define _HTTP_REDIRECT(X)	printf("Location: %s\n\n",X)
#>>>########################################################
#<<<########################################################
# @macro
$define _HTML_MADFORM	`&sequence`("<div style=\"text-align:center\">\n<form action=\"",_URL_MADCGI,"\">\n<div><input type=\"text\" size=\"50\" name=\"",_CGIVAR_MADEXPR,"\"><input type=\"submit\" value=\"Submit\"></div></form></div>")
#>>>########################################################
#<<<########################################################
# @macro
### THESE URL MUST BE ABSOLUTE AND FIXED
$define _URL_MADLOGO		"http://algo.inria.fr/mad/images/mad-logo-150.png"
$define _URL_MADTEXT		"http://algo.inria.fr/mad/images/mad-text-300.png"

$define _URL_MAD		"http://algo.inria.fr/mad/index.html"
$define _URL_MADCGI		"http://algo.inria.fr/bin/esf"
$define _URL_MADPACKAGE		"http://algo.inria.fr/mad/downloads/MAD.tar.gz"
$define _URL_ESF		"http://algo.inria.fr/esf"

#$define _URL_MAPLECGI		"http://algo.inria.fr/meunier/scripts/maple-cgi.html"
#$define _URL_MADPOSTER_PS	"http://algo.inria.fr/mad/downloads/poster.ps

### FILES
$define _FD_MADINDEX	["mad","index"]
$define _FD_MADTMPFILE	["mad","tmp",CommonLib:-UniqueId()]
#>>>########################################################
#<<<########################################################
# @macro
$define _BANNER_PARAM(T,I)	`if`(assigned(T[I]),T[I],`html/banner/table`[I])
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
MAD := module()
	option `Copyright (c) 2001-2007 INRIA. All rights reserved.`,package, load=moduleinit;
### BEGIN DECLARATION #########################
local
	moduleinit,
	`HTMX/more`,
	`MADLaTeX/more`,
	`default/export`,
	`error/export`,
	`html/banner/bottom`,
	`html/banner/table`,
	`html/banner/top`,
	`html/csstable`,
	`intro/body`;
export
	CGIBind,
	CGIRequest,
	Chapter,
	Document,
	Equation,
	Export,
	FILEARCH,
	InlineMath,
	MADDocument,
	Paragraph,
	Plot,
	Ref,
	Section,
	TableOfContents,
	VERSION;
### END DECLARATION ###########################

### BEGIN FUNCTIONS ###########################
#<<<########################################################
### Description: error document output to stdout without
# writing any file to disk
### Argument sequence:
#	title::MAD
#	(optional)::MAD
### Output format::NULL (side effect to stdout)
############################################################
`error/export` := proc(title)
    _HTTP_HEADER;
    # HTML body
    Export(MADDocument(
	# document options
	'htmlbannerlinks'='VOID',
	'htmltitle'="Error",
	'htmlbannertop'=`&sequence`(_HTML_VSKIP(20),"<div style=\"font-weight:bold;font-size:xx-large;text-align:center;color:",_RGB_ORANGE,"\">Internal Server Error</div>"),
	
	# document body
	`&error`(`&bf`(`&fontsize`('xxlarge',title))),
	_HTML_VSKIP(10),
	`&error`(args[2..-1])),HTMX,'output'='stdout');
end;	# `error/export`
#>>>########################################################
#<<<########################################################
### Description: new definitions for MADLaTeX. This procedure
# is called by 'moduleinit'. Indeed, there is no way one can
# save these new definitions into the package MADLaTeX from this
# package.
############################################################
`MADLaTeX/more` := proc()
    MADLaTeX:-TextSymbol[`&mad`] := cat("\\href{",_URL_MAD,"}{MAD}");
    MADLaTeX:-TextSymbol[`&maple`] := "\\href{http://www.maplesoft.com/}{\\sf Maple}";
    MADLaTeX:-TextSymbol[`&postscript`] := "{\\sf PostScript}";
    MADLaTeX:-TextSymbol[`&latex`] := "\\LaTeX";
    MADLaTeX:-TextSymbol[`&html`] := "{\\sf HTML}";
    MADLaTeX:-TextSymbol[`&pdf`] := "{\\sf PDF}";
    MADLaTeX:-TextSymbol[`&esf`] := cat("\\href{",_URL_ESF,"}{ESF}");
    MADLaTeX:-TextSymbol[`&dlmf`] := "\\href{http://dlmf.nist.gov/Contents/}{DLMF}";
    MADLaTeX:-TextSymbol[`&lgpl`] := "\\href{http://www.gnu.org/copyleft/lesser.html}{LGPL}";
end;	# `MADLaTeX/more`
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: top banner; adjusted for a resolution of
# 1024 pixels (approx.).
### Argument sequence: Seq(MAD) + options
### Output format::MAD
############################################################
`html/banner/top` := proc()
    local optdcl,largs,bpar,logo,text,algo,mytoc,myotherfmts,i;
    
    # default values for options
    bpar := `html/banner/table`;
    mytoc := NULL;
    myotherfmts := NULL;
    optdcl := { 
	'paramtable'=['table','bpar'],
	'tableofcontents'=['anything','mytoc'],
	# otherformats: [fd,Seq(ext::string=text::MAD)]
	'otherformats'=[proc(x) type(x,'list') and evalb(nops(x)>1) and type(x[1],`DocumentGenerator/FileDescriptor`) and type(x[2..-1],'list'('string'='anything')) end,'myotherfmts']
	};
    largs := CommonLib:-RemoveYieldOptions([args],optdcl);
    
    logo := _BANNER_PARAM(bpar,"top-image-logo");
    text := _BANNER_PARAM(bpar,"top-image-text");
    algo := _BANNER_PARAM(bpar,"top-image-algo");
    
    # banner::MAD
    `&sequence`(
	`&commentline`("BEGIN TOP BANNER"),
	"<div style=\"position:relative;left:0px\">\n",# container
	_HTML_SP,# This forces the display of the banner for some browsers.
	
	`if`(mytoc<>NULL,`&sequence`(
	    "<div style=\"position:absolute;left:48px;top:160px;width:172px;z-index:1\">\n",	    
	    `&setoption`(HTMX:-DocStyle,['tocmaxindent'=10,'toccssbasename'="menu-toc"],mytoc),
	    # relative positioning for the following strokes (such that they are
	    # after the toc).
	    "<div style=\"position:relative;top:5px;\">",
	    _HTML_RECT_LEFT(0,40,127,3,_BANNER_PARAM(bpar,"top-stroke-toc-horiz")),
	    _HTML_RECT_LEFT(0,180,15,3,_BANNER_PARAM(bpar,"top-stroke-toc-horiz")),	
	    _HTML_RECT_LEFT(-45,172,3,65,_BANNER_PARAM(bpar,"top-stroke-toc-vert")),
	    "</div>",
	    "</div>"),NULL),
		
	# The right part of the banner should come first, such that it
	# gets underneath the left part when the browser's window is too small.
	_HTML_RECT_RIGHT(20,45,3,105,_BANNER_PARAM(bpar,"top-stroke-vert-3")),
	_HTML_RECT_RIGHT(138,45,3,25,_BANNER_PARAM(bpar,"top-stroke-vert-3")),
	_HTML_RECT_RIGHT(130,20,350,3,_BANNER_PARAM(bpar,"top-stroke-horiz-2")),
	`if`(algo = 'VOID',NULL,
	    _HTML_IMAGE_RIGHT(algo[1],algo[2],algo[3],algo[4],algo[5])),
	
	# Left part of the banner.
	_HTML_RECT_LEFT(40,20,20,3,_BANNER_PARAM(bpar,"top-stroke-horiz-1")),
	_HTML_RECT_LEFT(40,53,500,3,_BANNER_PARAM(bpar,"top-stroke-horiz-1")),
	_HTML_RECT_LEFT(20,45,3,300,_BANNER_PARAM(bpar,"top-stroke-vert-1")),
	_HTML_RECT_LEFT(20,220,3,15,_BANNER_PARAM(bpar,"top-stroke-vert-2")),
	_HTML_RECT_LEFT(48,220,3,70,_BANNER_PARAM(bpar,"top-stroke-vert-2")),
	_HTML_RECT_LEFT(145,220,3,50,_BANNER_PARAM(bpar,"top-stroke-vert-2")),
	# Images (left part of the banner)
	`if`(logo = 'VOID',NULL,
	    # op(1..4,logo) looks like a single argument to the macro
	    _HTML_IMAGE_LEFT(logo[1],logo[2],logo[3],logo[4],logo[5])),
	`if`(text = 'VOID',NULL,
	    # idem above
	    _HTML_IMAGE_LEFT(text[1],text[2],text[3],text[4],text[5])),
	    
	 # Links to other formats
	`if`(myotherfmts=NULL,NULL,
	    # myotherfmts::[fd,ext::string=text::MAD]
	    `&sequence`("<div style=\"position:absolute;left:230px;top:145px;z-index:1\" class=\"otherformats\">",
		CommonLib:-SepSeq(_HTML_SP,seq(_HTML_LINKSTR("otherformats",`&file->address`(myotherfmts[1],'relative',op(1,myotherfmts[i])),op(2,myotherfmts[i])),i=2..nops(myotherfmts))),"</div>")),
	    
	# Extra arguments are put into a table
	`&commentline`("BEGIN TOP BANNER CONTENT"),
	"<table style=\"width:100%%;position:absolute;left:0px;top:40px;z-index:1\">\n<tr>\n",
	"<td><div style=\"width:220px;\">&nbsp;</div></td>\n",
	"<td style=\"width:100%%\">",
	op(largs),# content
	"</td>\n",
	"<td><div style=\"width:45px;\">&nbsp;</div></td>\n",
	"</tr>\n</table>\n",
	`&commentline`("END TOP BANNER CONTENT"),
	
	"</div>\n",# container
	_HTML_VSKIP(_BANNER_PARAM(bpar,"top-bottom-vskip")),
	`&commentline`("END TOP BANNER")
    );
end;	# `html/banner/top`
#>>>########################################################
#<<<########################################################
# @scope=export
# @exportvar=CGIBind
### Description: this procedure is the entry point of a CGI.
# The request is to be formatted as a sequence of
# string=string, where the left (resp. right) string represents
# the HTML variable (see cgi.macros.mpl) (resp. its value). 
# Note that the value is represented as a string: 'parse' is
# to be invoked on it.
# The procedure 'CGIRequest' only forwards to some other procedures,
# according to the table 'CGIBind' that associates the value
# of the CGI variable _CGIVAR_COMMAND to a procedure. Such
# a "bound" procedure is responsible for 1) checking the
# arguments (esp. syntax and hack) and for 2) writing a valid
# HTTP response (including the header) to stdout. By default,
# the 'CGIBind' table is empty and the MAD demo is returned.
### Argument sequence::Seq(string=string)
### Output format::string (HTTP)
############################################################
CGIRequest := proc()
    local i,tmp;
    _OPTION;

    # argument type checking 
    if not type([args],'list'('string'='string')) then
	    `error/export`("Invalid Request","The common gateway interface (CGI) program sent an invalid request to the ",`&maple`," server. Please use this ",_HTML_LINK("error",_URL_MAPLECGI,"validated CGI program"),".");
	return;
    fi;
    
    # bind
    if hasoption([args],_CGIVAR_COMMAND,'tmp') and assigned(CGIBind[tmp]) then
	CGIBind[tmp](args);
    else# default = demo
	`default/export`(args);
    fi;
end;	# CGIRequest
#>>>########################################################
#<<<########################################################
# @scope=local
### Description: bottom banner; same comments and interface
# as above.
############################################################
`html/banner/bottom` := proc()
    local optdcl,largs,bpar;
    
    # default values for options
    bpar := `html/banner/table`;# default parameters
    optdcl := { 
	'paramtable'=['table','bpar']
	};
    largs := CommonLib:-RemoveYieldOptions([args],optdcl);

    # banner::MAD
    `&sequence`(
	`&commentline`("BEGIN BOTTOM BANNER"),
	# This enclosing table is used to avoid the
	# bug with the links.
	"<table><tr><td style=\"width:100%%\">\n",
	_HTML_VSKIP(_BANNER_PARAM(bpar,"bottom-top-vskip")),
	"<div style=\"position:relative\">\n",# container
	_HTML_SP,# This forces the display of the banner for some browsers.
	
	_HTML_RECT_LEFT(-10,220,3,70,_BANNER_PARAM(bpar,"bottom-stroke-vert-1")),
	_HTML_RECT_LEFT(0,195,20,3,_BANNER_PARAM(bpar,"bottom-stroke-horiz-1")),
	_HTML_RECT_LEFT(0,228,400,3,_BANNER_PARAM(bpar,"bottom-stroke-horiz-1")),
	
	`&localrawheader`(# extra style
	    [HTMX],
	    "<style type=\"text/css\">\n",
	    ".copyright{font-size:small;color:",_RGB_GRAY,"}\n",
	    ".copyright a:hover{background-color:",_RGB_GRAY_LIGHT,"}\n",
	    ".browser-compliance{font-size:small;color:",_RGB_GRAY,"}\n",
	    ".browser-compliance a:hover{background-color:",_RGB_GRAY_LIGHT,"}\n",
	    "</style>\n"),

	`if`(_BANNER_PARAM(bpar,"bottom-text"),
	    `&sequence`(
	# HTML4.01 and CSS1 compliance
	_HTML_BOX_LEFT(5,235,400,_HTML_CLASS("browser-compliance",`&sequence`("This web site is",`&sp`,_HTML_LINK("browser-compliance","http://validator.w3.org","compliant"),`&sp`,"with",`&sp`,_HTML_LINK("browser-compliance","http://www.w3.org/TR/html401","HTML 4.01"),`&sp`,"and",`&sp`,_HTML_LINK("browser-compliance","http://www.w3.org/TR/REC-CSS1","CSS 1"),"."))),
	
 	# copyright and date
	_HTML_BOX_LEFT(28,235,400,_HTML_CLASS("copyright",`&sequence`("Copyright ",`&copyright`,`&sp`,"2001-",CommonLib:-Date('YYYY'),`&sp`,"by the",`&sp`,_HTML_LINK("copyright","http://algo.inria.fr","Algorithms Project"),`&sp`,"and",`&sp`,_HTML_LINK("copyright","http://www.inria.fr","INRIA"),".",_HTML_BR,"All rights reserved. Created: ",CommonLib:-Date('STD1'),".")))
		    ),NULL),# `if`

	# Extra arguments.
	op(largs),
	    
	"</div>\n",# container
	"</td></tr></table>\n",		
	_HTML_VSKIP(_BANNER_PARAM(bpar,"bottom-bottom-vskip")),
	`&commentline`("END BOTTOM BANNER")
    );
end;	# `html/banner/bottom`
#>>>########################################################
#<<<########################################################
### Description: this procedure is bound by default
# to 'CGIRequest', which means that this procedure is
# responsible for outputing a VALID HTTP response.
# The argument sequence is the same a 'CGIRequest'.
############################################################
`default/export` := proc()
    local tmp,fd;

    if hasoption([args],_CGIVAR_MADEXPR,tmp) and tmp <> "" then
	# check syntax
	try 
	    tmp := eval(parse(tmp));# this may be a sequence
	    if nops([tmp])>1 then# this is a sequence
		tmp := [tmp];
	    fi;
	catch "Error, incorrect syntax in parse":
	    `error/export`("Syntax Error","There is a syntax error in the expression you entered. Please retry.",_HTML_MADFORM);
	    return;
	catch:# maple error
	    `error/export`("Server Error","The request you submitted caused an error. Please try another request.",_HTML_MADFORM);
	    return;	    
	end;
	
	if type(tmp,{'PLOT','PLOT3D'}) then
	    tmp := Plot(tmp);
	else
	    # check if can translate
	    try
		MADLaTeX:-latex(tmp,'string');
	    catch:
		`error/export`("MAD Error","The expression you entered contained a sub expression that is not supported yet. Please try another request.",_HTML_MADFORM);
		return;	    
	    end;
	    tmp := Equation(tmp);
	fi;

	# file descriptor
	fd := _FD_MADTMPFILE;
	Export(MADDocument('htmltitle'="MAD - Mathematical Abstract Document",'file'=fd,Chapter("Demo",'displaynumbering'=false,Paragraph("Result of your request:",tmp),Paragraph("Try another request:",_HTML_MADFORM),"<div style=\"text-align:center\">Back to ",`&mad`,"</div>")),HTMX,'format'='html');
	Export(MADDocument('file'=fd,Chapter("Demo",'displaynumbering'=false,Paragraph("Result of your request:",tmp))),LaTeX,'format'=['ps','pdf','dvi']);
	_HTTP_REDIRECT(CommonLib:-FileName(CommonLib:-FileExtension(fd,"html"),DocumentGenerator:-FILEARCH('basename')));	
    else# not a request: generate the default index.html
	Export(MADDocument('htmltitle'="MAD - Mathematical Abstract Document",'htmlbannertoc'=`&toc`('infinity'),'file'=_FD_MADINDEX,`intro/body`()),HTMX,'format'='html');
	Export(MADDocument('file'=_FD_MADINDEX,`intro/body`()),LaTeX,'format'=['ps','pdf','dvi']);
	_HTTP_REDIRECT(CommonLib:-FileName(CommonLib:-FileExtension(_FD_MADINDEX,"html"),DocumentGenerator:-FILEARCH('basename')));	
	fi;
end;	# `default/export`
#>>>########################################################
#<<<########################################################
### Description: new definitions for HTMX. This procedure
# is called by 'moduleinit'. Indeed, there is no way one can
# save these new definitions into the package HTMX from this
# package.
############################################################
`HTMX/more` := proc()
    HTMX:-TextSymbol[`&mad`] := _HTML_LINKSTR("link",_URL_MAD,"MAD");
    HTMX:-TextSymbol[`&maple`] := _HTML_LINKSTR("link","http://www.maplesoft.com/","Maple");
    HTMX:-TextSymbol[`&postscript`] := "PostScript";
    HTMX:-TextSymbol[`&latex`] := "LaTeX";
    HTMX:-TextSymbol[`&html`] := "HTML";
    HTMX:-TextSymbol[`&pdf`] := "PDF";
    HTMX:-TextSymbol[`&esf`] := _HTML_LINKSTR("link",_URL_ESF,"ESF");
    HTMX:-TextSymbol[`&dlmf`] := _HTML_LINKSTR("link","http://dlmf.nist.gov/Contents/","DLMF");
    HTMX:-TextSymbol[`&lgpl`] := _HTML_LINKSTR("link","http://www.gnu.org/copyleft/lesser.html","LGPL");
end;	# `HTMX/more`
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: the ENTIRE style of documents is (and SHOULD
# BE) brought by 'Document'; see also 'DocStyle' (below).
### Options:
# - 'file', 'header', 'setoption' (see DocumentGenerator:-Document)
# - 'csstable'::table
# - 'htmlbannertable'::table
# - 'htmlbannertop'::MAD
# - 'htmlbannerbottom'::MAD
############################################################
MADDocument := proc()
    local optdcl,largs,
	# options for DocumentGenerator:-Document
	myfiledesc,
	mysetoption,
	myheader,
	# options
	mycsstable,
	myhtmlbannertable,	
	myhtmlbannertop,
	myhtmlbannerbottom,
	myhtmltitle,
	myhtmlbannertoc,
	myhtmlbannerlinks
	;
    _OPTION;
	
    ### Default values for options
    myfiledesc := 'VOID';
    mysetoption := 'VOID';
    myheader := 'VOID';

    mycsstable := `html/csstable`;# see html.style.mpl
    myhtmlbannertable := `html/banner/table`;# see html.banner.mpl
    myhtmlbannertop := 'VOID';
    myhtmlbannerbottom := 'VOID';
    myhtmltitle := "";
    myhtmlbannertoc := 'VOID';
    myhtmlbannerlinks := ["tex"="LaTeX","dvi"="DVI","ps"="PostScript","pdf"="PDF"];
    
    ### Yield options
    optdcl := {
	'file'=[{`DocumentGenerator/FileDescriptor`,'VOID'},'myfiledesc'],    
	'setoption'=[{'VOID','list'},'mysetoption'],	
	'header'=[{'VOID','list'},'myheader'],
	
	'csstable'=['table','mycsstable'],		
	'htmlbannertable'=['table','myhtmlbannertable'],
	'htmlbannertop'=['anything','myhtmlbannertop'],# MAD
	'htmlbannerbottom'=['anything','myhtmlbannerbottom'],# MAD
	'htmltitle'=['string','myhtmltitle'],
	'htmlbannertoc'=['anything','myhtmlbannertoc'],
	'htmlbannerlinks'=[{'VOID','list'('string'='anything')},'myhtmlbannerlinks']
	};

    largs := CommonLib:-RemoveYieldOptions([args],optdcl);
    
    ### Process options
    
    # file
    if myfiledesc = 'VOID' then
	# automatic name provided here, instead of doc.file.mpl
	myfiledesc := [CommonLib:-UniqueId()];
    fi;
    
    # merge ALL options form 'setoption'
    mysetoption := [
	### more options are put FIRST (see `&setoption` in doc.translate.mpl), such that they can override 
	`if`(mysetoption='VOID',NULL,op(mysetoption)),
    
	### HTMX
	HTMX:-MathStyle,
	    'width'=60,
	    'locale'='_us',
	HTMX:-DocStyle,
	    'equationnumbering'='right',# 'left', 'none'
	    'bodyheader'=true,# for top banner
	    'bodyfooter'=true,# for bottom banner
	    'plotwidth'='default',# HTML unit
	    'csstable'=mycsstable,
	    'documenttitle'=myhtmltitle,
	### LaTeX
	LaTeX:-MathStyle,
	    'width'=60,
	    'locale'='_us',
	LaTeX:-DocStyle,
	    'plotwidth'="6cm"
	];

    # merge ALL raw header (dynamic)
    myheader := [
	### HTMX
	[HTMX],
	    _HTML_META("author","The Algorithms Project, INRIA Rocquencourt, France"),
	    _HTML_META("copyright",cat("&copy; 2001-",CommonLib:-Date('YYYY')," by the Algorithms Project and INRIA. All rights reserved.")),	
	    _HTML_META("description","MAD: Mathematical Abstract Document"),
	### LaTeX
	[LaTeX],
	    `&commentline`("Copyright ",`&copyright`," 2001-",CommonLib:-Date('YYYY')," by the Algorithms Project and INRIA. All rigths reserved."),
	### more RAW header
	`if`(myheader='VOID',NULL,op(myheader))];
	
    myhtmlbannertop := `html/banner/top`(`if`(myhtmlbannerlinks='VOID',NULL,'otherformats'=[myfiledesc,op(myhtmlbannerlinks)]),`if`(myhtmlbannertoc='VOID',NULL,'tableofcontents'=myhtmlbannertoc),'paramtable'=myhtmlbannertable,`if`(myhtmlbannertop='VOID',NULL,myhtmlbannertop));
    myhtmlbannerbottom := `html/banner/bottom`('paramtable'=myhtmlbannertable,`if`(myhtmlbannerbottom='VOID',NULL,myhtmlbannerbottom));
	
    ### MAD output
    DocumentGenerator:-Document(
	### Options
	'file'=myfiledesc,
	'setoption'=mysetoption,
	'header'=myheader,

	### Body
	`&only`([HTMX],myhtmlbannertop,
	    # put "main" into a table
	    "<table style=\"width:100%%\">\n<tr>\n",
	    "<td><div style=\"width:220px;\">&nbsp;</div></td>\n",
	    "<td style=\"width:100%%\">\n"),
	op(largs),
	`&only`([HTMX],
	    # close above table
	    "</td>\n",
	    "<td><div style=\"width:45px;\">&nbsp;</div></td>\n",
	    "</tr>\n</table>\n",
	    myhtmlbannerbottom)
    );
end;	# MADDocument
#>>>########################################################
#<<<########################################################
`intro/body` := proc()
    local intro,demo,down;
    
    ### Introduction
    intro := Section("Presentation",
	
	Paragraph(`&mad`," (",`&em`("Mathematical Abstract Document"),") is a document preparation system integrated with ",`&maple`,". ",`&mad`," first allows to export ",`&maple`," expressions into several standard formats, such as ",`&latex`,", ",`&postscript`,", ",`&pdf`," and ",`&html`,". This feature is partially available in ",`&maple`," and it has been improved: a special effort has been put on the rendering of mathematical objects. ",`&mad`," also defines a data structure (labelled tree) that represents a document. Such an ",`&dq`("abstract")," document is independent of the output format and thus reduces to its logical structure and a semantic description of its content. As a data structure, an abstract document can be manipulated by ",`&maple`," procedures. In particular, entire documents can be produced automatically from minimal inputs."),
	
	Paragraph("Integrating ",`&mad`," into a computer algebra system such as ",`&maple`," brings the following original features: 1) the automation of the computation and of the typesetting of mathematical formulae, graphs and tables; 2) the automation of the building of documents thanks to the underlying programming language."),

	Paragraph("The system ",`&mad`," was created in order to generate the interface of the ",`&esf`," (",`&em`("Encyclopedia of Special Functions"),") . The ",`&esf`," is an ",`&em`("automatically")," generated encyclopedia, where all mathematical formulae and graphs are computed by generic algorithms. The ",`&esf`," can be compared up to some extent with the Abramowitz and Stegun\'s ",`&em`("Handbook of Mathematical Functions")," and the ",`&dlmf`," project. This kind of mathematical document sets the following requirements for it to be easily used: pretty display (human readable format) and semantics (machine understandable format) of the mathematical formulae, cross references for browsing and quoting, paper version in several formats. The system ",`&mad`," is designed to automate the edition and the production of such mathematical documents.")
	);# end Section

    ### Demo
    demo := Section("Demo",
	`&only`([LaTeX],
	Paragraph("An interactive demo that shows how mathematical objects are handled is available at ",`&linkurl`(_URL_MAD))),
	`&only`([HTMX],
	Paragraph("This interactive demo shows how mathematical objects are handled. ")),
	
	Paragraph("Formulae are translated to ",`&latex`," first and then converted to pictures. An automatic line-breaking algorithm spreads the formulae that do not fit within the page over several lines. Graphs are directly exported to pictures by ",`&maple`," ."),
	`&only`([HTMX],
	Paragraph("Enter any ",`&maple`," expression (even a plot) in the ",`&maple`," syntax in the box below. Note that ",`&em`("only")," ",`&maple`," objects that have an obvious and unambiguous mathematical meaning are supported by default."),_HTML_MADFORM)
	);# end Section
	
    ### Downloads
    down := Section("Download",
	Paragraph("The system ",`&mad`," is distributed under ",`&lgpl`," license. The distribution contains a ",`&maple`," archive, which includes the online documentation, and the ",`&maple`," source code. A README file explains how to make ",`&mad`," available on your system from the distribution. ",`&linkurl`(_URL_MADPACKAGE,"Download MAD.")," (300 Ko tar.gz). "));

    ### MAD output
    Chapter("MAD",
	'displaynumbering'=false,
	`&localrawheader`([HTMX],_HTML_METASTR("keywords","mad,maple,web,maple web interface,maple latex,maple html")),
	intro,
	demo,
	down);
end;	# `intro/body`
#>>>########################################################
#<<<########################################################
# @scope=local
# @load
### Decription: initialization
############################################################
moduleinit := proc()
    `LaTeX/more`();# see LaTeX.more.mpl
    `HTMX/more`();# see HTMX.more.mpl
    unprotect('CGIBind');# see cgi.mpl
end:	# moduleinit
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: simplified interface of Export
############################################################
Export := proc(d)
    local fdl_htmx,fdl_latex;
    _OPTION;
    if nargs = 1 then
	# resolving references for LaTeX
	DocumentGenerator:-ResolveRef(d,LaTeX);
	# ResolveRef implied for HTMX
	DocumentGenerator:-Export(d,HTMX,'format'=['html'],'filelist'=fdl_htmx);	
	DocumentGenerator:-Export(d,LaTeX,'format'=['dvi','ps','pdf'],'filelist'=fdl_latex);	
	
	# output is the list of generated files (primary output)
	[op(fdl_htmx),op(fdl_latex)];
	
    else# plain version
	DocumentGenerator:-Export(args);
    fi;
end;	# Export
#>>>########################################################
### END FUNCTIONS #############################
### BEGIN RAW INCLUSION #######################
### 11/02/02: @rawinclude is used in practice for
# initializations, which must be done after the
# definitions and declarations.
#<<<########################################################
# @rawinclude
# @sharevar=`html/banner/table`
# The parameters of the banners are gathered into a table
# that has the following default values (which produce the
# MAD interface).
### Top banner
`html/banner/table`["top-stroke-horiz-1"] := _RGB_BLUE;# color
`html/banner/table`["top-stroke-horiz-2"] := _RGB_ORANGE;# color
`html/banner/table`["top-stroke-vert-1"] := _RGB_ORANGE;# color
`html/banner/table`["top-stroke-vert-2"] := _RGB_ORANGE;# color
`html/banner/table`["top-stroke-vert-3"] := _RGB_BLUE;# color
`html/banner/table`["top-image-logo"] := [50,60,150,60,_URL_MADLOGO];# ::[top,LEFT,width,height,string or MAD]
`html/banner/table`["top-image-text"] := [120,160,300,22,_URL_MADTEXT];# idem as "top-image-logo"
`html/banner/table`["top-image-algo"] := 'VOID';# ::[top,RIGHT,width,height,string or MAD]
`html/banner/table`["top-bottom-vskip"] := 135;# ::integer (pixels)
`html/banner/table`["top-stroke-toc-horiz"] := _RGB_BLUE;# color
`html/banner/table`["top-stroke-toc-vert"] := _RGB_ORANGE;# color
### Bottom banner
`html/banner/table`["bottom-stroke-horiz-1"] := _RGB_BLUE;# color
`html/banner/table`["bottom-stroke-vert-1"] := _RGB_ORANGE;# color
`html/banner/table`["bottom-bottom-vskip"] := 50;# ::integer (pixels)
`html/banner/table`["bottom-top-vskip"] := 0;# ::integer (pixels)
`html/banner/table`["bottom-text"] := true;# ::boolean
#>>>########################################################
#<<<########################################################
### Description: OVERRIDE THE DEFAULT CSS TABLE (see HTMX)
# @sharevar=`html/csstable`
# @rawinclude
############################################################
`html/csstable`["body"] := cat(
    "background-color:",_RGB_WHITE,
    ";color:",_RGB_BLACK,
    ";font-family:arial,sans-serif;font-size:medium");

# sectioning
`html/csstable`["div.main"] := "margin-left:30px;margin-right:30px";
`html/csstable`["h1.chapter"] := cat("text-align:center;color:",_RGB_ORANGE,";");
`html/csstable`["h1.section1"] := cat("color:",_RGB_BLUE,";");
`html/csstable`["h2.section2"] := cat("color:",_RGB_BLUE,";");
`html/csstable`["h3.section3"] := cat("color:",_RGB_BLUE,";");
`html/csstable`["h4.section4"] := cat("color:",_RGB_BLUE,";");
`html/csstable`["h5.section5"] := cat("color:",_RGB_BLUE,";");
`html/csstable`["h6.section6"] := cat("color:",_RGB_BLUE,";");

# menu toc
`html/csstable`["div.menu-toc"] := "font-size:small";
`html/csstable`["div.menu-toc-title"] := cat("font-size:medium;font-weight:bold;margin-top:1em;margin-left:1em;color:",_RGB_ORANGE,";");
`html/csstable`["table.menu-toc"] := "margin:1em";
`html/csstable`["a.menu-toc"] := cat("text-decoration:none;color:",_RGB_BLACK,";");
`html/csstable`["a.menu-toc:hover"] := cat("background-color:",_RGB_GRAY_LIGHT,";");

# standard toc
`html/csstable`["div.tableofcontents"] := cat("border-width:1px;border-color:",_RGB_ORANGE,";border-style:solid;background-color:",_RGB_BLUE_LIGHT,";");
`html/csstable`["a.tableofcontents"] := cat("margin:0;text-decoration:none;color:",_RGB_BLACK,";");
`html/csstable`["a.tableofcontents:hover"] := cat("margin:0;background-color:",_RGB_WHITE,";");

### MAD SPECIFIC
`html/csstable`["div.otherformats"] := "font-size:small";# html.banner.mpl
`html/csstable`["a.otherformats"] := cat("text-decoration:none;color:",_RGB_BLACK,";");# html.banner.mpl
`html/csstable`["a.otherformats:hover"] := cat("background-color:",_RGB_GRAY_LIGHT,";");# html.banner.mpl
`html/csstable`["a.link"] := cat("color:",_RGB_BLACK,";");# general purpose link, HTMX.more.mpl
`html/csstable`["a.link:hover"] := cat("background-color:",_RGB_GRAY_LIGHT,";");
#>>>########################################################
#<<<########################################################
# @rawinclude
# @exportvar=Document
# @exportvar=Chapter
# @exportvar=Section
# @exportvar=Paragraph
# @exportvar=Equation
# @exportvar=InlineMath
# @exportvar=Plot
# @exportvar=Ref
# @exportvar=TableOfContents
# @exportvar=FILEARCH
Document := DocumentGenerator:-Document;
Chapter := DocumentGenerator:-Chapter;
Section := DocumentGenerator:-Section;
Paragraph := DocumentGenerator:-Paragraph;
Equation := DocumentGenerator:-Equation;
InlineMath := DocumentGenerator:-InlineMath;
Plot := DocumentGenerator:-Plot;
Ref := DocumentGenerator:-Ref;
TableOfContents := DocumentGenerator:-TableOfContents;
FILEARCH := DocumentGenerator:-FILEARCH;
#>>>########################################################
VERSION := 1.445;
### END RAW INCLUSION #########################

end:
### END MODULE #################################
#SAVELIBNAME
#savelib('MAD'):
