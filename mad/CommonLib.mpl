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

# CommonLib - a Maple library of general purpose routines
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
### The package 'CommonLib' defines a collection of general
# purpose functions. Note that the arguments are NEVER checked.
############################################################
### Export:
# <name>			<file>
# - RemoveYieldOptions		options.mpl
# - WithLocalOptions		options.mpl
# - CopyOptions			options.mpl
# - MakeProcOptions		options.mpl
# - AlphaNumeric		string.mpl
# - AssertNoHack		string.mpl
# - FileName			file.mpl
# - FileNew			file.mpl
# - FileClose			file.mpl
# - FileRemove			file.mpl
# - FilePrint			file.mpl
# - FileCommand			file.mpl
# - FileOptions			file.mpl
# - RelativePath		file.mpl
# - SepSeq			tools.mpl
# - AName			tools.mpl
# - Date			date.mpl
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
$define _ERR_BADOS	error "OS not supported"

### Copyright
$define _COPYRIGHT	`Copyright (c) 2001-2007 INRIA. All rights reserved.`
$define _OPTION		option _COPYRIGHT

### file.mpl
$define _FILE_UP	".."
$define _STRING_FILEDESC(x)	StringTools:-Split(StringTools:-Trim(x),"/")
#<<<###########################################
### @exit
### Generated: 04/12/2007 17:17:50
### Module name: CommonLib
#>>>###########################################

#<<<########################################################
# @macro
$define _SSYSTEM_DATE(X)	ssystem(cat("date +\"",X,"\""))
#>>>########################################################
#<<<########################################################
# @macro
$define _ALPHANUM_EXCLUDE	["_",".","-"]
#>>>########################################################
### BEGIN MODULE ##############################
CommonLib := module()
	option `Copyright (c) 2001-2007 INRIA. All rights reserved.`,package, load=moduleinit;
### BEGIN DECLARATION #########################
local
	absolutefilename,
	fileopen,
	formatfiledescriptor,
	isdirectory,
	moduleinit,
	thedirectoryseparator,
	therootdirectory;
export
	AName,
	AlphaNumeric,
	AssertNoHack,
	CopyOptions,
	Date,
	FileClose,
	FileCommand,
	FileExtension,
	FileName,
	FileNew,
	FileOptions,
	FilePrint,
	FileRemove,
	MakeProcOptions,
	NameSubs,
	RelativePath,
	RemoveYieldOptions,
	SepSeq,
	SplitList,
	UniqueId,
	WithLocalOptions;
global
	VOID,
	`type/FileDescriptor`,
	`type/PLOT`,
	`type/PLOT3D`,
	`type/VOID`;
### END DECLARATION ###########################

### BEGIN FUNCTIONS ###########################
#<<<########################################################
# @scope=local
### Argument sequence:
#	l::list(string)
### Output format::boolean (including FAIL)
### Description: this procedure returns true (resp. false)
# if l is a directory (resp. a file). FAIL is returned
# otherwise.
############################################################
isdirectory := proc(l)
    local ans;
    try
	ans := isdir(absolutefilename(l));
    catch "file or directory does not exist":
	ans := FAIL;
    end;
    ans;
end;	# absolutefilename
#>>>########################################################
#<<<########################################################
# @scope=global
############################################################
`type/FileDescriptor` := proc(x)
    _OPTION;
    type(x,'list');
end;	# `type/FileDescriptor`
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence
#	l::list(string)
#	m::{READ,WRITE,APPEND}
### Output format::string
### Description: this procedure opens the file represented
# by a list in the specified mode. It is harmless to open
# an already opened file (even in a different mode); the
# directories that lead to the file are created if needed.
# The returned string is the actual name of the file.
############################################################
fileopen := proc(l,m)
    local i,f,tmp;
    # look first in opened files
    tmp := iostatus()[4..-1];# list of opened files
    f := absolutefilename(l);
    for i in tmp do
	if i[2] = f then
	    # same mode?
	    if (i[5] = 'READ' and m = 'READ') or
		(i[5] = 'WRITE' and member(m,{'WRITE','APPEND'})) then
		# nothing to do
		return f;
	    else# not same mode
		fclose(f);# close
		procname(l,m);# reopen with good mode
	    fi;
	fi;
    od;
    
    # not an opened file, then open it
    tmp := isdirectory(l);
    if tmp = true then# this is an existing directory
	error "cannot overwrite a directory by a file";
    elif tmp = false then# this is an existing file
	fopen(f,m);
    else# FAIL: there is nothing yet
	# go create directories if needed
	for i to nops(l) - 1 do
	    if isdirectory(l[1..i]) = FAIL then
		mkdir(absolutefilename(l[1..i]));
	    fi;
	od;
	# create the file
	fopen(f,m);
    fi;
    f;
end;	# fileopen
#>>>########################################################
#<<<########################################################
# @scope=global
`type/PLOT` := proc(x) evalb(op(0,x)='PLOT'); end;
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	optproc::procedure
### Output format::set(symbol=anything)
### Description: this procedure initializes the local options
# with the values of the options that are returned by 'optproc'.
############################################################
CopyOptions:= proc(optproc)
    local i;
    _OPTION;
#    _ERR_TYPE(optproc,'procedure');
    
    # option=value
    {seq(i=optproc(i),i=optproc())};
end;	# CopyOptions
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	locopt::{CopyOptions,name}
#	optproc::procedure
#	p::procedure
#	(optional) seq(anything)
### Output format: same as 'p'
### Description: call the procedure 'p' (which is assumed to
# be affected by options) with the arguments args[4..-1],
# while the local options are used. Note that 'locopt' may
# also be passed by name, in which case it is assigned to
# the local options AFTER 'p' is called.
############################################################
WithLocalOptions := proc(locopt,optproc,p)
    local opt,i,res;
    _OPTION;
    
    # current options
    opt := {seq(i=optproc(i),i=optproc())};# CopyOptions(optproc);
    
    # set local options
    if type(locopt,'name') then
	seq(optproc(i),i=eval(locopt));
    else seq(optproc(i),i=locopt); fi;
    
    # do whatever has to be done
    res := p(args[4..-1]);

    # get local options (that may have been updated by 'p')
    if type(locopt,'name') then
	assign(locopt,{seq(i=optproc(i),i=optproc())});# CopyOptions(optproc)
    fi;
    
    # restore options
    seq(optproc(i),i=opt);
    
    # output
    res;
end;	# WithLocalOptions
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence
#	fd::FileDescriptor
#	fmt::string
#	(optional)
### Output format::NULL
### Description: print to file; the file is automatically
# opened in 'APPEND' mode; same syntax as 'fprintf'.
############################################################
FilePrint := proc(fd,fmt)
    local f;
    _OPTION;
#    _ERR_TYPE(fd,'FileDescriptor');
#    _ERR_TYPE(fmt,'string');
    f := formatfiledescriptor(fd);
    f := fileopen(f,'APPEND');
    fprintf(f,args[2..-1]);
    NULL;
end;	# FilePrint
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: compute a unique identifier
### Output format::integer
############################################################
UniqueId := proc()
    local t,r,l;
    # iolib(25) encodes the date, accuracy is second.
    t := irem(iolib(25),10^6);# contain year, month, ...: truncated
    r := rand();
    l := `if`(r=0,1,length(r));# length of rand (instead of log)
    t*10^(l)+r;
end;	# UniqueId
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: this procedure outputs a local exported name.
# This name is a "new" name, in the sense that it is different
# of every other names, but it is NOT a new symbol. In particular,
# when saved to a repository, this name is first converted to
# a symbol (loss of the scope).
############################################################
AName := proc()
    local _AName;
    _OPTION;
    _AName;
end;	# AutoName
#>>>########################################################
#<<<########################################################
# @scope=global
`type/VOID` := proc(x) evalb(x='VOID'); end;
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	arglist::list
#	optdcl::{set,list}({symbol,string}=[{procedure,type},name])
### Output format::list
### Description: 'arglist' is typically the list of the
# arguments of another procedure that has options in the
# format name=value. This procedure removes the options
# from 'arglist', returns the cleaned list, and checks the
# options as specified in 'optdcl'. 'optdcl' declares
# the options in the format optionname=[type,name] (in which
# case the value is checked against the specified type and
# assigned to the specified name) or optionname=[procedure,name],
# where the name is optional (in which case the given procedure
# - that MUST output a boolean - is called with the value; if a
# name is provided then the value is assigned to it, otherwise
# the assignement is assumed to be done as a side effect of the
# passed procedure). Note that the names of the options may be
# string.
### History:
# 02.25.03: #! syntax of 'optdcl' modified
# 03.04.03: 
############################################################
RemoveYieldOptions := proc(arglist,optdcl)
    local tytbl,optind,myval,myopt,i,chk;
    _OPTION;
#    _ERR_TYPE(arglist,'list');
#    _ERR_TYPE(optdcl,{'set','list'});
    
    seq(assign(tytbl[op(1,i)]=op(2,i)),i=optdcl);
    # index of options within 'arglist'
    optind := {};

    for i to nops(arglist) do
	if type(arglist[i],`=`) then
	    myopt,myval := op(arglist[i]);
	    if assigned(tytbl[myopt]) then
#!		if type(tytbl[myopt][2],'type') then
		if type(tytbl[myopt][1],'type') then
#!		    chk := type(myval,tytbl[myopt][2]);
    		    chk := type(myval,tytbl[myopt][1]);
		else# 'procedure'
#!		    chk := tytbl[myopt][2](myval);
		    chk := tytbl[myopt][1](myval);
		fi;
		if not chk then
		    error "%1=%2: %2: invalid argument",myopt,myval;
		else
		    optind := optind union {i};
#!		    assign(tytbl[myopt][1],myval);
		    if nops(tytbl[myopt]) = 2 then
			assign(tytbl[myopt][2],myval);	fi;
		    # else: side effect assumed
		fi;
	    fi;
	fi;
    od;
    # remove options from the list
    subsop(seq(i=NULL,i=optind),arglist);
end;	# RemoveYieldOptions
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence
#	fd::FileDescriptor
### Output format::NULL
############################################################
FileClose := proc(fd)
    _OPTION;
#    _ERR_TYPE(fd,'FileDescriptor');
    fclose(absolutefilename(formatfiledescriptor(fd)));
end;	# FileClose
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	fd::FileDescriptor
### Description: remove (delete) file
############################################################
FileRemove := proc(fd)
    local l;
    _OPTION;
    l := formatfiledescriptor(fd);
    if isdirectory(l) = false then# this is a file
	fremove(absolutefilename(l));
    fi;
    NULL;
end;	# FileRemove
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence:
#	l::list(string)
#	(optional) r::string
### Output format::string
### Description: this procedure computes the absolute (physical)
# path to a file. The optional argument is taken to
# be the root directory.
############################################################
absolutefilename := proc(l,r)
    local i,rdir;
    if nargs > 1 then
	rdir := r;
    else
	rdir := therootdirectory;
    fi;
    cat(rdir,seq(op([thedirectoryseparator,i]),i=l));
end;	# absolutefilename
#>>>########################################################
#<<<########################################################
# @scope=export
### Output format::boolean
### Argument sequence:
#	x::string
### Description: when Maple is used as a server, it receives
# input from users. This input must be syntactically correct
# and should also be checked against bad-intentioned users,
# who may be willing to hack the server. The user input is
# passed to Maple as a string, which is meant to be parsed
# by 'parse'. 'AssertNoHack' is to be called BEFORE parsing
# and it checks whether the input CONTAINS (ie: 'x' is not
# parsed nor evaluated here) Maple commands that makes request
# to the OS.
### Output format::boolean
############################################################
AssertNoHack := proc(x)
    local hl,i;
    _OPTION;

    ### List of hacks:
    # - '!' when used as an escape to the OS (ie at the beginning
    # of a line) makes 'parse' return an syntax error.
    # - 'restart' is ignored when called from within a procedure
    hl := [ "quit","done","exit",
	"rmdir","mkdir",
	"fopen","fremove",#"fclose",
	"open",#"close",
	"process",
	"getenv","currentdir",
	"iolib","system","ssystem"];
	
    for i in hl do
	if StringTools:-Search(i,x) > 0 then return false; fi;
    od;
    true;

end;	# AssertNoHack
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	fd_from::FileDescriptor
#	fd_to::FileDescriptor
### Output format::string
### Description: this procedure computes the relative path
# between 2 files that are described by their file descriptors.
# If the file descriptors are the same, then the name of the
# file is returned.
############################################################
RelativePath := proc(fd_from,fd_to)
    local i,nup,down;
    _OPTION;    
    # look for common path (directory)
    for i to min(nops(fd_from),nops(fd_to))-1 do
	if fd_from[i] = fd_to[i] then next
	else break fi
    od;
    # relative path
    nup := nops(fd_from)-i;# number of _FILE_UP
    down := fd_to[i..-1];# downward path
    # string
    cat("",SepSeq(thedirectoryseparator,_FILE_UP$nup,
	op(map(AlphaNumeric,down,_ALPHANUM_EXCLUDE))));
end;	# RelativePath
#>>>########################################################
#<<<########################################################
# @scope=local
# @load
### Description: the stuffs that are in this procedure must
# be executed at initialization time. If it's done at creation
# time (ie in the body of the module), then the values are
# taken from the system that created the module.
############################################################
moduleinit := proc()
    therootdirectory := currentdir();
    thedirectoryseparator := kernelopts('dirsep');
end;	# moduleinit
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: this procedure splits the list 'l' at the
# positions where 'p' evaluates to true. Each sub-list
# contains the element where the list was split (at 1st postion)
# and the elements of 'l' up to the next split.
### Argument sequence:
#	p::{type,procedure}
#	l::list
### Output format::Seq(list)
############################################################
SplitList := proc(p,l)
    local i,j,tbl,chkproc;
    _OPTION;
    tbl := table();# ensure valid for 'entries'
    if type(p,'type') then chkproc := proc(x) type(x,p); end;
    elif type(p,'procedure') then chkproc := p;
    else _ERR_BADARGS; fi;
    
    for i to nops(l) do
	if chkproc(l[i]) then
	    for j from i+1 to nops(l) do
		if chkproc(l[j]) then break; fi;
	    od;
	    tbl[i] := l[i],op(l[i+1..j-1]);
	fi;
    od;
    entries(tbl);
end;
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: this procedure substitutes names by other
# names into an expression. For instance, the sequence
# a=b, b=a that is substitued into a^b results b^a (instead
# of a^a with 'subs').
### Argument sequence:
#	args[1..-2]::name=name
#	args[-1]::anything
############################################################
NameSubs := proc()
    local i,an,tmp,r;
    _OPTION;
    r := 1..nargs-1;
    an := [seq(AName(),i=r)];
    tmp := subs(seq(op(1,args[i])=an[i],i=r),args[-1]);
    subs(seq(an[i]=op(2,args[i]),i=r),tmp);
end;	# NameSubs
#>>>########################################################
#<<<########################################################
# @scope=global
`type/PLOT3D` := proc(x) evalb(op(0,x)='PLOT3D'); end;
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: date information in several formats. The
# current implementation makes system calls and it is thus 
# not portable.
### Output format::string
### History:
# 06/02/03: added `CommonLib/Date`
############################################################
Date := proc(fmt)
    local stat;
    global `CommonLib/Date`;
    _OPTION;
    if fmt = 'ISO8601' then
	stat := _SSYSTEM_DATE("%Y-%m-%dT%H:%M:%S");
    elif fmt = 'YYYY' then
	stat := _SSYSTEM_DATE("%Y");
    elif fmt = 'STD1' then
	stat := _SSYSTEM_DATE("%b %e %Y %H:%M:%S");
    else _ERR_BADARGS; fi;
    
    if op(1,stat) = 0 then StringTools:-Trim(op(2,stat));
    elif type(`CommonLib/Date`,'procedure') then
	# look for user-defined procedure
	`CommonLib/Date`(fmt);
    else
	"date not available";
	#_ERR_BADOS;
    fi;
end;	# Date
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	fd::FileDescriptor
#	cmd::string
#	(optional) anything
### Output format::ssystem
### Description: invoke the OS-specific command 'cmd' on the
# file 'fd' that is described by its FileDescriptor. The
# command 'cmd' is a string with the same syntax as 'printf';
# the special %F is used to denote the name of the file.
# Eg: FileCommand(fd,"latex %F").
############################################################
FileCommand := proc(fd,cmd)
    local mydir,mycmd,res;
    _OPTION;
    mydir := currentdir();
    # substitute %F to its value
    mycmd := StringTools:-SubstituteAll(cmd,"%F",formatfiledescriptor(fd[-1]));

    # change directory to execute the command
    currentdir(absolutefilename(formatfiledescriptor(fd[1..-2])));
    # execute
    res := ssystem(sprintf(mycmd,args[3..-1]));

    # back to original directory
    currentdir(mydir);
    
    res;
end;	# FileCommand
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	x::anything
#	(optional) list(string)
### Output format::string
### Description: convert any object into string made up
# of alphanumeric ([a-zA-Z0-9]) characters.
### Note: this implementation is NOT injective (one would
# need to use a character as escape character), but it
# should be enough. The optional argument is a list of
# characters (a character is represented by a string of
# length 1) that are NOT converted into alphanumeric
# characters. This option was added for keeping the "."
# in the extensions of the files.
############################################################
AlphaNumeric := proc(x)
    local str,len,i,j,res,tmp,extra;
    _OPTION;
    if nargs > 1 then
#	_ERR_TYPE(args[2],list('string'));
	extra := args[2];
    else extra := []; fi;

    str := convert(x,'string');
    if StringTools:-AndMap(X->StringTools:-IsAlphaNumeric(X)
	or member(X,extra),str) then
	str;
    else# some characters are not alphanumeric
	len := length(str);
	for i to len do
	    if StringTools:-IsAlphaNumeric(str[i])
		or member(str[i],extra) then
		res[i] := str[i];
	    else# non alphanumeric character nor extra
		res[i] := StringTools:-Ord(str[i]);# [0..255]
		# res[i] fits on 3 digits, but it can also fit
		# on 2 letters (0="A",25="Z")
		tmp := convert(res[i],'base',26);
		tmp := [0$(2-nops(tmp)),op(tmp)];# list of 2 integers in [0..25]
		# StringTools:-Ord("A") = 65
		res[i] := seq(StringTools:-Char(j+65),j=tmp);
	    fi;
	od;
	cat(seq(res[i],i=1..len));
    fi;
end;	# AlphaNumeric
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	fd::`DocumentGenerator/FileDescriptor`
#	ext::{string,'VOID'}
### Output format::`DocumentGenerator/FileDescriptor`
### Description: appends the extension "ext" that does NOT
# include the ".".
############################################################
FileExtension := proc(fd,ext)
    if ext = 'VOID' then fd;
    else
	[op(1..-2,fd),cat(CommonLib:-AlphaNumeric(fd[-1],_ALPHANUM_EXCLUDE),
	`if`(ext="","",cat(".",ext)))];
    fi;
end;	# FileExtension
#>>>########################################################
#<<<########################################################
# @scope=local
### Argument sequence
#	fd::FileDescriptor
### Output format::list(string)
### Description: convert a FileDescriptor (list of anything)
# into a list of non empty strings, which is expected to be
# a valid path in the underlying OS.
############################################################
formatfiledescriptor := proc(fd)
#    map(AlphaNumeric,subs(""=NULL,fd))
    # the "." MUST not be converted into alphanumeric character
    map(AlphaNumeric,subs(""=NULL,fd),_ALPHANUM_EXCLUDE);
end;	# formatfiledescriptor
#>>>########################################################
#<<<########################################################
# @scope=export
### Description: this procedure generates a procedure for
# reading and setting the options, which are specified
# as for 'RemoveYieldOptions'.
############################################################
MakeProcOptions := proc(OPTDCL)
    _OPTION;
    proc()
	local optdcl,i,largs;
	_OPTION;
	optdcl := OPTDCL;
	if nargs = 0 then# list options
	    map2(op,1,optdcl);
	elif nargs = 1 and not type(args[1],`=`) then
	    # read option
	    for i in optdcl do
		if op(1,i) = args[1] then
		    return eval(op([2,2],i));
		fi;
	    od;
	    error "%1: invalid option",args[1];
	else# set option
	    # ??? RemoveYieldOptions is NOT evaluated if it
	    # is not called with its fully qualified name ???
	    largs := thismodule:-RemoveYieldOptions([args],optdcl);
	    if nops(largs) = 0 then NULL;
	    else error "%1: invalid arguments",largs; fi;
	    fi;
    end;
end;	# MakeProcOptions
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	fd::FileDescriptor
#	(optional) patch::string
### Output format::string
### Description: actual name that is associated to a FileDescriptor.
# The optional argument is used to patch the rootdirectory.
############################################################
FileName := proc(fd)
    local rdir;
    _OPTION;    
#    _ERR_TYPE(fd,'FileDescriptor');    
    if nargs > 1 then
	rdir := args[2];
    else
	rdir := NULL;
    fi;
    absolutefilename(formatfiledescriptor(fd),rdir);
end;	# FileName
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence:
#	sep::anything
#	args[2..-1]::anything
### Description: the sequence args[2..-1] is returned as the
# sequence args[2],sep,args[3],sep,...
############################################################
SepSeq := proc(sep)
    local i;
    _OPTION;
    if nargs = 1 then NULL
    else seq(op([args[i],sep]),i=2..nargs-1),args[-1]; fi
end;	# SepSeq
#>>>########################################################
#<<<########################################################
# @scope=export
### Argument sequence
#	fd::FileDescriptor
### Output format::NULL
### Description: open file in WRITE mode (overwrite)
############################################################
FileNew := proc(fd)
    _OPTION;
#    _ERR_TYPE(fd,'FileDescriptor');
    fileopen(formatfiledescriptor(fd),'WRITE');
    NULL;
end;	# FileNew
#>>>########################################################
### END FUNCTIONS #############################
### BEGIN RAW INCLUSION #######################
### 11/02/02: @rawinclude is used in practice for
# initializations, which must be done after the
# definitions and declarations.
#<<<########################################################
### Shared variables:
# @sharevar=therootdirectory
# @sharevar=thedirectoryseparator
### Initialization for this file:
# @rawinclude
#therootdirectory := currentdir();# this goes in moduleinit,
# since currendir() is evaluated here at module construction.
#thedirectoryseparator := "/";
#>>>########################################################
#<<<########################################################
# @exportvar=FileOptions
# @rawinclude
### Description: set/read options for File*
FileOptions := MakeProcOptions({
    'rootdir'=[proc(x) if type(x,'string') and 
	traperror(isdir(x)) = true then true;
	else error "%1: invalid root directory",x; fi; end,
	'therootdirectory']
    #,'dirsep'=['string','thedirectoryseparator']
    });
#>>>########################################################
#<<<########################################################
# @globalvar=VOID
# @rawinclude
### Description: the symbol VOID is used to denote a missing
# parameter in a sequence.
unprotect('VOID');
VOID := 'VOID';
protect('VOID');
#>>>########################################################
### END RAW INCLUSION #########################

end:
### END MODULE #################################
#SAVELIBNAME
#savelib('`type/FileDescriptor`','`type/PLOT`','`type/VOID`','`type/PLOT3D`','VOID','CommonLib');
