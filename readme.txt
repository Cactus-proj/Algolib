Algolib is a collection of Maple packages that has been developped by
the Algorithms Project at INRIA since 1991.

The current maintainers are:
- Frederic Chyzak (frederic.chyzak@inria.fr), mainly for Mgfun
- Bruno Salvy (bruno.salvy@inria.fr), mainly for gfun

VERSION
The present version, 13.0, is intended to run under Maple13 and should work with Maple14.

INSTALLATION
1. Download the files algolib.mla and algolib.hdb
2. Prepend the location of the directory containing those files to the variable libname:
	libname:="path_to_directory_containing_algolib",libname:
3. Check that the installation was correct by typing
	_algolibcontent();
which should show the current content of algolib and indicate the version number.

Each package of Algolib can then be used using the command with (e.g., "with(Mgfun);" or "with(gfun);").

LICENSE
Algolib is released under LGPL v2.1.  See lgpl-2.1.txt.
