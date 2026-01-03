# Environment variables:
#   + MAPLEAPP: path to the Maple executable.  This cannot be named
#     MAPLE as this would break on Cygwin.
#   + NO_NETWORK: if set, make will skip trying fetching submodules

SHELL=/bin/bash
ifeq (X$(MAPLEAPP), X)
MAPLEAPP=maple
endif
export MAPLEAPP

MLA=algolib.mla
HDB=algolib.hdb

# All .mws and .mw files, whether in help or in some submodule, are
# viewed as a help page and should be considered for recreation of
# $(HDB).  (All but those of gfun ...)
HELP_PAGES = $(shell find help gdev mgfun -name '*.mws')
HELP_PAGES_2 = $(shell find help gdev mgfun -name '*.mw')

.SUFFIXES:
.SUFFIXES: .mws .mw .ok

.PHONY: all clean clean_help very_clean compile test help export

# Still in active development or out of Maple, therefore distributed
# via algolib:
#   encyclopedia, gdev, gfun, Holonomy, Mgfun, regexpcount
# Same as in Maple (as of January 2010), therefore not distributed via algolib:
#   combstruct, Groebner, Ore_algebra, MultiSeries
# (if the latter are reactivated, they will become again part of algolib).
#
# Additionally, packages with with very low activity are not submodules,
# but directly subdirectories of algolib.

# This prepares a library ready to be tested and the help pages.
# Tests are then run by `make test'.
# A release is obtained by `make export'.
all: $(MLA) compile help

clean:
	rm -f `find . -name '*.date' -o -name '*.out' -o -name '*.tim' -o -name '*.ok' -o -name '*.mla' -o -name '*.hdb'` failed_tests.txt

clean_help:
	rm -f $(HELP_PAGES:.mws=.ok) $(HELP_PAGES_2:.mw=.ok) $(HDB)

very_clean: clean
	@echo 'Caution!  Locally committed commits that have not been pushed'
	@echo 'to the server will be lost.  Confirm?  [y/N] '
	@read c && [ "X$$c" = Xy ] && rm -rf gdev gfun mgfun || echo 'Aborted.'

$(MLA):
	echo 'march(create, "'$(MLA)'", 1000);' | $(MAPLEAPP)
	ln -f $(MLA) gdev/gdev.mla
	mkdir -p gfun/lib && ln -f $(MLA) gfun/lib/gfun.mla
	ln -f $(MLA) mgfun/Mgfun.mla
	ln -f $(MLA) regexpcount/regexpcount.mla
	ln -f $(MLA) encyclopedia/encyclopedia.mla

compile:
	$(MAKE) -C gdev algolib_compile
	$(MAKE) -C gfun algolib_compile
	$(MAKE) -C mgfun algolib_compile
	$(MAKE) -C regexpcount algolib_compile
	$(MAKE) -C encyclopedia algolib_compile
	$(MAKE) -C algolib algolib_compile

test:
	rm -f failed_tests.txt
	$(MAKE) -C gdev algolib_test
	$(MAKE) -C gfun algolib_test
	$(MAKE) -C mgfun algolib_test
	$(MAKE) -C regexpcount algolib_test
	echo ; echo 'Summary of failed tests:' ; cat failed_tests.txt ; echo '(End of summary.)'
	@grep -v 'make\[' failed_tests.txt > /tmp/ft$$.txt || true
	@! cat /tmp/ft$$.txt | grep -q .

# Other help pages are under development.
help: $(HELP_PAGES:.mws=.ok) $(HELP_PAGES_2:.mw=.ok)
	$(MAKE) -C gdev HDB=../$(HDB) algolib_help
	$(MAKE) -C gfun HDB=../$(HDB) algolib_help
	$(MAKE) -C mgfun HDB=../$(HDB) algolib_help

.mws.ok:
	@n=`echo $< | cut -d/ -f2- | sed -e 's-.mws--g'` ; \
	l=$(<:.mws=.loc) ; \
	[ -e $$l ] || { echo Non-existent $$l ; exit 1 ; } ; \
	{ cat helptool.mpl ; \
	  echo 'helptool("'$<'","'$$n'",'`cat $$l`', "'$(HDB)'") :' ; } \
	| $(MAPLEAPP) | grep -v autoerror | grep rror && { echo Problem with $$l ; exit 1 ; } || \
	{ true ; touch $*.ok ; }

.mw.ok:
	@n=`echo $< | cut -d/ -f2- | sed -e 's-.mw--g'` ; \
	l=$(<:.mw=.loc) ; \
	[ -e $$l ] || { echo Non-existent $$l ; exit 1 ; } ; \
	{ cat helptool.mpl ; \
	  echo 'helptool("'$<'","'$$n'",'`cat $$l`', "'$(HDB)'") :' ; } \
	| $(MAPLEAPP) | grep -v autoerror | grep rror && { echo Problem with $$l ; exit 1 ; } || \
	{ true ; touch $*.ok ; }
