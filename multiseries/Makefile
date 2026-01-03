SHELL=/bin/bash
ifeq (X$(MAPLEAPP), X)
MAPLEAPP=maple
endif

MLA=MultiSeries.mla

ifeq ($(shell uname), CYGWIN_NT-6.1)
SAVELIBNAME=$(shell echo C:/cygwin$(CURDIR)/$(MLA) | sed -e s%/%\\\\\\\\\\\\\\\\%g)
else
SAVELIBNAME=$(CURDIR)/$(MLA)
endif

SOURCES = \
  src/MultiSeries.mpl

TESTS = $(wildcard tst/*.tst)

.SUFFIXES:
.SUFFIXES: .mpl .ok .tst

all: $(MLA) algolib_compile

clean:
	rm `find . -name '*.ok' -o -name '*.out' -o -name '*.tim'`

$(MLA):
	echo 'march(create, "'$(MLA)'", 10);' | $(MAPLEAPP)

algolib_compile: $(SOURCES:.mpl=.ok)

.mpl.ok:
	{ echo 'savelibname := "'"$(SAVELIBNAME)"'" :' ; sed -e s/#savelib/savelib/g $< ; } | $(MAPLEAPP) && touch $*.ok

testing: $(MLA)
	$(MAKE) -j 4 tests

tests: $(TESTS:.tst=.ok)

algolib_test: testing
	for n in $(TESTS:.tst=.ok) ; do [ -e $$n ] || echo multiseries/$$n ; done >> ../failed_tests.txt

.tst.ok: $(MLA)
	$(MAPLEAPP) -s -t -b $(MLA) -B -c 'TRY := TestTools:-Try ;' $< >$*.out 2>$*.tim
	grep -q -e failed.running.the.following.code $*.out || touch $*.ok
