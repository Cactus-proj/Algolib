SHELL=/bin/bash
ifeq (X$(MAPLE), X)
MAPLE=maple
endif

MLA=MultiSeries.mla

SOURCES = \
  src/MultiSeries.mpl

TESTS = $(wildcard tst/*.tst)

.SUFFIXES:
.SUFFIXES: .mpl .ok .tst

all: $(MLA) algolib_compile

clean:
	rm $(find . -name '*.ok' -o -name '*.out' -o -name '*.tim')

$(MLA):
	echo 'march(create, "'$(MLA)'", 10);' | $(MAPLE)

algolib_compile: $(SOURCES:.mpl=.ok)

.mpl.ok:
	{ echo 'savelibname := "'$(CURDIR)/$(MLA)'" :' ; \
	 sed -e s/#savelib/savelib/g $< ; \
	} | $(MAPLE) && touch $*.ok

testing: $(MLA)
	$(MAKE) -j 4 tests

tests: $(TESTS:.tst=.ok)

algolib_test: testing
	for n in $(TESTS:.tst=.ok) ; do [ -e $$n ] || echo multiseries/$$n ; done >> ../failed_tests.txt

.tst.ok: $(MLA)
	$(MAPLE) -s -t -b $(MLA) -B -c 'TRY := TestTools:-Try ;' $< >$*.out 2>$*.tim
	grep -q -e failed.running.the.following.code $*.out || touch $*.ok
