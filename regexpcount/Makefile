ifeq (X$(MAPLE), X)
MAPLE=maple
endif

SOURCES=regexpcount.mpl
TESTS=regexpcount.test
MLA=regexpcount.mla

.SUFFIXES:
.SUFFIXES: .mpl .date .test .ok

all:
	@echo 'This package is intended to be used as part of algolib only.'

algolib_compile: $(SOURCES:.mpl=.date)

testing: $(MLA)
	$(MAKE) -j 4 tests

tests: $(TESTS:.test=.ok)

algolib_test: testing
	for n in $(TESTS:.test=.ok) ; do [ -e $$n ] || echo regexpcount/$$n ; done >> ../failed_tests.txt

.mpl.date:
	sed -e 's/#savelib/savelib/g' $< | $(MAPLE) -s -q -e2 -b $(MLA) -B && touch $*.date

.test.ok: $(MLA)
	$(MAPLE) -s -t -b $(MLA) -B $< >$*.out 2>$*.tim
	grep -v -e \# -e okay $*.out || touch $*.ok
