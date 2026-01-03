ifeq (X$(MAPLE), X)
MAPLE=maple
endif

SRC=$(shell cat package_list.txt | while read n ; do echo $$n.mpl ; done)
TESTS=$(shell cat test_list.txt | while read n ; do echo $$n.test ; done)
MLA=gdev.mla

.SUFFIXES:
.SUFFIXES: .mpl .date .test .ok

all: compilation testing

compilation: $(MLA)
	$(MAKE) algolib_compile

algolib_compile: $(SRC:.mpl=.date)

testing: $(MLA)
	$(MAKE) -j 4 tests

tests: $(TESTS:.test=.ok)

algolib_test: testing
	for n in $(TESTS:.test=.ok) ; do [ -e $$n ] || echo gdev/$$n ; done >> ../failed_tests.txt

$(MLA):
	rm -f $@
	echo "march('create', \"$@\", 150) ;" | $(MAPLE) -s -q

.mpl.date:
	sed -e 's/#savelib/savelib/g' $< | $(MAPLE) -s -q -e2 -b $(MLA) -B && touch $*.date

.test.ok: $(MLA)
	$(MAPLE) -s -t -b $(MLA) -B $< >$*.out 2>$*.tim
	grep -v -e \# -e okay $*.out || touch $*.ok
