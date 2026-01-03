ifeq (X$(MAPLE), X)
MAPLE = maple
endif

# May be overriden when this Makefile is called recursively from that of
# algolib/ddmf/whatever
MLA = lib/gfun.mla
HDB = lib/gfun.hdb


# Delete the default suffixes, then set up our own
.SUFFIXES: 
.SUFFIXES: .mpl .date .tst .out .ok

.PHONY: all clean test gtest ngtest testing algolib_compile \
    algolib_help algolib_test


all: $(MLA) $(HDB)

# Note that the following rule deletes lib/gfun.mla.  We may eventually
# split this rule to suit versions of the top-level Makefile of algolib
# that create a symlink gfun/lib/gfun.mla -> somewhere/algolib.mla.
clean:
	rm -f *~ */*~ */*/*~ tst/*.{out,ok} NumGfun/test/*.{out,ok} \
	    lib/* *.date */*.date

### MLA

SOURCEDIRS = -I . -I NumGfun
SRC = gfun.mpl ratinterp.mi *.mm NumGfun/*.mm ported/*.mm

# %.date records the last time the contents of %.mpl were added to
# $(MLA). Note that we cannot write a generic rule %.date: %.mpl (unless
# we add a target like
# gfun.mpl: $(SRC)
# 	touch gfun.mpl
# which is probably not a good idea)
gfun.date: $(SRC)
	sed -e 's/^#savelib/savelib/' gfun.mpl | \
	  ${MAPLE} -s -q -e2 ${SOURCEDIRS} -b $(MLA) -B && touch $@

$(MLA): $(SRC)
	mkdir -p lib
	rm -f $(MLA) gfun.date
	echo "march('create',\"${MLA}\",500);" | ${MAPLE} -s -q
	$(MAKE) gfun.date

### HDB

HELPSRC = help/makehelp.mpl help/*.mws help/NumGfun/*.mws

# same mechanism as above
lib/.hdbdate: $(HELPSRC)
	$(MAPLE) -c 'THE_HDB := \"'$(HDB)'\" \;' help/makehelp.mpl \
	    > /dev/null  &&  touch $@

$(HDB): $(HELPSRC)
	mkdir -p lib
	rm -f $@ lib/.hdbdate
	$(MAKE) lib/.hdbdate

### Tests

GFUN_TESTS = $(wildcard tst/*.tst)
NUMGFUN_TESTS = $(wildcard NumGfun/test/*.tst)
TESTS = $(GFUN_TESTS) $(NUMGFUN_TESTS)

#%.out: %.tst gfun.date NumGfun/test/testutils.mm
%.out: %.tst $(MLA_STATUS) NumGfun/test/testutils.mm
	ulimit -v 1048576 && \
	    ${MAPLE} -s -t -q -e2 -A2 -b "lib/" -B -I NumGfun/test < $< > $@

.out.ok:
	grep -q -e failed.running.the.following.code $*.out || touch $*.ok

test: $(TESTS:.tst=.out)

gtest: $(GFUN_TESTS:.tst=.out)

ngtest: $(NUMGFUN_TESTS:.tst=.out)

testing:
	$(MAKE) -j 4 test


### algolib API

algolib_compile: gfun.date

algolib_help: lib/.hdbdate

algolib_test: testing $(TESTS:.tst=.ok)
	for n in $(TESTS:.tst=.ok) ; \
	    do \
	    	[ -e $$n ] || echo gfun/$$n ; \
	    done >> ../failed_tests.txt

