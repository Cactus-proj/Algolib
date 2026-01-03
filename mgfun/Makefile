SHELL=/bin/bash
ifeq (X$(MAPLE), X)
MAPLE=maple
endif

MLA=Mgfun.mla
HDB=Mgfun.hdb

SOURCES = \
  Lib-Mgfun/Holonomy.mpl \
  Lib-Mgfun/Mgfun.mpl \
  Lib-Mgfun/ApplyOpr_AlgSubs.mpl \
  Lib-Mgfun/qbinomial.mpl \
  Lib-Mgfun/qfactorial.mpl \
  Lib-Mgfun/qpochhammer.mpl

# For the list of excluded tests (and reasons), see KnownFailures.txt.
TESTS = $(shell find Test-Mgfun -name '*.test' | grep -v -e Holonomy/gordon_k4_r4 -e Mgfun/kelvin -e CreativeTelescoping/ising_ising3 -e CreativeTelescoping/qshift_qshift2 -e CreativeTelescoping/binom7_binom_quadruple)
HELP_PAGES = $(shell find Doc-Mgfun -name '*.mws')
HELP_PAGES_2 = $(shell find Doc-Mgfun -name '*.mw')

.SUFFIXES:
.SUFFIXES: .mpl .ok .test .tim .mws .mw

all: lib

clean: clean_lib clean_test
	rm -f $(HDB)

clean_lib:
	rm -f $(MLA) $(SOURCES:.mpl=.ok)

clean_test:
	rm -f $(TESTS:.test=.tim) $(TESTS:.test=.out)

clean_failed_test:
	rm -f `grep Test ../failed_tests.txt | cut -d\: -f1 | sed -e 's/.out/.tim/g'`
	rm -f `grep Test ../failed_tests.txt | cut -d\: -f1 | sed -e 's/.tim/.out/g'`

lib: $(MLA) algolib_compile

algolib_compile: $(SOURCES:.mpl=.ok)

# Two calls here are to make sure the .depend file is generated well.
test: lib
	$(MAKE) -C Test-Mgfun/CreativeTelescoping
	$(MAKE) -C Test-Mgfun/CreativeTelescoping
	$(MAKE) -j 4 all_tests

all_tests: $(TESTS:.test=.tim)

analyse: $(TESTS:.test=.tim)
	@grep -H Execution.stopped:.CPU.time.limit.reached $(TESTS:.test=.tim) | sed -e 's/:.*/:cpu/g'
	@grep -H Execution.stopped:.Memory.allocation.failed $(TESTS:.test=.tim) | sed -e 's/:.*/:memory/g'
	@grep -c -v -e \# -e okay $(TESTS:.test=.out) | grep -v :0$

analyse_detaillee:
	@for t in $(TESTS:.test=.out) ; do \
		echo ; echo $$t | sed -e s%Test-Mgfun/%%g -e s%.out%%g ; \
		grep -n -C 2 -v -e \# -e okay $$t || true ; \
	done

algolib_test: test
	$(MAKE) analyse >> ../failed_tests.txt

$(MLA):
	echo 'march(create, "'$(MLA)'", 10);' | $(MAPLE)

Lib-Mgfun/Holonomy.ok: Lib-Mgfun/hilbert_dimension_do_it.mpl Lib-Mgfun/fglm_algorithm.mpl Lib-Mgfun/fglm_termination_proc.mpl Lib-Mgfun/Holonomy/*.mm

Lib-Mgfun/Mgfun.ok: Lib-Mgfun/Mgfun/*.mm Lib-Mgfun/Mgfun/CreativeTelescoping/*.mm

.mpl.ok:
	{ echo 'savelibname := "'$(CURDIR)/$(MLA)'" :' ; \
	 sed -e s/#savelib/savelib/g $< ; \
	} | $(MAPLE) -I Lib-Mgfun && touch $*.ok

# Limit to 3 minutes and 2 GB of data.
# This may require resetting your ulimit -v/-t beforehand.
.test.tim:
	@echo `date` $< | tee -a logs.txt
	@ulimit -v 2097152 && $(MAPLE) -s -t -T 180 -b $(MLA) -B < $< >$*.out 2>$*.tim | grep -v -e \# -e okay || true
	@{ echo -e -n date= ; date ; echo -e -n host= ; hostname ; } >>$*.tim

algolib_help: $(HELP_PAGES:.mws=.ok) $(HELP_PAGES_2:.mw=.ok)

.mws.ok:
	@n=`echo $< | cut -d/ -f2- | sed -e 's-.mws--g'` ; \
	l=$(<:.mws=.loc) ; \
	[ -e $$l ] || { echo Non-existent $$l ; exit 1 ; } ; \
	{ cat helptool.mpl ; \
	  echo 'helptool("'$<'","'$$n'",'`cat $$l`', "'$(HDB)'") :' ; } \
	| $(MAPLE) | grep rror && { echo Problem with $$l ; exit 1 ; } || \
	{ true ; touch $*.ok ; }

.mw.ok:
	@n=`echo $< | cut -d/ -f2- | sed -e 's-.mw--g'` ; \
	l=$(<:.mw=.loc) ; \
	[ -e $$l ] || { echo Non-existent $$l ; exit 1 ; } ; \
	{ cat helptool.mpl ; \
	  echo 'helptool("'$<'","'$$n'",'`cat $$l`', "'$(HDB)'") :' ; } \
	| $(MAPLE) | grep rror && { echo Problem with $$l ; exit 1 ; } || \
	{ true ; touch $*.ok ; }
