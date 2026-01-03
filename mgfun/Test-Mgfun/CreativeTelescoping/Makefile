SHELL=/bin/bash

DEPEND_EXISTS=$(shell test -f .depend && echo yes || echo no)

TESTS = $(shell grep -H '<test name="[^"]*">' tests/*.xml | sed -e s/:/\"/g | cut -d\" -f1,3 | sed -e s/.xml\"/_/g | cut -c7- | while read a ; do echo $$a.test ; done | sort)

.PHONY: all clean files

all: .depend files

.depend: tests/*.xml
	@for n in tests/*.xml ; do \
	  b=`basename $$n .xml` ; \
	  echo `grep '<test name="[^"]*">' $$n | cut -d\" -f2 | sort | while read a ; do echo "$$b"_"$$a".test ; done`: $$n ; echo -e "\t"./gener_test.pl --tools_path=Test-Mgfun/CreativeTelescoping $$n ; echo ; \
	done > .depend

ifeq ($(DEPEND_EXISTS),yes)
include .depend
endif

clean:
	rm -f .depend *.test

ifeq ($(DEPEND_EXISTS),yes)
files: $(TESTS)
else
files:
endif
