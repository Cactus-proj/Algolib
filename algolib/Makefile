ifeq (X$(MAPLEAPP), X)
MAPLEAPP=maple
endif

MLA=../algolib.mla

SOURCES = _algolibcontent.mpl

.SUFFIXES:
.SUFFIXES: .mpl .date

all:
	@echo 'This package is intended to be used as part of algolib only.'

algolib_compile: $(SOURCES:.mpl=.date)

.mpl.date:
	sed -e 's/#savelib/savelib/g' $< | $(MAPLEAPP) -s -q -e2 -b $(MLA) -B && touch $*.date
