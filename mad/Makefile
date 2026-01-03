ifeq (X$(MAPLEAPP), X)
MAPLEAPP=maple
endif

MLA=MAD.mla

SOURCES = CommonLib.mpl DocumentGenerator.mpl LaTeX.mpl HTMX.mpl MAD.mpl

.SUFFIXES:
.SUFFIXES: .mpl .date

all:
	@echo 'This package is intended to be used as part of algolib only.'

algolib_compile: $(SOURCES:.mpl=.date)

.mpl.date:
	sed -e 's/#savelib/savelib/g' $< | $(MAPLEAPP) -s -q -e2 -b $(MLA) -B && touch $*.date
