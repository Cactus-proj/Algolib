ifeq (X$(MAPLE), X)
MAPLE=maple
endif

MLA=encyclopedia.mla
OK=encyclopedia.ok

all: init ind.m maintable.m rmmla $(OK)

init:
	make -C gdev compilation

ind.m maintable.m: database.mpl enc_functions.mpl createtable.mpl
	$(MAPLE) -b gdev/gdev.mla -B < createtable.mpl

rmmla:
	rm -f $(MLA)

$(MLA):
	echo "march('create', \"$@\", 20) ;" | $(MAPLE) -s -q

algolib_compile: init $(OK)

# Do not do any "rm" for this target, as it is used by algolib!
$(OK): $(MLA) ind.m maintable.m saveall.mpl encyclopedia.mpl
	$(MAPLE) -b $(MLA) -B saveall.mpl && touch $(OK)
