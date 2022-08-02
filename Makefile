### Makefile for beardbolt.  Lifted from Makefile for Eglot.
EMACS?=emacs
ELFILES := beardbolt.el
ELCFILES := $(ELFILES:.el=.elc)

all: compile

%.elc: %.el
	$(EMACS) -Q -L . --batch -f batch-byte-compile $<

compile: $(ELCFILES)

clean:
	find . -iname '*.elc' -exec rm {} \;

benchmark: compile
	$(EMACS) -Q -L . --batch -l beardbolt-benchmark starters/slow-to-process.cpp
	$(EMACS) -Q -L . --batch -l beardbolt-benchmark starters/vector-emplace-back.cpp
	$(EMACS) -Q -L . --batch -l beardbolt-benchmark starters/unordered-multimap-emplace.cpp

.PHONY: all compile clean check
