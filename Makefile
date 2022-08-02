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

.PHONY: all compile clean check
