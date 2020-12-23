FLAGS = -fPIC -O2 -g
HFLAGS = $(FLAGS) -threaded -rtsopts -v0 -XNoImplicitPrelude -XTupleSections
DONE1 = $(wildcard [1-9][ab].hs)
DONE2 = $(wildcard [12][0-9][ab].hs)
GHCCOMMAND = $(shell command -v ghc)
DOCDIR = $(shell echo "$$(dirname $(GHCCOMMAND))/$$(dirname $$(readlink $(GHCCOMMAND)))/../share/doc")
HADDOCK_FILES = $(wildcard $(DOCDIR)/*/html/libraries/*/*.haddock)
HADDOCK_FLAGS = $(foreach file,$(HADDOCK_FILES),-i $(dir $(file)),$(file))

all: $(sort $(DONE1:%.hs=%.output)) $(sort $(DONE2:%.hs=%.output)) | doc
	@for output in $^; do /bin/echo -n "$${output/.output}: "; cat "$$output"; done

clean:
	@rm -f -- [0-9][0-9][ab] [0-9][ab] *.o *.hi *.so *.a

distclean: clean
	@rm -f -- *.output

%a.output: %a %.input
	@./$< +RTS -N8 < $*.input > $@

%b.output: %b %.input
	@./$< +RTS -N8 < $*.input > $@

%:: %.hs
	@ghc $(HFLAGS) -o $@ $^
	@rm -f -- $*.hi $*.o

doc: AOC.hs
	@rm -rf doc AOC.txt
	@haddock -o doc --html --hyperlinked-source --use-unicode $(HADDOCK_FLAGS) $< >& /dev/null
	@rm -f doc/doc-index*.html
	@haddock --hoogle --package-name aoc $< >& /dev/null

.PRECIOUS: %
.PHONY: all clean distclean doc
