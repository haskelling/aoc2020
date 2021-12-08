MAKEFLAGS += --silent
FLAGS = -O2
HFLAGS = $(FLAGS) -rtsopts -v0 -XNoImplicitPrelude -XTupleSections -XBangPatterns
DONE1 = $(wildcard [1-9][ab].hs)
DONE2 = $(wildcard [12][0-9][ab].hs)
GHCCOMMAND = $(shell command -v ghc)
DOCDIR = $(shell echo "$$(dirname $(GHCCOMMAND))/$$(dirname $$(readlink $(GHCCOMMAND)))/../share/doc")
HADDOCK_FILES = $(wildcard $(DOCDIR)/*/html/libraries/*/*.haddock)
HADDOCK_FLAGS = $(foreach file,$(HADDOCK_FILES),-i $(dir $(file)),$(file))

all: $(sort $(DONE1:%.hs=%.output)) $(sort $(DONE2:%.hs=%.output)) | doc/AOC.html
	@for output in $^; do /bin/echo -n "$${output/.output}: "; cat "$$output"; done

installdeps:
	cabal install --lib vector split hashable parsec mtl extra
	cabal install --overwrite-policy=always doctest

test:
	@make > /dev/null
	@make | grep -v 'ch:' > .output
	@diff -u .expected_output .output

updatetest:
	@make > /dev/null
	@make | grep -v 'ch:' > .expected_output

prof: prof.pdf

%.pdf: %.latex
	@latex -output-format=pdf $< > /dev/null
	@rm -f *-eps-converted-to.pdf *.aux $<.log $<.dvi

prof.latex: $(sort $(DONE1:%.hs=%.eps)) $(sort $(DONE2:%.hs=%.eps))
	@echo "\\\\documentclass{article}\n\\\\usepackage[a4paper, margin=5mm]{geometry}\n\
\\\\usepackage{graphicx}\n\\\\begin{document}\n\
$(foreach file,$^,\n\\\\begin{figure}\n\\\\includegraphics[scale=1]{$(file:.eps=)}\n\\\\end{figure}\n)\
\n\\\\end{document}" > prof.latex

%a.hp: %a %.input
	@./$< +RTS -hT -i0.001 < $*.input > /dev/null

%b.hp: %b %.input
	@./$< +RTS -hT -i0.001 < $*.input > /dev/null

%.eps: %.hp
	@hp2ps -c -e200mm $<
	@mv $*.ps $*.eps

clean:
	@rm -f -- [0-9][0-9][ab] [0-9][ab] *.o *.hi *.so *.a *.hp *.eps *.aux *.latex *.log *.pdf

distclean: clean
	@rm -rf -- *.output doc aoc.txt

%a.output: %a %.input
	@./$< < $*.input > $@

%b.output: %b %.input
	@./$< < $*.input > $@

%:: %.hs
	@ghc $(HFLAGS) -o $@ $^
	@rm -f -- $*.hi $*.o

doc/AOC.html: AOC.hs
	@rm -rf doc AOC.txt
	@haddock -o doc --html --hyperlinked-source --use-unicode $(HADDOCK_FLAGS) $< > /dev/null
	@rm -f doc/doc-index*.html
	@haddock --hoogle --package-name aoc $< >& /dev/null
	@doctest AOC

.PRECIOUS: %
.PHONY: all clean distclean test updatetest prof
