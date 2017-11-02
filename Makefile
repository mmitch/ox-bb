SOURCES=$(wildcard *.el)
TARGETS=$(addsuffix c,$(SOURCES))

%.elc: %.el
	emacs -Q --batch -f batch-byte-compile $<

all:	compile

compile: $(TARGETS)

clean:
	rm -f *.elc
	rm -f *~
