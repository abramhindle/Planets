ifndef OCAMLC 
	OCAMLC=ocamlc
endif
ifndef OCAMLOPT
	OCAMLOPT=ocamlopt
endif
ifndef CAMLP4O
	CAMLP4O=camlp4o
endif
ifndef PREFIX
	PREFIX=/usr/local
endif

INCLUDES=-I +labltk
CAMLP4=-pp $(CAMLP4O)
OCAMLDEP=ocamldep
CAMLLIBS=unix.cma str.cma labltk.cma # libjpf.cma mylibs.cma
OCAMLFLAGS=$(INCLUDES) $(CAMLP4) -g -custom $(CAMLLIBS) -cclib -lunix 
OCAMLOPTFLAGS=$(INCLUDES) $(CAMLP4) $(CAMLLIBS:.cma=.cmxa) -inline 50 -cclib -lunix

ifdef PROFILING
  OCAMLC=ocamlcp
  OCAMLOPTFLAGS = -p $(INCLUDES) $(CAMLLIBS:.cma=.cmxa) -inline 40 -cclib -lunix
endif

OBJS =  augSet.cmx augMap.cmx mTimer.cmx common.cmx lstrings.cmx options.cmx \
	constants.cmx fqueue.cmx state.cmx saveState.cmx \
	rk4.cmx fast_physics.cmx \
	collision.cmx physics.cmx help.cmx display.cmx

VERSION := $(shell cat VERSION)
PREFIX = planets-$(VERSION)
FILES := $(shell sed -e s/.*/$(PREFIX)\\/\&/ FILES)
BINFILES := $(shell sed -e s/.*/$(PREFIX)\\/\&/ BINFILES)

all: planets planets.1.gz

install: planets.1.gz
	if [ -x planets ]; then install planets $(PREFIX)/bin/planets; fi
	if [ -x planets.bc ]; then install planets.bc $(PREFIX)/bin/planets; fi
	if [ -x $(PREFIX)/share/applications ]; \
	then install planets.desktop $(PREFIX)/share/applications; fi
	if [ -x $(PREFIX)/share/applnk/Games ]; \
	then install planets.desktop $(PREFIX)/share/applnk/Games; fi
	if [ -x $(PREFIX)/share/pixmaps ]; \
	then install planets.png $(PREFIX)/share/pixmaps; fi
	if [ -x $(PREFIX)/share/man/man1 ]; \
	then install planets.1.gz $(PREFIX)/share/man/man1; fi

planets.1.gz: planets.1
	gzip -c planets.1 > planets.1.gz

rpm: planets.spec src
	rpmbuild -ta ../planets-$(VERSION).tgz

src: planets.spec
	if [ ! -x planets-$(VERSION) ]; then ln -s . planets-$(VERSION); fi
	tar cfz ../planets-$(VERSION).tgz $(FILES)
	rm planets-$(VERSION)

w32: planets
	cp /usr/bin/cygwin1.dll .
	zip -r ../planets.zip planets.exe cygwin1.dll getting_started.html \
	README.txt KEYBINDINGS.txt LICENSE CHANGES BUGS CREDITS \
	COPYING VERSION uni.*
	rm cygwin1.dll

bin: all
	if [ ! -x planets-$(VERSION) ]; then ln -s . planets-$(VERSION); fi
	tar cfz ../planets-x86_Linux-$(VERSION).tgz $(BINFILES)
	rm planets-$(VERSION)

planets: $(OBJS)
	$(OCAMLOPT) -o planets $(OCAMLOPTFLAGS) $^

planets.bc: $(OBJS:.cmx=.cmo)
	$(OCAMLC) -o planets.bc $(OCAMLFLAGS) $^

test: test.ml
	$(OCAMLC) -o test $(OCAMLFLAGS) $^

sqrt: sqrt.ml
	$(OCAMLOPT) -o sqrt $(OCAMLOPTFLAGS) $^

collision: constants.cmx options.cmx fqueue.cmx state.cmx collision.cmx
	$(OCAMLOPT) -o collision $(OCAMLOPTFLAGS) $^

convert: convert.ml
	$(OCAMLC) -o convert $(OCAMLFLAGS) $^


common.ml: common.src.ml VERSION
	sed s/__VERSION__/$(VERSION)/ < common.src.ml > common.ml

planets.spec: planets.src.spec VERSION
	sed s/__VERSION__/$(VERSION)/ < planets.src.spec > planets.spec

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f *.[0-9].gz
	rm -f *.exe
	rm -f *.obj
	rm -f *.o
	rm -f *.cm[iox]
	rm -f planets
	rm -f planets.bc

# Dependencies
dep:
	$(OCAMLDEP)  $(CAMLP4) $(INCLUDES) *.ml *.mli > .depend

include .depend

