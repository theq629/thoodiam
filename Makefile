NAME=roguelike
NEEDPKGS=containers,gamestuff
OCAMLBUILDOPTS=-use-ocamlfind -cflag -g
OCAMLBUILDNATIVEOPTS=$(OCAMLBUILDOPTS) -no-plugin -pkg $(NEEDPKGS),curses
OCAMLBUILDNATIVE=ocamlbuild $(OCAMLBUILDNATIVEOPTS) -I src
OCAMLBUILDJSOPTS=$(OCAMLBUILDOPTS) -pkg $(NEEDPKGS) -plugin-tag 'package(js_of_ocaml.ocamlbuild)'
OCAMLBUILDJS=ocamlbuild $(OCAMLBUILDJSOPTS) -I src

PROGS=roguelike_curses
WEBPROGS=roguelike_htmlcanvas

PROGFILES=$(addsuffix .native, $(PROGS))
WEBPROGFILES=$(addsuffix .html, $(WEBPROGS)) $(addsuffix .css, $(WEBPROGS)) $(addsuffix .js, $(WEBPROGS))

all: bin web

bin:
	$(OCAMLBUILDNATIVE) $(PROGFILES)

web:
	$(OCAMLBUILDJS) $(WEBPROGFILES)

%.js: .FORCE
	$(OCAMLBUILD) $(OCAMLBUILDOPTS) $@

clean:
	ocamlbuild -clean
