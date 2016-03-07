NAME=thoodiam
NEEDPKGS=containers,gamestuff
PROGS=thoodiam_curses
WEBPROGS=thoodiam_htmlcanvas
OCAMLBUILDOPTS=-use-ocamlfind -cflag -g

OCAMLBUILD=ocamlbuild $(OCAMLBUILDOPTS) -I src -I src/curses -no-plugin -pkg $(NEEDPKGS),curses
OCAMLBUILDJS=ocamlbuild $(OCAMLBUILDOPTS) -I src -I src/htmlcanvas -pkg $(NEEDPKGS) -plugin-tag 'package(js_of_ocaml.ocamlbuild)'

PROGFILES=$(addsuffix .native, $(PROGS))
WEBPROGFILES=$(addsuffix .html, $(WEBPROGS)) $(addsuffix .css, $(WEBPROGS)) $(addsuffix .js, $(WEBPROGS))

all: bin js

bin:
	$(OCAMLBUILD) $(PROGFILES)

js:
	$(OCAMLBUILDJS) $(WEBPROGFILES)

%.js: .FORCE
	$(OCAMLBUILD) $(OCAMLBUILDOPTS) $@

clean:
	ocamlbuild -clean
