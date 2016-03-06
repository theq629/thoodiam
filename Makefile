NAME=roguelike
OCAMLBUILDOPTS=-use-ocamlfind -cflag -g -pkg containers,gamestuff,curses
OCAMLBUILD=ocamlbuild $(OCAMLBUILDOPTS) -I src
PROGS=roguelike_curses

PROGFILES=$(addsuffix .native, $(PROGS))

all: bin

bin:
	$(OCAMLBUILD) $(PROGFILES)

clean:
	ocamlbuild -clean
