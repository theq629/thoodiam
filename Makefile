NAME=roguelike
OCAMLBUILDOPTS=-use-ocamlfind -cflag -g -pkg containers,gamestuff
OCAMLBUILD=ocamlbuild $(OCAMLBUILDOPTS) -I src
PROGS=roguelike

PROGFILES=$(addsuffix .native, $(PROGS))

all: bin

bin:
	$(OCAMLBUILD) $(PROGFILES)

clean:
	ocamlbuild -clean
