MODULES=deck command main
OBJECTS=$(MODULES:=.cmo)
TEST=deck_test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

docs:
	mkdir -p doc
	ocamldoc -d doc -html deck.mli

clean:
	ocamlbuild -clean
	rm -rf doc
