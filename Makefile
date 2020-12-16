MODULES=deck command main blackjack authors tools poker player gamestate baccarat
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg yojson

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

docs: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package ANSITerminal\
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

zip:
	zip src.zip *.ml* _tags .merlin .ocamlinit Makefile INSTALL.txt *.json

clean:
	ocamlbuild -clean
	rm -rf doc.private src.zip
