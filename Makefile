OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=byte

example:
	$(OCAMLBUILD) example.$(TARGET)

serveur:
	$(OCAMLBUILD) serveur.$(TARGET)

network:
	$(OCAMLBUILD) network.$(TARGET)

clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
