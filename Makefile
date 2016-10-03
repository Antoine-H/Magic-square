localS:
	ocamlbuild -use-ocamlfind src/local/Main.native

complete:
	ocamlbuild -use-ocamlfind src/complete/Complete.native

all:
	make localS
	make complete

clean:
	rm -Rf _build/
