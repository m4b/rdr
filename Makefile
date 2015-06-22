.PHONY: build install

build:
	ocamlbuild.native -lib unix -lib str src/Rdr.native && mv _build/src/Rdr.native rdr && rm Rdr.native

install:
	cp rdr ${HOME}/bin/
