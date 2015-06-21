.PHONY: build

build:
	ocamlbuild.native -lib unix -lib str src/Rdr.native && mv _build/src/Rdr.native rdr
