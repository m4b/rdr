#!/bin/bash

ocamlbuild.native -cflags -bin-annot src/Rdr.native -- bin/test && mv Rdr.native rdr
