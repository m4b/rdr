#!/bin/bash
ocamlbuild.native -lib unix -lib str src/Rdr.native && mv Rdr.native rdr
