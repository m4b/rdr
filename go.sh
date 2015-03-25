#!/bin/bash
ocamlbuild.native -lib unix -lib str src/Rdr.native -- bin/toc && mv Rdr.native rdr
# && ./graph.sh toc_call_graph.gv
