#!/bin/bash
ocamlbuild.native -lib unix -lib str src/Rdr.native -- bin/other && mv Rdr.native rdr
# && ./graph.sh toc_call_graph.gv
