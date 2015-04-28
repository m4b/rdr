# Todos

* symlinks in `darwin/usr/lib/` for various dylibs (`libz.dylib`, `libsymsea.dylib`) were resolved at copy time into their links; as a result, redundant searches are displayed for `_deflate`, etc.  Need to scrub them for testing

* add `-f` command line switch in combination with `-b`, instead of using the anon arg (or make the anon arg an optional version of that)

  * add different message if using marshalled map, like searching map marshall, or what the map marshal was generated from, etc