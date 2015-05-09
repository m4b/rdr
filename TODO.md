# Todos

* symlinks in `darwin/usr/lib/` for various dylibs (`libz.dylib`, `libsymsea.dylib`) were resolved at copy time into their links; as a result, redundant searches are displayed for `_deflate`, etc.  Need to scrub them for testing

  * add different message if using marshalled map, like searching map marshall, or what the map marshal was generated from, etc

  * In order to implement this, require perhaps a record with meta data like name, when generated, what from, date, etc.

* bin/libsystem_c.dylib has discrepancies in the sections where non lazy symbols are imported (probably, dyld_stub_binder), and several other symbols purportedly glommed into the section; something seems amiss; the non-lazy section is only declared to be the size of a single imported symbol (8 bytes), my guess is the interpreter isn't working quiet right yet?