# Feature TODOs

This is a non-exhaustive list of some features that would be nice.

1. 32-bit ELF and 32-bit mach.
   Honestly, not too interested in implementing this, but will be more than happy to accept a pull request which does implement it.  It shouldn't be too hard, just more tedious and boring than anything.  Philosophically, 32-bit binaries in my opinion are dead, or at least should be, and the only reason we still have them are because of crappy windows game designers linking against 32-bit libraries and porting their shoddy code to linux, and strangely enough, hacker dudes who write reams of tutorials on 32-bit binary structure, disassembly, etc.  The faster we usher their death on in the real world, the better.  Nevertheless, practicality rules, and if there is enough demand, should eventually implement...

2. Versioned and/or named symbol maps.  So, `rdr -m <name of map>` to use a specific map, say built with `rdr -b <name of map>`; similarly, versioning maps so that upgrades don't cause segfaults :disappointed:.

3. Faster map; don't just marshall, use a custom binary trie, similar to the export trie in mach binaries, to do fast symbol lookups ourselves.  Much of the time spent looking up symbols is the initial load of the 70 meg file to marshall into the map; the lookup is a fraction of that time.  I'd prefer to just walk the binary tree with an open fd in `O(log(n))` every time, instead of the large overhead of reading the _entire_ file in; this becomes especially prevalent on linux when `rdr -i <path to bin>` is invoked.  Write now the `ToL` ref cell is a _hack_.  And of course, the larger the trie, especially with something like `rdr -r -b "/usr/bin/ /usr/local"` might become prohibitive w.r.t load times, since the map could in principle be hundreds of megs.

4. x86 interpreter.  Yea, I know it's crazy; but this is the feature I'll be working on.  Maybe it's too much, but I want it, for other reasons...  Even a simple one would be worth the time and wheel reinvention of creating an x86 backend/AST.

5. Of course the interpreter would also need a disassembler; for philosophical reasons I'd prefer the backend disassembler **not** use GNU BFD or llvm; the philosophical reason is essentially outlined in [a blog post](http://www.m4b.io/the-fault-is-not).  The gist is that multiple redundancy is scientific; think of it as triangulation of the semantics of an instruction set.  Of course interim solutions (hacks) are welcome to get functionality off the ground, i.e., the -D flag in `rdr -m -D -f printf`.

6. A completely unsafe, hacky compiler for some unspecified binary/assembly-ish language.  Rationale: so I mess around with instructions a lot; assembly doesn't work, because I want to input, for example, `0x66 0xe8 0x44 0xff 0x7f 0xff 0xff`, and see what the _silicon_ says that instruction sequence is.  Right now I load up a precompiled C binary with integer sigils or repeated instruction sequences, and replace them with whatever I want in a hex editor.  It's laborious and not very rewarding.  Prototype cycles are slow.  A more structured approach would be nice.  Most importantly, whatever name this assembly/binary language has _must_ be awesome.


# Bug TODOs

	* symlinks in `darwin/usr/lib/` for various dylibs (`libz.dylib`, `libsymsea.dylib`) were resolved at copy time into their links; as a result, redundant searches are displayed for `_deflate`, etc.  Need to scrub them for testing
	* add different message if using marshalled map, like searching map marshall, or what the map marshal was generated from, etc
	* In order to implement this, require perhaps a record with meta data like name, when generated, what from, date, etc.
	* `bin/libsystem_c.dylib` has discrepancies in the sections where non lazy symbols are imported (probably, `dyld_stub_binder`), and several other symbols purportedly glommed into the section; something seems amiss; the non-lazy section is only declared to be the size of a single imported symbol (8 bytes), my guess is the interpreter isn't working quiet right yet?
