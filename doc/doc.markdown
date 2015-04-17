---
title: Mach-O Binaries
author: m4b
layout: post
tags: [reverse engineering, hacking, symbols, imports, exports, trie, fsa, blog, tutorial, osx, mac, object, binary, binaries, linux]
---

> εἰ γὰρ ὤφελον, ὦ Κρίτων, οἷοί τ᾽ εἶναι οἱ πολλοὶ τὰ μέγιστα κακὰ ἐργάζεσθαι, ἵνα οἷοί τ᾽
> ἦσαν καὶ ἀγαθὰ τὰ μέγιστα, καὶ καλῶς ἂν εἶχεν. νῦν δὲ οὐδέτερα οἷοί τε: οὔτε γὰρ
> φρόνιμον οὔτε ἄφρονα δυνατοὶ ποιῆσαι, ποιοῦσι δὲ τοῦτο ὅτι ἂν τύχωσι.

![/usr/lib dependencies](lib_dependency_graph_all.png)

This is a writeup detailing some of the peculiarities of the mach binary format.

There are several resources available locally, and on the internet, but none of them were/are very comprehensive:

* Apple C headers, primarily check out:
  1. `/usr/include/mach-o/loader.h` (I've read this header so many times it's probably embarassing; what's more embarassing though are the typos and the grammatically incorrect sentences commenting important structures)
  2. and to a lesser extent, `/usr/include/mach-o/stab.h` and `/usr/include/mach-o/nlist.h`
* Old article [mostly about dyld](http://newosxbook.com/articles/DYLD.html). The bind opcode discussion somewhat helpful, but very incomplete
* Read up on your [Leb128](http://en.wikipedia.org/wiki/LEB128) byte streams
* But if you need to *really* figure out the bind opcodes, you should just slog through the [dyld source code](https://opensource.apple.com/source/dyld/dyld-353.2.1/src/ImageLoaderMachOCompressed.cpp), paying particular attention to the method `ImageLoaderMachOCompressed::eachBind`
* And once you're done figuring out the bind opcodes, in order to understand mach exports you should cross reference the comments in `/usr/include/mach-o/loader.h` for the `dyld_info_command` struct with two methods in dyld's source code for loading modern "compressed" binary images (`ImageLoaderMachOCompressed.cpp`): `ImageLoaderMachOCompressed::trieWalk` and `ImageLoaderMachOCompressed::findExportedSymbol`.  Hint: it's a [trie](http://en.wikipedia.org/wiki/Trie).

I'm sure there are other resources, but they're usually old.  I couldn't find much, sorry.  

Mostly, this is simply a record so I do not forget.

## Load Commands

So, after the mach header (which I won't detail here), a mach binary has a sequence of load commands.  A simple C program like:

````c
#include<stdio.h>

long int maximum(long int x, long int y){
  return x > y ? x : y;
}

int main (){

  long int max = maximum(2,3);

  printf("max: %lu\n", max);

  return 0;
}
````

will probably contain about 15-16 load commands.  Load commands essentially contain important information like:
  * where the segments are (the place where code and data are in the binary), `LC_SEGMENT_64` for 64 bit binaries
  * whether it's a library, `LC_ID_DYLIB` (or its soname if you're coming from a linux background)
  * what libraries it imports,  `LC_LOAD_DYLIB` et. al. (which is important for OSX's support for two-level namespaces, which are awesome BTW)
  * what program interpreter it uses, `LC_LOAD_DYLINKER`, usually `/usr/lib/dyld`
  * where the main entry point is, `LC_MAIN` ( for OSX10.5 or greater: before that it's `LC_UNIXTHREAD`, which requires the link editor to embed `/usr/lib/crt1.o` in every binary, which sets up the stack, etc.)
  * the all-important `LC_DYLD_INFO_ONLY` (or `LC_DYLD_INFO`), which is essentially OSX's `_DYNAMIC`, or the place where you can get all the information you need for imports and exports (even when the binary is stripped)
  * the linux/bsd nlist symbol table, given by `LC_SYMTAB`, which can be stripped
  * and so on and so forth.

If you compile the above C program with something like `gcc -o max max.c`, you can use a tool like `otool -l max` to look at the load commands.  `otool` is OSX's `objdump`, sort of.

My way cooler program (currently) outputs:

````
MachO x86-64 EXECUTE
( 0): SEGMENT_64 (0x19) 72
( 1): SEGMENT_64 (0x19) 552
( 2): SEGMENT_64 (0x19) 232
( 3): SEGMENT_64 (0x19) 72
( 4): DYLD_INFO_ONLY (0x80000022) 48
	rebase_off: 0x2000 rebase_size: 8
	bind_off: 0x2008 bind_size: 24
	weak_bind_off: 0x0 weak_bind_size: 0
	lazy_bind_off: 0x2020 lazy_bind_size: 16
	export_off: 0x2030 export_size: 64
( 5): SYMTAB (0x2) 24
	symoff: 0x20b8 nsyms: 5 stroff: 0x2118 strsize: 64
( 6): DYSYMTAB (0xb) 80
	ilocalsym: 0x0 nlocalsym: 0 iextdefsym: 0x0 nextdefsym: 3
	iundefsym: 0x3 nundefsym: 2 tocoff: 0x0 ntoc: 0
	modtaboff: 0x0 nmodtab: 0 extrefsymoff: 0x0 nextrefsyms: 0
	indirectsymoff: 0x2108 nindirectsyms: 4 extreloff: 0x0 nextrel: 0
	locreloff: 0x0 nlocrel: 0
( 7): LOAD_DYLINKER (0xe) 32
( 8): UUID (0x1b) 24
( 9): VERSION_MIN_MACOSX (0x24) 16
	version: OSX 10.10 sdk: OSX 10.10
(10): SOURCE_VERSION (0x2a) 16
(11): MAIN (0x80000028) 24
	offset: 0xf50 stacksize: 0x0
(12): LOAD_DYLIB (0xc) 56
	/usr/lib/libSystem.B.dylib
(13): FUNCTION_STARTS (0x26) 16
(14): DATA_IN_CODE (0x29) 16
(15): DYLIB_CODE_SIGN_DRS (0x2b) 16

Libraries (1)
Exports (3)
Imports (2)
````

## Libraries

OSX, for better or worse (better), uses a two-level namespace for resolving undefined symbol references (imports).  What that means is that imported symbols have a pointer to *which* library they're from; which is _awesome_.

If you've got a million imports, do you know how a flat namespace dynamic library loader will resolve that undefined symbol?  Yes, it will load each referenced library,  searching its exported symbols until it finds a match, and then returns that symbol (or rather the offset to the symbol + the virtual memory address of the dylib in the plt or got).

So for flat namespace lookups:

1. Symbol lookup is linear in the size of the number of imported symbols, in the worst case
2. The first match is the resolved symbol, so the chance of namespace collisions and using the wrong symbol (at no fault of your own), is more likely.  But then again, library writers probably shouldn't be naming their symbols "foo" anyway.

Moreover, from a static analysis perspective, two level namespaces make it easier to analyze binaries, their dependencies, etc., since the imports tell you where they come from, statically, so you don't have to run the program, like `ldd` does, which is safer from a security perspective.

Of course this kind of simple static analysis won't catch dynamic loading of libraries at runtime using the standard api at `/usr/include/dlfcn.h`, but, like, whatever, man.

Returning to mach imports, while there are some edge cases, which I'll detail in the imports section, basically the pointer is an index to what can be thought of as an array of libraries.  The array is formed by taking the libraries in the order they appear in the load commands, and indexing them in that manner, **starting from 1**.

So if I have two libraries, `/usr/lib/libfoo.1.dylib` and `/usr/lib/libbar.1.dylib`, and 3 imported symbols, they will either have 1 or 2 as their index, for `libfoo` and `libbar`, respectively.

A binary can also reference libraries using variable path prefixes, of which there are three:

* `@rpath`
* `@executable_path`
* `@loader_path`

You'll typically come across `@rpath`; if you feel like hacking around with your own dylib, you'll probably use `@rpath` or `@executable_path`.  

You should probably read the Apple [runpath dependent library documentation](https://developer.apple.com/library/mac/documentation/DeveloperTools/Conceptual/DynamicLibraries/100-Articles/RunpathDependentLibraries.html), if you're more curious, because I always forget the exact details.

## Symbols

All mach imported and exported symbol information is contained in the linkedit segment.

The linkedit segment is usually described by the last `LC_SEGMENT_64` load command, with the descriptive name "__LINKEDIT"; it contains lots of information, like the nlist symbol table, and the imports, as well as code signing details, rebasing and relocation info, etc.

If you're curious, the first place you should head to is `/usr/lib/mach-o/loader.h` (once again) and find the `dyld_info_command` struct, where a comment like this (as of March 2015) awaits you:

> The dyld_info_command contains the file offsets and sizes of
> the new compressed form of the information dyld needs to
> load the image.  This information is used by dyld on Mac OS X
> 10.6 and later.  All information pointed to by this command
> is encoded using byte streams, so no endian swapping is needed
> to interpret it.

Byte streams?  If you haven't already, time to go learn [Uleb128](http://en.wikipedia.org/wiki/LEB128#Unsigned_LEB128) and [Sleb128](http://en.wikipedia.org/wiki/LEB128#Signed_LEB128), and preferably implement it.

For future posterity and reference, let's actually just put the whole struct declaration from the header, glorious comments and all, right here:

````c
struct dyld_info_command {
   uint32_t   cmd;		/* LC_DYLD_INFO or LC_DYLD_INFO_ONLY */
   uint32_t   cmdsize;		/* sizeof(struct dyld_info_command) */

    /*
     * Dyld rebases an image whenever dyld loads it at an address different
     * from its preferred address.  The rebase information is a stream
     * of byte sized opcodes whose symbolic names start with REBASE_OPCODE_.
     * Conceptually the rebase information is a table of tuples:
     *    <seg-index, seg-offset, type>
     * The opcodes are a compressed way to encode the table by only
     * encoding when a column changes.  In addition simple patterns
     * like "every n'th offset for m times" can be encoded in a few
     * bytes.
     */
    uint32_t   rebase_off;	/* file offset to rebase info  */
    uint32_t   rebase_size;	/* size of rebase info   */

    /*
     * Dyld binds an image during the loading process, if the image
     * requires any pointers to be initialized to symbols in other images.  
     * The bind information is a stream of byte sized
     * opcodes whose symbolic names start with BIND_OPCODE_.
     * Conceptually the bind information is a table of tuples:
     *    <seg-index, seg-offset, type, symbol-library-ordinal, symbol-name, addend>
     * The opcodes are a compressed way to encode the table by only
     * encoding when a column changes.  In addition simple patterns
     * like for runs of pointers initialzed to the same value can be
     * encoded in a few bytes.
     */
    uint32_t   bind_off;	/* file offset to binding info   */
    uint32_t   bind_size;	/* size of binding info  */

    /*
     * Some C++ programs require dyld to unique symbols so that all
     * images in the process use the same copy of some code/data.
     * This step is done after binding. The content of the weak_bind
     * info is an opcode stream like the bind_info.  But it is sorted
     * alphabetically by symbol name.  This enable dyld to walk
     * all images with weak binding information in order and look
     * for collisions.  If there are no collisions, dyld does
     * no updating.  That means that some fixups are also encoded
     * in the bind_info.  For instance, all calls to "operator new"
     * are first bound to libstdc++.dylib using the information
     * in bind_info.  Then if some image overrides operator new
     * that is detected when the weak_bind information is processed
     * and the call to operator new is then rebound.
     */
    uint32_t   weak_bind_off;	/* file offset to weak binding info   */
    uint32_t   weak_bind_size;  /* size of weak binding info  */

    /*
     * Some uses of external symbols do not need to be bound immediately.
     * Instead they can be lazily bound on first use.  The lazy_bind
     * are contains a stream of BIND opcodes to bind all lazy symbols.
     * Normal use is that dyld ignores the lazy_bind section when
     * loading an image.  Instead the static linker arranged for the
     * lazy pointer to initially point to a helper function which
     * pushes the offset into the lazy_bind area for the symbol
     * needing to be bound, then jumps to dyld which simply adds
     * the offset to lazy_bind_off to get the information on what
     * to bind.  
     */
    uint32_t   lazy_bind_off;	/* file offset to lazy binding info */
    uint32_t   lazy_bind_size;  /* size of lazy binding infs */

    /*
     * The symbols exported by a dylib are encoded in a trie.  This
     * is a compact representation that factors out common prefixes.
     * It also reduces LINKEDIT pages in RAM because it encodes all  
     * information (name, address, flags) in one small, contiguous range.
     * The export area is a stream of nodes.  The first node sequentially
     * is the start node for the trie.  
     *
     * Nodes for a symbol start with a uleb128 that is the length of
     * the exported symbol information for the string so far.
     * If there is no exported symbol, the node starts with a zero byte.
     * If there is exported info, it follows the length.  
	 *
	 * First is a uleb128 containing flags. Normally, it is followed by
     * a uleb128 encoded offset which is location of the content named
     * by the symbol from the mach_header for the image.  If the flags
     * is EXPORT_SYMBOL_FLAGS_REEXPORT, then following the flags is
     * a uleb128 encoded library ordinal, then a zero terminated
     * UTF8 string.  If the string is zero length, then the symbol
     * is re-export from the specified dylib with the same name.
	 * If the flags is EXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER, then following
	 * the flags is two uleb128s: the stub offset and the resolver offset.
	 * The stub is used by non-lazy pointers.  The resolver is used
	 * by lazy pointers and must be called to get the actual address to use.
     *
     * After the optional exported symbol information is a byte of
     * how many edges (0-255) that this node has leaving it,
     * followed by each edge.
     * Each edge is a zero terminated UTF8 of the addition chars
     * in the symbol, followed by a uleb128 offset for the node that
     * edge points to.
     *  
     */
    uint32_t   export_off;	/* file offset to lazy binding info */
    uint32_t   export_size;	/* size of lazy binding infs */
};
````

The various offset and size values in the struct fields contain all the information one needs to extract the imported and exported symbols in a binary, **_even after stripping_**.

Mach exports and mach imports use a trie, and a finite state automaton, respectively, to compress their information.  Time to brush the dust off that old CS theory book.

### Exports

Mach exports, or globally visible symbols, or things you can link against without error, or "a subset of the things you are able to reference in a C header file", reside in the linkedit segment, in a data structure called a trie which you probably learned about in your CS Algorithms I class, and then promptly forgot.  

And now here comes the pain (actually this is probably the coolest part of mach binaries, once you figure it out, in spite of the obtuse and questionably grammatical documentation).

To better illustrate this point, it helps to have a very simple dynamic library.

Here you go:

````c
//libtoc.c
int toc_extern_export = 0xb1b1eb0b;
const long kTOC_MAGICAL_FUN = 0xdeadbeef;

long int toc_maximum(long int x, long int y){
  return x > y ? x : y;
}

long int toc_XX_unicode(long int x, long int y){
  return x * y << 2;
}
````

````c
//libtoc.h
#ifndef _LIBTOC_H_
#define _LIBTOC_H_
extern const long kTOC_MAGICAL_FUN;

/*
 a constant definition "exported" by library
 i quote it because it isn't really exported, as it doesn't show up in the exports anywhere
 it basically exists emphemerally in any source code which #includes this header,
 and then disappears after compilation,
 as no symbol is associated with it, and it returns to it's pure anonymous,
 integer brothers and sisters
 of course this has advantages wrt client side speed, since the value can
 typically be used as an immediate, no memory reads are performed,
 and the dynamic linker doesn't have to load the value into the got
*/
#define TOC_MAX_FOO  20

// a global variable exported by library
// if you don't know what extern means you should probably look that up
// .. it helps to think that this file, when you #include it, will pretty much
// be transcluded into your source file, after the c preprocessor runs
extern int toc_extern_export;

// function prototypes for a function exported by library:
extern long int toc_maximum(long int x, long int y);
extern long int toc_XX_unicode(long int x, long int y);

#endif
````

Also don't make fun of my C.

We'll use that header later, but for now, we can compile the above on OSX (or linux but this isn't an article on Elf dude) using:

`clang -dynamiclib -o libtoc.dylib libtoc.c`

or

`clang -shared -o libtoc.dylib libtoc.c` or `gcc -shared -o libtoc.dylib libtoc.c`

Not sure what the precise difference is between `-dynamiclib` and `-shared`; on my system, the binaries have the same size, but differ.

For fun, run `nm -a libtoc.dylib` and check the output, should be something like:

````
0000000000000f90 S _kTOC_MAGICAL_FUN
0000000000000f70 T _toc_XX_unicode
0000000000001000 D _toc_extern_export
0000000000000f30 T _toc_maximum
                 U dyld_stub_binder
````

Now for even more fun, run `strip libtoc.dylib` and rerun `nm` again; it should complain that there isn't a name list... because there isn't, you stripped it.

Unfortunately, `nm` uses the nlist symbol table to generate its output, and after you strip it, it's pretty worthless on OSX (not so much on linux, with the right flags; unless of course you `sstrip` the binary, then you're in a sort of similar circumstance to what we're in now).

Fortunately, we can still statically determine the symbols that `libtoc` exports by getting the exports offset from the `dyld_info_command` struct field, and walking the export trie.

#### Export Trie

Using `otool -l` and grepping through the output for `LC_DYLD_INFO_ONLY` to find the `export_off` value, I highly recommend opening up a hex editor (`M-x hexl-find-file` if you're cool), and jumping to that location in the binary.  Mine is 0x2000, yours will probably be different.

When you do, you'll see something like this:

````
00002000: 0001 5f00 0500 0274 6f63 5f00 1f6b 544f  .._....toc_..kTO
00002010: 435f 4d41 4749 4341 4c5f 4655 4e00 4f00  C_MAGICAL_FUN.O.
00002020: 036d 6178 696d 756d 0045 5858 5f75 6e69  .maximum.EXX_uni
00002030: 636f 6465 004a 6578 7465 726e 5f65 7870  code.Jextern_exp
00002040: 6f72 7400 5403 00b0 1e00 0300 f01e 0003  ort.T...........
00002050: 0090 1f00 0300 8020 0000 0000 0000 0000  ....... ........
````

which is the entire export trie, since `export_size` indicates it is 96 bytes (0x60).

A mach export trie can be summarized as follows:

1. Every node either has terminal string information, or not.  There is at most one (1) terminal string information at a mach trie node.

  a. If the node does not have terminal information, then this is marked by a 0x00 byte.

  b. If it does, then instead of 0x00, a uleb128 represents the _size_ of the terminal string information at this node (not including the number of bytes used for the size uleb).  This is called "optional exported information". **Note:** the dyld source code notes that the only time the export information size is greater than 127 is when the terminal type is a "reexport with name". We'll get to this, but the byte 127 is very special for leb128 encodings.  Its binary representation is: 0111 1111; i.e., it's the maximum integer we can represent without using another Leb128 byte, because a clear 8th bit marks the end of the leb128 byte stream.

2. If the node has exported information, following the size is a byte for the export symbol flags. Exported symbols have both a **kind** and for lack of a better word, a **type**.  To get the kind, you need to bitmask the flag with 0x3 to get the "kind" (i.e., it's in the lower two bits of the bytes' lower nibble), of which there are three possible cases (only 2 of which are documented in `loader.h`):

    * a regular symbol (0x00);
    * a thread local symbol (0x01)
    * an absolute symbol (0x02) ("documented" in dyld source code as an #ifndef in `ImageLoaderMachOCompressed.cpp`, line 42)

  A symbol also has four types, starting from the upper two bits of the flag's lower nibble, which determine the information that follows:

    * regular (type bits are empty, i.e., the flag is equal to the kind), with a uleb128 address following
    * weak (0x4), same as a regular symbol, except it's a weak definition, and dyld can return 0 if not found, and that's OK (I think)
    * a reexport (0x8), with a uleb128 library ordinal indexing into the library "array" where the real symbol is found, followed by an optional null terminated string designating the symbol's name in the library it's reexported from (this is when the exported information size can be greater than 127)
    * a stub (0x10), with a uleb128 stub offset followed by a uleb128 resolver offset

  Thread local symbols and absolute symbols _**cannot**_ be a stub type, otherwise dyld throws an exception (line 662 in `ImageLoaderMachOCompressed.cpp`).

  Most symbols are regular symbols, which give the offset in the binary to the symbol.

  Reexports are less common.  If you're familiar with GNU IFUNCS, they're sort of similar.  For example, you'll find reexports in `/usr/lib/system/libsystem_c.dylib` for some famous symbols you may remember like: `_strcmp`, `_memcpy`, `_strcpy` and other routines, known for such things as: major security vulnerabilities throughout the years.  The reexport library ordinal typically points to platform versions of these routines in `/usr/lib/system/libsystem_platform.dylib`, with "__platform" prefixed to the symbol name.

  Stubs are also less common (I think it's an Objective C Thing™; I don't see them in `libstdc++.dylib` or various Swift dylibs like `libswiftCore.dylib`). I don't really know what it means, but they're present in `/usr/lib/libobjc.dylib`, with `_objc_getProperty` being an example.

  Weak symbols I've typically only seen in `/usr/lib/libstdc++.dylib`, so I could be wrong about the interpretation, and it might be a C++ Thing™.

3. After a 0x00 node byte, or the optional exported information, is a _byte_ (not a uleb128 according to documentation) indicating the number of branches leaving this node (offset to num branches byte = num bytes in size uleb128 + size of optional exported information). Hence, there can only be 255 branches from a single node.  This is fine for ascii, but _not_ fine for unicode, cause, there's, like, a lot of those, man.  And yes, mach-o binaries support unicode symbols.  And yes, you should probably be nodding your head in deep appreciation for this scintillating piece of sweetness implemented by the OSX devs.

4. After the number of branches, follow the branches, one after the other.  A branch consists of a:

  1. A null-terminated string representing the prefix at that node in the trie, and

  2. A uleb128 ostensibly representing the branches' pointer to its child node, but in reality, an offset **_from the start of the trie_** to the next node

5. And finally, as my favorite Professor-qua-logician liked to call it when recursively describing logical formulae, the "woody woodpecker clause": that's all folks.

That was a lot of text.  I love examples, so in the interest of generating more text, I'll walk the trie from `libtoc.dylib` using a program I wrote, and give some in depth analysis of the first few recursions.  Here is the output:

````
export init: 0x2000 0x2060

@ 0x2000 node: 0x0 current_symbol:
	BRAN 1
	(0) string: _ next_node: 0x2005

@ 0x2005 node: 0x0 current_symbol: _
	BRAN 2
	(0) string: _toc_ next_node: 0x201f
	(1) string: _kTOC_MAGICAL_FUN next_node: 0x204f

@ 0x204f node: 0x3 current_symbol: _kTOC_MAGICAL_FUN
	TERM 0 flags: 0x0
	0xf90 _kTOC_MAGICAL_FUN E -> libtoc.so  

@ 0x201f node: 0x0 current_symbol: _toc_
	BRAN 3
	(0) string: _toc_maximum next_node: 0x2045
	(1) string: _toc_XX_unicode next_node: 0x204a
	(2) string: _toc_extern_export next_node: 0x2054

@ 0x2054 node: 0x3 current_symbol: _toc_extern_export
	TERM 0 flags: 0x0
	0x1000 _toc_extern_export E -> libtoc.so  

@ 0x204a node: 0x3 current_symbol: _toc_XX_unicode
	TERM 0 flags: 0x0
	0xf70 _toc_XX_unicode E -> libtoc.so  

@ 0x2045 node: 0x3 current_symbol: _toc_maximum
	TERM 0 flags: 0x0
	0xf30 _toc_maximum E -> libtoc.so  
````

We start at the offset 0x2000 (given by `export_off` in the dyld info load command), and read a uleb128 for the node size, if any.  Our current symbol is the empty string.  So, we're right here:

````
00002000: 0001 5f00 0500 0274 6f63 5f00 1f6b 544f  .._....toc_..kTO
````

So we read the first byte, 0x00, then 0x01 (1 branch), which is followed by the (one) branch composed of the byte sequence: 0x5f 0x00 0x05.  It has the symbol "\_" (0x5f is ascii/UTF-8 for "\_"), the null terminator, and the offset to its child node, which is 5 bytes from 0x2000, or 0x2005 (0x05 in uleb128 is 5; bytes greater than 0x7f are multi-byte uleb128 numbers).

Since the algorithm above is depth first, and depth first searches are the best (especially on the internet, as a colleague used to say), let's goto 0x2005.  At 0x2005 we see 0x00 again (count 5 bytes in from the above hex), which tells use the node has no optional information; the next uleb128 tells us that there are 0x02 (2) branches, the first of which has the symbol "toc\_" (from the bytes 0x7f 0x6f 0x63 0x5f 0x00 in ascii/UTF-8), and points to its child node at 0x2000 + 0x1f, which is in the next line of the hex.

````
00002010: 435f 4d41 4749 4341 4c5f 4655 4e00 4f00  C_MAGICAL_FUN.O.
````

We still need to finish the second branch though, which is just after the byte 0x1f and starts with 0x6b on the first line of hex; the branch has the symbol "kTOC\_MAGICAL\_FUN" (which bleeds into the second line of hex above) and points to 0x2000 + 0x4f.

The trie walk repeats in this manner until we run out of nodes (or reach the trie start offset + trie size, in which case something went wrong).  The only difference is when a node also has terminal export symbol information; we can see an example of this in the node at offset 0x2054, whose current symbol is "kTOC\_MAGICAL\_FUN" (which is built up in the course of searching):

````
00002050: 0090 1f00 0300 8020 0000 0000 0000 0000  ....... ........
````

4 bytes in from 0x2050 lands us at the byte 0x03.  This is a uleb128 designating the size (3) of the export information at this node.  The next byte (0x00) is our export symbol flags, which means it's a regular symbol; after that we're given the uleb128 (0x80 0x20) representing its offset.  This is a two byte uleb128 sequence since 0x80 is greater than 0x7f, and whose value for brevity's sake is 0xf90 --- in other words the offset to the symbol "kTOC\_MAGICAL\_FUN" in the dynamic library `libtoc.dylib` binary is 0xf90.

If you've still got that hex editor running, jump to 0xf90 in `libtoc.dylib` and you should see `ef be ad de` --- dead beef, bro.  If you don't know why it appears like that I'm pretty amazed you read this far, but you should probably stop. ಠ_ಠ

Lastly, and **very importantly**, the _next_ uleb128 after whatever type the symbol is (so, depending on the type, it could be several bytes or uleb's later, see point 2. in the mach trie summary), is the number of child branches.  In the case of "kTOC_MAGICAL_FUN", the number is 0.  But, it doesn't have to be.  The node with terminal export information can also have children; a good example of this can be found in `/usr/lib/system/libsystem_c.dylib` with `_printf` and `_printf_l`, where the latter will be a child node containing the export information for `_printf`; don't terminate your loop (or recursion if you're awesome) when you encounter a "terminal" node, as strange as that sounds, because it might have children.

### Imports

![darwin /usr/lib/ imports wordmap](linux_imports.png)

Imports on mach are stored in the same linkedit section as exports are.  As mentioned earlier, they are  "encoded" using a finite state automaton.  The FSA accepts so-called "bind opcodes", which have immediate and uleb128/sleb128 arguments, which alter the current record.  When a "bind" bind opcode is encountered, it pushes the current import symbol information record onto a list or array, however you want to think about it.

Importantly, that same record, and all of its values, are modified by further bind commands; when a "done" opcode is encountered, the current record is pushed, and _then_ the record is cleared, starting you off with all zero values again.

What this scheme accomplishes is a dense encoding of the import information in the binary; many times only the symbol name is changed, and then the record is bound (because dyld auto-increments the binary offset/pointer value on a bind).

It's also important to note that there are three bind "sections":

* bind/non-lazy, (usually data variables, like exported ints)
* weak (a C++ Thing™)
* lazy (this will typically be exported functions from the library)

The non-lazy imports are bound on program load; thus they're already present on the program's entry to main.  

Weak symbols are bound after the non-lazy symbols are bound:

>    Some C++ programs require dyld to unique symbols so that all
>    images in the process use the same copy of some code/data.
>    This step is done after binding. The content of the weak_bind
>    info is an opcode stream like the bind_info.  But it is sorted
>    alphabetically by symbol name.  This enable dyld to walk
>    all images with weak binding information in order and look
>    for collisions.  If there are no collisions, dyld does
>    no updating.  That means that some fixups are also encoded
>    in the bind_info.  For instance, all calls to "operator new"
>    are first bound to libstdc++.dylib using the information
>    in bind_info.  Then if some image overrides operator new
>    that is detected when the weak_bind information is processed
>    and the call to operator new is then rebound.

The lazy symbols are bound at runtime.

I highly recommend reading the dyld source code if you want to understand dyld's import binding process.  There are particular details (like incrementing by a pointer size after each done call) that you can't really understand by staring at the structure, or reading the paltry `loader.h` comments.  `ImageLoaderMachOCompressed::eachBind` should send you on your way.

But if that isn't your cup of tea, you're in luck, because I'll do an in-depth writeup here. Since imports require a dynamic library to link against, and we just created and went over the exports of a simple dynamic library, we can use `libtoc.dylib`'s header file in a simple C program to illustrate mach imports.

Here is a simple C program using `libtoc.dylib`'s exported symbols:

````c
//toc.c
#include<stdio.h>
#include "include/libtoc.h"

int main (){
  long int max = toc_maximum(2,3);
  printf ("kTOC_MAGICAL_FUN: 0x%lx\n", kTOC_MAGICAL_FUN);
  printf ("toc_extern_export: 0x%x\n", toc_extern_export);
  printf ("===FUNS===\n");
  printf ("toc_XX_unicode: 0x%lu\n", toc_XX_unicode(3, 5));
  printf("toc_maximum: %lu\n", max);

  return 0;
}
````

To get this working right, without installing random libs into your `/usr/lib`, I'm going to assume you're in some folder called `toc` (where `toc` resides doesn't matter; put it in `~` or `/tmp/` for all I care).  In addition, you should have the following files and directories _exactly_ as follows:  `toc/include/libtoc.h` and `toc/lib/libtoc.dylib` and `toc/toc.c`.  

Now, we need to recompile the dynamic lib (or use `install_name_tool`, but if you know how to do that, you can compile `toc.c` as normal and then run it on the compiled binary to use `libtoc.dylib` without recompilation).

There are a number of ways to do this, but this should work:

`gcc -dynamiclib -install_name @executable_path/lib/libtoc.dylib lib/libtoc.c -o lib/libtoc.dylib`

What this did was generate a dynamic library named `libtoc.dylib` with an install name (`LC_DYLD_ID`) of "@executable_path/lib/libtoc.dylib".  What _this_ does is basically require any binary linking against this newly recompiled `libtoc.dylib` to be **executed** in a folder which contains a folder named `lib`, which contains `libtoc.dylib` --- which if you setup your directory structure above, is the case.  It's weird, but whatever.

You should be able to compile `toc.c` now with something like:

`gcc -o toc -Llib -ltoc  toc.c`

And _now_, you should be able to execute `toc`, which prints:

````
kTOC_MAGICAL_FUN: 0xdeadbeef
toc_extern_export: 0xb1b1eb0b
===FUNS===
toc_XX_unicode: 0x60
toc_maximum: 3
````

If not, either my directions are wrong, or you didn't follow my directions (exclusive or).

#### Mach Import Bind FSA

Now that we've got a running mach-o binary with imports, we can run something like `otool -l | grep bind_` to get the import binding offsets.  My own program's output tells me:

````
( 4): DYLD_INFO_ONLY (0x80000022) 48
       rebase_off: 0x2000 rebase_size: 8
 	     bind_off: 0x2008 bind_size: 80
	     weak_bind_off: 0x0 weak_bind_size: 0
	     lazy_bind_off: 0x2058 lazy_bind_size: 56
	     export_off: 0x2090 export_size: 48
````

Which says that the there aren't any weak bindings (only relevant for C++) and that the regular, non-lazy bindings start at offset 0x2008 and the lazy bindings (typically functions) start at 0x2058.

Open your favorite hex editor again and jump to your `bind_off` (as I said, mine is 0x2008, yours will probably be different).  You should see something like:

````
00002000: 1122 2053 0000 0000 1140 5f6b 544f 435f  ." S.....@_kTOC_
00002010: 4d41 4749 4341 4c5f 4655 4e00 5172 1090  MAGICAL_FUN.Qr..
00002020: 405f 746f 635f 6578 7465 726e 5f65 7870  @_toc_extern_exp
00002030: 6f72 7400 9012 4064 796c 645f 7374 7562  ort...@dyld_stub
00002040: 5f62 696e 6465 7200 80e0 ffff ffff ffff  _binder.........
00002050: ffff 0190 0000 0000 7220 1140 5f74 6f63  ........r .@_toc
````

**Note**: we start at 0x2008, and end at 0x2008 + 0x4f (because our size is 0x50, which is 80 in decimal), which means we start at that 0x11 byte, and end just before the 0x72 byte.

The FSA always starts (or is initialized with) with a blank record, which looks something like:

````
seg_index: 0 seg_offset: 0x0 lib_ordinal: 0 type: 0 flags: 0 special_dylib: 1
````

The special dylib isn't actually a field, but it's useful consider it as one.  You see this value set by the BIND_OPCODE_SET_DYLIB_SPECIAL_IMM opcode, which says the import comes from itself, from the main executable, or is a flat lookup symbol (like linux).

When the FSA starts its loop (or recurses), it always eats a single byte, which it then bitmasks with 0xF0 to get the bind "opcode".  All of the valid (import) bind opcodes are as follows:

````
0x00 ->  BIND_OPCODE_DONE
0x10 ->  BIND_OPCODE_SET_DYLIB_ORDINAL_IMM
0x20 ->  BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB
0x30 ->  BIND_OPCODE_SET_DYLIB_SPECIAL_IMM
0x40 ->  BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM
0x50 ->  BIND_OPCODE_SET_TYPE_IMM
0x60 ->  BIND_OPCODE_SET_ADDEND_SLEB
0x70 ->  BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB
0x80 ->  BIND_OPCODE_ADD_ADDR_ULEB
0x90 ->  BIND_OPCODE_DO_BIND
0xa0 ->  BIND_OPCODE_DO_BIND_ADD_ADDR_ULEB
0xb0 ->  BIND_OPCODE_DO_BIND_ADD_ADDR_IMM_SCALED
0xc0 ->  BIND_OPCODE_DO_BIND_ULEB_TIMES_SKIPPING_ULEB
````

Our FSA starts on this line of hex, at offset 0x8:

````
00002000: 1122 2053 0000 0000 1140 5f6b 544f 435f  ." S.....@_kTOC_
````

So in our case, we're looking at `BIND_OPCODE_SET_DYLIB_ORDINAL_IMM` (0x11 & 0xF0 == 0x10), which means to set the library ordinal field to the value in the immediate argument place.

All immediate arguments are obtained by bitmasking the byte with, you guessed it, 0x0F.  In this case, the library indexed by 1, which is our `libtoc.dylib` (or rather `@executable_path/lib/libtoc.dylib`).

The dylib ordinal imm opcode is a single byte in length, so we eat another byte (0x40) and examine its opcode: BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM.

Our immediate is 0x0, which tells us to set the flags to 0x0 (which means it is neither a weak import 0x1 or a weak definition 0x8).  Now, for this opcode, what follows is the symbol name, which is a null-terminated UTF-8 string.

This takes us into the second line of hex:

````
00002010: 4d41 4749 4341 4c5f 4655 4e00 5172 1090  MAGICAL_FUN.Qr..
````

At the 11th byte, we hit our null terminator, which yields the symbol name "kTOC_MAGICAL_FUN".  We are now free to eat another opcode byte, which is 0x51.

Our table tells us that this is BIND_OPCODE_SET_TYPE_IMM, which sets the type of the import to 0x1.

The types of an import are:

* BIND_TYPE_POINTER (1)
* BIND_TYPE_TEXT_ABSOLUTE32 (2)
* BIND_TYPE_TEXT_PCREL32 (3)

You'll almost always see pointer.

We now eat another opcode byte, this time 0x72: BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB.  This opcode tells us to set the segment index to the immediate value (2), and its segment offset value to the uleb128 that follows, in this case: 0x10.

This means that after dyld runs, it will set the value of this symbol (*symbol) to what the pointer at offset 0x10 in segment 2 in the binary is pointing to.  In other words, if you `lldb toc` and set a breakpoint on main, then examine this address, you'll find more 0xdeadbeef.

In any event, eating the offset uleb128 puts us exactly at the byte 0x90, which is the opcode for BIND_OPCODE_DO_BIND.

Now, this is **extremely important** if you're writing your own implementation: the record up to this point is now _committed_ (in dyld's case, it sends it off to a handler), and **_then_** the seg_offset is incremented by the size of a platform pointer (so 32 or 64 bits), and the bind FSA recurses with the record intact (or loops depending on your implementation).

You can see it in this switch case in dyld's source code (also note the _sweet_ typo in "symboFlags"):

````c
case BIND_OPCODE_DO_BIND:
      if ( address >= segmentEndAddress )
        throwBadBindingAddress(address, segmentEndAddress, segmentIndex, start, end, p);
      (this->*handler)(context, address, type, symbolName, symboFlags, addend, libraryOrdinal, "", &last, false);
      address += sizeof(intptr_t);
      break;
````

And that's pretty much it; the FSA marches on until it reaches the end of the bind import information.

It's also important to note that the BIND_OPCODE_DONE opcode doesn't actually say "stop reading import symbol information"; rather it resets the import symbol information back to their zero values.

As for the meanings and structure of the other opcodes, they're pretty straightforward, except perhaps the scaling opcodes.  Usually all the hints you need are in the opcode names.  Just read the dyld source code if any of them confuse you.

Lastly, I've attached the output of my bind opcode program running on the `toc` binary:

````
BIND_OPCODE_SET_DYLIB_ORDINAL_IMM -> 0x11
0x0 with  seg_index: 0 seg_offset: 0x0 lib_ordinal: 0 type: 0 flags: 0 special_dylib: 1

BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM -> 0x40
0x1 with  seg_index: 0 seg_offset: 0x0 lib_ordinal: 1 type: 0 flags: 0 special_dylib: 1

BIND_OPCODE_SET_TYPE_IMM -> 0x51
0x14 with _kTOC_MAGICAL_FUN seg_index: 0 seg_offset: 0x0 lib_ordinal: 1 type: 0 flags: 0 special_dylib: 1

BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB -> 0x72
0x15 with _kTOC_MAGICAL_FUN seg_index: 0 seg_offset: 0x0 lib_ordinal: 1 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_DO_BIND -> 0x90
0x17 with _kTOC_MAGICAL_FUN seg_index: 2 seg_offset: 0x10 lib_ordinal: 1 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM -> 0x40
0x18 with _kTOC_MAGICAL_FUN seg_index: 2 seg_offset: 0x18 lib_ordinal: 1 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_DO_BIND -> 0x90
0x2c with _toc_extern_export seg_index: 2 seg_offset: 0x18 lib_ordinal: 1 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_SET_DYLIB_ORDINAL_IMM -> 0x12
0x2d with _toc_extern_export seg_index: 2 seg_offset: 0x20 lib_ordinal: 1 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM -> 0x40
0x2e with _toc_extern_export seg_index: 2 seg_offset: 0x20 lib_ordinal: 2 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_ADD_ADDR_ULEB -> 0x80
0x40 with dyld_stub_binder seg_index: 2 seg_offset: 0x20 lib_ordinal: 2 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_DO_BIND -> 0x90
0x4b with dyld_stub_binder seg_index: 2 seg_offset: 0x0 lib_ordinal: 2 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_DONE -> 0x0
0x4c with dyld_stub_binder seg_index: 2 seg_offset: 0x8 lib_ordinal: 2 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_DONE -> 0x0
0x4d with  seg_index: 0 seg_offset: 0x0 lib_ordinal: 0 type: 0 flags: 0 special_dylib: 1

BIND_OPCODE_DONE -> 0x0
0x4e with  seg_index: 0 seg_offset: 0x0 lib_ordinal: 0 type: 0 flags: 0 special_dylib: 1

BIND_OPCODE_DONE -> 0x0
0x4f with  seg_index: 0 seg_offset: 0x0 lib_ordinal: 0 type: 0 flags: 0 special_dylib: 1
End of opcode stream

BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB -> 0x72
0x0 with  seg_index: 0 seg_offset: 0x0 lib_ordinal: 0 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_SET_DYLIB_ORDINAL_IMM -> 0x11
0x2 with  seg_index: 2 seg_offset: 0x20 lib_ordinal: 0 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM -> 0x40
0x3 with  seg_index: 2 seg_offset: 0x20 lib_ordinal: 1 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_DO_BIND -> 0x90
0x14 with _toc_XX_unicode seg_index: 2 seg_offset: 0x20 lib_ordinal: 1 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_DONE -> 0x0
0x15 with _toc_XX_unicode seg_index: 2 seg_offset: 0x28 lib_ordinal: 1 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB -> 0x72
0x16 with  seg_index: 0 seg_offset: 0x0 lib_ordinal: 0 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_SET_DYLIB_ORDINAL_IMM -> 0x11
0x18 with  seg_index: 2 seg_offset: 0x28 lib_ordinal: 0 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM -> 0x40
0x19 with  seg_index: 2 seg_offset: 0x28 lib_ordinal: 1 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_DO_BIND -> 0x90
0x27 with _toc_maximum seg_index: 2 seg_offset: 0x28 lib_ordinal: 1 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_DONE -> 0x0
0x28 with _toc_maximum seg_index: 2 seg_offset: 0x30 lib_ordinal: 1 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB -> 0x72
0x29 with  seg_index: 0 seg_offset: 0x0 lib_ordinal: 0 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_SET_DYLIB_ORDINAL_IMM -> 0x12
0x2b with  seg_index: 2 seg_offset: 0x30 lib_ordinal: 0 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM -> 0x40
0x2c with  seg_index: 2 seg_offset: 0x30 lib_ordinal: 2 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_DO_BIND -> 0x90
0x35 with _printf seg_index: 2 seg_offset: 0x30 lib_ordinal: 2 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_DONE -> 0x0
0x36 with _printf seg_index: 2 seg_offset: 0x38 lib_ordinal: 2 type: 1 flags: 0 special_dylib: 1

BIND_OPCODE_DONE -> 0x0
0x37 with  seg_index: 0 seg_offset: 0x0 lib_ordinal: 0 type: 1 flags: 0 special_dylib: 1
End of opcode stream
````

## Coda

There's a lot more to talk about; rebase opcodes, estimating routine sizes, dependency graphs, unicode symbols, etc., etc.

The graph that adorns the beginning of this article is generated from my program, which computes the dynamic library interdependencies in `/usr/lib/*` (really any directory I specify).  I plan to open source the software at some point soon; hopefully after elf binary analysis is properly integrated.

The software tool is completely written in OCaml, and many thanks to the researchers at INRIA and the small, but dedicated community surrounding the language.

But I'll just end the article with another graph my tool generates, this time of the much linked-against `libz.dylib`, in all its logical splendor:

![libz](libz.dylib.gv.png)
