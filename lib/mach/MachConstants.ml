(* Convienence constants for return values from dyld_get_sdk_version() and friends. *)
let kDYLD_MACOSX_VERSION_10_4  = 0x000A0400
let kDYLD_MACOSX_VERSION_10_5  = 0x000A0500
let kDYLD_MACOSX_VERSION_10_6  = 0x000A0600
let kDYLD_MACOSX_VERSION_10_7  = 0x000A0700
let kDYLD_MACOSX_VERSION_10_8  = 0x000A0800
let kDYLD_MACOSX_VERSION_10_9  = 0x000A0900
let kDYLD_MACOSX_VERSION_10_10 = 0x000A0A00

let kDYLD_IOS_VERSION_2_0 =  0x00020000
let kDYLD_IOS_VERSION_2_1 =  0x00020100
let kDYLD_IOS_VERSION_2_2 =  0x00020200
let kDYLD_IOS_VERSION_3_0 =  0x00030000
let kDYLD_IOS_VERSION_3_1 =  0x00030100
let kDYLD_IOS_VERSION_3_2 =  0x00030200
let kDYLD_IOS_VERSION_4_0 =  0x00040000
let kDYLD_IOS_VERSION_4_1 =  0x00040100
let kDYLD_IOS_VERSION_4_2 =  0x00040200
let kDYLD_IOS_VERSION_4_3 =  0x00040300
let kDYLD_IOS_VERSION_5_0 =  0x00050000
let kDYLD_IOS_VERSION_5_1 =  0x00050100
let kDYLD_IOS_VERSION_6_0 =  0x00060000
let kDYLD_IOS_VERSION_6_1 =  0x00060100
let kDYLD_IOS_VERSION_7_0 =  0x00070000
let kDYLD_IOS_VERSION_7_1 =  0x00070100
let kDYLD_IOS_VERSION_8_0 =  0x00080000

(* Segment and Section Constants *)

(*
 * The flags field of a section structure is separated into two parts a section
 * type and section attributes.  The section types are mutually exclusive (it
 * can only have one type) but the section attributes are not (it may have more
 * than one attribute).
 *)
let kSECTION_TYPE =   0x000000ff (* 256 section types *)
let kSECTION_ATTRIBUTES =  0xffffff00 (*  24 section attributes *)

(* Constants for the type of a section *)
let kS_REGULAR =  0x0 (* regular section *)
let kS_ZEROFILL =  0x1 (* zero fill on demand section *)
let kS_CSTRING_LITERALS = 0x2 (* section with only literal C strings*)
let kS_4BYTE_LITERALS = 0x3 (* section with only 4 byte literals *)
let kS_8BYTE_LITERALS = 0x4 (* section with only 8 byte literals *)
let kS_LITERAL_POINTERS = 0x5 (* section with only pointers to *)
     (*  literals *)
(*
 * For the two types of symbol pointers sections and the symbol stubs section
 * they have indirect symbol table entries.  For each of the entries in the
 * section the indirect symbol table entries, in corresponding order in the
 * indirect symbol table, start at the index stored in the reserved1 field
 * of the section structure.  Since the indirect symbol table entries
 * correspond to the entries in the section the number of indirect symbol table
 * entries is inferred from the size of the section divided by the size of the
 * entries in the section.  For symbol pointers sections the size of the entries
 * in the section is 4 bytes and for symbol stubs sections the byte size of the
 * stubs is stored in the reserved2 field of the section structure.
 *)
let kS_NON_LAZY_SYMBOL_POINTERS = 0x6 (* section with only non-lazy
         symbol pointers *)
let kS_LAZY_SYMBOL_POINTERS =  0x7 (* section with only lazy symbol
         pointers *)
let kS_SYMBOL_STUBS =    0x8 (* section with only symbol
         stubs, byte size of stub in
         the reserved2 field *)
let kS_MOD_INIT_FUNC_POINTERS = 0x9 (* section with only function
         pointers for initialization*)
let kS_MOD_TERM_FUNC_POINTERS = 0xa (* section with only function
         pointers for termination *)
let kS_COALESCED =   0xb (* section contains symbols that
         are to be coalesced *)
let kS_GB_ZEROFILL =   0xc (* zero fill on demand section
         (that can be larger than 4
         gigabytes) *)
let kS_INTERPOSING =   0xd (* section with only pairs of
         function pointers for
         interposing *)
let kS_16BYTE_LITERALS =  0xe (* section with only 16 byte
         literals *)
let kS_DTRACE_DOF =   0xf (* section contains 
         DTrace Object Format *)
let kS_LAZY_DYLIB_SYMBOL_POINTERS = 0x10 (* section with only lazy
         symbol pointers to lazy
         loaded dylibs *)
(*
 * Section types to support thread local variables
 *)
let kS_THREAD_LOCAL_REGULAR =                  0x11  (* template of initial 
         values for TLVs *)
let kS_THREAD_LOCAL_ZEROFILL =                 0x12  (* template of initial 
         values for TLVs *)
let kS_THREAD_LOCAL_VARIABLES =                0x13  (* TLV descriptors *)
let kS_THREAD_LOCAL_VARIABLE_POINTERS =        0x14  (* pointers to TLV 
                                                          descriptors *)
let kS_THREAD_LOCAL_INIT_FUNCTION_POINTERS =   0x15  (* functions to call
         to initialize TLV
         values *)

(*
 * Constants for the section attributes part of the flags field of a section
 * structure.
 *)
let kSECTION_ATTRIBUTES_USR =  0xff000000 (* User setable attributes *)
let kS_ATTR_PURE_INSTRUCTIONS = 0x80000000 (* section contains only true
         machine instructions *)
let kS_ATTR_NO_TOC =    0x40000000 (* section contains coalesced
         symbols that are not to be
         in a ranlib table of
         contents *)
let kS_ATTR_STRIP_STATIC_SYMS = 0x20000000 (* ok to strip static symbols
         in this section in files
         with the MH_DYLDLINK flag *)
let kS_ATTR_NO_DEAD_STRIP =  0x10000000 (* no dead stripping *)
let kS_ATTR_LIVE_SUPPORT =  0x08000000 (* blocks are live if they
         reference live blocks *)
let kS_ATTR_SELF_MODIFYING_CODE = 0x04000000 (* Used with i386 code stubs
         written on by dyld *)
(*
 * If a segment contains any sections marked with S_ATTR_DEBUG then all
 * sections in that segment must have this attribute.  No section other than
 * a section marked with this attribute may reference the contents of this
 * section.  A section with this attribute may contain no symbols and must have
 * a section type S_REGULAR.  The static linker will not copy section contents
 * from sections with this attribute into its output file.  These sections
 * generally contain DWARF debugging info.
 *) 
let kS_ATTR_DEBUG =   0x02000000 (*  debug section *)
let kSECTION_ATTRIBUTES_SYS =  0x00ffff00 (* system setable attributes *)
let kS_ATTR_SOME_INSTRUCTIONS = 0x00000400 (* section contains some
         machine instructions *)
let kS_ATTR_EXT_RELOC =  0x00000200 (* section has external
         relocation entries *)
let kS_ATTR_LOC_RELOC =  0x00000100 (* section has local
         relocation entries *)


(*
 * The names of segments and sections in them are mostly meaningless to the
 * link-editor.  But there are few things to support traditional UNIX
 * executables that require the link-editor and assembler to use some names
 * agreed upon by convention.
 *
 * The initial protection of the "__TEXT" segment has write protection turned
 * off (not writeable).
 *
 * The link-editor will allocate common symbols at the end of the "__common"
 * section in the "__DATA" segment.  It will create the section and segment
 * if needed.
 *)

(* The currently known segment names and the section names in those segments *)

let kSEG_PAGEZERO = "__PAGEZERO" (* the pagezero segment which has no *)
     (* protections and catches NULL *)
     (* references for MH_EXECUTE files *)


let kSEG_TEXT = "__TEXT" (* the tradition UNIX text segment *)
let kSECT_TEXT = "__text" (* the real text part of the text *)
     (* section no headers, and no padding *)
let kSECT_FVMLIB_INIT0 = "__fvmlib_init0" (* the fvmlib initialization *)
      (*  section *)
let kSECT_FVMLIB_INIT1 = "__fvmlib_init1" (* the section following the *)
             (*  fvmlib initialization *)
      (*  section *)

let kSEG_DATA = "__DATA" (* the tradition UNIX data segment *)
let kSECT_DATA = "__data" (* the real initialized data section *)
     (* no padding, no bss overlap *)
let kSECT_BSS = "__bss"  (* the real uninitialized data section*)
     (* no padding *)
let kSECT_COMMON = "__common" (* the section common symbols are *)
     (* allocated in by the link editor *)

let kSEG_OBJC = "__OBJC" (* objective-C runtime segment *)
let kSECT_OBJC_SYMBOLS = "__symbol_table" (* symbol table *)
let kSECT_OBJC_MODULES = "__module_info" (* module information *)
let kSECT_OBJC_STRINGS = "__selector_strs" (* string table *)
let kSECT_OBJC_REFS = "__selector_refs" (* string table *)

let kSEG_ICON =  "__ICON" (* the icon segment *)
let kSECT_ICON_HEADER = "__header" (* the icon headers *)
let kSECT_ICON_TIFF =   "__tiff" (* the icons in tiff format *)

let kSEG_LINKEDIT = "__LINKEDIT" (* the segment containing all structs *)
     (* created and maintained by the link *)
     (* editor.  Created with -seglinkedit *)
     (* option to ld(1) for MH_EXECUTE and *)
     (* FVMLIB file types only *)

let kSEG_UNIXSTACK = "__UNIXSTACK" (* the unix stack segment *)

let kSEG_IMPORT = "__IMPORT" (* the segment for the self (dyld) *)
     (* modifing code stubs that has read, *)
     (* write and execute permissions *)
