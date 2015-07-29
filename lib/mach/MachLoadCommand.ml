(* 
TODO: 
   (1) Verify the new load commands: Figure out lazy load of libraries and load weak dylibs
*)

exception Bad_load_command of int

type lc = | UUID
          | SEGMENT
          | SEGMENT_64
          | SYMTAB
          | DYSYMTAB
          | THREAD 
          | LOAD_DYLIB 
          | ID_DYLIB 
          | PREBOUND_DYLIB 
          | LOAD_DYLINKER 
          | ID_DYLINKER 
          | ROUTINES 
          | ROUTINES_64 
          | TWOLEVEL_HINTS 
          | SUB_FRAMEWORK 
          | SUB_UMBRELLA 
          | SUB_LIBRARY 
          | SUB_CLIENT 
          | MAIN 
          | SYMSEG 
          | UNIXTHREAD 
          | VERSION_MIN_IPHONEOS 
          | VERSION_MIN_MACOSX 
          | SOURCE_VERSION
          (* new *)
          | REQ_DYLD 
          | REEXPORT_DYLIB 
          | FUNCTION_STARTS 
          | DATA_IN_CODE
          | DYLIB_CODE_SIGN_DRS
          | RPATH
          | DYLD_INFO_ONLY
          | LOAD_UPWARD_DYLIB
          | SEGMENT_SPLIT_INFO
          | CODE_SIGNATURE
          (* newer  *)
          | LAZY_LOAD_DYLIB
          | LC_DYLD_INFO (* not sure what the diff is... luckily this seems very isolated, libsymsea uses it  (symantic library) *)
          | LC_LOAD_WEAK_DYLIB

(* 
 struct segment_command { uint32_t cmd; uint32_t cmdsize; char segname[16]; uint32_t vmaddr; uint32_t vmsize; uint32_t fileoff; uint32_t filesize; vm_prot_t maxprot; vm_prot_t initprot; uint32_t nsects; uint32_t flags; }; 
 *)

let lc_to_int = 
  function
  | UUID -> 0x1b
  | SEGMENT -> 1
  | SEGMENT_64 -> 0x19
  | SYMTAB -> 0x2
  | DYSYMTAB -> 0xb
  | THREAD -> 4
  | LOAD_DYLIB -> 0xc
  | ID_DYLIB -> 13
  | PREBOUND_DYLIB -> 16
  | LOAD_DYLINKER -> 0xe
  | ID_DYLINKER -> 15
  | ROUTINES -> 17
  | ROUTINES_64 -> 26
  | TWOLEVEL_HINTS -> 22 
  | SUB_FRAMEWORK -> 18
  | SUB_UMBRELLA -> 19
  | SUB_LIBRARY -> 21
  | SUB_CLIENT -> 20 
  | MAIN -> 0x80000028
  | UNIXTHREAD  -> 5
  | SYMSEG -> 3
  | VERSION_MIN_IPHONEOS -> 37 
  | VERSION_MIN_MACOSX -> 0x24
  | SOURCE_VERSION -> 0x2A
  (* new *)
  | REEXPORT_DYLIB -> 0x8000001f
  | REQ_DYLD -> 0x80000000
  | FUNCTION_STARTS -> 0x26
  | DATA_IN_CODE -> 0x29 
  | DYLIB_CODE_SIGN_DRS -> 0x2B
  | RPATH -> 0x8000001c
  | DYLD_INFO_ONLY -> 0x80000022
  | LOAD_UPWARD_DYLIB -> 0x80000023
  | SEGMENT_SPLIT_INFO -> 0x1E
  | CODE_SIGNATURE -> 0x1D
  (* really new *)
  | LAZY_LOAD_DYLIB -> 0x20
  | LC_DYLD_INFO -> 0x22
  | LC_LOAD_WEAK_DYLIB -> 0x80000018

let lookup_lc = 
  function
  | 0x1b -> UUID
  | 1 -> SEGMENT
  | 0x19 -> SEGMENT_64
  | 0x2 -> SYMTAB
  | 0xb -> DYSYMTAB
  | 4 -> THREAD
  | 0xc -> LOAD_DYLIB
  | 13 -> ID_DYLIB
  | 16 -> PREBOUND_DYLIB
  | 0xe -> LOAD_DYLINKER
  | 15 -> ID_DYLINKER
  | 17 -> ROUTINES
  | 26 -> ROUTINES_64
  | 22 -> TWOLEVEL_HINTS
  | 18 -> SUB_FRAMEWORK
  | 19 -> SUB_UMBRELLA
  | 21 -> SUB_LIBRARY
  | 20 -> SUB_CLIENT
  | 0x80000028 -> MAIN (* 0x28 | LC_REQ_DYLD *)
  | 5 -> UNIXTHREAD (* this is the same as TIME?  *)
  | 3 -> SYMSEG
  | 37 -> VERSION_MIN_IPHONEOS
  | 0x24 -> VERSION_MIN_MACOSX
  | 0x2A -> SOURCE_VERSION
  (* new *)
  | 0x8000001f -> REEXPORT_DYLIB
  | 0x80000000 -> REQ_DYLD
  | 0x26 -> FUNCTION_STARTS
  | 0x29 -> DATA_IN_CODE
  | 0x2B -> DYLIB_CODE_SIGN_DRS
  | 0x8000001c -> RPATH
  | 0x80000022 -> DYLD_INFO_ONLY
  | 0x80000023 -> LOAD_UPWARD_DYLIB
  | 0x1E -> SEGMENT_SPLIT_INFO
  | 0x1D -> CODE_SIGNATURE
  (* really new *)
  | 0x20 -> LAZY_LOAD_DYLIB
  | 0x22 -> LC_DYLD_INFO 
  | 0x80000018 -> LC_LOAD_WEAK_DYLIB
  | i -> raise @@ Bad_load_command i

let lc_to_string = 
  function
  | UUID -> "UUID"
  | SEGMENT -> "SEGMENT"
  | SEGMENT_64 -> "SEGMENT_64"
  | SYMTAB -> "SYMTAB"
  | DYSYMTAB -> "DYSYMTAB"
  | THREAD -> "THREAD"
  | LOAD_DYLIB -> "LOAD_DYLIB"
  | ID_DYLIB -> "ID_DYLIB"
  | PREBOUND_DYLIB -> "PREBOUND_DYLIB"
  | LOAD_DYLINKER -> "LOAD_DYLINKER"
  | ID_DYLINKER -> "ID_DYLINKER"
  | ROUTINES -> "ROUTINES"
  | ROUTINES_64 -> "ROUTINES_64"
  | TWOLEVEL_HINTS -> "TWOLEVEL_HINTS"
  | SUB_FRAMEWORK -> "SUB_FRAMEWORK"
  | SUB_UMBRELLA -> "SUB_UMBRELLA"
  | SUB_LIBRARY -> "SUB_LIBRARY"
  | SUB_CLIENT -> "SUB_CLIENT"
  | MAIN -> "MAIN"
  | UNIXTHREAD -> "UNIXTHREAD"
  | SYMSEG -> "SYMSEG"
  | VERSION_MIN_IPHONEOS -> "VERSION_MIN_IPHONEOS"
  | VERSION_MIN_MACOSX -> "VERSION_MIN_MACOSX"
  | SOURCE_VERSION -> "SOURCE_VERSION"
  (* new *)         
  | REEXPORT_DYLIB -> "REEXPORT_DYLIB"
  | REQ_DYLD  -> "REQ_DYLD"
  | FUNCTION_STARTS -> "FUNCTION_STARTS"
  | DATA_IN_CODE -> "DATA_IN_CODE"
  | DYLIB_CODE_SIGN_DRS -> "DYLIB_CODE_SIGN_DRS"
  | RPATH -> "RPATH"
  | DYLD_INFO_ONLY -> "DYLD_INFO_ONLY"
  | LOAD_UPWARD_DYLIB -> "LOAD_UPWARD_DYLIB"
  | SEGMENT_SPLIT_INFO -> "SEGMENT_SPLIT_INFO"
  | CODE_SIGNATURE -> "CODE_SIGNATURE"
  (* really new *)
  | LAZY_LOAD_DYLIB -> "LAZY_LOAD_DYLIB"
  | LC_DYLD_INFO -> "LC_DYLD_INFO" 
  | LC_LOAD_WEAK_DYLIB -> "LC_LOAD_WEAK_DYLIB"

(* ========================== *)
(* Specific Load Command Structures *)
(* ========================== *)

type section = {
    sectname: string;		(* 16 bytes *)
    segname: string;		(* 16 bytes *)
    (* 4 bytes each *)
    addr: int;
    size: int;
    offset: int;
    align: int;
    reloff: int;
    nreloc: int;
    flags: int;
    reserved1: int;
    reserved2: int;
  }

let sizeof_section = 68	(* bytes *)

(* verified matches c struct naming *)
type section_64 = {
    sectname: string;		(* 16 bytes *)
    segname: string;		(* 16 bytes *)
    addr: int;			(* 8 bytes *)
    size: int;			(* 8 bytes *)
    (* 4 bytes each *)
    offset: int;
    align: int;
    reloff: int;
    nreloc: int;
    flags: int;
    reserved1: int;
    reserved2: int;
    reserved3: int;
  }

let sizeof_section_64 = 80	(* bytes *)

let sections64_to_string sections =
  let b = Buffer.create ((Array.length sections) * 32) in
  let indent = "\t\t" in
  Array.iteri (fun i section ->
	      Printf.sprintf "\n%s(%2d) %s addr: 0x%x size: 0x%x\n%soffset: 0x%x align: %d reloff: 0x%x nreloc: 0x%x\n%sflags: 0x%x r1: 0x%x r2: 0x%x r3: 0x%x\n"
			     indent i section.sectname section.addr section.size
			     indent section.offset section.align section.reloff section.nreloc
			     indent section.flags section.reserved1 section.reserved2 section.reserved3
	     |> Buffer.add_string b
	     ) sections;
  Buffer.contents b

type segment_command = {
  segname: string;
  vmaddr: int;
  vmsize: int;
  fileoff: int;
  filesize: int;
  maxprot: int;
  initprot: int;
  nsects: int;
  flags: int;
  sections: section array;
  }

let sizeof_segment_command_64 = 48 (* 56 - 8 *)			 

type segment_command_64 = {
  segname: string; (* 16 bytes *)
  vmaddr: int; (* 8 bytes *)
  vmsize: int; (* 8 bytes *)
  fileoff: int; (* 8 bytes *)
  filesize: int; (* 8 bytes *)
  maxprot: int;  (* 4 int *)
  initprot: int; (* 4 int *)
  nsects: int;   (* 4  *)
  flags: int;    (* 4 *)
  sections: section_64 array; 	(* extra *)
}

let sizeof_segment_command_64 = 64 (* 72 - 8 *)

(*
 * This is the second set of the symbolic information which is used to support
 * the data structures for the dynamically link editor.
 *
 * The original set of symbolic information in the symtab_command which contains
 * the symbol and string tables must also be present when this load command is
 * present.  When this load command is present the symbol table is organized
 * into three groups of symbols:
 * local symbols (static and debugging symbols) - grouped by module
 * defined external symbols - grouped by module (sorted by name if not lib)
 * undefined external symbols (sorted by name if MH_BINDATLOAD is not set,
 *             and in order the were seen by the static
 *        linker if MH_BINDATLOAD is set)
 * In this load command there are offsets and counts to each of the three groups
 * of symbols.
 *
 * This load command contains a the offsets and sizes of the following new
 * symbolic information tables:
 * table of contents
 * module table
 * reference symbol table
 * indirect symbol table
 * The first three tables above (the table of contents, module table and
 * reference symbol table) are only present if the file is a dynamically linked
 * shared library.  For executable and object modules, which are files
 * containing only one module, the information that would be in these three
 * tables is determined as follows:
 *  table of contents - the defined external symbols are sorted by name
 * module table - the file contains only one module so everything in the
 *         file is part of the module.
 * reference symbol table - is the defined and undefined external symbols
 *
 * For dynamically linked shared library files this load command also contains
 * offsets and sizes to the pool of relocation entries for all sections
 * separated into two groups:
 * external relocation entries
 * local relocation entries
 * For executable and object modules the relocation entries continue to hang
 * off the section structures.
 *)
type dysymtab_command  = {
  ilocalsym: int; (* index to local symbols *)
  nlocalsym: int; (* number of local symbols *)

  iextdefsym: int;    (* index to externally defined symbols *)
  nextdefsym: int;    (* number of externally defined symbols *)

  iundefsym: int; (* index to undefined symbols *)
  nundefsym: int; (* number of undefined symbols *)

  tocoff: int; (* file offset to table of contents *)
  ntoc: int; (* number of entries in table of contents *)
  modtaboff: int; (* file offset to module table *)
  nmodtab: int; (* number of module table entries *)

  extrefsymoff: int; (* offset to referenced symbol table *)
  nextrefsyms: int; (* number of referenced symbol table entries *)

  indirectsymoff: int; (* file offset to the indirect symbol table *)
  nindirectsyms: int;  (* number of indirect symbol table entries *)
  extreloff: int; (* offset to external relocation entries *)
  nextrel: int; (* number of external relocation entries *)
  locreloff: int; (* offset to local relocation entries *)
  nlocrel: int; (* number of local relocation entries *)
}

let sizeof_dysymtab_command = 72 (* bytes *)

type symtab_command = {
  symoff: int;
  nsyms: int;
  stroff: int;
  strsize: int;
}

(*
*
 * Dynamicly linked shared libraries are identified by two things.  The
 * pathname (the name of the library as found for execution), and the
 * compatibility version number.  The pathname must match and the compatibility
 * number in the user of the library must be greater than or equal to the
 * library being used.  The time stamp is used to record the time a library was
 * built and copied into user so it can be use to determined if the library used
 * at runtime is exactly the same as used to built the program.
 *
struct dylib {
    union lc_str  name;   /* library's path name */
    uint32_t timestamp;   /* library's build time stamp */
    uint32_t current_version;  /* library's current version number */
    uint32_t compatibility_version; /* library's compatibility vers number*/
};
 *)

(*
 *
 * A dynamically linked shared library (filetype == MH_DYLIB in the mach header)
 * contains a dylib_command (cmd == LC_ID_DYLIB) to identify the library.
 * An object that uses a dynamically linked shared library also contains a
 * dylib_command (cmd == LC_LOAD_DYLIB, LC_LOAD_WEAK_DYLIB, or
 * LC_REEXPORT_DYLIB) for each library it uses.
 *
struct dylib_command {
uint32_t cmd;  /* LC_ID_DYLIB, LC_LOAD_{,WEAK_}DYLIB,
LC_REEXPORT_DYLIB */
uint32_t cmdsize; /* includes pathname string */
struct dylib dylib;  /* the library identification */
};
 *)

(* so we just use the dylib struct as the dylib command *)
type dylib_command = 
  {   lc_str : string (* offset to zero-terminated string *); (* __LP64__ mentioned, which makes this a char * pointer, but doesn't seem present in 64 bit binaries... *)
      timestamp: int;
      current_version: int;
      compatibility_version: int;
  }

let sizeof_dylib_command = 16 (*bytes*)

(*
struct dyld_info_command {
    uint32_t   cmd;  /* LC_DYLD_INFO or LC_DYLD_INFO_ONLY */
    uint32_t   cmdsize;  /* sizeof(struct dyld_info_command) */
    uint32_t   rebase_off; /* file offset to rebase info  */
    uint32_t   rebase_size; /* size of rebase info   */
    uint32_t   bind_off; /* file offset to binding info   */
    uint32_t   bind_size; /* size of binding info  */
    uint32_t   weak_bind_off; /* file offset to weak binding info   */
    uint32_t   weak_bind_size;  /* size of weak binding info  */
    uint32_t   lazy_bind_off; /* file offset to lazy binding info */
    uint32_t   lazy_bind_size;  /* size of lazy binding infs */
    uint32_t   export_off; /* file offset to lazy binding info */
    uint32_t   export_size; /* size of lazy binding infs */
};
 *)

type dyld_info_command = 
  {
    rebase_off: int;
    rebase_size: int;
    bind_off: int;
    bind_size: int;
    weak_bind_off: int;
    weak_bind_size: int;
    lazy_bind_off: int;
    lazy_bind_size: int;
    export_off: int;
    export_size: int;
  }

let sizeof_dylib_info = 40

(*
 * The version_min_command contains the min OS version on which this 
 * binary was built to run.
 *
 * LC_VERSION_MIN_MACOSX or LC_VERSION_MIN_IPHONEOS  *)
type version_min_command = {
  version: int; (* X.Y.Z is encoded in nibbles xxxx.yy.zz *)
  sdk: int;  (* X.Y.Z is encoded in nibbles xxxx.yy.zz *)
}

let sizeof_version_min_command = 8 (* bytes *)

(*
 * The entry_point_command is a replacement for thread_command.
 * It is used for main executables to specify the location (file offset)
 * of main().  If -stack_size was used at link time, the stacksize
 * field will contain the stack size need for the main thread.
 *)
type entry_point_command = {
  entryoff: int; (* uint64_t file (__TEXT) offset of main() *)
  stacksize: int ;(* uint64_t if not zero, initial stack size *) 
}

let sizeof_entry_point_command = 16 (* bytes *)

type id_dylib_command = dylib_command

type load_command_header = {
  cmd: lc;
  cmdsize: int;
}

let sizeof_load_command = 8

type dylinker = {
    lc_str : string (* offset to zero-terminated string *);
  }

type lc_t = 
  | SEGMENT_64 of segment_command_64
  | DYLINKER of dylinker		    
  | DYLIB of dylib_command
  | SYMTAB of symtab_command
  | DYLD_INFO of dyld_info_command
  | DYSYMTAB of dysymtab_command
  | VERSION of version_min_command
  | ENTRY_POINT of entry_point_command
  | Unimplemented of bytes

(* add printing mechanisms here *)
let lc_t_to_string = 
  function
  | SEGMENT_64 segment ->
    Printf.sprintf "\n\t%s vmaddr: 0x%x vmsize: 0x%x\n\tfileoff: 0x%x filesize: 0x%x\n\tmaxprot: %d initprot: %d nsects: %d flags: 0x%x\n%s"
		   segment.segname segment.vmaddr segment.vmsize
		   segment.fileoff segment.filesize segment.maxprot
		   segment.initprot segment.nsects segment.flags
		   (sections64_to_string segment.sections)
      
  | SYMTAB symtab -> 
    Printf.sprintf "\n\tsymoff: 0x%x nsyms: %u stroff: 0x%x strsize: %u"
      symtab.symoff 
      symtab.nsyms
      symtab.stroff
      symtab.strsize

  | DYSYMTAB dysymtab -> 
    Printf.sprintf "\n\tilocalsym: 0x%x nlocalsym: %d iextdefsym: 0x%x nextdefsym: %d\n\tiundefsym: 0x%x nundefsym: %d tocoff: 0x%x ntoc: %d\n\tmodtaboff: 0x%x nmodtab: %d extrefsymoff: 0x%x nextrefsyms: %d\n\tindirectsymoff: 0x%x nindirectsyms: %d extreloff: 0x%x nextrel: %d\n\tlocreloff: 0x%x nlocrel: %d"
      dysymtab.ilocalsym
      dysymtab.nlocalsym
      dysymtab.iextdefsym
      dysymtab.nextdefsym
      dysymtab.iundefsym
      dysymtab.nundefsym
      dysymtab.tocoff
      dysymtab.ntoc
      dysymtab.modtaboff
      dysymtab.nmodtab
      dysymtab.extrefsymoff
      dysymtab.nextrefsyms
      dysymtab.indirectsymoff
      dysymtab.nindirectsyms
      dysymtab.extreloff
      dysymtab.nextrel
      dysymtab.locreloff
      dysymtab.nlocrel

  | DYLD_INFO dyld_info ->
    Printf.sprintf "\n\trebase_off: 0x%x rebase_size: %u \n\tbind_off: 0x%x bind_size: %u \n\tweak_bind_off: 0x%x weak_bind_size: %u \n\tlazy_bind_off: 0x%x lazy_bind_size: %u \n\texport_off: 0x%x export_size: %u"
      dyld_info.rebase_off
      dyld_info.rebase_size
      dyld_info.bind_off
      dyld_info.bind_size
      dyld_info.weak_bind_off
      dyld_info.weak_bind_size
      dyld_info.lazy_bind_off
      dyld_info.lazy_bind_size
      dyld_info.export_off
      dyld_info.export_size
  | DYLINKER dylinker ->
    Printf.sprintf "\n\t%s"
      dylinker.lc_str 
  | DYLIB load_dylib ->
    Printf.sprintf "\n\t%s"
      load_dylib.lc_str 
  (* verbose version *)
  (*    Printf.sprintf "\n\tlc_str: %s timestamp: %u current_version: %u compatibility_version: %u"
        load_dylib.lc_str
                     load_dylib.timestamp
        load_dylib.current_version
        load_dylib.compatibility_version
  *)     
  | VERSION version ->
    Printf.sprintf "\n\tversion: %s sdk: %s" (MachVersion.version_to_string version.version) (MachVersion.version_to_string version.sdk)
  | ENTRY_POINT ep ->
    Printf.sprintf "\n\toffset: 0x%x stacksize: 0x%x" ep.entryoff ep.stacksize
  | _ ->
    Printf.sprintf ""

let load_command_to_string (cmd, cmdsize, lc_t) = 
  Printf.sprintf "%s (0x%x) %d %s" (lc_to_string cmd) (lc_to_int cmd) cmdsize (lc_t_to_string lc_t)

let print_load_command lc = 
  Printf.printf "%s\n" (load_command_to_string lc)

let print_load_commands lcs = 
  List.iteri (fun i lc -> Printf.printf "(%2d): " i; print_load_command lc) lcs;
  print_string "\n"

let get_load_command_header binary offset = 
  let cmd = lookup_lc @@ Binary.u32 binary offset in
  let cmdsize = Binary.u32 binary (offset + 4) in
  cmd,cmdsize

let get_data binary offset size = 
  Bytes.sub binary offset size

(* specific load command constructors *)
let get_symtable binary = 
  let symoff = Binary.u32 binary 0 in
  let nsyms = Binary.u32 binary 4 in
  let stroff =  Binary.u32 binary 8 in
  let strsize =  Binary.u32 binary 12 in
  {symoff; nsyms; stroff; strsize;}

let get_dysymtable binary = 
  let ilocalsym = Binary.u32 binary 0 in
  let nlocalsym = Binary.u32 binary 4 in

  let iextdefsym =  Binary.u32 binary 8 in
  let nextdefsym =  Binary.u32 binary 12 in

  let iundefsym = Binary.u32 binary 16 in
  let nundefsym = Binary.u32 binary 20 in

  let tocoff =  Binary.u32 binary 24 in
  let ntoc =  Binary.u32 binary 28 in
  let modtaboff = Binary.u32 binary 32 in
  let nmodtab = Binary.u32 binary 36 in

  let extrefsymoff =  Binary.u32 binary 40 in
  let nextrefsyms =  Binary.u32 binary 44 in

  let indirectsymoff =  Binary.u32 binary 48 in
  let nindirectsyms =  Binary.u32 binary 52 in
  let extreloff =  Binary.u32 binary 56 in
  let nextrel =  Binary.u32 binary 60 in
  let locreloff =  Binary.u32 binary 64 in
  let nlocrel =  Binary.u32 binary 68 in

  {
    ilocalsym; nlocalsym; iextdefsym; nextdefsym;
    iundefsym; nundefsym; tocoff; ntoc; modtaboff; nmodtab;
    extrefsymoff; nextrefsyms;
    indirectsymoff; nindirectsyms; extreloff; nextrel; locreloff; nlocrel;
  }

let get_dyld_info binary = 
  let rebase_off = Binary.u32 binary 0 in
  let rebase_size = Binary.u32 binary 4 in
  let bind_off =  Binary.u32 binary 8 in
  let bind_size =  Binary.u32 binary 12 in
  let weak_bind_off =  Binary.u32 binary 16 in
  let weak_bind_size =  Binary.u32 binary 20 in
  let lazy_bind_off =  Binary.u32 binary 24 in
  let lazy_bind_size =  Binary.u32 binary 28 in
  let export_off =  Binary.u32 binary 32 in
  let export_size =  Binary.u32 binary 36 in
  {rebase_off; rebase_size; bind_off; bind_size; weak_bind_off; weak_bind_size; lazy_bind_off; lazy_bind_size; export_off; export_size;}

(* consider glomming all libs into single binary with 0...N load commands and library indexed at 1... *)
let get_dylib binary = 
  let lc_str_offset = Binary.u32 binary 0 in
  let timestamp = Binary.u32 binary 4 in
  let current_version =  Binary.u32 binary 8 in
  let compatibility_version =  Binary.u32 binary 12 in
  let lc_str = Binary.string binary (lc_str_offset - sizeof_load_command) in (* technically should use the lc_str_offset but need (lc_str_offset - sizeof_load_command) because offset from start of the load_command and we chopped off the first 8 bytes of the lc*)
  {lc_str; timestamp; current_version; compatibility_version;}

(* version for osx and ios *)
let get_version binary = 
  let version = Binary.u32 binary 0 in
  let sdk = Binary.u32 binary 4 in
  {version; sdk;}

let get_main binary = 
  let entryoff = Binary.u64 binary 0 in
  let stacksize = Binary.u64 binary 8 in
  {entryoff; stacksize;}

let get_dylinker binary = 
  let lc_str_offset = Binary.u32 binary 0 in
  let lc_str = Binary.string binary (lc_str_offset - sizeof_load_command) in 
  {lc_str;}

let get_section64 binary offset =
  (*   Printf.printf "initial o: %d\n" o; *)
  let sectname = Binary.string binary offset ~maxlen:(15+offset) in
  (*   Printf.printf "sectname: %s o: %d\n" sectname o; *)
  let segname = Binary.string binary (16+offset) ~maxlen:(15+16+offset) in
  (*   Printf.printf "segname: %s o: %d\n" segname o; *)
  let addr,o = Binary.u64o binary (offset+32) in
  let size,o = Binary.u64o binary o in
  let offset,o = Binary.u32o binary o in
  let align,o = Binary.u32o binary o in
  let reloff,o = Binary.u32o binary o in
  let nreloc,o = Binary.u32o binary o in
  let flags,o = Binary.u32o binary o in
  let reserved1,o = Binary.u32o binary o in
  let reserved2,o = Binary.u32o binary o in
  let reserved3,o = Binary.u32o binary o in
  {sectname; segname; addr; size; offset; align; reloff; nreloc; flags; reserved1; reserved2; reserved3;}

let get_sections64 binary nsects offset =
  let rec loop count acc =
    if (count >= nsects) then
      List.rev acc |> Array.of_list
    else
      let section = get_section64 binary ((sizeof_section_64*count)+offset) in
      loop (count+1) (section::acc)
  in loop 0 []

let get_segment64 binary = 
  let segname = Binary.string binary 0 ~maxlen:15 in
  let vmaddr = Binary.u64 binary 16 in
  let vmsize =  Binary.u64 binary 24 in
  let fileoff = Binary.u64 binary 32 in
  let filesize =  Binary.u64 binary 40 in
  let maxprot = Binary.u32 binary 48 in
  let initprot = Binary.u32 binary 52 in
  let nsects = Binary.u32 binary 56 in
  let flags = Binary.u32 binary 60 in
  let sections = get_sections64 binary nsects 64 in
  {segname; vmaddr; vmsize; fileoff; filesize; maxprot; initprot; nsects; flags; sections;}

(* type of load commands; may need to change *)
type t = (lc * int * lc_t) list

let rec get_load_commands_it binary offset ncmds acc = 
  if (ncmds <= 0) then
    List.rev acc
  else
    let cmd,cmdsize = get_load_command_header binary offset in
    let bytes = Bytes.sub binary (offset + sizeof_load_command) (cmdsize - sizeof_load_command) in
    let lc_t = 
      match cmd with
      | SEGMENT_64 ->
	 SEGMENT_64 (get_segment64 bytes)
      | LOAD_DYLINKER ->
	 DYLINKER (get_dylinker bytes)
      | SYMTAB -> 
        SYMTAB (get_symtable bytes)
      | DYSYMTAB ->
        DYSYMTAB (get_dysymtable bytes)
      | DYLD_INFO_ONLY | LC_DYLD_INFO -> 
        DYLD_INFO (get_dyld_info bytes)
      | LOAD_DYLIB | REEXPORT_DYLIB | ID_DYLIB | LOAD_UPWARD_DYLIB | LAZY_LOAD_DYLIB | LC_LOAD_WEAK_DYLIB -> 
        DYLIB (get_dylib bytes)
      | VERSION_MIN_MACOSX | VERSION_MIN_IPHONEOS ->
        VERSION (get_version bytes)
      | MAIN ->
        ENTRY_POINT (get_main bytes)
      | _ ->
        Unimplemented (get_data binary (offset + sizeof_load_command) (cmdsize - sizeof_load_command)) in
    let load_command = (cmd, cmdsize, lc_t) in
    get_load_commands_it binary (offset + cmdsize) (ncmds - 1) (load_command::acc)

let get_load_commands binary offset ncmds sizeofcmds =
  let load_command_bytes = Bytes.sub binary offset sizeofcmds in
  get_load_commands_it load_command_bytes 0 ncmds []

exception Missing_load_command of string

(* lc, cmdsize, and lc_t *)
let rec find_load_command lc lcs : (lc * int * lc_t)=
  match lcs with
  | [] -> raise @@ Missing_load_command (lc_to_string lc)
  | ((cmd, _, _) as lc')::lcs ->
    if (lc == cmd) then lc'
    else
      find_load_command lc lcs

let get_segments lcs =
  let rec loop lcs acc =
  match lcs with
  | [] -> List.rev acc
  | (cmd, _, (SEGMENT_64 segment))::lcs ->
     loop lcs (segment::acc)
  | lc::lcs -> loop lcs acc
  in loop lcs []

let rec get_dyld_info lcs =
  match lcs with
  | [] -> None
  | (cmd,cmdsize, DYLD_INFO dyldinfo)::lcs ->
    if (cmd = DYLD_INFO_ONLY || cmd = LC_DYLD_INFO) then Some dyldinfo
    else
      get_dyld_info lcs
  | lc::lcs -> get_dyld_info lcs

let rec get_lib_name lcs =
  match lcs with
  | [] -> None
  | (ID_DYLIB, _, DYLIB id)::lcs ->
    Some id
  | lc::lcs -> get_lib_name lcs

let get_libraries lcs =
  let rec loop lcs acc =
    match lcs with
    | [] -> 
      Array.of_list @@ List.rev acc
    | ((cmd, _, _) as lc)::lcs ->
      if (cmd = LOAD_DYLIB) then loop lcs (lc::acc)
      else
        loop lcs acc
  in loop lcs []

let print_libraries libs =
  if ((Array.length libs) <> 0) then
    begin
      Printf.printf "Libraries (%d)\n" @@ (Array.length libs - 1);
      Array.iteri (fun i lib ->
		  if (i <> 0) then
		    Printf.printf "%s\n" lib) libs
    end

let get_libraries lcs self = 
  let rec loop lcs acc =
    match lcs with
    | [] -> 
      Array.of_list @@ List.rev acc
    | (cmd, _, DYLIB dylib)::lcs ->
      begin
        match cmd with
        | LOAD_DYLIB | REEXPORT_DYLIB | LOAD_UPWARD_DYLIB | LAZY_LOAD_DYLIB | LC_LOAD_WEAK_DYLIB ->
          loop lcs (dylib.lc_str::acc)
        | _ ->
          loop lcs acc
      end
    | _::lcs ->
      loop lcs acc
  in loop lcs [self]

let cmd (cmd', _, _) = cmd'
let cmdsize (_, cmdsize', _) = cmdsize'
let lc_t (_, _, lc_t') = lc_t'
