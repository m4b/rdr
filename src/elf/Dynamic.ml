(*   union
    {
      Elf64_Xword d_val;  (* Integer value *)
      Elf64_Addr d_ptr;   (* Address value *)
    } d_un;
 *)

(* d_tag values *)

(* Legal values for d_tag (dynamic entry type).  *)

type tag =
  | NULL
  | NEEDED
  | PLTRELSZ
  | PLTGOT 
  | HASH
  | STRTAB 
  | SYMTAB 
  | RELA
  | RELASZ 
  | RELAENT 
  | STRSZ
  | SYMENT 
  | INIT 
  | FINI 
  | SONAME
  | RPATH
  | SYMBOLIC
  | REL 
  | RELSZ
  | RELENT
  | PLTREL
  | DEBUG
  | TEXTREL
  | JMPREL
  | BIND_NOW
  | INIT_ARRAY
  | FINI_ARRAY
  | INIT_ARRAYSZ
  | FINI_ARRAYSZ
  | RUNPATH
  | FLAGS
  | ENCODING
  | PREINIT_ARRAY
  | PREINIT_ARRAYSZ
  | NUM
      (* val *)
  | GNU_PRELINKED
  | GNU_CONFLICTSZ
  | GNU_LIBLISTSZ
  | CHECKSUM
  | PLTPADSZ
  | MOVEENT
  | MOVESZ
  | FEATURE_1
  | POSFLAG_1
  | SYMINSZ
  | SYMINENT
(* ptr *)
  | GNU_HASH
  | TLSDESC_PLT
  | TLSDESC_GOT
  | GNU_CONFLICT
  | GNU_LIBLIST
  | CONFIG
  | DEPAUDIT
  | AUDIT
  | PLTPAD
  | MOVETAB
  | SYMINFO
(* gnu extensions *)
  | VERSYM
  | RELACOUNT
  | RELCOUNT
  | FLAGS_1
  | VERDEF
  | VERDEFNUM
  | VERNEED
  | VERNEEDNUM

let tag_to_string =
  function
  | NULL -> "NULL"
  | NEEDED -> "NEEDED"
  | PLTRELSZ -> "PLTRELSZ"
  | PLTGOT  -> "PLTGOT "
  | HASH -> "HASH"
  | STRTAB  -> "STRTAB "
  | SYMTAB  -> "SYMTAB "
  | RELA -> "RELA"
  | RELASZ  -> "RELASZ "
  | RELAENT  -> "RELAENT "
  | STRSZ -> "STRSZ"
  | SYMENT  -> "SYMENT "
  | INIT  -> "INIT "
  | FINI  -> "FINI "
  | SONAME -> "SONAME"
  | RPATH -> "RPATH"
  | SYMBOLIC -> "SYMBOLIC"
  | REL  -> "REL "
  | RELSZ -> "RELSZ"
  | RELENT -> "RELENT"
  | PLTREL -> "PLTREL"
  | DEBUG -> "DEBUG"
  | TEXTREL -> "TEXTREL"
  | JMPREL -> "JMPREL"
  | BIND_NOW -> "BIND_NOW"
  | INIT_ARRAY -> "INIT_ARRAY"
  | FINI_ARRAY -> "FINI_ARRAY"
  | INIT_ARRAYSZ -> "INIT_ARRAYSZ"
  | FINI_ARRAYSZ -> "FINI_ARRAYSZ"
  | RUNPATH -> "RUNPATH"
  | FLAGS -> "FLAGS"
  | ENCODING -> "ENCODING"
  | PREINIT_ARRAY -> "PREINIT_ARRAY"
  | PREINIT_ARRAYSZ -> "PREINIT_ARRAYSZ"
  | NUM -> "NUM"
(* val *)
  | GNU_PRELINKED -> "GNU_PRELINKED"
  | GNU_CONFLICTSZ -> "GNU_CONFLICTSZ"
  | GNU_LIBLISTSZ -> "GNU_LIBLISTSZ"
  | CHECKSUM -> "CHECKSUM"
  | PLTPADSZ -> "PLTPADSZ"
  | MOVEENT -> "MOVEENT"
  | MOVESZ -> "MOVESZ"
  | FEATURE_1 -> "FEATURE_1"
  | POSFLAG_1 -> "POSFLAG_1"
  | SYMINSZ -> "SYMINSZ"
  | SYMINENT -> "SYMINENT"
(* ptr *)
  | GNU_HASH -> "GNU_HASH"
  | TLSDESC_PLT -> "TLSDESC_PLT"
  | TLSDESC_GOT -> "TLSDESC_GOT"
  | GNU_CONFLICT -> "GNU_CONFLICT"
  | GNU_LIBLIST -> "GNU_LIBLIST"
  | CONFIG -> "CONFIG"
  | DEPAUDIT -> "DEPAUDIT"
  | AUDIT -> "AUDIT"
  | PLTPAD -> "PLTPAD"
  | MOVETAB -> "MOVETAB"
  | SYMINFO -> "SYMINFO"
(* gnu extensions *)
  | VERSYM -> "VERSYM"
  | RELACOUNT -> "RELACOUNT"
  | RELCOUNT -> "RELCOUNT"
  | FLAGS_1 -> "FLAGS_1"
  | VERDEF -> "VERDEF"
  | VERDEFNUM -> "VERDEFNUM"
  | VERNEED -> "VERNEED"
  | VERNEEDNUM -> "VERNEEDNUM"

		    
exception Unknown_tag of string
	     
let get_tag =
  function
  | 0 -> NULL
  | 1 -> NEEDED 
  | 2 -> PLTRELSZ
  | 3 -> PLTGOT
  | 4 -> HASH
  | 5 -> STRTAB
  | 6 -> SYMTAB
  | 7 -> RELA
  | 8 -> RELASZ
  | 9 -> RELAENT
  | 10 -> STRSZ
  | 11 -> SYMENT
  | 12 -> INIT
  | 13 -> FINI
  | 14 -> SONAME
  | 15 -> RPATH
  | 16 -> SYMBOLIC
  | 17 -> REL
  | 18 -> RELSZ
  | 19 -> RELENT
  | 20 -> PLTREL
  | 21 -> DEBUG
  | 22 -> TEXTREL
  | 23 -> JMPREL
  | 24 -> BIND_NOW
  | 25 -> INIT_ARRAY
  | 26 -> FINI_ARRAY
  | 27 -> INIT_ARRAYSZ
  | 28 -> FINI_ARRAYSZ
  | 29 -> RUNPATH
  | 30 -> FLAGS
  (*   | 32 -> ENCODING *)
  | 32 -> PREINIT_ARRAY
  | 33 -> PREINIT_ARRAYSZ
  | 34 -> NUM
	    (* val *)
  | 0x6ffffdf5 -> GNU_PRELINKED
  | 0x6ffffdf6 -> GNU_CONFLICTSZ
  | 0x6ffffdf7 -> GNU_LIBLISTSZ
  | 0x6ffffdf8 -> CHECKSUM
  | 0x6ffffdf9 -> PLTPADSZ
  | 0x6ffffdfa -> MOVEENT
  | 0x6ffffdfb -> MOVESZ
  | 0x6ffffdfc -> FEATURE_1
  | 0x6ffffdfd -> POSFLAG_1
  | 0x6ffffdfe -> SYMINSZ
  | 0x6ffffdff -> SYMINENT

(* ptr *)
  | 0x6ffffef5 -> GNU_HASH
  | 0x6ffffef6 -> TLSDESC_PLT
  | 0x6ffffef7 -> TLSDESC_GOT
  | 0x6ffffef8 -> GNU_CONFLICT
  | 0x6ffffef9 -> GNU_LIBLIST
  | 0x6ffffefa -> CONFIG
  | 0x6ffffefb -> DEPAUDIT
  | 0x6ffffefc -> AUDIT
  | 0x6ffffefd -> PLTPAD
  | 0x6ffffefe -> MOVETAB
  | 0x6ffffeff -> SYMINFO
  (* gnu extensions *)
  | 0x6ffffff0 -> VERSYM
  | 0x6ffffff9 -> RELACOUNT
  | 0x6ffffffa -> RELCOUNT
  | 0x6ffffffb -> FLAGS_1
  | 0x6ffffffc -> VERDEF
  | 0x6ffffffd -> VERDEFNUM
  | 0x6ffffffe -> VERNEED
  | 0x6fffffff -> VERNEEDNUM
		    
  | tag -> raise @@ Unknown_tag (Printf.sprintf "0x%x" tag)

type dyn64 =
  {
    d_tag: tag;   (* Dynamic entry type *)
    d_un: int;
  }

let sizeof_dyn64 = 16

let is_null dyn64 =
  match dyn64.d_tag with
  | NULL -> true
  | _ -> false
		     
let dyn64_to_string dyn64 = Printf.sprintf "%s 0x%x" (tag_to_string dyn64.d_tag) dyn64.d_un

let get_dynamic_entry bytes offset =
  let d_tag = Binary.u64 bytes offset |> get_tag in
  let d_un = Binary.u64 bytes (offset + 8) in
  {d_tag; d_un}

let get_dynamic bytes offset size =
  let len = offset + size in
  let rec loop pos acc =
    let entry = get_dynamic_entry bytes pos in
    if (pos > len || is_null entry) then
      List.rev acc
    else
      loop (pos + sizeof_dyn64) (entry::acc)
  in loop offset []

(* we use program headers in case the section headers were stripped *)
let get_DYNAMIC binary program_headers =
  match ProgramHeader.get_dynamic_program_header program_headers with
  | None -> []
  | Some section ->
     get_dynamic binary section.ProgramHeader.p_offset section.ProgramHeader.p_filesz
	  
let print_dyn64 dyn64 =
     dyn64_to_string dyn64 |> Printf.printf "%s\n"
	  
let print_DYNAMIC dynamic =
  Printf.printf "Dynamic (%d):\n" @@ List.length dynamic;
  List.iter print_dyn64 dynamic

let get_dynamic_symbol_offset_data dynamic =
  let rec loop (x,y,z) dynamic =
  match dynamic with
  | [] -> x,y,z
  | elem::dynamic ->
     if (elem.d_tag = SYMTAB) then
       loop (elem.d_un, y, z) dynamic
     else if (elem.d_tag = STRTAB) then
       loop (x, elem.d_un, z) dynamic
     else if (elem.d_tag = STRSZ) then
       loop (x, y, elem.d_un) dynamic
     else
       loop (x,y,z) dynamic
  in loop (-1,-1,-1) dynamic

let rec get_soname_offset dynamic =
  match dynamic with
  | [] -> raise Not_found
  | elem::dynamic ->
     if (elem.d_tag = SONAME) then
       elem.d_un
     else
       get_soname_offset dynamic
	    
let get_dynamic_strtab binary offset size =
  Bytes.sub binary offset size

let get_dynamic_strtab_data dynamic =    
      List.fold_left (fun (x,y) elem ->
		    if (elem.d_tag = STRTAB) then
		      elem.d_un,y
		    else if (elem.d_tag = STRSZ) then
		      x,elem.d_un
		    else
		      x,y) (-1,-1) dynamic

let get_dynamic_symtab dynamic =
      List.fold_left (fun acc elem ->
		    if (elem.d_tag = SYMTAB) then
		      Some elem
		    else
		      acc) None dynamic

let get_libraries dynamic strtab =
  List.fold_left (fun acc elem ->
		  if (elem.d_tag = NEEDED) then
		    (Binary.string strtab elem.d_un)::acc
		  else
		    acc) [] dynamic		     
	    
(* build the symbol table from the vm_adjusted and the references to strtab and symtab, NOTE: vm_adjusted will be equal to 0 if it's a dylib like libc because offset = vaddr in phdr *)
(* TODO: also probably change this to array? *)
let get_dynamic_symbols binary masks symtab_offset strtab_offset strtab_size =
  let symtab_size = strtab_offset - symtab_offset in
  (*   Printf.printf "DEBUG 0x%x 0x%x 0x%x 0x%x\n" symtab_offset strtab_offset symtab_size strtab_size; *)
  SymbolTable.get_symbol_table_adjusted binary masks symtab_offset symtab_size strtab_offset strtab_size
					    
let kDT_NULL =  0  (* Marks end of dynamic section *)
let kDT_NEEDED = 1  (* Name of needed library *)
let kDT_PLTRELSZ = 2  (* Size in bytes of PLT relocs *)
let kDT_PLTGOT = 3  (* Processor defined value *)
let kDT_HASH =  4  (* Address of symbol hash table *)
let kDT_STRTAB = 5  (* Address of string table *)
let kDT_SYMTAB = 6  (* Address of symbol table *)
let kDT_RELA =  7  (* Address of Rela relocs *)
let kDT_RELASZ = 8  (* Total size of Rela relocs *)
let kDT_RELAENT = 9  (* Size of one Rela reloc *)
let kDT_STRSZ = 10  (* Size of string table *)
let kDT_SYMENT = 11  (* Size of one symbol table entry *)
let kDT_INIT =  12  (* Address of init function *)
let kDT_FINI =  13  (* Address of termination function *)
let kDT_SONAME = 14  (* Name of shared object *)
let kDT_RPATH = 15  (* Library search path (deprecated) *)
let kDT_SYMBOLIC = 16  (* Start symbol search here *)
let kDT_REL =  17  (* Address of Rel relocs *)
let kDT_RELSZ = 18  (* Total size of Rel relocs *)
let kDT_RELENT = 19  (* Size of one Rel reloc *)
let kDT_PLTREL = 20  (* Type of reloc in PLT *)
let kDT_DEBUG = 21  (* For debugging; unspecified *)
let kDT_TEXTREL = 22  (* Reloc might modify .text *)
let kDT_JMPREL = 23  (* Address of PLT relocs *)
let kDT_BIND_NOW = 24  (* Process relocations of object *)
let kDT_INIT_ARRAY = 25  (* Array with addresses of init fct *)
let kDT_FINI_ARRAY = 26  (* Array with addresses of fini fct *)
let kDT_INIT_ARRAYSZ = 27  (* Size in bytes of DT_INIT_ARRAY *)
let kDT_FINI_ARRAYSZ = 28  (* Size in bytes of DT_FINI_ARRAY *)
let kDT_RUNPATH = 29  (* Library search path *)
let kDT_FLAGS = 30  (* Flags for the object being loaded *)
let kDT_ENCODING = 32  (* Start of encoded range *)
let kDT_PREINIT_ARRAY = 32  (* Array with addresses of preinit fct*)
let kDT_PREINIT_ARRAYSZ = 33  (* size in bytes of DT_PREINIT_ARRAY *)
let kDT_NUM = 34  (* Number used *)
let kDT_LOOS =  0x6000000d (* Start of OS-specific *)
let kDT_HIOS =  0x6ffff000 (* End of OS-specific *)
let kDT_LOPROC = 0x70000000 (* Start of processor-specific *)
let kDT_HIPROC = 0x7fffffff (* End of processor-specific *)
(* let kDT_PROCNUM = kDT_MIPS_NUM (* Most used by any processor *) *)

(* DT_* entries which fall between DT_VALRNGHI & DT_VALRNGLO use the
   Dyn.d_un.d_val field of the Elf*_Dyn structure.  This follows Sun's
   approach.  *)
let kDT_VALRNGLO = 0x6ffffd00
let kDT_GNU_PRELINKED = 0x6ffffdf5 (* Prelinking timestamp *)
let kDT_GNU_CONFLICTSZ = 0x6ffffdf6 (* Size of conflict section *)
let kDT_GNU_LIBLISTSZ = 0x6ffffdf7 (* Size of library list *)
let kDT_CHECKSUM = 0x6ffffdf8
let kDT_PLTPADSZ = 0x6ffffdf9
let kDT_MOVEENT = 0x6ffffdfa
let kDT_MOVESZ = 0x6ffffdfb
let kDT_FEATURE_1 = 0x6ffffdfc (* Feature selection (DTF_ *)
let kDT_POSFLAG_1 = 0x6ffffdfd (* Flags for DT_* entries, effecting
        the following DT_* entry.  *)
let kDT_SYMINSZ = 0x6ffffdfe (* Size of syminfo table (in bytes) *)
let kDT_SYMINENT = 0x6ffffdff (* Entry size of syminfo *)
let kDT_VALRNGHI = 0x6ffffdff
let get_DT_VALTAGIDX tag = kDT_VALRNGHI - (tag) (* Reverse order! *)
let kDT_VALNUM = 12

(* DT_* entries which fall between DT_ADDRRNGHI & DT_ADDRRNGLO use the
   Dyn.d_un.d_ptr field of the Elf*_Dyn structure.

   If any adjustment is made to the ELF object after it has been
   built these entries will need to be adjusted.  *)
let kDT_ADDRRNGLO = 0x6ffffe00
let kDT_GNU_HASH = 0x6ffffef5 (* GNU-style hash table.  *)
let kDT_TLSDESC_PLT = 0x6ffffef6
let kDT_TLSDESC_GOT = 0x6ffffef7
let kDT_GNU_CONFLICT = 0x6ffffef8 (* Start of conflict section *)
let kDT_GNU_LIBLIST = 0x6ffffef9 (* Library list *)
let kDT_CONFIG = 0x6ffffefa (* Configuration information.  *)
let kDT_DEPAUDIT = 0x6ffffefb (* Dependency auditing.  *)
let kDT_AUDIT = 0x6ffffefc (* Object auditing.  *)
let kDT_PLTPAD = 0x6ffffefd (* PLT padding.  *)
let kDT_MOVETAB = 0x6ffffefe (* Move table.  *)
let kDT_SYMINFO = 0x6ffffeff (* Syminfo table.  *)
let kDT_ADDRRNGHI = 0x6ffffeff
let get_kDT_ADDRTAGIDX tag = kDT_ADDRRNGHI - (tag) (* Reverse order! *)
let kDT_ADDRNUM = 11

(* The versioning entry types.  The next are defined as part of the
   GNU extension.  *)
let kDT_VERSYM = 0x6ffffff0

let kDT_RELACOUNT = 0x6ffffff9
let kDT_RELCOUNT = 0x6ffffffa

(* These were chosen by Sun.  *)
let kDT_FLAGS_1 = 0x6ffffffb (* State flags, see DF_1_* below.  *)
let kDT_VERDEF = 0x6ffffffc (* Address of version definition
        table *)
let kDT_VERDEFNUM = 0x6ffffffd (* Number of version definitions *)
let kDT_VERNEED = 0x6ffffffe (* Address of table with needed
        versions *)
let kDT_VERNEEDNUM = 0x6fffffff (* Number of needed versions *)
let get_DT_VERSIONTAGIDX tag = kDT_VERNEEDNUM - (tag) (* Reverse order! *)
let kDT_VERSIONTAGNUM = 16

(* fuck sun *)
(* Sun added these machine-independent extensions in the "processor-specific"
   range.  Be compatible.  *)
let kDT_AUXILIARY =    0x7ffffffd      (* Shared object to load before self *)
let kDT_FILTER =       0x7fffffff      (* Shared object to get values from *)
(* let get_DT_EXTRATAGIDX tag = ((4)-((4) (tag) <<1>>1)-1) *)
let kDT_EXTRANUM = 3


(* d_val *)

(* Values of `d_un.d_val' in the DT_FLAGS entry.  *)
let kDF_ORIGIN = 0x00000001 (* Object may use DF_ORIGIN *)
let kDF_SYMBOLIC = 0x00000002 (* Symbol resolutions starts here *)
let kDF_TEXTREL = 0x00000004 (* Object contains text relocations *)
let kDF_BIND_NOW = 0x00000008 (* No lazy binding for this object *)
let kDF_STATIC_TLS = 0x00000010 (* Module uses the static TLS model *)

(* State flags selectable in the `d_un.d_val' element of the DT_FLAGS_1
   entry in the dynamic section.  *)
let kDF_1_NOW = 0x00000001 (* Set RTLD_NOW for this object.  *)
let kDF_1_GLOBAL = 0x00000002 (* Set RTLD_GLOBAL for this object.  *)
let kDF_1_GROUP = 0x00000004 (* Set RTLD_GROUP for this object.  *)
let kDF_1_NODELETE = 0x00000008 (* Set RTLD_NODELETE for this object.*)
let kDF_1_LOADFLTR = 0x00000010 (* Trigger filtee loading at runtime.*)
let kDF_1_INITFIRST = 0x00000020 (* Set RTLD_INITFIRST for this object*)
let kDF_1_NOOPEN = 0x00000040 (* Set RTLD_NOOPEN for this object.  *)
let kDF_1_ORIGIN = 0x00000080 (* $ORIGIN must be handled.  *)
let kDF_1_DIRECT = 0x00000100 (* Direct binding enabled.  *)
let kDF_1_TRANS = 0x00000200
let kDF_1_INTERPOSE = 0x00000400 (* Object is used to interpose.  *)
let kDF_1_NODEFLIB = 0x00000800 (* Ignore default lib search path.  *)
let kDF_1_NODUMP = 0x00001000 (* Object can't be dldump'ed.  *)
let kDF_1_CONFALT = 0x00002000 (* Configuration alternative created.*)
let kDF_1_ENDFILTEE = 0x00004000 (* Filtee terminates filters search. *)
let kDF_1_DISPRELDNE = 0x00008000 (* Disp reloc applied at build time. *)
let kDF_1_DISPRELPND = 0x00010000 (* Disp reloc applied at run-time.  *)
let kDF_1_NODIRECT = 0x00020000 (* Object has no-direct binding. *)
let kDF_1_IGNMULDEF = 0x00040000
let kDF_1_NOKSYMS = 0x00080000
let kDF_1_NOHDR = 0x00100000
let kDF_1_EDITED = 0x00200000 (* Object is modified after built.  *)
let kDF_1_NORELOC = 0x00400000
let kDF_1_SYMINTPOSE = 0x00800000 (* Object has individual interposers.  *)
let kDF_1_GLOBAUDIT = 0x01000000 (* Global auditing required.  *)
let kDF_1_SINGLETON = 0x02000000 (* Singleton symbols are used.  *)
