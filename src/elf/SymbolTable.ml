(* builds the debug symbol table; will be empty if stripped *)

open SectionHeader

(* Legal values for ST_BIND subfield of st_info (symbol binding).  *)
let kSTB_LOCAL = 0  (* Local symbol *)
let kSTB_GLOBAL = 1  (* Global symbol *)
let kSTB_WEAK = 2  (* Weak symbol *)
let kSTB_NUM =  3  (* Number of defined types.  *)
let kSTB_LOOS = 10  (* Start of OS-specific *)
let kSTB_GNU_UNIQUE = 10  (* Unique symbol.  *)
let kSTB_HIOS = 12  (* End of OS-specific *)
let kSTB_LOPROC = 13  (* Start of processor-specific *)
let kSTB_HIPROC = 15  (* End of processor-specific *)

(* Legal values for ST_TYPE subfield of st_info (symbol type).  *)

let kSTT_NOTYPE = 0  (* Symbol type is unspecified *)
let kSTT_OBJECT = 1  (* Symbol is a data object *)
let kSTT_FUNC = 2  (* Symbol is a code object *)
let kSTT_SECTION = 3  (* Symbol associated with a section *)
let kSTT_FILE = 4  (* Symbol's name is file name *)
let kSTT_COMMON = 5  (* Symbol is a common data object *)
let kSTT_TLS =  6  (* Symbol is thread-local data object*)
let kSTT_NUM =  7  (* Number of defined types.  *)
let kSTT_LOOS = 10  (* Start of OS-specific *)
let kSTT_GNU_IFUNC = 10  (* Symbol is indirect code object *)
let kSTT_HIOS = 12  (* End of OS-specific *)
let kSTT_LOPROC = 13  (* Start of processor-specific *)
let kSTT_HIPROC = 15  (* End of processor-specific *)
(* ================ *)

let symbol_bind_to_string bind =
  match bind with
  | 0 -> "LOCAL"
  | 1 -> "GLOBAL"
  | 2 -> "WEAK"
  | 3 -> "NUM "
  (* | 10 -> "LOOS" *)
  | 10 -> "GNU_UNIQUE" (* not sure what this is *)
  | 12 -> "HIOS"
  | 13 -> "LOPROC"
  | 15 -> "HIPROC"
  | _ -> "UNKNOWN SYMBOL BIND"

let symbol_type_to_string sttype =
  match sttype with
  | 0 -> "NOTYPE"
  | 1 -> "OBJECT"
  | 2 -> "FUNC"
  | 3 -> "SECTION"
  | 4 -> "FILE"
  | 5 -> "COMMON"
  | 6 -> "TLS "
  | 7 -> "NUM "
  (* | 10 -> "LOOS" *)
  | 10 -> "GNU_IFUNC" (* this is an "indirect function" which chooses an implementation at runtime *)
  | 12 -> "HIOS"
  | 13 -> "LOPROC"
  | 15 -> "HIPROC"
  | _ -> "UNKNOWN SYMBOL TYPE"

(* 
 * How to extract and insert information held in the st_info field.

#define ELF32_ST_BIND(val)  (((unsigned char) (val)) >> 4)
#define ELF32_ST_TYPE(val)  ((val) & 0xf)
#define ELF32_ST_INFO(bind, type) (((bind) << 4) + ((type) & 0xf))
 *)

let get_bind info = info lsr 4
let get_type info = info land 0xf

(* Symbol visibility specification encoded in the st_other field.  *)
let kSTV_DEFAULT = 0  (* Default symbol visibility rules *)
let kSTV_INTERNAL = 1  (* Processor specific hidden class *)
let kSTV_HIDDEN = 2  (* Sym unavailable in other modules *)
let kSTV_PROTECTED = 3  (* Not preemptible, not exported *)

let symbol_visibility_to_string visibility =
  match visibility with
  | 0 -> "DEFAULT"
  | 1 -> "INTERNAL"
  | 2 -> "HIDDEN"
  | 3 -> "PROTECTED"
  | _ -> "UKNOWN SYMBOL VISIBILITY"

(* How to extract and insert information held in the st_other field. *)
	   
let get_visibility other = other land 0x03

(* =========================== *)
(*
            typedef struct {
               uint32_t      st_name;
               unsigned char st_info;
               unsigned char st_other;
               uint16_t      st_shndx;
               Elf64_Addr    st_value;
               uint64_t      st_size;
           } Elf64_Sym;
*)

type symbol_entry =
  {
    mutable name: string;
    st_name: int;   (* 4 *)
    st_info: int;  (* 1 *)
    st_other: int;  (* 1 *)
    st_shndx: int;  (* 2 *)
    st_value: int;  (* 8 *)
    st_size: int;  (* 8 *)
  }

let sizeof_symbol_entry = 24

let symbol_to_string symbol =
  Printf.sprintf "%s 0x%x %s %s %s size: %d index: %d "
   symbol.name
   symbol.st_value
   (get_bind symbol.st_info |> symbol_bind_to_string)
   (get_type symbol.st_info |> symbol_type_to_string)
   (get_visibility symbol.st_other |> symbol_visibility_to_string)
   symbol.st_size
   symbol.st_shndx

let get_symbol_entry bytes offset =
    let name = "" in
    let st_name = Binary.u32 bytes offset in
    let st_info = Binary.u8 bytes (offset + 4) in 
    let st_other = Binary.u8 bytes (offset + 5) in
    let st_shndx = Binary.u16 bytes (offset + 6) in
    let st_value = Binary.u64 bytes (offset + 8) in
    let st_size = Binary.u64 bytes (offset + 16) in
    {
      name;
      st_name;
      st_info;
      st_other;
      st_shndx;
      st_value;
      st_size;
    }

let get_symbol_entry_adjusted bytes masks offset =
    let name = "" in
    let st_name = Binary.u32 bytes offset in
    let st_info = Binary.u8 bytes (offset + 4) in 
    let st_other = Binary.u8 bytes (offset + 5) in
    let st_shndx = Binary.u16 bytes (offset + 6) in
    let st_value = Binary.u64 bytes (offset + 8) |> ProgramHeader.adjust masks in
    let st_size = Binary.u64 bytes (offset + 16) in
    {
      name;
      st_name;
      st_info;
      st_other;
      st_shndx;
      st_value;
      st_size;
    }
      
let get_symtab bytes offset size =
  let len = size + offset in
  let rec loop pos acc =
    if (pos >= len) then
      acc
    else
      loop (pos + sizeof_symbol_entry) ((get_symbol_entry bytes pos)::acc)
  in loop offset []

(* adjusted by vm addr mask list offsets *)
let get_symtab_adjusted bytes masks offset size =
  let len = size + offset in
  let rec loop pos acc =
    if (pos >= len) then
      acc
    else
      loop (pos + sizeof_symbol_entry) ((get_symbol_entry_adjusted bytes masks pos)::acc)
  in loop offset []

(* update the symbol name using the symbol table data offset into the binary *)
let amend_symbol_table binary offset size symbol_table =
  List.iter (fun sym ->
	      sym.name <- Binary.string binary (offset + sym.st_name);
	     ) symbol_table;
  symbol_table
	  
let print_symbol_table entries =
  Printf.printf "Symbol Table (%d):\n" @@ List.length entries;
  List.iteri (fun i elem -> Printf.printf "(%d) %s\n" i @@ symbol_to_string elem) entries

let get_symbol_table binary section_headers =
  match find_sections_by_type SectionHeader.kSHT_SYMTAB section_headers with
  | [] -> []
  | sh::shs ->
     let offset = sh.sh_offset in
     let size = sh.sh_size in
     let strtab = section_headers.(sh.sh_link) in
     get_symtab binary offset size |> amend_symbol_table binary strtab.sh_offset strtab.sh_size

let get_symbol_table_with_offsets binary offset size strtab_offset strtab_size =
     get_symtab binary offset size |> amend_symbol_table binary strtab_offset strtab_size

let get_symbol_table_adjusted binary masks offset size strtab_offset strtab_size =
  get_symtab_adjusted binary masks offset size
  |> amend_symbol_table binary strtab_offset strtab_size
  |> List.rev 			(* otherwise relocs backwards *)

(* goblin *)

(* hacky function to filter imports from exports, etc. *)
(* todo use proper variants here ffs *)
let get_goblin_kind entry bind stype =
  if (entry.st_value = 0x0
       && entry.st_shndx = 0
       && entry.name <> "")
  then
      (*        && stype = "FUNC")
      || (bind = "GLOBAL" && stype = "OBJECT")
 *)
    GoblinSymbol.Import
  else if (bind = "LOCAL") then
    GoblinSymbol.Local
  else if ((bind = "GLOBAL"
	    || (bind = "WEAK" && (stype = "FUNC"
				  || stype = "IFUNC"
				  || stype = "OBJECT")))
	   && entry.st_value <> 0) then
    GoblinSymbol.Export
  else GoblinSymbol.Other

(* polymorphic variants don't need to be qualified by module
 since they are open and the symbol is unique *)
let symbol_entry_to_goblin_symbol
      ~tol:tol ~libs:libs ~relocs:relocs (soname,install_name) index entry =
  let bind   = (get_bind entry.st_info |> symbol_bind_to_string) in
  let stype  = (get_type entry.st_info |> symbol_type_to_string) in
  let name   = `Name entry.name in
  let offset =
    `Offset (
       if (entry.st_value = 0) then
	 (* this _could_ be relatively expensive *)
	 ElfReloc.get_size index relocs
       else
	 entry.st_value)
  in
  let size = `Size entry.st_size in
  let kind = `Kind (get_goblin_kind entry bind stype) in
  let lib =
    (* TODO: this is a complete disaster; *)
    match kind with
    | `Kind GoblinSymbol.Export ->
       `Lib (soname,install_name)
    | `Kind GoblinSymbol.Import ->
       if (ToL.is_empty tol) then
	 `Lib ("∅","∅")
       else
	 let l = (ToL.get_libraries ~bin_libs:libs entry.name tol) in
	 `Lib (l,l)
    | _ ->
       `Lib ("","")
  in
  let data = `PrintableData
	      (Printf.sprintf
		 "%s %s" bind stype) in
  [name; lib; offset; size; kind; data]

let symbols_to_goblin ?use_tol:(use_tol=true) ~libs:libs soname dynsyms relocs =
  let tol =
    try
      if (use_tol) then ToL.get () else ToL.empty
    with ToL.Not_built ->
      ToL.empty
  in
  List.mapi
    (symbol_entry_to_goblin_symbol
       ~tol:tol ~libs:libs ~relocs:relocs soname) dynsyms
