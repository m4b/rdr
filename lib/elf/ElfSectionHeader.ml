(*
            typedef struct {
               uint32_t   sh_name;
               uint32_t   sh_type;
               uint64_t   sh_flags;
               Elf64_Addr sh_addr;
               Elf64_Off  sh_offset;
               uint64_t   sh_size;
               uint32_t   sh_link;
               uint32_t   sh_info;
               uint64_t   sh_addralign;
               uint64_t   sh_entsize;
           } Elf64_Shdr;
 *)

type section_header =
  {
    mutable name: string;
    sh_name: int; 		(* 4 *)
    sh_type: int;		(* 4 *)
    sh_flags: int;		(* 8 *)
    sh_addr: int;		(* 8 *)
    sh_offset: int;		(* 8 *)
    sh_size: int;		(* 8 *)
    sh_link: int;		(* 4 *)
    sh_info: int;		(* 4 *)
    sh_addralign: int;		(* 8 *)
    sh_entsize: int;		(* 8 *)
  }

let sizeof_section_header = 64 	(* bytes *)    

(* 64 bits; t32 will be 32-bits *)
type t = section_header array

let kSHT_NULL=  0(* Section header table entry unused *)
let kSHT_PROGBITS=  1(* Program data *)
let kSHT_SYMTAB=  2(* Symbol table *)
let kSHT_STRTAB=  3(* String table *)
let kSHT_RELA=  4(* Relocation entries with addends *)
let kSHT_HASH=  5(* Symbol hash table *)
let kSHT_DYNAMIC=  6(* Dynamic linking information *)
let kSHT_NOTE=  7(* Notes *)
let kSHT_NOBITS=  8(* Program space with no data (bss) *)
let kSHT_REL=  9(* Relocation entries, no addends *)
let kSHT_SHLIB=  10(* Reserved *)
let kSHT_DYNSYM=  11(* Dynamic linker symbol table *)
let kSHT_INIT_ARRAY=  14(* Array of constructors *)
let kSHT_FINI_ARRAY=  15(* Array of destructors *)
let kSHT_PREINIT_ARRAY= 16(* Array of pre-constructors *)
let kSHT_GROUP=  17(* Section group *)
let kSHT_SYMTAB_SHNDX=  18(* Extended section indeces *)
let kSHT_NUM=  19(* Number of defined types.  *)
let kSHT_LOOS=  0x60000000(* Start OS-specific.  *)
let kSHT_GNU_ATTRIBUTES= 0x6ffffff5(* Object attributes.  *)
let kSHT_GNU_HASH=  0x6ffffff6(* GNU-style hash table.  *)
let kSHT_GNU_LIBLIST=  0x6ffffff7(* Prelink library list *)
let kSHT_CHECKSUM=  0x6ffffff8(* Checksum for DSO content.  *)
let kSHT_LOSUNW=  0x6ffffffa(* Sun-specific low bound.  *)
let kSHT_SUNW_move=  0x6ffffffa
let kSHT_SUNW_COMDAT=   0x6ffffffb
let kSHT_SUNW_syminfo=  0x6ffffffc
let kSHT_GNU_verdef=  0x6ffffffd(* Version definition section.  *)
let kSHT_GNU_verneed=  0x6ffffffe(* Version needs section.  *)
let kSHT_GNU_versym=  0x6fffffff(* Version symbol table.  *)
let kSHT_HISUNW=  0x6fffffff(* Sun-specific high bound.  *)
let kSHT_HIOS=  0x6fffffff(* End OS-specific type *)
let kSHT_LOPROC=  0x70000000(* Start of processor-specific *)
let kSHT_HIPROC=  0x7fffffff(* End of processor-specific *)
let kSHT_LOUSER=  0x80000000(* Start of application-specific *)
let kSHT_HIUSER=  0x8fffffff(* End of application-specific *)

let shtype_to_string shtype =
  match shtype with 
  | 0 -> "NULL"
  | 1 -> "PROGBITS"
  | 2 -> "SYMTAB"
  | 3 -> "STRTAB"
  | 4 -> "RELA"
  | 5 -> "HASH"
  | 6 -> "DYNAMIC"
  | 7 -> "NOTE"
  | 8 -> "NOBITS"
  | 9 -> "REL"
  | 10 -> "SHLIB"
  | 11 -> "DYNSYM"
  | 14 ->"INIT_ARRAY"
  | 15 -> "FINI_ARRAY"  
  | 16 -> "PREINIT_ARRAY" 
  | 17 -> "GROUP"
  | 18 ->  "SYMTAB_SHNDX"  
  | 19 -> "NUM"
  | 0x60000000 -> "LOOS"
  | 0x6ffffff5 -> "GNU_ATTRIBUTES" 
  | 0x6ffffff6 -> "GNU_HASH"  
  | 0x6ffffff7 -> "GNU_LIBLIST"  
  | 0x6ffffff8 -> "CHECKSUM"
  | 0x6ffffffa -> "LOSUNW" (* SUNW_move *)
  | 0x6ffffffb ->"SUNW_COMDAT"   
  | 0x6ffffffc -> "SHT_SUNW_syminfo"
  | 0x6ffffffd -> "GNU_verdef"
  | 0x6ffffffe -> "GNU_verneed"
  | 0x6fffffff -> "GNU_versym" 	(* HISUNW, END HIOS *)
  | 0x70000000 -> "LOPROC"
  | 0x7fffffff -> "HIPROC"
  | 0x80000000 -> "LOUSER"
  | 0x8fffffff -> "HIUSER"
  | _ -> "UNKNOWN SECTION HEADER TYPE"

(* Legal values for sh_flags (section flags).  *)

let kSHF_WRITE = (1 lsl 0)	(* Writable *)
let kSHF_ALLOC = (1 lsl 1)	(* Occupies memory during execution *)
let kSHF_EXECINSTR = (1 lsl 2)	(* Executable *)
let kSHF_MERGE = (1 lsl 4)	(* Might be merged *)
let kSHF_STRINGS = (1 lsl 5)	(* Contains nul-terminated strings *)
let kSHF_INFO_LINK = (1 lsl 6)	(* `sh_info' contains SHT index *)
let kSHF_LINK_ORDER =  (1 lsl 7)	(* Preserve order after combining *)
let kSHF_OS_NONCONFORMING = (1 lsl 8)	(* Non-standard OS specific handling required *)
let kSHF_GROUP = (1 lsl 9)	(* Section is member of a group.  *)
let kSHF_TLS =  (1 lsl 10)	(* Section hold thread-local data.  *)
let kSHF_MASKOS = 0x0ff00000	(* OS-specific.  *)
let kSHF_MASKPROC = 0xf0000000	(* Processor-specific *)
let kSHF_ORDERED = (1 lsl 30)	(* Special ordering requirement (Solaris).  *)
let kSHF_EXCLUDE = (1 lsl 31)	(* Section is excluded unless referenced or allocated (Solaris).*)
		     
let to_string sh =
  Printf.sprintf "%-18s %-11s \n\tflags: 0x%02x addr: 0x%08x offset 0x%04x size: 0x%03x link: %2d info: %2d addralign: %2d entsize: %d"
		 (if (sh.name <> "") then
		    sh.name
		  else Printf.sprintf "0x%x" sh.sh_name)
		 (shtype_to_string sh.sh_type)
		 sh.sh_flags
		 sh.sh_addr
		 sh.sh_offset
		 sh.sh_size
		 sh.sh_link
		 sh.sh_info
		 sh.sh_addralign
		 sh.sh_entsize

let print_section_headers shs =
  Printf.printf "Section Headers (%d):\n" @@ Array.length shs;
  Array.iteri (fun i sh -> Printf.printf "(%2d) %s\n" i @@ to_string sh) shs

let get_section_header binary offset =
  let sh_name = Binary.u32 binary offset in
  let sh_type = Binary.u32 binary (offset + 4) in
  let sh_flags = Binary.u64 binary (offset + 8) in
  let sh_addr = Binary.u64 binary (offset + 16) in
  let sh_offset = Binary.u64 binary (offset + 24) in
  let sh_size = Binary.u64 binary (offset + 32) in
  let sh_link = Binary.u32 binary (offset + 40) in
  let sh_info = Binary.u32 binary (offset + 44) in
  let sh_addralign = Binary.u64 binary (offset + 48) in
  let sh_entsize = Binary.u64 binary (offset + 56) in
  let name = "" in
  {
    name;
    sh_name;
    sh_type;
    sh_flags;
    sh_addr;
    sh_offset;
    sh_size;
    sh_link;
    sh_info;
    sh_addralign;
    sh_entsize;
  }

(* unused *)
module IntMap = Map.Make(struct type t = int let compare = compare end)

let print_map = IntMap.iter (fun key elem -> Printf.printf "0x%x -> %s\n" key elem) 

let get_name key map = try IntMap.find key map with Not_found -> ""

let get_section_names binary offset size =
  let rec loop pos map =
    if (pos >= (offset + size)) then map
    else
      let string, pos' = Binary.stringo binary pos in
      loop pos' (IntMap.add (pos - offset) string map)
  in loop offset (IntMap.empty)
(* unused *)

let rec find_shstrtab shs =
  match shs with
  | [] -> None
  | sh::shs ->
     begin
       if ((sh.sh_flags) = 0 && (sh.sh_type = kSHT_STRTAB)) then
       Some sh
       else
       find_shstrtab shs
     end

let update_section_headers_with_names binary shs =
  match find_shstrtab shs with
  | Some sh ->
     let base_offset = sh.sh_offset in
     List.iter (fun sh ->
		(* must be very careful, the strtab _relies_ on null termination of strings, and performs optimizations, like jumping into the middle of a string, e.g., rela.plt and just .plt to perform a kind of trie-esque string saving format... *)
		sh.name <- Binary.string binary (base_offset + sh.sh_name);
	       ) shs;
     shs
  | None ->
     (* just return the unmodifed section headers if we can't find the shstrtab *)
     shs

(* TODO: use array? *)
let get_section_headers binary shoff shentsize shnum =
    let rec loop count offset acc =
    if (count >= shnum) then
      List.rev acc |> update_section_headers_with_names binary |> Array.of_list
    else
      let ph = get_section_header binary offset in
      loop (count + 1) (offset + shentsize) (ph::acc)
  in loop 0 shoff []

let find_section_by_type section_type shs =
  let section = ref None in
  for i = 0 to (Array.length shs) - 1 do
    if (shs.(i).sh_type = section_type) then
      section := Some shs.(i)
  done;
  section

let find_sections_by_type section_type shs =
  Array.fold_left (fun acc elem ->
		   if (elem.sh_type = section_type) then
		     (elem::acc)
		   else
		     acc) [] shs    

let get_dynamic_section shs =
  Array.fold_left (fun acc elem ->
		  if (elem.sh_type = kSHT_DYNAMIC) then
		     Some elem
		   else
		     acc) None shs 
		  

let set_section_header bytes sh offset =
    Binary.set_uint bytes sh.sh_name 4 offset
  |> Binary.set_uint bytes sh.sh_type 4
  |> Binary.set_uint bytes sh.sh_flags 8
  |> Binary.set_uint bytes sh.sh_addr 8
  |> Binary.set_uint bytes sh.sh_offset 8
  |> Binary.set_uint bytes sh.sh_size 8
  |> Binary.set_uint bytes sh.sh_link 4
  |> Binary.set_uint bytes sh.sh_info 4
  |> Binary.set_uint bytes sh.sh_addralign 8
  |> Binary.set_uint bytes sh.sh_entsize 8

let set bytes shs offset =
    Array.fold_left (fun acc sh -> set_section_header bytes sh acc) offset shs

let to_bytes shs =
  let b = Bytes.create ((Array.length shs) * sizeof_section_header) in
  ignore @@ set b shs 0;
  b

