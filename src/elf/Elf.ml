open Binary

type e_ident =
  {
    ei_magic: int; (* 7fELF mother fucker *)
    ei_class: int;
    ei_data: int; (* 1 = little endian, 2 = big endian *)
    ei_version: int;
    ei_osabi: int; (* often set to zero for all platforms linux = 3 *)
    ei_abiversion: int; (* after kernel 2.6 set to zero *)
    ei_pad: int; (* 7 bytes of padding, making next offset 0x10 *)
  }

let sizeof_e_ident = 16 (* bytes *)    

let kMAGIC_ELF = 0x7f454c46
let kCIGAM_ELF = 0x464c457f 
		   
type elf_header64 =
  {
    e_ident: e_ident;
    e_type: int; (* 2 bytes 1 = relocatable, 2 = executable, 3 = shared 4 = core *)
    e_machine: int; (* 3E = x86-64 B7 = AArch64 *) (* 2 bytes *)
    e_version: int; (* 4 bytes, set to 1 *)
    e_entry: int; (* either 4 or 8 bytes if 32-bit or 64-bit *)
    e_phoff: int; (* Points to the start of the program header table. It usually follows the file header immediately making the offset 0x40 for 64-bit ELF executables. *)
    e_shoff: int; (* Points to the start of the section header table. *)
    e_flags: int; (* 4 bytes *)
    e_ehsize: int; (* remaining are 2 bytes; Contains the size of this header, normally 64 bytes (0x40) for 64-bit and 52 for 32-bit format. *)
    e_phentsize: int (* Contains the size of a program header table entry. *);
    e_phnum: int (* Contains the number of entries in the program header table. *);
    e_shentsize: int (* Contains the size of a section header table entry. *);
    e_shnum: int (* Contains the number of entries in the section header table. *);
    e_shstrndx: int (* Contains index of the section header table entry that contains the section names. *);        
  }

let sizeof_elf_header64 = 48 (* bytes *)        

let e_ident_to_string e_ident =
  Printf.sprintf "magic: %x \nclass: 0x%x \ndata: 0x%x \nversion: 0x%x \nosabi: 0x%x \nabiversion: 0x%x\n"
	  e_ident.ei_magic
	  e_ident.ei_class
	  e_ident.ei_data
	  e_ident.ei_version
	  e_ident.ei_osabi
	  e_ident.ei_abiversion
	  
let print_verbose_elf_header64 header =
  Printf.printf "%s type: %d machine: 0x%x version: %d entry: 0x%x phoff: 0x%x shoff: 0x%x flags: 0x%x ehsize: 0x%x phentsize: 0x%x phnum: 0x%x shentsize: 0x%x shnum: 0x%x shstrndx: 0x%x\n" (e_ident_to_string header.e_ident)
	 header.e_type
	 header.e_machine
	 header.e_version
	 header.e_entry
	 header.e_phoff
	 header.e_shoff
	 header.e_flags
	 header.e_ehsize
	 header.e_phentsize
	 header.e_phnum
	 header.e_shentsize
	 header.e_shnum
	 header.e_shstrndx

let print_elf_header64 header =
  Printf.printf "ELF %s %s\n"
		(Constants.machine_to_string header.e_machine)
		(Constants.etype_to_string header.e_type)

	 
let get_e_ident bytes =
  let one = Char.code @@ Bytes.get bytes 0 in
  let two = (Char.code @@ Bytes.get bytes 1) lsl 8 in
  let three = (Char.code @@ Bytes.get bytes 2) lsl 16 in
  let four = (Char.code @@ Bytes.get bytes 3) lsl 24 in
  let ei_magic = one lor two lor three lor four in
  let ei_class = Char.code @@ Bytes.get bytes 4 in
  let ei_data = Char.code @@ Bytes.get bytes 5 in
  let ei_version = Char.code @@ Bytes.get bytes 6 in
  let ei_osabi = Char.code @@ Bytes.get bytes 7 in
  let ei_abiversion = Char.code @@ Bytes.get bytes 8 in
  let ei_pad = 0x0 in
  {ei_magic; ei_class; ei_data; ei_version; ei_osabi; ei_abiversion; ei_pad;}
    
let get_elf_header64 binary =
  let e_ident = get_e_ident binary in
  let i = sizeof_e_ident in
  let e_type = i16 binary i in
  let e_machine = i16 binary (i + 2) in
  let e_version = i32 binary (i + 4) in
  let e_entry = i64 binary (i + 8) in
  let e_phoff = i64 binary (i + 16) in
  let e_shoff = i64 binary (i + 24) in
  let e_flags = i32 binary (i + 32) in
  let e_ehsize = i16 binary (i + 36) in
  let e_phentsize = i16 binary (i + 38) in
  let e_phnum = i16 binary (i +  40) in
  let e_shentsize = i16 binary (i + 42) in
  let e_shnum = i16 binary (i + 44) in
  let e_shstrndx = i16 binary (i + 46) in
  {
    e_ident; e_type; e_machine; e_version; e_entry; e_phoff;
    e_shoff; e_flags; e_ehsize; e_phentsize; e_phnum;
    e_shentsize; e_shnum; e_shstrndx;
  }
    
let analyze binary =
  let header = get_elf_header64 binary in
  let program_headers = ProgramHeader.get_program_headers binary header.e_phoff header.e_phentsize header.e_phnum in
  let vaddr_masks = ProgramHeader.get_vaddr_masks program_headers in
  (*   List.iter (fun x -> Printf.printf "0x%x\n" x) vaddr_masks; *)
  let section_headers = SectionHeader.get_section_headers binary header.e_shoff header.e_shentsize header.e_shnum in
  let symbol_table = SymbolTable.get_symbol_table binary section_headers in
  let _DYNAMIC = Dynamic.get_DYNAMIC binary program_headers in
  let symtab_offset, strtab_offset, strtab_size = Dynamic.get_dynamic_symbol_offset_data _DYNAMIC in
  let symtab_offset = ProgramHeader.adjust vaddr_masks symtab_offset in
  let strtab_offset = ProgramHeader.adjust vaddr_masks strtab_offset in
  (*   Printf.printf "0x%x 0x%x 0x%x\n" symtab_offset strtab_offset strtab_size; *)
  let dynamic_strtab = Dynamic.get_dynamic_strtab binary strtab_offset strtab_size in
  let dynamic_symbols = Dynamic.get_dynamic_symbols binary vaddr_masks symtab_offset strtab_offset strtab_size in
  print_elf_header64 header;
  ProgramHeader.print_program_headers program_headers;
  SectionHeader.print_section_headers section_headers;
  (* SymbolTable.print_symbol_table symbol_table; *)
  Dynamic.print_DYNAMIC _DYNAMIC;
  SymbolTable.print_symbol_table dynamic_symbols;
