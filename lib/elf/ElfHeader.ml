open Binary
       
type e_ident =
  {
    ei_magic: int; (* 7fELF 4 bytes *)
    ei_class: int; (* 1 = 32 bit, 2 = 64 bit, 1 byte *)
    ei_data: int; (* 1 = little endian, 2 = big endian, 1 byte *)
    ei_version: int;            (* 1 byte  *)
    ei_osabi: int; (* often set to zero for all platforms linux = 3 1 byte *)
    ei_abiversion: int; (* after kernel 2.6 set to zero 1 byte *)
    ei_pad: int; (* 7 bytes of padding, making next offset 0x10 *)
  }

let sizeof_e_ident = 16 (* bytes *)    

let kMAGIC_ELF = 0x7f454c46
let kCIGAM_ELF = 0x464c457f 

(* 64 bit is default; 32 will be t32 *)
type t =
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
  Printf.printf "ELF %s %s @ 0x%x\n"
		(ElfConstants.machine_to_string header.e_machine)
		(ElfConstants.etype_to_string header.e_type)
		(header.e_entry)

(* hack to check whether 64 bit without consuming stuff *)
let check_64bit bytes =
  let ei_class = Binary.u8 bytes 4 in			    (* 4 bytes in past 7fELF *)
  ei_class = 2

let is_lib header = header.e_type = 3
let is_supported header = header.e_type = 2 || header.e_type = 3 (* executable or dy lib *)

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

let is_64bit e_ident = e_ident.ei_class = 2
		       
let get_elf_header64 binary =
  let e_ident = get_e_ident binary in
  let i = sizeof_e_ident in
  let e_type = u16 binary i in
  let e_machine = u16 binary (i + 2) in
  let e_version = u32 binary (i + 4) in
  let e_entry = u64 binary (i + 8) in
  let e_phoff = u64 binary (i + 16) in
  let e_shoff = u64 binary (i + 24) in
  let e_flags = u32 binary (i + 32) in
  let e_ehsize = u16 binary (i + 36) in
  let e_phentsize = u16 binary (i + 38) in
  let e_phnum = u16 binary (i +  40) in
  let e_shentsize = u16 binary (i + 42) in
  let e_shnum = u16 binary (i + 44) in
  let e_shstrndx = u16 binary (i + 46) in
  {
    e_ident; e_type; e_machine; e_version; e_entry; e_phoff;
    e_shoff; e_flags; e_ehsize; e_phentsize; e_phnum;
    e_shentsize; e_shnum; e_shstrndx;
  }

let e_ident_to_bytes bytes offset ident = ()
  (* kCIGAM_ELF  set                 (*  *) *)

let to_bytes bytes offset header = 
  let b = Bytes.create sizeof_elf_header64 in
  ()
  (*   Bytes.blit *)
