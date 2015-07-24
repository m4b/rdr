(* TODO: refactor get_elf_header64 to not use explicit additions and use a bytes offset *)

open Binary
(* 
#directory "/Users/matthewbarney/git/rdr/_build/lib/utils/";;
#directory "/Users/matthewbarney/git/rdr/_build/lib/elf/";;
#load "Binary.cmo";;
#load "ElfConstants.cmo";;
 *)
       
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
  let one = u8 bytes 0 in
  let two = (u8 bytes 1) lsl 8 in
  let three = (u8 bytes 2) lsl 16 in
  let four = (u8 bytes 3) lsl 24 in
  let ei_magic = one lor two lor three lor four in
  let ei_class,o = u8o bytes 4 in
  let ei_data,o = u8o bytes o in
  let ei_version,o = u8o bytes o in
  let ei_osabi,o = u8o bytes o in
  let ei_abiversion,o = u8o bytes o in
  let ei_pad = 0x0 in
  {ei_magic; ei_class; ei_data; ei_version; ei_osabi; ei_abiversion; ei_pad;}

let is_64bit e_ident = e_ident.ei_class = 2

let get_elf_header64 binary =
  let e_ident = get_e_ident binary in
  let o = sizeof_e_ident in
  let e_type,o = u16o binary o in
  let e_machine,o = u16o binary o in
  let e_version,o = u32o binary o in
  let e_entry,o = u64o binary o in
  let e_phoff,o = u64o binary o in
  let e_shoff,o = u64o binary o in
  let e_flags,o = u32o binary o in
  let e_ehsize,o = u16o binary o in
  let e_phentsize,o = u16o binary o in
  let e_phnum,o = u16o binary o in
  let e_shentsize,o = u16o binary o in
  let e_shnum,o = u16o binary o in
  let e_shstrndx,o = u16o binary o in
  {
    e_ident; e_type; e_machine; e_version; e_entry; e_phoff;
    e_shoff; e_flags; e_ehsize; e_phentsize; e_phnum;
    e_shentsize; e_shnum; e_shstrndx;
  }

let set_e_ident bytes ident offset = 
  Binary.set_uint bytes ident.ei_magic 4 offset (* this should be big-endian i believe *)
  |> Binary.set_uint bytes ident.ei_class 1
  |> Binary.set_uint bytes ident.ei_data 1
  |> Binary.set_uint bytes ident.ei_version 1
  |> Binary.set_uint bytes ident.ei_osabi 1
  |> Binary.set_uint bytes ident.ei_abiversion 1
  |> Binary.set_uint bytes ident.ei_pad 7

let set bytes header offset =
  set_e_ident bytes header.e_ident offset
  |> Binary.set_uint bytes header.e_type 2
  |> Binary.set_uint bytes header.e_machine 2
  |> Binary.set_uint bytes header.e_version 4
  |> Binary.set_uint bytes header.e_entry 8
  |> Binary.set_uint bytes header.e_phoff 8
  |> Binary.set_uint bytes header.e_shoff 8
  |> Binary.set_uint bytes header.e_flags 4
  |> Binary.set_uint bytes header.e_ehsize 2
  |> Binary.set_uint bytes header.e_phentsize 2
  |> Binary.set_uint bytes header.e_phnum 2
  |> Binary.set_uint bytes header.e_shentsize 2
  |> Binary.set_uint bytes header.e_shnum 2
  |> Binary.set_uint bytes header.e_shstrndx 2

let to_bytes header = 
  let b = Bytes.create sizeof_elf_header64 in
  let offset = set b header 0 in
  ignore offset;
  b

let h0 =
{e_ident =
  {ei_magic = 1179403647; ei_class = 2; ei_data = 1; ei_version = 1;
   ei_osabi = 0; ei_abiversion = 0; ei_pad = 0};
 e_type = 2; e_machine = 183; e_version = 1; e_entry = 4319080; e_phoff = 64;
 e_shoff = 845672; e_flags = 0; e_ehsize = 0; e_phentsize = 0; e_phnum = 0;
 e_shentsize = 0; e_shnum = 0; e_shstrndx = 0}

let h1 = "\x7f\x45\x4c\x46\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xb7\x00\x01\x00\x00\x00\x68\xe7\x41\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x68\xe7\x0c\x00\x00\x00\x00\x00"

let h2 = "\x7f\x45\x4c\x46\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xb7\x00\x01\x00\x00\x00\x68\xe7\x41\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x68\xe7\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"

(* let u1 = get_elf_header64 h1 |> print_elf_header64 *)
