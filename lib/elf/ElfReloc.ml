(*
           typedef struct {
               Elf32_Addr r_offset;
               uint32_t   r_info;
               int32_t    r_addend;
           } Elf32_Rela;

           typedef struct {
               Elf64_Addr r_offset;
               uint64_t   r_info;
               int64_t    r_addend;
           } Elf64_Rela;
 *)

(*
#define ELF64_R_SYM(i)			((i) >> 32)
#define ELF64_R_TYPE(i)			((i) & 0xffffffff)
 *)

type rela64 =
  {
    offset: int;
    info: int;
    addend: int;
    symindex: int;
    symtype: int;
  }

let sizeof_rela64 = 24 		(* bytes *)

let get_sym64  i = i asr 32
let get_type64 i = i land 0xffffffff

let print_rela64 r =
  Printf.printf
    "r_offset: 0%x r_info: 0x%x addend: 0x%x index: %d type: 0x%x\n"
    r.offset
    r.info
    r.addend
    r.symindex
    r.symtype

let print_relocs64 = List.iter print_rela64

let get_reloc64 binary o =
  let offset,o = Binary.u64o binary o in
  let info,o = Binary.u64o binary o in
  let addend,_ = Binary.u64o binary o in
  let symindex = get_sym64 info in
  let symtype = get_type64 info in
  {offset; info; addend; symindex; symtype;}

let rec get_reloc64_loop binary boundary pos acc =
  if (pos >= boundary) then
    acc
  else
    let reloc = get_reloc64 binary pos in
    get_reloc64_loop
      binary boundary (pos + sizeof_rela64) (reloc::acc)

let get_relocs64 binary (relasz,rela,pltrelsz,jmprel) =
  let nl = get_reloc64_loop binary (rela+relasz) rela [] in
  let l = get_reloc64_loop binary (pltrelsz+jmprel) jmprel [] in
  l @ nl |> List.rev
  
let get_size index relocs =
  try 
    (List.nth relocs index).offset
  with _ -> 0x0
	      (* 
  List.fold_left (fun acc reloc ->
		  if (reloc.name = name) then
		    reloc.offset
		  else
		    acc 	(* 0x0 *)
		 ) 0x0 relocs
 *)







