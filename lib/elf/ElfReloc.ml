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
    r_offset: int;              (* 8 bytes *)
    r_info: int;                (* 8 bytes *)
    r_addend: int;              (* 8 bytes, signed *)
    symindex: int;
    symtype: int;
  }

let sizeof_rela64 = 24 		(* bytes *)

(* 64-bit default, 32t will be 32 bits *)
type t = rela64 list

let set_rela64 bytes rela offset = 
  Binary.set_uint bytes rela.r_offset 8 offset
  |> Binary.set_uint bytes rela.r_info 8
  |> Binary.set_uint bytes rela.r_addend 8

let set bytes relocs offset =
  List.fold_left (fun acc rela -> set_rela64 bytes rela acc) offset relocs

let to_bytes relocs =
  let b = Bytes.create (List.length relocs * sizeof_rela64) in
  ignore @@ set b relocs 0;
  b

let get_sym64  i = i asr 32
let get_type64 i = i land 0xffffffff

let print_rela64 r =
  Printf.printf
    "r_offset: 0%x r_info: 0x%x r_addend: 0x%x index: %d type: 0x%x\n"
    r.r_offset
    r.r_info
    r.r_addend
    r.symindex
    r.symtype

let print_relocs64 = List.iter print_rela64

let get_reloc64 binary o =
  let r_offset,o = Binary.u64o binary o in
  let r_info,o = Binary.u64o binary o in
  let r_addend,_ = Binary.u64o binary o in
  let symindex = get_sym64 r_info in
  let symtype = get_type64 r_info in
  {r_offset; r_info; r_addend; symindex; symtype;}

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
    (List.nth relocs index).r_offset
  with _ -> 0x0
