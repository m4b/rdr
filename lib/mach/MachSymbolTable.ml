(* TODO: replace functions with Binary string functions *)
(* TODO: remove bytes sub in binary calls *)

open Printf

open Binary
open MachLoadCommand.Types

(*
struct nlist_64 {
    union {
        uint32_t  n_strx; /* index into the string table */
    } n_un;
    uint8_t n_type;        /* type flag, see below */
    uint8_t n_sect;        /* section number or NO_SECT */
    uint16_t n_desc;       /* see <mach-o/stab.h> */
    uint64_t n_value;      /* value of this symbol (or stab offset) */
};
 *)

type nlist = {
  n_strx: int; (* 4 *)
  n_type: int; (* 1 *)
  n_sect: int; (* 1 *)
  n_desc: int; (* 2 *)
  n_value: int; (* 8 *)
}

let kNLIST_TYPE_MASK = 0xe
let kNLIST_TYPE_GLOBAL = 0x1
let kNLIST_TYPE_LOCAL = 0x0

type symbol = nlist * string

type t = symbol list

let sizeof_nlist = 16

(* printing *)

let nlist_to_string nlist =
  sprintf "strx: %4u type: 0x%02x sect: %x desc: 0x%3u value: %x"
    nlist.n_strx
    nlist.n_type
    nlist.n_sect
    nlist.n_desc
    nlist.n_value

let print_nlist nlist =
  printf "%s\n" @@ nlist_to_string nlist

let print_nlists nlists =
  List.iteri (fun i nlist -> printf "(%3d): " i; print_nlist nlist) nlists; (* because 3 space formatting is _enough_ *)
  (* i.e., need to learn format module *)
  printf "\n"

let print symlist =
  List.iteri (fun i (nlist,symname) ->
      printf "%-10x %s sect: %x type: %002x desc: 0x%x\n" nlist.n_value symname  nlist.n_sect nlist.n_type nlist.n_desc
    ) symlist;
  printf "\n"

(* build nlist, symtabs, symlist *)

let rec get_nlist_it binary offset nsyms acc =
  if (nsyms <= 0) then
    List.rev acc
  else
    let n_strx,o = u32o binary offset in
    let n_type,o = u8o binary o in
    let n_sect,o = u8o binary o in
    let n_desc,o = u16o binary o in
    let n_value,o = u64o binary o in
    let nlist = {n_strx; n_type; n_sect; n_desc; n_value;} in
    get_nlist_it binary o (nsyms - 1) (nlist::acc)

(* TODO: remove all these bytes.sub calls so can change binary into stream if necessary *)
let rec get_symlist_it binary strsize nlists acc = 
  match nlists with
    [] -> List.rev acc
  | nlist::nlists ->
    (* TODO: replace this with Binary string functions *)
    let index = Bytes.index_from binary nlist.n_strx '\000' in
    let symname = Bytes.sub_string binary nlist.n_strx (index - nlist.n_strx) in
    get_symlist_it binary strsize nlists ((nlist,symname)::acc)

let get_symlist binary symtab nlists = 
  let str_bytes = Bytes.sub binary symtab.stroff symtab.strsize in
  get_symlist_it str_bytes symtab.strsize nlists []

let get_symbols binary symtab =
    let nlist_bytes = Bytes.sub binary symtab.symoff (sizeof_nlist * symtab.nsyms) in
    let nlists = get_nlist_it nlist_bytes 0 symtab.nsyms [] in
    let symbols = get_symlist binary symtab nlists in
    symbols
