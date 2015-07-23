open Printf

open Binary
open MachLoadCommand

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
  strx: int; (* 32 *)
  n_type: int; (* byte *)
  n_sect: int; (* byte *)
  n_desc: int; (* uint16_t *)
  n_value: int; (* 64 byte *)
}

let kNLIST_TYPE_MASK = 0xe
let kNLIST_TYPE_GLOBAL = 0x1
let kNLIST_TYPE_LOCAL = 0x0

type symbol = nlist * string

let sizeof_nlist = 4 + 1 + 1 + 2 + 8

(* printing *)

let nlist_to_string nlist = 
  sprintf "strx: %4u type: 0x%02x sect: %x desc: 0x%3u value: %x" 
    nlist.strx
    nlist.n_type
    nlist.n_sect
    nlist.n_desc
    nlist.n_value

let print_nlist nlist = 
  printf "%s\n" (nlist_to_string nlist)

let print_nlists nlists =
  List.iteri (fun i nlist -> printf "(%3d): " i; print_nlist nlist) nlists; (* because 3 space formatting is _enough_ *)
  (* i.e., need to learn format module *)
  printf "\n"

let print_symlist symlist =
  List.iteri (fun i (nlist,symname) ->
      printf "%-10x %s sect: %x type: %002x desc: 0x%x\n" nlist.n_value symname  nlist.n_sect nlist.n_type nlist.n_desc
    ) symlist;
  printf "\n"

(* build nlist, symtabs, symlist *)

let rec get_nlist_it binary offset nsyms acc = 
  if (nsyms <= 0) then
    List.rev acc
  else
    let strx = u32 binary offset in
    let n_type = u8 binary (offset + 4) in
    let n_sect = u8 binary (offset + 5) in
    let n_desc = u16 binary (offset + 6) in
    let n_value = u64 binary (offset + 8) in
    let nlist = {strx; n_type; n_sect; n_desc; n_value;} in
    get_nlist_it binary (offset + sizeof_nlist) (nsyms - 1) (nlist::acc)

type t = (nlist * string) list

let rec get_symlist_it binary strsize nlists acc = 
  match nlists with
    [] -> List.rev acc
  | nlist::nlists ->
    let index = Bytes.index_from binary nlist.strx '\000' in
    let symname = Bytes.sub_string binary nlist.strx (index - nlist.strx) in
    get_symlist_it binary strsize nlists ((nlist,symname)::acc)

let get_symlist binary (cmd, cmdsize, symtab_command) nlists = 
  match symtab_command with
    SYMTAB symtab ->
    let str_bytes = Bytes.sub binary symtab.stroff symtab.strsize in
    get_symlist_it str_bytes symtab.strsize nlists []
  | _ -> []

(* Move this out to remove goblin deps *)
let nlist_flag_to_symbol_kind = 
  function
  | 0xe -> Goblin.Symbol.Local
  | 0xf -> Goblin.Symbol.Export
  | 0x1 -> Goblin.Symbol.Import
  | _ -> Goblin.Symbol.Other

let nlist_to_symbol_data (nlist, symbol) =
  let kind = `Kind (nlist_flag_to_symbol_kind @@ (nlist.n_type)) in
  let name = `Name (symbol) in
  let offset = `Offset (nlist.n_value) in
  [name; offset; kind]

let get_symbols binary ((_,_, symtab_command) as cmd) =
  match symtab_command with
  | SYMTAB symtab ->
    let nlist_bytes = Bytes.sub binary symtab.symoff (sizeof_nlist * symtab.nsyms) in
    let nlists = get_nlist_it nlist_bytes 0 symtab.nsyms [] in
    let symbols = get_symlist binary cmd nlists in
    (* remove this dep here *)
    let goblin_symbols = List.map (nlist_to_symbol_data) symbols in
    goblin_symbols
  | _ -> []

let filter_by_kind kind = List.filter (fun symbol -> try Goblin.Symbol.find_symbol_kind symbol = kind with Not_found -> false)

let print_symbols symbols =
  List.iter (Goblin.Symbol.print_symbol_data ~like_nlist:true) symbols
