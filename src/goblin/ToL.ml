(* Trie of Life is in goblin because it's cross platform *)
(* Map from symbolname -> GoblinSymbol *)

module SystemSymbolMap = Map.Make(String)

type t = SystemSymbolMap

(* Map Wrapper functions *)
let num_symbols = SystemSymbolMap.cardinal 

let empty = SystemSymbolMap.empty

let is_empty = SystemSymbolMap.is_empty

let find_symbol key (map) = SystemSymbolMap.find key map

let get_libraries key (map) =
  try
    let symbols = SystemSymbolMap.find key map in
    Generics.list_with_stringer ~omit_singleton_braces:true
	    (fun symbol ->
	     GoblinSymbol.find_symbol_lib symbol)
	    symbols
  with Not_found ->
       "Unknown"

let print_map map =
  SystemSymbolMap.iter (
      fun key values ->
      Printf.printf "%s -> %s\n" key
      @@ (Generics.list_with_stringer
	    (fun export ->
	     GoblinSymbol.find_symbol_lib export)
	    values)) map

exception Not_built

let get () =
  let f = Storage.get_path "tol" in
  if (Sys.file_exists f) then
    let ic = open_in_bin f in
    let map = Marshal.from_channel ic in
    close_in ic;
    map
  else
    raise Not_built
