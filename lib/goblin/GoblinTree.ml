(* Trie of Life is in goblin because it's cross platform *)
(* Map from symbolname -> Goblin.Symbol *)

module SymbolMap = Map.Make(String)

type branch = {
    library: string;
    export: GoblinExport.t
  }

type t = branch list SymbolMap.t

let num_symbols map = SymbolMap.cardinal map

let find = SymbolMap.find

let add name export map  =
  SymbolMap.add name export map

let empty:t = SymbolMap.empty

let is_empty = SymbolMap.is_empty

let singleton: 't SymbolMap.t ref = ref empty

let get_libraries symbol map =
  try
    let branches = SymbolMap.find symbol map in
    Generics.list_with_stringer
      ~newline:true ~omit_singleton_braces:true
      (fun branch -> branch.library)
      branches
  with Not_found ->
       "Unknown"

let print_map map =
  SymbolMap.iter (
      fun name branches ->
      Printf.printf "%s -> %s\n" name
      @@ Generics.list_with_stringer
	    (fun branch ->
             branch.library)
	    branches) map

exception Not_built

let get () =
  let f = RdrStorage.get_path "gtree" in
  if (Sys.file_exists f) then
    if (is_empty !singleton) then
      begin
	let ic = open_in_bin f in
	let map = Marshal.from_channel ic in
	close_in ic;
	singleton := map;
	map
      end
    else
      !singleton
  else
    raise Not_built
