(* Trie of Life is in goblin because it's cross platform *)
(* Map from symbolname -> GoblinSymbol *)

module SystemSymbolMap = Map.Make(String)

type t = [ `Kind of GoblinSymbol.symbol_kind
           | `Lib of string * string
           | `Name of string
           | `Offset of int
	   | `PrintableData of string
           | `Size of int
           | `Reexport of [ `As of string * string | `From of string ]
           | `Size of int
           | `Stub of int * int
	   | `Flags of int
	 ]
	   list list SystemSymbolMap.t

(* type 'goblin t =
  ([> `Kind of GoblinSymbol.symbol_kind
           | `Lib of string
           | `Name of string
           | `Offset of int
	   (* 	   | `PrintableData of string *)
           | `Size of int ] as 'goblin)
	   list list SystemSymbolMap.t
 *)
(* Map Wrapper functions *)
let num_symbols (map) = SystemSymbolMap.cardinal map

let find = SystemSymbolMap.find

(* let add symbol (data:'goblin list list) (map:'goblin t)  = *)
let add symbol (data) (map:t)  =
  SystemSymbolMap.add symbol data map

let empty:t = SystemSymbolMap.empty

let is_empty = SystemSymbolMap.is_empty

(* let singleton:'goblin t = ref empty *)
let singleton: 't SystemSymbolMap.t ref = ref empty

let find_symbol key (map) = SystemSymbolMap.find key map

(* bin_libs is for elf, to basically do an intersection to reduce size *)
let get_libraries ?bin_libs:(bin_libs=[]) symbol map =
  try
    let symbols = SystemSymbolMap.find symbol map in
    Generics.list_with_stringer
      ~newline:true ~omit_singleton_braces:true
      (fun symbol ->
       let libname,libinstall_name = GoblinSymbol.find_symbol_lib symbol in
       libname (* TODO: check if correct *)
      )
      symbols
  with Not_found ->
       "Unknown"

let print_map map =
  SystemSymbolMap.iter (
      fun key values ->
      Printf.printf "%s -> %s\n" key
      @@ (Generics.list_with_stringer
	    (fun export ->
	     GoblinSymbol.find_symbol_lib export |> fst)
	    (* TODO: check if correct *)
	    values)) map

exception Not_built

let get () =
  let f = Storage.get_path "tol" in
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
