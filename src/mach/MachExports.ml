(* TODO:

   (* TOOO: make export data more generic, in a higher up module *)
   (1) Weak of regular_symbol_info type probably needs to be added ?
   (3) /usr/lib/libstdc++.6.0.9.dylib has flag 0xc at many offsets... they're weak 
*)

(*
for testing
#directory "/Users/matthewbarney/projects/binreader/_build/src/utils/";;
#directory "/Users/matthewbarney/projects/binreader/_build/src/mach/";;
#directory "/Users/matthewbarney/projects/binreader/_build/src/goblin/";;
#load "Binary.cmo";;
#load "InputUtils.cmo";;
#load "Version.cmo";;
#load "Nlist.cmo";;
#load "LoadCommand.cmo";;
#load "BindOpcodes.cmo";;
#load "GoblinSymbol.cmo";;
#load "Leb128.cmo";;
#load "Imports.cmo";;
#load "Exports.cmo";;
#load "Macho.cmo";;
*)
open Binary
open LoadCommand

(*
 * "The following are used on the flags byte of a terminal node
 * in the export information."
 *)
let kEXPORT_SYMBOL_FLAGS_KIND_MASK         = 0x03
let kEXPORT_SYMBOL_FLAGS_KIND_REGULAR      = 0x00
let kEXPORT_SYMBOL_FLAGS_KIND_ABSOLUTE     = 0x02 (* this is a symbol not present in the loader.h but only in the dyld compressed image loader source code, and only available with a #def macro for export flags but libobjc. def has this *)
let kEXPORT_SYMBOL_FLAGS_KIND_THREAD_LOCAL = 0x01
let kEXPORT_SYMBOL_FLAGS_WEAK_DEFINITION   = 0x04
let kEXPORT_SYMBOL_FLAGS_REEXPORT          = 0x08
let kEXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER = 0x10

type symbol_kind = | REGULAR | ABSOLUTE | THREAD_LOCAL | UNKNOWN_SYMBOL_KIND of int

exception Unknown_symbol_kind of string

let get_symbol_kind kind =
  match kind with
  | 0x00 -> REGULAR
  | 0x01 -> THREAD_LOCAL
  | 0x02 -> ABSOLUTE
  | _ -> UNKNOWN_SYMBOL_KIND kind

(* "If the flags is EXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER, then following the flags is two uleb128s: the stub offset and the resolver offset. The stub is used by non-lazy pointers.  The resolver is used by lazy pointers and must be called to get the actual address to use." *)
type stub_symbol_info = {stub_offset: int; resolver_offset: int; flags: int}

(* if lib_symbol_name None then same symbol name, otherwise reexport of lib_symbol_name with name in the trie *)
(* "If the string is zero length, then the symbol is re-export from the specified dylib with the same name" *)
type reexport_symbol_info = {lib: string; lib_symbol_name: string option; flags: int}
type regular_symbol_info = {address: int; flags: int}

type export_symbol_info = 
  | Regular of regular_symbol_info
  | Reexport of reexport_symbol_info
  | Stub of stub_symbol_info

module ExportMap = Map.Make(String)

 (* a datum is a single unit, 
    so data is a list of such possible datums *)
type mach_export_data = 
  [ 
    | GoblinSymbol.symbol_datum 
    (* we extend with mach specific details: *)
    | `Reexport of [`As of string * string | `From of string]
    | `Stub of int * int
    | `Flags of int
  ] list

type export_map = mach_export_data ExportMap.t

exception Unimplemented_symbol_flag of int * string

let export_info_to_mach_export_data name soname = 
  function
  | Regular symbol -> 
    [
      `Name name;
      `Offset symbol.address;
      `Kind GoblinSymbol.Export;
      `Flags symbol.flags;
      `Lib soname;
    ]
  | Reexport symbol -> 
    begin
      match symbol.lib_symbol_name with
      | Some name' ->
        [
          `Name name;
          `Reexport (`As (name',symbol.lib));
          `PrintableData (Printf.sprintf "REEX as %s from %s"  name' symbol.lib);
          `Kind GoblinSymbol.Export;
          `Flags symbol.flags;
          `Lib soname;
        ]
      | None ->
        [
          `Name name;
          `Reexport (`From symbol.lib);
          `PrintableData (Printf.sprintf "REEX from %s" symbol.lib);
          `Kind GoblinSymbol.Export;
          `Flags symbol.flags;
          `Lib soname;
        ]
    end
  | Stub symbol ->
    [
      `Name name;
      `Stub (symbol.stub_offset, symbol.resolver_offset);
      `PrintableData (Printf.sprintf "STUB 0x%x 0x%x" symbol.stub_offset symbol.resolver_offset);
      `Kind GoblinSymbol.Export;
      `Flags symbol.flags;
      `Lib soname;
    ]

let rec find_reexport =
  function
  | [] -> raise Not_found
  | (`Reexport _) as reex :: _->
    reex
  | _::rest -> find_reexport rest

let rec find_stub =
  function
  | [] -> raise Not_found
  | `Stub stub :: _ ->
    stub
  | _::rest -> find_stub rest

let rec find_flags =
  function
  | [] -> raise Not_found
  | `Flags flags :: _ ->
    flags
  | _::rest -> find_flags rest

 let mach_export_data_to_export_info data = 
  try 
    let reex = find_reexport data in
    begin
      match reex with
      | `Reexport (`As (name, lib)) ->
        let lib_symbol_name = Some name in
        let flags = find_flags data in
        Reexport {lib; lib_symbol_name; flags}
      | `Reexport (`From lib) -> 
        let lib_symbol_name = None in
        let flags = find_flags data in
        Reexport {lib; lib_symbol_name; flags}
    end
  with Not_found ->
    try
      let stub_offset,resolver_offset = find_stub data in
      let flags = find_flags data in
      Stub {stub_offset;resolver_offset; flags}
    with Not_found ->
      let address = GoblinSymbol.find_symbol_offset data in
      let flags = find_flags data in
      Regular {address; flags}
(*
lel ocam you so cray: 
List.nth poly1 1 |> snd |> function | `Offset i -> i | _ -> raise Not_found;;
 *)

let mach_export_datum_to_string ?use_kind:(use_kind=true) ?use_flags:(use_flags=false) ?use_lib:(use_lib=true) datum =
  match datum with
  | #GoblinSymbol.symbol_datum as datum -> GoblinSymbol.symbol_datum_to_string ~use_kind:use_kind ~use_lib:use_lib ~use_printable:false datum
  | `Reexport `As (name,lib) -> Printf.sprintf "REEX as %s from %s" name lib
  | `Reexport `From lib -> Printf.sprintf "REEX from %s" lib
  | `Stub (i1,i2) -> Printf.sprintf "STUB 0x%x 0x%x" i1 i2
  | `Flags flags -> if (use_flags) then Printf.sprintf "FLAGS 0x%x" flags else ""

let mach_export_datum_ordinal =
  function
  | #GoblinSymbol.symbol_datum as datum -> GoblinSymbol.symbol_datum_ordinal datum
  | `Reexport `As (_,_)         (* fall through *)
  | `Flags _
  | `Reexport `From _
  | `Stub _ -> GoblinSymbol.kORDINAL_RIGHT

let sort_mach_export_data (data) =
  List.sort (fun a b ->
      let e1 = mach_export_datum_ordinal a in
      let e2 = mach_export_datum_ordinal b in
      Pervasives.compare e1 e2
    ) data

let mach_export_data_to_string ?use_kind:(use_kind=true) ?use_flags:(use_flags=false) ?use_lib:(use_lib=true) (data:mach_export_data) =
  let data = sort_mach_export_data data in
  let b = Buffer.create ((List.length data) * 15) in
  List.iter (fun elem ->
      Buffer.add_string b @@ mach_export_datum_to_string ~use_kind:use_kind ~use_flags:use_flags ~use_lib:use_lib elem;
      Buffer.add_string b " "
    ) data;
  Buffer.contents b

(* lessening of the data set, ignores mach specific extensions *)
let mach_export_data_to_symbol_data list =
  List.filter
    (
      function
      (* this is sweet *)
      | #GoblinSymbol.symbol_datum as datum -> ignore datum; true (* ignore the warning, can't use _ :( we just need to see if it's a subset of goblin symbols, is all *)
      | _ -> false
    ) list

let export_info_to_string ei = 
  match ei with
  | Regular info ->
    Printf.sprintf "0x%x REGU" info.address 
  | Reexport info ->
    begin
      match info.lib_symbol_name with
      | None ->
        Printf.sprintf "from %s REEX" info.lib
      | Some original_symbol ->
        Printf.sprintf "from %s -> %s REEX" info.lib original_symbol
    end
  | Stub info ->
    Printf.sprintf "(0x%x 0x%x) STUB" info.stub_offset info.resolver_offset

let export_map_to_string map = 
  let b = Buffer.create ((ExportMap.cardinal map) * 15) in
  ExportMap.iter (fun key symbol ->
      (* TODO: change this to a different printer, this will be used disassembling single binaries, so more mach info, the better *)
      Buffer.add_string b @@ GoblinSymbol.symbol_data_to_string ~basic_export:true symbol;
    ) map;
  Buffer.contents b

let export_map_to_mach_export_data_list map =
  ExportMap.fold (fun key export acc -> export::acc) map []

let mach_export_data_list_to_export_map list =
  List.fold_left (fun acc export -> ExportMap.add (GoblinSymbol.find_symbol_name export) export acc) ExportMap.empty list

let print_exports exports = 
  Printf.printf "Exports (%d):\n" @@ List.length exports;
  List.iter 
  (fun symbol -> mach_export_data_to_string ~use_kind:false ~use_lib:false symbol |> Printf.printf "%s\n" ) exports
(*   ExportMap.iter (fun key symbol -> mach_export_data_to_string symbol |> Printf.printf "%s\n" ) map *)

let print_mach_export_data ?simple:(simple=false) ?goblin:(goblin=false) export = 
  if (not goblin) then
    mach_export_data_to_string ~use_lib:(not simple) export |> Printf.printf "%s\n"
  else
    GoblinSymbol.symbol_data_to_string ~basic_export:true export |> Printf.printf "%s\n"

let find_map export map = 
  ExportMap.find export map

let find symbol = 
  List.find (fun export -> (GoblinSymbol.find_symbol_name export) = symbol)

let length exports = List.length exports

let length_map = ExportMap.cardinal

let empty_map = ExportMap.empty

let empty = []

let fold = List.fold_left

let fold_map f (map:export_map) = ExportMap.fold f map

(* ======================= *)
(*  BINARY work *)
(* ======================= *)

let get_symbol_type bytes libs flags offset =
  match flags land kEXPORT_SYMBOL_FLAGS_KIND_MASK |> get_symbol_kind with
  | REGULAR -> 
    if (flags land kEXPORT_SYMBOL_FLAGS_REEXPORT <> 0) then (* 0x8 *)
      let lib_ordinal, offset = Leb128.get_uleb128 bytes offset in
      let lib_symbol_name' = Binary.istring bytes offset in
      let lib = libs.(lib_ordinal) in
      let lib_symbol_name = if lib_symbol_name' = "" then None else Some lib_symbol_name' in
      Reexport {lib; lib_symbol_name; flags}
    else if (flags land kEXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER <> 0) then (* 0x10 *)
      let stub_offset, offset = Leb128.get_uleb128 bytes offset in
      let resolver_offset, offset = Leb128.get_uleb128 bytes offset in
      Stub {stub_offset; resolver_offset; flags}
      (* else if (flags = kEXPORT_SYMBOL_FLAGS_WEAK_DEFINITION) then (*0x40 unused*) *)
    else
      let address, _ = Leb128.get_uleb128 bytes offset in
      Regular {address; flags}
  | THREAD_LOCAL | ABSOLUTE ->
    if (flags land kEXPORT_SYMBOL_FLAGS_REEXPORT <> 0) then (* 0x8 *)
      let lib_ordinal, offset = Leb128.get_uleb128 bytes offset in
      let lib = libs.(lib_ordinal) in
      let lib_symbol_name' = Binary.istring bytes offset in
      let lib_symbol_name = if lib_symbol_name' = "" then None else Some lib_symbol_name' in
      Reexport {lib; lib_symbol_name; flags}
    else
      let address, _ = Leb128.get_uleb128 bytes offset in
      Regular {address; flags}
  | UNKNOWN_SYMBOL_KIND kind ->
    Printf.printf "WARNING: unknown kind 0x%x from flags 0x%x in get_symbol_type at offset %d\n" kind flags offset;
    let address, _ = Leb128.get_uleb128 bytes offset in
    Regular {address; flags}


let interp = false
let debug = interp
(* current_symbol accumulates the symbol name until we hit a terminal, which we then add to the list as a key to the flags and location *)
(* this is the meat of the binary work, 
   reads out the trie encoded and builds the export list *)
let rec get_exports_it bytes base size libs current_symbol pos acc = 
  if (pos >= size) then
    (* shouldn't happen because we terminate based on the trie structure itself, not pos *)
    acc
  else
    begin
      (* reexport_with_name has size > 127, so otherwise fits into one byte, 
         but pulling out the uleb regardless should work just fine... *)
      let terminal_size,pos = Leb128.get_uleb128 bytes pos in
      if (interp) then ignore @@ read_line ();
      if (debug) then Printf.printf "@ 0x%x node: 0x%x current_symbol: %s\n" (pos-1) terminal_size current_symbol;
      if (terminal_size = 0) then (* node with no symbol info with num branches following *)
        let num_branches,pos = Leb128.get_uleb128 bytes pos in
        if (debug) then Printf.printf "\tBRAN %d\n" num_branches;
        let branches = get_branches bytes base num_branches current_symbol 0 pos [] in
        get_nodes bytes base size libs branches acc
      else (* terminal, but the tricky part, which cost me much sorrow, is that they can have children... *)
        (* pos + 1 = flags *)
        let num_children,children_start = Leb128.get_uleb128 bytes (pos + terminal_size) in (* skip past the symbol info to get the number of children *)
        let flags,pos = Leb128.get_uleb128 bytes pos in
        if (debug) then Printf.printf "\tTERM %d flags: 0x%x\n" num_children flags;
        let export = (get_symbol_type bytes libs flags pos |> (export_info_to_mach_export_data current_symbol libs.(0))) in
        if (debug) then begin Printf.printf "\t"; print_mach_export_data export end;
        let acc = export::acc in
        if (num_children = 0) then
          acc
        else
          (* without this line was losing like 15% of the exports, because missing terminals-with-children; 
             only accidentally verified when grepped the size of nlist's 0xf (exports) wc -l count 
             with this functions returned symbol count... *)
          let branches = get_branches bytes base num_children current_symbol 0 children_start [] in
          get_nodes bytes base size libs branches acc
    end
and get_nodes bytes base size libs branches acc =
  match branches with
    [] -> 
    begin
      acc (* terminating right here *)
    end
  | (string, next_node)::ns ->
    let acc' = get_exports_it bytes base size libs string next_node acc in
    (*     Printf.printf "string: %s pos: 0x%x size: 0x%x\n" string next_node size; *)
    get_nodes bytes base size libs ns acc'
and get_branches bytes base count current_symbol curr pos branches =
  if (curr >= count) then
    branches
  else
    let string,pos = Binary.stringo bytes pos in
    let key = current_symbol ^ string in
    let next_node,pos = Leb128.get_uleb128 bytes pos in
    if (debug) then Printf.printf "\t(%d) string: %s next_node: 0x%x\n" curr key (base + next_node);
    get_branches bytes base count current_symbol (curr+1) pos ((key, (base + next_node))::branches)

(* TODO: see todos above *)
(* entry point for doing the work *)
let get_exports binary dyld_info libs  = 
  let boundary = (dyld_info.LoadCommand.export_size + dyld_info.LoadCommand.export_off) in
  let base = dyld_info.LoadCommand.export_off in
  if (debug) then Printf.printf "export init: 0x%x 0x%x\n" base boundary;
  get_exports_it binary base boundary libs "" base [] |>
  GoblinSymbol.sort_symbols |> GoblinSymbol.compute_size 0x0 (* FIX THIS WITH EXTRA NLIST DATA *) |> List.rev (* for some reason compute size wasn't reversing... ? *)

(* ======================== *)

(* test bytes (3 exported symbols) from compiled max *)
(*  *)
let unit1 = List.map (fun x -> Char.chr x) [0x00; 0x01; 0x5f; 0x00; 0x05; 0x00; 0x02; 0x5f; 0x6d; 0x68; 0x5f; 0x65; 0x78; 0x65; 0x63; 0x75; 0x74; 0x65; 0x5f; 0x68; 0x65; 0x61; 0x64; 0x65; 0x72; 0x00; 0x1f; 0x6d; 0x61; 0x00; 0x23; 0x02; 0x00; 0x00; 0x00; 0x00; 0x02; 0x78; 0x69; 0x6d; 0x75; 0x6d; 0x00; 0x30; 0x69; 0x6e; 0x00; 0x35; 0x03; 0x00; 0xc0; 0x1e; 0x00; 0x03; 0x00; 0xd0; 0x1e; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;]

let e1  = Bytes.init (List.length unit1) (fun i -> List.nth unit1 i)

(* 
let test1 = get_exports_it e1 0 64 [|"/usr/lib/libderp.so"; "/usr/lib/libthuglife.so"|] "" 0 []
 *)
