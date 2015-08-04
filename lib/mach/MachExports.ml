(* TODO:

   (1) Weak of regular_symbol_info type probably needs to be added ?
   (3) /usr/lib/libstdc++.6.0.9.dylib has flag 0xc at many offsets... they're weak 
*)

open Binary
open MachLoadCommand

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

type symbol_kind =
  | REGULAR
  | ABSOLUTE
  | THREAD_LOCAL
  | UNKNOWN_SYMBOL_KIND of int

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
type reexport_symbol_info =
  {lib: string; lib_symbol_name: string option; flags: int}
type regular_symbol_info = {address: int; flags: int}

type export_info = 
  | Regular of regular_symbol_info
  | Reexport of reexport_symbol_info
  | Stub of stub_symbol_info

type export =
  {
    info: export_info;
    name: string;
    size: int;
    offset: int;
  }

type t = export list

exception Unimplemented_symbol_flag of int * string

let export_info_to_string =
  function
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
    Printf.sprintf "(0x%x 0x%x) STUB"
      info.stub_offset info.resolver_offset

let export_to_string export =
  Printf.sprintf "%s (0x%x) { %s }\n" 
    export.name
    export.size
    (export_info_to_string export.info)

let exports_to_string exports = 
  List.fold_left (fun acc import -> 
      (Printf.sprintf "%s" @@ export_to_string import) ^ acc
    ) "" exports

let print_export export =
  Printf.printf "%s\n" @@ export_to_string export

let print exports =
  Printf.printf "Exports (%d):\n" @@ List.length exports;
  List.iter print_export exports

let length exports = List.length exports

let compute_size exports:t =
  let rec loop acc exports =
    match exports with
    | [] -> acc
    | e1::[] ->
      (e1::acc) |> List.rev
    | e1::(e2::_ as rest) ->
      let size =
        if (e1.offset = 0x0 || e2.offset = 0x0) then
          0x0
        else
          e2.offset - e1.offset
      in
      loop ({e1 with size}::acc) rest
  in
  loop [] exports

let empty = []

let sort = List.sort (fun e1 e2 ->
    match e1.info with
    | Regular symbol1 ->
      begin
        match e2.info with
        | Regular symbol2 ->
          compare symbol1.address symbol2.address
        | _ ->
          1
      end
    | Reexport symbol1 ->
      begin
        match e2.info with
        | Reexport symbol2 ->
          compare symbol1.lib symbol2.lib
        | _ ->
          -1
      end
    | Stub symbol1 ->
      begin
        match e2.info with
        | Stub symbol2 ->
          compare symbol1.stub_offset symbol2.stub_offset
        | _ ->
          -1
      end
  )

(* ======================= *)
(*  BINARY work *)
(* ======================= *)

let get_export bytes libs flags offset =
  match flags land kEXPORT_SYMBOL_FLAGS_KIND_MASK |> get_symbol_kind
  with
  | REGULAR -> 
    if (flags land kEXPORT_SYMBOL_FLAGS_REEXPORT <> 0) then (* 0x8 *)
      let lib_ordinal, offset = Leb128.get_uleb128 bytes offset in
      let lib_symbol_name' = Binary.string bytes offset in
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
      let lib_symbol_name' = Binary.string bytes offset in
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
        let info = get_export bytes libs flags pos in
        let offset = match info with | Regular symbol -> symbol.address | _ -> 0x0 in
        let export = {info; name = current_symbol; offset; size = 0} in
        if (debug) then begin Printf.printf "\t"; print_export export end;
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

(* entry point for doing the work *)
let get_exports binary dyld_info libs :t =
  let boundary = (dyld_info.MachLoadCommand.Types.export_size + dyld_info.MachLoadCommand.Types.export_off) in
  let base = dyld_info.MachLoadCommand.Types.export_off in
  if (debug) then Printf.printf "export init: 0x%x 0x%x\n" base boundary;
  get_exports_it binary base boundary libs "" base [] |> sort |> compute_size

(* ======================== *)

(* test bytes (3 exported symbols) from compiled max *)
(*  
let unit1 = List.map (fun x -> Char.chr x) [0x00; 0x01; 0x5f; 0x00; 0x05; 0x00; 0x02; 0x5f; 0x6d; 0x68; 0x5f; 0x65; 0x78; 0x65; 0x63; 0x75; 0x74; 0x65; 0x5f; 0x68; 0x65; 0x61; 0x64; 0x65; 0x72; 0x00; 0x1f; 0x6d; 0x61; 0x00; 0x23; 0x02; 0x00; 0x00; 0x00; 0x00; 0x02; 0x78; 0x69; 0x6d; 0x75; 0x6d; 0x00; 0x30; 0x69; 0x6e; 0x00; 0x35; 0x03; 0x00; 0xc0; 0x1e; 0x00; 0x03; 0x00; 0xd0; 0x1e; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;]

let e1  = Bytes.init (List.length unit1) (fun i -> List.nth unit1 i)

let test1 = get_exports_it e1 0 64 [|"/usr/lib/libderp.so"; "/usr/lib/libthuglife.so"|] "" 0 []
 *)
