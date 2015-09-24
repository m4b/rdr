(* Trie of Life is in goblin because it's cross platform *)
(* Map from symbolname -> Goblin.Symbol *)

module SymbolMap = Map.Make(String)

open GoblinExport

type branch = {
    library: string;
    aliases: string list;
    export: GoblinExport.t
  }

let pp_branch ppf branch =
  RdrPrinter.pp_h
    Format.str_formatter
    ~brackets:true
    RdrPrinter.pp_string branch.aliases;
  let aliases = Format.flush_str_formatter() in
  Format.fprintf
    ppf "@[%a -> %s %s@]"
    GoblinExport.pp branch.export
    branch.library
    aliases

let show_branch branch =
  pp_branch Format.str_formatter branch; Format.flush_str_formatter()

let print_branch branch =
  pp_branch Format.std_formatter branch; Format.print_newline()

type t = branch list SymbolMap.t

let num_symbols map = SymbolMap.cardinal map

let fold = SymbolMap.fold

let find = SymbolMap.find

let add name export map  =
  SymbolMap.add name export map

let empty:t = SymbolMap.empty

let is_empty = SymbolMap.is_empty

let singleton: 't SymbolMap.t ref = ref empty

let flatten map =  
  fold
    (fun key values acc ->
     (* list.fold acc in different arg pos than map.fold arg wtf *)
     List.fold_left
       (fun acc data ->
        data::acc) acc values
    ) map []

let show_flat list =
  let b = Buffer.create ((List.length list) * 15) in
  let rec loop =
    function
    | [] -> Buffer.contents b
    | branch::branches ->
       Buffer.add_string b
       @@ show_branch branch;
       Buffer.add_string b "\n";
       loop branches
  in loop list

let sort_symbols
      ?compare_libs:(compare_libs=false)
      list =
  List.sort (fun a b ->
	 (* first compare libs, export must have a lib *)
	 (* yea... but not just for exports anymore, so either need to return empty lib or check for Not_found... *)
	 let l1 = a.library in
	 let l2 = b.library in
	 let n1 = a.export.name in
	 let n2 = b.export.name in
	 if (not compare_libs || l1 = l2) then
           try
             let o1 = a.export.offset in
             try
               let o2 = b.export.offset in
	       if (o1 = o2) then
		 Pervasives.compare n1 n2
	       else
		 Pervasives.compare o1 o2
             with Not_found ->
               1
           with Not_found ->
             -1
	 else
           Pervasives.compare l1 l2
	) list

let get_libraries symbol map =
  try
    let branches = SymbolMap.find symbol map in
    Generics.list_with_stringer
      ~newline:true ~omit_singleton_braces:true
      (fun branch -> branch.library)
      branches
  with Not_found ->
       "Unknown"

(* aliases are case sensitive - PE will be case insensitive *)
let resolve_import ~case_sensitive:sensitive (branches: branch list) libraries =
  let libraries = if (sensitive) then
      libraries else List.map String.lowercase libraries
  in
  let rec loop acc branches =
    match branches with
    | [] ->
      begin
        match acc with
        | [] -> "Unresolved"
        | library::[] -> library
        | library::libraries ->
          Printf.sprintf
            "Multiple Libraries Resolved\n%s"
          @@ Generics.list_to_string acc
      end
    | branch::branches ->
      let lib = if (sensitive) then
          branch.library else String.lowercase branch.library
      in
      if (List.mem lib libraries
          || List.exists
            (fun alias ->
               if (sensitive) then
                 List.mem alias libraries
               else
                 List.mem (String.lowercase alias) libraries
            )
            branch.aliases
         ) then
        loop (branch.library::acc) branches
      else
        loop acc branches
  in loop [] branches

let resolve_library ~case_sensitive ~name ~libraries ~tree =
  try
    let branches = find name tree in
    resolve_import ~case_sensitive branches libraries
  with Not_found -> "Unknown"

(* 
let resolve_ordinal ~ordinal ~lib ~tree =
  let bindings = SymbolMap.bindings in
  let rec loop bindings =
    match bindings with
    | [] -> None
    | (key, branches)::bindings ->
      (* if lib = some branches.library && ordinal = some branches' exports' ordinal then that branches' exports' name *)
 *)

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
