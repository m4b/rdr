open Goblin
open Goblin.Import
open RdrUtils.Printer

(* TODO:
 * add stats
 * add graphing ?
*)

(* 
with coverage

real	0m0.150s
user	0m0.103s
sys	0m0.040s

no coverage
real	0m0.148s
user	0m0.099s
sys	0m0.044s
*)
let debug = false

type transitive_symbol = {
  name: string;
  providers: string list;
}

let pp_transitive_symbol ppf ts =
  Format.fprintf ppf "@[%s => "
    ts.name;
  pp_seq ~brackets:true ppf pp_string ts.providers;
  Format.close_box()

module S = Set.Make(struct
    type t = string
    let compare ts1 ts2 = compare ts1 ts2
  end)

module M = Map.Make(String)

let pp_set ppf (set:S.t) =
  S.iter (fun elem -> Format.fprintf ppf "@ "; pp_string ppf elem) set
(*   S.iter (pp_transitive_symbol ppf) set *)

let add filter (map, set) (imports:Goblin.Import.t array) =
  Array.fold_left (fun (set,map) import ->
      if (List.mem import.lib filter) then
        let name = import.name in
        let set = S.add name set in
        let map =
          if (M.mem name map) then
            let providers = M.find name map in
            M.add name (import.lib::providers) map
          else
            map
        in
        set,map
      else
        set,map
    ) (set,map) imports

let add_s set filter (imports:Goblin.Import.t array) =
  Array.fold_left (fun set import ->
      if (List.mem import.lib filter) then
        let name = import.name in
        let set = S.add name set in
        set
      else
        set
    ) set imports

let lib_fallback_paths =
  if (RdrCommand.is_osx()) then
    ["/usr/local/lib/"; "/usr/lib/"; "/lib/";]
  else if (RdrCommand.is_linux()) then
    ["/usr/local/lib/"; "/usr/lib/";]
  else
    (* ... windows ? *)
    ["/usr/lib/";]

let rec close_libraries set visited filter libs =
  let ppf = Format.std_formatter in
  if (debug) then Format.fprintf Format.std_formatter "@[<v 2>@ ";
  let solution = Array.fold_left (fun (set,visited) lib ->
      if (debug) then begin
        ignore @@ read_line() end;
        let goblin,visited =
          if (Sys.file_exists lib) then
            if (not @@ S.mem lib visited) then
              let visited = S.add lib visited in
              if (debug) then
                begin
                  Format.fprintf ppf "@ NOT Visited %s@ NEW Visited Set:@[<v 2>@ " lib;
                  pp_set Format.std_formatter visited;
                  Format.fprintf ppf "@]"
                end;
              Some (RdrObject.try_goblin ~coverage:false lib),visited
            else
              begin
                if (debug) then Format.fprintf ppf "@ Visited %s" lib;
                None,visited
              end
          else
            let name = Filename.basename lib in
            let fallback_libs = List.map ( fun x -> x ^ name) lib_fallback_paths in
            let fallbacks_visited = 
              List.fold_left 
                (fun acc lib -> (S.mem lib visited) || acc) false fallback_libs
            in
            if (debug) then
              begin
                Format.fprintf Format.std_formatter "@ NOT FOUND %s@ Searching:@ " name;
                Format.open_vbox 2;
                Format.print_space();
                pp_slist Format.std_formatter fallback_libs;
                Format.print_space();
                Format.close_box() 
              end;
            if (not fallbacks_visited) then
              fallback_search visited fallback_libs
            else
              None,visited
        in
        match goblin with
        | Some goblin ->
          let set = add_s set filter goblin.imports in
          if (debug) then begin
            Format.fprintf Format.std_formatter "@ Scanning %s's libaries:@ " (Filename.basename lib);
            Format.open_vbox 2;
            Format.print_space();
            pp_slist Format.std_formatter (Array.to_list goblin.libs); Format.print_space();
            Format.close_box()
          end;
          if (goblin.Goblin.libs = [||]) then
            set,visited
          else
            close_libraries set visited filter goblin.libs
        | None ->
          (* TODO: refactor so actually unfound libraries are warned against, this is a serious issue if so *)
          (*           Format.eprintf "@ <Rdr.TransitiveClosure.close_libraries> lib %s not found@." lib; flush stdout; *)
          set,visited
    ) (set,visited) libs
  in
  if (debug) then Format.fprintf Format.std_formatter "@]";
  solution
and fallback_search visited libs =
  match libs with
  | [] -> None,visited
  | lib::libs ->
    try
      if (not @@ S.mem lib visited) then
        Some (RdrObject.try_goblin ~coverage:false lib),(S.add lib visited)
      else
        None,visited
    with _ ->
      fallback_search visited libs

(* solution *)
type t = {
  symbols: string list;
  transitive_library_dependencies: string list;
  filter: string list;
}

let pp ppf t =
  Format.fprintf ppf "@[@[<v 2>Transitive Closure filter@ ";
  pp_slist ppf t.filter;
  Format.fprintf ppf "@]";
  Format.fprintf ppf "@[<v 2>Symbols@ ";
  RdrUtils.Printer.pp_slist ppf t.symbols;
  Format.fprintf ppf "@]";
  Format.fprintf ppf "@[<v 2>Transitive Library Dependencies@ ";
  RdrUtils.Printer.pp_slist ppf t.transitive_library_dependencies;
  Format.fprintf ppf "@]@]@."

let print t =
  pp Format.std_formatter t

(* TODO: do not need to scan the goblin's libraries, just the set of libraries created from every import's exporting library *)
let compute filename filter =
  let goblin = RdrObject.try_goblin ~coverage:false filename in
  let set = Array.fold_left (fun acc import ->
      if (List.mem import.lib filter) then
        let name = import.name in
        (*         let providers = [import.lib] in *)
        S.add name acc
        (*         S.add {name; providers} acc *)
      else
        acc
    ) S.empty goblin.imports
  in
  let symbols,transitive_library_dependencies =
    close_libraries set S.empty filter goblin.libs
  in
  {symbols = S.elements symbols;
   transitive_library_dependencies = S.elements transitive_library_dependencies;
   filter;
  }
