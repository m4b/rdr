(* 
  TODO: 
   (1) add this offset/polymorphic sorting, etc., to macho binary analysis to infer the size of objects per library...!
 *)

(*
for testing

#directory "/Users/matthewbarney/projects/binreader/_build/src/utils/";;
#directory "/Users/matthewbarney/projects/binreader/_build/src/mach/";;
#directory "/Users/matthewbarney/projects/binreader/_build/src/goblin/";;
#load "Unix.cma";;
#load "Binary.cmo";;
#load "InputUtils.cmo";;
#load "Version.cmo";;
#load "LoadCommand.cmo";;
#load "BindOpcodes.cmo";;
#load "Goblin.cmo";;
#load "GoblinSymbol.cmo";;
#load "Leb128.cmo";;
#load "Imports.cmo";;
#load "MachExports.cmo";;
#load "Macho.cmo";;
#load "MachHeader.cmo";;
#load "CpuTypes.cmo";;
#load "Generics.cmo";;
#load "Graph.cmo";;
#load "Object.cmo";;
#load "Fat.cmo";;
*)
open Unix

module SystemSymbolMap = Map.Make(String)

(* eventually put .dll in there i guess *)
(* also implement .a i guess : fucking static libs
   --- ios will have a lot of those, burned into the binary, fun fun *)
let libraryish_suffixes = [".dylib"; ".so"; ".a"]

let has_libraryish_suffix string =
  List.fold_left (fun acc suffix ->
      acc || (Filename.check_suffix string suffix)) false libraryish_suffixes

(* rename this to object stack, wtf *)
let build_lib_stack recursive verbose dirs =
  let stack = Stack.create() in
  let rec dir_loop dirs stack =
    match dirs with
    | [] -> stack
    | dir::dirs ->
      (* add the / if missing, as a convenience to the user *)
      let dir = if (Filename.check_suffix dir Filename.dir_sep) then dir else dir ^ Filename.dir_sep in
      dir_loop dirs (read_dir dir stack)
  and read_dir dir stack =
    let dir_fd = Unix.opendir dir in
    let not_done = ref true in
    let count = ref 0 in
    while (!not_done) do
      try 
        let f = dir ^ (Unix.readdir dir_fd) in
        let f_stat = Unix.lstat f in (* lstat can see symbolic links *)
        match f_stat.st_kind with
        | Unix.S_REG ->
          if (has_libraryish_suffix @@ Filename.basename f) then
            begin
              if (verbose) then Printf.printf "using %s\n" f;
              incr count;
              Stack.push f stack
            end;
        | Unix.S_DIR ->
          if (recursive && not (Filename.check_suffix f ".")) then
            (* because read_dir returns the stack, maybe make this unit? *)
            ignore @@ read_dir (f ^ Filename.dir_sep) stack
          else
            ();
          (* ignore symbolic links and other file types *)
        | _ -> ();
      with 
      | End_of_file -> 
        not_done := false;
        Unix.closedir dir_fd;
        if (verbose) then Printf.printf "Pushed %d libs from %s\n" !count dir
      | Unix_error (error,s1,s2) ->
        (* probably surpress this error? *)
        if (verbose) then Printf.eprintf "Error: %s %s %s\n" (error_message error) s1 s2 
    done;
    stack
  in dir_loop dirs stack

let build_polymorphic_map ?recursive:(recursive=false) ?graph:(graph=false) ?verbose:(verbose=true) dirs =
  if (verbose) then Printf.printf "Building map...\n";
  let libstack = build_lib_stack recursive verbose dirs in
  if (verbose) then 
    begin
      Printf.printf "Total libs: %d\n" (Stack.length libstack);
      Printf.printf "... Done\n";
    end;
  let rec loop map lib_deps = 
    if (Stack.is_empty libstack) then 
      begin
        if (graph) then
          Graph.graph_lib_dependencies lib_deps;
        map
      end
    else
      let lib = Stack.pop libstack in
      let bytes = Object.get_bytes ~verbose:verbose lib in
      match bytes with
      | Object.Mach binary ->
        (* could do a |> Mach.to_goblin here ? *)
        let binary = Mach.analyze ~verbose:false binary lib in
        (* let symbols = MachExports.export_map_to_mach_export_data_list binary.Mach.exports in *)
        let symbols = binary.Mach.exports in
        (* now we fold over the export -> polymorphic variant list of [mach_export_data] mappings returned from above *)
        let map' = Array.fold_left (fun acc data -> 
            (* this is bad, not checking for weird state of no export symbol name,
               but since i construct the data it isn't possible... right? *)
            let symbol = GoblinSymbol.find_symbol_name data in
            try 
              (* if the symbol has a library-data mapping, then add the new lib-export data object to the export data list *)
              let data' = SystemSymbolMap.find symbol acc in
              SystemSymbolMap.add symbol (data::data') acc
            with
            | Not_found ->
              (* we don't have a record of this export symbol mapping; 
                 create a new singleton list with the data (we already know the `Lib because MachExport's job is to add that) *)
              SystemSymbolMap.add symbol [data] acc
          ) map symbols in
        loop map' ((binary.Mach.name, binary.Mach.libs)::lib_deps)
      | Object.Elf binary ->
        (* TODO: hurr durr iman elf *)
        loop map lib_deps
      | _ ->
        loop map lib_deps
  in loop SystemSymbolMap.empty []

(* flattens symbol -> [libs] map to [mach_export_data] *)
let flatten_polymorphic_map_to_list map =  
  SystemSymbolMap.fold
    (fun key values acc ->
       (* list.fold acc in different arg pos than map.fold arg wtf *)
       List.fold_left
         (fun acc data ->
            data::acc) acc values
    ) map []

let polymorphic_list_to_string list =
  let b = Buffer.create ((List.length list) * 15) in
  let rec loop =
    function
    | [] -> Buffer.contents b
    | export::exports ->
      Buffer.add_string b @@ MachExports.mach_export_data_to_string ~use_flags:false export;
      Buffer.add_string b "\n";
      loop exports
  in loop list

let num_symbols = SystemSymbolMap.cardinal 

(* these no longer work because of massive api changes, good job matt *)
(* 
let print_map map =
  SystemSymbolMap.iter (fun symbol libs -> Printf.printf "%s -> %s\n" symbol 
                         @@ Generics.string_tuple_list_to_string libs
                       ) map



let map_to_string map =
  let b = Buffer.create ((num_symbols map) * 15) in
  SystemSymbolMap.iter (fun symbol libs -> Printf.sprintf "%s -> %s\n" symbol 
                         @@ Generics.string_tuple_list_to_string libs |> Buffer.add_string b
                       ) map;
  Buffer.contents b
 *)

let find_symbol key (map) = SystemSymbolMap.find key map
