(* 
  TODO: 
(-2): Add meta data to marshalled symbolmap/tol
(-1): /usr/lib/libmcheck.a with magic: 0x7f454c46 --- why does the .a file have an elf magic number?
(0) : Fix the suffix problem; doesn't work very well with linux, and osx frameworks won't get recognized either...
(1) : add this offset/polymorphic sorting, etc., to macho binary analysis to infer the size of objects per library...!
 *)

(*
for testing

#directory "../../_build/src/utils/";;
#directory "../../_build/src/mach/";;
#directory "../../_build/src/goblin/";;
#directory "../../_build/src/elf/";;

#load "Str.cma";;
#load "Unix.cma";;
#load "InputUtils.cmo";;
#load "Binary.cmo";;
#load "Leb128.cmo";;
#load "Generics.cmo";;
#load "Config.cmo";;
#load "Command.cmo";;
#load "Storage.cmo";;

#load "Goblin.Symbol.cmo";;
#load "Goblin.cmo";;

#load "Mach.Exports.cmo";;
#load "Nlist.cmo";;
#load "CpuTypes.cmo";;
#load "BindOpcodes.cmo";;
#load "Mach.Imports.cmo";;
#load "MachHeader.cmo";;
#load "Version.cmo";;
#load "LoadCommand.cmo";;
#load "Fat.cmo";;
#load "ReadMach.cmo";;

#load "ToL.cmo";;

#load "ReadElf.cmo";;
#load "ElfReloc.cmo";;
#load "ElfHeader.cmo";;
#load "ElfConstants.cmo";;
#load "SymbolTable.cmo";;
#load "Dynamic.cmo";;
#load "SectionHeader.cmo";;
#load "ProgramHeader.cmo";;

#load "LibRdr.Object.cmo";;
#load "Graph.cmo";;
*)
open Unix
open Config

(* eventually put .dll in there i guess *)
(* also implement .a i guess : fucking static libs
   --- ios will have a lot of those, burned into the binary, fun fun *)
let libraryish_suffixes = [".dylib"; ".so";]
(* 
TODO: banned suffixes, remove after solving problem with:
/usr/lib/libmcheck.a with magic: 0x7f454c46
 *)
let banned_suffixes = [".a"]			    

let has_libraryish_suffix string =
  List.fold_left (fun acc suffix ->
		  acc || (Filename.check_suffix string suffix)) false libraryish_suffixes

let has_banned_suffix string =
  List.fold_left (fun acc suffix ->
		  acc || (Filename.check_suffix string suffix)) false banned_suffixes

(* osx specific framework reading; needs some (a lot) of tuning *)
let read_frameworks ~verbose dir stack =
  let dir_fd = Unix.opendir dir in
  let count = ref 0 in
  try
    while true do
      let f = dir ^ (Unix.readdir dir_fd) in
      let f_stat = Unix.lstat f in (* sees symbolic links *)
      match f_stat.st_kind with
      | Unix.S_DIR ->
	 if (Filename.check_suffix f "framework") then
	   let framework = Filename.chop_suffix (Filename.basename f) ".framework" in
	   Stack.push (f ^ Filename.dir_sep ^ framework) stack
	 else
	   ();
      | _ -> ();
    done
  with
  | End_of_file ->
     Unix.closedir dir_fd;
     if (verbose) then Printf.printf "Pushed %d libs from %s\n" !count dir
  | Unix_error (error,s1,s2) ->
     Unix.closedir dir_fd;
     if (verbose) then Printf.printf "Error: %s %s %s\n" (error_message error) s1 s2

let rec read_framework_dirs ~verbose:verbose ~frameworks:frameworks stack =
  match frameworks with
  | [] -> stack
  | dir::dirs ->
     let dir = if (Filename.check_suffix dir Filename.dir_sep) then
		 dir
	       else
		 dir ^ Filename.dir_sep
     in
     read_frameworks ~verbose:verbose dir stack;
     read_framework_dirs ~verbose:verbose ~frameworks:dirs stack

(* rename this to object stack, wtf *)
let build_lib_stack recursive verbose dirs =
  let stack = Stack.create() in
  let rec dir_loop dirs stack =
    match dirs with
    | [] -> stack
    | dir::dirs ->
       (* add the / if missing, as a convenience to the user *)
       let dir = if (Filename.check_suffix dir Filename.dir_sep) then
		   dir
		 else
		   dir ^ Filename.dir_sep
       in
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
	   let base = Filename.basename f in
	   (*            if (has_libraryish_suffix base) then *)
	   if (not (has_banned_suffix base)) then
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

let output_stats tbl =
  let oc = open_out (Storage.get_path "stats") in
  output_string oc "symbol,count\n";
  Hashtbl.iter (fun a b ->
		let string = Printf.sprintf "%s,%d\n" a b in
		output_string oc string;
	       ) tbl;
  close_out oc

let build_polymorphic_map config =
  let dirs = config.base_symbol_map_directories in
  let framework_dirs = config.framework_directories in
  let recursive = config.recursive in
  let verbose = config.verbose in
  let tbl = Hashtbl.create ((List.length dirs) * 100) in
  if (verbose) then Printf.printf "Building map...\n";
  let libstack =
      if (Command.is_osx ()) then
	build_lib_stack recursive verbose dirs
	|> read_framework_dirs ~verbose:config.verbose ~frameworks:framework_dirs
      else
	build_lib_stack recursive verbose dirs
  in
  if (verbose) then 
    begin
      Printf.printf "Total libs: %d\n" (Stack.length libstack);
      Printf.printf "... Done\n";
    end;
  let rec loop map lib_deps = 
    if (Stack.is_empty libstack) then 
      begin
	output_stats tbl;
        Graph.graph_lib_dependencies ~use_dot_storage:true lib_deps;
	if (config.graph) then
	  Graph.graph_library_dependencies ~use_sfdp:(Command.is_linux()) ~use_dot_storage:true;
        map
      end
    else
      let lib = Stack.pop libstack in
      let bytes = try LibRdr.Object.get ~verbose:verbose lib with _ -> LibRdr.Object.Unknown (lib, "Exception reading binary") in
      let name = Filename.basename lib in
      let install_name = lib in
      let config = {config with silent=true; verbose=false; name; install_name; filename=lib} in
      match bytes with
      (* could do a |> ReadMach.to_goblin here ? --- better yet, to goblin, then map building code after to avoid DRY violations *)
      | LibRdr.Object.Mach binary ->
         let binary = ReadMach.analyze config binary in
	 let imports = binary.Goblin.imports in
	 Array.iter
	   (fun import ->
       (* 	    let symbol = Goblin.Mach.Imports.import_name import in *)
	    let symbol = import.Goblin.Import.name in
	    if (Hashtbl.mem tbl symbol) then
	      let count = Hashtbl.find tbl symbol in
	      Hashtbl.replace tbl symbol (count + 1)
	    else
	      Hashtbl.add tbl symbol 1
	   ) imports;
         (* let symbols = Mach.Exports.export_map_to_mach_export_data_list binary.ReadMach.exports in *)
         let symbols = binary.Goblin.exports in
         (* now we fold over the export -> polymorphic variant list of [mach_export_data] mappings returned from above *)
         let map' =
	   Array.fold_left
	     (fun acc data ->
	      let data = Goblin.Symbol.from_goblin_export
			   data ~libname:binary.Goblin.name ~libinstall_name:binary.Goblin.install_name in
	      (* this is bad, not checking for weird state of no export symbol name, but since i construct the data it isn't possible... right? *)
	      let symbol = Goblin.Symbol.find_symbol_name data in
	      try 
		(* if the symbol has a library-data mapping, then add the new lib-export data object to the export data list *)
		let data' = ToL.find symbol acc in
		ToL.add symbol (data::data') acc
	      with
	      | Not_found ->
		 (* we don't have a record of this export symbol mapping;
                    create a new singleton list with the data
                    (we already know the `Lib because MachExport's job is to add that) *)
		 ToL.add symbol [data] acc
	     ) map symbols
	 in
         loop map' ((binary.Goblin.name, binary.Goblin.libs)::lib_deps)
      | LibRdr.Object.Elf binary ->
         (* hurr durr iman elf *)
         let binary = ReadElf.analyze config binary in
	 let imports = binary.Goblin.imports in
	 Array.iter
	   (fun import ->
	    let symbol = import.Goblin.Import.name in
	    if (Hashtbl.mem tbl symbol) then
	      let count = Hashtbl.find tbl symbol in
	      Hashtbl.replace tbl symbol (count + 1)
	    else
	      Hashtbl.add tbl symbol 1
	   ) imports;
         let symbols = binary.Goblin.exports in
         let map' =
	   Array.fold_left
	     (fun acc data -> 
	      let symbol = data.Goblin.Export.name in
	      let data = Goblin.Symbol.from_goblin_export
			   data ~libname:binary.Goblin.name ~libinstall_name:binary.Goblin.install_name
	      in (* yea, i know, whatever; it keeps the cross-platform polymorphism, aieght *)
	      try 
		(* if the symbol has a library-data mapping, then add the new lib-export data object to the export data list *)
		let data' = ToL.find symbol acc in
		ToL.add symbol (data::data') acc
	      with
	      | Not_found ->
		 (* we don't have a record of this export symbol mapping; create a new singleton list with the data (we already know the `Lib because MachExport's job is to add that) except this is elf, so we also have to "promise" we did that there. Starting to become untenable and not very maintainable code *)
		 ToL.add symbol [data] acc
	     ) map symbols in
         loop map' ((binary.Goblin.name, binary.Goblin.libs)::lib_deps)
      | _ ->
         loop map lib_deps
  in loop ToL.empty []

(* flattens symbol -> [libs] map to [goblin symbols] *)
let flatten_polymorphic_map_to_list map =  
  ToL.SystemSymbolMap.fold
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
       Buffer.add_string b
       (* need to not use mach export to string? *)
       @@ Goblin.Mach.Exports.mach_export_data_to_string
	    ~use_flags:false export;
       Buffer.add_string b "\n";
       loop exports
  in loop list

let use_symbol_map config =
  let symbol = config.search_term in
  try
    let map = ToL.get () in
    (* =================== *)
    (* SEARCHING *)
    (* =================== *)
    if (config.search) then
      (* rdr -m -f <symbol_name> *)
      begin
        let recursive_message = if (config.recursive) then 
				  " (recursively)" 
				else ""
        in
        Printf.printf "searching %s%s for %s:\n"
		      (Generics.list_to_string 
			 ~omit_singleton_braces:true 
			 config.base_symbol_map_directories)
		      recursive_message
		      symbol; flush Pervasives.stdout;
        try
          ToL.find_symbol symbol map
          |> List.iter 
	       (fun data ->
		Goblin.Symbol.print_symbol_data ~with_lib:true data;
		if (config.disassemble) then
		  begin

		    let lib = Goblin.Symbol.find_symbol_lib data in
		    let startsym = Goblin.Symbol.find_symbol_offset data in
		    let size = Goblin.Symbol.find_symbol_size data in (* this may not be correct size... but i do it for the lulz *)
		    let ic = open_in_bin (snd lib) in
		    seek_in ic startsym;
		    let code = really_input_string ic size |> Binary.to_hex_string in
		    close_in ic;
		    flush Pervasives.stdout;
		    Printf.printf "\t\n";
		    (* NOW FOR THE HACKS *)
 		    if (Command.program_in_path "llvm-mc") then
		      Sys.command
		      @@ Printf.sprintf
			"echo \"%s\" | llvm-mc --disassemble" code
		      |> ignore
		    else
		      Printf.eprintf "Error: llvm-mc not installed or not in ${PATH}\n";
		  end
	       );
        with Not_found ->
	  Printf.printf "not found\n"; ()
      end
    else
      if (config.graph) then
	Graph.graph_library_dependencies ~use_sfdp:(Command.is_linux()) ~use_dot_storage:false
      else
      (* rdr -m -w *)
      let export_list = flatten_polymorphic_map_to_list map
                        |> Goblin.Symbol.sort_symbols ~compare_libs:true
      in
      let export_list_string = polymorphic_list_to_string export_list in
      if (config.write_symbols) then
        begin
          let f = Storage.get_path "symbols" in (* write to our .rdr *)
          let oc = open_out f in
          Printf.fprintf oc "%s" export_list_string;
          close_out oc;
        end
      else
	(* TODO: print stats here instead of dumping the whole shitshow, require verbose to do that *)
	(* rdr -m*)
        if (config.verbose) then
	  Printf.printf "%s\n" export_list_string
  with ToL.Not_built ->
    Printf.eprintf "Searching without a marshalled system map is very slow (on older systems) and a waste of energy; run `rdr -b` first (it will build a marshalled system map, $HOME/.rdr/tol, for fast lookups), then search with `rdr -m -f <symbol_name>`... Have a nice day!\n";
    flush Pervasives.stdout;
    exit 1

(* =================== *)
(* BUILDING *)
(* =================== *)
(* rdr -b *)
let build_symbol_map config =
  Printf.printf "Building system map... This can take a while, please be patient... "; flush Pervasives.stdout;
  let map = build_polymorphic_map config in
  let f = Storage.get_path "tol" in
  let oc = open_out_bin f in
  Marshal.to_channel oc map [];
  close_out oc;
  Printf.printf "Done!\n"


(* unit testing *)
(*  
let findf libs elem = 
  let lib = Goblin.Symbol.find_symbol_lib elem |> fst in
  List.mem lib libs

let sort = Goblin.Symbol.sort_symbols

let map = ToL.get ()
let flat = flatten_polymorphic_map_to_list map
let sflat = Goblin.Symbol.sort_symbols flat
let small = List.find_all  (findf ["/usr/lib/libz.1.dylib"; "/usr/lib/libobjc.A.dylib"]) flat
*)

