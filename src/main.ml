(* TODO: 
   (0) add a symbol like --map-all to run `rdr -b -d /usr/lib /System /Libraries -r`
       which would essentially build a map of the entire system, or something like that
   (1) mach static offsets for functions like printf in the standard lib needs to be slid by the offset of the 64-bit binary inside the fat binary, otherwise all wrong.
 *)

let version = "3.0"

open Config (* because only has a record type *)
open Goblin.Tree
open Goblin.Export

type os = Darwin | Linux | Other

let get_os () = 
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  if (uname = "Darwin") then Darwin
  else if (uname = "Linux") then Linux
  else Other
	 
let use_map = ref false
let graph = ref false
let verbose = ref false
let print_headers = ref false
let print_libraries = ref false
let print_exports = ref false
let print_imports = ref false
let print_coverage = ref false
let print_sections = ref false
let print_goblin = ref false
let recursive = ref false
let write_symbols = ref false
let marshal_symbols = ref false
let print_nlist = ref false
let base_symbol_map_directories = ref ["/usr/lib/"]
let framework_directories = ref []
let anonarg = ref ""

let disassemble = ref false
let disassemble_offset = ref 0x0
let search_term_string = ref ""

let print_version = ref false

let get_config () =
  let analyze = not (!use_map || !marshal_symbols) in
  let search = !search_term_string <> "" in
  let silent =
    not analyze && not !verbose
  in (* so we respect verbosity if searching*)
  let use_tol =
    !print_imports || !verbose || (!graph && analyze)
  in
  let resolve_imports =
    !print_imports || !verbose || (!graph && analyze)
    || !print_goblin
  in
  let install_name =
    if (Filename.is_relative !anonarg) then
      (Sys.getcwd()) ^ Filename.dir_sep ^ !anonarg
    else
      !anonarg
  in
  let name = Filename.basename install_name in
  {
    Config.analyze;
    name;
    install_name;
    silent;
    search;
    use_tol;
    resolve_imports;
    consume_bytes = false;
    print_nlist = !print_nlist;
    verbose = !verbose;
    print_headers = !print_headers;      
    print_libraries = !print_libraries;
    print_exports = !print_exports;
    print_imports = !print_imports;
    print_coverage = !print_coverage;
    print_sections = !print_sections;
    disassemble = !disassemble;
    use_map = !use_map;
    recursive = !recursive;
    write_symbols = !write_symbols;
    marshal_symbols = !marshal_symbols;
    base_symbol_map_directories = !base_symbol_map_directories;
    framework_directories = !framework_directories;
    graph = !graph;
    filename = !anonarg; 	(* TODO: this should be Filename.basename, but unchanged for now *)
    search_term = !search_term_string;
    print_goblin = !print_goblin;
  } 
    
let set_base_symbol_map_directories dir_string =
  (* Printf.printf "%s\n" dir_string; *)
  let dirs = Str.split (Str.regexp "[ :]+") dir_string |> List.map String.trim in
  match dirs with
  | [] -> raise @@ Arg.Bad "Invalid argument: directories must be separated by spaces or :, -d /usr/local/lib, /some/other/path"
  | _ -> 
     (* Printf.printf "setting dirs: %s\n" @@ Generics.list_to_string dirs; *)
     base_symbol_map_directories := dirs

let set_framework_directories dir_string =
  (* Printf.printf "Framework directories: %s\n" dir_string; *)
  let dirs = Str.split (Str.regexp "[ :]+") dir_string |> List.map String.trim in
  match dirs with
  | [] -> ()
  | _ ->
     (* Printf.printf "setting framework dirs: %s\n" @@ Generics.list_to_string dirs; *)
     framework_directories := dirs

let set_anon_argument string =
  anonarg := string
	       
let analyze config binary =
  let goblin = match binary with
    | Rdr.Object.Mach bytes ->
       ReadMach.analyze config bytes
    | Rdr.Object.Elf bytes ->
       ReadElf.analyze config bytes
    | Rdr.Object.PE32 bytes ->
       ReadPE.analyze config bytes
    | Rdr.Object.Unknown (string, filename) ->
       failwith (Printf.sprintf "%s: %s" string filename)
  in
  if (config.print_goblin) then
    begin
      Goblin.print goblin
    end
  else
    if (config.search) then
    try
      let symbol =
        Goblin.get_export
          config.search_term goblin.Goblin.exports
      in
      Goblin.Export.print symbol;
      if (config.disassemble) then
        Rdr.Utils.Command.disassemble
          goblin.Goblin.install_name
          symbol.Goblin.Export.offset
          symbol.Goblin.Export.size
    with Not_found ->
      Printf.printf "";
  else 
    if (config.graph) then
      Graph.graph_goblin 
	~draw_imports:true
	~draw_libs:true goblin
      @@ Filename.basename config.filename    

let use_symbol_map config =
  let symbol = config.search_term in
  try
    let map = Goblin.Tree.get () in
    (* =================== *)
    (* SEARCHING *)
    (* =================== *)
    if (config.search) then
      (* rdr -m -f <symbol_name> *)
      begin
        Printf.printf "searching %s for %s:\n"
		      (Generics.list_to_string 
			 ~omit_singleton_braces:true 
			 config.base_symbol_map_directories)
		      symbol; flush Pervasives.stdout;
        try
          Goblin.Tree.find symbol map
          |> List.iter 
	       (fun branch ->
		Goblin.Tree.print_branch branch;
		if (config.disassemble) then
		  begin
		    let offset = branch.export.offset in
		    let size = branch.export.size in
                    Rdr.Utils.Command.disassemble
                      branch.library
                      offset
                      size
		  end
	       );
        with Not_found ->
	  Printf.printf "not found\n"; ()
      end
    else
      if (config.graph) then
	Graph.graph_library_dependencies
          ~use_sfdp:(Rdr.Utils.Command.is_linux())
          ~use_dot_storage:false
      else
        (* rdr -m -w *)
        let export_list =
          Goblin.Tree.flatten map
          |> Goblin.Tree.sort_symbols ~compare_libs:true
        in
        let export_list_string =
          Goblin.Tree.show_flat export_list in
        if (config.write_symbols) then
          begin
            let f = Rdr.Utils.Storage.get_path "symbols" in (* write to our .rdr *)
            let oc = open_out f in
            Printf.fprintf oc "%s" export_list_string;
            close_out oc;
          end
        else
	  (* TODO: print stats here instead of dumping the whole shitshow, require verbose to do that *)
	  (* rdr -m*)
          if (config.verbose) then
	    Printf.printf "%s\n" export_list_string
  with Goblin.Tree.Not_built ->
    Printf.eprintf "Searching without a marshalled system map is very slow (on older systems) and a waste of energy; run `rdr -b` first (it will build a marshalled system map, $HOME/.rdr/gtree, for fast lookups), then search with `rdr -m -f <symbol_name>`... Have a nice day!\n";
    flush Pervasives.stdout;
    exit 1

let build_symbol_map config =
  Printf.printf "Building system map... This can take a while, please be patient... "; flush Pervasives.stdout;
  let libs =
    let dirs = config.base_symbol_map_directories in
    let framework_dirs = config.framework_directories in
    let recursive = config.recursive in
    let verbose = config.verbose in
    if (Rdr.Utils.Command.is_osx ()) then
      Rdr.Map.build_lib_stack
        ~recursive:recursive
        ~verbose:verbose
        ~dirs:dirs
      |> Rdr.Map.read_framework_dirs
           ~verbose:verbose
           ~frameworks:framework_dirs
    else
      Rdr.Map.build_lib_stack
        ~recursive:recursive
        ~verbose:verbose
        ~dirs:dirs
  in
  let map = Rdr.Map.build
              ~verbose:config.verbose
              ~graph:config.graph
              ~libs:libs
  in
  let f = Rdr.Utils.Storage.get_path "gtree" in
  let oc = open_out_bin f in
  Marshal.to_channel oc map [];
  close_out oc;
  Printf.printf "Done!\n"

let main =
  let speclist =
    [("-m", Arg.Set use_map, "Use a pre-marshalled system symbol map; use this in conjunction with -f, -D, -g, or -w");
     ("-g", Arg.Set graph, "Creates a graphviz file; generates lib dependencies if -b given");
     ("-d", Arg.String (set_base_symbol_map_directories), "String of space separated directories to build symbol map from; default is /usr/lib");
     ("-F", Arg.String (set_framework_directories), "(OSX Only, Experimental) String of space or colon separated base framework directories to additionally search when building the symbol map");
     ("-r", Arg.Set recursive, "Recursively search directories for binaries; use with -b");
     ("-v", Arg.Set verbose, "Print all the things");
     ("--version", Arg.Set print_version, "Print the version and exit");
     ("-h", Arg.Set print_headers, "Print the header"); 
     ("-l", Arg.Set print_libraries, "Print the dynamic libraries");     
     ("-e", Arg.Set print_exports, "Print the exported symbols");
     ("-i", Arg.Set print_imports, "Print the imported symbols");
     ("-c", Arg.Set print_coverage, "Print the byte coverage");
     ("-s", Arg.Set print_nlist, "Print the symbol table, if present");
     ("-f", Arg.Set_string search_term_string, "Find symbol in binary");
     ("-b", Arg.Set marshal_symbols, "Build a symbol map and write to $(HOME)/.rdr/tol; default directory is /usr/lib, change with -d");
     ("-w", Arg.Set write_symbols, "Write out a flattened system map to $(HOME)/.rdr/symbols (good for grepping)");
     ("-G", Arg.Set print_goblin, "Print using the goblin binary format");
     ("--goblin", Arg.Set print_goblin, "Print using the goblin binary format");
     ("-D", Arg.Set disassemble, "Disassemble found symbol(s)");
     ("--dis", Arg.Set disassemble, "Disassemble found symbol(s)");
     ("--do", Arg.Int (fun i -> disassemble_offset := i), "Disassemble at offset");
     ("--sections", Arg.Set print_sections, "Print the sections: sections headers for elf; segments for mach; section tables for PE");
    ] in
  let usage_msg = "usage: rdr [-r] [-b] [-m] [-d] [-g] [-G --goblin] [-v | -l | -e | -i] [<path_to_binary>]\noptions:" in
  Arg.parse speclist set_anon_argument usage_msg;
  if (!print_version) then
    begin
      Printf.printf "v%s\n" version;
      exit 0
    end
  else
    (* BEGIN program init *)  
    Rdr.Utils.Storage.create_dot_directory (); (* make our .rdr/ if we haven't already *)
  let config = get_config () in
  if (!disassemble_offset <> 0) then
    Rdr.Utils.Command.disassemble config.filename !disassemble_offset 100
  else
  if (config.analyze && config.filename = "") then
    if (config.verbose) then
      begin
        (* hack to print version *)
        Printf.printf "v%s\n" version;
        exit 0
      end
    else      
      begin
        Printf.eprintf "Error: no path to binary given\n";
        Arg.usage speclist usage_msg;
        exit 1;
      end;
  if (config.use_map) then
    (* -m *)
    use_symbol_map config
  else if (config.marshal_symbols) then
    (* -b build a marshalled symbol map*)
    build_symbol_map config
  else
    (* analyzing a binary using anon arg *)
    Rdr.Object.get config.filename |> analyze config
