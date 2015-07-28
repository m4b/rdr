(* TODO: 
   (0) add a symbol like --map-all to run `rdr -b -d /usr/lib /System /Libraries -r`
       which would essentially build a map of the entire system, or something like that
*)

let version = "1.3"

open Config (* because only has a record type *)

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
let use_goblin = ref false
let recursive = ref false
let write_symbols = ref false
let marshal_symbols = ref false
let print_nlist = ref false
let base_symbol_map_directories = ref ["/usr/lib/"]
let framework_directories = ref []
let anonarg = ref ""

let disassemble = ref false
let search_term_string = ref ""

let print_version = ref false

let get_config () =
  let analyze = not (!use_map || !marshal_symbols) in
  let search = !search_term_string <> "" in
  let silent = not analyze && not !verbose in (* so we respect verbosity if searching*)
  let use_tol = !print_imports || !verbose || (!graph && analyze) in
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
    consume_bytes = false;
    print_nlist = !print_nlist;
    verbose = !verbose;
    print_headers = !print_headers;      
    print_libraries = !print_libraries;
    print_exports = !print_exports;
    print_imports = !print_imports;      
    disassemble = !disassemble;
    use_map = !use_map;
    recursive = !recursive;
    write_symbols = !write_symbols;
    marshal_symbols = !marshal_symbols;
    base_symbol_map_directories = !base_symbol_map_directories;
    framework_directories = !framework_directories;
    graph = !graph;
    filename = !anonarg; 	(* TODO: this should be Filname.basename, but unchanged for now *)
    search_term = !search_term_string;
    use_goblin = !use_goblin;
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
	       
let main =
  let speclist = 
    [("-m", Arg.Set use_map, "Use a pre-marshalled system symbol map; use this in conjunction with -f, -D, -g, or -w");
     ("-g", Arg.Set graph, "Creates a graphviz file; generates lib dependencies if -b given");
     ("-d", Arg.String (set_base_symbol_map_directories), "String of space separated directories to build symbol map from; default is /usr/lib");
     ("-F", Arg.String (set_framework_directories), "(OSX Only) String of space or colon separated base framework directories to additionally search when building the symbol map");
     ("-r", Arg.Set recursive, "Recursively search directories for binaries; use with -b");
     ("-v", Arg.Set verbose, "Print all the things");
     ("--version", Arg.Set print_version, "Print the version and exit");
     ("-h", Arg.Set print_headers, "Print the header"); 
     ("-l", Arg.Set print_libraries, "Print the dynamic libraries");     
     ("-e", Arg.Set print_exports, "Print the exported symbols");
     ("-i", Arg.Set print_imports, "Print the imported symbols");
     ("-s", Arg.Set print_nlist, "Print the symbol table, if present");
     ("-f", Arg.Set_string search_term_string, "Find symbol in binary");
     ("-b", Arg.Set marshal_symbols, "Build a symbol map and write to $(HOME)/.rdr/tol; default directory is /usr/lib, change with -d");
     ("-w", Arg.Set write_symbols, "Write out a flattened system map to $(HOME)/.rdr/symbols (good for grepping)");
     ("-G", Arg.Set use_goblin, "Use the goblin binary format");
     ("--goblin", Arg.Set use_goblin, "Use the goblin binary format");
     ("-D", Arg.Set disassemble, "Disassemble all found symbols");
     ("--dis", Arg.Set disassemble, "Disassemble all found symbols");
    ] in
  let usage_msg = "usage: rdr [-r] [-b] [-m] [-d] [-g] [-G --goblin] [-v | -l | -e | -i] [<path_to_binary> | <symbol_name>]\noptions:" in
  Arg.parse speclist set_anon_argument usage_msg;
  if (!print_version) then
    begin
      Printf.printf "v%s\n" version;
      exit 0
    end
  else
  (* BEGIN program init *)  
  Storage.create_dot_directory (); (* make our .rdr/ if we haven't already *)
  let config = get_config () in
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
    SymbolMap.use_symbol_map config
  else if (config.marshal_symbols) then
    (* -b build a marshalled symbol map*)
    SymbolMap.build_symbol_map config
  else
    (* analyzing a binary using anon arg *)
    Object.get_bytes config.filename |> Object.analyze config
