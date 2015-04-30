(* TODO: 
   (5) add a symbol like --map to run `rdr -b -L /usr/lib /System /Libraries -r`
       which would essentially build a map of the entire system
*)

open Config 			(* because only has a record type *)

type os = Darwin | Linux | Other

let get_os () = 
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  if (uname = "Darwin") then Darwin
  else if (uname = "Linux") then Linux
  else Other
	 
(* TODO: consider moving these refs to a globals module which is get and set from there *)
let use_map = ref false
let graph = ref false
let verbose = ref false
let use_goblin = ref false
let recursive = ref false
let write_symbols = ref false
let marshal_symbols = ref false
let print_nlist = ref false
let base_symbol_map_directories = ref ["/usr/lib/"]
let anonarg = ref ""

let disassemble = ref false
let search_term_string = ref ""

let get_config () =
  let analyze = not (!use_map || !marshal_symbols) in
  {
      Config.analyze;
      silent = false;
      print_nlist = !print_nlist;
      verbose = !verbose;
      disassemble = !disassemble;
      use_map = !use_map;
      recursive = !recursive;
      write_symbols = !write_symbols;
      marshal_symbols = !marshal_symbols;
      base_symbol_map_directories = !base_symbol_map_directories;
      graph = !graph;
      filename = !anonarg;
      search_term = !search_term_string;
      use_goblin = !use_goblin;
  } 
		      
let set_base_symbol_map_directories dir_string = 
  (* Printf.printf "%s\n" dir_string; *)
  let dirs = Str.split (Str.regexp "[ ]+") dir_string |> List.map String.trim in
  match dirs with
  | [] -> raise @@ Arg.Bad "Invalid argument: directories must be separated by spaces, -d /usr/local/lib, /some/other/path"
  | _ -> 
    (* Printf.printf "setting dirs: %s\n" @@ Generics.list_to_string dirs; *)
    base_symbol_map_directories := dirs

let set_anon_argument string =
  anonarg := string
	       
let main =
  let speclist = 
    [("-m", Arg.Set use_map, "Use a pre-marshalled system symbol map; use this in conjunction with -f, -g, or -w");
     ("-g", Arg.Set graph, "Creates a graphviz file; generates lib dependencies if -b given");
     ("-d", Arg.String (set_base_symbol_map_directories), "String of space separated directories to build symbol map from; default is /usr/lib");
     ("-r", Arg.Set recursive, "Recursively search directories for binaries");
     ("-v", Arg.Set verbose, "Be verbose");
     ("-s", Arg.Set print_nlist, "Print the symbol table, if present");
     ("-f", Arg.Set_string search_term_string, "Find symbol in binary");
     ("-b", Arg.Set marshal_symbols, "Build a symbol map and write to $(HOME)/.rdr/tol; default is /usr/lib, change with -d");
     ("-w", Arg.Set write_symbols, "Write out a flattened system map to $(HOME)/.rdr/symbols (good for grepping)");
     ("-G", Arg.Set use_goblin, "Use the goblin binary format");
     ("--goblin", Arg.Set use_goblin, "Use the goblin binary format");
     ("-D", Arg.Set disassemble, "Disassemble all found symbols");
     ("--dis", Arg.Set disassemble, "Disassemble all found symbols");

     (* ("-n", Arg.Int (set_max_files), "Sets maximum number of files to list"); *)
     (* ("-d", Arg.String (set_directory), "Names directory to list files"); *)
    ] in
  let usage_msg = "usage: rdr [-r] [-b] [-d] [-g] [-G --goblin] [-v] [<path_to_binary> | <symbol_name>]\noptions:" in
  Arg.parse speclist set_anon_argument usage_msg;
  (* BEGIN program init *)  
  Output.create_dot_directory (); (* make our .rdr/ if we haven't already *)
  let config = get_config () in
  if (config.analyze && config.filename = "") then
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
