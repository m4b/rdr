(* TODO: 
   (5) add a symbol like --map to run `rdr -b -L /usr/lib /System /Libraries -r`
       which would essentially build a map of the entire system
*)

exception Unimplemented_binary_type of string

type os = Darwin | Linux | Other

let get_os () = 
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  if (uname = "Darwin") then Darwin
  else if (uname = "Linux") then Linux
  else Other

let get_symbol_filename name =
  try 
    (Sys.getenv "HOME") ^ Filename.dir_sep ^ "." ^ name
  with
  | Not_found ->
    Sys.getcwd() ^ Filename.dir_sep ^ name ^ "." ^ name

(* TODO: consider moving these refs to a globals module which is get and set from there *)
let build = ref false
let graph = ref false
let verbose = ref false
let use_goblin = ref false
let recursive = ref false
let write_symbols = ref false
let marshal_symbols = ref false
let print_nlist = ref false
let symbol = ref ""
let base_symbol_map_directories = ref ["/usr/lib/"]
let anonarg = ref ""

let set_base_symbol_map_directories dir_string = 
  (* Printf.printf "%s\n" dir_string; *)
  let dirs = Str.split (Str.regexp "[ ]+") dir_string |> List.map String.trim in
  match dirs with
  | [] -> raise @@ Arg.Bad "Invalid argument: directories must be sepearted by spaces, -d /usr/local/lib, /some/other/path"
  | _ -> 
    (* Printf.printf "setting dirs: %s\n" @@ Generics.list_to_string dirs; *)
    base_symbol_map_directories := dirs

let set_anon_argument string =
  anonarg := string

let build_system_map () =
  begin
    let symbol = !anonarg in
    let searching = (symbol <> "") in
    let graph = not searching && !graph in
    if (searching) then 
      (* cuz on slow systems i want to see this message first *)
      begin
        let recursive_message = if (!recursive) then 
				  " (recursively)" 
				else ""
        in
        Printf.printf "searching %s%s for %s:\n"
		      (Generics.list_to_string 
			 ~omit_singleton_braces:true 
			 !base_symbol_map_directories)
		      recursive_message
		      symbol; flush stdout
      end;
    if (searching) then
      (* rdr -b <symbol_name> *)
      let f = get_symbol_filename "tol" in
      let ic = open_in_bin f in
      let map = Marshal.from_channel ic in
      
      begin
        try
          SymbolMap.find_symbol symbol map
          |> List.iter 
	       (fun data ->
		  (* MachExports.print_mach_export_data data *)
		  GoblinSymbol.print_symbol_data ~with_lib:true ~like_export:true data
	       );
        with Not_found ->
	  Printf.printf "not found\n"; ()
      end
    else
      begin
	let map = SymbolMap.build_polymorphic_map 
		    ~recursive:!recursive 
		    ~graph:graph 
		    ~verbose:!verbose 
		    !base_symbol_map_directories
	in
        (* rdr -b -g *)
        let export_list = SymbolMap.flatten_polymorphic_map_to_list map
                          |> GoblinSymbol.sort_symbols 
        in
        let export_list_string = SymbolMap.polymorphic_list_to_string export_list in
        if (!write_symbols) then
          begin
            let f = get_symbol_filename "symbols" in
            let oc = open_out f in
            Printf.fprintf oc "%s" export_list_string;
            close_out oc;
          end
	else if (!marshal_symbols) then
          begin
            let f = get_symbol_filename "tol" in
            let oc = open_out_bin f in
	    Marshal.to_channel oc map [];
            close_out oc;
          end
        else
          Printf.printf "%s\n" export_list_string;
      end
  end
	       
let main =
  let speclist = 
    [("-b", Arg.Set build, "Builds a system symbol map; use this in conjunction with -m or -w to write the results to disk; -f will look for a marshalled file from -m to speed up arbitary symbol lookup times");
     ("-g", Arg.Set graph, "Creates a graphviz file; generates lib dependencies if -b given");
     ("-d", Arg.String (set_base_symbol_map_directories), "String of space separated directories to build symbol map from; default is /usr/lib");
     ("-r", Arg.Set recursive, "Recursively search directories for binaries");
     ("-v", Arg.Set verbose, "Be verbose");
     ("-s", Arg.Set print_nlist, "Print the symbol table, if present");
     ("-f", Arg.Set_string symbol, "Find symbol in binary");
     ("-m", Arg.Set marshal_symbols, "Marshal the generated system map to your home directory (speeds up arbitrary symbol search times substantially)");
     ("-w", Arg.Set write_symbols, "Write out the flattened system map .symbols file to your home directory");
     ("-G", Arg.Set use_goblin, "Use the goblin binary format");
     ("--goblin", Arg.Set use_goblin, "Use the goblin binary format");
     (* ("-n", Arg.Int (set_max_files), "Sets maximum number of files to list"); *)
     (* ("-d", Arg.String (set_directory), "Names directory to list files"); *)
    ] in
  let usage_msg = "usage: rdr [-r] [-b] [-d] [-g] [-G --goblin] [-v] [<path_to_binary> | <symbol_name>]\noptions:" in
  Arg.parse speclist set_anon_argument usage_msg;
  (* BEGIN program init *)
  if (!anonarg = "" && not !build) then
    begin
      Printf.eprintf "Error: no path to binary given\n";
      Arg.usage speclist usage_msg;
      exit 1;
    end;
  if (!build) then
    (* -b *)
    build_system_map ()
  else
    (* rdr <binary> *)
    let filename = !anonarg in
    let analyze  = !symbol = "" in
    let silent = not analyze && not !verbose in (* so we respect verbosity if searching... cl boolean configuration sucks *)
    (* ===================== *)
    (* MACH *)
    (* ===================== *)
    match Object.get_bytes filename with
    | Object.Mach bytes ->
      let binary = Mach.analyze ~silent:silent ~print_nlist:!print_nlist ~lc:analyze ~verbose:!verbose bytes filename in
      if (not !verbose && analyze) then
      begin
        Printf.printf "Libraries (%d)\n" @@ (binary.Mach.nlibs - 1); (* because 0th element is binary itself *)
        Printf.printf "Exports (%d)\n" @@ binary.Mach.nexports;
        Printf.printf "Imports (%d)\n" @@ binary.Mach.nimports
      end;
      if (not analyze) then
        try
          Mach.find_export_symbol !symbol binary |> MachExports.print_mach_export_data ~simple:true
        (* TODO: add find import symbol *)
        with Not_found ->
          Printf.printf "";
      else 
      if (!graph) then
        if (!use_goblin) then
          begin
            let goblin = Mach.to_goblin binary in
            Graph.graph_goblin ~draw_imports:true ~draw_libs:true goblin (Filename.basename filename);
          end
        else
          Graph.graph_mach_binary 
            ~draw_imports:true 
            ~draw_libs:true 
            binary 
            (Filename.basename filename)
    (* ===================== *)
    (* ELF *)
    (* ===================== *)
    | Object.Elf binary ->
       if (!build) then
	 build_system_map ()
       else
	 (* analyze the binary and print program headers, etc. *)
	 let binary = Elf.analyze ~silent:silent ~nlist:!print_nlist ~verbose:!verbose ~filename:filename binary in
	 if (not analyze) then
           try
             Elf.find_export_symbol !symbol binary |> Goblin.print_export
           with Not_found ->
             Printf.printf "";
	 else
	 if (!graph) then Graph.graph_goblin binary @@ Filename.basename filename;
    | Object.Unknown ->
      raise @@ Unimplemented_binary_type "Unknown binary"
