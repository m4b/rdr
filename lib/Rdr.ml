module Elf = Elf
module Mach = Mach
module Goblin = Goblin
module Utils = RdrUtils
module Object = RdrObject

module Map = struct

    include SymbolMap

    let build ~verbose:verbose ~graph:graph ~libs:libstack =
      let tbl = Hashtbl.create ((Stack.length libstack) * 100) in
      (* 
      if (verbose) then Printf.printf "Building map...\n";
      if (verbose) then 
        begin
          Printf.printf "Total libs: %d\n" (Stack.length libstack);
          Printf.printf "... Done\n";
        end
       *)
      let rec loop map lib_deps = 
        if (Stack.is_empty libstack) then
          (* 
          begin
	    output_stats tbl;
            Graph.graph_lib_dependencies ~use_dot_storage:true lib_deps;
	    if (graph) then
	      Graph.graph_library_dependencies ~use_sfdp:(Command.is_linux()) ~use_dot_storage:true;
          end
           *)
          map
        else
          let lib = Stack.pop libstack in
          (* this might throw errors again, especially reading errors, etc. *)
          let bytes = Object.get ~verbose:verbose lib in
          let name = Filename.basename lib in
          let install_name = lib in
          let binary = 
            match bytes with
            | Object.Mach binary ->
               let mach = Mach.get binary in
               Goblin.Mach.to_goblin mach install_name
            | Object.Elf binary ->
               (* hurr durr iman elf *)
               Elf.get binary |> Goblin.Elf.from install_name
            | Object.PE32 binary ->
               PE.get binary |> Goblin.PE.from install_name
            | Object.Unknown (lib,error) ->
               failwith "ERROR"
               (*                loop map lib_deps *)
          in
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
	       let data =
                 Goblin.Symbol.from_goblin_export
		   data
                   ~libname:binary.Goblin.name
                   ~libinstall_name:binary.Goblin.install_name
	       in
	       try 
		 let data' = ToL.find symbol acc in
		 ToL.add symbol (data::data') acc
	       with
	       | Not_found ->
		  (* we don't have a record of this export symbol mapping; create a new singleton list with the data (we already know the `Lib because MachExport's job is to add that) except this is elf, so we also have to "promise" we did that there. Starting to become untenable and not very maintainable code *)
		  ToL.add symbol [data] acc
	      ) map symbols in
          loop map' ((binary.Goblin.name, binary.Goblin.libs)::lib_deps)
      in loop ToL.empty []
  end
