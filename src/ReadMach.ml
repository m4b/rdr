(* TODO: 
   (0) add load segment boundaries, and nlists locals as a parameters to the compute size
   (1) compute final sizes after imports, locals, 
       and exports are glommed into a goblin symbol soup, using all the information available
 *)

open Printf

open LoadCommand
open Config (* only contains a record *)

type mach_binary = {
  name: string;
  install_name: string;
  imports: MachImports.mach_import_data array;
  nimports: int;
  exports: MachExports.mach_export_data array;
  nexports: int;
  islib: bool;
  libs: string array;
  nlibs: int;
  code: bytes;
}

let imports_to_string imports = 
  let b = Buffer.create (Array.length imports) in
  Array.fold_left (fun acc import -> 
      Buffer.add_string acc
      @@ Printf.sprintf "%s"
      @@ MachImports.mach_import_data_to_string import;
      acc
    ) b imports |> Buffer.contents

let exports_to_string exports =
  let b = Buffer.create (Array.length exports) in
  Array.fold_left (fun acc export -> 
      Buffer.add_string acc
      @@ Printf.sprintf "%s"
      @@ MachExports.mach_export_data_to_string export;
      acc
    ) b exports |> Buffer.contents

let binary_to_string binary = 
  let libstr = if (binary.islib) then " (LIB)" else "" in
  Printf.sprintf "%s%s:\nMachImports (%d):\n%sMachExports (%d):\n%s\n"
    binary.name libstr
    (binary.nimports)
    (imports_to_string binary.imports)
    (binary.nexports)
    (exports_to_string binary.exports)

let debug = false

let create_binary (name,install_name) (nls,las) exports islib libs =
  (* flatten and condense import info *)
  let imports = nls @ las |> Array.of_list in
  let nimports = Array.length imports in
  let exports = Array.of_list exports in
  let nexports = Array.length exports in (* careful here, due to aliasing, if order swapped, in trouble *)
  let nlibs = Array.length libs in
  let code = Bytes.empty in
  {name; install_name; imports; nimports; exports; nexports; islib; libs; nlibs; code}

let to_goblin mach =
  let name = mach.name in
  let install_name = mach.install_name in
  let libs = mach.libs in
  let nlibs = mach.nlibs in
  let exports =
    Array.init (mach.nexports)
	       (fun i ->
		let export = mach.exports.(i) in
		(MachExports.mach_export_data_to_symbol_data export
		 |> GoblinSymbol.to_goblin_export))
  in
  let nexports = mach.nexports in
  let imports =
    Array.init (mach.nimports)    
      (fun i ->
	 let import = mach.imports.(i) in
         let name = GoblinSymbol.find_symbol_name import in
         let lib = GoblinSymbol.find_symbol_lib import |> fst in
         let is_lazy = MachImports.is_lazy import in 
         let idx = i in
         let offset = GoblinSymbol.find_symbol_offset import in
         let size = GoblinSymbol.find_symbol_size import in  
  {Goblin.Import.name = name; lib; is_lazy; idx; offset; size}) in
  let nimports = mach.nimports in
  let islib = mach.islib in
  let code = mach.code in
  {Goblin.name; install_name; islib; libs; nlibs; exports; nexports; imports; nimports; code}

let analyze config binary = 
  let mach_header = MachHeader.get_mach_header binary in
  let lcs = LoadCommand.get_load_commands binary MachHeader.sizeof_mach_header mach_header.MachHeader.ncmds mach_header.MachHeader.sizeofcmds in
  if (not config.silent) then
    begin
      if (not config.search) then MachHeader.print_header mach_header;     
      if (config.verbose || config.print_headers) then LoadCommand.print_load_commands lcs
    end;
  let name = 
    match LoadCommand.get_lib_name lcs with
    | Some dylib ->
      dylib.lc_str
    | _ -> config.name (* we're not a dylib *)
  in
  let install_name = config.install_name in
  (* lib.(0) = install_name *)
  let segments = LoadCommand.get_segments lcs in
  let libraries = LoadCommand.get_libraries lcs install_name in 
  (* move this inside of dyld, need the nlist info to compute locals... *)
  let islib = mach_header.MachHeader.filetype = MachHeader.kMH_DYLIB in
  let dyld_info = LoadCommand.get_dyld_info lcs in
  match dyld_info with
  | Some dyld_info ->
    (* TODO: add load segment boundaries, and nlists locals as a parameters *)
    let symbols = 
      try
        let symtab = LoadCommand.find_load_command LoadCommand.SYMTAB lcs in
        Nlist.get_symbols binary symtab
      with Not_found ->
        []
    in
    let locals = Nlist.filter_by_kind GoblinSymbol.Local symbols in
    ignore locals;
    let exports = MachExports.get_exports binary dyld_info libraries in 
    (* TODO: yea, need to fix imports like machExports; send in the libraries,
       do all that preprocessing there, and not in create binary *)
    let imports = MachImports.get_imports binary dyld_info libraries segments in
    if (not config.silent) then
      begin
	if (config.verbose || config.print_libraries) then LoadCommand.print_libraries libraries;
	if (config.verbose || config.print_exports) then MachExports.print_exports exports;
	if (config.verbose || config.print_imports) then MachImports.print_imports imports;
	if (config.print_nlist) then Nlist.print_symbols symbols;	
      end;
    (* TODO: compute final sizes here, after imports, locals, 
       and exports are glommed into a goblin soup, using all the information available*)
    create_binary (name,install_name) imports exports islib libraries
  | None ->
    if (config.verbose && not config.silent) then Printf.printf "No dyld_info_only\n";
    create_binary (name,install_name) MachImports.empty MachExports.empty islib libraries

let find_export_symbol symbol binary =
  let len = binary.nexports in
  let rec loop i =
    if (i >= len) then raise Not_found
    else if (GoblinSymbol.find_symbol_name binary.exports.(i) = symbol) then
      binary.exports.(i)
    else
      loop (i + 1)
  in loop 0    


let find_import_symbol symbol binary =
  MachImports.find symbol binary.imports
