(* TODO: 
   (0) add load segment boundaries, and nlists locals as a parameters to the compute size
   (1) compute final sizes after imports, locals, 
       and exports are glommed into a goblin symbol soup, using all the information available
   (2) determine what/how to lookup flat namespaces
    
 *)

open Printf

open MachHeader
open LoadCommand
open Config (* only contains a record *)

type mach_binary = {
  name: string;
  soname: string;
  imports: MachImports.import array;
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
      Buffer.add_string acc @@ Printf.sprintf "%s" @@ MachImports.import_to_string import;
      acc
    ) b imports |> Buffer.contents

let exports_to_string exports =
  let b = Buffer.create (Array.length exports) in
  Array.fold_left (fun acc export -> 
      Buffer.add_string acc @@ Printf.sprintf "%s" @@ MachExports.mach_export_data_to_string export;
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

(* TODO: figure out the flat look up situation *)
let create_binary (name,soname) (nls,las) exports islib libs =
  (*   
  Array.iter (fun e -> Printf.printf "%s\n" e) libraries;
  Printf.printf "nls (%d)\n" (Array.length nls);
  Printf.printf "las (%d)\n" (Array.length las);
  *)
  let len = Array.length nls in
  (* flatten and condense import info *)
  let imports = Array.init ((Array.length nls) + (Array.length las))
      (fun index ->
         if (debug) then Printf.printf "index %d\n" index;
         if (debug) then Printf.printf "len %d\n" len;
         let bi,is_lazy = if (index < len) then nls.(index),false else las.(index - len),true in
         let dylib = 
           if (bi.MachImports.special_dylib = BindOpcodes.kBIND_SPECIAL_DYLIB_SELF) then 
             begin
               if (debug) then Printf.printf "dylib self\n";
               name 
             end
           else if 
             (bi.MachImports.special_dylib = BindOpcodes.kBIND_SPECIAL_DYLIB_FLAT_LOOKUP) then
             begin
               if (debug) then Printf.printf "flatlookup\n";
               "@FLAT_LOOKUP"
             end
           else if(bi.MachImports.special_dylib = BindOpcodes.kBIND_SPECIAL_DYLIB_MAIN_EXECUTABLE) then
             begin
               if (debug) then Printf.printf "main executable\n";
               "@MAIN_EXE"
             end
           else
             begin
               if (debug) then Printf.printf "regular %d\n" bi.MachImports.special_dylib;
               (* this will crash the app if we come across a different ordinal *)
               libs.(bi.MachImports.symbol_library_ordinal) 
             end
         in
         {MachImports.bi; dylib; is_lazy}) in
  let nimports = Array.length imports in
  let exports = Array.of_list exports in
  let nexports = Array.length exports in (* careful here, due to aliasing, if order swapped, in trouble *)
  let nlibs = Array.length libs in
  let code = Bytes.empty in
  {name; soname; imports; nimports; exports; nexports; islib; libs; nlibs; code}

let to_goblin mach =
  let name = mach.name in
  let soname = mach.soname in
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
		{Goblin.Import.name = import.MachImports.bi.MachImports.symbol_name; lib = import.MachImports.dylib; is_lazy = import.MachImports.is_lazy; idx = 0x0; offset = 0x0; size = 0x0 })
  in
  let nimports = mach.nimports in
  let islib = mach.islib in
  let code = mach.code in
  {Goblin.name; soname; islib; libs; nlibs; exports; nexports; imports; nimports; code}

let analyze config binary = 
  let mach_header = MachHeader.get_mach_header binary in
  if ((config.verbose || config.print_headers) && not config.silent) then MachHeader.print_header mach_header;
  let lcs = LoadCommand.get_load_commands binary MachHeader.sizeof_mach_header mach_header.ncmds mach_header.sizeofcmds in
  if ((config.verbose || config.print_headers) && not config.silent) then LoadCommand.print_load_commands lcs;
  let soname = 
    match LoadCommand.get_lib_name lcs with
    | Some dylib ->
      dylib.lc_str
    | _ -> Filename.basename config.filename
  in
  let name = Filename.basename soname in
  (* lib.(0) = soname *)
  let libraries = LoadCommand.get_libraries lcs soname in 
  if ((config.verbose || config.print_libraries) && not config.silent) then 
      LoadCommand.print_libraries libraries;
  (* move this inside of dyld, need the nlist info to compute locals... *)
  let islib = mach_header.filetype = kMH_DYLIB in
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
    if ((config.verbose || config.print_exports) && not config.silent) then MachExports.print_exports exports;
    (* TODO: yea, need to fix imports like machExports; send in the libraries, do all that preprocessing there, and not in create binary *)
    let imports = MachImports.get_imports binary dyld_info in 
    if ((config.verbose || config.print_imports) && not config.silent) then MachImports.print_imports imports;
    if (config.print_nlist && not config.silent) then Nlist.print_symbols symbols;
    (* TODO: compute final sizes here, after imports, locals, 
       and exports are glommed into a goblin soup, using all the information available*)
    create_binary (name,soname) imports exports islib libraries
  | None ->
    if (config.verbose && not config.silent) then Printf.printf "No dyld_info_only\n";
    create_binary (name,soname) MachImports.empty MachExports.empty islib libraries

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

(* ******************** *)
(*   
  MachExports.print_exports exports;
  MachImports.print_imports imports;
  print_string @@ binary_to_string mach_binary;
 *)
