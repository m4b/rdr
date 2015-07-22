open Config

let debug = false

(* goblin *)

(* hacky function to filter imports from exports, etc. *)
(* todo use proper variants here ffs *)
let get_goblin_kind entry bind stype =
  if (entry.Elf.SymbolTable.st_value = 0x0
       && entry.Elf.SymbolTable.st_shndx = 0
       && entry.Elf.SymbolTable.name <> "") (* ignore first \0 entry *)
  then
    GoblinSymbol.Import
  else if (bind = "LOCAL") then
    GoblinSymbol.Local
  else if ((bind = "GLOBAL"
	    || (bind = "WEAK" && (stype = "FUNC"
				  || stype = "IFUNC"
				  || stype = "OBJECT")))
	   && entry.Elf.SymbolTable.st_value <> 0) then
    GoblinSymbol.Export
  else GoblinSymbol.Other

(* polymorphic variants don't need to be qualified by module
 since they are open and the symbol is unique *)
let symbol_entry_to_goblin_symbol
      ~tol:tol ~libs:libs ~relocs:relocs (soname,install_name) index entry =
  let bind   = (Elf.SymbolTable.get_bind entry.Elf.SymbolTable.st_info |> Elf.SymbolTable.symbol_bind_to_string) in
  let stype  = (Elf.SymbolTable.get_type entry.Elf.SymbolTable.st_info |> Elf.SymbolTable.symbol_type_to_string) in
  let name   = `Name entry.Elf.SymbolTable.name in
  let offset =
    `Offset (
       if (entry.Elf.SymbolTable.st_value = 0) then
	 (* this _could_ be relatively expensive *)
	 Elf.Reloc.get_size index relocs
       else
	 entry.Elf.SymbolTable.st_value)
  in
  let size = `Size entry.Elf.SymbolTable.st_size in
  let kind = `Kind (get_goblin_kind entry bind stype) in
  let lib =
    (* TODO: this is a complete disaster; *)
    match kind with
    | `Kind GoblinSymbol.Export ->
       `Lib (soname,install_name)
    | `Kind GoblinSymbol.Import ->
       if (ToL.is_empty tol) then
	 `Lib ("∅","∅")
       else
	 let l = (ToL.get_libraries ~bin_libs:libs entry.Elf.SymbolTable.name tol) in
	 `Lib (l,l)
    | _ ->
       `Lib ("","")
  in
  let data = `PrintableData
	      (Printf.sprintf
		 "%s %s" bind stype) in
  [name; lib; offset; size; kind; data]

let symbols_to_goblin ?use_tol:(use_tol=true) ~libs:libs soname dynsyms relocs =
  let tol =
    try
      if (use_tol) then ToL.get () else ToL.empty
    with ToL.Not_built ->
      ToL.empty
  in
  List.mapi
    (symbol_entry_to_goblin_symbol
       ~tol:tol ~libs:libs ~relocs:relocs soname) dynsyms

let create_goblin_binary soname install_name libraries islib goblin_exports goblin_imports =
  let name = soname in
  let install_name = install_name in
  let libs = Array.of_list (soname::libraries) in (* to be consistent... for graphing, etc. *)
  let nlibs = Array.length libs in
  let exports =
    Array.of_list
    @@ List.map (GoblinSymbol.to_goblin_export) goblin_exports
  in
  let nexports = Array.length exports in
  let imports =
    Array.of_list
    @@ List.map (GoblinSymbol.to_goblin_import) goblin_imports
  in
  let nimports = Array.length imports in
  (* empty code *)
  let code = Bytes.empty in
  {Goblin.name;
   install_name; islib; libs; nlibs; exports; nexports;
   imports; nimports; code}

let analyze config binary =
  let elf = Elf.get binary in
  (* for consistency and display, goblin makes everything have names *)
  let soname = if (elf.Elf.soname = "") then config.name else elf.Elf.soname in
  if (not (Elf.Header.is_supported elf.Elf.header)) then
    (* for relocs, esp /usr/lib/crt1.o *)
    create_goblin_binary
      config.name config.install_name [] false [] []
  else
    let goblin_symbols =
      symbols_to_goblin
	~use_tol:config.use_tol
	~libs:elf.Elf.libraries
	(soname,config.install_name)
	elf.Elf.dynamic_symbols
	elf.Elf.relocations
      |> GoblinSymbol.sort_symbols
      |> function | [] -> [] | syms -> List.tl syms
      (* because the head (the first entry, after sorting)
         is a null entry, and also _DYNAMIC can be empty *)
    in
    let goblin_imports =
      List.filter
	(fun symbol ->
	 GoblinSymbol.find_symbol_kind symbol
	 |> function
	   | GoblinSymbol.Import -> true
	   | _ -> false) goblin_symbols
    in
    let goblin_exports =
      List.filter
	(fun symbol ->
	 GoblinSymbol.find_symbol_kind symbol
	 |> function
	   | GoblinSymbol.Export -> true
	   | _ -> false) goblin_symbols
    in
    (* print switches *)
    if (not config.silent) then
      begin
      if (not config.search) then Elf.Header.print_elf_header64 elf.Elf.header;
      if (config.verbose || config.print_headers) then
	begin
	  Elf.ProgramHeader.print_program_headers elf.Elf.program_headers;
	  Elf.SectionHeader.print_section_headers elf.Elf.section_headers
	end;
	if (config.print_headers) then Elf.Dynamic.print_dynamic elf.Elf._dynamic;
	if (config.print_nlist) then
	  symbols_to_goblin ~use_tol:config.use_tol ~libs:elf.Elf.libraries (soname,config.install_name) elf.Elf.symbol_table elf.Elf.relocations
	  |> GoblinSymbol.sort_symbols
	  |> List.iter
	       (GoblinSymbol.print_symbol_data ~like_nlist:true);
	if (config.verbose || config.print_libraries) then
	  begin
	    if (elf.Elf.is_lib) then Printf.printf "Soname: %s\n" soname;
	    Printf.printf "Libraries (%d)\n" (List.length elf.Elf.libraries);
	    List.iter (Printf.printf "\t%s\n") elf.Elf.libraries
	  end;
	if (config.verbose || config.print_exports) then
	  begin
	    Printf.printf "Exports (%d)\n" (List.length goblin_exports);
	    List.iter (GoblinSymbol.print_symbol_data) goblin_exports
	  end;
	if (config.verbose || config.print_imports) then
	  begin
	    Printf.printf "Imports (%d)\n" (List.length goblin_imports);
	    List.iter (GoblinSymbol.print_symbol_data ~with_lib:true) goblin_imports
	  end
      end;
    (* ============== *)
    (* create goblin binary *)
    create_goblin_binary
      soname      
      config.install_name
      elf.Elf.libraries
      elf.Elf.is_lib
      goblin_exports
      goblin_imports    

let find_export_symbol symbol binary = Goblin.get_export symbol binary.Goblin.exports
