open Binary
open Config

let debug = false

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
  let header = ElfHeader.get_elf_header64 binary in
  let program_headers =
    ProgramHeader.get_program_headers
      binary
      header.ElfHeader.e_phoff
      header.ElfHeader.e_phentsize
      header.ElfHeader.e_phnum
  in
  let slide_sectors =
    ProgramHeader.get_slide_sectors program_headers
  in
  let section_headers =
    SectionHeader.get_section_headers
      binary
      header.ElfHeader.e_shoff
      header.ElfHeader.e_shentsize
      header.ElfHeader.e_shnum
  in
  if (not config.silent) then
    begin
      if (not config.search) then ElfHeader.print_elf_header64 header;
      if (config.verbose || config.print_headers) then
	begin
	  ProgramHeader.print_program_headers program_headers;
	  SectionHeader.print_section_headers section_headers
	end;
    end;
  if (not (ElfHeader.is_supported header)) then
    (* for relocs, esp /usr/lib/crt1.o *)
    create_goblin_binary
      config.name config.install_name [] false [] []
  else
    let is_lib = (ElfHeader.is_lib header) in
    let symbol_table = SymbolTable.get_symbol_table binary section_headers in
    let _DYNAMIC = Dynamic.get_DYNAMIC binary program_headers in
    let symtab_offset, strtab_offset, strtab_size =
      Dynamic.get_dynamic_symbol_offset_data _DYNAMIC slide_sectors
    in
    let dynamic_strtab =
      Dynamic.get_dynamic_strtab binary strtab_offset strtab_size
    in
    let libraries = Dynamic.get_libraries _DYNAMIC dynamic_strtab in
    let dynamic_symbols =
      Dynamic.get_dynamic_symbols
	binary
	slide_sectors
	symtab_offset
	strtab_offset
	strtab_size
    in
    let soname =
      try 
	let offset = Dynamic.get_soname_offset _DYNAMIC in
	Binary.string binary (strtab_offset + offset)
      with Not_found -> config.name (* we're not a dylib *)
    in
    let relocs =
      Dynamic.get_reloc_data _DYNAMIC slide_sectors
      |> ElfReloc.get_relocs64 binary
    in
    let goblin_symbols =
      SymbolTable.symbols_to_goblin ~use_tol:config.use_tol ~libs:libraries soname dynamic_symbols relocs
      |> GoblinSymbol.sort_symbols_with List.sort |> List.tl
      (* because the head (the first entry, after sorting)
         is a null entry *)
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
	if (config.print_headers) then Dynamic.print_DYNAMIC _DYNAMIC;
	if (config.print_nlist) then
	  SymbolTable.symbols_to_goblin ~libs:libraries soname symbol_table relocs
	  |> GoblinSymbol.sort_symbols ~nocompare_libs:true
	  |> List.iter
	       (GoblinSymbol.print_symbol_data ~like_nlist:true);
	if (config.verbose || config.print_libraries) then
	  begin
	    if (is_lib) then Printf.printf "Soname: %s\n" soname;
	    Printf.printf "Libraries (%d)\n" (List.length libraries);
	    List.iter (Printf.printf "\t%s\n") libraries
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
      libraries
      is_lib
      goblin_exports
      goblin_imports    

let find_export_symbol symbol binary = Goblin.get_export symbol binary.Goblin.exports
