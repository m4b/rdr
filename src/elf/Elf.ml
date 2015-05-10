open Binary
open Config

let debug = false

let create_goblin_binary filename soname libraries islib goblin_exports goblin_imports =
  let name = filename in
  let soname = soname in
  let libs = Array.of_list (soname::libraries) in (* to be consistent... for graphing, etc. *)
  let nlibs = Array.length libs in
  let exports = Array.of_list @@ List.map (GoblinSymbol.to_goblin_export) goblin_exports
  in
  let nexports = Array.length exports in
  let imports =
    Array.of_list @@ List.map (GoblinSymbol.to_goblin_import) goblin_imports
  in
  let nimports = Array.length imports in
  (* empty code *)
  let code = Bytes.empty in
  {Goblin.name; soname; islib; libs; nlibs; exports; nexports; imports; nimports; code}

let analyze config binary =
  let header = ElfHeader.get_elf_header64 binary in
  let program_headers =
    ProgramHeader.get_program_headers
      binary
      header.ElfHeader.e_phoff
      header.ElfHeader.e_phentsize
      header.ElfHeader.e_phnum
  in
  let slide_sectors = ProgramHeader.get_slide_sectors program_headers in
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
    create_goblin_binary config.filename config.filename [] false [] []
  else
    (*
    let symbol_table = SymbolTable.get_symbol_table binary section_headers in
    ignore symbol_table;
    *)
    let _DYNAMIC = Dynamic.get_DYNAMIC binary program_headers in
    (*   Printf.printf "_DYNAMIC %d\n" @@ List.length _DYNAMIC;
    Dynamic.print_DYNAMIC _DYNAMIC;
    *)
    let symtab_offset, strtab_offset, strtab_size = Dynamic.get_dynamic_symbol_offset_data _DYNAMIC in
    if (debug) then Printf.printf "0x%x 0x%x 0x%x\n" symtab_offset strtab_offset strtab_size;
    if (debug) then List.iter (ProgramHeader.print_slide_sector) slide_sectors;
    let symtab_offset = ProgramHeader.adjust slide_sectors symtab_offset in
    if (debug) then Printf.printf "adjusted symtab_offset 0x%x\n" symtab_offset;
    let strtab_offset = ProgramHeader.adjust slide_sectors strtab_offset in
    if (debug) then Printf.printf "adjusted strtab_offset 0x%x\n" strtab_offset;
    let dynamic_strtab = Dynamic.get_dynamic_strtab binary strtab_offset strtab_size in
    (*   Printf.printf "dynamic_strtab %d\n" @@ Bytes.length dynamic_strtab; *)
    let libraries = Dynamic.get_libraries _DYNAMIC dynamic_strtab in
    (*   Printf.printf "libraries: %d\n" @@ List.length libraries; flush stdout; *)
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
	Binary.string binary offset 
      with Not_found -> config.filename
    in
    let goblin_symbols =
      SymbolTable.symbols_to_goblin soname dynamic_symbols
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
	  List.iter
	    (GoblinSymbol.print_symbol_data ~like_nlist:true)
	    goblin_symbols;
	if (config.verbose || config.print_libraries) then
	  begin
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
    (* TODO: use the strippable symbol table data when available; for example, putwchar doesn't return, but calls _Unwind_Resume, a local symbol, at it's terminus instruction, byte 343 + 4 for instruction size = 347 (it's reported size - 1); *)
  
    (* ============== *)
    (* create goblin binary *)
    create_goblin_binary config.filename soname libraries (ElfHeader.is_lib header) goblin_exports goblin_imports    

let find_export_symbol symbol binary = Goblin.get_export symbol binary.Goblin.exports
