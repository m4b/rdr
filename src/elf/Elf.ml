open Binary
       
let analyze ?nlist:(nlist=false) ~verbose ~filename binary =
  let header = ElfHeader.get_elf_header64 binary in
  let program_headers = ProgramHeader.get_program_headers binary header.ElfHeader.e_phoff header.ElfHeader.e_phentsize header.ElfHeader.e_phnum in
  let vaddr_masks = ProgramHeader.get_vaddr_masks program_headers in
  (*   List.iter (fun x -> Printf.printf "0x%x\n" x) vaddr_masks; *)
  let section_headers = SectionHeader.get_section_headers binary header.ElfHeader.e_shoff header.ElfHeader.e_shentsize header.ElfHeader.e_shnum in
  let symbol_table = SymbolTable.get_symbol_table binary section_headers in
  ignore symbol_table;
  let _DYNAMIC = Dynamic.get_DYNAMIC binary program_headers in
  let symtab_offset, strtab_offset, strtab_size = Dynamic.get_dynamic_symbol_offset_data _DYNAMIC in
  let symtab_offset = ProgramHeader.adjust vaddr_masks symtab_offset in
  let strtab_offset = ProgramHeader.adjust vaddr_masks strtab_offset in
  (*   Printf.printf "0x%x 0x%x 0x%x\n" symtab_offset strtab_offset strtab_size; *)
  let dynamic_strtab = Dynamic.get_dynamic_strtab binary strtab_offset strtab_size in
  let libraries = Dynamic.get_libraries _DYNAMIC dynamic_strtab in
  let dynamic_symbols = Dynamic.get_dynamic_symbols binary vaddr_masks symtab_offset strtab_offset strtab_size in
  let soname =
    try 
      let offset = Dynamic.get_soname_offset _DYNAMIC in
      Binary.istring binary offset 
    with Not_found -> filename
  in
  ElfHeader.print_elf_header64 header;
  ProgramHeader.print_program_headers program_headers;
  SectionHeader.print_section_headers section_headers;
  let goblin_symbols =
    SymbolTable.symbols_to_goblin soname dynamic_symbols
    |> GoblinSymbol.sort_symbols_with List.sort |> List.tl (* because the head (the first entry, after sorting) is a null entry *)
  in
  let goblin_imports = List.filter
			 (fun symbol ->
			  GoblinSymbol.find_symbol_kind symbol
			  |> function
			    | GoblinSymbol.Import -> true
			    | _ -> false) goblin_symbols
  in
  let goblin_exports = List.filter
			 (fun symbol ->
			  GoblinSymbol.find_symbol_kind symbol
			  |> function
			    | GoblinSymbol.Export -> true
			    | _ -> false) goblin_symbols
  in
  if (verbose) then
    begin
      Dynamic.print_DYNAMIC _DYNAMIC;
      if (nlist) then List.iter (GoblinSymbol.print_symbol_data ~like_nlist:true) goblin_symbols;
      Printf.printf "Libraries (%d)\n" (List.length libraries);
      List.iter (Printf.printf "\t%s\n") libraries;
      Printf.printf "Exports (%d)\n" (List.length goblin_exports);
      List.iter (GoblinSymbol.print_symbol_data ~like_export:true) goblin_exports;
      Printf.printf "Imports (%d)\n" (List.length goblin_imports);
      List.iter (GoblinSymbol.print_symbol_data ~like_export:true) goblin_imports
    end;
   (* TODO: use the strippable symbol table data when available; for example, putwchar doesn't return, but calls _Unwind_Resume, a local symbol, at it's terminus instruction, byte 343 + 4 for instruction size = 347 (it's reported size - 1); *)
   
   (* ============== *)
   (* create goblin binary *)
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
  let islib = ElfHeader.is_lib header in
  let code = Bytes.empty in
  {Goblin.name; soname; islib; libs; nlibs; exports; nexports; imports; nimports; code}
     
