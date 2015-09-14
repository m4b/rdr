open Config
open Rdr.Utils

let debug = false

let analyze config binary =
  let elf = Elf.get ~meta_only:true binary in
  (* for consistency and display, goblin makes everything have names *)
  let soname = if (elf.Elf.soname = "") then
                 config.name else elf.Elf.soname
  in
  let goblin = Goblin.Elf.from soname elf in  
  (* print switches *)
  if (not config.silent) then
    begin
      if (not config.search) then 
        Elf.Header.print_elf_header64 elf.Elf.header;
      if (config.verbose || config.print_headers) then
	begin
          Elf.Header.print_elf_header64
            ~verbose:(config.verbose || config.print_headers)
            elf.Elf.header;
	  Elf.ProgramHeader.print_program_headers elf.Elf.program_headers;
	  Elf.SectionHeader.print_section_headers elf.Elf.section_headers;
          Elf.Dynamic.print_dynamic elf.Elf._dynamic;
          if (elf.Elf.interpreter <> "") then 
            Printf.printf "Interpreter: %s\n" elf.Elf.interpreter
	end;
      if (config.verbose || config.print_nlist) then
        Elf.SymbolTable.print elf.Elf.symbol_table;
                              (* 
	Goblin.Elf.symbols_to_goblin
          ~use_tol:config.use_tol
          ~libs:elf.Elf.libraries
          (soname,config.install_name)
          elf.Elf.symbol_table
          elf.Elf.relocations
	|> Goblin.Symbol.sort_symbols
	|> List.iter
	  (Goblin.Symbol.print_symbol_data ~like_nlist:true);
 *)
      if (config.verbose || config.print_libraries) then
	begin
	  if (elf.Elf.is_lib) then
            Printf.printf "Soname: %s\n" soname;
	  Printf.printf
            "Libraries (%d)\n"
            (List.length elf.Elf.libraries);
	  List.iter (Printf.printf "\t%s\n") elf.Elf.libraries
	end;
      if (config.verbose || config.print_exports) then
	begin
	  Printf.printf
            "Exports (%d)\n"
            (Array.length goblin.Goblin.exports);
          Goblin.print_exports goblin.Goblin.exports
	end;
      if (config.verbose || config.print_imports) then
	begin

	  Printf.printf
            "Imports (%d)\n"
            (Array.length goblin.Goblin.imports);
          Goblin.print_imports goblin.Goblin.imports
        end;
      if (config.verbose || config.print_coverage) then
        ByteCoverage.print elf.Elf.byte_coverage
    end;
  goblin
