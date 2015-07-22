(* TODO: Consider moving this and mach's deps on goblin into goblin with from_elf, from_mach *)
(* TODO: locate code section, or rest of raw binary, should be length - (sum of headers) *)

module Header = Header
module ProgramHeader = ProgramHeader
module SectionHeader = SectionHeader
module Reloc = Reloc
module Constants = Constants
module Dynamic = Dynamic                        
module SymbolTable = SymbolTable

type t = {
  header: Header.t;
  program_headers: ProgramHeader.t;
  section_headers: SectionHeader.t;
  _dynamic: Dynamic.t;
  dynamic_symbols: SymbolTable.t;
  symbol_table: SymbolTable.t;
  relocations: Reloc.t;
  is_lib: bool;
  soname: string;
  libraries: string list;
  size: int;
  code: bytes;
}

(* TODO: locate code section, or rest of raw binary, should be length - (sum of headers) *)
let get binary =
  let header = Header.get_elf_header64 binary in
  let program_headers =
    ProgramHeader.get_program_headers
      binary
      header.Header.e_phoff
      header.Header.e_phentsize
      header.Header.e_phnum
  in
  let slide_sectors =
    ProgramHeader.get_slide_sectors program_headers
  in
  let section_headers =
    SectionHeader.get_section_headers
      binary
      header.Header.e_shoff
      header.Header.e_shentsize
      header.Header.e_shnum
  in
  let size = Bytes.length binary in
  if (not (Header.is_supported header)) then
    (* for relocs, esp /usr/lib/crt1.o *)
    {
        header;
        program_headers;
        section_headers;
        size;
        _dynamic = [];
        dynamic_symbols = [];
        symbol_table = [];
        relocations = [];
        is_lib = false;
        soname = "";
        libraries = [];
        code = Bytes.create 0;  (* TODO: fix  *)
    }
  else
    let is_lib = (Header.is_lib header) in
    let symbol_table = SymbolTable.get_symbol_table binary section_headers in
    let _dynamic = Dynamic.get_dynamic binary program_headers in
    let symtab_offset, strtab_offset, strtab_size =
      Dynamic.get_dynamic_symbol_offset_data _dynamic slide_sectors
    in
    let dynamic_strtab =
      Dynamic.get_dynamic_strtab binary strtab_offset strtab_size
    in
    let libraries = Dynamic.get_libraries _dynamic dynamic_strtab in
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
	let offset = Dynamic.get_soname_offset _dynamic in
	Binary.string binary (strtab_offset + offset)
      with Not_found -> "" (* we're not a dylib *)
    in
    let relocations =
      Dynamic.get_reloc_data _dynamic slide_sectors
      |> Reloc.get_relocs64 binary
    in
    {
        header;
        program_headers;
        section_headers;
        size;
        _dynamic;
        dynamic_symbols;
        symbol_table;
        relocations;
        is_lib;
        soname;
        libraries;
        code = Bytes.create 0;  (* TODO: fix *)
    }

(* TODO: Consider moving this and mach's deps on goblin into goblin with from_elf, from_mach *)
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
  (* TODO: empty code *)
  let code = Bytes.empty in
  {Goblin.name;
   install_name; islib; libs; nlibs; exports; nexports;
   imports; nimports; code}
