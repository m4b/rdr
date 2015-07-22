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
  code: bytes;
}

let get binary name =
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
  if (not (Header.is_supported header)) then
    (* for relocs, esp /usr/lib/crt1.o *)
    {
        header;
        program_headers;
        section_headers;
        _dynamic = [];
        dynamic_symbols = [];
        symbol_table = [];
        relocations = [];
        is_lib = false;
        soname = name;
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
      with Not_found -> name (* we're not a dylib *)
    in
    let relocations =
      Dynamic.get_reloc_data _dynamic slide_sectors
      |> Reloc.get_relocs64 binary
    in
    {
        header;
        program_headers;
        section_headers;
        _dynamic;
        dynamic_symbols;
        symbol_table;
        relocations;
        is_lib;
        soname;
        libraries;
        code = Bytes.create 0;
    }
