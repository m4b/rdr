module Header = ElfHeader
module ProgramHeader = ElfProgramHeader
module SectionHeader = ElfSectionHeader
module Reloc = ElfReloc
module Constants = ElfConstants
module Dynamic = ElfDynamic                        
module SymbolTable = ElfSymbolTable
module Coverage = ElfCoverage

let debug = false

type t = {
  header: Header.t;
  program_headers: ProgramHeader.t;
  section_headers: SectionHeader.t;
  _dynamic: Dynamic.t;
  dynamic_symbols: SymbolTable.t;
  symbol_table: SymbolTable.t;
  relocations: Reloc.t;
  is_lib: bool;
  is_64: bool;
  soname: string;
  interpreter: string;
  libraries: string list;
  size: int;
  byte_coverage: ByteCoverage.t;
  entry: int64;
  raw_code: bytes;              (* list *)
}

let get ?coverage:(coverage=true) ?meta_only:(meta_only=false) binary =
  let header = Header.get_elf_header64 binary in
  let entry = Int64.of_int header.Header.e_entry in
  let is_64 = Header.is_64bit header.Header.e_ident in
  if (debug) then Header.print_elf_header64 header;
  let program_headers =
    ProgramHeader.get_program_headers
      binary
      header.Header.e_phoff
      header.Header.e_phentsize
      header.Header.e_phnum
  in
  if (debug) then ProgramHeader.print_program_headers program_headers;
  let interpreter = ProgramHeader.get_interpreter binary program_headers in
  if (debug) then Printf.printf "interpreter: %s\n" interpreter;
  let slide_sectors =
    ProgramHeader.get_slide_sectors program_headers
  in
  if (debug) then
    begin
      Printf.printf "slide sectors\n";
      ProgramHeader.print_slide_sectors slide_sectors;
    end;
  let section_headers =
    SectionHeader.get_section_headers
      binary
      header.Header.e_shoff
      header.Header.e_shentsize
      header.Header.e_shnum
  in
  if (debug) then SectionHeader.print_section_headers section_headers;
  let size = Bytes.length binary in
  if (debug) then Printf.printf "size: 0x%x\n" size;
  let is_lib = (Header.is_lib header) in
  if (debug) then Printf.printf "is_lib: %b\n" is_lib;
  let symbol_table =
    SymbolTable.get_symbol_table binary section_headers
    |> SymbolTable.sort
  in
  (*   if (debug) then SymbolTable.print_symbol_table symbol_table; *)
  let _dynamic = Dynamic.get_dynamic binary program_headers in
  (*   if (debug) then Dynamic.print_dynamic _dynamic; *)
  let symtab_offset, strtab_offset, strtab_size =
    Dynamic.get_dynamic_symbol_offset_data _dynamic slide_sectors
  in
  if (debug) then
    Printf.printf "symtab_offset: 0x%x strtab_offset: 0x%x strtab_size: 0x%x\n"
                  symtab_offset strtab_offset strtab_size;
  (* broken right here for /usr/lib/go/pkg/tool/linux_amd64/cgo *)
  let dynamic_strtab =
    Dynamic.get_dynamic_strtab binary strtab_offset strtab_size
  in
  (*   if (debug) then Printf.printf "dynamic_strtab: %s\n" dynamic_strtab; *)
  let libraries = Dynamic.get_libraries _dynamic dynamic_strtab in
  let dynamic_symbols =
    Dynamic.get_dynamic_symbols
      binary
      slide_sectors
      symtab_offset
      strtab_offset
      strtab_size
    |> SymbolTable.sort
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
  if (debug) then Reloc.print_relocs64 relocations;
  let byte_coverage =
    if (coverage) then
      ElfCoverage.compute_byte_coverage header program_headers section_headers size binary
    else
      ByteCoverage.null
  in
  (* TODO: fix *)
  let raw_code = if (meta_only) then
      Bytes.create 0 
    else
      Bytes.create 0 in
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
    is_64;
    soname;
    interpreter;
    libraries;
    raw_code;
    entry;
    byte_coverage;
  }

let print elf = 
  Header.print_elf_header64 ~verbose:true elf.header;
  ProgramHeader.print_program_headers elf.program_headers;
  SectionHeader.print_section_headers elf.section_headers;
  Dynamic.print_dynamic elf._dynamic;
  SymbolTable.print elf.dynamic_symbols;
  SymbolTable.print elf.symbol_table;
  Reloc.print_relocs64 elf.relocations;
  ByteCoverage.print elf.byte_coverage
