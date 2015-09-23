module BindOpcodes = MachBindOpcodes
module CpuTypes = MachCpuTypes
module Fat = MachFat
module LoadCommand = MachLoadCommand
module Constants = MachConstants
(* todo remove plural to match elf? *)
module Exports = MachExports
module Header = MachHeader
module Imports = MachImports
module Section = MachSection
module SymbolTable = MachSymbolTable
module RebaseOpcodes = MachRebaseOpcodes
module Version = MachVersion
module Coverage = MachCoverage

open LoadCommand.Types

let debug = false

type t = {
  header: Header.t;
  load_commands: LoadCommand.t;
  imports: Imports.t;
  nimports: int;
  exports: Exports.t;
  nexports: int;
  nlist: SymbolTable.t;
  nnlist: int;
  name: string;
  is_lib: bool;
  libraries: string array;
  nlibraries: int;
  size: int;
  raw_code: bytes;
  entry: int64;
  byte_coverage: ByteCoverage.t;
}

let binary_to_string binary =
  let libstr = if (binary.is_lib) then " (LIB)" else "" in
  Printf.sprintf "%s%s:\nImports (%d):\n%sExports (%d):\n%s"
    binary.name libstr
    (binary.nimports)
    (Imports.imports_to_string binary.imports)
    (binary.nexports)
    (Exports.exports_to_string binary.exports)

let print binary = 
  Printf.printf "%s" @@ binary_to_string binary;
  ByteCoverage.print binary.byte_coverage

let get binary =
  let size = Bytes.length binary in
  let header = Header.get_mach_header binary in
  let load_commands = LoadCommand.get_load_commands binary
      Header.sizeof_mach_header
      header.Header.ncmds
      header.Header.sizeofcmds
  in
  let entry = LoadCommand.get_entry load_commands |> Int64.of_int in
  let name = LoadCommand.get_lib_name load_commands (* if "" we're not a dylib *)
  in
  let segments = LoadCommand.get_segments load_commands in
  let libraries = LoadCommand.get_libraries load_commands name in (* TODO: watch this, with the libs.(0) *)
  (* move this inside of dyld, need the nlist info to compute locals... *)
  let is_lib = header.Header.filetype = Header.kMH_DYLIB in
  let nlist =
    match LoadCommand.get_load_command LC_SYMTAB load_commands with
    | Some (LC_SYMTAB symtab) ->
      SymbolTable.get_symbols binary symtab
    | _ -> []
  in
  let dyld_info = LoadCommand.get_dyld_info load_commands in
  let exports, imports =
    match dyld_info with
    | Some dyld_info ->
      (* TODO: add load segment boundaries, and nlists locals as a parameters *)
      let exports = 
        Exports.get_exports binary dyld_info libraries
      in
      (* TODO: yea, need to fix imports like machExports; send in the libraries,
         do all that preprocessing there, and not in create binary *)
      let imports =
        Imports.get_imports
          binary dyld_info libraries segments
      in
      exports,imports
    | None ->
      [],[]
  in
  let nnlist = List.length nlist in
  let nimports = List.length imports in
  let nexports = List.length exports in
  let nlibraries = Array.length libraries in
  let raw_code = Bytes.empty in
  (* TODO: add bytecoverage computer *)
  {
    header; load_commands; name; nlist; nnlist;
    imports; nimports; exports; nexports;
    is_lib; libraries; nlibraries; raw_code; size;
    byte_coverage = Coverage.compute header load_commands size binary;
    entry;
  }
