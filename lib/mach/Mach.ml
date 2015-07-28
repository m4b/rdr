module BindOpcodes = MachBindOpcodes
module CpuTypes = MachCpuTypes
module Fat = MachFat
module LoadCommand = MachLoadCommand
module Constants = MachConstants
(* todo remove plural to match elf? *)
module Exports = MachExports
module Header = MachHeader
module Imports = MachImports
module Segment64 = MachSegment64
module Nlist = MachNlist
module RebaseOpcodes = MachRebaseOpcodes
module Version = MachVersion

let debug = false

type t = {
  header: Header.t;
  load_commands: LoadCommand.t;
  imports: Imports.t;
  nimports: int;
  exports: Exports.t;
  nexports: int;
  nlist: Nlist.t;
  nnlist: int;
  name: string;
  is_lib: bool;
  libraries: string array;
  nlibraries: int;
  size: int;
  raw_code: bytes;
}

    (* 
TODO: REPLACE WITH Printers
let imports_to_string imports = 
  let b = Buffer.create (Array.length imports) in
  Array.fold_left (fun acc import ->
      Buffer.add_string acc
      @@ Printf.sprintf "%s"
      @@ Imports.mach_import_data_to_string import;
      acc
    ) b imports |> Buffer.contents

let exports_to_string exports =
  let b = Buffer.create (Array.length exports) in
  Array.fold_left (fun acc export ->
      Buffer.add_string acc
      @@ Printf.sprintf "%s"
      @@ Exports.mach_export_data_to_string export;
      acc
    ) b exports |> Buffer.contents

let binary_to_string binary = 
  let libstr = if (binary.is_lib) then " (LIB)" else "" in
  Printf.sprintf "%s%s:\nImports (%d):\n%sExports (%d):\n%s\n"
    binary.name libstr
    (binary.nimports)
    (imports_to_string binary.imports)
    (binary.nexports)
    (exports_to_string binary.exports)
 *)

let get binary =
  let size = Bytes.length binary in
  let header = Header.get_mach_header binary in
  let load_commands = LoadCommand.get_load_commands binary
      Header.sizeof_mach_header
      header.Header.ncmds
      header.Header.sizeofcmds
  in
  let name =
    match LoadCommand.get_lib_name load_commands with
    | Some dylib ->
      dylib.LoadCommand.lc_str
    | _ -> "" (* we're not a dylib *)
  in
  let segments = LoadCommand.get_segments load_commands in
  let libraries = LoadCommand.get_libraries load_commands name in (* TODO: watch this, with the libs.(0) *)
  (* move this inside of dyld, need the nlist info to compute locals... *)
  let is_lib = header.Header.filetype = Header.kMH_DYLIB in
  let nlist =
    try
      let symtab = LoadCommand.find_load_command LoadCommand.SYMTAB load_commands in
      Nlist.get_symbols binary symtab
    with Not_found ->
      []
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
  {
    header; load_commands; name; nlist; nnlist;
    imports; nimports; exports; nexports;
    is_lib; libraries; nlibraries; raw_code; size
  }
