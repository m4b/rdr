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
  (* todo make this goblin independent by adding the to_goblin step in the readelf analyzer *)
  imports: Imports.mach_import_data array;
  nimports: int;
  exports: Exports.mach_export_data array;
  nexports: int;
  nlist: Nlist.t;
  nnlist: int;
  name: string;
  install_name: string;
  is_lib: bool;
  libraries: string array;
  nlibraries: int;
  size: int;
  raw_header: bytes;
  raw_code: bytes;
  raw_dyldinfo: bytes;
}

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


(* TODO: add header
let create_binary (name,install_name) (nls,las) exports islib libs =
  (* flatten and condense import info *)
  let imports = nls @ las |> Array.of_list in
  let nimports = Array.length imports in
  let exports = Array.of_list exports in
  let nexports = Array.length exports in (* careful here, due to aliasing, if order swapped, in trouble *)
  let nlibs = Array.length libs in
  let code = Bytes.empty in
  {name; install_name; imports; nimports; exports; nexports; islib; libs; nlibs; code}
 *)
