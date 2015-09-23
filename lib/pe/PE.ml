open Binary

module Header = PEHeader
module SectionTable = PESectionTable
module Import = PEImport
module Export = PEExport
module Characteristic = PECharacteristic
module MachineType = PEMachineType
module Utils = PEUtils
module Coverage = PEByteCoverage

open Header
open SectionTable

type t =
  {
    header: Header.t;
    sections: SectionTable.t;
    size: int;
    export_data: Export.export_data option;
    exports: Export.t;
    nexports: int;
    import_data: Import.import_data option;
    imports: Import.t;
    nimports: int;
    libraries: string list;
    nlibraries: int;
    name: string;
    is_lib: bool;
    entry: int64;
    byte_coverage: ByteCoverage.t;
  }

(* TODO: Add export/import data? *)
let pp ppf t =
  Format.fprintf ppf "@[<v>";
  Header.pp ppf t.header;
  Format.fprintf ppf "@ @ Exports(%d)@ " t.nexports;
  Export.pp ppf t.exports;
  Format.fprintf ppf "@ Imports(%d)@ " t.nimports;
  Import.pp ppf t.imports;
  Format.fprintf ppf "@]";
  Format.fprintf ppf "@ ";
  ByteCoverage.pp ppf t.byte_coverage;
  (*
  Format.fprintf ppf "@ @[<v 2>Export Data@ ";
  match t.export_data with
  | Some data ->
    Export.pp_export_data ppf data;
  | None ->
    Format.fprintf ppf "None";
  Format.fprintf ppf "@]";
 *)
  Format.fprintf ppf "@ IsLib: %b" t.is_lib;
  Format.fprintf ppf "@ Main: 0x%Lx" t.entry;
  Format.fprintf ppf "@ Sections@ ";
  PESectionTable.pp ppf t.sections;
  Format.fprintf ppf "@]"

let show t =
  pp Format.str_formatter t;
  Format.flush_str_formatter()

(* place in RdrPrinter.pp_option *)
let print_export_data export_data =
  let ppf = Format.std_formatter in
  Format.fprintf ppf "@ @[<v 2>Export Data@ ";
  match export_data with
  | Some data ->
    Export.pp_export_data ppf data;
  | None ->
    Format.fprintf ppf "None";
  Format.fprintf ppf "@]@."

let print t =
  pp Format.std_formatter t;
  Format.print_newline()

let print_header_stub t =
  Format.printf "@[<h>PE32 %s %s@ %s@ 0x%Lx@]@."
    (PEMachineType.show_machine t.header.coff_header.machine)
    (PECharacteristic.show_type t.header.coff_header.characteristics)
    "@"
    t.entry

let get ?coverage:(coverage=true) binary =
  let size = Bytes.length binary in
  let header = Header.get_header binary in
  let section_tables = SectionTable.get binary header in
  let is_lib =
    Characteristic.is_dll header.coff_header.characteristics
  in
  let export_data, name, exports, import_data,
      imports, libraries, entry =
    match header.Header.optional_header with
    | Some headers ->
      let export_data,name,exports =
        if (headers.data_directories.export_table = 0) then
          None,"",[]
        else
          let export_data = PEExport.get
              binary headers.data_directories
              section_tables
          in
          Some export_data,
          export_data.PEExport.name,
          PEExport.get_exports binary export_data section_tables
      in
      let import_data, imports, libraries =
        if (headers.data_directories.import_table = 0) then
          None,[],[]
        else
          let import_data = PEImport.get
              binary
              headers.data_directories
              section_tables
          in
          let imports = PEImport.get_imports import_data in
          let libs = PEImport.get_libraries imports in
          Some import_data, imports, libs
      in
      let main_offset =
        try
          PEUtils.find_offset
            headers.standard_fields.address_of_entry_point
            section_tables
          |> Int64.of_int
        with Not_found -> 0x0L
      in
      export_data,name,exports,import_data,
      imports, libraries, main_offset
    | None ->
      None,"",[],None,[],[],0x0L
  in
  (* this is a performance bottleneck
     probably due to the symbol additions *)
  let byte_coverage =
    if (coverage) then
    Coverage.compute_byte_coverage
      header size
      export_data exports
      import_data imports
      section_tables binary
    else
      ByteCoverage.null
  in
  {
    header;
    sections = section_tables;
    size;
    export_data;
    import_data;
    exports;
    nexports = List.length exports;
    imports;
    nimports = List.length imports;
    libraries;
    nlibraries = List.length libraries;
    is_lib;
    name;
    entry;
    byte_coverage;
  }
