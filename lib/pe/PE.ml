open Binary

module Header = PEHeader
module Import = PEImport
module Export = PEExport
module Characteristic = PECharacteristic
module MachineType = PEMachineType
module Utils = PEUtils
module Coverage = PEByteCoverage

open Header

(* 
let is_dll characteristics =
  let characteristic = characteristic_to_int IMAGE_FILE_DLL in
  characteristics land characteristic = characteristic
 *)

type t =
  {
    header: Header.t;
    size: int;
    export_data: Export.export_data option;
    exports: Export.t;
    nexports: int;
    import_data: Import.import_data option;
    imports: Import.t;
    nimports: int;
    libraries: string list;
    nlibraries: int;
    is_lib: bool;
    main_offset: int;
    byte_coverage: ByteCoverage.t;
  }

let pp ppf t =
  Format.fprintf ppf "@[<v>";
  Header.pp ppf t.header;
  Format.fprintf ppf "@ @ Exports(%d)@ " t.nexports;
  (* todo: add exports *)
  (* Import.pp ppf t.exports; *)
  Format.fprintf ppf "@ Imports(%d)@ " t.nimports;
  Import.pp ppf t.imports;
  Format.fprintf ppf "@ @[<v 2>Libraries(%d)@ " t.nlibraries;
  RdrUtils.Printer.pp_seq ppf RdrUtils.Printer.pp_string t.libraries;
  Format.fprintf ppf "@]";
  Format.fprintf ppf "@ ";
  ByteCoverage.pp ppf t.byte_coverage;
  Format.fprintf ppf "@ IsLib: %b" t.is_lib;
  Format.fprintf ppf "@ Main: 0x%x" t.main_offset;
  Format.fprintf ppf "@]"

let get binary =
  let size = Bytes.length binary in
  let header = Header.get_header binary in
  let section_tables = header.Header.section_tables in
  let is_lib =
    Characteristic.is_dll header.coff_header.characteristics
  in
  let export_data, exports, import_data,
      imports, libraries, main_offset =
    match header.Header.optional_header with
    | Some headers ->
      let export_data,exports =
        if (headers.data_directories.export_table = 0) then
          None,[]
        else
          let export_data = PEExport.get
              binary headers.data_directories
              section_tables
          in
          Some export_data,
          (PEExport.get_exports export_data section_tables)
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
        PEUtils.find_offset
          headers.standard_fields.address_of_entry_point
          section_tables
      in
      export_data,exports,import_data,
      imports, libraries, main_offset
    | None ->
      None,[],None,[],[],0x0
  in
  let byte_coverage =
    Coverage.compute_byte_coverage
      header size
      export_data exports
      import_data imports
      section_tables binary
  in
  {
    header;
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
    main_offset;
    byte_coverage;
  }
