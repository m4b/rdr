open Binary

module Header = PEHeader
module Import = PEImport
module Export = PEExport
module Utils = PEUtils

open Header

type t =
  {
    header: Header.t;
    size: int;
    exports: Export.export_directory_table list;
    imports: Import.import_directory_table list;
  } [@@deriving (show)]

let get binary =
  let size = Bytes.length binary in
  let header = Header.get_header binary in
  let exports, imports =
    match header.Header.optional_header with
    | Some headers ->
       let export_rva = headers.data_directories.export_table in
       let export_offset = Utils.get_offset export_rva header.Header.section_tables in
       let export_directory_table = Export.get_export_directory_table binary export_offset in
       (* 
       Printf.printf "offset export: 0x%x\n" export_offset;
        *)
       [export_directory_table],[]
    | None ->
       [],[]
  in
  {
    header;
    size;
    exports;
    imports;
    (* 
    load_commands; name; nlist; nnlist;
    imports; nimports; exports; nexports;
    is_lib; libraries; nlibraries; raw_code; size;
    byte_coverage = Coverage.compute header load_commands size;
 *)
  }
