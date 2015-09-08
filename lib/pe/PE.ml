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
    exports: Export.t option;
    imports: Import.t option;
  } [@@deriving (show)]

let get binary =
  let size = Bytes.length binary in
  let header = Header.get_header binary in
  let section_tables = header.Header.section_tables in
  let exports, imports =
    match header.Header.optional_header with
    | Some headers ->
       let export_data =
         if (headers.data_directories.export_table = 0) then
           None
         else
           Some (PEExport.get binary headers.data_directories section_tables) in
       let import_data = PEImport.get binary headers.data_directories section_tables in
    (*        (Some export_data),(Some import_data) *)
       export_data,(Some import_data)       
    | None ->
       None,None
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
