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
    imports: Import.import_directory_table list;
  } [@@deriving (show)]

let get binary =
  let size = Bytes.length binary in
  let header = Header.get_header binary in
  let section_tables = header.Header.section_tables in
  let exports, imports =
    match header.Header.optional_header with
    | Some headers ->
       let export_data = PEExport.get binary headers.data_directories section_tables in
       (*
       let export_rva = headers.data_directories.export_table in
       let export_offset =
         Utils.get_offset export_rva section_tables
       in
       let export_directory_table =
         Export.get_export_directory_table binary export_offset
       in
       let export_name_table_offset =
         Utils.get_offset
           export_directory_table.Export.name_rva
           section_tables
       in
       let nexports =
         export_directory_table.Export.number_of_name_pointers
       in
       Printf.printf "name table offset 0x%x\n" export_name_table_offset;
       let export_name_table =
         Export.get_export_name_table
           binary
           nexports
           export_name_table_offset
       in
        *)
       let import_rva = headers.data_directories.import_table in
       let import_offset =
         Utils.get_offset import_rva section_tables
       in
       let import_directory_table =
         Import.get_import_directory_table binary import_offset
       in
       (* Printf.printf "%s\n" @@ Export.show_export_name_table export_name_table; *)
       (* 
       Printf.printf "offset export: 0x%x\n" export_offset;
       *)
       (Some export_data),[import_directory_table]
    | None ->
       None,[]
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
