open PEHeader

type export_directory_table = {
    export_flags: int [@size 4];
    time_date_stamp: int [@size 4];
    major_version: int [@size 2];
    minor_version: int [@size 2];
    name_rva: int [@size 4];
    ordinal_base: int [@size 4];
    address_table_entries: int [@size 4];
    number_of_name_pointers: int [@size 4];
    export_address_table_rva: int [@size 4];
    name_pointer_rva: int [@size 4];
    ordinal_table_rva: int [@size 4];
  }

let sizeof_export_directory_table = 40 (* bytes *)

let pp_export_directory_table ppf table =
  Format.fprintf ppf "ExportFlags: 0x%x@ TimeDateStamp: %d@ MajorVersion: %d@ MinorVersion: %d@ NameRVA: 0x%x@ OrdinalBase: 0x%x@ AddressTableEntries: %d@ NumberOfNamePointers: %d@ ExportAddressTableRVA: 0x%x@ NamePointerRVA: 0x%x@ OrdinalTableRVA: 0x%x"
    table.export_flags
    table.time_date_stamp
    table.major_version
    table.minor_version
    table.name_rva
    table.ordinal_base
    table.address_table_entries
    table.number_of_name_pointers
    table.export_address_table_rva
    table.name_pointer_rva
    table.ordinal_table_rva

let show_export_directory_table table =
  pp_export_directory_table Format.str_formatter table;
  Format.flush_str_formatter()

let get_export_directory_table binary offset :export_directory_table =
  let export_flags,o = Binary.u32o binary offset in
  let time_date_stamp,o = Binary.u32o binary o in
  let major_version,o = Binary.u16o binary o in
  let minor_version,o = Binary.u16o binary o in
  let name_rva,o = Binary.u32o binary o in
  let ordinal_base,o = Binary.u32o binary o in
  let address_table_entries,o = Binary.u32o binary o in
  let number_of_name_pointers,o = Binary.u32o binary o in
  let export_address_table_rva,o = Binary.u32o binary o in
  let name_pointer_rva,o = Binary.u32o binary o in
  let ordinal_table_rva = Binary.u32 binary o in
  {export_flags;time_date_stamp;major_version;minor_version;name_rva;ordinal_base;address_table_entries;number_of_name_pointers;export_address_table_rva;name_pointer_rva;ordinal_table_rva;}

type export_address_table_entry =
  | ExportRVA of int [@size 4]
  | ForwarderRVA of int [@size 4]

let sizeof_export_address_table_entry = 4

let pp_export_address_table_entry ppf entry =
  match entry with
  | ExportRVA rva ->
    Format.fprintf ppf "Export 0x%x" rva
  | ForwarderRVA rva ->
    Format.fprintf ppf "Forwarder 0x%x" rva

type export_address_table = export_address_table_entry list

let pp_export_address_table ppf table =
  RdrUtils.Printer.pp_seq ppf pp_export_address_table_entry table

let get_export_address_table binary offset address_table_entries =
  let rec loop acc count o =
    if (count >= address_table_entries) then
      List.rev acc
    else
      let pointer,o = Binary.u32o binary o in
      loop ((ExportRVA pointer)::acc) (count+1) o
  in loop [] 0 offset

(* array of rvas into the export name table; 
   export name is defined iff pointer table has pointer to the name*)
type name_pointer_table = (int [@size 4]) list

let pp_name_pointer_table ppf table =
  RdrUtils.Printer.pp_h ppf RdrUtils.Printer.pp_hex table

let get_name_pointer_table binary offset number_of_name_pointers =
  let rec loop acc count o =
    if (count >= number_of_name_pointers) then
      List.rev acc
    else
      let pointer,o = Binary.u32o binary o in
      loop (pointer::acc) (count+1) o
  in loop [] 0 offset

(* array of indexes into the export addres table *)
(* idx = ordinal - ordinalbase *)
type export_ordinal_table = (int [@size 2]) list

let pp_export_ordinal_table ppf table =
  RdrUtils.Printer.pp_h ppf RdrUtils.Printer.pp_hex table

let get_export_ordinal_table 
    binary offset number_of_name_pointers =
  let rec loop acc count o =
    if (count >= number_of_name_pointers) then
      List.rev acc
    else
      let idx,o = Binary.u16o binary o in
      loop (idx::acc) (count+1) o
  in loop [] 0 offset

type export_name_table = bytes list

let pp_export_name_table ppf table =
  RdrUtils.Printer.pp_h ppf RdrUtils.Printer.pp_string table

let get_export_name_table binary nexports offset =
  let rec loop acc count current =
    let name,o = Binary.stringo binary current in
    if (count >= nexports) then
      List.rev acc
    else
      loop (name::acc) (count+1) o
  in loop [] 0 offset

type export_data =
  {
    export_directory_table: export_directory_table;
    name_pointer_table: name_pointer_table;
    export_ordinal_table: export_ordinal_table;
    export_address_table: export_address_table;
    export_name_table: export_name_table;
  }

let pp_export_data ppf data =
  Format.fprintf ppf "@[<v 2>Export Directory Table";
  Format.fprintf ppf
    "@ %a" pp_export_directory_table data.export_directory_table;
  Format.fprintf ppf "@]";
  Format.fprintf ppf "@ @[<v 2>Name Pointer Table";
  Format.fprintf ppf
    "@ %a" pp_name_pointer_table data.name_pointer_table;
  Format.fprintf ppf "@]";
  Format.fprintf ppf "@ @[<v 2>Export Ordinal Table";
  Format.fprintf ppf
    "@ %a" pp_export_ordinal_table data.export_ordinal_table;
  Format.fprintf ppf "@]";
  Format.fprintf ppf "@ @[<v 2>Export Address Table";
  Format.fprintf ppf
    "@ %a" pp_export_address_table data.export_address_table;
  Format.fprintf ppf "@]";
  Format.fprintf ppf "@ @[<v 2>Export Name Table";
  Format.fprintf ppf
    "@ %a" pp_export_name_table data.export_name_table;
  Format.fprintf ppf "@]@]"

let show_export_data data =
  pp_export_data Format.str_formatter data;
  Format.flush_str_formatter()

let print_export_data data =
  pp_export_data Format.std_formatter data;
  Format.print_newline()

let get binary data_directories section_tables =
  let export_rva = data_directories.export_table in
  let export_offset =
    PEUtils.find_offset export_rva section_tables
  in
  let export_directory_table =
    get_export_directory_table binary export_offset
  in
  let number_of_name_pointers =
    export_directory_table.number_of_name_pointers
  in
  let address_table_entries =
    export_directory_table.address_table_entries
  in
  let name_pointer_table_offset =
    PEUtils.find_offset
      export_directory_table.name_pointer_rva
      section_tables
  in
  let export_address_table_offset =
    PEUtils.find_offset
      export_directory_table.export_address_table_rva
      section_tables
  in
  let export_ordinal_table_offset =
    PEUtils.find_offset
      export_directory_table.ordinal_table_rva
      section_tables
  in  
  let export_name_table_offset =
    PEUtils.find_offset
      export_directory_table.name_rva
      section_tables
  in
  (*   Printf.printf "name table offset 0x%x\n" export_name_table_offset; *)
  let name_pointer_table =
    get_name_pointer_table
      binary
      name_pointer_table_offset
      number_of_name_pointers
  in
  let export_ordinal_table =
    get_export_ordinal_table
      binary
      export_ordinal_table_offset
      number_of_name_pointers    
  in
  let export_address_table =
    get_export_address_table
      binary
      export_address_table_offset
      address_table_entries
  in
  let export_name_table =
    get_export_name_table
      binary
      number_of_name_pointers
      export_name_table_offset
  in
  {
    export_directory_table;
    name_pointer_table;
    export_ordinal_table;
    export_address_table;
    export_name_table;
  }

type synthetic_export = {
  name: string;
  offset: int;
  size: int;
}

let pp_synthetic_export ppf export =
  Format.fprintf ppf "%16x %s (%d)"
    export.offset export.name export.size

let show_synthetic_export export =
  pp_synthetic_export Format.str_formatter export;
  Format.flush_str_formatter()

let print_synthetic_export export =
  pp_synthetic_export Format.std_formatter export

type t = synthetic_export list

let pp ppf t =
  RdrUtils.Printer.pp_seq ppf pp_synthetic_export t

let show t =
  pp Format.str_formatter t;
  Format.flush_str_formatter()

let print t =
  let ppf = Format.std_formatter in
  Format.fprintf ppf "@ @ Exports(%d)@ " (List.length t);
  pp Format.std_formatter t;
  Format.print_newline()

let sort =
  List.sort (fun ex1 ex2 -> 
      Pervasives.compare ex1.offset ex2.offset
    ) 

[@@@invariant sorted]
let compute_size exports =
  let rec loop acc exports =
    match exports with
    | [] ->
      List.rev acc
    | ex1::[] ->
      List.rev (ex1::acc)
    | ex1::((ex2::_) as exports) ->
      loop
        ({ex1 with size = (ex2.offset - ex1.offset)}::acc)
        exports
  in loop [] exports

let get_exports export_data sections :t =
  let names = export_data.export_name_table in
  let addresses = export_data.export_address_table in
  List.mapi (fun i name ->
      let offset = 
        match (List.nth addresses i) with 
        | ExportRVA rva ->
          PEUtils.find_offset rva sections
        | ForwarderRVA rva ->
          rva
      in
      {name; offset; size = 0}
    ) names |> sort |> compute_size
