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
  } [@@deriving show]

let sizeof_export_directory_table = 40 (* bytes *)

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
                        [@@deriving show]

let sizeof_export_address_table_entry = 4

type export_address_table = export_address_table_entry list [@@deriving show]

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
type name_pointer_table = (int [@size 4]) list [@@deriving show]

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
type export_ordinal_table = (int [@size 2]) list [@@deriving show]

let get_export_ordinal_table 
    binary offset number_of_name_pointers
  =
  let rec loop acc count o =
    if (count >= number_of_name_pointers) then
      List.rev acc
    else
      let idx,o = Binary.u16o binary o in
      loop (idx::acc) (count+1) o
  in loop [] 0 offset

type export_name_table = bytes list [@@deriving show]

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
  } [@@deriving show]

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
} [@@deriving show]

type t = synthetic_export list [@@deriving show]

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
