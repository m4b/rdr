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

type export_address_table = export_address_table_entry list

(* 
let get_export_address_table binary offset =
  let rec loop acc i =
    let entry =
      get_export_address_table_entry
        binary
        (offset + (i*sizeof_export_address_table_entry))
    in
    if (is_null entry) then
      List.rev (entry::acc)
    else
      loop (entry::acc) (i+1)
  in loop [] 0
 *)
