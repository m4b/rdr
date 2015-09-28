type data_directory =
  {
    virtual_address: int [@size 4];
    size: int [@size 4];
  }

let sizeof_data_directory = 8

let get_data_directory binary offset =
  let virtual_address,o = Binary.u32o binary offset in
  let size,_ = Binary.u32o binary o in
  {virtual_address; size}

type t = data_directory list

let get binary offset count =
  let rec loop acc offset i =
    if (i >= count) then
      List.rev acc
    else
      let entry = get_data_directory binary offset in
      loop (entry::acc) (offset + sizeof_data_directory) (i+1)
  in loop [] offset 0

let get_export_table (t:t) =
  try Some (List.nth t 0) with Failure _ -> None

let get_import_table (t:t) =
  try Some (List.nth t 1) with Failure _ -> None

let get_resource_table (t:t) =
  try Some (List.nth t 2) with Failure _ -> None

let get_exception_table (t:t) =
  try Some (List.nth t 3) with Failure _ -> None

let get_certificate_table (t:t) =
  try Some (List.nth t 4) with Failure _ -> None

let get_base_relocation_table (t:t) =
  try Some (List.nth t 5) with Failure _ -> None

let get_debug_table (t:t) =
  try Some (List.nth t 6) with Failure _ -> None

let get_architecture (t:t) =
  try Some (List.nth t 7) with Failure _ -> None

let get_global_ptr (t:t) =
  try Some (List.nth t 8) with Failure _ -> None

let get_tls_table (t:t) =
  try Some (List.nth t 9) with Failure _ -> None

let get_load_config_table (t:t) =
  try Some (List.nth t 10) with Failure _ -> None

let get_bound_import_table (t:t) =
  try Some (List.nth t 11) with Failure _ -> None

let get_import_address_table (t:t) =
  try Some (List.nth t 12) with Failure _ -> None

let get_delay_import_descriptor (t:t) =
  try Some (List.nth t 13) with Failure _ -> None

let get_clr_runtime_header (t:t) =
  try Some (List.nth t 14) with Failure _ -> None

let pp ppf t =
  Format.fprintf ppf "@[<v 2>Data Directories@ ";
  if (t = []) then
    begin
      Format.fprintf ppf "None@]";
    end
  else
    begin
      List.iteri (fun i dd ->
          match i with
          | i when i = 0 ->
            Format.fprintf ppf
              "ExportTable: 0x%x@ SizeOfExportTable: 0x%x"
              dd.virtual_address dd.size
          | i when i = 1 ->
            Format.fprintf ppf
              "@ ImportTable: 0x%x@ SizeOfImportTable: 0x%x"
              dd.virtual_address dd.size
          | i when i = 2 ->
            Format.fprintf ppf
              "@ ResourceTable: 0x%x@ SizeOfResourceTable: 0x%x"
              dd.virtual_address dd.size
          | i when i = 3 ->
            Format.fprintf ppf
              "@ ExceptionTable: 0x%x@ SizeOfExceptionTable: 0x%x"
              dd.virtual_address dd.size
          | i when i = 4 ->
            Format.fprintf ppf
              "@ CertificateTable: 0x%x@ SizeOfCertificateTable: 0x%x"
              dd.virtual_address dd.size
          | i when i = 5 ->
            Format.fprintf ppf
              "@ BaseRelocationTable: 0x%x@ SizeOfBaseRelocationTable: 0x%x"
              dd.virtual_address dd.size
          | i when i = 6 ->
            Format.fprintf ppf
              "@ Debug: 0x%x@ SizeOfDebugTable: 0x%x"
              dd.virtual_address dd.size
          | i when i = 7 ->
            Format.fprintf ppf
              "@ Architecture: 0x%x@ SizeOfArchitecture: 0x%x"
              dd.virtual_address dd.size
          | i when i = 8 ->
            Format.fprintf ppf
              "@ GlobalPtr: 0x%x@ SizeOfGlobalPtr: 0x%x"
              dd.virtual_address dd.size
          | i when i = 9 ->
            Format.fprintf ppf
              "@ TlsTable: 0x%x@ SizeOfTlsTable: 0x%x"
              dd.virtual_address dd.size
          | i when i = 10 ->
            Format.fprintf ppf
              "@ LoadConfigTable: 0x%x@ SizeOfLoadConfigTable: 0x%x"
              dd.virtual_address dd.size
          | i when i = 11 ->
            Format.fprintf ppf
              "@ BoundImport: 0x%x@ SizeOfBoundImport: 0x%x"
              dd.virtual_address dd.size
          | i when i = 12 ->
            Format.fprintf ppf
              "@ ImportAddressTable: 0x%x@ SizeOfImportAddressTable: 0x%x"
              dd.virtual_address dd.size
          | i when i = 13 ->
            Format.fprintf ppf
              "@ DelayImportDescriptor: 0x%x@ SizeOfDelayImportDescriptor: 0x%x"
              dd.virtual_address dd.size
          | i when i = 14 ->
            Format.fprintf ppf
              "@ ClrRuntimeHeader: 0x%x@ SizeOfClrRuntimeHeader: 0x%x"
              dd.virtual_address dd.size
          | i when i = 15 ->
            Format.fprintf ppf
              "@ Reserved: 0x%x@ SizeOfReserved: 0x%x@ " 
              dd.virtual_address dd.size
          | _ ->
            ()
        ) t;
      Format.fprintf ppf "@]"
    end

let show t =
  pp Format.str_formatter t;
  Format.flush_str_formatter()

let print t =
  pp Format.std_formatter t
