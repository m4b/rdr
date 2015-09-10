open PEHeader

let debug = false

type synthetic_import_lookup_table_entry =
  | OrdinalNumber of int [@size 16]
  | HintNameTableRVA of int [@size 31]
                            [@@deriving show]

type hint_name_table_entry = {
    hint: int [@size 2];
    name: string;
  } [@@deriving show]

type hint_name_table = hint_name_table_entry list [@@deriving show]

let get_hint_name_table_entry binary offset :hint_name_table_entry =
  let hint,o = Binary.u16o binary offset in
  let name = Binary.string binary o in
  {hint;name}

(* 32-bit *)
type import_lookup_table_entry = {
    bitfield: int [@size 32];
    _synthetic: synthetic_import_lookup_table_entry;
    _hint_name_table_entry: hint_name_table_entry option;
  } [@@deriving show]

(* 32-bit *)
type import_lookup_table = import_lookup_table_entry list [@@deriving show]

(* 32-bit *)
let get_synthetic_import_lookup_table binary offset sections :import_lookup_table_entry list =
  let rec loop acc o =
    let bitfield,o = Binary.u32o binary o in
    if (bitfield = 0) then
      begin
        if (debug) then Printf.printf "imports done\n";
        List.rev acc
      end
    else
      let _synthetic = 
        if (0x800000000 land bitfield = 1) then (* import by ordinal *)
          OrdinalNumber (0xffff land bitfield)
        else
          HintNameTableRVA (0x8ffffffff land bitfield)
      in
      let _hint_name_table_entry =
        match _synthetic with
        | HintNameTableRVA rva ->
          if (debug) then Printf.printf "searching for RVA 0x%x " rva;
          begin
          try
            let offset = PEUtils.find_offset rva sections in
            if (debug) then Printf.printf "offset 0x%x\n" offset;
            flush stdout;
            Some (get_hint_name_table_entry binary offset)
          with Not_found -> 
            begin 
              if (debug) then Printf.printf "... NONE\n";
              None 
            end
          end
        | _ -> None
      in
      let entry =
        {bitfield; _synthetic; _hint_name_table_entry}
      in
      loop (entry::acc) o
  in loop [] offset  

(* 32-bit *)
let get_import_lookup_table binary offset =
  let rec loop acc o =
    let entry,o = Binary.u32o binary o in
    if (entry = 0) then
      List.rev acc
    else
      loop (entry::acc) o
  in loop [] offset

(* where binding occurs; virtual memory addresses of imported symbols
 resolved by the loader are placed here
 *)
type import_address_table = int list [@@deriving show]

let get_import_address_table binary offset =
  let rec loop acc o =
    let entry,o = Binary.u32o binary o in
    if (entry = 0) then
      List.rev acc
    else
      loop (entry::acc) o
  in loop [] offset

type import_directory_entry = {
    import_lookup_table_rva: int [@size 4];
    time_date_stamp: int [@size 4];
    forwarder_chain: int [@size 4];
    name_rva: int [@size 4];
    import_address_table_rva: int [@size 4];
  } [@@deriving show]

type synthetic_import_directory_entry = {
    import_directory_entry: import_directory_entry;
    _name: string [@computed];
    _import_lookup_table: import_lookup_table [@computed];
    _import_address_table: import_address_table [@computed];
  } [@@deriving show]

let sizeof_import_directory_entry = 20 (* bytes *)

type import_directory_table = synthetic_import_directory_entry list [@@deriving show]

let get_import_directory_entry binary offset :import_directory_entry =
  let import_lookup_table_rva,o = Binary.u32o binary offset in
  let time_date_stamp,o = Binary.u32o binary o in
  let forwarder_chain,o = Binary.u32o binary o in
  let name_rva,o = Binary.u32o binary o in
  let import_address_table_rva = Binary.u32 binary o in
  {import_lookup_table_rva;time_date_stamp;forwarder_chain;name_rva;import_address_table_rva;}

let get_synthetic_import_directory_entry binary offset sections :synthetic_import_directory_entry =
  let import_directory_entry = get_import_directory_entry binary offset in
  let name_rva = import_directory_entry.name_rva in
  let _name = try
      let _name_offset = PEUtils.find_offset name_rva sections in
      Binary.string binary _name_offset
    with Not_found -> ""
  in
  let import_lookup_table_rva = import_directory_entry.import_lookup_table_rva in
  let import_lookup_table_offset =
    PEUtils.find_offset
      import_lookup_table_rva
      sections
  in
  let _import_lookup_table =
    get_synthetic_import_lookup_table binary import_lookup_table_offset sections
  in
  let import_address_table_offset =
    PEUtils.find_offset
      import_directory_entry.import_address_table_rva
      sections
  in
  let _import_address_table =
    get_import_address_table binary import_address_table_offset
  in
  {import_directory_entry;_name;_import_lookup_table;_import_address_table;}

let directory_entry_is_null entry =
  (entry.import_lookup_table_rva = 0) &&
    (entry.time_date_stamp = 0) &&
      (entry.forwarder_chain = 0) &&
        (entry.name_rva = 0) &&
          (entry.import_address_table_rva = 0)

let get_import_directory_table binary offset sections=
  let rec loop acc i =
    let entry =
      get_import_directory_entry
        binary
        (offset + (i*sizeof_import_directory_entry))
    in
    if (directory_entry_is_null entry) then
      begin
        if (debug) then Printf.printf "import directory table done\n"; flush stdout;
        List.rev acc
      end
    else
      let synthetic_entry =
        get_synthetic_import_directory_entry
          binary
          (offset + (i*sizeof_import_directory_entry))
          sections
      in
      loop (synthetic_entry::acc) (i+1)
  in loop [] 0

type import_data = import_directory_table [@@deriving show]

let get binary data_directories sections =
  let import_directory_table_rva = data_directories.import_table in
  let import_directory_table_offset =
    try PEUtils.find_offset import_directory_table_rva sections with
    | Not_found -> 0
  in
  let import_directory_table =
    get_import_directory_table
      binary
      import_directory_table_offset
      sections
  in
  if (debug) then Printf.printf "finished import directory table\n"; flush stdout;
  import_directory_table

type synthetic_import = {
  name: string;
  dll: string;
  ordinal: int [@size 2];
} [@@deriving show]

let get_synthetic_import dll (entry:import_lookup_table_entry) =
  let name,ordinal = match entry._hint_name_table_entry with
    | Some hint_entry ->
      hint_entry.name,hint_entry.hint (* ordinal *)
    | None ->
      match entry._synthetic with
      | OrdinalNumber ordinal ->
        "",ordinal
      | HintNameTableRVA rva ->
        begin
          Printf.eprintf "<PE.Import> warning hint/name table rva from %s without hint 0x%x\n" dll rva;
        (* this shouldn't be possible *)
          "",0
        end
  in
  {name; ordinal; dll;}

type t = synthetic_import list [@@deriving show]

let get_imports import_data :t =
  let rec loop acc entries =
    match entries with
    | [] -> List.rev @@ List.concat acc
    | entry::entries' ->
      let dll = entry._name in
      if (debug) then Printf.printf "getting imports from %s\n" dll; flush stdout;
      let imports = List.map (get_synthetic_import dll) entry._import_lookup_table in
      loop (imports::acc) entries'
  in loop [] import_data

module LibSet = Set.Make(String)

let get_libraries imports =
  List.fold_left (fun acc import ->
      LibSet.add import.dll acc
      ) LibSet.empty imports |> LibSet.elements

let i0 = Binary.list_to_bytes [0x48; 0x31; 0x00; 0x00; 0x62; 0x31; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0xf8; 0x30; 0x00; 0x00; 0x10; 0x31; 0x00; 0x00; 0xde; 0x30; 0x00; 0x00; 0x32; 0x31; 0x00; 0x00; 0xc8; 0x30; 0x00; 0x00; 0xb4; 0x30; 0x00; 0x00; 0x8c; 0x31; 0x00; 0x00; 0x96; 0x31; 0x00; 0x00; 0xb6; 0x31; 0x00; 0x00; 0xc8; 0x31; 0x00; 0x00; 0xdc; 0x31; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x84; 0x30; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x3e; 0x31; 0x00; 0x00; 0x0c; 0x30; 0x00; 0x00; 0x78; 0x30; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x80; 0x31; 0x00; 0x00; 0x00; 0x30; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x48; 0x31; 0x00; 0x00; 0x62; 0x31; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0xf8; 0x30; 0x00; 0x00; 0x10; 0x31; 0x00; 0x00; 0xde; 0x30; 0x00; 0x00; 0x32; 0x31; 0x00; 0x00; 0xc8; 0x30; 0x00; 0x00; 0xb4; 0x30; 0x00; 0x00; 0x8c; 0x31; 0x00; 0x00; 0x96; 0x31; 0x00; 0x00; 0xb6; 0x31; 0x00; 0x00; 0xc8; 0x31; 0x00; 0x00; 0xdc; 0x31; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;]

let unit1 = get_import_directory_table i0 0 
