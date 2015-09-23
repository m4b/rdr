open PEHeader

let debug = false

type hint_name_table_entry = {
    hint: int [@size 2];
    name: string;
  }

let pp_hint_name_table_entry ppf h =
  Format.fprintf ppf "@[%s hint: 0x%x@]" h.name h.hint

let show_hint_name_table_entry h =
  pp_hint_name_table_entry Format.str_formatter h;
  Format.flush_str_formatter()

let get_hint_name_table_entry binary offset :hint_name_table_entry =
  let hint,o = Binary.u16o binary offset in
  let name = Binary.string binary o in
  {hint;name}

type synthetic_import_lookup_table_entry =
  | OrdinalNumber of int [@size 16]
  | HintNameTableRVA of ((int [@size 31]) * hint_name_table_entry)

let pp_synthetic_import_lookup_table_entry ppf import =
  match import with
  | OrdinalNumber ordinal -> Format.fprintf ppf "@[ordinal: 0x%x@]" ordinal
  | HintNameTableRVA (rva,entry) -> Format.fprintf ppf "@[%a@ rva: 0x%x@]" pp_hint_name_table_entry entry rva

let show_synthetic_import_lookup_table_entry entry =
  pp_synthetic_import_lookup_table_entry Format.str_formatter entry;
  Format.flush_str_formatter()

(* 32-bit *)
type import_lookup_table_entry = {
    bitfield: int [@size 32];
    _synthetic: synthetic_import_lookup_table_entry;
  }

let pp_import_lookup_table_entry ppf entry =
  Format.fprintf ppf "@[%a@ bitfield: 0x%x@]" pp_synthetic_import_lookup_table_entry entry._synthetic entry.bitfield

let show_import_lookup_table_entry entry =
  pp_import_lookup_table_entry Format.str_formatter entry;
  Format.flush_str_formatter()

(* 32-bit *)
type import_lookup_table = import_lookup_table_entry list

let pp_import_lookup_table ppf table =
  match table with
  | [] -> Format.fprintf ppf "@[None@]"
  | x::[] -> Format.fprintf ppf "@[%a@]" pp_import_lookup_table_entry x
  | x::xs ->
    Format.fprintf ppf "@[<v 2>@,%a@ " pp_import_lookup_table_entry x;
    List.iter (fun x -> Format.fprintf ppf "@,%a@ " pp_import_lookup_table_entry x) xs;
    Format.fprintf ppf "@]"

let show_pp_import_lookup_table table =
  pp_import_lookup_table Format.str_formatter table;
  Format.flush_str_formatter()

let kIMPORT_BY_ORDINAL_32 = 0x8000_0000l
let kIMPORT_RVA_MASK_32 = 0x8fff_ffffl

let import_by_ordinal_32 bitfield =
  Int32.logand kIMPORT_BY_ORDINAL_32 (Int32.of_int bitfield) = kIMPORT_BY_ORDINAL_32

let get_import_rva_32 bitfield =
  Int32.logand kIMPORT_RVA_MASK_32 (Int32.of_int bitfield) |> Int32.to_int

(* 32-bit *)
let get_synthetic_import_lookup_table binary offset sections :import_lookup_table =
  let rec loop acc o =
    let bitfield,o = Binary.u32o binary o in
    if (bitfield = 0) then
      begin
        if (debug) then Printf.printf "imports done\n";
        List.rev acc
      end
    else
      let _synthetic =
        if (import_by_ordinal_32 bitfield) then
          begin
            let ordinal = 0xffff land bitfield in
            if (debug) then Printf.printf "importing by ordinal 0x%x " ordinal;
            OrdinalNumber ordinal
          end
        else
          let rva = get_import_rva_32 bitfield in
          let hentry =
            begin
              if (debug) then Printf.printf "searching for RVA 0x%x " rva;
              try
                let offset = PEUtils.find_offset rva sections in
                if (debug) then Printf.printf "offset 0x%x\n" offset;flush stdout;
                get_hint_name_table_entry binary offset
              with Not_found ->
                begin
                  if (debug) then Printf.printf "... NONE\n";
                  {hint = 0; name = ""}
                end
            end
          in
          HintNameTableRVA (rva, hentry)
      in
      let entry =
        {bitfield; _synthetic;}
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
type import_address_table = int list

let pp_import_address_table ppf list =
  match list with
  | [] -> Format.fprintf ppf "@[[]@]"
  | x::[] -> Format.fprintf ppf "@[[0x%x]@]" x
  | x::xs ->
    Format.fprintf ppf "@[[@]@[@ 0x%x@ " x;
    List.iter (fun x -> Format.fprintf ppf "0x%x@ " x) xs;
    Format.fprintf ppf "@]@[]@]"

let sizeof_import_address_table_entry = 4 (* bytes *)

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
  }

let pp_import_directory_entry ppf entry =
  Format.fprintf ppf 
    "import_lookup_table_rva: 0x%x@ time_date_stamp: %d@ forwarder_chain: 0x%x@ name_rva: 0x%x@ import_address_table_rva: 0x%x"
    entry.import_lookup_table_rva
    entry.time_date_stamp
    entry.forwarder_chain
    entry.name_rva
    entry.import_address_table_rva

type synthetic_import_directory_entry = {
    import_directory_entry: import_directory_entry;
    _name: string [@computed];
    _import_lookup_table: import_lookup_table [@computed];
    _import_address_table: import_address_table [@computed];
  }

let pp_synthetic_import_directory_entry ppf entry =
  Format.fprintf ppf 
    "@[@[<v>%s@ @]@[<v 2>@ %a@ lookup_table:@ @[%a@]@ address_table:@ @[%a@]@]@]"
    entry._name
    pp_import_directory_entry entry.import_directory_entry
    pp_import_lookup_table entry._import_lookup_table
    pp_import_address_table entry._import_address_table

let sizeof_import_directory_entry = 20 (* bytes *)

type import_directory_table = synthetic_import_directory_entry list

let pp_import_directory_table ppf table =
  Format.fprintf ppf "Import Data:";
  match table with
  | [] -> Format.fprintf ppf "@[No Import Data@]"
  | x::[] -> Format.fprintf ppf "@[<v 2>%a@]" pp_synthetic_import_directory_entry x
  | x::xs ->
    Format.fprintf ppf "@[<v 2>@, %a@ " pp_synthetic_import_directory_entry x;
    List.iter (fun x -> Format.fprintf ppf "@, %a@ " pp_synthetic_import_directory_entry x) xs;
    Format.fprintf ppf "@]"

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

type import_data = import_directory_table

let pp_import_data ppf data =
  pp_import_directory_table ppf data

let show_import_data data =
  pp_import_data Format.str_formatter data;
  Format.flush_str_formatter()

let print_import_data data =
  pp_import_data Format.std_formatter data;
  Format.print_newline()

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

(* TODO: figure out best way to implement offset *)
type synthetic_import = {
  name: string;
  dll: string;
  ordinal: int [@size 2];
  offset: int [@size 4];
  size: int;
} [@@deriving show]

let pp_synthetic_import ppf import =
  Format.fprintf ppf "@[%16x %s -> %s[%d]@]"
    import.offset import.name import.dll import.ordinal

let show_pp_synthetic_import import =
  pp_synthetic_import Format.str_formatter import;
  Format.flush_str_formatter()

let get_synthetic_import dll import_base i (entry:import_lookup_table_entry) =
  (* let offset = try PEUtils.find_offset entry. *)
  let offset = import_base + (i * sizeof_import_address_table_entry) in
  let name,ordinal = match entry._synthetic with
    | HintNameTableRVA (rva, hint_entry) ->
      let res = hint_entry.name,hint_entry.hint (* ordinal *) in
      if (hint_entry.name = "" && hint_entry.hint = 0) then
        Printf.eprintf "<PE.Import> warning hint/name table rva from %s without hint 0x%x\n" dll rva;
      res
    | OrdinalNumber ordinal ->
      let name = Printf.sprintf "ORDINAL %d" ordinal in
      name,ordinal
  in
  {name; ordinal; dll; size = 4; offset}

type t = synthetic_import list

let pp ppf t =
  RdrPrinter.pp_seq ppf pp_synthetic_import t

let print t =
   pp Format.std_formatter t

let get_imports import_data :t =
  let rec loop acc entries =
    match entries with
    | [] -> List.rev @@ List.concat acc
    | entry::entries' ->
      let dll = entry._name in
      if (debug) then Printf.printf "getting imports from %s\n" dll; flush stdout;
      let imports = List.mapi (get_synthetic_import dll entry.import_directory_entry.import_address_table_rva) entry._import_lookup_table in
      loop (imports::acc) entries'
  in loop [] import_data

module LibSet = Set.Make(String)

let print_libraries libraries =
  let ppf = Format.std_formatter in
  Format.fprintf ppf  "@ @[<v 2>Libraries(%d)@ "
    (List.length libraries);
  RdrUtils.Printer.pp_seq
    ppf RdrUtils.Printer.pp_string libraries;
  Format.print_newline()

let get_libraries imports =
  List.fold_left (fun acc import ->
      LibSet.add import.dll acc
      ) LibSet.empty imports |> LibSet.elements

let i0 = Binary.list_to_bytes [0x48; 0x31; 0x00; 0x00; 0x62; 0x31; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0xf8; 0x30; 0x00; 0x00; 0x10; 0x31; 0x00; 0x00; 0xde; 0x30; 0x00; 0x00; 0x32; 0x31; 0x00; 0x00; 0xc8; 0x30; 0x00; 0x00; 0xb4; 0x30; 0x00; 0x00; 0x8c; 0x31; 0x00; 0x00; 0x96; 0x31; 0x00; 0x00; 0xb6; 0x31; 0x00; 0x00; 0xc8; 0x31; 0x00; 0x00; 0xdc; 0x31; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x84; 0x30; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x3e; 0x31; 0x00; 0x00; 0x0c; 0x30; 0x00; 0x00; 0x78; 0x30; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x80; 0x31; 0x00; 0x00; 0x00; 0x30; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x48; 0x31; 0x00; 0x00; 0x62; 0x31; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0xf8; 0x30; 0x00; 0x00; 0x10; 0x31; 0x00; 0x00; 0xde; 0x30; 0x00; 0x00; 0x32; 0x31; 0x00; 0x00; 0xc8; 0x30; 0x00; 0x00; 0xb4; 0x30; 0x00; 0x00; 0x8c; 0x31; 0x00; 0x00; 0x96; 0x31; 0x00; 0x00; 0xb6; 0x31; 0x00; 0x00; 0xc8; 0x31; 0x00; 0x00; 0xdc; 0x31; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;]

let unit1 = get_import_directory_table i0 0 
