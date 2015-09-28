(* 

scan string for beef_maximum from libbeef.dll: 5589e58b450839450c0f4d450c
`beef_maximum` must have final computed offset of 0x510 in libbeef.dll, _NOT_ the current value of 0x51f.
this is verified via `gdb /r beef_maximum` on compute stick with cygwin to get the bytes, and --scan on linux box showing the bytes location for that exact function/byte sequence.

 this symbol in msvcrt.dll seems to have a -1 ordinal: ??0__non_rtti_object@@QAE@ABV0@@Z
 atol -> ntdll.dll[2073]
 RtlCreateUserStack -> ntdll.dll[782] GOT 796
 strchr -> ntdll.dll[2121] GOT 2130
 RtlFreeSid -> ntdll.dll[920] GOT 933
*)

open PEHeader
open PEDataDirectories

let debug = false

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
  Format.fprintf ppf "ExportFlags: 0x%x@ TimeDateStamp: %d@ MajorVersion: %d@ MinorVersion: %d@ NameRVA: 0x%x@ OrdinalBase: %d@ AddressTableEntries: %d@ NumberOfNamePointers: %d@ ExportAddressTableRVA: 0x%x@ NamePointerRVA: 0x%x@ OrdinalTableRVA: 0x%x"
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

let get_export_address_table binary offset base export_begin size address_table_entries sections =
  let export_end = export_begin + size in
  let rec loop acc count o =
    if (count >= (address_table_entries)) then
      List.rev acc
    else
      let rva,o = Binary.u32o binary o in
      (*
      let one = PEUtils.find_offset rva sections in
      let two = PEUtils.find_offset export_begin sections in
      let three = two + size in

      Printf.printf "rva: 0x%x 0x%x - 0x%x\n" rva export_begin export_end;
      Printf.printf "addr: 0x%x 0x%x - 0x%x\n" one two three;
      *)
      if (not @@ PEUtils.is_in_range rva export_begin export_end) then
        loop ((ExportRVA rva)::acc) (count+1) o
      else
        begin
          (* Printf.printf "FOUND 0x%x\n" rva; *)
          loop ((ForwarderRVA rva)::acc) (count+1) o
        end
  in loop [] 0 offset

(* array of rvas into the export name table; 
   export name is defined iff pointer table has pointer to the name*)
type export_name_pointer_table = (int [@size 4]) list

let pp_export_name_pointer_table ppf table =
  RdrUtils.Printer.pp_h ppf RdrUtils.Printer.pp_hex table

let get_export_name_pointer_table binary offset number_of_name_pointers =
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

type export_data =
  {
    export_directory_table: export_directory_table;
    export_name_pointer_table: export_name_pointer_table;
    export_ordinal_table: export_ordinal_table;
    export_address_table: export_address_table;
    name: string;
  }

let pp_export_data ppf data =
  Format.fprintf ppf "@[<v 2>Export Directory Table";
  Format.fprintf ppf
    "@ %a" pp_export_directory_table data.export_directory_table;
  Format.fprintf ppf "@]";
  Format.fprintf ppf "@ @[<v 2>Name Pointer Table";
  Format.fprintf ppf
    "@ %a" pp_export_name_pointer_table data.export_name_pointer_table;
  Format.fprintf ppf "@]";
  Format.fprintf ppf "@ @[<v 2>Export Ordinal Table";
  Format.fprintf ppf
    "@ %a" pp_export_ordinal_table data.export_ordinal_table;
  Format.fprintf ppf "@]";
  Format.fprintf ppf "@ @[<v 2>Export Address Table";
  Format.fprintf ppf
    "@ %a" pp_export_address_table data.export_address_table;
  Format.fprintf ppf "@]"

let show_export_data data =
  pp_export_data Format.str_formatter data;
  Format.flush_str_formatter()

let print_export_data data =
  pp_export_data Format.std_formatter data;
  Format.print_newline()

let get binary dd section_tables =
  let export_rva = dd.virtual_address in
  Printf.printf "0x%x \n" export_rva; flush stdout;
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
  let name_offset =
    PEUtils.find_offset
      export_directory_table.name_rva
      section_tables
  in
  if (debug) then 
    Printf.printf "<PEExport.get> pointers: 0x%x  ordinals: 0x%x addresses: 0x%x\n"
      name_pointer_table_offset export_ordinal_table_offset export_address_table_offset;
  let export_name_pointer_table =
    get_export_name_pointer_table
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
      export_directory_table.ordinal_base
      export_rva
      dd.size
      address_table_entries
      section_tables
  in
  let name =
    Binary.string binary name_offset
  in
  {
    export_directory_table;
    export_name_pointer_table;
    export_ordinal_table;
    export_address_table;
    name;
  }

type reexport =
  | DLLName of string * string
  | DLLOrdinal of string * int

let get_reexport string =
  try
    let i = String.index string '.' in
    let dll = String.sub string 0 i in
    let len = (String.length string) - i - 1 in
    let rest = String.sub string (i+1) len in
    match rest.[0] with
    | '#' ->
      let len = (String.length rest) - 1 in
      let ordinal = int_of_string @@ String.sub rest 1 len in
      DLLOrdinal (dll, ordinal)
    | _ -> DLLName (dll, rest)
  with _ -> DLLName ("<PEExport.reexport> bad reexport",string)

type synthetic_export = {
  name: string;
  offset: int;
  rva: int;
  size: int;
  reexport: reexport option;
}

let pp_synthetic_export ppf export =
  Format.fprintf ppf "%16x %s (%d)"
    export.offset export.name export.size;
  match export.reexport with
  | None -> ()
  | Some reexport ->
    Format.fprintf ppf " => ";
    begin
      match reexport with
      | DLLName (lib,name) ->
        Format.fprintf ppf "@[<h 2>%s.%s@]" lib name
      | DLLOrdinal (lib,ord) ->
        Format.fprintf ppf "@[<h 2>%s[%d]@]" lib ord
    end

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

let get_exports binary export_data sections :t =

  let pointers = export_data.export_name_pointer_table in
  let addresses = export_data.export_address_table in
  let ordinals = export_data.export_ordinal_table in
  let ordinal_base = export_data.export_directory_table.ordinal_base in
  List.mapi (fun i ptr ->
      let name,offset,rva,reexport =
        (* 
        Printf.printf "i: %d ptr: 0x%x \n" i ptr;
        *)
        let name_offset = PEUtils.find_offset ptr sections in
        let name = Binary.string binary name_offset in
        let ordinal = List.nth ordinals i in
        let address_index = ordinal in
        if (debug) then
          Printf.printf "name: %s name_offset: 0x%x ordinal: %d address_index: %d "
            name name_offset ordinal address_index;
        if (address_index < 0 || (address_index >= List.length addresses)) then
          begin
            Printf.eprintf "<PEExport.get_export> bad index for %s: %d %d %d len: %d\n"
              name (i+ordinal_base) ordinal address_index (List.length addresses);
            name,0x0,0x0,None
          end
        else
          begin
            match (List.nth addresses address_index) with
            | ExportRVA rva ->
              let offset = PEUtils.find_offset rva sections in
              if (debug) then Printf.printf "0x%x\n" offset;
              name,offset,rva,None

            | ForwarderRVA rva ->
              let stroffset = PEUtils.find_offset rva sections in
              (* Printf.printf "stroffset 0x%x\n" stroffset; *)
              let string = Binary.string binary stroffset in
              (* Printf.printf "string %s\n" string; *)
              name,rva,rva,Some(get_reexport string)
          end
      in
      {name; offset; rva; reexport; size = 0}
     ) pointers |> sort |> compute_size
