(* MACH EXPORT -> GOBLIN *)
module Exports = struct
  open MachExports

  let find symbol = 
    List.find
      (fun export ->
         (GoblinSymbol.find_symbol_name export) = symbol)

  (* a datum is a single unit, 
     so data is a list of such possible datums *)
  type mach_export_data = 
    [ 
      | GoblinSymbol.symbol_datum 
      (* we extend with mach specific details: *)
      | `Reexport of [`As of string * string | `From of string]
      | `Stub of int * int
      | `Flags of int
    ] list

  let export_info_to_mach_export_data name soname =
    function
    | MachExports.Regular symbol -> 
      [
        `Name name;
        `Offset symbol.address;
        `Kind GoblinSymbol.Export;
        `Flags symbol.flags;
        `Lib (soname, soname);
      ]
    | MachExports.Reexport symbol -> 
      begin
        match symbol.lib_symbol_name with
        | Some name' ->
          [
            `Name name;
            `Reexport (`As (name',symbol.lib));
            `PrintableData (Printf.sprintf "REEX as %s from %s" name' symbol.lib);
            `Kind GoblinSymbol.Export;
            `Flags symbol.flags;
            `Lib (soname, soname);
          ]
        | None ->
          [
            `Name name;
            `Reexport (`From symbol.lib);
            `PrintableData (Printf.sprintf "REEX from %s" symbol.lib);
            `Kind GoblinSymbol.Export;
            `Flags symbol.flags;
            `Lib (soname, soname);
          ]
      end
    | MachExports.Stub symbol ->
      [
        `Name name;
        `Stub (symbol.stub_offset, symbol.resolver_offset);
        `PrintableData (Printf.sprintf "STUB 0x%x 0x%x" symbol.stub_offset symbol.resolver_offset);
        `Kind GoblinSymbol.Export;
        `Flags symbol.flags;
        `Lib (soname, soname);
      ]

  let rec find_reexport =
    function
    | [] -> raise Not_found
    | (`Reexport _) as reex :: _->
      reex
    | _::rest -> find_reexport rest

  let rec find_stub =
    function
    | [] -> raise Not_found
    | `Stub stub :: _ ->
      stub
    | _::rest -> find_stub rest

  let rec find_flags =
    function
    | [] -> raise Not_found
    | `Flags flags :: _ ->
      flags
    | _::rest -> find_flags rest

  let mach_export_data_to_export_info data =
    try
      let reex = find_reexport data in
      begin
        match reex with
        | `Reexport (`As (name, lib)) ->
          let lib_symbol_name = Some name in
          let flags = find_flags data in
          Reexport {lib; lib_symbol_name; flags}
        | `Reexport (`From lib) ->
          let lib_symbol_name = None in
          let flags = find_flags data in
          Reexport {lib; lib_symbol_name; flags}
      end
    with Not_found ->
      try
        let stub_offset,resolver_offset = find_stub data in
        let flags = find_flags data in
        Stub {stub_offset;resolver_offset; flags}
      with Not_found ->
        let address = GoblinSymbol.find_symbol_offset data in
        let flags = find_flags data in
        Regular {address; flags}

  let mach_export_datum_to_string ?use_kind:(use_kind=true) ?use_flags:(use_flags=false) ?use_lib:(use_lib=true) datum =
    match datum with
    | #GoblinSymbol.symbol_datum as datum ->
      GoblinSymbol.symbol_datum_to_string
        ~use_kind:use_kind
        ~use_lib:use_lib
        ~use_printable:false datum
    | `Reexport `As (name,lib) ->
      Printf.sprintf "REEX as %s from %s" name lib
    | `Reexport `From lib ->
      Printf.sprintf "REEX from %s" lib
    | `Stub (i1,i2) ->
      Printf.sprintf "STUB 0x%x 0x%x" i1 i2
    | `Flags flags ->
      if (use_flags) then
        Printf.sprintf "FLAGS 0x%x" flags
      else ""

  let mach_export_datum_ordinal =
    function
    | #GoblinSymbol.symbol_datum as datum ->
      GoblinSymbol.symbol_datum_ordinal datum
    | `Reexport `As (_,_)         (* fall through *)
    | `Flags _
    | `Reexport `From _
    | `Stub _ -> GoblinSymbol.kORDINAL_RIGHT

  let sort_mach_export_data (data) =
    List.sort (fun a b ->
        let e1 = mach_export_datum_ordinal a in
        let e2 = mach_export_datum_ordinal b in
        Pervasives.compare e1 e2
      ) data

  let mach_export_data_to_string
      (* should probably be true to show REEX ? *)
      ?use_kind:(use_kind=false)
      ?use_flags:(use_flags=false)
      ?use_lib:(use_lib=true)
      (data:mach_export_data)
    =
    let data = sort_mach_export_data data in
    let b = Buffer.create ((List.length data) * 15) in
    List.iter
      (fun elem ->
         Buffer.add_string b
         @@ mach_export_datum_to_string
	   ~use_kind:use_kind
	   ~use_flags:use_flags
	   ~use_lib:use_lib
	   elem;
         Buffer.add_string b " "
      ) data;
    Buffer.contents b

  (* lessening of the data set, ignores mach specific extensions *)
  let mach_export_data_to_symbol_data list =
    List.filter
      (
        function
        (* this is sweet *)
        | #GoblinSymbol.symbol_datum as datum ->
	  ignore datum; true
        (* ignore the warning, can't use _ 
           :( we just need to see if it's a subset of goblin symbols,
           is all *)
        | _ -> false
      ) list

  let print exports = 
    Printf.printf "Exports (%d):\n" @@ List.length exports;
    List.iter 
      (fun symbol ->
         mach_export_data_to_string
           ~use_kind:false ~use_lib:false symbol
         |> Printf.printf "%s\n"
      ) exports

  let print_mach_export_data
      ?simple:(simple=false)
      ?goblin:(goblin=false)
      export
    = 
    if (not goblin) then
      mach_export_data_to_string
        ~use_lib:(not simple) export
      |> Printf.printf "%s\n"
    else
      GoblinSymbol.symbol_data_to_string
        ~basic_export:true export
      |> Printf.printf "%s\n"

  (* TODO: move this into ReadMach *)
  (* 
  |> GoblinSymbol.sort_symbols
  |> GoblinSymbol.compute_size 0x0 (* FIX THIS WITH EXTRA NLIST DATA *)
  |> List.rev (* for some reason compute size wasn't reversing... ? *)
 *)

  let empty = []

end

(* MACH IMPORT -> GOBLIN *)
module Imports = struct
  open MachImports

  type mach_import_data = 
    [ 
      | GoblinSymbol.symbol_datum 
      (* we extend with mach specific details: *)
      | `Flags of int
      | `IsLazy of bool (* this is getting too hacky *)
    ] list

  open MachLoadCommand

  let mach_import_to_goblin libraries segments ~is_lazy:is_lazy (import:bind_information) =
    let offset = (List.nth segments import.seg_index).fileoff + import.seg_offset in
    let size = if (import.bind_type == MachBindOpcodes.kBIND_TYPE_POINTER) then 8 else 0 in
    let libname = libraries.(import.symbol_library_ordinal) in
    [
      `Name import.symbol_name;
      `Offset offset;
      `Kind GoblinSymbol.Import;
      `Size size;
      `Lib (libname, libname);
      `Flags import.symbol_flags;
      `IsLazy is_lazy
    ]

  let get_imports binary dyld_info libs segments =
    let bind_off = dyld_info.MachLoadCommand.bind_off in
    let bind_size = dyld_info.MachLoadCommand.bind_size in
    let lazy_bind_off = dyld_info.MachLoadCommand.lazy_bind_off in
    let lazy_bind_size = dyld_info.MachLoadCommand.lazy_bind_size in
    let non_lazy_bytes = Bytes.sub binary bind_off bind_size in
    let lazy_bytes = Bytes.sub binary lazy_bind_off lazy_bind_size in
    let non_lazy_imports = bind_interpreter non_lazy_bytes 0 bind_size false in
    let lazy_imports = bind_interpreter lazy_bytes 0 lazy_bind_size true in
    let nl = List.map
	(mach_import_to_goblin libs segments ~is_lazy:false)
	non_lazy_imports |> GoblinSymbol.sort_symbols in
    let la = List.map
	(mach_import_to_goblin libs segments ~is_lazy:true)
	lazy_imports |> GoblinSymbol.sort_symbols in
    nl,la

  let mach_import_data_to_string (data:mach_import_data) =
    GoblinSymbol.symbol_data_to_string data

  let rec is_lazy = 
    function
    | [] -> raise Not_found
    | `IsLazy islazy :: _ -> islazy
    | _::remainder -> is_lazy remainder

  let import_name = GoblinSymbol.find_symbol_name

  let import_lib import = GoblinSymbol.find_symbol_lib import |> fst

  let print (nlas,las) = 
    let n1 = List.length nlas in
    let n2 = List.length las in
    Printf.printf "Imports (%d):\n" @@ (n1 + n2);
    Printf.printf "  Non-lazy (%d):\n" n1;
    List.iter
      (fun data ->
         GoblinSymbol.print_symbol_data ~with_lib:true data) nlas;
    Printf.printf "  Lazy (%d):\n" n2;
    List.iter
      (fun data ->
         GoblinSymbol.print_symbol_data ~with_lib:true data) las

  let print_imports_deprecated (nlas, las) = 
    let n1 = Array.length nlas in
    let n2 = Array.length las in
    Printf.printf "Imports (%d):\n" @@ (n1 + n2);
    Printf.printf "Non-lazy (%d):\n" n1;
    Array.iteri (fun i bi ->
        Printf.printf "%s\n" @@ bind_information_to_string bi) nlas;
    Printf.printf "Lazy (%d):\n" n2;
    Array.iter (fun bi ->
        Printf.printf "%s\n" @@ bind_information_to_string bi) las

  let empty = [],[]

  let find string array =
    let len = Array.length array in
    let rec loop i =
      if (i >= len) then raise Not_found
      else 
        let symbol = import_name array.(i) in
        if (string = symbol) then
          array.(i)
        else
          loop (i + 1)
    in loop 0
end

(* MACH NLIST -> GOBLIN *)
module Nlist = struct

  open MachNlist

  let nlist_flag_to_symbol_kind =
    function
    | 0xe -> GoblinSymbol.Local
    | 0xf -> GoblinSymbol.Export
    | 0x1 -> GoblinSymbol.Import
    | _ -> GoblinSymbol.Other

  let nlist_to_symbol_data (nlist, symbol) =
    let kind = `Kind (nlist_flag_to_symbol_kind @@ (nlist.MachNlist.n_type)) in
    let name = `Name (symbol) in
    let offset = `Offset (nlist.MachNlist.n_value) in
    [name; offset; kind]

  let filter_by_kind kind = List.filter (fun symbol -> try GoblinSymbol.find_symbol_kind symbol = kind with Not_found -> false)

  let print_symbols symbols =
    List.iter (GoblinSymbol.print_symbol_data ~like_nlist:true) symbols

  (* 
TODO: maps from nlist to goblin
    let goblin_symbols = List.map (nlist_to_symbol_data) symbols in
    goblin_symbols

 *)
end
