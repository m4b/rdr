(* 
TODO:

* verify sizes
* consider postprocessing sizes with coverage information?
* implement certificate and other data directories
 *)

open ByteCoverage
open PEHeader
open PEDataDirectories
open PESectionTable
open PEExport
open PEImport

let debug = false

let compute_import_lookup_coverage sections acc import =
  try
    match import._synthetic with
    | HintNameTableRVA (rva, entry) ->
      let r1 = PEUtils.find_offset rva sections in
      let r2 = r1 + (String.length entry.name) + 2 (* size black magic number *) in
      let extra = entry.name ^ " // Import Symbol" in
      ByteCoverage.add
        (create_data
           ~tag:Symbol
           ~r1:r1
           ~r2:r2
           ~extra:extra
           ~understood:true
        ) acc
    | OrdinalNumber ordinal ->
      acc
  with Not_found -> acc

let compute_import_entry_coverage sections acc entry =
  (* TODO: fix with auto-computed offset? *)
  let acc = try
      let r1 = PEUtils.find_offset entry.import_directory_entry.name_rva sections in
      let extra = entry._name ^ " // Library Name" in
      ByteCoverage.add
        (create_data
           ~tag:String
           ~r1:r1
           ~r2:(r1 + (String.length entry._name))
           ~extra:extra
           ~understood:true
        ) acc
    with Not_found -> acc
  in
  let r1 = PEUtils.find_offset entry.import_directory_entry.import_address_table_rva sections in
  let r2 =
    r1 +
    (List.length entry._import_address_table*(PEImport.sizeof_import_address_table_entry))
  in
  let extra = entry._name ^ " // Import Address Table" in
  let acc =
    ByteCoverage.add
      (create_data
         ~tag:Meta
         ~r1:r1
         ~r2:r2
         ~extra:extra
         ~understood:true
      ) acc
  in List.fold_left (compute_import_lookup_coverage sections) acc entry._import_lookup_table

let compute_import_data_coverage (dd:PEDataDirectories.data_directory) import_data imports sections data =
  assert (dd.virtual_address <> 0);
  try 
    let size = dd.size in
    let r1 = PEUtils.find_offset dd.virtual_address sections in
    let data =
    ByteCoverage.add
      (create_data
         ~tag:Semantic
         ~r1:r1
         ~r2:(r1 + size)
         ~extra:"Import Directory Table"
         ~understood:true
      ) data
    in
    List.fold_left (compute_import_entry_coverage sections) data import_data
  with Not_found -> data

let compute_exports_data_coverage (exports:PEExport.t) data =
  List.fold_left (fun acc (export:synthetic_export) ->
      ByteCoverage.add
        (create_data
         ~tag:Code
         ~r1:export.offset
         ~r2:(export.offset+export.size)
         ~extra:export.name
         ~understood:true
      ) acc
    ) data exports

let compute_export_data_coverage (dd:PEDataDirectories.data_directory) export_data (exports:PEExport.t) sections data =
  assert (dd.virtual_address <> 0);
  let size = dd.size in
  try
    let r1 = 
      PEUtils.find_offset dd.virtual_address sections
    in
    let r2 = r1 + size in
    let data = ByteCoverage.add
      (create_data
         ~tag:SymbolTable
         ~r1:r1
         ~r2:r2
         ~extra:"Export Table"
         ~understood:true
      ) data
    in
    let r1 =
      PEUtils.find_offset
        export_data.export_directory_table.name_pointer_rva
        sections
    in
    (* TODO: fold through the name pointers getting strings and their size to add a StringTable section *)
    let r2 =
      r1
      (* TODO: verify pointer table size is 4 *)
      + ((List.length export_data.export_name_pointer_table) * 4)
      (* bytes *)
    in
    let tag = SymbolTable in
    let extra = "Name Pointer Table" in
    let data = ByteCoverage.add
      (create_data
         ~tag:tag
         ~r1:r1
         ~r2:r2
         ~extra:extra
         ~understood:true
      ) data
    in
    let r1 =
      PEUtils.find_offset
        export_data.export_directory_table.ordinal_table_rva
        sections
    in
    let r2 =
      r1
      (* TODO: verify ordinal size is 4 *)
      + ((List.length export_data.export_ordinal_table) * 4)
    in
    let tag = SymbolTable in
    let extra = "Ordinal Table" in
    let data = ByteCoverage.add
      (create_data
         ~tag:tag
         ~r1:r1
         ~r2:r2
         ~extra:extra
         ~understood:true
      ) data
    in
    let r1 =
      PEUtils.find_offset
        export_data.export_directory_table.export_address_table_rva
        sections
    in
    let r2 =
      r1
      (* TODO: verify address table is 4 *)
      + ((List.length export_data.export_address_table) * 4)
    in
    let tag = SymbolTable in
    let extra = "Address Table" in
    ByteCoverage.add
      (create_data
         ~tag:tag
         ~r1:r1
         ~r2:r2
         ~extra:extra
         ~understood:true
      ) data
    |> compute_exports_data_coverage exports
  with Not_found -> data

let compute_data_directory_coverage
    optional_header
    export_data (exports:PEExport.t)
    import_data (imports:PEImport.t)
    sections data =
  match optional_header with
  | None ->
    data
  | Some header ->
    let dds = header.PEOptionalHeader.data_directories in
    let data =
      match export_data with
      | Some d ->
        begin
        match PEDataDirectories.get_export_table dds with
        | None ->
          data
        | Some dd ->
          compute_export_data_coverage dd d exports sections data
        end
      | _ -> data
    in
    match import_data with
    | Some d ->
      begin
        match PEDataDirectories.get_import_table dds with
        | None ->
          data
        | Some dd ->
          compute_import_data_coverage dd d imports sections data
      end
    | _ -> data

module SectionMap = Map.Make(String)

(* TODO: use characteristics for better tag resolution *)
let known_sections =
  List.fold_left (fun acc (name,key) ->
      SectionMap.add name key acc
    )
    SectionMap.empty
    [(".idata", SymbolTable);
     (".edata", SymbolTable);
     (".text", Semantic);
     (".rdata", Data);
     (".data", Data);
     (".tls", Data);
     (".reloc", Rela);
     (".rsrc", PlatformSpecific);
     (".wixburn", PlatformSpecific);
    ]

let compute_section_table_coverage sections data =
  List.fold_left (fun acc section ->
      let r1 = section.pointer_to_raw_data in
      let r2 = r1 + section.size_of_raw_data in
      let extra = "Section Table // " ^ section.name in
      if (SectionMap.mem section.name known_sections) then
        (* all semantic for now *)
        let tag = SectionMap.find section.name known_sections in
        ByteCoverage.add
          (create_data
             ~tag:tag
             ~r1:r1
             ~r2:r2
             ~extra:extra
             ~understood:true
          ) acc
      else
        ByteCoverage.add
          (create_data
             ~tag:Semantic
             ~r1:r1
             ~r2:r2
             ~extra:extra
             ~understood:true
          ) acc
    ) data sections

let compute_byte_coverage
    header size
    export_data exports
    import_data imports 
    sections binary :ByteCoverage.t =
  let dos_end = header.dos_header.pe_pointer in
  let coff_end = dos_end + PEHeader.sizeof_coff_header in
  let optional_end =
    coff_end + header.coff_header.size_of_optional_header
  in
  ByteCoverage.add
    (create_data
       ~tag:Meta
       ~r1:0
       ~r2:dos_end
       ~extra:"DOS Header"
       ~understood:true
    )
    ByteCoverage.empty
  |> ByteCoverage.add
    (create_data
       ~tag:Meta
       ~r1:dos_end
       ~r2:coff_end
       ~extra:"COFF Header"
       ~understood:true
    )
  |> ByteCoverage.add
    (create_data
       ~tag:Meta
       ~r1:coff_end
       ~r2:optional_end
       ~extra:"Optional Headers"
       ~understood:true
    )
  |> compute_data_directory_coverage
    header.optional_header
    export_data exports
    import_data imports
    sections
  |> compute_section_table_coverage sections
  |> ByteCoverage.create size binary
