(* 
TODO:

* verify sizes
* implement import
* implement certificate and other data directories
 *)

open ByteCoverage
open PEHeader
open PEExport

let debug = false

(* TODO: implement import data coverage *)
let compute_import_data_coverage data_directory export_data sections data =
  data

let compute_exports_data_coverage exports data =
  List.fold_left (fun acc export ->
      ByteCoverage.add
        (create_data
         ~tag:Code
         ~r1:export.offset
         ~r2:(export.offset+export.size)
         ~extra:export.name
         ~understood:true
      ) acc
    ) data exports

let compute_export_data_coverage data_directory export_data exports sections data =
  assert (data_directory.export_table <> 0);
  let size = data_directory.size_of_export_table in
  try
    let r1 = 
      PEUtils.find_offset data_directory.export_table sections
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
        export_data.export_directory_table.name_rva
        sections
    in
    let r2 =
      r1
      + (List.fold_left (fun acc name ->
          (String.length name) + acc
        )
          0 export_data.export_name_table)
    in
    let tag = StringTable in
    let extra = "Name Table" in
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
        export_data.export_directory_table.name_pointer_rva
        sections
    in
    let r2 =
      r1
      (* TODO: verify pointer table size is 4 *)
      + ((List.length export_data.name_pointer_table) * 4)
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
        export_data.export_directory_table.name_pointer_rva
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

let compute_data_directory_coverage optional_header export_data exports import_data sections data =
  match optional_header with
  | None ->
    data
  | Some header ->
    let dd = header.data_directories in
    let data =
      match export_data with
      | Some d ->
        compute_export_data_coverage dd d exports sections data
      | _ -> data
    in
    match import_data with
    | Some d ->
      compute_import_data_coverage dd d sections data
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
        let tag = SectionMap.find section.name known_sections in
        ByteCoverage.add
          (create_data
             ~tag:tag (* TODO: 
                         we need to add semantic tag,
                         but lose so much understanding when so *)
             ~r1:r1
             ~r2:r2
             ~extra:extra
             ~understood:true
          ) acc
      else
        (* TODO: 
           remove this hack for understanding
           if we can get more granularity *)
        ByteCoverage.add
          (create_data
             ~tag:Meta
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
    export_data exports import_data
    sections
  |> compute_section_table_coverage sections
  |> ByteCoverage.create size binary
