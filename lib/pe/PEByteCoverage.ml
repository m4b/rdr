open ByteCoverage
open PEHeader

let debug = false

let compute_import_data_coverage data_directory export_data sections data =
  data

let compute_export_data_coverage data_directory (export_data:PEExport.export_data) sections data =
  assert (data_directory.export_table <> 0);
  let size = data_directory.size_of_export_table in
  try
    let r1 = 
      PEUtils.find_offset data_directory.export_table sections
    in
    let r2 = r1 + size in
    ByteCoverage.add
      (create_data
         ~tag:Meta
         ~r1:r1
         ~r2:r2
         ~extra:"Export Table"
         ~understood:true
      ) data
  with Not_found -> data

let compute_data_directory_coverage optional_header (export_data:PEExport.export_data option) import_data sections data =
  match optional_header with
  | None ->
    data
  | Some header ->
    let dd = header.data_directories in
    let data =
      match export_data with
      | Some d ->
        compute_export_data_coverage dd d sections data
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
     (".text", Code);
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
             ~tag:tag
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

let compute_byte_coverage header size (export_data:PEExport.export_data option) (import_data:PEImport.import_data option) sections binary :ByteCoverage.t =
  let dos_end = header.dos_header.pe_pointer in
  let coff_end = dos_end + PEHeader.sizeof_coff_header in
  let optional_end = coff_end + header.coff_header.size_of_optional_header in
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
  |> compute_section_table_coverage sections
  |> compute_data_directory_coverage header.optional_header export_data import_data sections
  |> ByteCoverage.create size binary
