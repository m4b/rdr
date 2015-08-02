open ByteCoverage
open MachLoadCommand
open MachLoadCommand.Types
open MachConstants

let debug = false

let compute_header_coverage header dataset =
  let mhsize = MachHeader.sizeof_mach_header in
  let lcsize = header.MachHeader.sizeofcmds in
  let range_start = 0 in
  let range_end = mhsize + range_start in
  let extra = "Mach Header" in
  ByteCoverage.add
    (create_data
    ~tag:Meta
    ~r1:range_start
    ~r2:range_end
    ~extra:extra
    ~understood:true
    )
    dataset
  |>
  ByteCoverage.add
    (create_data
    ~tag:Meta
    ~r1:mhsize
    ~r2:(lcsize+mhsize)
    ~extra:"LoadCommands"
    ~understood:true
    )

(* granular dyldinfo/LINKEDIT coverage here *)
let compute_dyldinfo_coverage load_commands dataset =
  (*
      dyld_info.rebase_off
      dyld_info.rebase_size
      dyld_info.bind_off
      dyld_info.bind_size
      dyld_info.weak_bind_off
      dyld_info.weak_bind_size
      dyld_info.lazy_bind_off
      dyld_info.lazy_bind_size
      dyld_info.export_off
      dyld_info.export_size
 *)
  match MachLoadCommand.get_dyld_info load_commands with
  | Some dyld_info ->
    let r1 = dyld_info.rebase_off in
    let r2 = r1 + dyld_info.rebase_size in
    let dyldextra = cmd_int_to_string dyld_info.cmd in
    let extra = "rebase // " ^ dyldextra in
    add
      (create_data
         ~tag:Rela
         ~r1:r1
         ~r2:r2
         ~extra:extra
         ~understood:true) dataset
    |> add
      (create_data
         ~tag:Symbol
         ~r1:dyld_info.bind_off
         ~r2:(dyld_info.bind_off + dyld_info.bind_size)
         ~extra:("bind // " ^ dyldextra)
         ~understood:true)
    |> add
      (create_data
         ~tag:SymbolTable
         ~r1:dyld_info.weak_bind_off
         ~r2:(dyld_info.weak_bind_off + dyld_info.weak_bind_size)
         ~extra:("weak bind // " ^ dyldextra)
         ~understood:true)
    |> add
      (create_data
         ~tag:SymbolTable
         ~r1:dyld_info.lazy_bind_off
         ~r2:(dyld_info.lazy_bind_off + dyld_info.lazy_bind_size)
         ~extra:("lazy bind // " ^ dyldextra)
         ~understood:true)
    |> add
      (create_data
         ~tag:SymbolTable
         ~r1:dyld_info.export_off
         ~r2:(dyld_info.export_off + dyld_info.export_size)
         ~extra:("export // " ^ dyldextra)
         ~understood:true)
    |> add
      (create_data
         ~tag:Meta
         ~r1:dyld_info.rebase_off
         ~r2:(dyld_info.export_off + dyld_info.export_size)
         ~extra:extra
         ~understood:true)

| None -> dataset

let known_sections =
  [
    (kSECT_TEXT, Code);
    (kSECT_DATA, Data);
    (kSECT_COMMON, Semantic);
    (kSECT_BSS, Semantic);
    ("__cstring", String);
    ("__stubs", Rela);
    ("__stub_helper", Rela);
    ("__nl_symbol_ptr", Symbol);
    ("__la_symbol_ptr", Symbol);
  ]

module SectionMap = Map.Make(String)
(* add the known sections to our map *)
let section_map =
  List.fold_left (fun acc (sectname, tag) ->
      SectionMap.add sectname tag acc
    ) SectionMap.empty known_sections

let compute_section_coverage dataset (section:section_64) =
    if (debug) then
      Printf.printf "SECTION called: %s\n" section.sectname;
    let range_start = section.offset in
    let range_end = range_start + section.size in
    let tag,understood =
      try (SectionMap.find section.sectname section_map),true
      with Not_found -> Unknown,false
    in
    let extra =
      section.sectname ^ " // " ^ section.segname ^
      (
      if (tag <> Unknown) then
        ""
      else
        " // Unknown"
      )
    in
    add (create_data
           ~tag:tag
           ~r1:range_start
           ~r2:range_end
           ~extra:extra
           ~understood:understood)
      dataset

let compute_segment_coverage tag dataset (segment:segment_command_64) =
    let size = segment.filesize in
    if (debug) then
      Printf.printf "SEGMENT called: %s\n" segment.segname;
    let range_start = segment.fileoff in
    let range_end = size + range_start in
    let extra =
      segment.segname ^ " // " ^
      cmd_int_to_string segment.cmd
    in
    let dataset =
      List.fold_left compute_section_coverage
        dataset segment.sections
    in
    add
      (create_data
         ~tag:tag
         ~r1:range_start
         ~r2:range_end
           ~extra:extra
           ~understood:true)
      dataset

(* todo, maybe let tags be a list, or have a single main tag... *)
let known_segments =
  [
    (* 
    (kSEG_TEXT, [Code;Semantic]);
    (kSEG_DATA, [Data;Semantic]);
    (kSEG_OBJC, [Code;Semantic]);
    (kSEG_ICON, [Data;Semantic]);
    (kSEG_LINKEDIT, [Meta;Semantic]);
    *)
    (kSEG_TEXT, Semantic);
    (kSEG_DATA, Semantic);
    (kSEG_OBJC, Semantic);
    (kSEG_ICON, Semantic);
    (kSEG_LINKEDIT, Semantic);
  ]

let compute_segment_coverage (segments:segment_command_64 list) dataset =
  if (segments = []) then
    dataset
  else
    let f = (fun dataset (section_name, tag) ->
        match get_segment_by_name section_name segments with
        | Some segment ->
          compute_segment_coverage tag dataset segment
        | None -> dataset
        (*         List.fold_left (compute_segment_coverage section_type tag) dataset segments *)
      ) in
    (fun dataset -> List.fold_left f dataset known_segments) dataset

let compute header (load_commands:lc list) size =
  compute_header_coverage header ByteCoverage.empty
  |> compute_dyldinfo_coverage load_commands
  |> compute_segment_coverage (get_segments load_commands)
  |> ByteCoverage.create size
