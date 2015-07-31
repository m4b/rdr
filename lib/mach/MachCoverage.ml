open ByteCoverage
open MachLoadCommand
open MachLoadCommand.Types
open MachConstants

let debug = false

let compute_header_coverage header data =
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
    data
  |>
  ByteCoverage.add
    (create_data
    ~tag:Meta
    ~r1:mhsize
    ~r2:(lcsize+mhsize)
    ~extra:"LoadCommands"
    ~understood:true
    )

let compute_dyldinfo_coverage load_commands data =
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
    let range_start = dyld_info.rebase_off in
    let range_end = dyld_info.export_off + dyld_info.export_size in
    let extra = "dyldinfo // " ^ (cmd_int_to_string dyld_info.cmd) in
    add (create_data ~tag:Meta ~r1:range_start ~r2:range_end
    ~extra:extra ~understood:true) data
  | None -> data

let compute_segment_coverage tag data (segment:segment_command_64) =
    let size = segment.filesize in
    if (debug) then
      Printf.printf "called: %s\n" segment.segname;
    let range_start = segment.fileoff in
    let range_end = size + range_start in
    let extra =
      segment.segname ^ " // " ^
      cmd_int_to_string segment.cmd
    in
    add (create_data ~tag:tag ~r1:range_start ~r2:range_end
           ~extra:extra ~understood:true) data

let known_segments =
  [
    (kSEG_TEXT, Code);
    (kSEG_DATA, Data);
    (kSEG_OBJC, Code);
    (kSEG_ICON, Data);
    (kSEG_LINKEDIT, Meta);
  ]

let compute_segment_coverage (segments:segment_command_64 list) data =
  if (segments = []) then
    data
  else
    let f = (fun data (section_name, tag) ->
        match get_segment_by_name section_name segments with
        | Some segment ->
          compute_segment_coverage tag data segment
        | None -> data
        (*         List.fold_left (compute_segment_coverage section_type tag) data segments *)
      ) in
    (fun data -> List.fold_left f data known_segments) data

let compute header (load_commands:lc list) size =
  compute_header_coverage header ByteCoverage.empty
  |> compute_dyldinfo_coverage load_commands
  |> compute_segment_coverage (get_segments load_commands)
  |> ByteCoverage.create size
