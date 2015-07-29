open ByteCoverage
open MachLoadCommand
open MachSection

let debug = false

let compute_header_coverage header data =
  let size = header.MachHeader.sizeofcmds + MachHeader.sizeof_mach_header in
  let range_start = 0 in
  let range_end = size + range_start in
  let extra = "mach header and load commands meta data" in
  ByteCoverage.add
              {size; understood = true; tag = Meta;
               range_start; range_end; extra} data

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
    let size = 
      dyld_info.rebase_size 
      + dyld_info.bind_size 
      + dyld_info.weak_bind_size 
      + dyld_info.lazy_bind_size 
      + dyld_info.export_size 
    in
    assert (range_end = (range_start + size));
    add {size; tag = Meta; range_start; range_end; 
         understood = true; extra = "dyldinfo"}
      data
  | None -> data

(* 
let compute_segment_coverage tag data (segment:segment_command_64) =
  if (debug) then
    Printf.printf "called: %s\n" segment.segname;
  let size = segment.size in
  let range_start = segment.ElfSectionHeader.sh_offset in
  let range_end = size + range_start in
  let extra =
    segment.ElfSectionHeader.name ^ " // " ^
    ElfSectionHeader.shtype_to_string
      segment.ElfSectionHeader.sh_type
  in
  ByteCoverage.add
    {size; understood = true; tag;
     range_start; range_end; extra} data

let known_segments = 
  [
    (kSEG_TEXT, Code);
    (kSEG_DATA, Data);
    (kSEG_OBJC, Code);
    (kSEG_ICON, Data);
    (kSEG_LINKEDIT, Meta);
  ]

let compute_segment_coverage segments data =
  if (segments = []) then
    data
  else
    let f = (fun data (t, tag) ->
        let  =  section_type shs in
        List.fold_left (compute_segment_coverage tag) segments
      ) in
    (fun data -> List.fold_left f data known_segments)


let compute_section_coverage segments data =
  segname: string; (* 16 bytes *)
  vmaddr: int; (* 8 bytes *)
  vmsize: int; (* 8 bytes *)
  fileoff: int; (* 8 bytes *)
  filesize: int; (* 8 bytes *)
  maxprot: int;  (* 4 int *)
  initprot: int; (* 4 int *)
  nsects: int;   (* 4  *)
  flags: int;    (* 4 *)
  sections: section_64 array; 	(* extra *)
 *)  

let compute header load_commands size =
  compute_header_coverage header ByteCoverage.empty
  |> compute_dyldinfo_coverage load_commands
  |> ByteCoverage.create size

