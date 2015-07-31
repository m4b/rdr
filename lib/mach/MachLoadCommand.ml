(* 
# TODO: 
   * Figure out lazy load of libraries and load weak dylibs, currently sending them to dylib_command
*)

module Types = MachLoadCommandTypes

open MachLoadCommandTypes

type t = lc list

let sections64_to_string sections =
  let b = Buffer.create ((List.length sections) * 32) in
  let indent = "\t\t" in
  List.iteri (fun i section ->
      Printf.sprintf "\n%s(%2d) %s addr: 0x%x size: 0x%x\n%soffset: 0x%x align: %d reloff: 0x%x nreloc: 0x%x\n%sflags: 0x%x r1: 0x%x r2: 0x%x r3: 0x%x\n"
	indent i section.sectname section.addr section.size
	indent section.offset section.align section.reloff section.nreloc
	indent section.flags section.reserved1 section.reserved2 section.reserved3
      |> Buffer.add_string b
    ) sections;
  Buffer.contents b

(* add implemented printing mechanisms here *)
let load_command_to_string lc = 
  Printf.sprintf "%s (0x%x) %d %s" (cmd_to_string lc.cmd) (cmd_to_int lc.cmd) lc.cmdsize @@
  match lc.t with
  | LC_SEGMENT_64 lc ->
    Printf.sprintf "\n\t%s vmaddr: 0x%x vmsize: 0x%x\n\tfileoff: 0x%x filesize: 0x%x\n\tmaxprot: %d initprot: %d nsects: %d flags: 0x%x\n%s"
      lc.segname lc.vmaddr lc.vmsize
      lc.fileoff lc.filesize lc.maxprot
      lc.initprot lc.nsects lc.flags
      (sections64_to_string lc.sections)

  | LC_SYMTAB lc ->
    Printf.sprintf "\n\tsymoff: 0x%x nsyms: %u stroff: 0x%x strsize: %u"
      lc.symoff 
      lc.nsyms
      lc.stroff
      lc.strsize

  | LC_DYSYMTAB lc -> 
    Printf.sprintf "\n\tilocalsym: 0x%x nlocalsym: %d iextdefsym: 0x%x nextdefsym: %d\n\tiundefsym: 0x%x nundefsym: %d tocoff: 0x%x ntoc: %d\n\tmodtaboff: 0x%x nmodtab: %d extrefsymoff: 0x%x nextrefsyms: %d\n\tindirectsymoff: 0x%x nindirectsyms: %d extreloff: 0x%x nextrel: %d\n\tlocreloff: 0x%x nlocrel: %d"
      lc.ilocalsym
      lc.nlocalsym
      lc.iextdefsym
      lc.nextdefsym
      lc.iundefsym
      lc.nundefsym
      lc.tocoff
      lc.ntoc
      lc.modtaboff
      lc.nmodtab
      lc.extrefsymoff
      lc.nextrefsyms
      lc.indirectsymoff
      lc.nindirectsyms
      lc.extreloff
      lc.nextrel
      lc.locreloff
      lc.nlocrel

  | LC_DYLD_INFO_ONLY lc
  | LC_DYLD_INFO lc ->
    Printf.sprintf "\n\trebase_off: 0x%x rebase_size: %u \n\tbind_off: 0x%x bind_size: %u \n\tweak_bind_off: 0x%x weak_bind_size: %u \n\tlazy_bind_off: 0x%x lazy_bind_size: %u \n\texport_off: 0x%x export_size: %u"
      lc.rebase_off
      lc.rebase_size
      lc.bind_off
      lc.bind_size
      lc.weak_bind_off
      lc.weak_bind_size
      lc.lazy_bind_off
      lc.lazy_bind_size
      lc.export_off
      lc.export_size

  | LC_LOAD_DYLINKER lc
  | LC_ID_DYLINKER lc ->
    Printf.sprintf "\n\t%s"
      lc.name.str

  | LC_ID_DYLIB lc
  | LC_LOAD_UPWARD_DYLIB lc 
  | LC_LAZY_LOAD_DYLIB lc 
  | LC_LOAD_WEAK_DYLIB lc
  | LC_LOAD_DYLIB lc
  | LC_REEXPORT_DYLIB lc -> 
    (* short version
       Printf.sprintf "\n\t%s"
       lc.dylib.name.str
    *)
    Printf.sprintf "\n\tname: %s timestamp: %u current_version: %u compatibility_version: %u"
      lc.dylib.name.str
      lc.dylib.timestamp
      lc.dylib.current_version
      lc.dylib.compatibility_version

  | LC_VERSION_MIN_IPHONEOS lc
  | LC_VERSION_MIN_MACOSX lc ->
    Printf.sprintf "\n\tversion: %s sdk: %s" (MachVersion.version_to_string lc.version) (MachVersion.version_to_string lc.sdk)

  | LC_MAIN lc ->
    Printf.sprintf "\n\toffset: 0x%x stacksize: 0x%x" lc.entryoff lc.stacksize

  | lc ->
    ""

let print_load_command lc = 
  Printf.printf "%s\n" (load_command_to_string lc)

let print_load_commands lcs =
  List.iteri (fun i lc -> Printf.printf "(%2d): " i; print_load_command lc) lcs;
  print_string "\n"

let get_load_command_header binary offset =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  cmd,cmdsize,o

let get_load_command_raw binary offset size = 
  Bytes.sub binary offset size

let get_segment_by_name segname segments =
  try Some (List.find (fun segment -> segment.segname = segname) segments)
  with Not_found -> None

(* specific load command constructors *)
let get_symtable binary offset =
  let cmd,cmdsize,o = get_load_command_header binary offset in
  let symoff,o = Binary.u32o binary o in
  let nsyms,o = Binary.u32o binary o in
  let stroff,o =  Binary.u32o binary o in
  let strsize =  Binary.u32 binary o in
  {cmd; cmdsize; symoff; nsyms; stroff; strsize;}

let get_dysymtable binary offset =
  let cmd,cmdsize,o = get_load_command_header binary offset in
  let ilocalsym,o = Binary.u32o binary o in
  let nlocalsym,o = Binary.u32o binary o in

  let iextdefsym,o =  Binary.u32o binary o in
  let nextdefsym,o =  Binary.u32o binary o in

  let iundefsym,o = Binary.u32o binary o in
  let nundefsym,o = Binary.u32o binary o in

  let tocoff,o =  Binary.u32o binary o in
  let ntoc,o =  Binary.u32o binary o in
  let modtaboff,o = Binary.u32o binary o in
  let nmodtab,o = Binary.u32o binary o in

  let extrefsymoff,o =  Binary.u32o binary o in
  let nextrefsyms,o =  Binary.u32o binary o in

  let indirectsymoff,o =  Binary.u32o binary o in
  let nindirectsyms,o =  Binary.u32o binary o in
  let extreloff,o =  Binary.u32o binary o in
  let nextrel,o =  Binary.u32o binary o in
  let locreloff,o =  Binary.u32o binary o in
  let nlocrel =  Binary.u32 binary o in
  {
    cmd; cmdsize;
    ilocalsym; nlocalsym; iextdefsym; nextdefsym;
    iundefsym; nundefsym; tocoff; ntoc; modtaboff; nmodtab;
    extrefsymoff; nextrefsyms;
    indirectsymoff; nindirectsyms; extreloff; 
    nextrel; locreloff; nlocrel;
  }

let get_dyld_info binary offset =
  let cmd,cmdsize,o = get_load_command_header binary offset in
  let rebase_off,o = Binary.u32o binary o in
  let rebase_size,o = Binary.u32o binary o in
  let bind_off,o =  Binary.u32o binary o in
  let bind_size,o =  Binary.u32o binary o in
  let weak_bind_off,o =  Binary.u32o binary o in
  let weak_bind_size,o =  Binary.u32o binary o in
  let lazy_bind_off,o =  Binary.u32o binary o in
  let lazy_bind_size,o =  Binary.u32o binary o in
  let export_off,o =  Binary.u32o binary o in
  let export_size =  Binary.u32 binary o in
  {cmd; cmdsize;
   rebase_off; rebase_size; bind_off; bind_size;
   weak_bind_off; weak_bind_size; lazy_bind_off;
   lazy_bind_size; export_off; export_size;}

(* consider glomming all libs into single binary with 0...N load commands and library indexed at 1... *)
let get_dylib_command binary offset =
  let cmd,cmdsize,o = get_load_command_header binary offset in
  let stroffset,o = Binary.u32o binary o in
  let timestamp,o = Binary.u32o binary o in
  let current_version,o =  Binary.u32o binary o in
  let compatibility_version =  Binary.u32 binary o in
  let str = Binary.string binary (stroffset+offset) in (* technically should use the lc_str_offset but need (lc_str_offset - sizeof_load_command) because offset from start of the load_command and we chopped off the first 8 bytes of the lc*)
  Printf.printf "offset: 0x%x dylib name: %s\n" stroffset str;
  let name = {offset; str} in
  let dylib = {name; timestamp; current_version; compatibility_version;} in
  {cmd; cmdsize; dylib}

(* version for osx and ios *)
let get_version binary offset =
  let cmd,cmdsize,o = get_load_command_header binary offset in
  let version,o = Binary.u32o binary o in
  let sdk = Binary.u32 binary o in
  {cmd; cmdsize; version; sdk;}

let get_main binary offset =
  let cmd,cmdsize,o = get_load_command_header binary offset in
  let entryoff,o = Binary.u64o binary o in
  let stacksize = Binary.u64 binary o in
  {cmd; cmdsize; entryoff; stacksize;}

let get_dylinker binary offset =
  let cmd,cmdsize,o = get_load_command_header binary offset in
  let stroffset,o = Binary.u32o binary o in
  let str = Binary.string binary (offset+stroffset) in
  let name = {offset; str} in
  {cmd; cmdsize; name;}

let get_section64 binary offset =
  (*   Printf.printf "initial o: %d\n" o; *)
  let sectname,o = Binary.stringo binary offset ~num_bytes:16 in
  (*   Printf.printf "sectname: %s o: %d\n" sectname o; *)
  let segname,o = Binary.stringo binary o ~num_bytes:16 in
  (*   Printf.printf "segname: %s o: %d\n" segname o; *)
  let addr,o = Binary.u64o binary o in
  let size,o = Binary.u64o binary o in
  let offset,o = Binary.u32o binary o in
  let align,o = Binary.u32o binary o in
  let reloff,o = Binary.u32o binary o in
  let nreloc,o = Binary.u32o binary o in
  let flags,o = Binary.u32o binary o in
  let reserved1,o = Binary.u32o binary o in
  let reserved2,o = Binary.u32o binary o in
  let reserved3 = Binary.u32 binary o in
  {sectname; segname; addr; size; 
   offset; align; reloff; nreloc; flags; 
   reserved1; reserved2; reserved3;}

let get_sections64 binary nsects offset =
  let rec loop count acc =
    if (count >= nsects) then
      List.rev acc
    else
      let section = get_section64 binary ((sizeof_section_64*count)+offset) in
      (*       Printf.printf "sec %d\n" count; *)
      loop (count+1) (section::acc)
  in loop 0 []

let get_segment64 binary offset =
  let cmd,cmdsize,o = get_load_command_header binary offset in
  let segname,o = Binary.stringo ~num_bytes:16 binary o in
  let vmaddr,o = Binary.u64o binary o in
  let vmsize,o =  Binary.u64o binary o in
  let fileoff,o = Binary.u64o binary o in
  let filesize,o =  Binary.u64o binary o in
  let maxprot,o = Binary.u32o binary o in
  let initprot,o = Binary.u32o binary o in
  let nsects,o = Binary.u32o binary o in
  let flags,o = Binary.u32o binary o in
  (*   Printf.printf "vmaddr: 0x%x vmsize 0x%x fileoff 0x%x filesize 0x%x maxprot %d initprot %d nsects %d %d flags\n" vmaddr vmsize fileoff filesize maxprot initprot nsects flags; *)
  let sections = get_sections64 binary nsects o in
  {cmd; cmdsize; segname; vmaddr; vmsize;
   fileoff; filesize; maxprot; initprot;
   nsects; flags; sections;}

let rec get_load_commands_it binary offset ncmds acc =
  if (ncmds <= 0) then
    List.rev acc
  else
    let cmdi,cmdsize,_ = get_load_command_header binary offset in
    let cmd = cmdi |> to_cmd in
    (*     let bytes = Bytes.sub binary offset cmdsize - sizeof_load_command) in *)
    let bytes = binary in
    let t = 
      match cmd with
      | LC_SEGMENT_64 ->
	 LC_SEGMENT_64 (get_segment64 bytes offset)
      | LC_LOAD_DYLINKER ->
	 LC_LOAD_DYLINKER (get_dylinker bytes offset)
      | LC_SYMTAB -> 
        LC_SYMTAB (get_symtable bytes offset)
      | LC_DYSYMTAB ->
        LC_DYSYMTAB (get_dysymtable bytes offset)
      | LC_DYLD_INFO -> 
        LC_DYLD_INFO (get_dyld_info bytes offset)
      | LC_DYLD_INFO_ONLY ->
        LC_DYLD_INFO (get_dyld_info bytes offset)
      | LC_ID_DYLIB ->
        LC_ID_DYLIB (get_dylib_command bytes offset)
      | LC_LOAD_DYLIB | LC_REEXPORT_DYLIB | LC_LOAD_UPWARD_DYLIB | LC_LAZY_LOAD_DYLIB | LC_LOAD_WEAK_DYLIB -> 
        LC_LOAD_DYLIB (get_dylib_command bytes offset)
      | LC_VERSION_MIN_IPHONEOS ->
        LC_VERSION_MIN_IPHONEOS (get_version bytes offset)
      | LC_VERSION_MIN_MACOSX ->
        LC_VERSION_MIN_MACOSX (get_version bytes offset)
      | LC_MAIN ->
        LC_MAIN (get_main bytes offset)
      | _ ->
        LC_UNIMPLEMENTED {cmd=cmdi; cmdsize}
    in
    let load_command = {cmd; cmdsize; t} in
    (*     let load_command = (cmd, cmdsize, lc_t) in *)
    get_load_commands_it binary (offset + cmdsize) (ncmds - 1) (load_command::acc)

let get_load_commands binary offset ncmds sizeofcmds : t =
  get_load_commands_it binary offset ncmds []

let rec get_load_command cmd (lcs:Types.lc list) :Types.lc_t option =
  match lcs with
  | [] -> None
  | lc::lcs ->
    if (lc.cmd = cmd) then
      Some lc.t
    else
      get_load_command cmd lcs

let get_segments lcs =
  List.fold_left (fun acc lc ->
      match lc.t with
      | LC_SEGMENT_64 segment ->
        segment::acc
      | _ ->
        acc
    ) [] lcs |> List.rev

let rec get_dyld_info lcs =
  match lcs with
  | [] -> None
  | lc::lcs ->
    match lc.t with
    | LC_DYLD_INFO lc
    | LC_DYLD_INFO_ONLY lc ->
      Some lc
    | _ ->
      get_dyld_info lcs

let rec get_lib_name lcs =
  match lcs with
  | [] -> ""
  | lc::lcs ->
    match lc.t with
    | LC_ID_DYLIB lc ->
      lc.dylib.name.str
    | _ -> get_lib_name lcs

let print_libraries libs =
  if ((Array.length libs) <> 0) then
    begin
      Printf.printf "Libraries (%d)\n" @@ (Array.length libs - 1);
      Array.iteri (fun i lib ->
	  if (i <> 0) then
	    Printf.printf "%s\n" lib) libs
    end

let get_libraries lcs self =
  List.fold_left (fun acc lc ->
      match lc.t with
      | LC_LOAD_DYLIB lc
      | LC_REEXPORT_DYLIB lc
      | LC_LOAD_UPWARD_DYLIB lc
      | LC_LAZY_LOAD_DYLIB lc
      | LC_LOAD_WEAK_DYLIB lc ->
        lc.dylib.name.str::acc
      | _ -> acc
    ) [self] lcs |> List.rev |> Array.of_list
