(* 
# TODO: 
   * Rename lc without LC prefix to use prefix to be more in line with mach
   * Verify the new load commands: Figure out lazy load of libraries and load weak dylibs
*)

module Types = MachLoadCommandTypes

open MachLoadCommandTypes

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
let load_command_to_string = 
  function
  | LC_SEGMENT_64 lc ->
    Printf.sprintf "%s (0x%x) %d %s" (cmd_int_to_string lc.cmd) lc.cmd lc.cmdsize @@
    Printf.sprintf "\n\t%s vmaddr: 0x%x vmsize: 0x%x\n\tfileoff: 0x%x filesize: 0x%x\n\tmaxprot: %d initprot: %d nsects: %d flags: 0x%x\n%s"
      lc.segname lc.vmaddr lc.vmsize
      lc.fileoff lc.filesize lc.maxprot
      lc.initprot lc.nsects lc.flags
      (sections64_to_string lc.sections)

  | LC_SYMTAB lc ->
    Printf.sprintf "%s (0x%x) %d %s" (cmd_int_to_string lc.cmd) lc.cmd lc.cmdsize @@
    Printf.sprintf "\n\tsymoff: 0x%x nsyms: %u stroff: 0x%x strsize: %u"
      lc.symoff 
      lc.nsyms
      lc.stroff
      lc.strsize

  | LC_DYSYMTAB lc -> 
    Printf.sprintf "%s (0x%x) %d %s" (cmd_int_to_string lc.cmd) lc.cmd lc.cmdsize @@
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
    Printf.sprintf "%s (0x%x) %d %s" (cmd_int_to_string lc.cmd) lc.cmd lc.cmdsize @@
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
    Printf.sprintf "%s (0x%x) %d %s" (cmd_int_to_string lc.cmd) lc.cmd lc.cmdsize @@
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
    Printf.sprintf "%s (0x%x) %d %s" (cmd_int_to_string lc.cmd) lc.cmd lc.cmdsize @@
    Printf.sprintf "\n\tlc_str: %s timestamp: %u current_version: %u compatibility_version: %u"
      lc.dylib.name.str
      lc.dylib.timestamp
      lc.dylib.current_version
      lc.dylib.compatibility_version

  | LC_VERSION_MIN_IPHONEOS lc
  | LC_VERSION_MIN_MACOSX lc ->
    Printf.sprintf "%s (0x%x) %d %s" (cmd_int_to_string lc.cmd) lc.cmd lc.cmdsize @@
    Printf.sprintf "\n\tversion: %s sdk: %s" (MachVersion.version_to_string lc.version) (MachVersion.version_to_string lc.sdk)

  | LC_MAIN lc ->
    Printf.sprintf "%s (0x%x) %d %s" (cmd_int_to_string lc.cmd) lc.cmd lc.cmdsize @@
    Printf.sprintf "\n\toffset: 0x%x stacksize: 0x%x" lc.entryoff lc.stacksize

  | _ ->
    Printf.sprintf "UNIMPLEMENTED"

(* DRY violations, oh yea
    Printf.sprintf "%s (0x%x) %d %s" (cmd_int_to_string lc.cmd) lc.cmd lc.cmdsize @@
 *)

let print_load_command lc = 
  Printf.printf "%s\n" (load_command_to_string lc)

let print_load_commands lcs =
  List.iteri (fun i lc -> Printf.printf "(%2d): " i; print_load_command lc) lcs;
  print_string "\n"

let get_load_command_header binary offset =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  cmd,cmdsize

let get_data binary offset size = 
  Bytes.sub binary offset size

(* specific load command constructors *)
let get_symtable binary offset =
  let cmd,cmdsize = get_load_command_header binary offset in
  let symoff = Binary.u32 binary 0 in
  let nsyms = Binary.u32 binary 4 in
  let stroff =  Binary.u32 binary 8 in
  let strsize =  Binary.u32 binary 12 in
  LC_SYMTAB {cmd; cmdsize; symoff; nsyms; stroff; strsize;}

let get_dysymtable binary offset =
  let cmd,cmdsize = get_load_command_header binary offset in
  let ilocalsym = Binary.u32 binary 0 in
  let nlocalsym = Binary.u32 binary 4 in

  let iextdefsym =  Binary.u32 binary 8 in
  let nextdefsym =  Binary.u32 binary 12 in

  let iundefsym = Binary.u32 binary 16 in
  let nundefsym = Binary.u32 binary 20 in

  let tocoff =  Binary.u32 binary 24 in
  let ntoc =  Binary.u32 binary 28 in
  let modtaboff = Binary.u32 binary 32 in
  let nmodtab = Binary.u32 binary 36 in

  let extrefsymoff =  Binary.u32 binary 40 in
  let nextrefsyms =  Binary.u32 binary 44 in

  let indirectsymoff =  Binary.u32 binary 48 in
  let nindirectsyms =  Binary.u32 binary 52 in
  let extreloff =  Binary.u32 binary 56 in
  let nextrel =  Binary.u32 binary 60 in
  let locreloff =  Binary.u32 binary 64 in
  let nlocrel =  Binary.u32 binary 68 in
  {
    cmd; cmdsize;
    ilocalsym; nlocalsym; iextdefsym; nextdefsym;
    iundefsym; nundefsym; tocoff; ntoc; modtaboff; nmodtab;
    extrefsymoff; nextrefsyms;
    indirectsymoff; nindirectsyms; extreloff; 
    nextrel; locreloff; nlocrel;
  }

let get_dyld_info binary offset =
  let cmd,cmdsize = get_load_command_header binary offset in
  let rebase_off = Binary.u32 binary 0 in
  let rebase_size = Binary.u32 binary 4 in
  let bind_off =  Binary.u32 binary 8 in
  let bind_size =  Binary.u32 binary 12 in
  let weak_bind_off =  Binary.u32 binary 16 in
  let weak_bind_size =  Binary.u32 binary 20 in
  let lazy_bind_off =  Binary.u32 binary 24 in
  let lazy_bind_size =  Binary.u32 binary 28 in
  let export_off =  Binary.u32 binary 32 in
  let export_size =  Binary.u32 binary 36 in
  {cmd; cmdsize;
   rebase_off; rebase_size; bind_off; bind_size;
   weak_bind_off; weak_bind_size; lazy_bind_off;
   lazy_bind_size; export_off; export_size;}

let get_dylib binary =
  let offset = Binary.u32 binary 0 in
  let timestamp = Binary.u32 binary 4 in
  let current_version =  Binary.u32 binary 8 in
  let compatibility_version =  Binary.u32 binary 12 in
  let str = Binary.string binary offset in (* technically should use the lc_str_offset but need (lc_str_offset - sizeof_load_command) because offset from start of the load_command and we chopped off the first 8 bytes of the lc*)
  let name = {offset; str} in
  {name; timestamp; current_version; compatibility_version;}

(* consider glomming all libs into single binary with 0...N load commands and library indexed at 1... *)
let get_dylib_command binary offset =
  let cmd,cmdsize = get_load_command_header binary offset in
  {cmd; cmdsize; dylib = get_dylib binary}

(* version for osx and ios *)
let get_version binary offset =
  let cmd,cmdsize = get_load_command_header binary offset in
  let version = Binary.u32 binary 0 in
  let sdk = Binary.u32 binary 4 in
  {cmd; cmdsize; version; sdk;}

let get_main binary offset =
  let cmd,cmdsize = get_load_command_header binary offset in
  let entryoff = Binary.u64 binary 0 in
  let stacksize = Binary.u64 binary 8 in
  {cmd; cmdsize; entryoff; stacksize;}

let get_dylinker binary offset =
  let cmd,cmdsize = get_load_command_header binary offset in
  let offset = Binary.u32 binary 0 in
  let str = Binary.string binary offset in
  let name = {offset; str} in
  {cmd; cmdsize; name;}

let get_section64 binary offset =
  (*   Printf.printf "initial o: %d\n" o; *)
  let sectname = Binary.string binary offset ~maxlen:(15+offset) in
  (*   Printf.printf "sectname: %s o: %d\n" sectname o; *)
  let segname = Binary.string binary (16+offset) ~maxlen:(15+16+offset) in
  (*   Printf.printf "segname: %s o: %d\n" segname o; *)
  let addr,o = Binary.u64o binary (offset+32) in
  let size,o = Binary.u64o binary o in
  let offset,o = Binary.u32o binary o in
  let align,o = Binary.u32o binary o in
  let reloff,o = Binary.u32o binary o in
  let nreloc,o = Binary.u32o binary o in
  let flags,o = Binary.u32o binary o in
  let reserved1,o = Binary.u32o binary o in
  let reserved2,o = Binary.u32o binary o in
  let reserved3,o = Binary.u32o binary o in
  {sectname; segname; addr; size; 
   offset; align; reloff; nreloc; flags; 
   reserved1; reserved2; reserved3;}

let get_sections64 binary nsects offset =
  let rec loop count acc =
    if (count >= nsects) then
      List.rev acc
    else
      let section = get_section64 binary ((sizeof_section_64*count)+offset) in
      loop (count+1) (section::acc)
  in loop 0 []

let get_segment64 binary offset =
  let cmd,cmdsize = get_load_command_header binary offset in
  let segname = Binary.string binary 0 ~maxlen:15 in
  let vmaddr = Binary.u64 binary 16 in
  let vmsize =  Binary.u64 binary 24 in
  let fileoff = Binary.u64 binary 32 in
  let filesize =  Binary.u64 binary 40 in
  let maxprot = Binary.u32 binary 48 in
  let initprot = Binary.u32 binary 52 in
  let nsects = Binary.u32 binary 56 in
  let flags = Binary.u32 binary 60 in
  let sections = get_sections64 binary nsects 64 in
  {cmd; cmdsize; segname; vmaddr; vmsize;
   fileoff; filesize; maxprot; initprot;
   nsects; flags; sections;}

type t = lc list

let rec get_load_commands_it binary offset ncmds acc = 
  if (ncmds <= 0) then
    List.rev acc
  else
    (*     let bytes = Bytes.sub binary offset cmdsize - sizeof_load_command) in *)
    let bytes = binary in
    let lc_t = 
      match cmd with
      | SEGMENT_64 ->
	 SEGMENT_64 (get_segment64 bytes)
      | LOAD_DYLINKER ->
	 DYLINKER (get_dylinker bytes)
      | SYMTAB -> 
        SYMTAB (get_symtable bytes)
      | DYSYMTAB ->
        DYSYMTAB (get_dysymtable bytes)
      | DYLD_INFO_ONLY | LC_DYLD_INFO -> 
        DYLD_INFO (get_dyld_info bytes)
      | LOAD_DYLIB | REEXPORT_DYLIB | ID_DYLIB | LOAD_UPWARD_DYLIB | LAZY_LOAD_DYLIB | LC_LOAD_WEAK_DYLIB -> 
        DYLIB (get_dylib bytes)
      | VERSION_MIN_MACOSX | VERSION_MIN_IPHONEOS ->
        VERSION (get_version bytes)
      | MAIN ->
        ENTRY_POINT (get_main bytes)
      | _ ->
        Unimplemented (get_data binary (offset + sizeof_load_command) (cmdsize - sizeof_load_command)) in
    let load_command = (cmd, cmdsize, lc_t) in
    get_load_commands_it binary (offset + cmdsize) (ncmds - 1) (load_command::acc)

let get_load_commands binary offset ncmds sizeofcmds =
  let load_command_bytes = Bytes.sub binary offset sizeofcmds in
  get_load_commands_it load_command_bytes 0 ncmds []

exception Missing_load_command of string

let rec get_load_command cmd lcs =
  match lcs with
  | [] -> None
  | lc::lcs ->
    (* doomed, right here *)
    if (lc == cmd) then lc'
    else
      find_load_command lc lcs

let get_segments lcs =
  let rec loop lcs acc =
  match lcs with
  | [] -> List.rev acc
  | (cmd, _, (SEGMENT_64 segment))::lcs ->
     loop lcs (segment::acc)
  | lc::lcs -> loop lcs acc
  in loop lcs []

let rec get_dyld_info lcs =
  match lcs with
  | [] -> None
  | (cmd,cmdsize, DYLD_INFO dyldinfo)::lcs ->
    if (cmd = DYLD_INFO_ONLY || cmd = LC_DYLD_INFO) then Some dyldinfo
    else
      get_dyld_info lcs
  | lc::lcs -> get_dyld_info lcs

let rec get_lib_name lcs =
  match lcs with
  | [] -> None
  | (ID_DYLIB, _, DYLIB id)::lcs ->
    Some id
  | lc::lcs -> get_lib_name lcs

let get_libraries lcs =
  let rec loop lcs acc =
    match lcs with
    | [] -> 
      Array.of_list @@ List.rev acc
    | ((cmd, _, _) as lc)::lcs ->
      if (cmd = LOAD_DYLIB) then loop lcs (lc::acc)
      else
        loop lcs acc
  in loop lcs []

let print_libraries libs =
  if ((Array.length libs) <> 0) then
    begin
      Printf.printf "Libraries (%d)\n" @@ (Array.length libs - 1);
      Array.iteri (fun i lib ->
		  if (i <> 0) then
		    Printf.printf "%s\n" lib) libs
    end

let get_libraries lcs self = 
  let rec loop lcs acc =
    match lcs with
    | [] -> 
      Array.of_list @@ List.rev acc
    | (cmd, _, DYLIB dylib)::lcs ->
      begin
        match cmd with
        | LOAD_DYLIB | REEXPORT_DYLIB | LOAD_UPWARD_DYLIB | LAZY_LOAD_DYLIB | LC_LOAD_WEAK_DYLIB ->
          loop lcs (dylib.lc_str::acc)
        | _ ->
          loop lcs acc
      end
    | _::lcs ->
      loop lcs acc
  in loop lcs [self]
