open MachLoadCommandTypes
open Binary

let lc_count = 43

let get_load_command binary offset :load_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize = Binary.u32 binary o in
  {cmd;cmdsize;}


let get_section binary offset :section =
  let sectname,o = Binary.stringo binary offset ~num_bytes:16 in
  let segname,o = Binary.stringo binary o ~num_bytes:16 in
  let addr,o = Binary.u32o binary o in
  let size,o = Binary.u32o binary o in
  let offset,o = Binary.u32o binary o in
  let align,o = Binary.u32o binary o in
  let reloff,o = Binary.u32o binary o in
  let nreloc,o = Binary.u32o binary o in
  let flags,o = Binary.u32o binary o in
  let reserved1,o = Binary.u32o binary o in
  let reserved2 = Binary.u32 binary o in
  {sectname;segname;addr;size;offset;align;reloff;nreloc;flags;reserved1;reserved2;}

let get_sections binary nsects offset =
  let rec loop count acc =
    if (count >= nsects) then
      List.rev acc
    else
      let section = get_section binary ((sizeof_section*count)+offset) in
      loop (count+1) (section::acc)
  in loop 0 []

let get_section_64 binary offset :section_64 =
  let sectname,o = Binary.stringo binary offset ~num_bytes:16 in
  let segname,o = Binary.stringo binary o ~num_bytes:16 in
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
  {sectname;segname;addr;size;offset;align;reloff;nreloc;flags;reserved1;reserved2;reserved3;}

let get_sections_64 binary nsects offset =
  let rec loop count acc =
    if (count >= nsects) then
      List.rev acc
    else
      let section = get_section_64 binary ((sizeof_section_64*count)+offset) in
      loop (count+1) (section::acc)
  in loop 0 []

let get_segment_command binary offset :segment_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let segname,o = Binary.stringo binary o ~num_bytes:16 in
  let vmaddr,o = Binary.u32o binary o in
  let vmsize,o = Binary.u32o binary o in
  let fileoff,o = Binary.u32o binary o in
  let filesize,o = Binary.u32o binary o in
  let maxprot,o = Binary.u32o binary o in
  let initprot,o = Binary.u32o binary o in
  let nsects,o = Binary.u32o binary o in
  let flags,o = Binary.u32o binary o in
  let sections = get_sections binary nsects o in
  {cmd;cmdsize;segname;vmaddr;vmsize;fileoff;filesize;maxprot;initprot;nsects;flags;sections;}

let get_segment_command_64 binary offset :segment_command_64 =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let segname,o = Binary.stringo binary o ~num_bytes:16 in
  let vmaddr,o = Binary.u64o binary o in
  let vmsize,o = Binary.u64o binary o in
  let fileoff,o = Binary.u64o binary o in
  let filesize,o = Binary.u64o binary o in
  let maxprot,o = Binary.u32o binary o in
  let initprot,o = Binary.u32o binary o in
  let nsects,o = Binary.u32o binary o in
  let flags,o = Binary.u32o binary o in
  let sections = get_sections_64 binary nsects o in
  {cmd;cmdsize;segname;vmaddr;vmsize;fileoff;filesize;maxprot;initprot;nsects;flags;sections;}

let get_fvmlib binary offset o :fvmlib =
  let stroffset,o = Binary.u32o binary o in
  let str = Binary.string binary (offset+stroffset) in
  let name = {offset; str} in
  let minor_version,o = Binary.u32o binary o in
  let header_addr = Binary.u32 binary o in
  {name;minor_version;header_addr;}

let get_fvmlib_command binary offset :fvmlib_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let fvmlib = get_fvmlib binary offset o in
  {cmd;cmdsize;fvmlib;}

let get_dylib binary offset o :dylib =
  let stroffset,o = Binary.u32o binary o in
  let str = Binary.string binary (offset+stroffset) in
  let name = {offset; str} in
  let timestamp,o = Binary.u32o binary o in
  let current_version,o = Binary.u32o binary o in
  let compatibility_version = Binary.u32 binary o in
  {name;timestamp;current_version;compatibility_version;}

let get_dylib_command binary offset :dylib_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let dylib = get_dylib binary offset o in
  {cmd;cmdsize;dylib;}

let get_sub_framework_command binary offset :sub_framework_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let stroffset,o = Binary.u32o binary o in
  let str = Binary.string binary (offset+stroffset) in
  let umbrella = {offset; str} in
  {cmd;cmdsize;umbrella;}

let get_sub_client_command binary offset :sub_client_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let stroffset,o = Binary.u32o binary o in
  let str = Binary.string binary (offset+stroffset) in
  let client = {offset; str} in
  {cmd;cmdsize;client;}

let get_sub_umbrella_command binary offset :sub_umbrella_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let stroffset,o = Binary.u32o binary o in
  let str = Binary.string binary (offset+stroffset) in
  let sub_umbrella = {offset; str} in
  {cmd;cmdsize;sub_umbrella;}

let get_sub_library_command binary offset :sub_library_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let stroffset,o = Binary.u32o binary o in
  let str = Binary.string binary (offset+stroffset) in
  let sub_library = {offset; str} in
  {cmd;cmdsize;sub_library;}

let get_prebound_dylib_command binary offset :prebound_dylib_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let stroffset,o = Binary.u32o binary o in
  let str = Binary.string binary (offset+stroffset) in
  let name = {offset; str} in
  let nmodules,o = Binary.u32o binary o in
  let stroffset,o = Binary.u32o binary o in
  let str = Binary.string binary (offset+stroffset) in
  let linked_modules = {offset; str} in
  {cmd;cmdsize;name;nmodules;linked_modules;}

let get_dylinker_command binary offset :dylinker_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let stroffset,o = Binary.u32o binary o in
  let str = Binary.string binary (offset+stroffset) in
  let name = {offset; str} in
  {cmd;cmdsize;name;}

let get_thread_command binary offset :thread_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize = Binary.u32 binary o in
  {cmd;cmdsize;}

let get_routines_command binary offset :routines_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let init_address,o = Binary.u32o binary o in
  let init_module,o = Binary.u32o binary o in
  let reserved1,o = Binary.u32o binary o in
  let reserved2,o = Binary.u32o binary o in
  let reserved3,o = Binary.u32o binary o in
  let reserved4,o = Binary.u32o binary o in
  let reserved5,o = Binary.u32o binary o in
  let reserved6 = Binary.u32 binary o in
  {cmd;cmdsize;init_address;init_module;reserved1;reserved2;reserved3;reserved4;reserved5;reserved6;}

let get_routines_command_64 binary offset :routines_command_64 =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let init_address,o = Binary.u64o binary o in
  let init_module,o = Binary.u64o binary o in
  let reserved1,o = Binary.u64o binary o in
  let reserved2,o = Binary.u64o binary o in
  let reserved3,o = Binary.u64o binary o in
  let reserved4,o = Binary.u64o binary o in
  let reserved5,o = Binary.u64o binary o in
  let reserved6 = Binary.u64 binary o in
  {cmd;cmdsize;init_address;init_module;reserved1;reserved2;reserved3;reserved4;reserved5;reserved6;}

let get_symtab_command binary offset :symtab_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let symoff,o = Binary.u32o binary o in
  let nsyms,o = Binary.u32o binary o in
  let stroff,o = Binary.u32o binary o in
  let strsize = Binary.u32 binary o in
  {cmd;cmdsize;symoff;nsyms;stroff;strsize;}

let get_dysymtab_command binary offset :dysymtab_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let ilocalsym,o = Binary.u32o binary o in
  let nlocalsym,o = Binary.u32o binary o in
  let iextdefsym,o = Binary.u32o binary o in
  let nextdefsym,o = Binary.u32o binary o in
  let iundefsym,o = Binary.u32o binary o in
  let nundefsym,o = Binary.u32o binary o in
  let tocoff,o = Binary.u32o binary o in
  let ntoc,o = Binary.u32o binary o in
  let modtaboff,o = Binary.u32o binary o in
  let nmodtab,o = Binary.u32o binary o in
  let extrefsymoff,o = Binary.u32o binary o in
  let nextrefsyms,o = Binary.u32o binary o in
  let indirectsymoff,o = Binary.u32o binary o in
  let nindirectsyms,o = Binary.u32o binary o in
  let extreloff,o = Binary.u32o binary o in
  let nextrel,o = Binary.u32o binary o in
  let locreloff,o = Binary.u32o binary o in
  let nlocrel = Binary.u32 binary o in
  {cmd;cmdsize;ilocalsym;nlocalsym;iextdefsym;nextdefsym;iundefsym;nundefsym;tocoff;ntoc;modtaboff;nmodtab;extrefsymoff;nextrefsyms;indirectsymoff;nindirectsyms;extreloff;nextrel;locreloff;nlocrel;}

let get_dylib_table_of_contents binary offset :dylib_table_of_contents =
  let symbol_index,o = Binary.u32o binary offset in
  let module_index = Binary.u32 binary o in
  {symbol_index;module_index;}

let get_dylib_module binary offset :dylib_module =
  let module_name,o = Binary.u32o binary offset in
  let iextdefsym,o = Binary.u32o binary o in
  let nextdefsym,o = Binary.u32o binary o in
  let irefsym,o = Binary.u32o binary o in
  let nrefsym,o = Binary.u32o binary o in
  let ilocalsym,o = Binary.u32o binary o in
  let nlocalsym,o = Binary.u32o binary o in
  let iextrel,o = Binary.u32o binary o in
  let nextrel,o = Binary.u32o binary o in
  let iinit_iterm,o = Binary.u32o binary o in
  let ninit_nterm,o = Binary.u32o binary o in
  let objc_module_info_addr,o = Binary.u32o binary o in
  let objc_module_info_size = Binary.u32 binary o in
  {module_name;iextdefsym;nextdefsym;irefsym;nrefsym;ilocalsym;nlocalsym;iextrel;nextrel;iinit_iterm;ninit_nterm;objc_module_info_addr;objc_module_info_size;}

let get_dylib_module_64 binary offset :dylib_module_64 =
  let module_name,o = Binary.u32o binary offset in
  let iextdefsym,o = Binary.u32o binary o in
  let nextdefsym,o = Binary.u32o binary o in
  let irefsym,o = Binary.u32o binary o in
  let nrefsym,o = Binary.u32o binary o in
  let ilocalsym,o = Binary.u32o binary o in
  let nlocalsym,o = Binary.u32o binary o in
  let iextrel,o = Binary.u32o binary o in
  let nextrel,o = Binary.u32o binary o in
  let iinit_iterm,o = Binary.u32o binary o in
  let ninit_nterm,o = Binary.u32o binary o in
  let objc_module_info_size,o = Binary.u32o binary o in
  let objc_module_info_addr = Binary.u64 binary o in
  {module_name;iextdefsym;nextdefsym;irefsym;nrefsym;ilocalsym;nlocalsym;iextrel;nextrel;iinit_iterm;ninit_nterm;objc_module_info_size;objc_module_info_addr;}

let get_dylib_reference binary offset :dylib_reference =
  let isym,o = Binary.stringo binary offset ~num_bytes:24 in
  let flags = Binary.u64 binary o in
  {isym;flags;}

let get_twolevel_hints_command binary offset :twolevel_hints_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let offset,o = Binary.u32o binary o in
  let nhints = Binary.u32 binary o in
  {cmd;cmdsize;offset;nhints;}

let get_twolevel_hint binary offset :twolevel_hint =
  let isub_image,o = Binary.u64o binary offset in
  let itoc = Binary.string binary o ~max:24 in
  {isub_image;itoc;}

let get_prebind_cksum_command binary offset :prebind_cksum_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let cksum = Binary.u32 binary o in
  {cmd;cmdsize;cksum;}

let get_uuid_command binary offset :uuid_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let uuid = Binary.string binary o ~max:16 in
  {cmd;cmdsize;uuid;}

let get_rpath_command binary offset :rpath_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let stroffset,o = Binary.u32o binary o in
  let str = Binary.string binary (offset+stroffset) in
  let path = {offset; str} in
  {cmd;cmdsize;path;}

let get_linkedit_data_command binary offset :linkedit_data_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let dataoff,o = Binary.u32o binary o in
  let datasize = Binary.u32 binary o in
  {cmd;cmdsize;dataoff;datasize;}

let get_encryption_info_command binary offset :encryption_info_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let cryptoff,o = Binary.u32o binary o in
  let cryptsize,o = Binary.u32o binary o in
  let cryptid = Binary.u32 binary o in
  {cmd;cmdsize;cryptoff;cryptsize;cryptid;}

let get_encryption_info_command_64 binary offset :encryption_info_command_64 =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let cryptoff,o = Binary.u32o binary o in
  let cryptsize,o = Binary.u32o binary o in
  let cryptid,o = Binary.u32o binary o in
  let pad = Binary.u32 binary o in
  {cmd;cmdsize;cryptoff;cryptsize;cryptid;pad;}

let get_version_min_command binary offset :version_min_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let version,o = Binary.u32o binary o in
  let sdk = Binary.u32 binary o in
  {cmd;cmdsize;version;sdk;}

let get_dyld_info_command binary offset :dyld_info_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let rebase_off,o = Binary.u32o binary o in
  let rebase_size,o = Binary.u32o binary o in
  let bind_off,o = Binary.u32o binary o in
  let bind_size,o = Binary.u32o binary o in
  let weak_bind_off,o = Binary.u32o binary o in
  let weak_bind_size,o = Binary.u32o binary o in
  let lazy_bind_off,o = Binary.u32o binary o in
  let lazy_bind_size,o = Binary.u32o binary o in
  let export_off,o = Binary.u32o binary o in
  let export_size = Binary.u32 binary o in
  {cmd;cmdsize;rebase_off;rebase_size;bind_off;bind_size;weak_bind_off;weak_bind_size;lazy_bind_off;lazy_bind_size;export_off;export_size;}

let get_linker_option_command binary offset :linker_option_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let count = Binary.u32 binary o in
  {cmd;cmdsize;count;}

let get_symseg_command binary offset :symseg_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let offset,o = Binary.u32o binary o in
  let size = Binary.u32 binary o in
  {cmd;cmdsize;offset;size;}

let get_ident_command binary offset :ident_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize = Binary.u32 binary o in
  {cmd;cmdsize;}

let get_fvmfile_command binary offset :fvmfile_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let stroffset,o = Binary.u32o binary o in
  let str = Binary.string binary (offset+stroffset) in
  let name = {offset; str} in
  let header_addr = Binary.u32 binary o in
  {cmd;cmdsize;name;header_addr;}

let get_entry_point_command binary offset :entry_point_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let entryoff,o = Binary.u64o binary o in
  let stacksize = Binary.u64 binary o in
  {cmd;cmdsize;entryoff;stacksize;}

let get_source_version_command binary offset :source_version_command =
  let cmd,o = Binary.u32o binary offset in
  let cmdsize,o = Binary.u32o binary o in
  let version = Binary.u64 binary o in
  {cmd;cmdsize;version;}

let get_data_in_code_entry binary offset :data_in_code_entry =
  let offset,o = Binary.u32o binary offset in
  let length,o = Binary.u16o binary o in
  let kind = Binary.u16 binary o in
  {offset;length;kind;}


let get_t (cmd:cmd) bytes offset =
  match cmd with
    | LC_SEGMENT ->
      LC_SEGMENT (get_segment_command bytes offset)
    | LC_SYMTAB ->
      LC_SYMTAB (get_symtab_command bytes offset)
    | LC_SYMSEG ->
      LC_SYMSEG (get_symseg_command bytes offset)
    | LC_THREAD ->
      LC_THREAD (get_thread_command bytes offset)
    | LC_UNIXTHREAD ->
      LC_UNIXTHREAD (get_thread_command bytes offset)
    | LC_LOADFVMLIB ->
      LC_LOADFVMLIB (get_fvmlib_command bytes offset)
    | LC_IDFVMLIB ->
      LC_IDFVMLIB (get_fvmlib_command bytes offset)
    | LC_IDENT ->
      LC_IDENT (get_ident_command bytes offset)
    | LC_FVMFILE ->
      LC_FVMFILE (get_fvmfile_command bytes offset)
    | LC_PREPAGE ->
      LC_PREPAGE (get_load_command bytes offset)
    | LC_DYSYMTAB ->
      LC_DYSYMTAB (get_dysymtab_command bytes offset)
    | LC_LOAD_DYLIB ->
      LC_LOAD_DYLIB (get_dylib_command bytes offset)
    | LC_ID_DYLIB ->
      LC_ID_DYLIB (get_dylib_command bytes offset)
    | LC_LOAD_DYLINKER ->
      LC_LOAD_DYLINKER (get_dylinker_command bytes offset)
    | LC_ID_DYLINKER ->
      LC_ID_DYLINKER (get_dylinker_command bytes offset)
    | LC_PREBOUND_DYLIB ->
      LC_PREBOUND_DYLIB (get_prebound_dylib_command bytes offset)
    | LC_ROUTINES ->
      LC_ROUTINES (get_routines_command bytes offset)
    | LC_SUB_FRAMEWORK ->
      LC_SUB_FRAMEWORK (get_sub_framework_command bytes offset)
    | LC_SUB_UMBRELLA ->
      LC_SUB_UMBRELLA (get_sub_umbrella_command bytes offset)
    | LC_SUB_CLIENT ->
      LC_SUB_CLIENT (get_sub_client_command bytes offset)
    | LC_SUB_LIBRARY ->
      LC_SUB_LIBRARY (get_sub_library_command bytes offset)
    | LC_TWOLEVEL_HINTS ->
      LC_TWOLEVEL_HINTS (get_twolevel_hints_command bytes offset)
    | LC_PREBIND_CKSUM ->
      LC_PREBIND_CKSUM (get_prebind_cksum_command bytes offset)
    | LC_LOAD_WEAK_DYLIB ->
      LC_LOAD_WEAK_DYLIB (get_dylib_command bytes offset)
    | LC_SEGMENT_64 ->
      LC_SEGMENT_64 (get_segment_command_64 bytes offset)
    | LC_ROUTINES_64 ->
      LC_ROUTINES_64 (get_routines_command_64 bytes offset)
    | LC_UUID ->
      LC_UUID (get_uuid_command bytes offset)
    | LC_RPATH ->
      LC_RPATH (get_rpath_command bytes offset)
    | LC_CODE_SIGNATURE ->
      LC_CODE_SIGNATURE (get_linkedit_data_command bytes offset)
    | LC_SEGMENT_SPLIT_INFO ->
      LC_SEGMENT_SPLIT_INFO (get_linkedit_data_command bytes offset)
    | LC_REEXPORT_DYLIB ->
      LC_REEXPORT_DYLIB (get_dylib_command bytes offset)
    | LC_LAZY_LOAD_DYLIB ->
      LC_LAZY_LOAD_DYLIB (get_dylib_command bytes offset)
    | LC_ENCRYPTION_INFO ->
      LC_ENCRYPTION_INFO (get_encryption_info_command bytes offset)
    | LC_DYLD_INFO ->
      LC_DYLD_INFO (get_dyld_info_command bytes offset)
    | LC_DYLD_INFO_ONLY ->
      LC_DYLD_INFO_ONLY (get_dyld_info_command bytes offset)
    | LC_LOAD_UPWARD_DYLIB ->
      LC_LOAD_UPWARD_DYLIB (get_dylib_command bytes offset)
    | LC_VERSION_MIN_MACOSX ->
      LC_VERSION_MIN_MACOSX (get_version_min_command bytes offset)
    | LC_VERSION_MIN_IPHONEOS ->
      LC_VERSION_MIN_IPHONEOS (get_version_min_command bytes offset)
    | LC_FUNCTION_STARTS ->
      LC_FUNCTION_STARTS (get_linkedit_data_command bytes offset)
    | LC_DYLD_ENVIRONMENT ->
      LC_DYLD_ENVIRONMENT (get_dylinker_command bytes offset)
    | LC_MAIN ->
      LC_MAIN (get_entry_point_command bytes offset)
    | LC_DATA_IN_CODE ->
      LC_DATA_IN_CODE (get_linkedit_data_command bytes offset)
    | LC_SOURCE_VERSION ->
      LC_SOURCE_VERSION (get_source_version_command bytes offset)
    | LC_DYLIB_CODE_SIGN_DRS ->
      LC_DYLIB_CODE_SIGN_DRS (get_linkedit_data_command bytes offset)
    | LC_ENCRYPTION_INFO_64 ->
      LC_ENCRYPTION_INFO_64 (get_encryption_info_command_64 bytes offset)
    | LC_LINKER_OPTION ->
      LC_LINKER_OPTION (get_linkedit_data_command bytes offset)
    | LC_LINKER_OPTIMIZATION_HINT ->
      LC_LINKER_OPTIMIZATION_HINT (get_linkedit_data_command bytes offset)

