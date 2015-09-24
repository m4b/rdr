(* standard COFF fields *)
type standard_fields =
  {
    magic: int [@size 2];
    major_linker_version: int [@size 1];
    minor_linker_version: int [@size 1];
    size_of_code: int [@size 4];
    size_of_initialized_data: int [@size 4];
    size_of_uninitialized_data: int [@size 4];
    address_of_entry_point: int [@size 4];
    base_of_code: int [@size 4];
    base_of_data: int [@size 4]; (* absent in 64-bit PE32+ *)
  }

let sizeof_standard_fields = (3 * 8) + 4

let pp_standard_fields ppf standard =
  Format.fprintf ppf
    "@[<v 2>@[<h>Standard@ 0x%x@]@ MajorLinkerVersion: %d@ MinorLinkerVersion: %d@ SizeOfCode: 0x%x@ SizeOfInitializedData: 0x%x@ SizeOfUninitializedData: 0x%x@ AddressOfEntryPoint: 0x%x@ BaseOfCode: 0x%x@ BaseOfData: 0x%x@]"
    standard.magic
    standard.major_linker_version
    standard.minor_linker_version
    standard.size_of_code
    standard.size_of_initialized_data
    standard.size_of_uninitialized_data
    standard.address_of_entry_point
    standard.base_of_code
    standard.base_of_data

(* windows specific fields *)
type windows_fields =
  {
    image_base: int [@size 4];  (* 8 in 64-bit *)
    section_alignment: int [@size 4];
    file_alignment: int [@size 4];
    major_operating_system_version: int [@size 2];
    minor_operating_system_version: int [@size 2];
    major_image_version: int [@size 2];
    minor_image_version: int [@size 2];
    major_subsystem_version: int [@size 2];
    minor_subsystem_version: int [@size 2];
    win32_version_value: int [@size 4];
    size_of_image: int [@size 4];
    size_of_headers: int [@size 4];
    check_sum: int [@size 4];
    subsystem: int [@size 2];
    dll_characteristics: int [@size 2];
    size_of_stack_reserve: int [@size 4]; (* 8 64-bit *)
    size_of_stack_commit: int [@size 4];  (* 8 *)
    size_of_heap_reserve: int [@size 4];  (* 8 *)
    size_of_heap_commit: int [@size 4];   (* 8 *)
    loader_flags: int [@size 4];
    number_of_rva_and_sizes: int [@size 4];
  }

let sizeof_windows_fields = (8 * 8) + 4

let pp_windows_fields ppf windows =
  Format.fprintf ppf
    "@[<v 2>@[Windows@]@ ImageBase: 0x%x@ SectionAlignment: 0x%x@ FileAlignment: 0x%x@ MajorOperatingSystemVersion: %d@ MinorOperatingSystemVersion: %d@ MajorImageVersion: %d@ MinorImageVersion: %d@ MajorSubsystemVersion: %d@ MinorSubsystemVersion: %d@ Win32VersionValue: %d@ SizeOfImage: 0x%x@ SizeOfHeaders: 0x%x@ CheckSum: 0x%x@ Subsystem: 0x%x@ DLLCharacteristics: 0x%x@ SizeOfStackReserve: 0x%x@ SizeOfStackCommit: 0x%x@ SizeOfHeapReserve: 0x%x@ SizeOfHeapCommit: 0x%x@ LoaderFlags: 0x%x@ NumberOfRvaAndSizes: %d@]"
    windows.image_base
    windows.section_alignment
    windows.file_alignment
    windows.major_operating_system_version
    windows.minor_operating_system_version
    windows.major_image_version
    windows.minor_image_version
    windows.major_subsystem_version
    windows.minor_subsystem_version
    windows.win32_version_value
    windows.size_of_image
    windows.size_of_headers
    windows.check_sum
    windows.subsystem
    windows.dll_characteristics
    windows.size_of_stack_reserve
    windows.size_of_stack_commit
    windows.size_of_heap_reserve
    windows.size_of_heap_commit
    windows.loader_flags
    windows.number_of_rva_and_sizes

type t =
  {
    standard_fields: standard_fields;
    windows_fields: windows_fields;
    data_directories: PEDataDirectories.t;
  }

let pp ppf header =
  Format.fprintf ppf "@ ";
  pp_standard_fields ppf header.standard_fields;
  Format.fprintf ppf "@ ";
  pp_windows_fields ppf header.windows_fields;
  Format.fprintf ppf "@ ";
  PEDataDirectories.pp ppf header.data_directories

let get_standard_fields binary offset :standard_fields =
  let magic,o = Binary.u16o binary offset in
  let major_linker_version,o = Binary.u8o binary o in
  let minor_linker_version,o = Binary.u8o binary o in
  let size_of_code,o = Binary.u32o binary o in
  let size_of_initialized_data,o = Binary.u32o binary o in
  let size_of_uninitialized_data,o = Binary.u32o binary o in
  let address_of_entry_point,o = Binary.u32o binary o in
  let base_of_code,o = Binary.u32o binary o in
  let base_of_data = Binary.u32 binary o in
  {magic;major_linker_version;minor_linker_version;size_of_code;size_of_initialized_data;size_of_uninitialized_data;address_of_entry_point;base_of_code;base_of_data;}

let get_windows_fields binary offset :windows_fields =
  let image_base,o = Binary.u32o binary offset in
  let section_alignment,o = Binary.u32o binary o in
  let file_alignment,o = Binary.u32o binary o in
  let major_operating_system_version,o = Binary.u16o binary o in
  let minor_operating_system_version,o = Binary.u16o binary o in
  let major_image_version,o = Binary.u16o binary o in
  let minor_image_version,o = Binary.u16o binary o in
  let major_subsystem_version,o = Binary.u16o binary o in
  let minor_subsystem_version,o = Binary.u16o binary o in
  let win32_version_value,o = Binary.u32o binary o in
  let size_of_image,o = Binary.u32o binary o in
  let size_of_headers,o = Binary.u32o binary o in
  let check_sum,o = Binary.u32o binary o in
  let subsystem,o = Binary.u16o binary o in
  let dll_characteristics,o = Binary.u16o binary o in
  let size_of_stack_reserve,o = Binary.u32o binary o in
  let size_of_stack_commit,o = Binary.u32o binary o in
  let size_of_heap_reserve,o = Binary.u32o binary o in
  let size_of_heap_commit,o = Binary.u32o binary o in
  let loader_flags,o = Binary.u32o binary o in
  let number_of_rva_and_sizes = Binary.u32 binary o in
  {image_base;section_alignment;file_alignment;major_operating_system_version;minor_operating_system_version;major_image_version;minor_image_version;major_subsystem_version;minor_subsystem_version;win32_version_value;size_of_image;size_of_headers;check_sum;subsystem;dll_characteristics;size_of_stack_reserve;size_of_stack_commit;size_of_heap_reserve;size_of_heap_commit;loader_flags;number_of_rva_and_sizes;}

let get binary offset =
  let standard_fields =
    get_standard_fields binary offset
  in
  let wf_offset = offset + sizeof_standard_fields in
  let windows_fields = get_windows_fields binary wf_offset in
  let dd_offset = wf_offset + sizeof_windows_fields in
  let data_directories = 
    PEDataDirectories.get
      binary
      dd_offset
      windows_fields.number_of_rva_and_sizes
  in
  {standard_fields; windows_fields; data_directories;}

