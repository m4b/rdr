open Binary

type dos_header =
  {
    signature: int [@size 2]; (* 5a5d *)
    pe_pointer: int [@size 4];  (* at offset 0x3c *)
  }

type coff_header =
  {
    (* COFF Headers *)
    signature: int [@size 4, be]; (* 0x50450000 *)
    machine: int [@size 2];
    number_of_sections: int [@size 2];
    time_date_stamp: int [@size 4];
    pointer_to_symbol_table: int [@size 4];
    number_of_symbol_table: int [@size 4];
    size_of_optional_header: int [@size 2];
    characteristics: int [@size 2];
    (* standard COFF fields *)
    magic: int [@size 2];
    major_linker_version: int [@size 1];
    minor_linker_version: int [@size 1];
    size_of_code: int [@size 4];
    size_of_initialized_data: int [@size 4];
    size_of_uninitialized_data: int [@size 4];
    address_of_entry_point: int [@size 4];
    base_of_code: int [@size 4];
    base_of_data: int [@size 4];
    (* windows specific fields *)
    image_base: int [@size 4];
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
    size_of_stack_reserve: int [@size 4];
    size_of_stack_commit: int [@size 4];
    size_of_heap_reserve: int [@size 4];
    size_of_heap_commit: int [@size 4];
    loader_flags: int [@size 4];
    number_of_rva_and_sizes: int [@size 4];
  }

type t = {
    dos_header: dos_header;
    coff_header: coff_header;
  }

let get_dos_header binary offset :dos_header =
  let signature,o = Binary.u16o binary offset in
  let pe_pointer = Binary.u32 binary (offset+0x3c) in
  {signature;pe_pointer;}

let get_coff_header binary offset :coff_header =
  let signature,o = Binary.u32o binary offset in
  let machine,o = Binary.u16o binary o in
  let number_of_sections,o = Binary.u16o binary o in
  let time_date_stamp,o = Binary.u32o binary o in
  let pointer_to_symbol_table,o = Binary.u32o binary o in
  let number_of_symbol_table,o = Binary.u32o binary o in
  let size_of_optional_header,o = Binary.u16o binary o in
  let characteristics,o = Binary.u16o binary o in
  let magic,o = Binary.u16o binary o in
  let major_linker_version,o = Binary.u8o binary o in
  let minor_linker_version,o = Binary.u8o binary o in
  let size_of_code,o = Binary.u32o binary o in
  let size_of_initialized_data,o = Binary.u32o binary o in
  let size_of_uninitialized_data,o = Binary.u32o binary o in
  let address_of_entry_point,o = Binary.u32o binary o in
  let base_of_code,o = Binary.u32o binary o in
  let base_of_data,o = Binary.u32o binary o in
  let image_base,o = Binary.u32o binary o in
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
  {signature;machine;number_of_sections;time_date_stamp;pointer_to_symbol_table;number_of_symbol_table;size_of_optional_header;characteristics;magic;major_linker_version;minor_linker_version;size_of_code;size_of_initialized_data;size_of_uninitialized_data;address_of_entry_point;base_of_code;base_of_data;image_base;section_alignment;file_alignment;major_operating_system_version;minor_operating_system_version;major_image_version;minor_image_version;major_subsystem_version;minor_subsystem_version;win32_version_value;size_of_image;size_of_headers;check_sum;subsystem;dll_characteristics;size_of_stack_reserve;size_of_stack_commit;size_of_heap_reserve;size_of_heap_commit;loader_flags;number_of_rva_and_sizes;}

let get_header binary =
  let dos_header = get_dos_header binary 0 in
  let coff_header = get_coff_header binary dos_header.pe_pointer in
  {dos_header; coff_header}
