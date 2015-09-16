open PEHeader

type section_table = {
    name: string [@size 8];
    virtual_size: int [@size 4];
    virtual_address: int [@size 4];
    size_of_raw_data: int [@size 4];
    pointer_to_raw_data: int [@size 4];
    pointer_to_relocations: int [@size 4];
    pointer_to_linenumbers: int [@size 4];
    number_of_relocations: int [@size 2];
    number_of_linenumbers: int [@size 2];
    characteristics: int [@size 4];
  }

let sizeof_section_table = 8 * 5

let pp_section_table ppf section =
  Format.fprintf ppf
    "@[<v 2>%s@ VirtualSize: 0x%x@ VirtualAddress: 0x%x@ SizeOofRawData: 0x%x@ PointerToRawData: 0x%x@ PointerToRelocations: 0x%x@ PointerToLinenumbers: 0x%x@ NumberOfRelocations: 0x%x@ NumberOfLinenumbers: 0x%x@ Characteristics: 0x%x@]"
    section.name
    section.virtual_size
    section.virtual_address
    section.size_of_raw_data
    section.pointer_to_raw_data
    section.pointer_to_relocations
    section.pointer_to_linenumbers
    section.number_of_relocations
    section.number_of_linenumbers
    section.characteristics

type t = section_table list

let pp ppf t =
  Format.fprintf ppf "@ @[<v 2>Section Tables@ ";
  RdrPrinter.pp_seq ppf pp_section_table t

let rec get_section name sections =
  match sections with
  | [] -> raise Not_found
  | section::sections ->
     if (section.name = name) then
       section
     else
       get_section name sections

let get_section_table binary offset :section_table =
  let name,o = Binary.stringo binary offset ~num_bytes:8 in
  let virtual_size,o = Binary.u32o binary o in
  let virtual_address,o = Binary.u32o binary o in
  let size_of_raw_data,o = Binary.u32o binary o in
  let pointer_to_raw_data,o = Binary.u32o binary o in
  let pointer_to_relocations,o = Binary.u32o binary o in
  let pointer_to_linenumbers,o = Binary.u32o binary o in
  let number_of_relocations,o = Binary.u16o binary o in
  let number_of_linenumbers,o = Binary.u16o binary o in
  let characteristics = Binary.u32 binary o in
  {name;virtual_size;virtual_address;size_of_raw_data;pointer_to_raw_data;pointer_to_relocations;pointer_to_linenumbers;number_of_relocations;number_of_linenumbers;characteristics;}

let get binary (header:PEHeader.t) =
  let offset =
    header.dos_header.pe_pointer + PEHeader.sizeof_coff_header
    + header.coff_header.size_of_optional_header
  in
  let nsections = header.coff_header.number_of_sections in
  let rec loop acc count =
    if (count >= nsections) then
      List.rev acc
    else
      let o = offset + (count * sizeof_section_table) in
      let st = get_section_table binary o in
      loop (st::acc) (count+1)
  in loop [] 0
