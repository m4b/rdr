open PEHeader

let rva2offset rva (section:PEHeader.section_table) =
  (rva - section.virtual_address) + section.pointer_to_raw_data

let is_in_section rva section =
  section.virtual_address <= rva
  && rva < (section.virtual_address + section.virtual_size)

let rec find_offset rva (sections:PEHeader.section_table list) =
  match sections with
  | [] -> raise Not_found
  | section::sections ->
     if (is_in_section rva section) then
       rva2offset rva section
     else
       find_offset rva sections

                                      
