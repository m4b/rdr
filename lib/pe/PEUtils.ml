open PESectionTable

let is_in_range rva r1 r2 =
  r1 <= rva
  && rva < r2

let rva2offset rva (section:section_table) =
  (rva - section.virtual_address) + section.pointer_to_raw_data

let is_in_section rva section =
  section.virtual_address <= rva
  && rva < (section.virtual_address + section.virtual_size)

let rec find_offset rva (sections:PESectionTable.t) =
  match sections with
  | [] -> raise Not_found
  | section::sections ->
     if (is_in_section rva section) then
       rva2offset rva section
     else
       find_offset rva sections

                                      
