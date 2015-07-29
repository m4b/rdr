open ByteCoverage

let debug = false

let known_program_headers =
  [(ElfProgramHeader.kPT_INTERP, String);
   (ElfProgramHeader.kPT_NOTE, String);
   (ElfProgramHeader.kPT_DYNAMIC, Symbol);
  ]

let compute_program_header_coverage kind coverage ph =
  match ph with
  | Some ph ->
    if (debug) then
      Printf.printf "called: %s\n"
        (ElfProgramHeader.ptype_to_string
           ph.ElfProgramHeader.p_type);
    let size = ph.ElfProgramHeader.p_filesz in
    let range_start = ph.ElfProgramHeader.p_offset in
    let range_end = size + range_start in
    let extra = ElfProgramHeader.ptype_to_string ph.ElfProgramHeader.p_type in
    ByteCoverage.add
      {size; understood = true; tag = String;
       range_start; range_end; extra;} coverage
  | None -> coverage

let compute_program_header_coverage phs coverage =
  if (ElfProgramHeader.is_empty phs) then
    coverage
  else
    let f = (fun coverage (header_type, tag) ->
        let header = ElfProgramHeader.get_header header_type phs in
        compute_program_header_coverage tag coverage header
      ) in
    (fun coverage -> List.fold_left f coverage known_program_headers) coverage
   
let compute_section_coverage tag coverage section =
  if (debug) then
    Printf.printf "called: %s\n"
      (ElfSectionHeader.shtype_to_string
         section.ElfSectionHeader.sh_type);
  let size = section.ElfSectionHeader.sh_size in
  let range_start = section.ElfSectionHeader.sh_offset in
  let range_end = size + range_start in
  let extra =
    section.ElfSectionHeader.name ^ " // " ^
    ElfSectionHeader.shtype_to_string
      section.ElfSectionHeader.sh_type
  in
  ByteCoverage.add
    {size; understood = true; tag;
     range_start; range_end; extra} coverage

let known_sections =
  [(ElfSectionHeader.kSHT_SYMTAB, Symbol);
   (ElfSectionHeader.kSHT_STRTAB, StringTable);
   (ElfSectionHeader.kSHT_PROGBITS, Code);
   (ElfSectionHeader.kSHT_NOBITS, Semantic);
   (ElfSectionHeader.kSHT_INIT_ARRAY, Code);
   (ElfSectionHeader.kSHT_FINI_ARRAY, Code);
   (ElfSectionHeader.kSHT_DYNAMIC, Symbol);
   (ElfSectionHeader.kSHT_HASH, Symbol);
   (ElfSectionHeader.kSHT_RELA, Rela);
  ]

(* add a platform specific post process, for e.g. NOBITS *)
let compute_section_header_coverage h shs coverage =
  if (ElfSectionHeader.is_empty shs) then
    coverage
  else
    begin
      let size = h.ElfHeader.e_shentsize * h.ElfHeader.e_shnum in
      let range_start = h.ElfHeader.e_shoff in
      let range_end = size + range_start in
      ByteCoverage.add
        {size; understood = true;
         tag = Meta;
         range_start; range_end;
         extra = "section headers meta data"} coverage
    end
    |>
    begin
       let f = (fun m (section_type, tag) ->
          let sections = ElfSectionHeader.get_sections section_type shs in
          List.fold_left (compute_section_coverage tag) m sections
        ) in
      (fun m -> List.fold_left f m known_sections)
    end

let compute_byte_coverage h phs shs elf_size : ByteCoverage.t = 
  let size = 
    h.ElfHeader.e_ehsize + 
    (h.ElfHeader.e_phentsize * h.ElfHeader.e_phnum) 
  in
  ByteCoverage.add
    {size;
     understood = true;
     tag = Meta; range_start = 0;
     range_end = size;
     extra = "header + program headers meta data"}
    ByteCoverage.empty
  |> compute_program_header_coverage phs
  |> compute_section_header_coverage h shs
  |> ByteCoverage.create elf_size
