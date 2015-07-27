open ByteCoverage

let debug = false

let compute_program_header_coverage phs m =
  (* TODO: make this more DRY and finish up known coverage *)
  if (ElfProgramHeader.is_empty phs) then
    m
  else
    begin
      match ElfProgramHeader.get_interpreter_header phs with
      | Some ph ->
        let size = ph.ElfProgramHeader.p_filesz in
        let range_start = ph.ElfProgramHeader.p_offset in
        let range_end = size + range_start in
        let extra = ElfProgramHeader.ptype_to_string ph.ElfProgramHeader.p_type in
        ByteCoverage.Map.add range_start
          {size; understood = true; kind = String;
           range_start; range_end; extra;} m
      | None ->
        m
    end
    |> 
    begin
      match ElfProgramHeader.get_header ElfProgramHeader.kPT_NOTE phs with
      | Some ph ->
        (
          fun m -> 
            let size = ph.ElfProgramHeader.p_filesz in
            let range_start = ph.ElfProgramHeader.p_offset in
            let range_end = size + range_start in
            let extra = ElfProgramHeader.ptype_to_string ph.ElfProgramHeader.p_type in
            ByteCoverage.Map.add range_start
              {size; understood = true; kind = String;
               range_start; range_end; extra} m
        )
      | None ->
        (fun m -> m)
    end
    |>
    begin
      match ElfProgramHeader.get_dynamic_program_header phs with
      | Some ph ->
        (
          fun m -> 
            let size = ph.ElfProgramHeader.p_filesz in
            let range_start = ph.ElfProgramHeader.p_offset in
            let range_end = size + range_start in
            let extra = ElfProgramHeader.ptype_to_string ph.ElfProgramHeader.p_type in
            ByteCoverage.Map.add range_start
              {size; understood = true; kind = Symbol;
               range_start; range_end; extra} m
        )
      | None ->
        (fun m -> m)
    end

let compute_section_coverage kind map section =
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
  ByteCoverage.Map.add range_start
    {size; understood = true; kind;
     range_start; range_end; extra} map

let known_sections =
  [(ElfSectionHeader.kSHT_SYMTAB, Symbol);
   (ElfSectionHeader.kSHT_STRTAB, StringTable);
   (ElfSectionHeader.kSHT_PROGBITS, Code);
   (ElfSectionHeader.kSHT_NOBITS, Code);
   (ElfSectionHeader.kSHT_INIT_ARRAY, Code);
   (ElfSectionHeader.kSHT_FINI_ARRAY, Code);
   (ElfSectionHeader.kSHT_DYNAMIC, Symbol);
   (ElfSectionHeader.kSHT_HASH, Symbol);
   (ElfSectionHeader.kSHT_RELA, Rela);
  ]

let compute_section_header_coverage h shs m =
  if (ElfSectionHeader.is_empty shs) then
    m
  else
    begin
      let size = h.ElfHeader.e_shentsize * h.ElfHeader.e_shnum in
      let range_start = h.ElfHeader.e_shoff in
      let range_end = size + range_start in
      ByteCoverage.Map.add range_start
        {size; understood = true;
         kind = Meta;
         range_start; range_end;
         extra = "section headers meta data"} m
    end
    |>
    begin
       let f = (fun m (section_type, kind) ->
          let sections = ElfSectionHeader.get_sections section_type shs in
          List.fold_left (compute_section_coverage kind) m sections
        ) in
      (fun m -> List.fold_left f m known_sections)
    end

let compute_byte_coverage h phs shs : ByteCoverage.t = 
  let size = 
    h.ElfHeader.e_ehsize + 
    (h.ElfHeader.e_phentsize * h.ElfHeader.e_phnum) 
  in
  let m =
    ByteCoverage.Map.add 0
      {size;
       understood = true;
       kind = Meta; range_start = 0;
       range_end = size;
       extra = "header + program headers meta data"}
      ByteCoverage.Map.empty
  |> compute_program_header_coverage phs
  |> compute_section_header_coverage h shs in m
