open ByteCoverage

let debug = false

let known_program_headers =
  [(ElfProgramHeader.kPT_INTERP, String);
   (ElfProgramHeader.kPT_NOTE, String);
   (ElfProgramHeader.kPT_DYNAMIC, Symbol);
  ]

let compute_program_header_coverage kind data ph =
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
    (create_data
    ~tag:String
    ~r1:range_start
    ~r2:range_end
    ~extra:extra
    ~understood:true
    ) data
  | None -> data

let compute_program_header_coverage phs data =
  if (ElfProgramHeader.is_empty phs) then
    data
  else
    let f = (fun data (header_type, tag) ->
        let header = ElfProgramHeader.get_header header_type phs in
        compute_program_header_coverage tag data header
      ) in
    (fun data -> List.fold_left f data known_program_headers) data

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
   
let compute_section_coverage stype tag data section =
  if (debug) then
    Printf.printf "called: %s\n"
      (ElfSectionHeader.shtype_to_string
         section.ElfSectionHeader.sh_type);
  let size = section.ElfSectionHeader.sh_size in
  let range_start = section.ElfSectionHeader.sh_offset in
  let range_end = 
    if (stype = ElfSectionHeader.kSHT_NOBITS) then
      range_start
    else
      size + range_start
  in
  let extra =
    section.ElfSectionHeader.name ^ " // " ^
    ElfSectionHeader.shtype_to_string
      section.ElfSectionHeader.sh_type
  in
  ByteCoverage.add
    (create_data
    ~tag:tag
    ~r1:range_start
    ~r2:range_end
    ~extra:extra
    ~understood:true
    ) data

(* add a platform specific post process, for e.g. NOBITS *)
let compute_section_header_coverage h shs data =
  if (ElfSectionHeader.is_empty shs) then
    data
  else
    begin
      let size = h.ElfHeader.e_shentsize * h.ElfHeader.e_shnum in
      let range_start = h.ElfHeader.e_shoff in
      let range_end = size + range_start in
      ByteCoverage.add
        (create_data
           ~tag:Meta
           ~r1:range_start
           ~r2:range_end
           ~extra:"Section Headers"
           ~understood:true
        ) data
    end
    |>
    begin
       let f = (fun data (section_type, tag) ->
          let sections = ElfSectionHeader.get_sections section_type shs in
          List.fold_left (compute_section_coverage section_type tag) data sections
        ) in
      (fun data -> List.fold_left f data known_sections)
    end

let compute_byte_coverage h phs shs elf_size : ByteCoverage.t =
  let ehsize = h.ElfHeader.e_ehsize in
  let phsize = 
    (h.ElfHeader.e_phentsize * h.ElfHeader.e_phnum)
  in
  ByteCoverage.add
    (create_data
       ~tag:Meta
       ~r1:0
       ~r2:h.ElfHeader.e_ehsize
       ~extra:"ELF Header"
       ~understood:true
    )
    ByteCoverage.empty
  |> ByteCoverage.add
    (create_data
       ~tag:Meta
       ~r1:ehsize
       ~r2:(phsize+ehsize)
       ~extra:"Program Headers"
       ~understood:true
    )
  |> compute_program_header_coverage phs
  |> compute_section_header_coverage h shs
  |> ByteCoverage.create elf_size
