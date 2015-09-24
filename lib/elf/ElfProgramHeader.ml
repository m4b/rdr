type program_header32 =
  {
    p_type: int [@size 4];
    p_offset: int [@size 4];
    p_vaddr: int [@size 4];
    p_paddr: int [@size 4];
    p_filesz: int [@size 4];
    p_memsz: int [@size 4];
    p_flags: int [@size 4];
    p_align: int [@size 4];
  }

let sizeof_program_header32 = 8 * 4

(* 64 bit *)
type program_header =
  {
    p_type: int [@size 4];
    p_flags: int [@size 4];
    p_offset: int [@size 8];
    p_vaddr: int [@size 8];
    p_paddr: int [@size 8];
    p_filesz: int [@size 8];
    p_memsz: int [@size 8];
    p_align: int [@size 8];
  }

let sizeof_program_header = 56 	(* bytes *)

type t = program_header list

type t32 = program_header32 list

(* p type *)
let kPT_NULL =		0		(* Program header table entry unused *)
let kPT_LOAD =		1		(* Loadable program segment *)
let kPT_DYNAMIC =	2		(* Dynamic linking information *)
let kPT_INTERP =	3		(* Program interpreter *)
let kPT_NOTE =		4		(* Auxiliary information *)
let kPT_SHLIB =	        5		(* Reserved *)
let kPT_PHDR =		6		(* Entry for header table itself *)
let kPT_TLS =		7		(* Thread-local storage segment *)
let kPT_NUM =		8		(* Number of defined types *)
let kPT_LOOS =		0x60000000	(* Start of OS-specific *)
let kPT_GNU_EH_FRAME =	0x6474e550	(* GCC .eh_frame_hdr segment *)
let kPT_GNU_STACK    =  0x6474e551	(* Indicates stack executability *)
let kPT_GNU_RELRO    =	0x6474e552	(* Read-only after relocation *)
let kPT_PAX_FLAGS    =	0x65041580	(* pax security header, _not_ in ELF header *)
let kPT_LOSUNW	= 0x6ffffffa
let kPT_SUNWBSS	 = 0x6ffffffa	(* Sun Specific segment *)
let kPT_SUNWSTACK = 0x6ffffffb	(* Stack segment *)
let kPT_HISUNW	= 0x6fffffff
let kPT_HIOS	= 0x6fffffff	(* End of OS-specific *)
let kPT_LOPROC	= 0x70000000	(* Start of processor-specific *)
let kPT_HIPROC	= 0x7fffffff	(* End of processor-specific *)

let kPT_MIPS_REGINFO = 0x70000000	(* Register usage information *)
let kPT_MIPS_RTPROC =  0x70000001	(* Runtime procedure table. *)
let kPT_MIPS_OPTIONS = 0x70000002
let kPT_HP_TLS = (kPT_LOOS + 0x0)
let kPT_HP_CORE_NONE =	(kPT_LOOS + 0x1)
let kPT_HP_CORE_VERSION = (kPT_LOOS + 0x2)
let kPT_HP_CORE_KERNEL = (kPT_LOOS + 0x3)
let kPT_HP_CORE_COMM = 	(kPT_LOOS + 0x4)
let kPT_HP_CORE_PROC = 	(kPT_LOOS + 0x5)
let kPT_HP_CORE_LOADABLE = (kPT_LOOS + 0x6)
let kPT_HP_CORE_STACK = (kPT_LOOS + 0x7)
let kPT_HP_CORE_SHM = (kPT_LOOS + 0x8)
let kPT_HP_CORE_MMF = (kPT_LOOS + 0x9)
let kPT_HP_PARALLEL = (kPT_LOOS + 0x10)
let kPT_HP_FASTBIND = (kPT_LOOS + 0x11)
let kPT_HP_OPT_ANNOT = (kPT_LOOS + 0x12)
let kPT_HP_HSL_ANNOT = (kPT_LOOS + 0x13)
let kPT_HP_STACK = (kPT_LOOS + 0x14)
(* we don't care about this, and it aliases anyway
let kPT_PARISC_ARCHEXT = 0x70000000
let kPT_PARISC_UNWIND =	0x70000001
*)
let kPPC64_OPT_TLS = 1
let kPPC64_OPT_MULTI_TOC = 2

let kPT_ARM_EXIDX = (kPT_LOPROC + 1)	(* ARM unwind segment.  *)
let kPT_IA_64_ARCHEXT = (kPT_LOPROC + 0)	(* arch extension bits *)
let kPT_IA_64_UNWIND = (kPT_LOPROC + 1)	(* ia64 unwind bits *)
let kPT_IA_64_HP_OPT_ANOT = (kPT_LOOS + 0x12)
let kPT_IA_64_HP_HSL_ANOT = (kPT_LOOS + 0x13)
let kPT_IA_64_HP_STACK = (kPT_LOOS + 0x14)

let get_program_header32 binary offset :program_header32 =
  let p_type,o = Binary.u32o binary offset in (* &i *)
  let p_offset,o = Binary.u32o binary o in
  let p_vaddr,o = Binary.u32o binary o in
  let p_paddr,o = Binary.u32o binary o in
  let p_filesz,o = Binary.u32o binary o in
  let p_memsz,o = Binary.u32o binary o in
  let p_flags,o = Binary.u32o binary o in
  let p_align,_ = Binary.u32o binary o in
  {
    p_type;
    p_offset;
    p_vaddr;
    p_paddr;
    p_filesz;
    p_memsz;
    p_flags;
    p_align;
  }

let get_program_header binary offset =
  let p_type,o = Binary.u32o binary offset in
  let p_flags,o = Binary.u32o binary o in
  let p_offset,o = Binary.u64o binary o in
  let p_vaddr,o = Binary.u64o binary o in
  let p_paddr,o = Binary.u64o binary o in
  let p_filesz,o = Binary.u64o binary o in
  let p_memsz,o = Binary.u64o binary o in
  let p_align,_ = Binary.u64o binary o in
  {
    p_type;
    p_flags; (* 1=x 2=w 4=r *)
    p_offset;
    p_vaddr;
    p_paddr;
    p_filesz;
    p_memsz;
    p_align;
  }

let ptype_to_string ptype =
  match ptype with
  | 0 -> "PT_NULL"
  | 1 -> "PT_LOAD"
  | 2 -> "PT_DYNAMIC"
  | 3 -> "PT_INTERP"
  | 4 -> "PT_NOTE"
  | 5 -> "PT_SHLIB"
  | 6 -> "PT_PHDR"
  | 7 -> "PT_TLS"
  | 8 -> "PT_NUM"
  (*   | 0x60000000 -> "PT_LOOS" *)
  | 0x6474e550 -> "PT_GNU_EH_FRAME"
  | 0x6474e551 -> "PT_GNU_STACK"
  | 0x6474e552 -> "PT_GNU_RELRO"
  | 0x65041580 -> "PT_PAX_FLAGS"
  (*   | 0x6ffffffa -> "PT_LOSUNW" *)
  | 0x6ffffffa -> "PT_SUNWBSS"
  | 0x6ffffffb -> "PT_SUNWSTACK"
  (*   | 0x6fffffff -> "PT_HIOS" *)
  | pt when pt = kPT_MIPS_REGINFO -> "PT_MIPS_REGINFO"
  | pt when pt = kPT_MIPS_RTPROC -> "PT_MIPS_RTPROC"
  | pt when pt = kPT_MIPS_OPTIONS -> "PT_MIPS_OPTIONS"
  | pt when pt = kPT_HP_TLS -> "PT_HP_TLS"
  | pt when pt = kPT_HP_CORE_NONE -> "PT_HP_CORE_NONE"
  | pt when pt = kPT_HP_CORE_VERSION -> "PT_HP_CORE_VERSION"
  | pt when pt = kPT_HP_CORE_KERNEL -> "PT_HP_CORE_KERNEL"
  | pt when pt = kPT_HP_CORE_COMM -> "PT_HP_CORE_COMM"
  | pt when pt = kPT_HP_CORE_PROC -> "PT_HP_CORE_PROC"
  | pt when pt = kPT_HP_CORE_LOADABLE -> "PT_HP_CORE_LOADABLE"
  | pt when pt = kPT_HP_CORE_STACK -> "PT_HP_CORE_STACK"
  | pt when pt = kPT_HP_CORE_SHM -> "PT_HP_CORE_SHM"
  | pt when pt = kPT_HP_CORE_MMF -> "PT_HP_CORE_MMF"
  | pt when pt = kPT_HP_PARALLEL -> "PT_HP_PARALLEL"
  | pt when pt = kPT_HP_FASTBIND -> "PT_HP_FASTBIND"
  (* PT_LOOS + 0x12 *)
  | pt when pt = kPT_IA_64_HP_OPT_ANOT -> "PT_IA_64_HP_OPT_ANOT"
  | pt when pt = kPT_IA_64_HP_HSL_ANOT -> "PT_IA_64_HP_HSL_ANOT"
  | pt when pt = kPT_IA_64_HP_STACK -> "PT_IA_64_HP_STACK"
  (* PT_LOOS + 0x14 *)
  | pt when pt = kPT_HP_OPT_ANNOT -> "PT_HP_OPT_ANNOT"
  | pt when pt = kPT_HP_HSL_ANNOT -> "PT_HP_HSL_ANNOT"
  | pt when pt = kPT_HP_STACK -> "PT_HP_STACK"
  (* | 0x70000000 -> "PT_LOPROC" *)
  | pt when pt = kPT_ARM_EXIDX -> "PT_ARM_EXIDX"
  | pt when pt = kPT_IA_64_ARCHEXT -> "PT_IA_64_ARCHEXT"
  | pt when pt = kPT_IA_64_UNWIND -> "PT_IA_64_UNWIND"
  (* | 0x7fffffff -> "PT_HIPROC" *)
  | pt -> Printf.sprintf "PT_UNKNOWN 0x%x" pt

let flags_to_string flags =
  match flags with
  | 1 -> "X"
  | 2 -> "W"
  | 3 -> "W+X"
  | 4 -> "R"
  | 5 -> "R+X"
  | 6 -> "RW"
  | 7 -> "RW+X"
  | f -> Printf.sprintf "FLAG 0x%x" f

let is_empty phs = phs = []
	   
let program_header_to_string ph =
  Printf.sprintf "%-12s %-4s offset: 0x%04x vaddr: 0x%08x paddr: 0x%08x filesz: 0x%04x memsz: 0x%04x align: %d"
		 (ptype_to_string ph.p_type)
		 (flags_to_string ph.p_flags) (* 6 r/w 5 r/x *)
		 ph.p_offset
		 ph.p_vaddr
		 ph.p_paddr
		 ph.p_filesz
		 ph.p_memsz
		 ph.p_align
		 
let print_program_headers phs =
  Printf.printf "Program Headers (%d):\n" @@ List.length phs;
  List.iteri (fun i ph -> Printf.printf "%s\n" @@ program_header_to_string ph) phs
	    
let get_program_headers32 binary phoff phentsize phnum :t32 =
  let rec loop count offset acc =
    if (count >= phnum) then
      List.rev acc
    else
      let ph = get_program_header32 binary offset in
      loop (count + 1) (offset + phentsize) (ph::acc)
  in loop 0 phoff []

let get_program_headers binary phoff phentsize phnum =
  let rec loop count offset acc =
    if (count >= phnum) then
      List.rev acc
    else
      let ph = get_program_header binary offset in
      loop (count + 1) (offset + phentsize) (ph::acc)
  in loop 0 phoff []

let get_header32 (ph:int) (phs:t32) =
  try Some (List.find (fun (elem:program_header32) -> (elem.p_type = ph)) phs)
  with _ -> None

let get_main_program_header32 phs = get_header32 kPT_PHDR phs

let get_interpreter_header32 phs = get_header32 kPT_INTERP phs

let get_dynamic_program_header32 phs = get_header32 kPT_DYNAMIC phs

let get_interpreter32 binary phs =
  match get_interpreter_header32 phs with
  | Some ph ->
    Binary.string binary ~max:ph.p_filesz ph.p_offset
  | None -> ""

let get_header ph phs =
  try Some (List.find (fun elem -> (elem.p_type = ph)) phs)
  with _ -> None

let get_main_program_header phs = get_header kPT_PHDR phs

let get_interpreter_header phs = get_header kPT_INTERP phs

let get_dynamic_program_header phs = get_header kPT_DYNAMIC phs

let get_interpreter binary phs =
  match get_interpreter_header phs with
  | Some ph ->
    Binary.string binary ~max:ph.p_filesz ph.p_offset
  | None -> ""

(* TODO: move this to separate module, or replace the PEUtils similar functionality with a simple find_offset function *)
type slide_sector = {start_sector: int; end_sector: int; slide: int;}

let is_in_sector offset sector =
  (* should this be offset <= sector.end_sector ? *)
  offset >= sector.start_sector && offset < sector.end_sector

let is_contained_in s1 s2 =
  s1.start_sector >= s2.start_sector
  && s1.end_sector <= s2.end_sector

let join s1 s2 =
  let start_sector = min s1.start_sector s2.start_sector in
  let end_sector = max s1.end_sector s2.end_sector in
  assert (s1.slide = s2.slide);
  let slide = s1.slide in
  {start_sector; end_sector; slide}

let print_slide_sector sector =
  Printf.printf "0x%x: 0x%x - 0x%x\n"
		sector.slide sector.start_sector sector.end_sector

let print_slide_sectors sectors =
  List.iter (fun el -> print_slide_sector el) sectors
                
(* checks to see if the slides are equal; will this hold uniformly? *)
module SlideSet =
  Set.Make(
      struct type t = slide_sector
	     let compare =
	       (fun a b -> Pervasives.compare a.slide b.slide)
      end)

module Map = 
  Map.Make(struct
              type t = int
              let compare = compare
            end)

(* finds the vaddr masks *)
let get_slide_sectors phs =
  let map =
  List.fold_left 
    (fun acc ph ->
     let slide = ph.p_vaddr - ph.p_offset in
     if (slide <> 0) then
       let start_sector = ph.p_vaddr in
       let end_sector = start_sector + ph.p_filesz in (* this might need to be ph.p_memsz *)
       let s1 = {start_sector; end_sector; slide} in
       if (Map.mem slide acc) then
         let s2 = Map.find slide acc in
         if (is_contained_in s1 s2) then
           acc
         else
           Map.add slide (join s1 s2) acc
       else
         Map.add slide s1 acc
     else
       acc
    ) Map.empty phs 
  in
  Map.fold (fun k v acc -> v::acc) map []

(* checks if the offset is in the slide sector, 
 and adjusts using the sectors slide if so *)
let adjust sectors offset =
  List.fold_left (fun acc sector ->
		  if (is_in_sector offset sector) then
		    offset - sector.slide
		  else
		    acc) offset sectors

let set_program_header bytes ph offset =
  Binary.set_uint bytes ph.p_type 4 offset
  |> Binary.set_uint bytes ph.p_flags 4
  |> Binary.set_uint bytes ph.p_offset 8
  |> Binary.set_uint bytes ph.p_vaddr 8
  |> Binary.set_uint bytes ph.p_paddr 8
  |> Binary.set_uint bytes ph.p_filesz 8
  |> Binary.set_uint bytes ph.p_memsz 8
  |> Binary.set_uint bytes ph.p_align 8

let program_header_to_bytes ph =
  let b = Bytes.create sizeof_program_header in
  ignore @@ set_program_header b ph 0;
  b

let set bytes phs offset =
  (* fold over the list, accumulating the offset,
     and return the final offset *)
  List.fold_left (fun acc ph -> set_program_header bytes ph acc) offset phs

let to_bytes phs =
  let b = Bytes.create ((List.length phs) * sizeof_program_header) in
  ignore @@ set b phs 0;
  b

(* TODO: test to_bytes and set *)

(*
let get_p_type p_type =
  match p_type with
  | 0 -> PT_NULL		
  | 1 -> PT_LOAD		
  | 2 -> PT_DYNAMIC	
  | 3 -> PT_INTERP	
  | 4 -> PT_NOTE		
  | 5 -> PT_SHLIB	
  | 6 -> PT_PHDR		
  | 7 -> PT_TLS		
  | 8 -> PT_NUM		
  | 0x60000000 -> kPT_LOOS
  | 0x6474e550 -> PT_GNU_EH_FRAME	
  | 0x6474e551 -> PT_GNU_STACK	
  | 0x6474e552 -> PT_GNU_RELRO	
  | 0x6ffffffa -> PT_LOSUNW
  | 0x6ffffffa -> PT_SUNWBSS
  | 0x6ffffffb -> PT_SUNWSTACK
  | 0x6fffffff -> PT_HISUNW
  | 0x6fffffff -> PT_HIOS
  | 0x70000000 -> PT_LOPROC
  | 0x7fffffff -> PT_HIPROC
		  *)

