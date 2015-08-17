(* TODO add Import, Export, and Symbols/Nlist *)
(* TODO move ToL back to Goblin and tease out deps *)

(* hacky function to filter imports from exports, etc. *)
(* todo use proper variants here ffs *)

open ElfSymbolTable

let get_goblin_kind entry bind stype =
  if (entry.ElfSymbolTable.st_value = 0x0
      && entry.ElfSymbolTable.st_shndx = 0
      && entry.ElfSymbolTable.name <> "") (* ignore first \0 entry *)
  then
    GoblinSymbol.Import
  else if (bind = "LOCAL") then
    GoblinSymbol.Local
  else if ((bind = "GLOBAL"
	    || (bind = "WEAK" && (stype = "FUNC"
				  || stype = "IFUNC"
				  || stype = "OBJECT")))
	   && entry.ElfSymbolTable.st_value <> 0) then
    GoblinSymbol.Export
  else GoblinSymbol.Other

(* 
(* polymorphic variants don't need to be qualified by module
   since they are open and the symbol is unique *)
let symbol_entry_to_goblin_symbol
    ~tol:tol ~libs:libs ~relocs:relocs (soname,install_name) index entry =
  let bind   = (ElfSymbolTable.get_bind entry.ElfSymbolTable.st_info |> ElfSymbolTable.symbol_bind_to_string) in
  let stype  = (ElfSymbolTable.get_type entry.ElfSymbolTable.st_info |> ElfSymbolTable.symbol_type_to_string) in
  let name   = `Name entry.ElfSymbolTable.name in
  let offset =
    `Offset (
      if (entry.ElfSymbolTable.st_value = 0) then
	(* this _could_ be relatively expensive *)
	Elf.Reloc.get_size index relocs
      else
	entry.ElfSymbolTable.st_value)
  in
  let size = `Size entry.ElfSymbolTable.st_size in
  let kind = `Kind (get_goblin_kind entry bind stype) in
  let lib =
    (* TODO: this is a complete disaster; *)
    match kind with
    | `Kind GoblinSymbol.Export ->
      `Lib (soname,install_name)
    | `Kind GoblinSymbol.Import ->
      if (ToL.is_empty tol) then
	`Lib ("∅","∅")
      else
	let l = (ToL.get_libraries ~bin_libs:libs entry.ElfSymbolTable.name tol) in
	`Lib (l,l)
    | _ ->
      `Lib ("","")
  in
  let data = `PrintableData
      (Printf.sprintf
	 "%s %s" bind stype) in
  [name; lib; offset; size; kind; data]

let symbols_to_goblin ?use_tol:(use_tol=true) ~libs:libs soname dynsyms relocs =
  let tol =
    try
      if (use_tol) then ToL.get() else ToL.empty
    with ToL.Not_built ->
      ToL.empty
  in
  List.mapi
    (symbol_entry_to_goblin_symbol
       ~tol:tol ~libs:libs ~relocs:relocs soname) dynsyms
 *)
