(* TODO:
 * PE: implement is lazy, implement offset
 *)

(*
 GOBLIN:
 Generic Object Binary Logical Information Node
  yes, I just made that up
 *)

(* perhaps:
    export.(x) = code.(x)
   so:
    import.(x) = some abstract_binary.name 
     or import["sym_name"] ->  some abstract_binary.name
   or better yet: 
    import.(x) = name, idx && some abstract_binary.code.(idx)
   OR: 
    (ToL.find import.(x).lib).import.(x).idx = fun
 *)

(* tol#find import.lib -> AbstractBinary.t *)
(* (ab#get_import name).idx *)

module StringMap = Map.Make(String)

module Symbol = GoblinSymbol
module Import = GoblinImport
module Export = GoblinExport
module Tree = GoblinTree

type t =
  {
    name: string;                  (* name of this binary used in linking and imports, ID_DYLIB/SONAME, or basename if executable *)
    install_name: string;          (* fully qualified pathname to the binary/dylib NOTE: OSX ID_DYLIB = PWD of dylib *)
    islib: bool;                   (* are we a library? *)
    libs: string array;            (* lib string array *)
    nlibs: int;                    (* number of libs *)
    imports: Import.t array; (* the import map *) (* if this is an array much simpler *)
    nimports: int;                 (* number of imports *)
    exports: Export.t array; (* the export map *) (* if this is an array much simpler *)
    nexports: int;                 (* number of exports *)
    code: bytes;                   (* to bytes array or not to bytes array *)
  }
(* pro: if bytes array, then (tol#find import.lib).code.(import.idx) = the symbol's routine *)
(* con: if bytes array, must relocate/translate all references in export symbol code to be indexed in an array *)
(* pro: if not bytes array, then: import.offset <- (find import.name (tol#find import.lib)).address at dynamic bind time
   similarly: Bytes.sub (tol#find import.lib).code import.offset import.size = the symbol's routine
 *)

let get_symbol collection i = Array.get collection i

let _find pred arr =
  let size = Array.length arr in
  let rec loop i =
    if (i >= size) then raise Not_found
    else
      if (pred arr.(i)) then
        arr.(i)
      else
        loop (i + 1)
  in loop 0

let get_import import  =
  _find (fun symbol -> symbol.Import.name = import) 

let get_export export =
  _find (fun symbol -> symbol.Export.name = export)

let iter =
  Array.iter

let empty = [||]

let libs_to_string libs =
  let b = Buffer.create @@ Array.length libs in
  Array.iteri
    (fun i elem ->
     Buffer.add_string b @@ Printf.sprintf "(%d) %s\n" i elem)
    libs;
  Buffer.contents b

(* 
let imports_to_string imports =
  let b = Buffer.create @@ (Array.length imports) * 15 in (* just ballpark *)
  Array.iter (fun import ->
      Buffer.add_string b @@ Import.to_string import) imports;
  Buffer.contents b

let exports_to_string exports =
  let b = Buffer.create @@ (Array.length exports) * 15 in (* just ballpark *)
  Array.iter (fun export ->
      Buffer.add_string b @@ Export.to_string export) exports;
  Buffer.contents b
*)

let print_exports =
  iter Export.print

let print_imports =
  iter Import.print

(* 
let to_string goblin = Printf.sprintf "%s (%s):\nLibs (%d):\n%s\nExports (%d):\n%s\nImports (%d):\n%s"
    goblin.name goblin.install_name
    goblin.nlibs
    (libs_to_string goblin.libs)
    goblin.nimports
    (imports_to_string goblin.imports)
    goblin.nexports
    (exports_to_string goblin.exports)

let print goblin = Printf.printf "%s\n" @@ to_string goblin
 *)
module Mach = struct
    include GoblinMach
    open MachImports
    open MachExports

           [@@@invariant sorted, sizecomputed]
    let to_goblin mach install_name =
      let name = mach.Mach.name in
      let install_name = install_name in
      let libs = mach.Mach.libraries in
      let nlibs = mach.Mach.nlibraries in
      let exports =
        List.map
          (fun export ->
           let name = export.name in
           let size = export.size in
           let offset = export.offset in
           {Export.name; size; offset}
          ) mach.Mach.exports
        |> Array.of_list
      in
      let nexports = mach.Mach.nexports in
      let imports =
        List.mapi
          (fun i import ->
            let name = import.bi.symbol_name in
            let lib = import.dylib in
            let is_lazy = import.is_lazy in
            let offset = import.offset in
            let size = import.size in
            {Import.name = name; lib; is_lazy;
             idx = i; offset; size}
          ) mach.Mach.imports
        |> Array.of_list
      in
      let nimports = mach.Mach.nimports in
      let islib = mach.Mach.is_lib in
      let code = mach.Mach.raw_code in
      {name; install_name; islib; libs; nlibs;
       exports; nexports; imports; nimports; code}
  end

module Elf = struct
    include GoblinElf
    (* hacky function to filter imports from exports, etc. *)
    (* todo use proper variants here ffs *)
    let get_goblin_kind entry bind stype =
      if (entry.Elf.SymbolTable.st_value = 0x0
          && entry.Elf.SymbolTable.st_shndx = 0
          && entry.Elf.SymbolTable.name <> "") (* ignore first \0 entry *)
      then
        Symbol.Import
      else if (bind = "LOCAL") then
        Symbol.Local
      else if ((bind = "GLOBAL"
	        || (bind = "WEAK" && (stype = "FUNC"
				      || stype = "IFUNC"
				      || stype = "OBJECT")))
	       && entry.Elf.SymbolTable.st_value <> 0) then
        Symbol.Export
      else Symbol.Other

    let symbol_entry_to_goblin_symbol
          ~tol:tol ~libs:libs ~relocs:relocs
          (soname,install_name) index entry =
      let bind   = (Elf.SymbolTable.get_bind entry.Elf.SymbolTable.st_info |> Elf.SymbolTable.symbol_bind_to_string) in
      let stype  = (Elf.SymbolTable.get_type entry.Elf.SymbolTable.st_info |> Elf.SymbolTable.symbol_type_to_string) in
      let name   = `Name entry.Elf.SymbolTable.name in
      let offset =
        `Offset (
           if (entry.Elf.SymbolTable.st_value = 0) then
	     (* this _could_ be relatively expensive *)
	     Elf.Reloc.get_size index relocs
           else
	     entry.Elf.SymbolTable.st_value)
      in
      let size = `Size entry.Elf.SymbolTable.st_size in
      let kind = `Kind (get_goblin_kind entry bind stype) in
      let lib =
        (* TODO: this is a complete disaster; *)
        match kind with
        | `Kind Symbol.Export ->
           `Lib (soname,install_name)
        | `Kind Symbol.Import ->
           if (ToL.is_empty tol) then
	     `Lib ("∅","∅")
           else
	     let l = (ToL.get_libraries ~bin_libs:libs entry.Elf.SymbolTable.name tol) in
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
          if (use_tol) then ToL.get () else ToL.empty
        with ToL.Not_built ->
          ToL.empty
      in
      List.mapi
        (symbol_entry_to_goblin_symbol
           ~tol:tol ~libs:libs ~relocs:relocs soname) dynsyms

    let from ?use_tol:(use_tol=true) install_name elf =
      let name = elf.Elf.soname in
      let libs = Array.of_list (name::elf.Elf.libraries) in (* to be consistent... for graphing, etc. *)
      let nlibs = Array.length libs in
      let goblin_symbols =
        symbols_to_goblin
          ~use_tol:use_tol
          ~libs:elf.Elf.libraries
          (name,install_name)
          elf.Elf.dynamic_symbols
          elf.Elf.relocations
        |> Symbol.sort_symbols
        |> function | [] -> [] | syms -> List.tl syms
      (* because the head (the first entry, after sorting)
       is a null entry, and also _DYNAMIC can be empty *)
      in
      let goblin_imports =
        List.filter
          (fun symbol ->
	   Symbol.find_symbol_kind symbol
	   |> function
	     | Symbol.Import -> true
	     | _ -> false) goblin_symbols
      in
      let goblin_exports =
        List.filter
          (fun symbol ->
	   Symbol.find_symbol_kind symbol
	   |> function
	     | Symbol.Export -> true
	     | _ -> false) goblin_symbols
      in
      (* 
  let exports =
    Array.of_list
    @@ List.map (Goblin.Symbol.to_goblin_export) exports
  in
  let nexports = Array.length exports in
  let imports =
    Array.of_list
    @@ List.map (Goblin.Symbol.to_goblin_import) goblin_imports
  in
  let nimports = Array.length imports in
  (* empty code *)
  let code = Bytes.empty in
  {Goblin.name;
   install_name; islib; libs; nlibs; exports; nexports;
   imports; nimports; code}
       *)
      failwith "Unimplemented"
  end

module PE = struct
    open PE
    open PE.Import
    open PE.Export
    open GoblinImport
    open GoblinExport

    let from name pe =
      let goblin_imports :GoblinImport.t list =
        List.map
          (fun (symbol:PE.Import.synthetic_import) ->
           {name = symbol.name; lib = symbol.dll;
            (* TODO implement is lazy, implement offset*)
            is_lazy = false; idx = symbol.ordinal;
            offset = symbol.offset; size = symbol.size}
          )
          pe.imports
      in
      let goblin_exports =
        List.map
          (fun (symbol:PE.Export.synthetic_export) ->
           {name = symbol.name; offset = symbol.offset;
            size = symbol.size}
          )
          pe.exports
      in
      {
        name;
        install_name = name;
        islib = pe.is_lib;
        libs = Array.of_list pe.libraries;
        nlibs = pe.nlibraries;
        imports = Array.of_list goblin_imports;
        nimports = pe.nimports;
        exports = Array.of_list goblin_exports;
        nexports = pe.nexports;
        code = Bytes.empty;
      }
  end

