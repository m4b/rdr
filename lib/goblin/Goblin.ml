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

let iter = Array.iter

let empty = [||]

let print_exports =
  iter Export.print

let print_imports =
  iter Import.print

let pp ppf goblin =
  Format.fprintf
    ppf "@[<v>%s (%s)@ Libs (%d)@ @[<v 2>@ "
    goblin.name goblin.install_name
    goblin.nlibs;
    RdrPrinter.pp_strings Array.iter ppf goblin.libs;
    Format.fprintf ppf "@]@ ";
    Format.fprintf ppf "Imports (%d)@ @[<v 2>@ " goblin.nimports;
    iter (fun elem -> Import.pp ppf elem; Format.print_space())
         goblin.imports;
    Format.fprintf ppf "@]@ ";
    Format.fprintf ppf "Exports (%d)@ @[<v 2>@ " goblin.nexports;
    iter (fun elem -> Export.pp ppf elem; Format.print_space())
         goblin.exports;
    Format.fprintf ppf "@]@]@."

let show goblin =
  pp Format.str_formatter goblin; Format.flush_str_formatter()

let print goblin =
  pp Format.std_formatter goblin; Format.print_newline()

module Mach = struct
    (* remove this monstrosity of a file *)
    include GoblinMach
    open MachImports
    open MachExports

    (* because we add the base binary
       as the library because that was a great idea *)
    let get_libs libraries =
      if (Array.length libraries <= 1) then
        [||]
      else
        Array.sub libraries 1 (Array.length libraries - 1)

    [@@invariant sorted, sizecomputed]
    let from install_name mach =
      let name =
        if (mach.Mach.name = "") then
          Filename.basename install_name
        else mach.Mach.name
      in
      let libs = get_libs mach.Mach.libraries in
      let nlibs = Array.length libs in
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

   let to_goblin ~coverage filename binary =
      Mach.get ~coverage binary |> from filename
  end

module Elf = struct
    open Elf
    (* todo use proper variants here ffs *)
    let get_goblin_kind symbol bind stype =
      if (symbol.Elf.SymbolTable.st_value = 0x0
          && symbol.Elf.SymbolTable.st_shndx = 0
          && symbol.Elf.SymbolTable.name <> "") (* ignore first \0 symbol *)
      then
        Symbol.Import
      else if (bind = "LOCAL") then
        Symbol.Local
      else if ((bind = "GLOBAL"
	        || (bind = "WEAK" && (stype = "FUNC"
				      || stype = "IFUNC"
				      || stype = "OBJECT")))
	       && symbol.Elf.SymbolTable.st_value <> 0) then
        Symbol.Export
      else Symbol.Other

    let get_imports gtree libraries relocs symbols =
      List.fold_left
        (fun (acc,index) symbol ->
         let bind =
           Elf.SymbolTable.get_bind symbol.Elf.SymbolTable.st_info
           |> Elf.SymbolTable.symbol_bind_to_string
         in
         let stype =
           Elf.SymbolTable.get_type symbol.Elf.SymbolTable.st_info
           |> Elf.SymbolTable.symbol_type_to_string
         in
         let kind = get_goblin_kind symbol bind stype in
         match kind with
         | Symbol.Import ->
            let name = symbol.Elf.SymbolTable.name in
            let offset =
              if (symbol.Elf.SymbolTable.st_value = 0) then
	        Elf.Reloc.get_size index relocs
              else
	        symbol.Elf.SymbolTable.st_value
            in
            let size = symbol.Elf.SymbolTable.st_size in
            let lib = Tree.resolve_library ~case_sensitive:true ~name ~libraries ~tree:gtree in
            let import =
              {Import.name; lib;idx=0; is_lazy=true; offset; size}
            in
            (import::acc),(index+1)
         | _ ->
            acc,index+1
        ) ([],0) symbols |> fst |> List.rev

    let get_exports relocs symbols =
      List.fold_left
        (fun (acc,index) symbol ->
         let bind =
           Elf.SymbolTable.get_bind symbol.Elf.SymbolTable.st_info
           |> Elf.SymbolTable.symbol_bind_to_string
         in
         let stype =
           Elf.SymbolTable.get_type symbol.Elf.SymbolTable.st_info
           |> Elf.SymbolTable.symbol_type_to_string
         in
         let kind = get_goblin_kind symbol bind stype in
         match kind with
         | Symbol.Export ->
            let name = symbol.Elf.SymbolTable.name in
            let offset =
              if (symbol.Elf.SymbolTable.st_value = 0) then
	        (* this _could_ be relatively expensive *)
	        Elf.Reloc.get_size index relocs
              else
	        symbol.Elf.SymbolTable.st_value
            in
            let size = symbol.Elf.SymbolTable.st_size in
            let export = {Export.name; offset; size} in
            (export::acc),(index+1)
         | _ ->
            acc,index+1
        ) ([],0) symbols |> fst |> List.rev

    let from ?use_tree:(use_tree=true) install_name elf =
      let name =
        if (elf.soname = "") then
        Filename.basename install_name
        else elf.soname
      in
      let islib = elf.is_lib in
      let libs = Array.of_list elf.libraries in
      let nlibs = Array.length libs in
      let exports,imports =
        match elf.dynamic_symbols with
        | [] ->
           [||], [||]
        | symbols ->
           let exports =
             Array.of_list @@ get_exports elf.relocations symbols
           in
           let gtree =
             if (use_tree) then
               GoblinTree.get()
             else
               GoblinTree.empty
           in
           let imports =
             Array.of_list
             @@ get_imports
                  gtree
                  elf.libraries
                  elf.relocations
                  symbols
           in
           exports,imports
      in
      let nexports = Array.length exports in
      let nimports = Array.length imports in
      let code = Bytes.empty in
      {name;
       install_name; islib; libs; nlibs; exports; nexports;
       imports; nimports; code}

    let to_goblin ~coverage ~use_tree filename binary =
      Elf.get ~coverage binary |> from ~use_tree filename

  end

module PE = struct
    open PE
    open PE.Import
    open PE.Export
    open GoblinImport
    open GoblinExport

    let from install_name (pe:PE.t) =
      let name =
        if (pe.name = "") then
          Filename.basename install_name
        else pe.name
      in
      let imports =
        List.map
          (fun (symbol:PE.Import.synthetic_import) ->
           {name = symbol.name; lib = symbol.dll;
            (* TODO implement is lazy, implement offset*)
            is_lazy = false; idx = symbol.ordinal;
            offset = symbol.offset; size = symbol.size}
          )
          pe.imports |> Array.of_list
      in
      let exports =
        List.map
          (fun (symbol:PE.Export.synthetic_export) ->
           {name = symbol.name; offset = symbol.offset;
            size = symbol.size}
          )
          pe.exports |> Array.of_list
      in
      {
        name;
        install_name;
        islib = pe.is_lib;
        libs = Array.of_list pe.libraries;
        nlibs = pe.nlibraries;
        imports;
        nimports = pe.nimports;
        exports;
        nexports = pe.nexports;
        code = Bytes.empty;
      }

    let to_goblin ~coverage filename binary =
      PE.get ~coverage binary |> from filename
  end

