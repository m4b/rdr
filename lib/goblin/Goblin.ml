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

module Symbol = GoblinSymbol
module Import = GoblinImport
module Export = GoblinExport

module StringMap = Map.Make(String)

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
  Array.iteri (fun i elem ->
      Buffer.add_string b @@ Printf.sprintf "(%d) %s\n" i elem) libs;
  Buffer.contents b

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

let print_exports =
  Array.iter Export.print

let print_imports =
  Array.iter Import.print

let to_string goblin = Printf.sprintf "%s (%s):\nLibs (%d):\n%s\nExports (%d):\n%s\nImports (%d):\n%s"
    goblin.name goblin.install_name
    goblin.nlibs
    (libs_to_string goblin.libs)
    goblin.nimports
    (imports_to_string goblin.imports)
    goblin.nexports
    (exports_to_string goblin.exports)

let print goblin = Printf.printf "%s\n" @@ to_string goblin

module Mach = struct
  include GoblinMach
  open MachImports
  open MachExports

  (* @invariant sorted, sizecomputed *)
  let to_goblin mach install_name =
    let name = mach.Mach.name in
    let install_name = install_name in
    let libs = mach.Mach.libraries in
    let nlibs = mach.Mach.nlibraries in
    let exports = List.map
        (fun export ->
           let name = export.name in
           let size = export.size in
           let offset =
             match export.info with
             | Regular symbol ->
               symbol.address
             | _ -> 0x0
           in
           {Export.name; size; offset}
        ) mach.Mach.exports
    in
    let exports = Array.of_list exports in
    let nexports = mach.Mach.nexports in
    let imports =
      List.mapi
        (
          fun i import ->
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

(* TODO: add Elf for consistency *)
module Elf = struct
  include GoblinElf
  exception Unimplemented
  let to_goblin elf install_name = raise Unimplemented
  (* 

let create_goblin_binary soname install_name libraries islib goblin_exports goblin_imports =
  let name = soname in
  let install_name = install_name in
  let libs = Array.of_list (soname::libraries) in (* to be consistent... for graphing, etc. *)
  let nlibs = Array.length libs in
  let exports =
    Array.of_list
    @@ List.map (GoblinSymbol.to_goblin_export) goblin_exports
  in
  let nexports = Array.length exports in
  let imports =
    Array.of_list
    @@ List.map (GoblinSymbol.to_goblin_import) goblin_imports
  in
  let nimports = Array.length imports in
  (* empty code *)
  let code = Bytes.empty in
  {Goblin.name;
   install_name; islib; libs; nlibs; exports; nexports;
   imports; nimports; code}

 *)
(*     {name; install_name; islib; libs; nlibs; exports; nexports; imports; nimports; code} *)
end
