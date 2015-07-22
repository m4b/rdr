(* 
TODO:
(1) MAJOR TODO: change the exports and imports from maps to arrays or lists --- terrible mistake to use maps in first place...
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

module Import = struct 
  type t = 
    {
      name: string;        (* name of the imported symbol *)
      lib: string;         (* library which contains the binary *)
      is_lazy: bool;
      mutable idx: int;    (* the index into some goblin's export code section, typically generated via the dynamic linker *)
      mutable offset: int; (* offset into (tol#find import.lib).code *)
      size: int;           (* size of the imported symbol, in bytes *)
    }
end

module Export = struct 
  type t = 
    {
      name: string;     (* name of the exported symbol *)
      offset: int;      (* offset into the containing binary's byte array *)
      size: int;        (* size of the routine, in bytes *)
    }
end

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
      let squiggle = if (import.Import.is_lazy) then "~>" else "->" in
      Buffer.add_string b @@ Printf.sprintf "%s (%d) %s %s\n" import.Import.name import.Import.size squiggle import.Import.lib) imports;
  Buffer.contents b
                  
let exports_to_string exports = 
  let b = Buffer.create @@ (Array.length exports) * 15 in (* just ballpark *)
  Array.iter (fun export -> 
      Buffer.add_string b @@ Printf.sprintf "%s (%d) -> 0x%x\n" export.Export.name export.Export.size export.Export.offset) exports;
  Buffer.contents b

let print_export ?like_goblin:(like_goblin=true) export =
  if (like_goblin) then
    Printf.printf "0x%-16x %s (%d)\n" export.Export.offset export.Export.name export.Export.size
  else
    Printf.printf "%s (%d) -> 0x%x\n" export.Export.name export.Export.size export.Export.offset

let to_string goblin = Printf.sprintf "%s (%s):\nLibs (%d):\n%s\nExports (%d):\n%s\nImports (%d):\n%s"
    goblin.name goblin.install_name
    goblin.nlibs
    (libs_to_string goblin.libs)
    goblin.nimports
    (imports_to_string goblin.imports)
    goblin.nexports
    (exports_to_string goblin.exports)
