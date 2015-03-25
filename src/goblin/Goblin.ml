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
      mutable idx: int;    (* the index into some ab's export code section, typically generated via the dynamic linker *)
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
      name: string;                  (* name of this binary *)
      soname: string;                (* fully qualified pathname ID_DYLIB/SONAME or pwd if an executable *)
      islib: bool;                   (* are we a library? *)
      libs: string array;            (* lib string array *)
      nlibs: int;                    (* number of libs *)
      imports: Import.t StringMap.t; (* the import map *) (* if this is an array much simpler *)
      nimports: int;                 (* number of imports *)
      exports: Export.t StringMap.t; (* the export map *) (* if this is an array much simpler *)
      nexports: int;                 (* number of exports *)
      code: bytes;                   (* to bytes array or not to bytes array *)
    }
    (* pro: if bytes array, then (tol#find import.lib).code.(import.idx) = the symbol's routine *)
      (* con: if bytes array, must relocate/translate all references in export symbol code to be indexed in an array *)
      (* pro: if not bytes array, then: import.offset <- (find import.name (tol#find import.lib)).address at dynamic bind time
         similarly: Bytes.sub (tol#find import.lib).code import.offset import.size = the symbol's routine
      *)

let get_import import =
  StringMap.find import

let get_export export =
  StringMap.find export

let iter = 
  StringMap.iter

let empty = 
  StringMap.empty

let add =
  StringMap.add

let libs_to_string libs = 
  let b = Buffer.create @@ Array.length libs in
  Array.iteri (fun i elem -> 
      Buffer.add_string b @@ Printf.sprintf "(%d) %s\n" i elem) libs;
  Buffer.contents b

let imports_to_string imports = 
  let b = Buffer.create @@ (StringMap.cardinal imports) * 15 in (* just ballpark *)
  StringMap.iter (fun key import -> 
      let squiggle = if (import.Import.is_lazy) then "~>" else "->" in
      Buffer.add_string b @@ Printf.sprintf "%s (%d) %s %s\n" import.Import.name import.Import.size squiggle import.Import.lib) imports;
  Buffer.contents b

let exports_to_string exports = 
  let b = Buffer.create @@ (StringMap.cardinal exports) * 15 in (* just ballpark *)
  StringMap.iter (fun key export -> 
      Buffer.add_string b @@ Printf.sprintf "%s (%d) -> 0x%x\n" export.Export.name export.Export.size export.Export.offset) exports;
  Buffer.contents b

let to_string goblin = Printf.sprintf "%s (%s):\nLibs (%d):\n%s\nExports (%d):\n%s\nImports (%d):\n%s"
    goblin.name goblin.soname
    goblin.nlibs
    (libs_to_string goblin.libs)
    goblin.nimports
    (imports_to_string goblin.imports)
    goblin.nexports
    (exports_to_string goblin.exports)
