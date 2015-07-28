type t =
  {
    name: string;     (* name of the exported symbol *)
    offset: int;      (* offset into the containing binary's byte array *)
    size: int;        (* size of the routine, in bytes *)
  }

let to_string export =
  Printf.sprintf "0x%-16x %s (%d)" export.offset export.name export.size

let print export =
    Printf.printf "%s\n" @@ to_string export
