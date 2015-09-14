type t =
  {
    name: string;     (* name of the exported symbol *)
    offset: int;      (* offset into the containing binary's byte array *)
    size: int;        (* size of the routine, in bytes *)
  }

let pp ppf export =
  Format.fprintf ppf "@[%16x %s (%d)@]" export.offset export.name export.size

let show export =
  pp Format.str_formatter export; Format.flush_str_formatter()

let print export =
    pp Format.std_formatter export; Format.print_newline()
