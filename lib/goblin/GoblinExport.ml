type reexport =
  | From of string
  | Rename of string * string

let pp_reexport ppf reex =
  match reex with
  | From lib ->
    Format.fprintf ppf " => %s" lib
  | Rename (lib, rename) ->
    Format.fprintf ppf " => %s.%s" lib rename

let show_reexport reex =
  pp_reexport Format.str_formatter reex; Format.flush_str_formatter()

type t =
  {
    name: string;     (* name of the exported symbol *)
    offset: int;      (* offset into the containing binary's byte array *)
    size: int;        (* size of the routine, in bytes *)
    reexport: reexport option;
  }

let pp ppf export =
  let reex = 
    match export.reexport with
    | Some reexport ->
      show_reexport reexport
    | None -> ""
  in
  Format.fprintf ppf "@[%16x %s (%d)%s@]" export.offset export.name export.size reex

let show export =
  pp Format.str_formatter export; Format.flush_str_formatter()

let print export =
  pp Format.std_formatter export; Format.print_newline()
