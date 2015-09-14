type t =
  {
    name: string;        (* name of the imported symbol *)
    lib: string;         (* library which contains the binary *)
    is_lazy: bool;
    mutable idx: int;    (* the index into some goblin's export code section, typically generated via the dynamic linker *)
    offset: int; (* offset into (tol#find import.lib).code *)
    size: int;           (* size of the imported symbol, in bytes *)
  }

let pp ppf import =
  let squiggle = if (import.is_lazy) then "~>" else "->" in
  Format.fprintf ppf "@[%16x %s (%d) %s %s@]"
                 import.offset
                 import.name
                 import.size
                 squiggle
                 import.lib

let show import =
  pp Format.str_formatter import; Format.flush_str_formatter()

let print import =
  pp Format.std_formatter import; Format.print_newline()
