(* TODO: create a normalize map function *)

type kind = | Meta | Code | Unknown
            | Symbol | String | StringTable
            | Rela | PlatformSpecific
            | Data

let kind_to_string =
  function
  | Meta -> "Meta"
  | Code -> "Code"
  | Unknown -> "Unknown"
  | Symbol -> "Symbol"
  | String -> "String"
  | StringTable -> "StringTable"
  | Rela -> "Rela"
  | PlatformSpecific -> "PlatformSpecific"
  | Data -> "Data"

type data =
  {
    size: int;
    kind: kind;
    range_start: int;
    range_end: int;
    extra: string;
    understood: bool;
  }

module Map = Map.Make(
  struct 
    type t=int
    let compare= (fun a b -> Pervasives.compare a b)
  end)

type t = data Map.t

(* TODO check for inconsistencies in the map; *)
let total_coverage m = 
  Map.fold (fun key range acc ->
      if (range.understood) then
        range.size + acc
      else
        acc
    ) m 0

let percent m size = (float_of_int @@ total_coverage m) /.  (float_of_int size)

let data_to_string data = 
  Printf.sprintf "size: %d kind: %s\nrange_start: 0x%x range_end 0x%x understood: %b extra: %s" data.size (kind_to_string data.kind) data.range_start data.range_end data.understood data.extra

let print = 
  Map.iter (fun key data -> Printf.printf "%s\n" data)
