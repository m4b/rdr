(* TODO: create a normalize map function *)
(* Add a coverage struct with the map of understood data, and a map of unknown, which is the understood data ranges modulo the size *)


type kind = | Meta
            | Code
            | Unknown
            | Symbol
            | String
            | StringTable
            | Rela
            | PlatformSpecific
            | Data
            | Invalid

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
  | Invalid -> "Invalid"

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

let debug = false

type t = data Map.t

let create_data kind range_start range_end extra understood =
  let size = range_end - range_start in
  {size; kind; range_start; range_end; extra; understood}

let total_coverage m =
  Map.fold (fun key range acc ->
        range.size + acc
    ) m 0

let understood_coverage m =
  Map.fold (fun key range acc ->
      if (range.understood) then
        range.size + acc
      else
        acc
    ) m 0

let percent_coverage m size = (float_of_int @@ total_coverage m) /.  (float_of_int size)
let percent_understood m size = (float_of_int @@ understood_coverage m) /.  (float_of_int size)

let data_to_string data =
  Printf.sprintf "size: %d kind: %s\n  range_start: 0x%x range_end 0x%x understood: %b extra: %s"
    data.size
    (kind_to_string data.kind)
    data.range_start
    data.range_end
    data.understood
    data.extra

let print =
  Map.iter
    (fun key data -> Printf.printf "%s\n" (data_to_string data))

let stats map size =
  Printf.printf "Total Coverage: %d / %d = %f\n"
    (total_coverage map)
    size @@ percent_coverage map size;
  Printf.printf "Understood Coverage: %d / %d = %f\n"
    (understood_coverage map)
    size @@ percent_understood map size

(* TODO: normalize *)

(* @invariant sorted, normalized *)
let compute_unknown m size =
  let extra = "Unknown // Computed" in
  let bindings = Map.bindings m in
  let unknown =
    if (bindings = []) then
      [create_data Unknown 0 size extra false]
    else
      let rec loop acc =
        function
        | [] -> acc
        | (_,d)::[] ->
          if (d.range_end = size) then
            acc
          else
            let data = create_data Unknown d.range_end size extra false in
            if (debug) then Printf.printf "END %s\n" (data_to_string data);
            data::acc
        | (_,d1)::(_,d2 as next)::rest ->
          if (d1.range_end = d2.range_start) then
            loop acc (next::rest)
          else
          if (d1.range_end > d2.range_start) then
            loop
              ({d1 with
                size = d2.range_start - d1.range_start;
                kind = Invalid;
                range_end = d2.range_start;
                extra = (Printf.sprintf "%s // Computed INVALID range_end: 0x%x" d1.extra d1.range_end)}::acc)
              (next::rest)
          else
            let data = create_data Unknown d1.range_end d2.range_start extra false in
            if (debug) then Printf.printf "NEW %s\n" (data_to_string data);
            loop (data::acc) (next::rest)
      in loop [] bindings
  in
  (* add the unknown data back into the map *)
  List.fold_left (fun acc data -> Map.add data.range_start data acc) m unknown

let create m size =
  (* add normalize step here; pack everything into a struct? *)
  compute_unknown m size
