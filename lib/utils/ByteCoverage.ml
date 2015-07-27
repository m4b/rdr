type kind = | Meta | Code | Unknown | Symbol | String

type t =
  {
    size: int;
    kind: kind;
    range_start: int;
    range_end: int;
    extra: string;
  }

module Map = Map.Make(
  struct 
    type t=int
    let compare= (fun a b -> Pervasives.compare a b)
  end)

let total_coverage m = 
  Map.fold (fun key value acc ->
      value.size + acc
    ) m 0

let percent m size = (float_of_int @@ total_coverage m) /.  (float_of_int size)
