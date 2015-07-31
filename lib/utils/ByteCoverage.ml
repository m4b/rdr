(* TODO: create a normalize coverage function *)

let debug = true

type tag = | Meta
           | Code
           | Unknown
           | Symbol
           | String
           | StringTable
           | Rela
           | PlatformSpecific
           | Data
           | Invalid
           | Semantic

let tag_to_string =
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
  | Semantic -> "Semantic"

type data =
  {
    size: int;
    tag: tag;
    range_start: int;
    range_end: int;
    extra: string;
    understood: bool;
    container: bool;
  }

(* range specific *)
let is_contained d1 d2 = 
  (d1.range_start > d2.range_start) && (d1.range_end <= d2.range_end)
  || (d1.range_start >= d2.range_start) && (d1.range_end < d2.range_end)
let contains d2 d1 = is_contained d1 d2

let same_range d1 d2 = (d1.range_start = d2.range_start) && (d1.range_end = d2.range_end)

let sort a b =
  if (a.range_start = b.range_start) then
    if (a.range_end = b.range_end) then
      (Pervasives.compare a.tag b.tag)
    else if (contains a b) then
      -1
    else
      1
      (*     else
      -(Pervasives.compare a.range_end b.range_end)
 *)
  else
  if (contains a b) then
    -1
  else
    Pervasives.compare a.range_start b.range_start

(* 
  if (a.range_start < b.range_start) then
    if (contains a b) then
      1
    else
      -1
  else (* a.start >= b.start *)
  if (a.range_end > b.range_end) then
    1
  else
    -1 (* we don't consider overlapping instances for now *)
 *)

(* DataSet specific functions *)
module DataSet = Set.Make(
  struct 
    type t = data
    let compare = sort
  end)

let mem = DataSet.mem
let add e l = DataSet.add e l             (* DataSet.add *)
let empty = DataSet.empty                  (* DataSet.empty *)
(* let fold f coverage seed = DataSet.fold_left f seed coverage *)
let fold = DataSet.fold
let iter = DataSet.iter
let remove = DataSet.remove
let to_list s = DataSet.elements s
(* checks whether the range is redundant (covered) in our set already *)
let is_covered range coverage =
  DataSet.exists (fun data ->
      data <> range
      && (is_contained range data)
    ) coverage

(*   DataSet.exists (fun data ->
      data <> range
      && data.range_start = range.range_start
      && data.range_end = range.range_end
     ) coverage
*)

let is_unique x dataset =
  DataSet.for_all (fun y -> not (contains x y) && x <> y) dataset

let is_container x dataset =
  DataSet.exists (fun y -> (contains x y && x <> y)) dataset
  || is_unique x dataset

let is_sub_range x dataset =
  DataSet.exists (fun y -> is_contained x y && x <> y) dataset

(* partitions data into largest covering ranges, and redundant data *)
let normalize dataset =
  let s1,s2 =
    DataSet.partition (fun data -> 
        is_unique data dataset || is_sub_range data dataset) dataset
  in s2,s1 (* swap *)

(* finalizes the dataset by setting containerhood *)
let finalize dataset =
  to_list dataset |> List.map (fun el ->
      if (is_container el dataset) then
        {el with container = true}
      else
        el) |> DataSet.of_list

type t = {
  data: DataSet.t;
  size: int;
  total_coverage: int;
  total_understood: int;
  percent_coverage: float;
  percent_understood: float;
}

let data_to_string (data:data) =
  Printf.sprintf "size: %d tag: %s\n  range_start: 0x%x range_end 0x%x understood: %b container: %b extra: %s"
    data.size
    (tag_to_string data.tag)
    data.range_start
    data.range_end
    data.understood
    data.container
    data.extra

let print_data data =
  iter (fun data -> Printf.printf "%s\n" (data_to_string data)) data

let print coverage =
  print_data coverage.data;
  Printf.printf "Total Coverage: %d / %d = %f\n"
    coverage.total_coverage
    coverage.size
    coverage.percent_coverage;
  Printf.printf "Understood Coverage: %d / %d = %f\n"
    coverage.total_understood
    coverage.size
    coverage.percent_understood

let create_data
    ~tag:tag 
    ~r1:range_start 
    ~r2:range_end
    ~extra:extra
    ~understood:understood =
  let size = range_end - range_start in
  {size; tag; range_start; range_end; extra; understood; container = false}

let count data condition =
  fold (fun range acc ->
      if (condition range) then
        range.size + acc
      else
        acc
    ) data 0

let get_unique data =
  let redundant = fold
      (fun elt acc ->
         if (is_covered elt data) then
           if (is_covered elt acc) then
             acc
           else
             add elt acc
         else acc) data empty
  in
  DataSet.diff data redundant

let count_coverage unique =
  let total = count unique (fun range -> range.tag <> Semantic) in
  let understood = count unique (fun range -> range.understood && range.tag <> Semantic) in
  total, understood

(* @invariant sorted, normalized *)
let compute_unknown dataset size =
  let extra = "Unknown // Computed" in
  let bindings = to_list dataset in
  (*   if (debug) then print dataset; *)
  let unknown =
    if (bindings = []) then
      [create_data Unknown 0 size extra false]
    else
      let rec loop acc =
        function
        | [] -> acc
        | d::[] ->
          if (d.range_end >= size) then
            acc
          else
            let data = create_data Unknown d.range_end size extra false in
            if (debug) then Printf.printf "END %s\n" (data_to_string data);
            data::acc
        | d1::(d2::rest as tail)->
          if (d1.range_end = d2.range_start || same_range d1 d2) then
            loop acc tail
          else
            (* if it's semantic, and the first's range end is greater than the second's start (it contains it, which is guaranteed by our sorting), we ignore it *)
          if (d1.tag = Semantic && d1.range_end >= d2.range_start) then
            loop acc tail
          else
            let data = create_data Unknown d1.range_end d2.range_start extra false in
            if (debug) then Printf.printf "NEW %s\n" (data_to_string data);
            loop (data::acc) tail
      in loop [] bindings
  in
  (* add the unknown data back into the dataset *)
  List.fold_left (fun dataset data -> add data dataset) dataset unknown

(* the preliminary data comes from an oracle which knows about the binary format, e.g., elf or mach specific counters *)
let create size data =
  (* normalize right here, send only the largest covers into compute_unknown, similarly for counting *)
  let normalized_data,rest = normalize data in
  if (debug) then
    begin

      print_string "\nSIGIL Normalized ranges\n\n----------------------\n\n";
      print_data normalized_data;
      print_string "\nSIGIL Remainder\n\n---------------------------\n\n";
      print_data rest;
      print_string "\n\n---------------------------\n\n";
    end;
  let data = compute_unknown normalized_data size
             |> DataSet.union rest |> finalize in
  let total_coverage,total_understood = count_coverage normalized_data in
  let percent_coverage = (float_of_int total_coverage) /.  (float_of_int size) in
  let percent_understood = (float_of_int total_understood) /.  (float_of_int size) in
  {
    data;
    size;
    total_coverage;
    total_understood;
    percent_coverage;
    percent_understood;
  }

(* UNIT *)
(* unusual sequences, like one range starting in one range and ending in another need to be dealt with *)

let d1 = create_data ~r1:0 ~r2:0x100 ~understood:true ~tag:Meta ~extra:"Container1"
let d2 = create_data ~r1:0 ~r2:0x50 ~understood:true ~tag:Data ~extra:"is_contained11"
let d3 = create_data ~r1:0x50 ~r2:0x75 ~understood:true ~tag:Code ~extra:"is_contained12"
let d4 = create_data ~r1:0x100 ~r2:0x200 ~understood:true ~tag:Meta ~extra:"Container2"
let d5 = create_data ~r1:0x150 ~r2:0x175 ~understood:true ~tag:Code ~extra:"is_contained21"
let d6 = create_data ~r1:0x250 ~r2:0x300 ~understood:true ~tag:Meta ~extra:"Container3"
let d7 = create_data ~r1:0x250 ~r2:0x275 ~understood:true ~tag:Meta ~extra:"is_contained34"
let d8 = create_data ~r1:0x250 ~r2:0x250 ~understood:true ~tag:Meta ~extra:"null_is_contained31"
let d9 = create_data ~r1:0x250 ~r2:0x251 ~understood:true ~tag:Meta ~extra:"small_is_contained32"
let d10 = create_data ~r1:0x251 ~r2:0x252 ~understood:true ~tag:Meta ~extra:"small_is_contained33"

let d0 = create_data ~r1:0x200 ~r2:0x250 ~understood:true ~tag:Meta ~extra:"is_contained31"


let set1 =
  add d1 empty
  |> add d2 |> add d3
  |> add d4 |> add d5
  |> add d6 |> add d7 |> add d8
  |> add d9 |> add d10

let size = 0x250

let one,two = normalize set1
let un1 = compute_unknown one size
let u1 = DataSet.union un1 two

let t1 = create size set1

let pd = print_data
let p = print

let m1 = add d0 one
let l1 = m1 |> to_list
