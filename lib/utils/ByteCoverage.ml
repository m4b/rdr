(* TODO: create a normalize coverage function *)

let debug = false

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
  }

let sort a b =
  if (a.range_start = b.range_start) then
    if (a.range_end = b.range_end) then
      Pervasives.compare a.tag b.tag
    else
      Pervasives.compare a.range_end b.range_end
  else
  if (a.range_start < b.range_start) then
    -1
  else
    1

let same_range d1 d2 = (d1.range_start = d2.range_start) && (d1.range_end = d2.range_end)

(* DataSet specific functions *)
module DataSet = Set.Make(
  struct 
    type t=data
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
      && data.range_start = range.range_start
      && data.range_end = range.range_end
    ) coverage

type t = {
  data: DataSet.t;
  size: int;
  total_coverage: int;
  total_understood: int;
  percent_coverage: float;
  percent_understood: float;
}

let data_to_string (data:data) =
  Printf.sprintf "size: %d tag: %s\n  range_start: 0x%x range_end 0x%x understood: %b extra: %s"
    data.size
    (tag_to_string data.tag)
    data.range_start
    data.range_end
    data.understood
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

let create_data tag range_start range_end extra understood =
  let size = range_end - range_start in
  {size; tag; range_start; range_end; extra; understood}

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

let count_coverage data =
  let unique = get_unique data in
  let total = count unique (fun range -> range.tag <> Semantic) in
  let understood = count unique (fun range -> range.understood && range.tag <> Semantic) in
  total, understood

(* TODO: normalize *)

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
          if (d.range_end = size) then
            acc
          else
            let data = create_data Unknown d.range_end size extra false in
            if (debug) then Printf.printf "END %s\n" (data_to_string data);
            data::acc
        | d1::(d2::rest as tail)->
          if (d1.range_end = d2.range_start || same_range d1 d2) then
            loop acc tail
          else
            (* if it's semantic, and the first's range end is greater than the second's start
               (it contains it, which is guaranteed by our sorting), we ignore it *)
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
  let data = compute_unknown data size in
  let total_coverage,total_understood = count_coverage data in
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

