let debug = false

type tag = | Meta
           | Code
           | Unknown
           | Symbol
           | SymbolTable
           | String
           | StringTable
           | Rela
           | PlatformSpecific
           | Data
           | Invalid
           | Semantic
           | Zero

let tag_to_string =
  function
  | Meta -> "Meta"
  | Code -> "Code"
  | Unknown -> "Unknown"
  | Symbol -> "Symbol"
  | SymbolTable -> "SymbolTable"
  | String -> "String"
  | StringTable -> "StringTable"
  | Rela -> "Rela"
  | PlatformSpecific -> "PlatformSpecific"
  | Data -> "Data"
  | Invalid -> "Invalid"
  | Semantic -> "Semantic"
  | Zero -> "Zero"

let pp_tag ppf tag =
  Format.fprintf ppf "%s" @@ tag_to_string tag

(* MUST ADD THESE IF ADDING NEW TAG, probably a way to ensure compile time we're safe... *)
let tags = [
  "Meta";"Code";
  "Unknown";"Symbol";
  "SymbolTable";"String";
  "StringTable";"Rela";
  "PlatformSpecific";"Data";
  "Invalid";"Semantic";
  "Zero";]

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

let pp_data ppf data =
    Format.fprintf ppf "@[<v 2>@[<h>0x%x - 0x%x@]@ @[<h>%s * %s@ (0x%x)@]@ @[<h>understood: %b@ container: %b@]@]"
    data.range_start
    data.range_end
    (tag_to_string data.tag)
    data.extra
    data.size
    data.understood
    data.container

let show_data data =
  pp_data Format.str_formatter data;
  Format.flush_str_formatter()

(* range specific *)
let is_contained d1 d2 = 
  (d1.range_start > d2.range_start) && (d1.range_end <= d2.range_end)
  || (d1.range_start >= d2.range_start) && (d1.range_end < d2.range_end)
let contains d2 d1 = is_contained d1 d2
let same_range d1 d2 = (d1.range_start = d2.range_start) && (d1.range_end = d2.range_end)
(* end range specific *)

(* this is imperfect w.r.t list output sorting *)
let sort a b =
  if (a.range_start = b.range_start) then
    if (a.range_end = b.range_end) then
      (Pervasives.compare a.tag b.tag)
    else if (contains a b) then
      -1
    else
      1
  else
  if (contains a b) then
    -1
  else (* we don't consider overlapping instances for now *)
    Pervasives.compare a.range_start b.range_start

(* DataSet specific functions *)
module DataSet = Set.Make(
  struct 
    type t = data
    let compare = sort
  end)

let mem = DataSet.mem
let add data set = DataSet.add data set    (* DataSet.add *)
let empty = DataSet.empty                  (* DataSet.empty *)
let fold = DataSet.fold
let iter = DataSet.iter
let remove = DataSet.remove
let to_list s = DataSet.elements s
(* checks whether the range is redundant (covered) in our set already *)
let is_covered x dataset =
  DataSet.exists (fun y ->
      y <> x
      && (is_contained x y)
    ) dataset

type dataset = DataSet.t

let pp_dataset ppf dataset =
  Format.fprintf ppf "@[<v 2>@ ";
  iter (fun data -> Format.fprintf ppf "%a@ " pp_data data) dataset;
  Format.fprintf ppf "@]"

let show_dataset dataset =
  pp_dataset Format.str_formatter dataset;
  Format.flush_str_formatter()

let print_dataset dataset =
  pp_dataset Format.std_formatter dataset

type t = {
  data: dataset;
  size: int;
  total_coverage: int;
  total_understood: int;
  percent_coverage: float;
  percent_understood: float;
  tags: string list;
}

let pp ppf t =
  Format.fprintf ppf "@[<v 2>Byte Coverage(%d)" @@ DataSet.cardinal t.data;
  Format.fprintf ppf
    "@ size: 0x%x@ total_coverage: 0x%x@ total_understood: 0x%x@ percent_coverage: %f@ percent_understood: %f@ "
    t.size
    t.total_coverage
    t.total_understood
    t.percent_coverage
    t.percent_understood
  ;
  Format.fprintf ppf "%a" pp_dataset t.data;
  Format.fprintf ppf "@]"

let print coverage =
  pp Format.std_formatter coverage

let is_semantic x = x.tag = Semantic

(* there exists an equal range in the dataset *)
let is_same_range x dataset =
  DataSet.exists (fun y -> same_range x y && x <> y) dataset

(* is a range which is contained by another range *)
let is_sub_range x dataset =
  DataSet.exists (fun y ->
      not (is_semantic y)
      && is_contained x y 
      && x <> y) dataset

let contains_something x dataset =
  DataSet.exists (fun y -> contains x y) dataset

(* is a "byte island";
   there does not exist a range is contained by it *)
let is_unique x dataset =
  not (contains_something x dataset)
  && not (is_sub_range x dataset)

(* checks whether is a top-level container *)
let is_container x dataset =
  not (is_semantic x)
  &&
  ((contains_something x dataset
  && not (is_sub_range x dataset))
  || is_unique x dataset)

(* unused *)
let pick_with f dataset =
  fold (fun data acc ->
      if (f data) then
        data
      else
        acc)

(* partitions data into largest covering ranges, and redundant data *)
let normalize dataset =
  let norm,rest =
    DataSet.partition (fun data ->
        is_container data dataset) dataset
  in
  fold (fun el (acc,rest) ->
      if (is_same_range el acc) then
        acc,(add el rest)
      else
        (add el acc),rest
    ) norm (empty,rest)

(* finalizes the dataset by setting containerhood:
   a container is the first byte range found which is a container
   and which hasn't been added to the accumulator yet
*)
let finalize dataset =
  fold (fun el acc ->
      if (is_container el dataset
          && not (is_same_range el acc)) then
        add {el with container = true} acc
      else
        add el acc) dataset empty

(* creates data; defaults container to false and auto-computes size *)
let create_data
    ~tag:tag 
    ~r1:range_start 
    ~r2:range_end
    ~extra:extra
    ~understood:understood =
  let size = range_end - range_start in
  {size; tag; range_start; range_end; extra; understood; container = false}

(* count the byte ranges via a condition *)
let count data condition =
  fold (fun range acc ->
      if (condition range) then
        range.size + acc
      else
        acc
    ) data 0

(*  counts the coverage, primarily relying on range.container *)
let count_coverage dataset =
  let total = count dataset (fun range ->
      range.container
      && range.tag <> Semantic)
  in
  let understood = count dataset
      (fun range -> range.container
                    && range.understood
                    && range.tag <> Semantic)
  in
  total,understood


let rec zeroes_in_range r1 r2 binary =
  if (r1 >= r2) then true
  else
  if (Bytes.get binary r1 |> Char.code <> 0) then
    false
  else
    zeroes_in_range (r1+1) r2 binary

let zero_scan datalist binary =
  List.map (fun data ->
      if (not data.understood && zeroes_in_range data.range_start data.range_end binary) then
        {data with understood=true; tag=Zero; extra="Zeroes // Computed"}
      else
        data
    ) datalist

(* @invariant sorted, normalized *)
let compute_unknown dataset size binary =
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
            if (debug) then Printf.printf "END %s\n" (show_data data);
            data::acc
        | d1::(d2::rest as tail)->
          if (d1.range_end = d2.range_start || same_range d1 d2) then
            loop acc tail
          else
            (* if it's semantic, and the first's range end is
               greater than the second's start
               (it contains it, which is guaranteed by our sorting), we ignore it *)
          if (d1.tag = Semantic && d1.range_end >= d2.range_start) then
            loop acc tail
          else
            let data = create_data Unknown d1.range_end d2.range_start extra false in
            if (debug) then Printf.printf "NEW %s\n" (show_data data);
            loop (data::acc) tail
      in loop [] bindings
  in
  (* scan for zeroes and fixup *)
  zero_scan unknown binary
  (* add the unknown data back into the dataset *)
  |> List.fold_left (fun dataset data -> add data dataset) dataset

(* the preliminary data comes from an oracle which knows about the binary format, e.g., elf or mach specific counters *)
let create size binary data =
  (* normalize right here, send only the largest covers into compute_unknown, similarly for counting *)
  let normalized_data,rest = normalize data in
  if (debug) then
    begin
      print_string "\nSIGIL Normalized ranges\n\n----------------------\n\n";
      print_dataset normalized_data;
      print_string "\nSIGIL Remainder\n\n---------------------------\n\n";
      print_dataset rest;
      print_string "\n\n---------------------------\n\n";
    end;
  let data = compute_unknown normalized_data size binary
             |> DataSet.union rest |> finalize in
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
    tags;
  }

(* UNIT *)
(* unusual sequences, like one range starting in one range and ending in another need to be dealt with *)
(*
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
let d11 = create_data ~r1:0 ~r2:0x100 ~understood:true ~tag:Code ~extra:"SameContainer51"
let d12 = create_data ~r1:0x350 ~r2:0x400 ~understood:true ~tag:Meta ~extra:"IslandContainer4"


let d0 = create_data ~r1:0x200 ~r2:0x250 ~understood:true ~tag:Meta ~extra:"is_contained31"

let set1 =
  add d1 empty
  |> add d2 |> add d3
  |> add d4 |> add d5
  |> add d6 |> add d7 |> add d8
  |> add d9 |> add d10 |> add d11 |> add d12

let size = 0x400

let one,two = normalize set1
let un1 = compute_unknown one size
let u1 = DataSet.union un1 two
let f1 = finalize u1

let t1 = create size set1

let pd = print_data
let p = print

let m1 = add d0 one
let l1 = m1 |> to_list
*)
