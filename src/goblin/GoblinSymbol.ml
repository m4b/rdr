(* 
TODO: 
(1) size computation requires load command segment details (where stubs/text end, etc.) and/or nlist data in order to be precise, otherwise:
  a. the final symbol's boundary isn't known,
  b. non-exported locals' size is accidentally taken into consideration when computing a symbol
  c. section details aren't taken into consideration, like text / stub_helper boundary, which adds a huge fake amount to the computed size
   of a symbol occuring at the edge of a boundary;

(2) use an object? that way no list searching, and can guarantee the existence of certain datums, via types?

 *)

(* 
   Attempt to generically represent symbol information, primarily for printing and analysis, i.e., computing size, etc.
 *)

type symbol_kind = 
  | Export | Import | Local | Debug | Other

type symbol_datum = 
  [ 
    | `Name of string
    | `Offset of int 
    | `Size of int
    | `Lib of string
    | `Kind of symbol_kind
    | `PrintableData of string
  ]

let kORDINAL_LEFT = 1
let kORDINAL_CENTER = 5
let kORDINAL_RIGHT = 11
let kORDINAL_RIGHTMOST = 1000

let symbol_datum_ordinal = 
  function
  | `Offset _ -> kORDINAL_LEFT
  | `Name _ -> kORDINAL_LEFT + 1
  | `Size _ -> kORDINAL_LEFT + 2
  | `Lib _ -> kORDINAL_CENTER
  | `PrintableData _ ->  kORDINAL_RIGHT - 1
  | `Kind _ -> kORDINAL_CENTER - 1
  |  _ ->  kORDINAL_RIGHTMOST

let symbol_kind_to_string =
  function
  | Export -> "E"
  | Import -> "I"
  | Local -> "l"
  | Debug -> "d"
  | Other -> "O"

let symbol_datum_to_string ?use_kind:(use_kind=false) ?use_lib:(use_lib=true) ?use_printable:(use_printable=true) =
  function
  | `Name string -> string
  | `Offset o -> Printf.sprintf "0x%x" o
  | `Lib lib -> if (use_lib) then "-> " ^ lib else ""
  | `Size s -> Printf.sprintf "(%d)" s
  | `PrintableData string -> if (use_printable) then string else ""
  (* i know not supposed to do this but whatever *)
  | `Kind kind -> Printf.sprintf "%s" @@ symbol_kind_to_string kind
  | _ -> ""

(* probably should just use an object, 
   since a list of polymorphic variants essentially mimics many features of objects *)
(* data = [datum] *)
let sort_symbol_data data =
  List.sort (fun a b ->
      let e1 = symbol_datum_ordinal a in
      let e2 = symbol_datum_ordinal b in
      Pervasives.compare e1 e2
    ) data

(* assuming we receive symbol_datum list *)
let rec find_symbol_name = 
  function
  | [] -> raise Not_found
  | `Name string :: _ ->
    string
  | _::rest -> find_symbol_name rest

(* assuming we receive symbol_datum list *)
let rec find_symbol_lib = 
  function
  | [] -> raise Not_found
  | `Lib string :: _ ->
    string
  | _::rest -> find_symbol_lib rest

(* this definitely can be _not_ found, especially with mach *)
let rec find_symbol_offset = 
  function
  | [] -> raise Not_found
  | `Offset o :: _ ->
    o
  | _::rest -> find_symbol_offset rest

let rec find_symbol_printable = 
  function
  | [] -> raise Not_found
  | `PrintableData data :: _ ->
    data
  | _::rest -> find_symbol_printable rest

let rec find_symbol_size = 
  function
  | [] -> raise Not_found
  | `Size size :: _ ->
    size
  | _::rest -> find_symbol_size rest

let rec find_symbol_kind = 
  function
  | [] -> raise Not_found
  | `Kind kind :: _ ->
    kind
  | _::rest -> find_symbol_kind rest

let symbol_data_to_string ?basic_export:(basic_export=false) data =
  if (basic_export) then
    let name = find_symbol_name data in
    let offset = try Printf.sprintf " @ 0x%x" @@ find_symbol_offset data with Not_found -> "" in
    let size = try Printf.sprintf " (%d)" @@ find_symbol_size data with Not_found -> "" in
    Printf.sprintf "%s%s%s" name size offset
  else
    let data = sort_symbol_data data in
    let b = Buffer.create ((List.length data) * 15) in
    List.iter (fun elem ->
        Buffer.add_string b @@ symbol_datum_to_string elem;
        Buffer.add_string b " "
      ) data;
    Buffer.contents b

let print_symbol_data ?like_nlist:(like_nlist=false) data =
  if (like_nlist) then
    let offset = try Printf.sprintf "%016x" @@ find_symbol_offset data with Not_found -> Printf.sprintf "%16d" 0 in
    let kind = find_symbol_kind data |> symbol_kind_to_string in
    let name = find_symbol_name data in
    Printf.printf "%s %s %s\n" offset kind name
  else
    let offset = try Printf.sprintf "%016x" @@ find_symbol_offset data with Not_found -> Printf.sprintf "%16d" 0 in
    let size = try Printf.sprintf " (%d)" @@ find_symbol_size data with Not_found -> "" in
    let kind = find_symbol_kind data |> symbol_kind_to_string in
    let name = find_symbol_name data in
    Printf.printf "%s%s @ %s %s\n" name size offset kind

let sort_symbols list =
  (* symbol -> [[datum]] *)
  List.sort (fun a b ->       (* [[datum]] , [[datum]] *)
      (* such needs of monads right now *)
      (* first compare libs, export must have a lib *)
      let l1 = find_symbol_lib a in
      let l2 = find_symbol_lib b in
      if (l1 = l2) then
        try 
          let o1 = find_symbol_offset a in
          try 
            let o2 = find_symbol_offset b in
            Pervasives.compare o1 o2
          with Not_found ->
            1
          with Not_found ->
            -1
      else
        Pervasives.compare l1 l2
	    ) list

let sort_symbols_with sortf listorarray =
  (* symbol -> [[datum]] *)
  sortf (fun a b ->       (* [[datum]] , [[datum]] *)
      (* such needs of monads right now *)
      (* first compare libs, export must have a lib *)
      let l1 = find_symbol_lib a in
      let l2 = find_symbol_lib b in
      if (l1 = l2) then
        try 
          let o1 = find_symbol_offset a in
          try 
            let o2 = find_symbol_offset b in
            Pervasives.compare o1 o2
          with Not_found ->
            1
          with Not_found ->
            -1
      else
        Pervasives.compare l1 l2
    ) listorarray

(* TODO: needs extra data for more accurate calculation *)
(* @invariant (sorted list) *)
let compute_size text_boundary symbols =
  let rec loop symbols acc =
    match symbols with
    | [] -> acc
    | symbol::[] ->
      begin
        try 
          let offset = find_symbol_offset symbol in
          ((`Size (text_boundary - offset)::symbol)::acc)
        with
        | Not_found -> (symbol::acc)
      end
    | symbol1::symbol2::symbols ->
      begin
        try 
          let offset = find_symbol_offset symbol1 in
          let next_boundary = find_symbol_offset symbol2 in
          loop (symbol2::symbols) ((`Size (next_boundary - offset)::symbol1)::acc)
        with
        | Not_found -> loop (symbol2::symbols) (symbol1::acc)
      end
  in loop symbols []

let to_goblin_export symbol =
  let name = find_symbol_name symbol in
  let offset = try find_symbol_offset symbol with Not_found -> 0x0 in
  let size = try find_symbol_size symbol with Not_found -> 0x0 in
  {Goblin.Export.name; offset; size}
