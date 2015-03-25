module Obj = Map.Make(String)

exception JSON_exception of string

type json = 
  | JSONObject of json Obj.t
  | JSONArray of json array
  | Float of float
  | Int of int
  | Str of string
  | NULL

let rec json_to_string_it json b identation =
  match json with
  | JSONObject jobj ->
    if (Obj.is_empty jobj) then
      Buffer.add_string b "{}"
    else
      let identation' = identation + 4 in
      let spaces = Bytes.make identation' ' ' |> Bytes.to_string in
      let max,v = Obj.max_binding jobj in
      Buffer.add_string b "{\n";
      Obj.iter (fun key elem -> 
          Buffer.add_string b spaces;
          Buffer.add_string b @@ Printf.sprintf "\"%s\"" key;
          Buffer.add_string b " : ";
          json_to_string_it elem b identation';
          if (key <> max) then
            Buffer.add_string b ",\n"
          else
            Buffer.add_string b "\n"
        ) jobj;
      Buffer.add_string b spaces;
      Buffer.add_string b "}";
  | JSONArray jarray -> 
    let len = Array.length jarray in
    Buffer.add_string b "[";
    Array.iteri (fun i elem -> 
        json_to_string_it elem b identation;
        if (i < len - 1) then
          Buffer.add_string b ", ";
      ) jarray;
    Buffer.add_string b "]";
  | Int i -> string_of_int i |> Buffer.add_string b
  | Float f -> string_of_float f |> Buffer.add_string b
  | Str str -> Buffer.add_string b @@ Printf.sprintf "\"%s\"" str;
  | NULL -> Buffer.add_string b "null"

let json_to_string json = 
  let b = Buffer.create 0 in
  json_to_string_it json b (-4);
  Buffer.contents b

let print_json json = 
  Printf.printf "%s\n" @@ json_to_string json

let emp = Obj.empty
let ( $: ) key value = (Obj.add key value)
let e = ( $: )

let empty = JSONObject Obj.empty

let add key value json = 
  match json with
  | JSONObject obj ->
    JSONObject (Obj.add key value obj)
  | _ -> raise @@ JSON_exception "cannot add key/value pair to a non JSONObject"

let find key json =
  match json with
  | JSONObject obj ->
    Obj.find key obj
  | _ -> raise @@ JSON_exception "cannot get a value from a non JSONObject"

let set json i elem =
  match json with
  | JSONArray arr ->
    arr.(i) <- elem
  | _ -> raise @@ JSON_exception "cannot set a value in a non JSONArray"

let get json i =
  match json with
  | JSONArray arr ->
    arr.(i)
  | _ -> raise @@ JSON_exception "cannot get a value from a non JSONArray"

let unit0 = JSONObject (emp |> e "test" (Float 0.5) |> e "hiya" (Int 20))

let unit1 = JSONObject (Obj.empty |> Obj.add "test" (Float 0.5) |> Obj.add "hiya" (Int 20))

let unit2 = JSONObject (Obj.empty |> e "test" (Float 0.5) |> e "hiya" (Int 20) |> e "array" (JSONArray [| Int 40; Float 60.0; |]))

let unit3 = JSONObject (emp |> e "inside" unit2 |> e "sample" NULL)

let unit4 = JSONObject (emp |> e "inside" unit2 |> e "empty" (JSONObject emp))

let unit5 = empty |> add "inside" unit2 |> add "empty" empty |> add "sample" (JSONArray [|unit0|])
let unit6 = add "inside" unit2 empty

let unit7 = Int 1
let unit8 = JSONArray ([|unit7; Int 2; Int 3; Int 4; Int 5; Int 6;|])

let x = print_json unit2
let y = print_json unit0;;

#install_printer print_json;;
