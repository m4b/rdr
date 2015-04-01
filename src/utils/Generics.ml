let list_to_string ?omit_singleton_braces:(osb=false) list =
  let len = List.length list in
  if (len = 0) then "[]"
  else if (len = 1 && osb) then List.hd list
  else
    let b = Buffer.create ((List.hd list |> String.length) * len) in
    Buffer.add_string b "[";
    let rec loop ss = 
      match ss with
      | [] ->
        Buffer.contents b
      | s::[] ->
        Buffer.add_string b s;
        Buffer.add_string b "]";
        Buffer.contents b
      | s::ss ->
        Buffer.add_string b s;
        Buffer.add_string b ", ";
        loop ss
    in loop list

let list_with_stringer ?omit_singleton_braces:(osb=false) stringer list =
  let len = List.length list in
  if (len = 0) then "[]"
  else if (len = 1 && osb) then List.hd list |> stringer
  else
    let b = Buffer.create ((List.hd list |> stringer |> String.length) * len) in
    Buffer.add_string b "[";
    let rec loop ss = 
      match ss with
      | [] ->
        Buffer.contents b
      | s::[] ->
        Buffer.add_string b (stringer s);
        Buffer.add_string b "]";
        Buffer.contents b
      | s::ss ->
        Buffer.add_string b (stringer s);
        Buffer.add_string b ", ";
        loop ss
    in loop list

let string_tuple_list_to_string list =
  let len = List.length list in
  if (len = 0) then "[]"
  else if (len = 1) then 
    let lib,data = List.hd list in
    Printf.sprintf "%s @ %s" lib data
  else
    let b = Buffer.create ((List.hd list |> fst |> String.length) * len) in
    Buffer.add_string b "[";
    let rec loop ss = 
      match ss with
      | [] ->
        Buffer.contents b
      | (lib,data)::[] ->
        Buffer.add_string b lib;
        Buffer.add_string b " @ ";
        Buffer.add_string b data;
        Buffer.add_string b "]";
        Buffer.contents b
      | (lib, data)::ss ->
        Buffer.add_string b lib;
        Buffer.add_string b " @ ";
        Buffer.add_string b data;
        Buffer.add_string b ", ";
        loop ss
    in loop list      


let find pred arr =
  let size = Array.length arr in
  let rec loop i =
    if (i >= size) then raise Not_found
    else
      if (pred arr.(i)) then
	arr.(i)
      else
	loop (i + 1)
  in loop 0
      
  
