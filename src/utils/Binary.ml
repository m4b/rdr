let i64 binary offset = 
  let res = ref (Char.code @@ Bytes.get binary offset) in
  for i = 1 to 7 do
        res := !res lor (Char.code (Bytes.get binary (i + offset)) lsl (i * 8)); (* ugh-life *)
  done;
  !res

let i32 binary offset = 
  let res = ref (Char.code @@ Bytes.get binary offset) in
  for i = 1 to 3 do
        res := !res lor (Char.code (Bytes.get binary (i + offset)) lsl (i * 8)); (* ugh-life *)
  done;
  !res

let i16 binary offset = 
  let one = Char.code @@ Bytes.get binary offset in
  one lor (Char.code (Bytes.get binary (1 + offset)) lsl 8)

let i8 binary offset = 
  Char.code @@ Bytes.get binary offset

(* TODO: don't rely on zero terminate if possible, use length as optional argument? *)
let istring binary offset =
  let null_index = Bytes.index_from binary offset '\000' in
  if (null_index = offset) then ""
  else Bytes.sub_string binary offset (null_index - offset)

let stringo binary offset =
  let null_index = Bytes.index_from binary offset '\000' in
  if (null_index = offset) then "",offset+1
  else
    (Bytes.sub_string binary offset (null_index - offset)), (null_index + 1)

let i64be binary offset = 
  let res = ref ((Char.code @@ Bytes.get binary offset) lsl 56) in
  let counter = ref 6 in (* derp whatever *)
  for i = 1 to 7 do
        res := !res lor (Char.code (Bytes.get binary (i + offset)) lsl (!counter * 8)); (* ugh-life *)
	counter := !counter - 1;
  done;
  !res

let i32be binary offset = 
  let res = ref ((Char.code @@ Bytes.get binary offset) lsl 24) in
  let counter = ref 2 in (* derp whatever *)
  for i = 1 to 3 do
        res := !res lor (Char.code (Bytes.get binary (i + offset)) lsl (!counter * 8));
	counter := !counter - 1;
  done;
  !res

let i16be binary offset = 
  let one = Char.code @@ Bytes.get binary offset in
  (one lsl 8) lor (Char.code (Bytes.get binary (1 + offset)))

let print_bytes binary = 
  let () = Bytes.iter (fun b -> Printf.printf "%x" (Char.code b)) binary in
  Printf.printf "\n"



(*
testing
let unit1 = List.map (fun x -> Char.chr x) [0xca; 0xfe; 0xba; 0xbe;]

let e1  = Bytes.init (List.length unit1) (fun i -> List.nth unit1 i)

let res = (i32be e1 0) = 0xcafebabe
 *)


(* ==================== *)
