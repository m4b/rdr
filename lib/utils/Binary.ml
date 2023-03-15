let debug = false

(* legacy, non-offset producing versions, eventually swap out when generating binary structs/records *)
let u64 binary offset = 
  let res = ref (Char.code @@ Bytes.get binary offset) in
  for i = 1 to 7 do
        res := !res lor (Char.code (Bytes.get binary (i + offset)) lsl (i * 8)); (* ugh-life *)
  done;
  !res

let u32 binary offset = 
  let res = ref (Char.code @@ Bytes.get binary offset) in
  for i = 1 to 3 do
        res := !res lor (Char.code (Bytes.get binary (i + offset)) lsl (i * 8)); (* ugh-life *)
  done;
  !res

let u16 binary offset = 
  let one = Char.code @@ Bytes.get binary offset in
  one lor (Char.code (Bytes.get binary (1 + offset)) lsl 8)

let u8 binary offset = 
  Char.code @@ Bytes.get binary offset

(* TODO: SWAP THIS WITH ABOVE TO BREAK EVERYTHING AND FORCE OFFSET CODE MIGRATION *)
(* offset producing version *)
let u64o binary offset = 
  let res = ref (Char.code @@ Bytes.get binary offset) in
  for i = 1 to 7 do
        res := !res lor (Char.code (Bytes.get binary (i + offset)) lsl (i * 8)); (* ugh-life *)
  done;
  !res,(8+offset)

let u32o binary offset = 
  let res = ref (Char.code @@ Bytes.get binary offset) in
  for i = 1 to 3 do
        res := !res lor (Char.code (Bytes.get binary (i + offset)) lsl (i * 8)); (* ugh-life *)
  done;
  !res,(offset+4)

let u16o binary offset = 
  let one = Char.code @@ Bytes.get binary offset in
  (one lor (Char.code (Bytes.get binary (1 + offset)) lsl 8)), (offset+2)

let u8o binary offset = 
  (Char.code @@ Bytes.get binary offset),(offset+1)

(* strings and printing *)

let sub binary max offset =
  let max_idx = max + offset in
  let null_idx = Bytes.index_from binary offset '\000' in
  let len = if (null_idx > max_idx && max > 0) then max_idx else null_idx in
  if (len <= offset) then "",len
  else Bytes.sub_string binary offset (len - offset),len

let string binary ?max:(max=0) offset =
  let str,_ = sub binary max offset in str

let trim_null string =
  let idx = 
    try
      Bytes.index string '\000'
    with
    | Not_found -> 0 
  in
  if (idx <> 0) then
    Bytes.sub_string string 0 idx
  else
    string

let stringo binary ?num_bytes:(count=0) ?max:(max=0) offset =
  if (count > 0) then
    let o = offset+count in
    let string = Bytes.sub_string binary offset count in
    let trim = trim_null string in
    if (debug) then Printf.printf "trim: %s\n" trim;
    if (debug) then Printf.printf "string: %s\n" string;
    trim, o
  else
    let str,len = sub binary max offset in
    if (str = "") then "",offset+1
    else
      str, (len + 1)

let print_bytes binary = 
  let () = Bytes.iter (fun b -> Printf.printf "%x" (Char.code b)) binary in
  Printf.printf "\n"

let print_code binary = 
  let () = Bytes.iter (fun b -> Printf.printf "0x%x " (Char.code b)) binary in
  Printf.printf "\n"

let to_hex_string code =
  let buffer = Buffer.create ((String.length code) * 3) in
  String.iter (fun b -> Printf.sprintf "0x%x " (Char.code b) |> Buffer.add_string buffer) code;
  Buffer.contents buffer
						       
(* big endian *)
						       
let u64be binary offset = 
  let res = ref ((Char.code @@ Bytes.get binary offset) lsl 56) in
  let counter = ref 6 in (* derp whatever *)
  for i = 1 to 7 do
        res := !res lor (Char.code (Bytes.get binary (i + offset)) lsl (!counter * 8)); (* ugh-life *)
	counter := !counter - 1;
  done;
  !res

let u32be binary offset = 
  let res = ref ((Char.code @@ Bytes.get binary offset) lsl 24) in
  let counter = ref 2 in (* derp whatever *)
  for i = 1 to 3 do
        res := !res lor (Char.code (Bytes.get binary (i + offset)) lsl (!counter * 8));
	counter := !counter - 1;
  done;
  !res

let u16be binary offset = 
  let one = Char.code @@ Bytes.get binary offset in
  (one lsl 8) lor (Char.code (Bytes.get binary (1 + offset)))

(* signed integers*)
(* read the byte as a signed integer into our internal representation *)
let i8 binary offset =
  let res = Bytes.get binary offset |> Char.code in
  if (res land 0x80 = 0x80) then res - 256 else res

let _i16_sign = 0x8000
let _i16_subtractor = 0x1_0000

let i16 binary offset =
  let res = ref (Char.code @@ Bytes.get binary offset) in
  for i = 1 to 1 do
        res := !res lor (Char.code (Bytes.get binary (i + offset)) lsl (i * 8));
  done;
  Printf.printf "res: %x\n" !res;
  if (!res land _i16_sign = _i16_sign) then !res - _i16_subtractor else !res						  
						  
let _i32_sign = 0x8000_0000
let _i32_subtractor = 0x1_0000_0000
			
let i32 binary offset =
  let res = ref (Char.code @@ Bytes.get binary offset) in
  for i = 1 to 3 do
        res := !res lor (Char.code (Bytes.get binary (i + offset)) lsl (i * 8));
  done;
  Printf.printf "res: %x\n" !res;
  if (!res land _i32_sign = _i32_sign) then !res - _i32_subtractor else !res


(* TODO: finish this *)
(* 
let iL binary offset size =
  let sign = Int64.of_int 0x80 |> Int64.shift_left size in
  let res = ref ((Bytes.get binary offset) |> Char.code |> Int64.of_int)  in
  for i = 1 to size do
    let i64 = (Bytes.get binary (i + offset)) |> Char.code |> Int64.of_int in
    res := Int64.logor !res (Int64.shift_left i64 (i * 8));
    if (res land 0x800000000L = 0x800000000000000L) then res - 
  done;
  !res

 *)

(* unsigned long longs, exact 64-bit *)
let uL binary offset size = 
  let res = ref ((Bytes.get binary offset) |> Char.code |> Int64.of_int)  in
  for i = 1 to size do
    let i64 = (Bytes.get binary (i + offset)) |> Char.code |> Int64.of_int in
    res := Int64.logor !res (Int64.shift_left i64 (i * 8))
  done;
  !res

let u64L binary offset = uL binary offset 7
let u32L binary offset = uL binary offset 3
let u16L binary offset = uL binary offset 1
let u8L binary offset  = uL binary offset 0

(* To Bytes *)

(* 1,2,4,8 *)
let get_size i =
  if (i >= 0 && i <= 255) then
    1
  else if (i > 255 && i <= 65535) then
    2
  else if (i > 65535 && i <= 4294967295) then
    4
  else
    8

let set_uint bytes integer size offset = 
  for i = 0 to (size - 1) do
    let byte = Char.chr @@ ((integer lsr (8 * i)) land 0xff) in
    Bytes.set bytes (i + offset) byte
  done;
  offset + size

let uint_to_bytes integer size =
  let bytes = Bytes.create size in
  let _ = set_uint bytes integer size 0 in
  bytes

(* 
this is probably unreliable, due to ocaml's tagging and signage 
*)
let int_to_bytes integer =
  let size = get_size integer in
  let bytes = Bytes.create size in
  let _ = set_uint bytes integer size 0 in
  bytes

(* big endian *)
let set_uint_be bytes integer size offset = 
  for i = (size - 1) downto 0 do
    let shift = i * 8 in
    let byte = Char.chr @@ (integer lsr shift) land 0xff in
    Bytes.set bytes i byte
  done;
  offset + size

let uint_be_to_bytes integer size = 
  let bytes = Bytes.create size in
  let _ = set_uint_be bytes integer size 0 in
  bytes

(* to bytes testing
let elfk = 0x7f454c46

let u1 = (u16 (uint_to_bytes 0xdead 2) 0) = 0xdead
let u2 = (u32 (uint_to_bytes 0xdeadbeef 4) 0) = 0xdeadbeef
let u3 = (u64 (uint_to_bytes 0x7eadbeefbeefdead 8) 0) = 0x7eadbeefbeefdead
let u4 = (u32 (uint_be_to_bytes elfk 4) 0) = elfk
 *)

(*
testing
let signed_unit1 = Array.init 256 (fun i -> i) |> Array.to_list;;
let signed_res1 = (List.map (fun tmp -> if (tmp land 0x80 = 0x80) then tmp - 256 else tmp) signed_unit1) = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39; 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 54; 55; 56; 57; 58; 59; 60; 61; 62; 63; 64; 65; 66; 67; 68; 69; 70; 71; 72; 73; 74; 75; 76; 77; 78; 79; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90; 91; 92; 93; 94; 95; 96; 97; 98; 99; 100; 101; 102; 103; 104; 105; 106; 107; 108; 109; 110; 111; 112; 113; 114; 115; 116; 117; 118; 119; 120; 121; 122; 123; 124; 125; 126; 127; -128; -127; -126; -125; -124; -123; -122; -121; -120; -119; -118; -117; -116; -115; -114; -113; -112; -111; -110; -109; -108; -107; -106; -105; -104; -103; -102; -101; -100; -99; -98; -97; -96; -95; -94; -93; -92; -91; -90; -89; -88; -87; -86; -85; -84; -83; -82; -81; -80; -79; -78; -77; -76; -75; -74; -73; -72; -71; -70; -69; -68; -67; -66; -65; -64; -63; -62; -61; -60; -59; -58; -57; -56; -55; -54; -53; -52; -51; -50; -49; -48; -47; -46; -45; -44; -43; -42; -41; -40; -39; -38; -37; -36; -35; -34; -33; -32; -31; -30; -29; -28; -27; -26; -25; -24; -23; -22; -21; -20; -19; -18; -17; -16; -15; -14; -13; -12; -11; -10; -9; -8; -7; -6; -5; -4; -3; -2; -1]


let unit1 = List.map (fun x -> Char.chr x) [0xca; 0xfe; 0xba; 0xbe;]

let e1  = Bytes.init (List.length unit1) (fun i -> List.nth unit1 i)

let res = (i32be e1 0) = 0xcafebabe
let e2 = "\255\255\255\255\255\255\255\255"
let res2 = u64L e2 0 = -1L

 *)
