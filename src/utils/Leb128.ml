(* 
for testing
#directory "/Users/matthewbarney/projects/binreader/_build/src/utils/";;
#load "Binary.cmo";;
 *)

open Binary

let d byte_stream =
  let rec loop pos shift acc =
    let byte = Char.code @@ List.nth byte_stream pos in
    let high_order_bit_is_zero = (byte land 0x80) = 0x0 in
    let acc' = acc lor ((byte land 0x7f) lsl shift) in
    if (high_order_bit_is_zero) then
      acc'
    else
      loop (pos + 1) (shift + 7) acc' in
  loop 0 0 0

let d2 byte_stream =
  let rec loop pos shift acc =
    let byte = Char.code @@ List.nth byte_stream pos in
    let high_order_bit_is_zero = (byte land 0x80) = 0x0 in
    let acc' = acc lor ((byte land 0x7f) lsl shift) in
    if (high_order_bit_is_zero) then
      (acc', pos+1)
    else
      loop (pos + 1) (shift + 7) acc' in
  loop 0 0 0

let decode_uleb128 byte_stream =
  let rec loop pos shift acc =
    let byte = Binary.i8 byte_stream pos in
    let high_order_bit_is_zero = (byte land 0x80) = 0x0 in
    let acc' = acc lor ((byte land 0x7f) lsl shift) in
    if (high_order_bit_is_zero) then
      acc'
    else
      loop (pos + 1) (shift + 7) acc' in
  loop 0 0 0

let num_bytes integer = 
  let rec loop integer acc =
    let integer' = integer lsr 7 in
    if integer' == 0 then
      acc
    else
      loop integer' (acc + 1) in
  loop integer 1

(* hasn't been tested extensively at all *)
let encode_uleb128 integer = 
  let size = num_bytes integer in
  let byte_stream = Bytes.create size in
  (* shift integer to the right by mod 7s, 
     then bitmask with 0111 1111 (0x7f)
     then set the highest bit (lor 0x80), repeat 
  *)  
  for i = 0 to size - 2 do
    let shift = i * 7 in
    let byte = ((integer lsr shift) land 0x7f) lor 0x80 in
    Bytes.set byte_stream i (Char.chr byte);
  done;
  (* perform the final byte encoding with 0 for msb *)
  let byte = (integer lsr (7 * (size - 1))) land 0x7f in
  Bytes.set byte_stream (size - 1) (Char.chr byte);
  byte_stream

(*  624485  = 0xE5 0x8E 0x26  = "\229\142&" *)

(* TODO: I'm pretty sure some bind commands might be relying on integer overflow, but since ocaml tags using a bit, we might not get this effect? *)
(* returns decoded uleb128 and next position in stream *)
let get_uleb128 byte_stream pos =
  let rec loop pos shift acc =
    let byte = Binary.i8 byte_stream pos in
    let high_order_bit_is_zero = (byte land 0x80) = 0x0 in
    let acc' = acc lor ((byte land 0x7f) lsl shift) in
    if (high_order_bit_is_zero) then
      (acc',pos+1)
    else
      loop (pos + 1) (shift + 7) acc' in
  loop pos 0 0

let print_uleb128 bytes =
  Bytes.iter (fun byte -> Printf.printf "%x " @@ Char.code byte) bytes;
  Printf.printf "\n"

(* result = 0;
   shift = 0;
   size = number of bits in signed integer;
   while(true) {
   byte = next byte in input;
   result |= (low order 7 bits of byte << shift);
   shift += 7;
   if (high order bit of byte == 0)
   break;
   }


   /* sign bit of byte is second high order bit (0x40) */
   if ((shift <size) && (sign bit of byte is set))
   /* sign extend */
   this right here took me forever to understand and i still dont
   result |= - (1 << shift); *)

(* returns decoded sleb128 and next position in stream *)
let get_sleb128 byte_stream pos =
  let rec loop pos shift acc =
    let byte = Binary.i8 byte_stream pos in
    let high_order_bit_is_zero = (byte land 0x80) = 0x0 in
    let acc' = acc lor ((byte land 0x7f) lsl shift) in
    if (high_order_bit_is_zero) then
      let sign_bit = byte land 0x40 in
      if (sign_bit = 0x40) then
        begin
          (* derp... lerp? *)
          let acc' =  acc' lor -(1 lsl (shift + 7)) in 
          acc', pos + 1 
        end
      else
        begin
          acc', pos + 1 
        end
    else
      loop (pos + 1) (shift + 7) acc' 
  in loop pos 0 0;;

(* 
encoded      unsigned          signed
bytes        interpretation    interpretation
-------      --------------    --------------
10           +10               +10
45           +45               -3b
8e 32        +190e             +190e
c1 57        +2bc1             -143f
80 80 80 3f  +7e00000          +7e00000
80 80 80 4f  +9e00000          -6200000
*)

(* testing *)
let gs bytes = 
  let b = Bytes.init (List.length bytes) (fun i -> Char.chr @@ List.nth bytes i) in
  get_sleb128 b 0

let gu bytes = 
  let b = Bytes.init (List.length bytes) (fun i -> Char.chr @@ List.nth bytes i) in
  get_uleb128 b 0

let hext i = 
  Printf.printf "0x%x\n" @@ fst i

let hex i = 
  Printf.printf "0x%x\n" i

let p i = Printf.printf "%d\n" @@ fst i

let unita = List.map (fun x -> Char.chr x) [0xca; 0xfe; 0xba; 0xbe;]
let unit0 = [0x10]
let unit1 = [0x45]
let unit2 = [0x8e; 0x32]
let unit3 = [0xc1; 0x57]
let unit4 = [0x80; 0x80; 0x80; 0x3f] (*+0x7e00000*) (*+0x7e00000*)
let unit5 = [0x80; 0x80; 0x80; 0x4f] (* +0x9e00000 -0x6200000 *)

let res = -624485
(* -624485 *)
let u1 = [0x9b; 0xf1; 0x59];

  (* 
let e1  = Bytes.init (List.length unit1) (fun i -> List.nth unit1 i)

 *)
  (* printf = "b0d2 10" @ 0x9cc92 in libsystem_c.dylib *)
