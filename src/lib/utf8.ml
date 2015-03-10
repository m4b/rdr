(* ocamlopt.opt utf8.ml -o utf8 *)

let ( $ ) f x = f x

let rec case pred item list =
  match list with
      [] -> raise Not_found
    | x::xs -> 
	if pred item (fst x) then
	  snd x
	else
	  case pred item xs

let getSeq codepoint = 
  case (<=) codepoint [(0x7f,1);
		  (0x7ff,2);
		  (0xffff,3);
		  (0x10ffff,4)]
(* maximum is below, but unicode specifies nothing above 0x10ffff *)

let returnString s charlist =
let rec returnString' s charlist offset =
  match charlist with
    [] -> s
  | c::cs -> 
      begin 
	s.[offset] <- c;
	returnString' s cs (offset + 1)
      end
in returnString' s charlist 0 

(* doesn't seem to need '\000' *)
let from_bytes bytes offset =
  let null_index = Bytes.index_from bytes offset '\000' in
  Bytes.sub_string bytes offset (null_index - 1)

(* get rid of deprecations *)
let from_int codepoint =
  let numBytes = getSeq codepoint in
  match numBytes with
      1 -> 
	Char.escaped $ Char.chr codepoint
    | 2 -> 
	let one = Char.chr $ 0b11000000 lor (codepoint lsr 6) in
	let two = Char.chr $ 0b10000000 lor (codepoint land 0x3f) in
	let s = String.create numBytes in
	returnString s [one;two]

    | 3 ->
	let one = Char.chr $ 0b11100000 lor (codepoint lsr 12) in
	let two = Char.chr $ 0b10000000 lor ((codepoint land 0xfc0) lsr 6) in
	let three = Char.chr $ 0b10000000 lor (codepoint land 0x3f) in
	let s = String.create numBytes in
	returnString s [one;two;three]

    | 4 ->
	let one = Char.chr $ 0b11110000 lor (codepoint lsr 18) in
	let two = Char.chr $ 0b10000000 lor ((codepoint land 0x3f000) lsr 12) in
	let three = Char.chr $ 0b10000000 lor ((codepoint land 0xfc0) lsr 6) in
	let four = Char.chr $ 0b10000000 lor (codepoint land 0x3f) in
	let s = String.create numBytes in
	returnString s [one;two;three;four]

    | _ ->
	raise Not_found

let u1 = ['\xc8';'\x80';'\x0a';'\x00';'\x02';'\x01';'\x01';'\x00';]
let e1 = Bytes.init (List.length u1) (fun i -> List.nth u1 i)

(* c880 0a *)
