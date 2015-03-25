open Printf

let readAll fileName =
  let ic = open_in_bin fileName in
  let byte = ref 0 in
  printf "size: %d\n" (in_channel_length ic);
  try 
    while (true) do
      begin
	byte := input_byte ic;
	printf "%x " (!byte)
      end
    done
  with End_of_file -> 
    begin
      printf "\n";
	 exit 0
       end

let input_i32 ic = 
  let one = input_byte ic in
  let two = (input_byte ic) lsl 8 in
  let three = (input_byte ic) lsl 16 in 
  let four = (input_byte ic) lsl 24 in
  one lor two lor three lor four

(* big endian *)
let input_i32be ic = 
  let one = (input_byte ic) lsl 24 in
  let two = (input_byte ic) lsl 16 in
  let three = (input_byte ic) lsl 8 in 
  let four = input_byte ic in
  one lor two lor three lor four

let discard_n_bytes n ic = 
  seek_in ic ((pos_in ic) + n)

let input_n_bytes n ic = 
  let res = ref (input_byte ic) in
  for i = 1 to (n - 1) do
    let counter = 8 * i in
    let byte = input_byte ic in
    res := !res lor (byte lsl counter)
  done;
  !res
