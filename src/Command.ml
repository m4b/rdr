let program_in_path program_name =
  (* nuuuuu hacks *)
  let ret = Sys.command
	    @@ Printf.sprintf
		 "which %s 2>&1 > /dev/null" program_name
  in
  ret = 0

let is_linux () = 
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  uname = "Linux"

let is_osx () = 
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  uname = "Darwin"

let disassemble file offset size =
  let ic = open_in_bin file in
  seek_in ic offset;
  let code = really_input_string ic size |> Binary.to_hex_string in
  close_in ic;
  flush stdout;
  Printf.printf "\t\n";
  (* NOW FOR THE HACKS *)
  if (program_in_path "llvm-mc") then
    Sys.command
    @@ Printf.sprintf
	 "echo \"%s\" | llvm-mc --disassemble" code
    |> ignore
  else
    Printf.eprintf "Error: llvm-mc not installed or not in ${PATH}\n";
