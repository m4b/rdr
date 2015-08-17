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
