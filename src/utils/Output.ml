let dot_directory_name = "rdr" ^ Filename.dir_sep

let get_dot_directory () =
  (Sys.getenv "HOME") ^ Filename.dir_sep ^ "." ^ dot_directory_name

let create_dot_directory () =
  try
    Unix.access (get_dot_directory ()) [Unix.F_OK]
  with
  | Unix.Unix_error (Unix.ENOENT,_,_) ->
    Unix.mkdir (get_dot_directory ()) 0o740

let with_dot_directory filename =
  (get_dot_directory ()) ^ filename
      
