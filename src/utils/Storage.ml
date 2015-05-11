let dot_directory_name = "rdr" ^ Filename.dir_sep

let get_dot_directory () =
  (Sys.getenv "HOME") ^ Filename.dir_sep ^ "." ^ dot_directory_name

let create_dot_directory () =
  if (not @@ Sys.file_exists @@ get_dot_directory ()) then
    Unix.mkdir (get_dot_directory ()) 0o740

let get_path filename =
  (get_dot_directory ()) ^ filename

let graph_name = "lib_dependency_graph.gv"

let get_path_graph () =
  get_path graph_name



