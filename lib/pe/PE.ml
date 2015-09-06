open Binary

module Header = PEHeader


type t =
  {
    header: Header.t;
    size: int;
  }

let get binary =
  let size = Bytes.length binary in
  let header = Header.get_header binary in
  {
    header;
    size;
    (* 
    load_commands; name; nlist; nnlist;
    imports; nimports; exports; nexports;
    is_lib; libraries; nlibraries; raw_code; size;
    byte_coverage = Coverage.compute header load_commands size;
 *)
  }
