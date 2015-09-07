open Binary

module Header = PEHeader
module Import = PEImport

type t =
  {
    header: Header.t;
    size: int;
  } [@@deriving (show, yojson)]

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
