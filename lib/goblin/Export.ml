type t = 
  {
    name: string;     (* name of the exported symbol *)
    offset: int;      (* offset into the containing binary's byte array *)
    size: int;        (* size of the routine, in bytes *)
  }
