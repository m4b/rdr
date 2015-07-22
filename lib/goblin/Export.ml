type t = 
  {
    name: string;     (* name of the exported symbol *)
    offset: int;      (* offset into the containing binary's byte array *)
    size: int;        (* size of the routine, in bytes *)
  }

let print ?like_goblin:(like_goblin=true) export =
  if (like_goblin) then
    Printf.printf "0x%-16x %s (%d)\n" export.offset export.name export.size
  else
    Printf.printf "%s (%d) -> 0x%x\n" export.name export.size export.offset
