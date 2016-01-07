type header = {
     flavor: int [@size 4];
     count: int [@size 4];
}

exception Derp

let pp ppf flavor = raise Derp

let show_flavor p = raise Derp

type thread = {
     header: header;
     state: int list;
}

type t =
  | X86_64 of thread
  | Unimplemented of header


(* unimplemented, see machine/thread_status.h for rest of values:
   uint32_t flavor		   flavor of thread state
   uint32_t count		   count of longs in thread state
   struct XXX_thread_state state   thread state for this flavor
   ... *)

let get_thread_state binary offset count =
    let rec loop acc current offset =
    	if (current < count) then
	   List.rev acc
	else
	   let reg,o = Binary.u32o binary offset in
    	   loop (reg::acc) (current + 1) o
	in
    loop [] 0 offset

let get_thread binary o =
    let flavor,o = Binary.u32o binary o in
    let count,o = Binary.u32o binary o in
    let header = {flavor; count} in
    let state = get_thread_state binary o count in
    X86_64 {header; state}

let get_entry thread =
    0x420