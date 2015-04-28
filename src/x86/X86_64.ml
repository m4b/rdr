(* TODO:
(1) not sure this approach is going to work;
    for raw_inst, I can't know _not_ to read a modRM or sib or displacement without first consulting the opcode; which means I need to perform a lookup using the opcode to tell me whether to eat a modRM or a sib or a displacement or an immediate; but then, if I'm doing a table lookup, I probably should just do all the work in the table lookup, including generating the disassembled instruction, etc?
 *)

(* prefix all these with x86 ? *)

(*         register contents     [register]      [register] + disp                    [register * sib] *)
(* type operand = [ `Register of int | `Memory of int | `MemoryDisp of int * int] (* SIB? | MemorySib of int * int *) *)

type operand = Register of int | Memory of int | MemoryDisp of int * int (* SIB? | MemorySib of int * int *)

type opcode = | OneByte of int | TwoByte of int * int | ThreeByte of int * int * int								       
type raw_inst =
  {
    legacy_prefix     : int list option;
    mandatory_prefix  : int option;
    rex_or_vex        : int option; (* rex with vex is #UD *)
    opcode            : opcode;
    modRM             : int option;
    sib               : int option;
    displacement      : bytes option;
    immediate         : bytes option;
  }
(* 
type group1 = [ `LOCK | `REPNE | `REP]    
type legacy_prefix = gro
 *)
let is_legacy_prefix prefix =
  match prefix with
  | 0xf0 | 0xf2 | 0xf3                      (* group1  *)
  | 0x2e | 0x36 | 0x3e | 0x26 | 0x64 | 0x65 (* group2  *)
  | 0x66					   (* group 3 *)
  | 0x67					   (* group 4 *)
    -> true
  | _ -> false

let leg1 = "\xf0\xf2\x2e\xff\x25\xd8"
	   
let is_rex prefix = prefix >= 0x40 && prefix <= 0x4f

(* this index out of bounds if "\xf0" or "", for example *)
let get_legacy_prefixes bytes offset =
  let rec loop acc offset =
    let byte = Char.code @@ Bytes.get bytes offset in
    if (is_legacy_prefix byte) then
      loop (byte::acc) (offset + 1)
    else
      Some (List.rev acc), offset
  in
  let byte = Char.code @@ Bytes.get bytes offset in
  if (is_legacy_prefix byte) then
    loop [byte] (offset + 1)
  else
    None, offset

let unit1 = (get_legacy_prefixes leg1 0) = (Some [240; 242; 46],3)
					     
(* mandatory prefix always before a rex or vex so sort of need a lookahead? *)
let get_rex_or_vex bytes offset =
  let byte = Binary.u8 bytes offset in
  if (is_rex byte) then
    Some byte
  else
    None

exception Unknown_instruction

(* i should probably just decode here, instead of turning it into a raw, then decoding it again... *)
let get_opcode bytes offset legacy_prefix mandatory_prefix rex_or_vex =
  let opcode = Binary.u8 bytes offset in
  match opcode with
  | 0x50 | 0x51
  | 0x52 | 0x53
  | 0x54 | 0x55
  | 0x56 | 0x57 ->
	    let opcode = OneByte opcode in
	    let modRM = None in
	    let sib = None in
	    let displacement = None in
	    let immediate = None in
      {legacy_prefix; mandatory_prefix; rex_or_vex; opcode; modRM; sib; displacement; immediate}
  | 0x58 | 0x59
  | 0x5a | 0x5b
  | 0x5c | 0x5d
  | 0x5e | 0x5f ->
	    let opcode = OneByte opcode in
	    let modRM = None in
	    let sib = None in
	    let displacement = None in
	    let immediate = None in
	    {legacy_prefix; mandatory_prefix; rex_or_vex; opcode; modRM; sib; displacement; immediate}
  | _ -> raise Unknown_instruction
      
let get_raw_inst bytes = raise Not_found

(* addl	1(%rip), %eax *)
let unit1 = "0x03 0x05 0x1 0x0 0x0 0x0"			       
