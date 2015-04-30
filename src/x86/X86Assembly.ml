type size = int

type operand =
  | Register of int 		(* int because index into registers 0-16 *)
  | Literal of int64 		(* >= 0x8000000000000000 are negative *)
  | Memory of int 		(* which register *)
  | MemoryDisp of int * int64 	(* which register and the displacement *)
(* SIB? | MemorySib of int * int *)
			  
type asm =
  | Add of size * operand * operand
