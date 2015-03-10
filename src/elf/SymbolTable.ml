(* Legal values for ST_BIND subfield of st_info (symbol binding).  *)
let kSTB_LOCAL = 0  (* Local symbol *)
let kSTB_GLOBAL = 1  (* Global symbol *)
let kSTB_WEAK = 2  (* Weak symbol *)
let kSTB_NUM =  3  (* Number of defined types.  *)
let kSTB_LOOS = 10  (* Start of OS-specific *)
let kSTB_GNU_UNIQUE = 10  (* Unique symbol.  *)
let kSTB_HIOS = 12  (* End of OS-specific *)
let kSTB_LOPROC = 13  (* Start of processor-specific *)
let kSTB_HIPROC = 15  (* End of processor-specific *)

(* Legal values for ST_TYPE subfield of st_info (symbol type).  *)

let kSTT_NOTYPE = 0  (* Symbol type is unspecified *)
let kSTT_OBJECT = 1  (* Symbol is a data object *)
let kSTT_FUNC = 2  (* Symbol is a code object *)
let kSTT_SECTION = 3  (* Symbol associated with a section *)
let kSTT_FILE = 4  (* Symbol's name is file name *)
let kSTT_COMMON = 5  (* Symbol is a common data object *)
let kSTT_TLS =  6  (* Symbol is thread-local data object*)
let kSTT_NUM =  7  (* Number of defined types.  *)
let kSTT_LOOS = 10  (* Start of OS-specific *)
let kSTT_GNU_IFUNC = 10  (* Symbol is indirect code object *)
let kSTT_HIOS = 12  (* End of OS-specific *)
let kSTT_LOPROC = 13  (* Start of processor-specific *)
let kSTT_HIPROC = 15  (* End of processor-specific *)
(* ================ *)

let symbol_bind_to_string bind =
  match bind with
  | 0 -> "LOCAL"
  | 1 -> "GLOBAL"
  | 2 -> "WEAK"
  | 3 -> "NUM "
  | 10 -> "LOOS"
  | 10 -> "GNU_UNIQUE"
  | 12 -> "HIOS"
  | 13 -> "LOPROC"
  | 15 -> "HIPROC"
  | _ -> "UNKNOWN SYMBOL BIND"

let symbol_type_to_string sttype =
  match sttype with
  | 0 -> "NOTYPE"
  | 1 -> "OBJECT"
  | 2 -> "FUNC"
  | 3 -> "SECTION"
  | 4 -> "FILE"
  | 5 -> "COMMON"
  | 6 -> "TLS "
  | 7 -> "NUM "
  | 10 -> "LOOS"
  | 10 -> "GNU_IFUNC"
  | 12 -> "HIOS"
  | 13 -> "LOPROC"
  | 15 -> "HIPROC"
  | _ -> "UNKNOWN SYMBOL TYPE"

(* 
 * How to extract and insert information held in the st_info field.

#define ELF32_ST_BIND(val)  (((unsigned char) (val)) >> 4)
#define ELF32_ST_TYPE(val)  ((val) & 0xf)
#define ELF32_ST_INFO(bind, type) (((bind) << 4) + ((type) & 0xf))
 *)

let get_bind info = info lsr 4
let get_type info = info land 0xf

(* Symbol visibility specification encoded in the st_other field.  *)
let kSTV_DEFAULT = 0  (* Default symbol visibility rules *)
let kSTV_INTERNAL = 1  (* Processor specific hidden class *)
let kSTV_HIDDEN = 2  (* Sym unavailable in other modules *)
let kSTV_PROTECTED = 3  (* Not preemptible, not exported *)

let symbol_visibility_to_string visibility =
  match visibility with
  | 0 -> "DEFAULT"
  | 1 -> "INTERNAL"
  | 2 -> "HIDDEN"
  | 3 -> "PROTECTED"
  | _ -> "UKNOWN SYMBOL VISIBILITY"

(* How to extract and insert information held in the st_other field. *)
	   
let get_visibility other = other land 0x03

(* =========================== *)
(*
            typedef struct {
               uint32_t      st_name;
               unsigned char st_info;
               unsigned char st_other;
               uint16_t      st_shndx;
               Elf64_Addr    st_value;
               uint64_t      st_size;
           } Elf64_Sym;
*)

type symbol_entry =
  {
    resolved_name: string;
    st_name: int;   (* 4 *)
    st_info: int;  (* 1 *)
    st_other: int;  (* 1 *)
    st_shndx: int;  (* 2 *)
    st_value: int;  (* 8 *)
    st_size: int;  (* 8 *)
  }

let symbol_to_string symbol =
  Printf.sprintf "%s 0x%x %s %s %s index: %d "
   symbol.resolved_name
   symbol.st_value
   (get_bind symbol.st_info |> symbol_bind_to_string)
   (get_type symbol.st_info |> symbol_type_to_string)
   (get_visibility symbol.st_other |> symbol_visibility_to_string)
   symbol.st_shndx
