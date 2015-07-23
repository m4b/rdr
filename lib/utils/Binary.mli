val u64 : bytes -> int -> int
val u32 : bytes -> int -> int
val u16 : bytes -> int -> int
val u8 : bytes -> int -> int
val u64o : bytes -> int -> int * int
val u32o : bytes -> int -> int * int
val u16o : bytes -> int -> int * int
val u8o : bytes -> int -> int * int
val string : bytes -> ?maxlen:int -> int -> string
val stringo : bytes -> ?maxlen:int -> int -> string * int
val print_bytes : bytes -> unit
val print_code : bytes -> unit
val to_hex_string : string -> string
val u64be : bytes -> int -> int
val u32be : bytes -> int -> int
val u16be : bytes -> int -> int
val i8 : bytes -> int -> int
val _i16_sign : int
val _i16_subtractor : int
val i16 : bytes -> int -> int
val _i32_sign : int
val _i32_subtractor : int
val i32 : bytes -> int -> int
val uL : bytes -> int -> int -> int64
val u64L : bytes -> int -> int64
val u32L : bytes -> int -> int64
val u16L : bytes -> int -> int64
val u8L : bytes -> int -> int64
