open Binary
open MachBindOpcodes
open MachLoadCommand

(* table of tuples:
   <seg-index, seg-offset, type, symbol-library-ordinal, symbol-name, addend>
   symbol flags are undocumented 
*)

(* TODO: move the special fields (if any) and match them with what's on disk/in c structs? *)
type bind_information = {
  seg_index: int;
  seg_offset: int;
  bind_type: int;
  symbol_library_ordinal: int;
  symbol_name: string;
  symbol_flags: int;
  addend: int;
  special_dylib: int; (* seeing self = 0 assuming this means the symbol is imported from itself, because its... libSystem.B.dylib? *)
}

type import = {
  bi: bind_information;
  dylib: string;
  is_lazy: bool;
  offset: int;
  size: int;
}

type t = import list

let empty = []

let import_to_string import = 
  if (import.is_lazy) then
    Printf.sprintf "%s ~> %s" import.bi.symbol_name import.dylib
  else
    Printf.sprintf "%s -> %s" import.bi.symbol_name import.dylib

let print_import import =
  Printf.printf "%s\n" @@ import_to_string import

let imports_to_string imports = 
  List.fold_left (fun acc import -> 
      (Printf.sprintf "%s" @@ import_to_string import) ^ acc
    ) "" imports

let print (imports:t) =
  List.iter print_import imports

(* TODO: dyld lazy binder sets bind type to initial record as MachBindOpcodes.kBIND_TYPE_POINTER *)
let empty_bind_information  = { seg_index = 0; seg_offset = 0x0; bind_type = 0x0; special_dylib = 1; symbol_library_ordinal = 0; symbol_name = ""; symbol_flags = 0; addend = 0}

let empty_bind_information is_lazy = 
  let bind_type = if (is_lazy) then MachBindOpcodes.kBIND_TYPE_POINTER else 0x0 in
{ seg_index = 0; seg_offset = 0x0; bind_type = bind_type; special_dylib = 1; symbol_library_ordinal = 0; symbol_name = ""; symbol_flags = 0; addend = 0}

let bind_information_to_string bi =
  Printf.sprintf "%s seg_index: %x seg_offset: 0x%x lib_ordinal: %d type: %d flags: %d special_dylib: %d"
    bi.symbol_name
    bi.seg_index
    bi.seg_offset
    bi.symbol_library_ordinal
    bi.bind_type
    bi.symbol_flags
    bi.special_dylib

let print_bind_information bis =
  List.iteri (fun i bi -> Printf.printf "%s\n" (bind_information_to_string bi)) bis

let sort =
  List.sort (fun i1 i2 ->
      if (i1.is_lazy) then
        if (i2.is_lazy) then
          compare i1.offset i2.offset
        else
          -1
      else
      if (i2.is_lazy) then
        1
      else
        compare i1.offset i2.offset
    )


(* interpreter for BIND opcodes:
    runs on prebound (non lazy) symbols (usually dylib extern consts and extern variables),
    and lazy symbols (usually dylib functions)
*)

let debug = false
let interp = false

let sizeof_ptr64 = 8

let bind_interpreter bytes pos size is_lazy = 
  let bind_info = empty_bind_information is_lazy in
  let rec loop pos acc bind_info =
    if (pos >= size) then
      begin
	if (debug) then Printf.printf "End of opcode stream\n";
	acc
      end
    else
      let opcode = i8 bytes pos in
      if (interp) then
        begin
          let i = read_line() in
          ignore i;
        end;
      if (debug) then 
        begin
          Printf.printf "%s -> 0x%x\n0x%x with %s\n" (MachBindOpcodes.opcode_to_string @@ MachBindOpcodes.get_opcode @@ opcode land kBIND_OPCODE_MASK) opcode pos (bind_information_to_string bind_info);
        end;
      match (get_opcode @@ opcode land kBIND_OPCODE_MASK) with
      
      (* we do nothing, don't update our records, and add a new, fresh record *)
      | BIND_OPCODE_DONE ->
        loop (pos + 1) acc (empty_bind_information is_lazy)

      | BIND_OPCODE_SET_DYLIB_ORDINAL_IMM ->
	let symbol_library_ordinal = opcode land kBIND_IMMEDIATE_MASK in
	loop (pos + 1) acc {bind_info with symbol_library_ordinal}

      | BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB ->
	let symbol_library_ordinal,pos' = Leb128.get_uleb128 bytes (pos + 1) in
	loop pos' acc {bind_info with symbol_library_ordinal}

      | BIND_OPCODE_SET_DYLIB_SPECIAL_IMM  ->
        (* dyld puts the immediate into the symbol_library_ordinal field... *)        
        let special_dylib = opcode land kBIND_IMMEDIATE_MASK in
        (* Printf.printf "special_dylib: 0x%x\n" special_dylib; *)
        loop (pos + 1) acc {bind_info with special_dylib}
   
      | BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM ->
	let symbol_flags = opcode land kBIND_IMMEDIATE_MASK in
	let symbol_name, pos' = stringo bytes (pos + 1) in
	loop pos' acc {bind_info with symbol_name; symbol_flags}

      | BIND_OPCODE_SET_TYPE_IMM ->
	let bind_type = opcode land kBIND_IMMEDIATE_MASK in
	loop (pos + 1) acc {bind_info with bind_type}

      | BIND_OPCODE_SET_ADDEND_SLEB ->
        let addend, pos' = Leb128.get_sleb128 bytes (pos + 1) in
        loop pos' acc {bind_info with addend}

      (* WARNING *)
      | BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB ->
	let seg_index = opcode land kBIND_IMMEDIATE_MASK in
        (* dyld sets the address to the segActualLoadAddress(segIndex) + uleb128:
           address = segActualLoadAddress(segmentIndex) + read_uleb128(p, end); *)
	let seg_offset,pos' = Leb128.get_uleb128 bytes (pos + 1) in
	loop pos' acc {bind_info with seg_index; seg_offset}

      | BIND_OPCODE_ADD_ADDR_ULEB ->
	let addr,pos' = Leb128.get_uleb128 bytes (pos + 1) in
	let seg_offset = bind_info.seg_offset + addr in
	loop pos' acc {bind_info with seg_offset}

      (* record the record by placing its value into our list*)
      | BIND_OPCODE_DO_BIND ->
        (* hmmmm, from dyld:
	   if ( address >= segmentEndAddress ) 
	   throwBadBindingAddress(address, segmentEndAddress, segmentIndex, start, end, p);
	   (this->*handler)(context, address, type, symbolName, symboFlags, addend, libraryOrdinal, "", &last);
	   address += sizeof(intptr_t); <------- THIS GUY
        *)
        let seg_offset = bind_info.seg_offset + sizeof_ptr64 (* sizeof_intptr_t *) in
	loop (pos + 1) (bind_info::acc) {bind_info with seg_offset}

        (* not verified value interpretation *)
      | BIND_OPCODE_DO_BIND_ADD_ADDR_ULEB ->
        (* dyld:
	   if ( address >= segmentEndAddress ) 
	   throwBadBindingAddress(address, segmentEndAddress, segmentIndex, start, end, p);
	   (this->*handler)(context, address, type, symbolName, symboFlags, addend, libraryOrdinal, "", &last);
	   address += read_uleb128(p, end) + sizeof(intptr_t);
        *)
        (* so we bind the old record, then increment bind info address for the next guy, plus the ptr offset? *)
        let addr,pos' = Leb128.get_uleb128 bytes (pos + 1) in
        let seg_offset = bind_info.seg_offset + addr + sizeof_ptr64 in
        loop pos' (bind_info::acc) {bind_info with seg_offset}
        
      | BIND_OPCODE_DO_BIND_ADD_ADDR_IMM_SCALED -> 
        (* dyld:				
           if ( address >= segmentEndAddress ) 
	   throwBadBindingAddress(address, segmentEndAddress, segmentIndex, start, end, p);
	   (this->*handler)(context, address, type, symbolName, symboFlags, addend, libraryOrdinal, "", &last);
	   address += immediate*sizeof(intptr_t) + sizeof(intptr_t);
	   break;
        *)
        (* similarly, we bind the old record, then perform address manipulation for the next record *)
	let scale = opcode land kBIND_IMMEDIATE_MASK in
        let seg_offset = bind_info.seg_offset + (scale * sizeof_ptr64) + sizeof_ptr64 in
        loop (pos + 1) (bind_info::acc) {bind_info with seg_offset}

      | BIND_OPCODE_DO_BIND_ULEB_TIMES_SKIPPING_ULEB ->
        (* dyld:
           count = read_uleb128(p, end);
	   skip = read_uleb128(p, end);
	   for (uint32_t i=0; i < count; ++i) {
	   if ( address >= segmentEndAddress ) 
	   throwBadBindingAddress(address, segmentEndAddress, segmentIndex, start, end, p);
	   (this->*handler)(context, address, type, symbolName, symboFlags, addend, libraryOrdinal, "", &last);
	   address += skip + sizeof(intptr_t);
	   }
	   break;
        *)
        let count,pos' = Leb128.get_uleb128 bytes (pos + 1) in
        let skip,pos'' = Leb128.get_uleb128 bytes pos' in
        let addr = ref bind_info.seg_offset in
        for i = 0 to count - 1 do
          addr := !addr + skip + sizeof_ptr64;
        done;
        let seg_offset = !addr in
        loop pos'' (bind_info::acc) {bind_info with seg_offset}
  in loop pos [] bind_info

let bind_information_to_import libraries segments ~is_lazy:is_lazy bi =
  let offset = (List.nth segments bi.seg_index).MachLoadCommand.Types.fileoff + bi.seg_offset in
  let size = if (bi.bind_type == MachBindOpcodes.kBIND_TYPE_POINTER) then 8 else 0 in
  let dylib = libraries.(bi.symbol_library_ordinal) in
  {bi; dylib; is_lazy; offset; size}

let get_imports binary dyld_info libs segments =
  let bind_off = dyld_info.MachLoadCommand.Types.bind_off in
  let bind_size = dyld_info.MachLoadCommand.Types.bind_size in
  let lazy_bind_off = dyld_info.MachLoadCommand.Types.lazy_bind_off in
  let lazy_bind_size = dyld_info.MachLoadCommand.Types.lazy_bind_size in
  let non_lazy_bytes = Bytes.sub binary bind_off bind_size in
  let lazy_bytes = Bytes.sub binary lazy_bind_off lazy_bind_size in
  let non_lazy_bi = bind_interpreter non_lazy_bytes 0 bind_size false in
  let lazy_bi = bind_interpreter lazy_bytes 0 lazy_bind_size true in
  let nli = List.map (bind_information_to_import libs segments ~is_lazy:false) non_lazy_bi in
  let li = List.map (bind_information_to_import libs segments ~is_lazy:true) lazy_bi in
  nli@li |> sort

(* non-lazy: extern [const] <var_type> <var_name> or specially requested prebound symbols ? *)
