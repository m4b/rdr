open Config
       
type t = | Mach of bytes | Elf of bytes | Unknown

exception Unimplemented_binary_type of string

let get_bytes ?verbose:(verbose=false) filename = 
  let ic = open_in_bin filename in
  if (in_channel_length ic < 4) then
    (* 4 bytes, less than any magic number we're looking for *)
    Unknown
  else
    let magic = InputUtils.input_i32be ic in
    if (verbose) then
      Printf.printf "opening %s with magic: 0x%x\n" filename magic;
    if (magic = Fat.kFAT_MAGIC) (* cafe babe *) then
      let nfat_arch = InputUtils.input_i32be ic in
      let sizeof_arch_bytes = nfat_arch * Fat.sizeof_fat_arch in
      let fat_arch_bytes = Bytes.create sizeof_arch_bytes in
      really_input ic fat_arch_bytes 0 sizeof_arch_bytes;
      let offset =
	Fat.get_x86_64_binary_offset fat_arch_bytes nfat_arch in
      match offset with
      | Some (offset, size) ->
	 seek_in ic offset;
	 let magic = InputUtils.input_i32be ic in
	 if (magic = MachHeader.kMH_CIGAM_64) then
           begin
             seek_in ic offset;
             let binary = Bytes.create size in
             really_input ic binary 0 size;
             close_in ic;
             Mach binary
           end
	 else
           begin
             close_in ic;
             Unknown
           end
      | None ->
	 close_in ic;
	 Printf.eprintf "ERROR, bad binary: %s\n" filename;
	 Unknown
	   (* backwards cause we read the 32bit int big E style *)
    else if (magic = MachHeader.kMH_CIGAM_64) then
      begin
	seek_in ic 0;  
	let binary = Bytes.create (in_channel_length ic) in
	really_input ic binary 0 (in_channel_length ic);
	close_in ic;
	Mach binary
      end 
    else if (magic = ElfHeader.kMAGIC_ELF) then
      begin
	seek_in ic 0;  
	let binary = Bytes.create (in_channel_length ic) in
	really_input ic binary 0 (in_channel_length ic);
	close_in ic;
	if (ElfHeader.check_64bit binary) then
	  Elf binary
	else
	  Unknown
      end
    else
      begin
	close_in ic;
	if (verbose) then
	  Printf.printf "ignoring binary: %s\n" filename;
	Unknown
      end

(* if I return goblin binaries and destroy all the beautiful structure of each kind of binary, i can have one return type without matches...
so... I should do it, right?
 *)
let analyze config binary =
  match binary with
  | Mach bytes ->
     let binary = ReadMach.analyze config bytes in     
     if (config.search) then
       try
         ReadMach.find_export_symbol
	   config.search_term binary
	 |> MachExports.print_mach_export_data ~simple:true
       (* TODO: add find import symbol *)
       with Not_found ->
         Printf.printf "";
     else 
       if (config.graph) then
         if (config.use_goblin) then
           begin
             let goblin = ReadMach.to_goblin binary in
             Graph.graph_goblin
	       ~draw_imports:true
	       ~draw_libs:true goblin
	     @@ Filename.basename config.filename;
           end
         else
           Graph.graph_mach_binary 
             ~draw_imports:true 
             ~draw_libs:true 
             binary 
             (Filename.basename config.filename);
  (* ===================== *)
  (* ELF *)
  (* ===================== *)
  | Elf binary ->
     (* analyze the binary and print program headers, etc. *)
     let binary = ReadElf.analyze config binary in
     if (config.search) then
       try
         ReadElf.find_export_symbol
	   config.search_term
	   binary |> Goblin.print_export
       with Not_found ->
         Printf.printf "";
     else
       if (config.graph) then
	 Graph.graph_goblin binary
	 @@ Filename.basename config.filename;
     
  | Unknown ->
     raise @@ Unimplemented_binary_type (Printf.sprintf "Unknown binary %s" config.install_name)
