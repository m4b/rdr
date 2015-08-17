module Elf = Elf
module Mach = Mach
module Goblin = Goblin
module Utils = RdrUtils

module Object = struct
  type t = | Mach of bytes | Elf of bytes | PE of bytes | Unknown of string * string
  let get ?verbose:(verbose=false) filename =
    let ic = open_in_bin filename in
    if (in_channel_length ic < 4) then
      (* 4 bytes, less than any magic number we're looking for *)
      begin
        close_in ic;
        Unknown (filename, "less than 4 bytes")
      end
    else
      (* BEGIN Binary cases *)
      let magic = Input.input_i32be ic in
      if (verbose) then
        Printf.printf "opening %s with magic: 0x%x\n" filename magic;
      (* MACH FAT *)
      if (magic = Mach.Fat.kFAT_MAGIC) (* cafe babe *) then
        let nfat_arch = Input.input_i32be ic in
        if (nfat_arch > 4) then (* hack to avoid java class file errors which have same magic num *)
          begin
            close_in ic;
            Unknown (filename, "mach fat too many archs (probably a java class file)")
          end
        else
          let sizeof_arch_bytes = nfat_arch * Mach.Fat.sizeof_fat_arch in
          let fat_arch_bytes = Bytes.create sizeof_arch_bytes in
          really_input ic fat_arch_bytes 0 sizeof_arch_bytes;
          let offset =
	    Mach.Fat.get_x86_64_binary_offset fat_arch_bytes nfat_arch in
          match offset with
          | Some (offset, size) ->
	    seek_in ic offset;
	    let magic = Input.input_i32be ic in
	    if (magic = Mach.Header.kMH_CIGAM_64) then
              begin
                seek_in ic offset;
                let binary = Bytes.create size in
                really_input ic binary 0 size;
                close_in ic;
                Mach binary
              end
	    else
              begin
                close_in ic; Unknown (filename, "mach fat has no 64 bit binaries")
              end
          | None ->
	    close_in ic;
	    Printf.eprintf "<Rdr.Object> ERROR, bad binary: %s\n" filename;
	    Unknown (filename, "mach fat has no binaries")
	    (* backwards cause we read the 32bit int big E style *)
            (* MACH *)
      else if (magic = Mach.Header.kMH_CIGAM_64) then
        begin
	  seek_in ic 0;
	  let binary = Bytes.create (in_channel_length ic) in
	  really_input ic binary 0 (in_channel_length ic);
	  close_in ic;
	  Mach binary
        end
        (* ELF *)
      else if (magic = Elf.Header.kMAGIC_ELF) then
        begin
	  seek_in ic 0;
	  let binary = Bytes.create (in_channel_length ic) in
	  really_input ic binary 0 (in_channel_length ic);
	  close_in ic;
	  if (Elf.Header.check_64bit binary) then
	    Elf binary
	  else
	    Unknown (filename, "elf binary is not 64-bit")
        end
        (* PE *)
        (*
      else if (magic = Elf.Header.kMAGIC_ELF) then
        begin
seek_in ic 0;
let binary = Bytes.create (in_channel_length ic) in
really_input ic binary 0 (in_channel_length ic);
close_in ic;
if (Elf.Header.check_64bit binary) then
Elf binary
else
Unknown filename
        end
       *)
      else
        begin
	  close_in ic;
	  if (verbose) then
	    Printf.printf "ignoring binary: %s\n" filename;
	  Unknown (filename, "unknown magic number")
        end
end
