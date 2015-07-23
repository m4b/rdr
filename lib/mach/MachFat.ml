open Printf
open Binary
open MachCpuTypes

(* big endian *)
type fat_header = {
  magic: int; (* 0xCAFEBABE *)
  nfat_arch: int;
}

let kFAT_MAGIC = 0xcafebabe
let sizeof_fat_header = 8 (* bytes *)

(*
 struct fat_arch { cpu_type_t cputype; cpu_subtype_t cpusubtype; uint32_t offset; uint32_t size; uint32_t align; }; 
 *)

(* big endian *)
type fat_arch = {
  cputype: int;
  cpusubtype: int;
  offset: int;
  size: int;
  align: int;
}

let sizeof_fat_arch = 20 (* bytes *)

let header_to_string header =
  sprintf "0x%x nfat_arch: %d\n" header.magic header.nfat_arch

let print_header header = 
  printf "%s" (header_to_string header)

let arch_to_string arch =
  sprintf "cputype: %d cpusubtype: %d offset: %d size: %d align: %d\n" arch.cputype arch.cpusubtype arch.offset arch.size arch.align

let print_arch arch = 
  printf "%s" (arch_to_string arch)

let get_fat_header binary = 
  let magic = u32be binary 0 in
  let nfat_arch = u32be binary 4 in
  {magic; nfat_arch;}

let get_fat_arch binary pos =
  let cputype = u32be binary pos in
  let cpusubtype = u32be binary (pos + 4) in
  let offset = u32be binary (pos + 8) in
  let size = u32be binary (pos + 12) in
  let align = u32be binary (pos + 16) in
  {cputype; cpusubtype; offset; size; align}

let is_x86_64 fat_arch = 
  fat_arch.cputype = kCPU_TYPE_X86_64

let is_arm64 fat_arch = 
  fat_arch.cputype = kCPU_TYPE_ARM64

let is_64 fat_arch = is_x86_64 fat_arch || is_arm64 fat_arch

let get_fatties binary count pos = 
  let rec loop count pos acc =
    if (count <= 0) then
      acc
    else
      let fatty = get_fat_arch binary pos in
      loop (count - 1) (pos + sizeof_fat_arch) (fatty::acc)
  in loop count pos []    

let get_x86_64_binary_offset ?verbose:(verbose=false) binary nfat = 
  let fatties = get_fatties binary nfat 0 in
  if (verbose) then List.iteri (fun i elem -> Printf.printf "\t(%d): " i; print_arch elem) fatties;
  try 
    let fatty = List.find (fun fh -> is_64 fh) fatties in
    Some (fatty.offset, fatty.size)
  with Not_found -> None
