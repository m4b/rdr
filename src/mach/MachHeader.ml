(* Constants for the flags field of the mach_header *)
let kMH_NOUNDEFS = 0x1  (* the object file has no undefined
                           references *)
let kMH_INCRLINK = 0x2  (* the object file is the output of an
                           incremental link against a base file
                           and can't be link edited again *)
let kMH_DYLDLINK = 0x4  (* the object file is input for the
                           dynamic linker and can't be staticly
                           link edited again *)
let kMH_BINDATLOAD = 0x8  (* the object file's undefined
                             references are bound by the dynamic
                             linker when loaded. *)
let kMH_PREBOUND = 0x10  (* the file has its dynamic undefined
                            references prebound. *)
let kMH_SPLIT_SEGS = 0x20  (* the file has its read-only and
                              read-write segments split *)
let kMH_LAZY_INIT = 0x40  (* the shared library init routine is
                             to be run lazily via catching memory
                             faults to its writeable segments
                             (obsolete) *)
let kMH_TWOLEVEL = 0x80  (* the image is using two-level name
                            space bindings *)
let kMH_FORCE_FLAT = 0x100  (* the executable is forcing all images
                               to use flat name space bindings *)
let kMH_NOMULTIDEFS = 0x200  (* this umbrella guarantees no multiple
                                defintions of symbols in its
                                sub-images so the two-level namespace
                                hints can always be used. *)
let kMH_NOFIXPREBINDING = 0x400 (* do not have dyld notify the
                                   prebinding agent about this
                                   executable *)
let kMH_PREBINDABLE =  0x800           (* the binary is not prebound but can
                                          have its prebinding redone. only used
                                           when MH_PREBOUND is not set. *)
let kMH_ALLMODSBOUND = 0x1000  (* indicates that this binary binds to
                                           all two-level namespace modules of
                                  its dependent libraries. only used
                                  when MH_PREBINDABLE and MH_TWOLEVEL
                                  are both set. *) 
let kMH_SUBSECTIONS_VIA_SYMBOLS = 0x2000(* safe to divide up the sections into
                                           sub-sections via symbols for dead
                                           code stripping *)
let kMH_CANONICAL =    0x4000  (* the binary has been canonicalized
                                  via the unprebind operation *)
let kMH_WEAK_DEFINES = 0x8000  (* the final linked image contains
                                  external weak symbols *)
let kMH_BINDS_TO_WEAK = 0x10000 (* the final linked image uses
                                   weak symbols *)

let kMH_ALLOW_STACK_EXECUTION = 0x20000(* When this bit is set, all stacks 
                                          in the task will be given stack
                                          execution privilege.  Only used in
                                          MH_EXECUTE filetypes. *)
let kMH_ROOT_SAFE = 0x40000           (* When this bit is set, the binary 
                                         declares it is safe for use in
                                         processes with uid zero *)

let kMH_SETUID_SAFE = 0x80000         (* When this bit is set, the binary 
                                         declares it is safe for use in
                                         processes when issetugid() is true *)

let kMH_NO_REEXPORTED_DYLIBS = 0x100000 (* When this bit is set on a dylib, 
                                           the static linker does not need to
                                           examine dependent dylibs to see
                                           if any are re-exported *)
let kMH_PIE = 0x200000   (* When this bit is set, the OS will
                            load the main executable at a
                            random address.  Only used in
                            MH_EXECUTE filetypes. *)
let kMH_DEAD_STRIPPABLE_DYLIB = 0x400000 (* Only for use on dylibs.  When
                                            linking against a dylib that
                                            has this bit set, the static linker
                                            will automatically not create a
                                            LC_LOAD_DYLIB load command to the
                                            dylib if no symbols are being
                                            referenced from the dylib. *)
let kMH_HAS_TLV_DESCRIPTORS = 0x800000 (* Contains a section of type 
                                          S_THREAD_LOCAL_VARIABLES *)

let kMH_NO_HEAP_EXECUTION = 0x1000000 (* When this bit is set, the OS will
                                         run the main executable with
                                         a non-executable heap even on
                                         platforms (e.g. i386) that don't
                                         require it. Only used in MH_EXECUTE
                                         filetypes. *)

let kMH_APP_EXTENSION_SAFE = 0x02000000 (* The code was linked for use in an
                                           application extension. *)


(* =============== *)

let flag_to_string flag =
  match flag with
  | 0x1 -> "MH_NOUNDEFS"
  | 0x2 -> "MH_INCRLINK"
  | 0x4 -> "MH_DYLDLINK"
  | 0x8 -> "MH_BINDATLOAD"
  | 0x10 -> "MH_PREBOUND"
  | 0x20 -> "MH_SPLIT_SEGS"
  | 0x40 -> "MH_LAZY_INIT"
  | 0x80 -> "MH_TWOLEVEL"
  | 0x100 -> "MH_FORCE_FLAT"
  | 0x200 -> "MH_NOMULTIDEFS"
  | 0x400 -> "MH_NOFIXPREBINDING"
  | 0x800 -> "MH_PREBINDABLE "
  | 0x1000 -> "MH_ALLMODSBOUND"
  | 0x2000 -> "MH_SUBSECTIONS_VIA_SYMBOLS"
  | 0x4000 -> "MH_CANONICAL = "
  | 0x8000 -> "MH_WEAK_DEFINES"
  | 0x10000 -> "MH_BINDS_TO_WEAK"
  | 0x20000 -> "MH_ALLOW_STACK_EXECUTION"
  | 0x40000 -> "MH_ROOT_SAFE"
  | 0x80000 -> "MH_SETUID_SAFE"
  | 0x100000 -> "MH_NO_REEXPORTED_DYLIBS"
  | 0x200000 -> "MH_PIE"
  | 0x400000 -> "MH_DEAD_STRIPPABLE_DYLIB"
  | 0x800000 -> "MH_HAS_TLV_DESCRIPTORS"
  | 0x1000000 -> "MH_NO_HEAP_EXECUTION"
  | 0x02000000 -> "MH_APP_EXTENSION_SAFE"
  | _ -> "UNKNOWN FLAG"


(* 

let kMH_NOUNDEFS = 0x1
let kMH_INCRLINK = 0x2
let kMH_DYLDLINK = 0x4
let kMH_BINDATLOAD = 0x8
let kMH_PREBOUND = 0x10
let kMH_SPLIT_SEGS = 0x20
let kMH_LAZY_INIT = 0x40
let kMH_TWOLEVEL = 0x80
let kMH_FORCE_FLAT = 0x100
let kMH_NOMULTIDEFS = 0x200
let kMH_NOFIXPREBINDING = 0x400
let kMH_PREBINDABLE =  0x800
let kMH_ALLMODSBOUND = 0x1000
let kMH_SUBSECTIONS_VIA_SYMBOLS = 0x2000
let kMH_CANONICAL =    0x4000
let kMH_WEAK_DEFINES = 0x8000
let kMH_BINDS_TO_WEAK = 0x10000
let kMH_ALLOW_STACK_EXECUTION = 0x20000
let kMH_ROOT_SAFE = 0x40000                                         
let kMH_SETUID_SAFE = 0x80000
let kMH_NO_REEXPORTED_DYLIBS = 0x100000
let kMH_PIE = 0x200000
let kMH_DEAD_STRIPPABLE_DYLIB = 0x400000
let kMH_HAS_TLV_DESCRIPTORS = 0x800000
let kMH_NO_HEAP_EXECUTION = 0x1000000
let kMH_APP_EXTENSION_SAFE = 0x02000000
 *)



(* 8 fields, although "caps" adds 1 more *)
type mach_header_64 = {
  magic:int;
  cputype:int;
  cpusubtype:int;
  caps:int;
  filetype:int;
  ncmds:int;
  sizeofcmds:int;
  flags:int;
  reserved:int;
}

(* magic constants *)
let kMH_MAGIC = 0xfeedface
let kMH_CIGAM = 0xcefaedfe
let kMH_MAGIC_64 = 0xfeedfacf
let kMH_CIGAM_64 = 0xcffaedfe

(*
 * Constants for the filetype field of the mach_header
 *)
let kMH_OBJECT      = 0x1 (* relocatable object file *)
let kMH_EXECUTE     = 0x2 (* demand paged executable file *)
let kMH_FVMLIB      = 0x3 (* fixed VM shared library file *)
let kMH_CORE        = 0x4 (* core file *)
let kMH_PRELOAD     = 0x5 (* preloaded executable file *)
let kMH_DYLIB       = 0x6 (* dynamically bound shared library *)
let kMH_DYLINKER    = 0x7 (* dynamic link editor *)
let kMH_BUNDLE      = 0x8 (* dynamically bound bundle file *)
let kMH_DYLIB_STUB  = 0x9 (* shared library stub for static *)
                          (*  linking only, no section contents *)
let kMH_DSYM        = 0xa (* companion file with only debug *)
                          (*  sections *)
let kMH_KEXT_BUNDLE = 0xb (* x86_64 kexts *)

let filetype_to_string filetype = 
  match filetype with
  | 0x1 -> "OBJECT"      
  | 0x2 -> "EXECUTE"     
  | 0x3 -> "FVMLIB"      
  | 0x4 -> "CORE"        
  | 0x5 -> "PRELOAD"     
  | 0x6 -> "DYLIB"       
  | 0x7 -> "DYLINKER"    
  | 0x8 -> "BUNDLE"      
  | 0x9 -> "DYLIB_STUB"  
  | 0xa -> "DSYM"        
  | 0xb -> "KEXT_BUNDLE" 
  | _ ->   "UNKNOWN FILETYPE"

let sizeof_mach_header = 32 (*bytes*)

let header_to_long_string header =
  Printf.sprintf "0x%x %d %d 0x%x %d %d %d 0x%x %x\n" header.magic header.cputype header.cpusubtype header.caps header.filetype header.ncmds header.sizeofcmds header.flags header.reserved

let print_long_header header = 
  Printf.printf "0x%x %d %d 0x%x %d %d %d 0x%x %x\n" header.magic header.cputype header.cpusubtype header.caps header.filetype header.ncmds header.sizeofcmds header.flags header.reserved

let print_header header = 
  Printf.printf "MachO %s %s\n" (CpuTypes.cpu_type_to_string header.cputype) (filetype_to_string header.filetype) (*  (flags_to_string header.flags) *)

let get_mach_header binary = 
  let magic = Binary.u32 binary 0 in
  let cputype = Binary.u32 binary 4 in
  let cpusubtype = Binary.i8 binary 8 in
  (*  discard_2_bytes *)
  let caps = Binary.i8 binary 11 in
  let filetype = Binary.u32 binary 12 in
  let ncmds = Binary.u32 binary 16 in
  let sizeofcmds = Binary.u32 binary 20 in
  let flags = Binary.u32 binary 24 in 
  let reserved = Binary.u32 binary 28 in
  let header = {magic = magic; cputype = cputype; cpusubtype = cpusubtype; caps = caps; filetype = filetype; ncmds = ncmds; sizeofcmds = sizeofcmds; flags = flags ; reserved = reserved} in
  header
