(* cpu types
==========
 #define	CPU_ARCH_MASK	0xff000000		/* mask for architecture bits */
 #define CPU_ARCH_ABI64	0x01000000		/* 64 bit ABI */
==========
 *)

let kCPU_ARCH_MASK  = 0xff000000
let kCPU_ARCH_ABI64 = 0x01000000

(*
 #define	CPU_TYPE_MC680x0	((cpu_type_t) 6)
 #define CPU_TYPE_X86		((cpu_type_t) 7)
 #define CPU_TYPE_I386		CPU_TYPE_X86		/* compatibility */
 #define	CPU_TYPE_X86_64		(CPU_TYPE_X86 | CPU_ARCH_ABI64)

/* skip CPU_TYPE_MIPS		((cpu_type_t) 8)	*/
/* skip 			((cpu_type_t) 9)	*/
 #define CPU_TYPE_MC98000	((cpu_type_t) 10)
 #define CPU_TYPE_HPPA           ((cpu_type_t) 11)
 #define CPU_TYPE_ARM		((cpu_type_t) 12)
 #define CPU_TYPE_ARM64          (CPU_TYPE_ARM | CPU_ARCH_ABI64)
 *)

let kCPU_TYPE_X86 = 7
let kCPU_TYPE_ARM = 12
let kCPU_TYPE_X86_64 = kCPU_TYPE_X86 lor kCPU_ARCH_ABI64
let kCPU_TYPE_ARM64 = kCPU_TYPE_ARM lor kCPU_ARCH_ABI64

let cpu_type_to_string cputype = 
  if (cputype = kCPU_TYPE_ARM64) then  "ARM64"
  else if (cputype = kCPU_TYPE_X86_64) then "x86-64"
  else if (cputype = kCPU_TYPE_ARM) then  "ARM"
  else if (cputype = kCPU_TYPE_X86) then "x86"
  else "UNIMPLEMENTED CPUTYPE"
