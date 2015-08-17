(* Legal values for e_type (object file type).  *)

let kET_NONE =		0		(* No file type *)
let kET_REL =		1		(* Relocatable file *)
let kET_EXEC =		2		(* Executable file *)
let kET_DYN =		3		(* Shared object file *)
let kET_CORE =		4		(* Core file *)
let kET_NUM =		5		(* Number of defined types *)
let kET_LOOS =		0xfe00		(* OS-specific range start *)
let kET_HIOS =		0xfeff		(* OS-specific range end *)
let kET_LOPROC =	0xff00		(* Processor-specific range start *)
let kET_HIPROC =	0xffff		(* Processor-specific range end *)

(* Legal values for e_machine (architecture).  *)

let kEM_NONE =		 0		(* No machine *)
let kEM_M32 =		 1		(* AT&T WE 32100 *)
let kEM_SPARC =	 2		(* SUN SPARC *)
let kEM_386 =		 3		(* Intel 80386 *)
let kEM_68K =		 4		(* Motorola m68k family *)
let kEM_88K =		 5		(* Motorola m88k family *)
let kEM_860 =		 7		(* Intel 80860 *)
let kEM_MIPS =		 8		(* MIPS R3000 big-endian *)
let kEM_S370 =		 9		(* IBM System/370 *)
let kEM_MIPS_RS3_LE =	10		(* MIPS R3000 little-endian *)

let kEM_PARISC =	15		(* HPPA *)
let kEM_VPP500 =	17		(* Fujitsu VPP500 *)
let kEM_SPARC32PLUS =	18		(* Sun's "v8plus" *)
let kEM_960 =		19		(* Intel 80960 *)
let kEM_PPC =		20		(* PowerPC *)
let kEM_PPC64 =	21		(* PowerPC 64-bit *)
let kEM_S390 =		22		(* IBM S390 *)

let kEM_V800 =		36		(* NEC V800 series *)
let kEM_FR20 =		37		(* Fujitsu FR20 *)
let kEM_RH32 =		38		(* TRW RH-32 *)
let kEM_RCE =		39		(* Motorola RCE *)
let kEM_ARM =		40		(* ARM *)
let kEM_FAKE_ALPHA =	41		(* Digital Alpha *)
let kEM_SH =		42		(* Hitachi SH *)
let kEM_SPARCV9 =	43		(* SPARC v9 64-bit *)
let kEM_TRICORE	=       44		(* Siemens Tricore *)
let kEM_ARC =		45		(* Argonaut RISC Core *)
let kEM_H8_300 =	46		(* Hitachi H8/300 *)
let kEM_H8_300H	=       47		(* Hitachi H8/300H *)
let kEM_H8S =		48		(* Hitachi H8S *)
let kEM_H8_500 =	49		(* Hitachi H8/500 *)
let kEM_IA_64 =	50		(* Intel Merced *)
let kEM_MIPS_X =	51		(* Stanford MIPS-X *)
let kEM_COLDFIRE =	52		(* Motorola Coldfire *)
let kEM_68HC12 =	53		(* Motorola M68HC12 *)
let kEM_MMA =		54		(* Fujitsu MMA Multimedia Accelerator*)
let kEM_PCP =		55		(* Siemens PCP *)
let kEM_NCPU =		56		(* Sony nCPU embeeded RISC *)
let kEM_NDR1 =		57		(* Denso NDR1 microprocessor *)
let kEM_STARCORE =	58		(* Motorola Start*Core processor *)
let kEM_ME16 =		59		(* Toyota ME16 processor *)
let kEM_ST100 =	60		(* STMicroelectronic ST100 processor *)
let kEM_TINYJ =	61		(* Advanced Logic Corp. Tinyj emb.fam*)
let kEM_X86_64 =	62		(* AMD x86-64 architecture *)
let kEM_PDSP =		63		(* Sony DSP Processor *)

let kEM_FX66 =		66		(* Siemens FX66 microcontroller *)
let kEM_ST9PLUS =	67		(* STMicroelectronics ST9+ 8/16 mc *)
let kEM_ST7 =		68		(* STmicroelectronics ST7 8 bit mc *)
let kEM_68HC16 =	69		(* Motorola MC68HC16 microcontroller *)
let kEM_68HC11 =	70		(* Motorola MC68HC11 microcontroller *)
let kEM_68HC08 =	71		(* Motorola MC68HC08 microcontroller *)
let kEM_68HC05 =	72		(* Motorola MC68HC05 microcontroller *)
let kEM_SVX =		73		(* Silicon Graphics SVx *)
let kEM_ST19 =		74		(* STMicroelectronics ST19 8 bit mc *)
let kEM_VAX =		75		(* Digital VAX *)
let kEM_CRIS =		76		(* Axis Communications 32-bit embedded processor *)
let kEM_JAVELIN =	77		(* Infineon Technologies 32-bit embedded processor *)
let kEM_FIREPATH =	78		(* Element 14 64-bit DSP Processor *)
let kEM_ZSP =		79		(* LSI Logic 16-bit DSP Processor *)
let kEM_MMIX =		80		(* Donald Knuth's educational 64-bit processor *)
let kEM_HUANY =	81		(* Harvard University machine-independent object files *)
let kEM_PRISM =	82		(* SiTera Prism *)
let kEM_AVR =		83		(* Atmel AVR 8-bit microcontroller *)
let kEM_FR30 =		84		(* Fujitsu FR30 *)
let kEM_D10V =		85		(* Mitsubishi D10V *)
let kEM_D30V =		86		(* Mitsubishi D30V *)
let kEM_V850 =		87		(* NEC v850 *)
let kEM_M32R =		88		(* Mitsubishi M32R *)
let kEM_MN10300 =	89		(* Matsushita MN10300 *)
let kEM_MN10200 =	90		(* Matsushita MN10200 *)
let kEM_PJ =		91		(* picoJava *)
let kEM_OPENRISC =	92		(* OpenRISC 32-bit embedded processor *)
let kEM_ARC_A5 =	93		(* ARC Cores Tangent-A5 *)
let kEM_XTENSA =	94		(* Tensilica Xtensa Architecture *)
let kEM_ALTERA_NIOS2 = 113		(* Altera Nios II *)
let kEM_AARCH64 =	183		(* ARM AARCH64 *)
let kEM_TILEPRO =	188		(* Tilera TILEPro *)
let kEM_MICROBLAZE =	189		(* Xilinx MicroBlaze *)
let kEM_TILEGX =	191		(* Tilera TILE-Gx *)
let kEM_NUM =		192

(* If it is necessary to assign new unofficial EM_* values, please
   pick large random numbers (0x8523, 0xa7f2, etc.) to minimize the
   chances of collision with official or non-GNU unofficial values.  *)

let kEM_ALPHA =	0x9026

let etype_to_string etype =
  match etype with
  | 0 -> "NONE"
  | 1 -> "REL"		
  | 2 -> "EXEC" 		
  | 3 -> "DYN"		
  | 4 -> "CORE"		
  | 5 -> "NUM"		
  | 0xfe00 -> "LOOS"		
  | 0xfeff -> "HIOS"		
  | 0xff00 -> "LOPROC"	
  | 0xffff -> "HIPROC"
  | _ -> "UKNOWN TYPE"
	   
let machine_to_string machine =
  match machine with
  | 0 -> "NONE"		 
  | 1 -> "M32"		 
  | 2 -> "SPARC"	 
  | 3 -> "386"		 
  | 4 -> "68K"		 
  | 5 -> "88K"		 
  | 7 -> "860"		 
  | 8 -> "MIPS"		 
  | 9 -> "S370"		 
  | 10 -> "MIPS_RS3_LE"
  | 15 -> "PARISC"	
  | 17 -> "VPP500"	
  | 18 -> "SPARC32PLUS"	
  | 19 -> "960"		
  | 20 -> "PPC"		
  | 21 -> "PPC64"	
  | 22 -> "S390"		
  | 36 -> "V800"		
  | 37 -> "FR20"		
  | 38 -> "RH32"		
  | 39 -> "RCE"		
  | 40 -> "ARM"		
  | 41 -> "FAKE_ALPHA"
  | 42 -> "SH"		
  | 43 -> "SPARCV9"	
  | 44 -> "TRICORE"       
  | 45 -> "ARC"		
  | 46 -> "H8_300"
  | 47 -> "H8_300H"
  | 48 -> "H8S"		
  | 49 -> "H8_500"
  | 50 -> "IA_64"
  | 51 -> "MIPS_X"
  | 52 -> "COLDFIRE"	
  | 53 -> "68HC12"	
  | 54 -> "MMA"		
  | 55 -> "PCP"		
  | 56 -> "NCPU"		
  | 57 -> "NDR1"		
  | 58 -> "STARCORE"	
  | 59 -> "ME16"		
  | 60 -> "ST100"	
  | 61 -> "TINYJ"	
  | 62 -> "X86_64" 	
  | 63 -> "PDSP"		
  | 66 -> "FX66"		
  | 67 -> "ST9PLUS"	
  | 68 -> "ST7"		
  | 69 -> "68HC16"	
  | 70 -> "68HC11"	
  | 71 -> "68HC08"	
  | 72 -> "68HC05"	
  | 73 -> "SVX"		
  | 74 -> "ST19"		
  | 75 -> "VAX"		
  | 76 -> "CRIS"		
  | 77 -> "JAVELIN"	
  | 78 -> "FIREPATH"	
  | 79 -> "ZSP"		
  | 80 -> "MMIX"		
  | 81 -> "HUANY"	
  | 82 -> "PRISM"	
  | 83 -> "AVR"		
  | 84 -> "FR30"		
  | 85 -> "D10V"		
  | 86 -> "D30V"		
  | 87 -> "V850"		
  | 88 -> "M32R"		
  | 89 -> "MN10300"	
  | 90 -> "MN10200"	
  | 91 -> "PJ"		
  | 92 -> "OPENRISC"	
  | 93 -> "ARC_A5"
  | 94 -> "XTENSA"	
  | 113 -> "ALTERA_NIOS2"
  | 183 -> "AARCH64"	
  | 188 -> "TILEPRO"	
  | 189 -> "MICROBLAZE"	
  | 191 -> "TILEGX"	
  | 192 -> "NUM"		
  | 0x9026 -> "ALPHA"
  | _ -> "UNKNOWN MACHINE"

	   
