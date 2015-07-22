    (*
     * Dyld binds an image during the loading process, if the image
     * requires any pointers to be initialized to symbols in other images.  
     * The bind information is a stream of byte sized 
     * opcodes whose symbolic names start with BIND_OPCODE_.
     * Conceptually the bind information is a table of tuples:
     *    <seg-index, seg-offset, type, symbol-library-ordinal, symbol-name, addend>
     * The opcodes are a compressed way to encode the table by only
     * encoding when a column changes.  In addition simple patterns
     * like for runs of pointers initialzed to the same value can be 
     * encoded in a few bytes.
     *)

(*
 * The following are used to encode rebasing information
 *)
let kREBASE_TYPE_POINTER				= 1
let kREBASE_TYPE_TEXT_ABSOLUTE32			= 2
let kREBASE_TYPE_TEXT_PCREL32				= 3

let kREBASE_OPCODE_MASK					= 0xF0
let kREBASE_IMMEDIATE_MASK				= 0x0F
let kREBASE_OPCODE_DONE					= 0x00
let kREBASE_OPCODE_SET_TYPE_IMM				= 0x10
let kREBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB		= 0x20
let kREBASE_OPCODE_ADD_ADDR_ULEB			= 0x30
let kREBASE_OPCODE_ADD_ADDR_IMM_SCALED			= 0x40
let kREBASE_OPCODE_DO_REBASE_IMM_TIMES			= 0x50
let kREBASE_OPCODE_DO_REBASE_ULEB_TIMES			= 0x60
let kREBASE_OPCODE_DO_REBASE_ADD_ADDR_ULEB		= 0x70
let kREBASE_OPCODE_DO_REBASE_ULEB_TIMES_SKIPPING_ULEB	= 0x80
