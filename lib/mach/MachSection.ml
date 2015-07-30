(*
 * A segment is made up of zero or more sections.  Non-MH_OBJECT files have
 * all of their segments with the proper sections in each, and padded to the
 * specified segment alignment when produced by the link editor.  The first
 * segment of a MH_EXECUTE and MH_FVMLIB format file contains the mach_header
 * and load commands of the object file before its first section.  The zero
 * fill sections are always last in their segment (in all formats).  This
 * allows the zeroed segment padding to be mapped into memory where zero fill
 * sections might be. The gigabyte zero fill sections, those with the section
 * type S_GB_ZEROFILL, can only be in a segment with sections of this type.
 * These segments are then placed after all other segments.
 *
 * The MH_OBJECT format has all of its sections in one segment for
 * compactness.  There is no padding to a specified segment boundary and the
 * mach_header and load commands are not part of the segment.
 *
 * Sections with the same section name, sectname, going into the same segment,
 * segname, are combined by the link editor.  The resulting section is aligned
 * to the maximum alignment of the combined sections and is the new section's
 * alignment.  The combined sections are aligned to their original alignment in
 * the combined section.  Any padded bytes to get the specified alignment are
 * zeroed.
 *
 * The format of the relocation entries referenced by the reloff and nreloc
 * fields of the section structure for mach object files is described in the
 * header file <reloc.h>.
 *)

(*
struct section_64 { (* for 64-bit architectures *)
 char  sectname[16]; (* name of this section *)
 char  segname[16]; (* segment this section goes in *)
 uint64_t addr;  (* memory address of this section *)
 uint64_t size;  (* size in bytes of this section *)
 uint32_t offset;  (* file offset of this section *)
 uint32_t align;  (* section alignment (power of 2) *)
 uint32_t reloff;  (* file offset of relocation entries *)
 uint32_t nreloc;  (* number of relocation entries *)
 uint32_t flags;  (* flags (section type and attributes)*)
 uint32_t reserved1; (* reserved (for offset or index) *)
 uint32_t reserved2; (* reserved (for count or sizeof) *)
 uint32_t reserved3; (* reserved *)
};

*)

type section = {
    sectname: string;		(* 16 bytes *)
    segname: string;		(* 16 bytes *)
    (* 4 bytes each *)
    addr: int;
    size: int;
    offset: int;
    align: int;
    reloff: int;
    nreloc: int;
    flags: int;
    reserved1: int;
    reserved2: int;
  }

let sizeof_section = 68	(* bytes *)

(* verified matches c struct naming *)
type section_64 = {
    sectname: string;		(* 16 bytes *)
    segname: string;		(* 16 bytes *)
    addr: int;			(* 8 bytes *)
    size: int;			(* 8 bytes *)
    (* 4 bytes each *)
    offset: int;
    align: int;
    reloff: int;
    nreloc: int;
    flags: int;
    reserved1: int;
    reserved2: int;
    reserved3: int;
  }

let sizeof_section_64 = 80	(* bytes *)

let sections64_to_string sections =
  let b = Buffer.create ((Array.length sections) * 32) in
  let indent = "\t\t" in
  Array.iteri (fun i section ->
	      Printf.sprintf "\n%s(%2d) %s addr: 0x%x size: 0x%x\n%soffset: 0x%x align: %d reloff: 0x%x nreloc: 0x%x\n%sflags: 0x%x r1: 0x%x r2: 0x%x r3: 0x%x\n"
			     indent i section.sectname section.addr section.size
			     indent section.offset section.align section.reloff section.nreloc
			     indent section.flags section.reserved1 section.reserved2 section.reserved3
	     |> Buffer.add_string b
	     ) sections;
  Buffer.contents b
