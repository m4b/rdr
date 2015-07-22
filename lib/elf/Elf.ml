module Header = Header
module ElfReloc = ElfReloc
module ElfConstants = ElfConstants
module Dynamic = Dynamic                        
module ProgramHeader = ProgramHeader
module SectionHeader = SectionHeader
module SymbolTable = SymbolTable

(* todo, change elf_header64 to .t, for love *)
type t = {
  header: Header.t;
  programHeader: ProgramHeader.program_header64;
  sectionHeader: SectionHeader.section_header;
  dynamic: Dynamic.t;
  symbolTable: SymbolTable.t;
  relocs: ElfReloc.t
}
                         
