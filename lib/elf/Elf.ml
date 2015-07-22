module Header = Header
module ProgramHeader = ProgramHeader
module SectionHeader = SectionHeader
module Reloc = Reloc
module Constants = Constants
module Dynamic = Dynamic                        
module SymbolTable = SymbolTable

(* todo, change elf_header64 to .t, for love *)
type t = {
  header: Header.t;
  programHeader: ProgramHeader.program_header64;
  sectionHeader: SectionHeader.section_header;
  dynamic: Dynamic.t;
  symbolTable: SymbolTable.t;
  relocations: Reloc.t
}
                         
