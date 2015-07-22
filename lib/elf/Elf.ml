module Header = Header
module ProgramHeader = ProgramHeader
module SectionHeader = SectionHeader
module Reloc = Reloc
module Constants = Constants
module Dynamic = Dynamic                        
module SymbolTable = SymbolTable

type t = {
  header: Header.t;
  programHeader: ProgramHeader.t;
  sectionHeader: SectionHeader.t;
  dynamic: Dynamic.t;
  symbolTable: SymbolTable.t;
  relocations: Reloc.t
}
                         
