(*
  #TODO
  * add load segment boundaries, and nlists locals as a parameters to the compute size
  * compute final sizes after imports, locals, and exports are glommed into a goblin symbol soup, using all the information available
*)

open Printf

open Mach.LoadCommand
open Goblin.Export
open Goblin.Import
open Config

let analyze config binary =
  let mach = Mach.get binary in
  let goblin = Goblin.Mach.to_goblin mach config.install_name in
  if (not config.silent) then
    begin
      if (not config.search) then Mach.Header.print_header mach.Mach.header;     
      if (config.verbose || config.print_headers) then Mach.LoadCommand.print_load_commands mach.Mach.load_commands;
      if (config.verbose || config.print_libraries) then Mach.LoadCommand.print_libraries mach.Mach.libraries;
      if (config.verbose || config.print_exports) then Goblin.print_exports goblin.Goblin.exports;
      if (config.verbose || config.print_imports) then Goblin.print_imports goblin.Goblin.imports;
      if (config.print_nlist) then Mach.SymbolTable.print mach.Mach.nlist;
      if (config.print_coverage) then 
        ByteCoverage.print mach.Mach.byte_coverage
    end;
  goblin

