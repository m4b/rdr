(* TODO: 
   (0) add load segment boundaries, and nlists locals as a parameters to the compute size
   (1) compute final sizes after imports, locals, 
       and exports are glommed into a goblin symbol soup, using all the information available
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
      (*       
      if (config.verbose || config.print_exports) then Mach.Exports.print mach.Mach.exports;
      if (config.verbose || config.print_imports) then Mach.Imports.print mach.Mach.imports;
      *)
      if (config.print_nlist) then Mach.Nlist.print mach.Mach.nlist;
    end;
  goblin

(* this will lookup the binary in the goblin binary *)
let find_export symbol (binary:Goblin.t) = 
  let len = binary.Goblin.nexports in
  let rec loop i =
    if (i >= len) then raise Not_found
    else if (binary.Goblin.exports.(i).name = symbol) then
      binary.Goblin.exports.(i)
    else
      loop (i + 1)
  in loop 0    

(* 
let find_import_symbol symbol binary =
  Mach.Imports.find symbol binary.imports
 *)

