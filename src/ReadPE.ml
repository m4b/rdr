open Config
open PE.Import
open PE.Export
open PE.Header
open Goblin.Import
open Goblin.Export

let analyze (config:Config.t) binary =
  let pe = PE.get ~coverage:config.print_coverage binary in
  (* print switches *)
  if (not config.silent) then
    begin
      if (not config.search) then
        PE.print_header_stub pe;
      if (config.verbose || config.print_headers) then
	begin
          (* TODO add section and optional header modules *)
          PE.Header.print pe.PE.header;
          let ppf = Format.std_formatter in
          Format.fprintf ppf "@ @[<v 2>Export Data@ ";
          Rdr.Utils.Printer.pp_option
            ppf PE.Export.pp_export_data pe.PE.export_data;
          Rdr.Utils.Printer.pp_option
            ppf PE.Import.pp_import_data pe.PE.import_data;
          Format.print_newline();
          PE.SectionTable.print pe.PE.sections;
        end;
      if (config.verbose || config.print_nlist) then
        (* NO-OP no debug symbols implemented yet *)
        ();
      if (config.verbose || config.print_libraries) then
        PE.Import.print_libraries pe.PE.libraries;
      if (config.verbose || config.print_exports) then
	begin
          PE.Export.print pe.PE.exports;
	end;
      if (config.verbose || config.print_imports) then
        begin
          PE.Import.print pe.PE.imports;
        end;
      if (config.verbose || config.print_coverage) then
        ByteCoverage.print pe.PE.byte_coverage
    end;
  Goblin.PE.from config.name pe

