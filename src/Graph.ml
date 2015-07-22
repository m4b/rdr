(* 
TODO:
(1) create sorted output, by sending in a polymorphic list instead of a map, sorted by address, etc. this will be easy with arrays of exports/imports
 *)

(*
for testing

#directory "/Users/matthewbarney/projects/binreader/_build/src/utils/";;
#directory "/Users/matthewbarney/projects/binreader/_build/src/mach/";;
#load "Binary.cmo";;
#load "InputUtils.cmo";;
#load "Version.cmo";;
#load "Nlist.cmo";;
#load "LoadCommand.cmo";;
#load "BindOpcodes.cmo";;
#load "Leb128.cmo";;
#load "Imports.cmo";;
#load "MachExports.cmo";;
#load "Macho.cmo";;
*)

open Binary
open MachImports
open MachExports
open ReadMach

(* 
digraph structs { 
node [shape=plaintext] 
struct1 [label=< 
<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0"> 
<TR>
<TD>left</TD><TD PORT="f1">mid dle</TD><TD PORT="f2">right</TD>
</TR> </TABLE>>]; struct2 [label=< <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0"> <TR><TD PORT="f0">one</TD><TD>two</TD></TR> </TABLE>>]; struct3 [label=< <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4"> <TR> <TD ROWSPAN="3">hello<BR/>world</TD> <TD COLSPAN="3">b</TD> <TD ROWSPAN="3">g</TD> <TD ROWSPAN="3">h</TD> </TR> <TR> <TD>c</TD><TD PORT="here">d</TD><TD>e</TD> </TR> <TR> <TD COLSPAN="3">f</TD> </TR> </TABLE>>]; struct1:f1 -> struct2:f0; struct1:f2 -> struct3:here; }
 *)

(* helper functions to avoid syntax errors in dot *)
let to_dot_name = 
  String.map (fun c -> 
      match c with 
      | '.' | '-' | '/' -> '_' 
      | '+' -> 'p'
      | c' -> c')

let from_dot_name = 
  String.map (fun c -> match c with | '_' -> '.' | c' -> c')
(* end helper *)

let graph_with_dot file =
  if (Command.program_in_path "dot") then
    let output = (Filename.chop_suffix file ".gv") ^ ".png" in
    Sys.command
    @@ Printf.sprintf "dot -o %s -n -Tpng %s" output file
    |> ignore
  else
    Printf.eprintf "Error: dot not installed; raw graphviz file %s written to disk\n" file

(* exports *)
let get_html_exports_header name fullname nexports = 
  (* multiline strings whitespace significant *)
  Printf.sprintf "
%s [label=<
  <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">
   <TR >
    <TD BGCOLOR=\"#d23939\" COLSPAN=\"3\">%s Exported Symbols (%d)</TD>
   </TR>
   <TR>
    <TD BGCOLOR=\"#ff6363\">SYMBOL</TD><TD BGCOLOR=\"#ff6363\">SIZE</TD><TD BGCOLOR=\"#ff6363\">ADDRESS</TD>
   </TR>
" name fullname nexports

let html_footer = "  </TABLE>\n>];\n"

let get_html_export_row symbol_name export libraries =
  let size = try GoblinSymbol.find_symbol_size export |> Printf.sprintf "%d" with Not_found -> "" in
  (* i'm being lazy as shit and converting it back *)
  match MachExports.mach_export_data_to_export_info export with
  | Regular info ->
    Printf.sprintf "   <TR>
    <TD PORT=\"%s\">%s</TD><TD>%s</TD><TD>0x%x</TD>
   </TR>
" symbol_name symbol_name size info.address
  | Reexport info -> 
    begin
      match info.lib_symbol_name with
      | Some str ->
        Printf.sprintf "   <TR>
    <TD PORT=\"%s\">%s</TD><TD>%s</TD><TD>%s <BR/>@ %s</TD>
   </TR>
" symbol_name symbol_name size str info.lib
      | None -> 
        Printf.sprintf "   <TR>
    <TD PORT=\"%s\">%s</TD><TD>%s</TD><TD>@ %s</TD>
   </TR>
" symbol_name symbol_name size info.lib
    end
  | Stub info -> 
    Printf.sprintf "   <TR>
    <TD PORT=\"%s\">%s</TD><TD>%s</TD><TD>0x%x , 0x%x</TD>
   </TR>
" symbol_name symbol_name size info.stub_offset info.resolver_offset

(* libs *)

let get_html_libs_header name fullname nlibs = 
  (* multiline strings whitespace significant *)
  Printf.sprintf "
%s [label=<
  <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">
   <TR>
    <TD BGCOLOR=\"lightblue\">%s Libraries (%d)</TD>
   </TR>
" name fullname (nlibs - 1)

let get_html_lib_row name index libname =
  Printf.sprintf "   <TR>
    <TD ALIGN=\"LEFT\" PORT=\"%d\">(%d) %s</TD>
   </TR>
" (index+1) (index+1) libname

(* imports *)
let get_html_imports_header name fullname nimports = 
  Printf.sprintf "
%s [label=<
  <TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">
   <TR >
    <TD BGCOLOR=\"#67e14f\" COLSPAN=\"2\">%s Imported Symbols (%d)</TD>
   </TR>
   <TR>
    <TD BGCOLOR=\"#8efd78\">SYMBOL</TD><TD BGCOLOR=\"#8efd78\">LIBRARY</TD>
   </TR>
" name fullname nimports

let get_html_import_row name import = 
  let is_lazy = MachImports.is_lazy import in
  let name = MachImports.import_name import in
  let lib = MachImports.import_lib import in
  let color = if (is_lazy) then "#e0ffda" else "#ffffff" in
  Printf.sprintf "   <TR>
    <TD BGCOLOR=\"%s\" PORT=\"%s\">%s</TD><TD BGCOLOR=\"%s\">%s</TD>
   </TR>
" color name name color lib
(* end *)

(* todo: add header and footer to a wrapper function? *)
let mach_to_html_dot (binary:mach_binary) draw_imports draw_libs = 
  let b = Buffer.create @@ binary.nexports * 16 in
  let name = to_dot_name binary.name in
  let header = Printf.sprintf "digraph %s {\n" name in
  Buffer.add_string b header;
  (*   Buffer.add_string b "rankdir=LR\n"; *)
  Buffer.add_string b "
rankdir=LR;
0, 1, 2, 3 [style=invis]
0->1->2->3 [style=invis]
node [shape=plaintext]\n";
  (* begin libs *)
  if (draw_libs) then
    begin
      let nodename = Printf.sprintf "%s_libs" name in
      Buffer.add_string b @@ Printf.sprintf "{ rank=same; 0->%s [style=invis]}" nodename;
      Buffer.add_string b @@ get_html_libs_header nodename binary.name binary.nlibs;
      (* was a stupid idea to include the binary in the libraries... *)
      Array.iteri (fun i lib -> if (i <> 0) then Buffer.add_string b @@ get_html_lib_row name (i - 1) lib) binary.libs;
      Buffer.add_string b html_footer;
    end;
  (* end libs *)
  (* begin exports *)
  let nodename = Printf.sprintf "%s_exports" name in
  Buffer.add_string b @@ Printf.sprintf "{ rank=same; 1->%s [style=invis]}" nodename;
  Buffer.add_string b @@ get_html_exports_header nodename binary.name binary.nexports;
  Array.iter (fun mach_export_data ->
      let name = GoblinSymbol.find_symbol_name mach_export_data in
      Buffer.add_string b @@ get_html_export_row name mach_export_data binary.libs
    ) binary.exports;
  Buffer.add_string b html_footer;
  (* end exports *)
  (* begin imports *)
  if (draw_imports) then
    begin
      let nodename = Printf.sprintf "%s_imports" name in
      Buffer.add_string b @@ Printf.sprintf "{ rank=same; 2->%s [style=invis]}" nodename;
      Buffer.add_string b @@ get_html_imports_header nodename binary.name binary.nimports;
      Array.iteri (fun i import -> Buffer.add_string b @@ get_html_import_row name import) binary.imports;
      Buffer.add_string b html_footer;
    end;  
  (* end imports *)
  Buffer.add_string b "}\n";
  Buffer.contents b

let graph_mach_binary ?draw_imports:(draw_imports=true) ?draw_libs:(draw_libs=true) binary filename = 
  let call_graph = mach_to_html_dot binary draw_imports draw_libs in
  (*   print_string call_graph; *)
  let file = filename ^ ".gv" in
  let oc = open_out file in
  Printf.fprintf oc "%s" call_graph;
  close_out oc;
  graph_with_dot file

(* lib dependency graph *)
(* [binary, [libs]] *)

(* todo add solid background color here *)
let lib_header = "digraph lib_deps {
rankdir=BT;
overlap=false;
outputorder=\"edgesfirst\";
node[shape=\"rect\"];
"

let lib_footer = "}\n"

let get_lib_nodes binary_name color libs =
  let b = Buffer.create (Array.length libs * 5) in
  Array.iteri
    (fun i lib ->
     if (i <> 0) then
       let node =
	 Printf.sprintf
	   "%s -> %s[color=\"%s\"];\n"
	   (to_dot_name binary_name)
	   (to_dot_name lib |> Filename.basename) color in
       Buffer.add_string b node
    ) libs;
  Buffer.contents b

let lib_graph (binary_name, libs) color =
  if (Array.length libs = 0) then
    ""
  else
    Printf.sprintf "
%s [label=\"%s\"]
%s
" (to_dot_name binary_name) binary_name (get_lib_nodes binary_name color libs)

let get_color color =
  let r = Printf.sprintf "%x" @@ int_of_float @@ color *. 255. in
  let g = Printf.sprintf "%x" @@ int_of_float @@ Random.float 1.0 *. 255. in
  let b = Printf.sprintf "%x" @@ int_of_float @@ Random.float 1.0 *. 255. in
  "#" ^ r ^ g ^ b

(* input: (binary name, lib dependency array) list *)
let graph_lib_dependencies ?use_dot_storage:(use_dot_storage=false) lib_deps =
  let b = Buffer.create 0 in
  Buffer.add_string b lib_header;
  let color_ratio = 1. /. (float_of_int @@ List.length lib_deps) in
  let color_counter = ref 1 in 
  let rec loop deps =
    match deps with
    | [] ->
      Buffer.add_string b lib_footer;
      let path =
	Storage.get_graph_path
	  ~graph_name:Storage.graph_name
	  ~use_dot_storage:use_dot_storage
      in
      let oc = open_out path in
      Printf.fprintf oc "%s" @@ Buffer.contents b;
      close_out oc 
    | d::deps ->
      lib_graph d (get_color ((float_of_int !color_counter) *. color_ratio)) |> Buffer.add_string b;
      incr color_counter;
      loop deps
  in loop lib_deps

(* ================================ *)
(* abstract binary (goblin) printer *)
(* ================================ *)

(* hack to print elf multi-libs better *)
let _newline_re = Str.regexp "\n"
let _sub = "\n<br></br>"
let add_brs libstring =
  (* first replace hack to ignore the first newline; this whole thing is a hack ;) *)
  Str.replace_first _newline_re "" libstring |> Str.global_replace _newline_re _sub

let get_goblin_html_export_row export =
    Printf.sprintf "   <TR>
    <TD PORT=\"%s\">%s</TD><TD>%d</TD><TD>0x%x</TD>
   </TR>
" export.Goblin.Export.name export.Goblin.Export.name export.Goblin.Export.size export.Goblin.Export.offset

let get_goblin_html_import_row import = 
  let color = if (import.Goblin.Import.is_lazy) then "#e0ffda" else "#ffffff" in
  Printf.sprintf "   <TR>
    <TD BGCOLOR=\"%s\" PORT=\"%s\">%s</TD><TD BGCOLOR=\"%s\">%s</TD>
   </TR>
" color import.Goblin.Import.name import.Goblin.Import.name color (add_brs import.Goblin.Import.lib)

let goblin_to_html_dot (binary:Goblin.t) draw_imports draw_libs = 
  let b = Buffer.create @@ binary.Goblin.nexports * 16 in
  let name = to_dot_name binary.Goblin.name in
  let header = Printf.sprintf "digraph %s {\n" name in
  Buffer.add_string b header;
  (*   Buffer.add_string b "rankdir=LR\n"; *)
  Buffer.add_string b "
rankdir=LR;
0, 1, 2, 3 [style=invis]
0->1->2->3 [style=invis]
node [shape=plaintext]\n";
  (* begin libs *)
  if (draw_libs) then
    begin
      let nodename = Printf.sprintf "%s_libs" name in
      Buffer.add_string b @@ Printf.sprintf "{ rank=same; 0->%s [style=invis]}" nodename;
      Buffer.add_string b @@ get_html_libs_header nodename binary.Goblin.name binary.Goblin.nlibs; (* goblin? *)
      Array.iteri (fun i lib -> if (i <> 0) then Buffer.add_string b @@ get_html_lib_row name (i-1) lib) binary.Goblin.libs;
      Buffer.add_string b html_footer;
    end;
  (* end libs *)
  (* begin exports *)
  let nodename = Printf.sprintf "%s_exports" name in
  Buffer.add_string b @@ Printf.sprintf "{ rank=same; 1->%s [style=invis]}" nodename;
  Buffer.add_string b @@ get_html_exports_header nodename binary.Goblin.name binary.Goblin.nexports;
  Goblin.iter (fun export ->
      Buffer.add_string b @@ get_goblin_html_export_row export
    ) binary.Goblin.exports;
  Buffer.add_string b html_footer;
  (* end exports *)
  (* begin imports *)
  if (draw_imports) then
    begin
      let nodename = Printf.sprintf "%s_imports" name in
      Buffer.add_string b @@ Printf.sprintf "{ rank=same; 2->%s [style=invis]}" nodename;
      Buffer.add_string b @@ get_html_imports_header nodename binary.Goblin.name binary.Goblin.nimports;
      Goblin.iter (fun import -> Buffer.add_string b @@ get_goblin_html_import_row import) binary.Goblin.imports;
      Buffer.add_string b html_footer;
    end;  
  (* end imports *)
  Buffer.add_string b "}\n";
  Buffer.contents b

let graph_library_dependencies ~use_sfdp:use_sfdp ~use_dot_storage:use_dot_storage =
  if (Command.program_in_path "dot") then
    let input =
      Storage.get_graph_path
	~graph_name:Storage.graph_name
	~use_dot_storage:true
    in
    let output_tmp =
	Storage.get_graph_path
	  ~graph_name:Storage.graph_name
	  ~use_dot_storage:use_dot_storage
    in
    let output =
      (Filename.chop_suffix output_tmp ".gv") ^ ".png"
    in
    let graph_program =
      Printf.sprintf
      (if (use_sfdp) then
	"sfdp -Gsize=50 -Goverlap=prism -o %s -Tpng %s"
      else
	"dot -o %s -n -Tpng %s"
      ) output input
    in
    Sys.command graph_program |> ignore
  else
    Printf.eprintf "Error: dot is not installed\n"
		  
let graph_goblin ?draw_imports:(draw_imports=true) ?draw_libs:(draw_libs=true) binary filename = 
  let graph = goblin_to_html_dot binary draw_imports draw_libs in
  let file = filename ^ ".gv" in
  let oc = open_out file in
  Printf.fprintf oc "%s" graph;
  close_out oc;
  graph_with_dot file
