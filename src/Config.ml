type t =
  {
    (* internal *)
    analyze: bool;
    search: bool;    
    silent: bool;
    consume_bytes: bool;
    use_tol: bool;
    name: string;
    install_name: string;
    (* analysis *)
    verbose: bool;			      (* -v *)
    print_headers: bool;		      (* -h *)
    print_nlist: bool;			      (* -s *)
    print_libraries: bool;		      (* -l *)
    print_exports: bool;		      (* -e *)
    print_imports: bool;		      (* -i *)
    print_coverage: bool;		      (* -c *)
    disassemble: bool;			      (* -D *)
    (* building *)
    use_map: bool;			      (* -m *)
    recursive: bool;			      (* -r *)
    write_symbols: bool;		      (* -w *)
    marshal_symbols: bool;                    (* -b *)
    base_symbol_map_directories: string list; (* -d *)
    framework_directories: string list; (* -F *)
    (* general *)
    graph: bool;		(* -g *)
    filename: string;		(* anonarg *)
    search_term: string; 	(* -f *)
    use_goblin: bool		(* -G *)
  }

