(* a fucking config file cause david told me I'm dumb for not having one... and DAVID BE MAD RIGHT *)

type basic_config =
  {
    (* analysis *)
    analyze: bool;			      (* internal *)
    silent: bool;			      (* internal *)
    print_nlist: bool;			      (* -s *)
    verbose: bool;			      (* -v *)
    disassemble: bool;			      (* -D *)
    (* building *)
    build: bool;			      (* -b *)
    recursive: bool;			      (* -r *)
    write_symbols: bool;		      (* -w *)
    marshal_symbols: bool;                    (* -m *)
    base_symbol_map_directories: string list; (* -d *)
    (* general *)
    graph: bool;		(* -g *)
    filename: string;		(* anonarg *)
    search_term: string; 	(* -f *)
    use_goblin: bool		(* -G *)
  }

