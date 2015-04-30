(* a fucking config file cause david told me I'm dumb for not having one... and DAVID BE MAD RIGHT *)

type basic_config =
  {
    (* internal *)
    analyze: bool;
    silent: bool;
    consume_bytes: bool;
    (* analysis *)
    print_nlist: bool;			      (* -s *)
    verbose: bool;			      (* -v *)
    disassemble: bool;			      (* -D *)
    (* building *)
    use_map: bool;			      (* -m *)
    recursive: bool;			      (* -r *)
    write_symbols: bool;		      (* -w *)
    marshal_symbols: bool;                    (* -b *)
    base_symbol_map_directories: string list; (* -d *)
    (* general *)
    graph: bool;		(* -g *)
    filename: string;		(* anonarg *)
    search_term: string; 	(* -f *)
    use_goblin: bool		(* -G *)
  }

