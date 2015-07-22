type t = 
  {
    name: string;        (* name of the imported symbol *)
    lib: string;         (* library which contains the binary *)
    is_lazy: bool;
    mutable idx: int;    (* the index into some goblin's export code section, typically generated via the dynamic linker *)
    mutable offset: int; (* offset into (tol#find import.lib).code *)
    size: int;           (* size of the imported symbol, in bytes *)
  }
