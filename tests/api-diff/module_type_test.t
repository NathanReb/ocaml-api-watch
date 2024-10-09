# Tests for module type type modifications

Here we generate a `.mli` file with a module type:

  $ cat > modtype_ref.mli << EOF
  > module type M = sig val x : int end
  > 
  > EOF

We generate the .cmi file

  $ ocamlc modtype_ref.mli

And now we run api-watcher on that same cmi file as both arguments,
there should be no diff:

  $ api-diff modtype_ref.cmi modtype_ref.cmi

### Adding a module type:

Generate a new .mli file with an additional module type
  $ cat > add_modtype.mli << EOF
  > module type M = sig val x : int end
  > module type P = sig val y : float end
  > 
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc add_modtype.mli

Run api-diff and check the output
  $ api-diff modtype_ref.cmi add_modtype.cmi
  diff module Add_modtype:
  +module type P = sig val y : float end
  
  [1]

### Removing a module type:

Generate a new .mli file with the module type removed
  $ cat > remove_modtype.mli << EOF
  > 
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc remove_modtype.mli

Run api-diff and check the output
  $ api-diff modtype_ref.cmi remove_modtype.cmi
  diff module Remove_modtype:
  -module type M = sig val x : int end
  
  [1]

### Modifying a module type:

Generate a new .mli file with the module type modified
  $ cat > modify_modtype.mli << EOF
  > module type M = sig val x : float end
  > 
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc modify_modtype.mli

Run api-diff and check the output
  $ api-diff modtype_ref.cmi modify_modtype.cmi
  diff module Modify_modtype.M:
  -val x : int
  +val x : float
  
  [1]

Generate a new .mli file with values, and submodule types
  $ cat > orig_modtype.mli << EOF
  > module type M = sig val x : float end
  > type ('a, 'b) result = Ok of 'a | Error of 'b
  >  val a : string -> int
  > val f : int -> string
  > module type D = sig
  >   val b : int list -> int
  >   val g : int -> string
  > end
  > module P : sig val a : int end
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc orig_modtype.mli

Generate a new .mli file with the values and submodule types modified
  $ cat > modified_modtype.mli << EOF
  > module type M = sig val x : float end
  > type ('a, 'b) result = Ok of 'a | Error of 'b
  > val a : string -> float
  > val f : int -> (string, string) result
  > module type D = sig
  >   val b : float list -> float
  >   val g : int -> (string, string) result
  > end
  > module type E = sig val x: int end
  > EOF

Compile the modified .mli file to a .cmi file
  $ ocamlc modified_modtype.mli

Run api-diff and check the output
  $ api-diff orig_modtype.cmi modified_modtype.cmi
  diff module Modified_modtype:
  -val a : string -> int
  +val a : string -> float
  -val f : int -> string
  +val f : int -> (string, string) result
  -module P: sig val a : int end
  +module type E = sig val x : int end
  
  diff module Modified_modtype.D:
  -val b : int list -> int
  +val b : float list -> float
  -val g : int -> string
  +val g : int -> (string, string) result
  
  [1]
