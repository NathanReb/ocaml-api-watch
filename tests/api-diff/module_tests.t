# Tests for module modifications

Here we generate a `.mli` file with a module:

  $ cat > mod_ref.mli << EOF
  > module M : sig val x : int end
  > 
  > EOF

We generate the .cmi file

  $ ocamlc mod_ref.mli

And now we run api-watcher on that same cmi file as both arguments,
there should be no diff:

  $ api-diff mod_ref.cmi mod_ref.cmi

### Adding a module:

Generate a new .mli file with an additional module
  $ cat > add_module.mli << EOF
  > module M : sig val x : int end
  > module N : sig val y : float end
  > 
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc add_module.mli

Run api-diff and check the output
  $ api-diff mod_ref.cmi add_module.cmi
  diff module Add_module:
  +module N: sig val y : float end
  
  [1]

### Removing a module:

Generate a new .mli file with the module removed
  $ cat > remove_module.mli << EOF
  > 
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc remove_module.mli

Run api-diff and check the output
  $ api-diff mod_ref.cmi remove_module.cmi
  diff module Remove_module:
  -module M: sig val x : int end
  
  [1]

### Modifying a module:

Generate a new .mli file with the module modified
  $ cat > modify_module.mli << EOF
  > module M : sig val x : float end
  > 
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc modify_module.mli

Run api-diff and check the output
  $ api-diff mod_ref.cmi modify_module.cmi
  diff module Modify_module.M:
  -val x : int
  +val x : float
  
  [1]

Generate a new .mli file with values and submodules
  $ cat > orig_module.mli << EOF
  > module M : sig val x : float end
  > type ('a, 'b) result = Ok of 'a | Error of 'b
  >  val a : string -> int
  > val f : int -> string
  > module D : sig
  >   val b : int list -> int
  >   val g : int -> string
  > end
  > EOF

Compile the new .mli file to a .cmi file
  $ ocamlc orig_module.mli

Generate a new .mli file with the values and submodules modified
  $ cat > modified_module.mli << EOF
  > module M : sig val x : float end
  > type ('a, 'b) result = Ok of 'a | Error of 'b
  > val a : string -> float
  > val f : int -> (string, string) result
  > module D : sig
  >   val b : float list -> float
  >   val g : int -> (string, string) result
  > end
  > module E : sig val x: int end
  > EOF

Compile the modified .mli file to a .cmi file
  $ ocamlc modified_module.mli

Run api-diff and check the output
  $ api-diff orig_module.cmi modified_module.cmi
  diff module Modified_module:
  -val a : string -> int
  +val a : string -> float
  -val f : int -> string
  +val f : int -> (string, string) result
  +module E: sig val x : int end
  
  diff module Modified_module.D:
  -val b : int list -> int
  +val b : float list -> float
  -val g : int -> string
  +val g : int -> (string, string) result
  
  [1]
