Here we generate a basic `.mli` file with two types and a function:

  $ cat > ref.mli << EOF
  > type t = int
  > type unused_type = string
  > val f : t -> string
  > EOF

We generate the .cmi file

  $ ocamlc ref.mli

# Tests for different .cmi files for type modifications

### A file with an additional type:

  $ cat > add_type.mli <<EOF
  > type t = int
  > type unused_type = string
  > type added_t = float
  > val f : t -> string
  > EOF

We generate the .cmi file

  $ ocamlc add_type.mli

Run api-watcher on the two cmi files, there should be a difference

  $ api-diff ref.cmi add_type.cmi
  diff module Add_type:
  +type added_t = float
  
  [1]

### A file with a removed type:

  $ cat > remove_type.mli <<EOF
  > type t = int
  > val f : t -> string
  > EOF

We generate the .cmi file

  $ ocamlc remove_type.mli

Run api-watcher on the two cmi files, there should be a difference

  $ api-diff ref.cmi remove_type.cmi
  diff module Remove_type:
  -type unused_type = string
  
  [1]

### A file with a modified type:

  $ cat > modify_type.mli <<EOF
  > type t = float
  > type unused_type = string
  > val f : t -> string
  > EOF

We generate a .cmi file

  $ ocamlc modify_type.mli

Run api-watcher on the two cmi files, there should be a difference

  $ api-diff ref.cmi modify_type.cmi
  diff module Modify_type:
  -type t = int
  +type t = float
  
  [1]
