Create the first version of a simple project
  $ mkdir -p project_v1/lib

  $ cat > project_v1/dune-project <<EOF
  > (lang dune 2.9)
  > (name myproject)
  > EOF

  $ cat > project_v1/lib/dune <<EOF
  > (library
  >  (name mylib))
  > EOF

Create math.ml file with basic math functions and an Advanced module
  $ cat > project_v1/lib/math.ml <<EOF
  > let add x y = x + y
  > let subtract x y = x - y
  > module Advanced = struct
  >   let square x = x * x
  >   type shape = Square | Circle
  > end
  > EOF

Create math.mli interface file
  $ cat > project_v1/lib/math.mli <<EOF
  > val add : int -> int -> int
  > val subtract : int -> int -> int
  > module Advanced : sig
  >   val square : int -> int
  >   type shape = Square | Circle
  > end
  > EOF

Create utils.ml file with a double function
  $ cat > project_v1/lib/utils.ml <<EOF
  > let double x = x * 2
  > EOF

Create utils.mli interface file
  $ cat > project_v1/lib/utils.mli <<EOF
  > val double : int -> int
  > EOF

Build the first version
  $ cd project_v1 && dune build && cd ..

Create the second version of the same project with some changes
  $ cp -r project_v1 project_v2

Update math.ml in project_v2 with new functions and modifications
  $ cat > project_v2/lib/math.ml <<EOF
  > let add x y z = x + y + z
  > let subtract x y = x - y
  > let multiply x y = x * y
  > module Advanced = struct
  >   let square x = x * x
  >   let cube x = x * x * x
  >   type shape = Square | Circle | Triangle
  > end
  > module New_module = struct
  >   let hello () = "Hello, World!"
  > end
  > EOF

Update math.mli in project_v2 to reflect changes
  $ cat > project_v2/lib/math.mli <<EOF
  > val add : int -> int -> int -> int
  > val subtract : int -> int -> int
  > val multiply : int -> int -> int
  > module Advanced : sig
  >   val square : int -> int
  >   val cube : int -> int
  >   type shape = Square | Circle | Triangle
  > end
  > module New_module : sig
  >   val hello : unit -> string
  > end
  > EOF

Update utils.ml in project_v2 with a new triple function
  $ cat > project_v2/lib/utils.ml <<EOF
  > let double x = x * 2
  > let triple x = x * 3
  > EOF

Update utils.mli in project_v2 to include the new triple function
  $ cat > project_v2/lib/utils.mli <<EOF
  > val double : int -> int
  > val triple : int -> int
  > EOF

Build the second version
  $ cd project_v2 && dune build && cd ..

Run the api-diff tool on the two project versions
  $ api-diff --main-module mylib  project_v1/_build/default/lib/.mylib.objs/byte project_v2/_build/default/lib/.mylib.objs/byte
  diff module Mylib.Math:
  -val add : int -> int -> int
  +val add : int -> int -> int -> int
  +val multiply : int -> int -> int
  +module New_module: sig val hello : unit -> string end
  
  diff module Mylib.Math.Advanced:
  +val cube : int -> int
  -type shape = Square | Circle
  +type shape = Square | Circle | Triangle
  
  diff module Mylib.Utils:
  +val triple : int -> int
  
  [1]
