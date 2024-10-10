open Api_watch
open Test_helpers

let%expect_test "test_diff_interface" =
  let result = Diff.interface ~module_name:"Main" ~reference:[] ~current:[] in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]

let%expect_test "Simple value, identical" =
  let reference = compile_interface {|val x : int|} in
  let current = compile_interface {|val x : int|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]

let%expect_test "Simple value, modified" =
  let reference = compile_interface {|val x : int|} in
  let current = compile_interface {|val x : string|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let ref_signature =
  compile_interface
    {|
    type t = int
    type unused_type = string
    val f : t -> string
    |}

let%expect_test "Same signature" =
  let result =
    Diff.interface ~module_name:"Main" ~reference:ref_signature
      ~current:ref_signature
  in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]

let add_value_signature =
  compile_interface
    {|
    type t = int
    type unused_type = string
    val f : t -> string
    val g : t -> t
    |}

let%expect_test "Adding a value" =
  let result =
    Diff.interface ~module_name:"Main" ~reference:ref_signature
      ~current:add_value_signature
  in
  Format.printf "%a" pp_diff_option result;
  [%expect {|Some (Module Main: {Modified (Supported [ Value (g, Added)])})|}]

let remove_value_signature =
  compile_interface {|
  type t = int
  type unused_type = string
  |}

let%expect_test "Removing a value" =
  let result =
    Diff.interface ~module_name:"Main" ~reference:ref_signature
      ~current:remove_value_signature
  in
  Format.printf "%a" pp_diff_option result;
  [%expect {|Some (Module Main: {Modified (Supported [ Value (f, Removed)])})|}]

let modify_value_signature =
  compile_interface
    {|
    type t = int
    type unused_type = string
    val f : t -> t
    |}

let%expect_test "Modifying a value" =
  let result =
    Diff.interface ~module_name:"Main" ~reference:ref_signature
      ~current:modify_value_signature
  in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (f, Modified)])})|}]

let add_type_signature =
  compile_interface
    {|
    type t = int
     type unused_type = string
     type added_t = float
     val f : t -> string
     |}

let%expect_test "Adding a type" =
  let result =
    Diff.interface ~module_name:"Main" ~reference:ref_signature
      ~current:add_type_signature
  in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Type (added_t, Added)])})|}]

let remove_type_signature =
  compile_interface {|
  type t = int
  val f : t -> string
  |}

let%expect_test "Removing a type" =
  let result =
    Diff.interface ~module_name:"Main" ~reference:ref_signature
      ~current:remove_type_signature
  in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Type (unused_type, Removed)])})|}]

let%expect_test "Modifying a simple type" =
  let reference = compile_interface {|type t = int|} in
  let current = compile_interface {|type t = string|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Type (t, Modified)])}) |}]

let%expect_test "Modifying a type used in a value" =
  let reference =
    compile_interface {|
    type t = int
    val f : t -> string
    |}
  in
  let current =
    compile_interface {|
    type t = float
    val f : t -> string
    |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| 
    Some (Module Main: {Modified (Supported [ Value (f, Modified); 
    Type (t, Modified)])})
    |}]

let ref_module_signature =
  compile_interface {|
  module M : sig val x : int end
  |}

let%expect_test "Same module" =
  let result =
    Diff.interface ~module_name:"Main" ~reference:ref_module_signature
      ~current:ref_module_signature
  in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]

let add_module_signature =
  compile_interface
    {|
    module M : sig val x : int end
    module N : sig val y : float end
    |}

let%expect_test "Adding a module" =
  let result =
    Diff.interface ~module_name:"Main" ~reference:ref_module_signature
      ~current:add_module_signature
  in
  Format.printf "%a" pp_diff_option result;
  [%expect {|Some (Module Main: {Modified (Supported [ Module N: Added])})|}]

let remove_module_signature = compile_interface {|

|}

let%expect_test "Removing a module" =
  let result =
    Diff.interface ~module_name:"Main" ~reference:ref_module_signature
      ~current:remove_module_signature
  in
  Format.printf "%a" pp_diff_option result;
  [%expect {|Some (Module Main: {Modified (Supported [ Module M: Removed])})|}]

let modify_module_signature =
  compile_interface {|
  module M : sig val x : float end
  |}

let%expect_test "Modifying a module" =
  let result =
    Diff.interface ~module_name:"Main" ~reference:ref_module_signature
      ~current:modify_module_signature
  in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Module M: {Modified (Supported [ Value (x, Modified)])}])})|}]

let%expect_test "One value more general than the other" =
  let general = compile_interface {|val x : 'a list|} in
  let specialized = compile_interface {|val x : float list|} in
  let result =
    Diff.interface ~module_name:"Main" ~reference:general ~current:specialized
  in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}];
  let rev_result =
    Diff.interface ~module_name:"Main" ~reference:specialized ~current:general
  in
  Format.printf "%a" pp_diff_option rev_result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Same abstract type" =
  let reference = compile_interface {|
    type t
    val x : t
  |} in
  let current = compile_interface {|
    type t
    val x : t
  |} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]

let%expect_test "Same record type" =
  let reference =
    compile_interface {|
    type t = {a:int; b:float}
    val x : t
  |}
  in
  let current =
    compile_interface {|
    type t = {a:int; b:float}
    val x : t
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]

let%expect_test "Adding a record field" =
  let reference =
    compile_interface {|
    type t = {a:int; b:float}
    val x : t
  |}
  in
  let current =
    compile_interface
      {|
    type t = {a:int; b:float; c:bool}
    val x : t
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Type (t, Modified)])}) |}]

let%expect_test "Removing a record field" =
  let reference =
    compile_interface {|
    type t = {a:int; b:float}
    val x : t
  |}
  in
  let current = compile_interface {|
    type t = {a:int}
    val x : t
  |} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Type (t, Modified)])}) |}]

let%expect_test "Modifying a record field" =
  let reference =
    compile_interface {|
    type t = {a:int; b:float}
    val x : t
  |}
  in
  let current =
    compile_interface {|
    type t = {a:int; b:string}
    val x : t
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Type (t, Modified)])}) |}]

let%expect_test "Same variant type" =
  let reference =
    compile_interface {|
    type t = A | B of int
    val x : t
  |}
  in
  let current =
    compile_interface {|
    type t = A | B of int
    val x : t
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]

let%expect_test "Adding a variant" =
  let reference =
    compile_interface {|
    type t = A | B of int
    val x : t
  |}
  in
  let current =
    compile_interface {|
    type t = A | B of int | C
    val x : t
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Type (t, Modified)])}) |}]

let%expect_test "Removing a variant" =
  let reference =
    compile_interface {|
    type t = A | B of int
    val x : t
  |}
  in
  let current = compile_interface {|
    type t = A
    val x : t
  |} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Type (t, Modified)])}) |}]

let%expect_test "Modifying a variant type" =
  let reference =
    compile_interface {|
    type t = A | B of int
    val x : t
  |}
  in
  let current =
    compile_interface {|
    type t = A | B of float
    val x : t
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Type (t, Modified)])}) |}]

let%expect_test "Inlined polymorphic variant, identical" =
  let reference = compile_interface {|val x : [ `A | `B ]|} in
  let current = compile_interface {|val x : [ `A | `B ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]

let%expect_test "Inlined polymorphic variant, modified" =
  let reference = compile_interface {|val x : [ `A | `B ]|} in
  let current = compile_interface {|val x : [ `A | `C ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Inlined polymorphic variant, extended" =
  let reference = compile_interface {|val x : [ `A | `B ]|} in
  let current = compile_interface {|val x : [ `A | `B | `C ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Inlined polymorphic variant, reduced" =
  let reference = compile_interface {|val x : [ `A | `B ]|} in
  let current = compile_interface {|val x : [ `A ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Named polymorphic variant, identical" =
  let reference =
    compile_interface {|
    type t = [ `A | `B ]
    val x : t
    |}
  in
  let current =
    compile_interface {|
    type t = [ `A | `B ]
    val x : t
    |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]

let%expect_test "Named polymorphic variant, modified" =
  let reference =
    compile_interface {|
    type t = [ `A | `B ]
    val x : t
    |}
  in
  let current =
    compile_interface {|
    type t = [ `A | `C ]
    val x : t
    |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|
    Some (Module Main: {Modified (Supported [ Value (x, Modified);
    Type (t, Modified)])})
    |}]

let%expect_test "Named polymorphic variant, extended" =
  let reference =
    compile_interface {|
    type t = [ `A | `B ]
    val x : t
    |}
  in
  let current =
    compile_interface {|
    type t = [ `A | `B | `C ]
    val x : t
    |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|
    Some (Module Main: {Modified (Supported [ Value (x, Modified); 
    Type (t, Modified)])})
    |}]

let%expect_test "Named polymorphic variant, reduced" =
  let reference =
    compile_interface {|
    type t = [ `A | `B ]
    val x : t
    |}
  in
  let current = compile_interface {|
    type t = [ `A ]
    val x : t
    |} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|
    Some (Module Main: {Modified (Supported [ Value (x, Modified);
    Type (t, Modified)])})
    |}]

let%expect_test "Open polymorphic variant, identical" =
  let reference = compile_interface {|val x : [> `A | `B ]|} in
  let current = compile_interface {|val x : [> `A | `B ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]

let%expect_test "Open polymorphic variant, modified" =
  let reference = compile_interface {|val x : [> `A | `B ]|} in
  let current = compile_interface {|val x : [> `A | `C ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Open polymorphic variant, extended" =
  let reference = compile_interface {|val x : [> `A | `B ]|} in
  let current = compile_interface {|val x : [> `A | `B | `C ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Open polymorphic variant, reduced" =
  let reference = compile_interface {|val x : [> `A | `B ]|} in
  let current = compile_interface {|val x : [> `A ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Less polymorphic variant, identical" =
  let reference = compile_interface {|val x : [< `A | `B ]|} in
  let current = compile_interface {|val x : [< `A | `B ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]

let%expect_test "Less polymorphic variant, modified" =
  let reference = compile_interface {|val x : [< `A | `B ]|} in
  let current = compile_interface {|val x : [< `A | `C ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Less polymorphic variant, extended" =
  let reference = compile_interface {|val x : [< `A | `B ]|} in
  let current = compile_interface {|val x : [< `A | `B | `C ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Less polymorphic variant, reduced" =
  let reference = compile_interface {|val x : [< `A | `B ]|} in
  let current = compile_interface {|val x : [< `A ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Changing from less to more polymorphic" =
  let reference = compile_interface {|val x : [< `A | `B ]|} in
  let current = compile_interface {|val x : [> `A | `B ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Changing from more to less polymorphic" =
  let reference = compile_interface {|val x : [> `A | `B ]|} in
  let current = compile_interface {|val x : [< `A | `B ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Changing from open and more to closed polymorphic" =
  let reference = compile_interface {|val x : [> `A | `B ]|} in
  let current = compile_interface {|val x : [ `A | `B ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Changing from open and less to closed polymorphic" =
  let reference = compile_interface {|val x : [< `A | `B ]|} in
  let current = compile_interface {|val x : [ `A | `B ]|} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]

let%expect_test "Extensible variant type, identical" =
  let reference =
    compile_interface
      {|
    type t = ..
    type t += A | B of int
    val x : t
  |}
  in
  let current =
    compile_interface
      {|
    type t = ..
    type t += A | B of int
    val x : t
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]

let%expect_test "Extensible variant type, extended" =
  let reference =
    compile_interface
      {|
    type t = ..
    type t += A | B of int
    val x : t
  |}
  in
  let current =
    compile_interface
      {|
    type t = ..
    type t += A | B of int | C
    val x : t
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {|Some (Module Main: {Modified (Unsupported)})|}]

let%expect_test "Extensible variant type, reduced" =
  let reference =
    compile_interface
      {|
    type t = ..
    type t += A | B of int 
    val x : t
  |}
  in
  let current =
    compile_interface {|
    type t = ..
    type t += A 
    val x : t
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {|Some (Module Main: {Modified (Unsupported)})|}]

let%expect_test "Extensible variant type, modified" =
  let reference =
    compile_interface
      {|
    type t = ..
    type t += A | B of int 
    val x : t
  |}
  in
  let current =
    compile_interface
      {|
    type t = ..
    type t += A | B of string
    val x : t
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {|Some (Module Main: {Modified (Unsupported)})|}]

let%expect_test "Changing from abstract to record type" =
  let reference = compile_interface {|
    type t 
    val x : t
  |} in
  let current =
    compile_interface {|
    type t = {a:string; b:int}
    val x : t
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Type (t, Modified)])}) |}]

let%expect_test "Changing from record to variant type" =
  let reference =
    compile_interface {|
    type t = {a:string; b:int}
    val x : t
  |}
  in
  let current =
    compile_interface {|
    type t = A of string | B of int 
    val x : t
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Type (t, Modified)])}) |}]

let%expect_test "Values referencing types with parameters, identical" =
  let reference =
    compile_interface
      {|
    type ('a, 'b) result = Ok of 'a | Error of 'b
    val x : (int ,string) result
  |}
  in
  let current =
    compile_interface
      {|
     type ('a, 'b) result = Ok of 'a | Error of 'b
    val x : (int ,string) result
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect {| None |}]

let%expect_test "Values referencing types with parameters, modified" =
  let reference =
    compile_interface
      {|
    type ('a, 'b) result = Ok of 'a | Error of 'b
    val x : (int ,string) result
  |}
  in
  let current =
    compile_interface
      {|
     type ('a, 'b) result = Ok of 'a | Error of 'b
    val x : (float ,string) result
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|Some (Module Main: {Modified (Supported [ Value (x, Modified)])})|}]
