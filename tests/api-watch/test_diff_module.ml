open Api_watch
open Test_helpers

let%expect_test "Modules with both value and submodule changes" =
  let reference =
    compile_interface
      {|
    type ('a, 'b) result = Ok of 'a | Error of 'b
    val f : int -> string
    module M : sig
      val g : int -> string
    end
  |}
  in
  let current =
    compile_interface
      {|
    type ('a, 'b) result = Ok of 'a | Error of 'b
    val f : int -> (string, string) result
    module M : sig
      val g : int -> (string, string) result
    end
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|
    Some (Module Main: {Modified (Supported [ Value (f, Modified);
    Module M: {Modified (Supported [ Value (g, Modified)])}])})|}]

let%expect_test "Modules with multiple value and submodule changes" =
  let reference =
    compile_interface
      {|
    type ('a, 'b) result = Ok of 'a | Error of 'b
    val a : string -> int 
    val f : int -> string
    module M : sig
      val b : int list -> int
      val g : int -> string
      val z : string
    end
  |}
  in
  let current =
    compile_interface
      {|
    type ('a, 'b) result = Ok of 'a | Error of 'b
    val a : string -> float
    val f : int -> (string, string) result
    module M : sig
      val b : float list -> float
      val g : int -> (string, string) result
      val z : string
    end
    module N : sig val x: int end
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|
    Some (Module Main: {Modified (Supported [ Value (a, Modified);
    Value (f, Modified);
    Module M: {Modified (Supported [ Value (b, Modified);
    Value (g, Modified)])};
    Module N: Added])})|}]

let%expect_test "Modules with both supported and unsupported changes" =
  let reference =
    compile_interface {|
  val x: int
  module M: sig 
  
  end|}
  in
  let current = compile_interface {|
  module M: sig
  type t
  end
  |} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|
    Some (Module Main: {Modified (Supported [ Value (x, Removed);
    Module M: {Modified (Unsupported)}])})|}]

let%expect_test "Submodules with different functor types" =
  let reference =
    compile_interface
      {|
    module type X = sig
      val x : int
    end

    module F (M : X) : sig
      val double : int
    end
  |}
  in
  let current =
    compile_interface
      {|
    module type Y = sig
      val y : float
    end

    module F (M : Y) : sig
      val double : int
    end
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|
    Some (Module Main: {Modified (Supported [ Module F: {Modified (Unsupported)};
    Module_type X: Removed;
    Module_type Y: Added])})
    |}]

let%expect_test "Submodule with module type modified from signature to functor"
    =
  let reference =
    compile_interface
      {|
    module M : sig
      val x : int
      val y : string
    end
  |}
  in
  let current =
    compile_interface
      {|
    module M : functor (X : sig val z : float end) -> sig
      val x : int
      val y : string
    end
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|
    Some (Module Main: {Modified (Supported [ Module M: {Modified (Unsupported)}])})|}]
