open Api_watch
open Test_helpers

let%expect_test "Module types with both value and submodule changes" =
  let reference =
    compile_interface
      {|
    type ('a, 'b) result = Ok of 'a | Error of 'b
    val f : int -> string
    module type C = sig
      val g : int -> string
    end
  |}
  in
  let current =
    compile_interface
      {|
    type ('a, 'b) result = Ok of 'a | Error of 'b
    val f : int -> (string, string) result
    module type M = sig
      val g : int -> (string, string) result
    end
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|
    Some (Module Main: {Modified (Supported [ Value (f, Modified);
    Module_type C: Removed;
    Module_type M: Added])})
    |}]

let%expect_test "Module types with multiple value and submodule changes" =
  let reference =
    compile_interface
      {|
    type ('a, 'b) result = Ok of 'a | Error of 'b
    val a : string -> int
    val f : int -> string
    module type M = sig
      val b : int list -> int
      val g : int -> string
      val z : string
    end
    module P : sig val x: float end
  |}
  in
  let current =
    compile_interface
      {|
    type ('a, 'b) result = Ok of 'a | Error of 'b
    val a : string -> float
    val f : int -> (string, string) result
    module type M = sig
      val b : float list -> float
      val g : int -> (string, string) result
      val z : string
    end
    module type N = sig val x: int end
    module P : sig val x: int end
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|
    Some (Module Main: {Modified (Supported [ Value (a, Modified);
    Value (f, Modified);
    Module P: {Modified (Supported [ Value (x, Modified)])};
    Module M: {Modified (Supported [ Value (b, Modified);
    Value (g, Modified)])};
    Module_type N: Added])})
    |}]

let%expect_test "Module types with both supported and unsupported changes" =
  let reference =
    compile_interface {|
  val x: int
  module type N = sig

  end|}
  in
  let current =
    compile_interface {|
  module type N = sig
  type t
  end
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  (* TODO: label becomes Module N instead of Module_type N*)
  [%expect
    {|
    Some (Module Main: {Modified (Supported [ Value (x, Removed);
    Module N: {Modified (Unsupported)}])})
    |}]
