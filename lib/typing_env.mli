(** Utilities to setup and manipulate typing environments *)

open Types

val for_diff : reference:signature -> current:signature -> Env.t
(** Returns a typing environment suited for diffing two versions,
    [reference] and [current], of the same signature.
    We need to force the compiler to treat types from the two signatures
    as equal.
    
    To do so, we first add the [reference] signature to our fresh
    typing environment and then add a modified version of [current]
    with type equalities set.
    Roughly put, if there is a [type t] declared in both signatures,
    by default, those will be attributed a separate ID by the compiler
    and considered incompatible, treating them as [type t1] and [type t2].
    To work around that, we first add [type t1] and then [type t2 = t1].
    It can be the case that the types are structuraly incompatible because they
    were modified between the two versions but the compiler will accept those.
*)
