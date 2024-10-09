(** Utilities for custom diff printing  *)

type conflict2 = { orig : string list; new_ : string list }

type t = conflict2 list String_map.t
(** Type for representing library interface diffs as text diff.

    Changes are arranged per fully qualified module path.
    Keys are module path, as strings, that map to the textual diff
    for the content of said module.
    
    The removal or addition of a module is listed under its parent.
    E.g. if [Main.M] was removed, this will show in the textual diff
    under the key ["Main"].
    On the other hand, if [Main.M] is present in both versions but received
    a new function [Main.M.do_something], this will show in the textual
    diff under the key ["Main.M"].
    Identical modules won't appear in the map.
 *)

val pp : Format.formatter -> t -> unit
(** Pretty-print the text diff in a human readable, git diff like format. *)

val from_diff : Diff.module_ -> t
(** Converts from a low-level diff to a textual diff *)

module With_colors : sig
  val pp : Format.formatter -> t -> unit
  (** Same as regular [pp] but prints added lines in green and removed lines
    in red. *)
end
