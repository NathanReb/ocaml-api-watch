(** Conversion from library diff abstraction to textual diff, line based
    diff *)

type t = Diffutils.Diff.t String_map.t
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

    Note that the individual [Diffutils.Diff.t] stored in the map only
    contain line changes, i.e. only [Diff {orig; new_}] hunks, no [Same s]
    ones. *)

val from_diff : Diff.module_ -> Diffutils.Diff.t String_map.t
