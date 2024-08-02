type ('item, 'diff) diff =
  | Added of 'item
  | Removed of 'item
  | Modified of 'diff

type 'a atomic_modification = { reference : 'a; current : 'a }

type value_diff = {
  vname : string;
  vdiff :
    (Types.value_description, Types.value_description atomic_modification) diff;
}

type module_diff = {
  mname : string;
  mdiff : (Types.module_declaration, module_modification) diff;
}

and module_modification = Unsupported | Supported of item_diff list
and item_diff = Value of value_diff | Module of module_diff

val diff_interface :
  module_name:string ->
  reference:Types.signature ->
  current:Types.signature ->
  module_diff option

module String_map : Map.S with type key = string

val to_text_diff : module_diff -> Diffutils.Diff.t String_map.t
