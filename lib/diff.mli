type ('item, 'diff) t = Added of 'item | Removed of 'item | Modified of 'diff
type 'a atomic_modification = { reference : 'a; current : 'a }

type value = {
  vname : string;
  vdiff :
    (Types.value_description, Types.value_description atomic_modification) t;
}

type module_ = {
  mname : string;
  mdiff : (Types.module_declaration, module_modification) t;
}

and module_modification = Unsupported | Supported of sig_item list
and sig_item = Value of value | Module of module_

val interface :
  module_name:string ->
  reference:Types.signature ->
  current:Types.signature ->
  module_ option
