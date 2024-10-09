open Types

type ('item, 'diff) t = Added of 'item | Removed of 'item | Modified of 'diff
type 'a atomic_modification = { reference : 'a; current : 'a }

type value = {
  vname : string;
  vdiff : (value_description, value_description atomic_modification) t;
}

type module_ = {
  mname : string;
  mdiff : (module_declaration, module_modification) t;
}

and modtype = {
  mtname : string;
  mtdiff : (modtype_declaration, module_modification) t;
}

and module_modification = Unsupported | Supported of sig_item list
and sig_item = Value of value | Module of module_ | Modtype of modtype

val interface :
  module_name:string ->
  reference:signature ->
  current:signature ->
  module_ option
