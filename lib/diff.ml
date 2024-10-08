open Types

type ('item, 'diff) t = Added of 'item | Removed of 'item | Modified of 'diff

type 'a atomic_modification = { reference : 'a; current : 'a }
(** The simplest diff representation for the modification of a value of type 'a.
     [reference] is the value before and [current] is the value after the change occurred.
     Use this type when there is no better representation available. *)

type value = {
  vname : string;
  vdiff : (value_description, value_description atomic_modification) t;
}
type class_ = {
  cname: string;
  cdiff: (class_declaration atomic_modification) t;
}
type module_ = {
  mname : string;
  mdiff : (module_declaration, module_modification) t;
}
and module_modification = Unsupported | Supported of sig_item list
and sig_item = Value of value | Module of module_

type item_type = Value_item | Module_item | Class_item [@@deriving ord]
type sig_items = Val of value_description | Mod of module_declaration | Cls of class_declaration

module Sig_item_map = Map.Make (struct
  type t = item_type * string [@@deriving ord]
end)

let extract_items items =
  List.fold_left
    (fun tbl item ->
      match item with
      | Sig_module (id, _, mod_decl, _, _) ->
          Sig_item_map.add (Module_item, Ident.name id) (Mod mod_decl) tbl
      | Sig_value (id, val_des, _) ->
          Sig_item_map.add (Value_item, Ident.name id) (Val val_des) tbl
      | Sig_class (id, cls_decl, _, _) ->
          Sig_item_map.add (Class_item, Ident.name id) (Cls cls_decl) tbl
      | _ -> tbl)
    Sig_item_map.empty items

let print_class_change = function
  | Added cls_name -> Printf.printf "Added class: %s\n" cls_name
  | Removed cls_name -> Printf.printf "Removed class: %s\n" cls_name
  | _ -> ()

let rec items ~reference ~current =
  let env = Typing_env.for_diff ~reference ~current in
  let ref_items = extract_items reference in
  let curr_items = extract_items current in
  Sig_item_map.merge
    (fun (item_type, name) ref_opt curr_opt ->
      match (item_type, ref_opt, curr_opt) with
      | Value_item, reference, current ->
          value_item ~typing_env:env ~name ~reference ~current
      | Module_item, reference, current ->
          module_item ~typing_env:env ~name ~reference ~current
      | Class_item, None, Some (Cls curr_cls) ->
          print_class_change (Added (Ident.name curr_cls));
          Some (Class { cname = name; cdiff = Added curr_cls })
      | Class_item, Some (Cls ref_cls), None ->
          print_class_change (Removed (Ident.name ref_cls));
          Some (Class { cname = name; cdiff = Removed ref_cls })
      | Class_item, Some (Cls reference), Some (Cls current) ->
          class_declaration ~typing_env:env ~name ~reference ~current
      | _ -> None)
    ref_items curr_items
  |> Sig_item_map.bindings |> List.map snd

and module_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | None, Some (Mod curr_md) ->
      Some (Module { mname = name; mdiff = Added curr_md })
  | Some (Mod ref_md), None ->
      Some (Module { mname = name; mdiff = Removed ref_md })
  | Some (Mod reference), Some (Mod current) ->
      module_declaration ~typing_env ~name ~reference ~current
  | _ -> assert false

and module_declaration ~typing_env ~name ~reference ~current =
  match (reference.md_type, current.md_type) with
  | Mty_signature ref_submod, Mty_signature curr_submod ->
      signatures ~typing_env ~reference:ref_submod ~current:curr_submod
      |> Option.map (fun mdiff -> Module { mname = name; mdiff })
  | ref_modtype, curr_modtype ->
      modtype_item ~loc:reference.md_loc ~typing_env ~name
        ~reference:ref_modtype ~current:curr_modtype

and class_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | None, Some (Cls curr_cls) ->
      print_class_change (Added (Ident.name curr_cls));
      Some (Class { cname = name; cdiff = Added curr_cls })
  | Some (Cls ref_cls), None ->
      print_class_change (Removed (Ident.name ref_cls));
      Some (Class { cname = name; cdiff = Removed ref_cls })
  | Some (Cls reference), Some (Cls current) ->
      class_declaration ~typing_env ~name ~reference ~current
  | _ -> assert false

and class_declaration ~typing_env ~name ~reference ~current =
  match Includemod.classes typing_env reference current with
  | Tcoerce_none -> None
  | _ -> Some (Class { cname = name; cdiff = Modified { reference; current } })

and signatures ~typing_env ~reference ~current =
  match items ~reference ~current with
  | [] -> (
      let coercion1 () =
        Includemod.signatures typing_env ~mark:Mark_both reference current
      in
      let coercion2 () =
        Includemod.signatures typing_env ~mark:Mark_both current reference
      in
      match (coercion1 (), coercion2 ()) with
      | Tcoerce_none, Tcoerce_none -> None
      | _, _ -> Some (Modified Unsupported)
      | exception Includemod.Error _ -> Some (Modified Unsupported))
  | item_changes -> Some (Modified (Supported item_changes))

let interface ~module_name ~reference ~current =
  let typing_env = Env.empty in
  signatures ~typing_env ~reference ~current
  |> Option.map (fun mdiff -> { mname = module_name; mdiff })