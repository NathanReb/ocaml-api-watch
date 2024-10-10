open Types

type ('item, 'diff) t = Added of 'item | Removed of 'item | Modified of 'diff

type 'a atomic_modification = { reference : 'a; current : 'a }
(** The simplest diff representation for the modification of a value of type 'a.
     [reference] is the value before and [current] is the value after the change occured.
     Use this type when there is no better representation available. *)

type value = {
  vname : string;
  vdiff : (value_description, value_description atomic_modification) t;
}

type type_ = {
  tname : string;
  tdiff : (type_declaration, type_declaration atomic_modification) t;
}

type module_ = {
  mname : string;
  mdiff : (module_declaration, module_modification) t;
}

and module_modification = Unsupported | Supported of sig_item list
and sig_item = Value of value | Module of module_ | Type of type_

type item_type = Value_item | Module_item | Type_item [@@deriving ord]

type sig_items =
  | Val of value_description
  | Mod of module_declaration
  | Typ of type_declaration * Ident.t

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
      | Sig_type (id, type_decl, _, _) ->
          Sig_item_map.add (Type_item, Ident.name id) (Typ (type_decl, id)) tbl
      | _ -> tbl)
    Sig_item_map.empty items

let modtype_item ~loc ~typing_env ~name ~reference ~current =
  let modtype_coercion1 () =
    Includemod.modtypes ~loc typing_env ~mark:Mark_both reference current
  in
  let modtype_coercion2 () =
    Includemod.modtypes ~loc typing_env ~mark:Mark_both current reference
  in
  match (modtype_coercion1 (), modtype_coercion2 ()) with
  | Tcoerce_none, Tcoerce_none -> None
  | _, _ -> Some (Module { mname = name; mdiff = Modified Unsupported })
  | exception Includemod.Error _ ->
      Some (Module { mname = name; mdiff = Modified Unsupported })

let type_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | Some (Typ (reference, _)), None ->
      Some (Type { tname = name; tdiff = Removed reference })
  | None, Some (Typ (current, _)) ->
      Some (Type { tname = name; tdiff = Added current })
  | Some (Typ (reference, refId)), Some (Typ (current, curId)) -> (
      let type_coercion1 () =
        Includecore.type_declarations ~equality:true ~loc:current.type_loc
          typing_env ~mark:true name current (Pident curId) reference
      in
      let type_coercion2 () =
        Includecore.type_declarations ~equality:true ~loc:reference.type_loc
          typing_env ~mark:true name reference (Pident refId) current
      in
      match (type_coercion1 (), type_coercion2 ()) with
      | None, None -> None
      | _, _ ->
          Some (Type { tname = name; tdiff = Modified { reference; current } }))
  | _ -> None

let value_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | Some (Val reference), None ->
      Some (Value { vname = name; vdiff = Removed reference })
  | None, Some (Val current) ->
      Some (Value { vname = name; vdiff = Added current })
  | Some (Val reference), Some (Val current) -> (
      let val_coercion1 () =
        Includecore.value_descriptions ~loc:current.val_loc typing_env name
          current reference
      in
      let val_coercion2 () =
        Includecore.value_descriptions ~loc:reference.val_loc typing_env name
          reference current
      in
      match (val_coercion1 (), val_coercion2 ()) with
      | Tcoerce_none, Tcoerce_none -> None
      | _, _ ->
          Some (Value { vname = name; vdiff = Modified { reference; current } })
      | exception Includecore.Dont_match _ ->
          Some (Value { vname = name; vdiff = Modified { reference; current } })
      )
  | _ -> None

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
      | Type_item, reference, current ->
          type_item ~typing_env:env ~name ~reference ~current)
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
