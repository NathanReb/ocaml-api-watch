open Types

type ('item, 'diff) diff =
  | Added of 'item
  | Removed of 'item
  | Modified of 'diff

type 'a atomic_modification = { reference : 'a; current : 'a }
(** The simplest diff representation for the modification of a value of type 'a.
     [reference] is the value before and [current] is the value after the change occured.
     Use this type when there is no better representation available. *)

type value_diff = {
  vname : string;
  vdiff : (value_description, value_description atomic_modification) diff;
}

type module_diff = {
  mname : string;
  mdiff : (module_declaration, module_modification) diff;
}

and module_modification = Unsupported | Supported of item_diff list
and item_diff = Value of value_diff | Module of module_diff

type item_type = Value_item | Module_item [@@deriving ord]
type sig_items = Val of value_description | Mod of module_declaration

module Sig_item_map = Map.Make (struct
  type t = item_type * string [@@deriving ord]
end)

module String_map = Map.Make (String)

let extract_non_alias_types ref_sig =
  List.fold_left
    (fun acc item ->
      match item with
      | Sig_type (id, ({ type_manifest = None; _ } as type_decl), _, _) ->
          String_map.add (Ident.name id) (id, type_decl) acc
      | _ -> acc)
    String_map.empty ref_sig

let set_type_equality ref_id ~ref_decl ~curr_decl =
  if List.length ref_decl.type_params = List.length curr_decl.type_params then
    let ref_path = Path.Pident ref_id in
    {
      curr_decl with
      type_manifest =
        Some
          (Btype.newgenty (Tconstr (ref_path, curr_decl.type_params, ref Mnil)));
    }
  else curr_decl

let set_type_equalities ~ref_sig ~curr_sig =
  let ref_non_alias_types = extract_non_alias_types ref_sig in
  List.map
    (fun item ->
      match item with
      | Sig_type
          ( curr_id,
            ({ type_manifest = None; _ } as curr_decl),
            rec_st,
            visibility ) -> (
          let name = Ident.name curr_id in
          match String_map.find_opt name ref_non_alias_types with
          | Some (ref_id, ref_decl) ->
              let new_decl = set_type_equality ref_id ~ref_decl ~curr_decl in
              Sig_type (curr_id, new_decl, rec_st, visibility)
          | None -> item)
      | _ -> item)
    curr_sig

let env_setup ~ref_sig ~curr_sig =
  let modified_curr_sig = set_type_equalities ~ref_sig ~curr_sig in
  let env = Env.empty in
  let env = Env.in_signature true env in
  let env = Env.add_signature ref_sig env in
  Env.add_signature modified_curr_sig env

let extract_items items =
  List.fold_left
    (fun tbl item ->
      match item with
      | Sig_module (id, _, mod_decl, _, _) ->
          Sig_item_map.add (Module_item, Ident.name id) (Mod mod_decl) tbl
      | Sig_value (id, val_des, _) ->
          Sig_item_map.add (Value_item, Ident.name id) (Val val_des) tbl
      | _ -> tbl)
    Sig_item_map.empty items

let diff_modtype_item ~loc ~typing_env ~name ~reference ~current =
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

let diff_value_item ~typing_env ~name ~reference ~current =
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

let rec diff_items ~reference ~current =
  let env = env_setup ~ref_sig:reference ~curr_sig:current in
  let ref_items = extract_items reference in
  let curr_items = extract_items current in
  Sig_item_map.merge
    (fun (item_type, name) ref_opt curr_opt ->
      match (item_type, ref_opt, curr_opt) with
      | Value_item, reference, current ->
          diff_value_item ~typing_env:env ~name ~reference ~current
      | Module_item, reference, current ->
          diff_module_item ~typing_env:env ~name ~reference ~current)
    ref_items curr_items
  |> Sig_item_map.bindings |> List.map snd

and diff_module_item ~typing_env ~name ~reference ~current =
  match (reference, current) with
  | None, None -> None
  | None, Some (Mod curr_md) ->
      Some (Module { mname = name; mdiff = Added curr_md })
  | Some (Mod ref_md), None ->
      Some (Module { mname = name; mdiff = Removed ref_md })
  | Some (Mod reference), Some (Mod current) ->
      diff_module_declaration ~typing_env ~name ~reference ~current
  | _ -> assert false

and diff_module_declaration ~typing_env ~name ~reference ~current =
  match (reference.md_type, current.md_type) with
  | Mty_signature ref_submod, Mty_signature curr_submod ->
      diff_signatures ~typing_env ~reference:ref_submod ~current:curr_submod
      |> Option.map (fun mdiff -> Module { mname = name; mdiff })
  | ref_modtype, curr_modtype ->
      diff_modtype_item ~loc:reference.md_loc ~typing_env ~name
        ~reference:ref_modtype ~current:curr_modtype

and diff_signatures ~typing_env ~reference ~current =
  match diff_items ~reference ~current with
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

let diff_interface ~module_name ~reference ~current =
  let typing_env = Env.empty in
  diff_signatures ~typing_env ~reference ~current
  |> Option.map (fun mdiff -> { mname = module_name; mdiff })

let vd_to_string name vd =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.value_description (Ident.create_local name) formatter vd;
  Format.pp_print_flush formatter ();
  Buffer.contents buf

let md_to_string name md =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.modtype formatter md.md_type;
  Format.pp_print_flush formatter ();
  "module " ^ name ^ ": " ^ Buffer.contents buf

let process_value_diff (val_diff : value_diff) =
  match val_diff.vdiff with
  | Added vd ->
      [
        Diffutils.Diff.Diff
          { orig = []; new_ = [ vd_to_string val_diff.vname vd ] };
      ]
  | Removed vd ->
      [
        Diffutils.Diff.Diff
          { orig = [ vd_to_string val_diff.vname vd ]; new_ = [] };
      ]
  | Modified { reference; current } ->
      [
        Diffutils.Diff.Diff
          {
            orig = [ vd_to_string val_diff.vname reference ];
            new_ = [ vd_to_string val_diff.vname current ];
          };
      ]

let to_text_diff (diff_result : module_diff) : Diffutils.Diff.t String_map.t =
  let rec process_module_diff module_path (module_diff : module_diff) acc =
    match module_diff.mdiff with
    | Modified Unsupported ->
        String_map.add module_path
          [
            Diffutils.Diff.Diff { orig = []; new_ = [ "<unsupported change>" ] };
          ]
          acc
    | Added curr_md ->
        let diff =
          [
            Diffutils.Diff.Diff
              { orig = []; new_ = [ md_to_string module_diff.mname curr_md ] };
          ]
        in
        String_map.update module_path
          (function
            | None -> Some diff | Some existing -> Some (existing @ diff))
          acc
    | Removed ref_md ->
        let diff =
          [
            Diffutils.Diff.Diff
              { orig = [ md_to_string module_diff.mname ref_md ]; new_ = [] };
          ]
        in
        String_map.update module_path
          (function
            | None -> Some diff | Some existing -> Some (existing @ diff))
          acc
    | Modified (Supported changes) ->
        List.fold_left
          (fun acc' change ->
            match change with
            | Value val_diff ->
                let diff = process_value_diff val_diff in
                String_map.update module_path
                  (function
                    | None -> Some diff | Some existing -> Some (existing @ diff))
                  acc'
            | Module sub_module_diff ->
                let sub_module_path =
                  match sub_module_diff.mdiff with
                  | Modified _ -> module_path ^ "." ^ sub_module_diff.mname
                  | Added _ | Removed _ -> module_path
                in
                process_module_diff sub_module_path sub_module_diff acc')
          acc changes
  in
  process_module_diff diff_result.mname diff_result String_map.empty
