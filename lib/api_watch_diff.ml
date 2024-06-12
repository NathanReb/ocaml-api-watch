open Types

type change = Added | Removed | Modified
type diff = Value of string * change | Any

module FieldMap = Map.Make (struct
  type t = string

  let compare = String.compare
end)

let env_setup ~ref_sig ~curr_sig =
  let env = Env.empty in
  let env = Env.in_signature true env in
  let env = Env.add_signature ref_sig env in
  Env.add_signature curr_sig env

let extract_values tbl items =
  List.fold_left
    (fun tbl item ->
      match item with
      | Sig_value (id, val_des, _) -> FieldMap.add (Ident.name id) val_des tbl
      | _ -> tbl)
    tbl items

let diff_value ~typing_env ~val_name ~reference ~current =
  let val_coercion1 () =
    Includecore.value_descriptions ~loc:current.val_loc typing_env val_name
      current reference
  in
  let val_coercion2 () =
    Includecore.value_descriptions ~loc:reference.val_loc typing_env val_name
      reference current
  in
  match (val_coercion1 (), val_coercion2 ()) with
  | Tcoerce_none, Tcoerce_none -> None
  | _, _ -> Some ()
  | exception Includecore.Dont_match _ -> Some ()

let compare_values ~reference ~current =
  let env = env_setup ~ref_sig:reference ~curr_sig:current in
  let ref_values = extract_values FieldMap.empty reference in
  let curr_values = extract_values FieldMap.empty current in
  let diffs =
    FieldMap.fold
      (fun val_name curr_vd acc ->
        if FieldMap.mem val_name ref_values then
          let ref_vd = FieldMap.find val_name ref_values in
          let value_differs =
            diff_value ~typing_env:env ~val_name ~reference:ref_vd
              ~current:curr_vd
          in
          match value_differs with
          | None -> acc
          | Some _ -> Value (val_name, Modified) :: acc
        else Value (val_name, Added) :: acc)
      curr_values []
  in
  FieldMap.fold
    (fun val_name _ref_vd acc ->
      if not (FieldMap.mem val_name curr_values) then
        Value (val_name, Removed) :: acc
      else acc)
    ref_values diffs

let diff_interface ~reference ~current =
  let value_diffs = compare_values ~reference ~current in
  if value_diffs = [] then
    let typing_env = Env.empty in
    let coercion1 () =
      Includemod.signatures typing_env ~mark:Mark_both reference current
    in
    let coercion2 () =
      Includemod.signatures typing_env ~mark:Mark_both current reference
    in
    match (coercion1 (), coercion2 ()) with
    | Tcoerce_none, Tcoerce_none -> []
    | _, _ -> [ Any ]
    | exception Includemod.Error _ -> [ Any ]
  else value_diffs
