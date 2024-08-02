open Types

let extract_non_alias_types signature =
  List.fold_left
    (fun acc item ->
      match item with
      | Sig_type (id, ({ type_manifest = None; _ } as type_decl), _, _) ->
          String_map.add (Ident.name id) (id, type_decl) acc
      | _ -> acc)
    String_map.empty signature

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

let set_type_equalities ~reference ~current =
  let ref_non_alias_types = extract_non_alias_types reference in
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
    current

let for_diff ~reference ~current =
  let modified_current = set_type_equalities ~reference ~current in
  let env = Env.empty in
  let env = Env.in_signature true env in
  let env = Env.add_signature reference env in
  Env.add_signature modified_current env
