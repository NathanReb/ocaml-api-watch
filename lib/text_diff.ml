open Types

type t = Diffutils.Diff.t String_map.t

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

let process_value_diff (val_diff : Diff.value) =
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

let from_diff (diff : Diff.module_) : Diffutils.Diff.t String_map.t =
  let rec process_module_diff module_path (module_diff : Diff.module_) acc =
    match diff.mdiff with
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
            match (change : Diff.sig_item) with
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
  process_module_diff diff.mname diff String_map.empty
