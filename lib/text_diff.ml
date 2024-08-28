open Types

type t = Diffutils.Diff.t String_map.t

let vd_to_lines name vd =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.value_description (Ident.create_local name) formatter vd;
  Format.pp_print_flush formatter ();
  CCString.lines (Buffer.contents buf)

let md_to_lines name md =
  let buf = Buffer.create 256 in
  let formatter = Format.formatter_of_buffer buf in
  Printtyp.modtype formatter md.md_type;
  Format.pp_print_flush formatter ();
  let module_str = "module " ^ name ^ ": " ^ Buffer.contents buf in
  CCString.lines module_str

let process_value_diff (val_diff : Diff.value) =
  match val_diff.vdiff with
  | Added vd ->
      [
        Diffutils.Diff.Diff { orig = []; new_ = vd_to_lines val_diff.vname vd };
      ]
  | Removed vd ->
      [
        Diffutils.Diff.Diff { orig = vd_to_lines val_diff.vname vd; new_ = [] };
      ]
  | Modified { reference; current } ->
      [
        Diffutils.Diff.Diff
          {
            orig = vd_to_lines val_diff.vname reference;
            new_ = vd_to_lines val_diff.vname current;
          };
      ]

let from_diff (diff : Diff.module_) : Diffutils.Diff.t String_map.t =
  let rec process_module_diff module_path (module_diff : Diff.module_) acc =
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
              { orig = []; new_ = md_to_lines module_diff.mname curr_md };
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
              { orig = md_to_lines module_diff.mname ref_md; new_ = [] };
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

let pp_diff fmt diff = Diffutils.Diff.pp Diffutils.Diff.git_printer fmt diff

let gen_pp pp_diff fmt t =
  let print_module_diff module_path diff =
    Format.fprintf fmt "diff module %s:\n" module_path;
    pp_diff fmt diff;
    Format.fprintf fmt "\n"
  in
  String_map.iter print_module_diff t

let pp fmt t = gen_pp pp_diff fmt t

module With_colors = struct
  let pp_l fmt ~color ~prefix ~line =
    Format.fprintf fmt "%a%a\n"
      Fmt.(styled color string)
      prefix
      Fmt.(styled color string)
      line

  let pp_add fmt line = pp_l fmt ~color:`Green ~prefix:"+" ~line
  let pp_remove fmt line = pp_l fmt ~color:`Red ~prefix:"-" ~line
  let pp_keep fmt line = Format.fprintf fmt " %s\n" line

  let printer =
    Diffutils.Diff.printer ~same:pp_keep ~diff:(fun fmt { orig; new_ } ->
        List.iter (pp_remove fmt) orig;
        List.iter (pp_add fmt) new_)

  let pp_diff fmt diff = Diffutils.Diff.pp printer fmt diff
  let pp fmt t = gen_pp pp_diff fmt t
end
