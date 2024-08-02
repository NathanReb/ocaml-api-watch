let run (`Ref_cmi reference) (`Current_cmi current) =
  let current = Cmi_format.read_cmi current in
  let reference = Cmi_format.read_cmi reference in
  let diff =
    Api_watch.Diff.interface ~module_name:current.cmi_name
      ~reference:reference.cmi_sign ~current:current.cmi_sign
  in
  match diff with
  | None -> 0
  | Some diff ->
      let text_diff = Api_watch.Text_diff.from_diff diff in
      let print_module_diff module_path diff =
        Printf.printf "diff module %s:\n" module_path;
        Diffutils.Diff.pp Diffutils.Diff.git_printer Format.std_formatter diff;
        Printf.printf "\n"
      in
      Api_watch.String_map.iter print_module_diff text_diff;
      1

let named f = Cmdliner.Term.(app (const f))

let ref_cmi =
  let docv = "CMI" in
  let doc = "The .cmi for the reference version" in
  named
    (fun x -> `Ref_cmi x)
    Cmdliner.Arg.(required & pos 0 (some file) None & info ~doc ~docv [])

let current_cmi =
  let docv = "CMI" in
  let doc = "The .cmi for the current version" in
  named
    (fun x -> `Current_cmi x)
    Cmdliner.Arg.(required & pos 1 (some file) None & info ~doc ~docv [])

let info =
  let open Cmdliner in
  Cmd.info "api-watcher" ~version:"%%VERSION%%" ~exits:Cmd.Exit.defaults
    ~doc:"List API changes between two versions of a library"

let term = Cmdliner.Term.(const run $ ref_cmi $ current_cmi)
let main = Cmdliner.Cmd.v info term
let () = Stdlib.exit @@ Cmdliner.Cmd.eval' main
