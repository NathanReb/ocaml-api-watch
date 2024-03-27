let parse_interface content =
  let lexbuf = Lexing.from_string content in
  Parse.interface lexbuf

let generate_signature intf =
  let typing_env =
    Typemod.initial_env ~loc:Location.none ~initially_opened_module:None
      ~open_implicit_modules:[]
  in
  let typed_tree = Typemod.type_interface typing_env intf in
  typed_tree.sig_type

let compile_interface (content : string) : Types.signature =
  let intf = parse_interface content in
  let signature = generate_signature intf in
  signature
