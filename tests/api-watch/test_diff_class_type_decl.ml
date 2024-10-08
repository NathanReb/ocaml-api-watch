open Api_watch
open Types

let%test_module "Class type declaration diffing" =
  (module struct
    let basic_class_type_decl =
      {
        clty_params = [];
        clty_type =
          Cty_signature
            {
              csig_self = Ctype.newvar ();
              csig_self_row = Ctype.newvar ();
              csig_vars = Vars.empty;
              csig_meths = Meths.empty;
            };
        clty_path = Path.Pident (Ident.create_local "test_class");
        clty_hash_type =
          {
            type_params = [];
            type_arity = 0;
            type_kind = Type_abstract Definition;
            type_private = Public;
            type_manifest = None;
            type_variance = [];
            type_separability = [];
            type_is_newtype = false;
            type_expansion_scope = Btype.lowest_level;
            type_loc = Location.none;
            type_attributes = [];
            type_immediate = Unknown;
            type_unboxed_default = false;
            type_uid = Uid.internal_not_actually_unique;
          };
        clty_variance = [];
        clty_loc = Location.none;
        clty_attributes = [];
        clty_uid = Uid.internal_not_actually_unique;
      }

    let signature_with_class_type decl =
      [
        Sig_class_type
          (Ident.create_local "test_class", decl, Trec_first, Exported);
      ]

    let empty_signature = []

    let%expect_test "Addition of class type declaration" =
      let reference = empty_signature in
      let current = signature_with_class_type basic_class_type_decl in
      let result = Diff.interface ~module_name:"Test" ~reference ~current in
      let success = result <> None in
      Printf.printf "Test result: %b\n" success;
      print_endline
        (if success then "Differences found (as expected)"
         else "Unexpected lack of differences");
      [%expect
        {|
        Test result: true
        Differences found (as expected)
      |}]

    let%expect_test "Removal of class type declaration" =
      let reference = signature_with_class_type basic_class_type_decl in
      let current = empty_signature in
      let result = Diff.interface ~module_name:"Test" ~reference ~current in
      let success = result <> None in
      Printf.printf "Test result: %b\n" success;
      print_endline
        (if success then "Differences found (as expected)"
         else "Unexpected lack of differences");
      [%expect
        {|
        Test result: true
        Differences found (as expected)
      |}]

    let%expect_test "Identical signatures have no differences" =
      let signature = signature_with_class_type basic_class_type_decl in
      let result =
        Diff.interface ~module_name:"Test" ~reference:signature
          ~current:signature
      in
      let success = result = None in
      Printf.printf "Test result: %b\n" success;
      print_endline
        (if success then "No differences (as expected)"
         else "Unexpected differences found");
      [%expect
        {|
        Test result: true
        No differences (as expected)
      |}]
  end)
