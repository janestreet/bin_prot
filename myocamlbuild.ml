(* OASIS_START *)
(* OASIS_STOP *)

module JS = Jane_street_ocamlbuild_goodies

let dev_mode = true

let setup_preprocessor_deps = function
  | After_rules ->
    dep ["pp_deps_for_src"] ["src/int_codes.mlh"; "src/config.h"];
  | _ -> ()

let dispatch = function
  | After_rules ->
    let env = BaseEnvLight.load () in
    let cc = BaseEnvLight.var_get "bytecomp_c_compiler" env in
    let cpp = S [A "-pp"; P (cc ^ " -E -xc -undef -w")] in
    flag ["ocamldep"; "ocaml"; "cpp"] cpp;
    flag ["compile";  "ocaml"; "cpp"] cpp;
    flag ["doc";      "ocaml"; "cpp"] cpp;
  | _ ->
    ()

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    JS.alt_cmxs_of_cmxa_rule hook;
    JS.pass_predicates_to_ocamldep hook;
    if dev_mode && not Sys.win32 then JS.track_external_deps hook;
    setup_preprocessor_deps hook;
    dispatch hook;
    dispatch_default hook)

