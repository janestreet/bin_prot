(* OASIS_START *)
(* OASIS_STOP *)

(* Temporary hacks *)
let js_hacks = function
  | After_rules ->
    rule "Generate a cmxs from a cmxa"
      ~dep:"%.cmxa"
      ~prod:"%.cmxs"
      ~insert:`top
      (fun env _ ->
         Cmd (S [ !Options.ocamlopt
                ; A "-shared"
                ; A "-linkall"
                ; A "-I"; A (Pathname.dirname (env "%"))
                ; A (env "%.cmxa")
                ; A "-o"
                ; A (env "%.cmxs")
            ]));

    (* Pass -predicates to ocamldep *)
    pflag ["ocaml"; "ocamldep"] "predicate" (fun s -> S [A "-predicates"; A s])
  | _ -> ()

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
    js_hacks hook;
    setup_preprocessor_deps hook;
    dispatch hook;
    dispatch_default hook)

