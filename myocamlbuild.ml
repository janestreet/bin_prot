(* OASIS_START *)
(* OASIS_STOP *)

let dispatch = function
  | After_rules ->
    let env = BaseEnvLight.load () in
    let system = BaseEnvLight.var_get "system" env in
    let cc = BaseEnvLight.var_get "bytecomp_c_compiler" env in
    let is_darwin = String.is_prefix "macos" system in
    let arch_sixtyfour = BaseEnvLight.var_get "arch_sixtyfour" env = "true" in

    let cpp = cc ^ " -E -xc -undef -w" in
    let cpp = if arch_sixtyfour then cpp ^ " -DARCH_SIXTYFOUR" else cpp in

    let cpp = S [A "-pp"; P cpp] in

    dep ["ocaml"; "ocamldep"; "mlh"] ["lib/int_codes.mlh"];

    flag ["ocamldep"; "ocaml"; "use_pa_bin_prot"]
      (S [A "-ppopt"; P "syntax/pa_bin_prot.cma"]);

    flag ["compile"; "ocaml"; "use_pa_bin_prot"]
      (S [A "-ppopt"; P "syntax/pa_bin_prot.cma"]);

    flag ["ocamldep"; "ocaml"; "cpp"] cpp;

    flag ["compile"; "ocaml"; "cpp"] cpp;

    flag ["doc"; "ocaml"; "cpp"] cpp;

    if is_darwin then
      flag ["compile"; "c"] (S [A "-ccopt"; A "-DOS_DARWIN"]);
  | _ ->
    ()

let () = Ocamlbuild_plugin.dispatch (fun hook -> dispatch hook; dispatch_default hook)
